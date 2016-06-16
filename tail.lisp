(load "~/quicklisp/setup.lisp")

(defvar *mytasks* (list))
(defvar *myfiles* (list))
(defvar *state* (make-hash-table :test 'equalp))
(defvar *walk-time* 100) ;; how often in secs do we want to go hunt for new files

(defvar *slacktoken* "DEFINE YOUR TOKEN HERE!")
(defvar *logdir* "/var/log/") ;; Define your location for ssh.log files to be found.
(defvar *ssh-log-channel* "ssh") ;; Name of channel to send to
(defvar *ssh-slack-user* "SSHBot") ;; Name of bot that sends messages to above channel.

(defvar *alert-count* 0)
(defvar *maxfiles* 400)

(ql:quickload '(:pcall :bordeaux-threads :drakma :cl-fad :cl-ppcre :local-time :split-sequence :cl-date-time-parser :fare-memoization))

(defmacro with-input-from-file ((stream-name file-name &rest args) &body body)
  "Evaluate BODY with STREAM-NAME bound to an input-stream from file
FILE-NAME. ARGS is passed directly to open."
  (when (member :direction args)
    (error "Can't specifiy :DIRECTION in WITH-INPUT-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :input ,@args)
     ,@body))

(defmacro with-followed-file ((stream-name file-name &rest args) &body body)
  `(with-input-from-file (,stream-name ,file-name ,@args)
     (loop
	(handler-case
	    (progn ,@body)
	  (end-of-file () (sleep 1))
	  (t (c) c)
	  ;;((or error simple-error file-error) () nil)
	  ))))

(defun follow-file-and-alert (x pattern reformatter deliver)
  (with-followed-file (s x)
    (let ((lline (read-line s)))
      (if (cl-ppcre:all-matches pattern lline)
	  (if (is-new-log lline 3600)
	      (send-message-to-all (funcall reformatter lline) deliver))))))

(defun tail-file (x)
  (with-followed-file (s x)
    (format t "~&~A~%" (read-line s))))

(defun send-message-to-all (msg deliver)
  (incf *alert-count*)
  (funcall deliver msg))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun slack-log-ssh (msg)
  (slack-send msg *ssh-log-channel* *ssh-slack-user*))

(defun slack-send (msg channel user)
    (slack-io-send msg *slacktoken* channel user))

(defun slack-io-send (msg token channel user)
  (let ((output (drakma:http-request
		 "https://slack.com/api/chat.postMessage"
		 :content (format nil "token=~A&channel=%23~A&text=~A&username=~A" token channel (replace-all msg " " "%20") user)
		 :method :post)))
    (format t "slack:~A~%" output)))

(defun get-year ()
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    year))

(defun now-in-secs ()
  (local-time:timestamp-to-unix
   (local-time:now)))

(fare-memoization:define-memo-function string-to-secs (date)
  (let* ((universal (cl-date-time-parser:PARSE-DATE-TIME date))
	 (ts (local-time:universal-to-timestamp universal))
	 (secs (local-time:timestamp-to-unix ts)))
    secs))

(defun get-date-from-line (log-entry)
  (let* ((words (cl-ppcre:split "\\s+" log-entry))
	 (ldate (format nil "~A ~A ~A ~A" (nth 0 words) (nth 1 words) (nth 2 words) (get-year))))
    ldate))

(defun is-new-log (log-entry seconds-ago)
  (let* ((now-sec (now-in-secs))
	 (log-date-sec (string-to-secs (get-date-from-line log-entry)))
	 (diff (- now-sec log-date-sec)))
    (if (and
	 (< diff seconds-ago)
	 (> diff 0))
	diff
	nil)))

(defun reformat-full-line (line)
  (format nil "~A" line))

(defun clojure-error-line (line)
  (replace-all line "$" "%0A"))
;; (if (search "$" line)
;;     (replace-all line "" "%0A")
;;     line))(

(defun ssh-reformat-line (line)
  (let* ((split (cl-ppcre:split "\\s+" line))
	 (time (nth 2 split))
	 (user (nth 8 split))
	 (target (nth 3 split))
	 (ip (nth 10 split))
	 (resolved (get-hostname-by-ip ip)))
    (format nil "~A: @~A ~A from ~A (~A)" time user target (or resolved ip) ip)))

(defun find-logs (logname logdir pattern reformatter deliver)
  (setf (pcall:thread-pool-size) *maxfiles*)
  (cl-fad:walk-directory
   logdir
   (lambda (x)
     (if (string= (file-namestring x) logname)
	 (push x *myfiles*)
	 (push
	  (bordeaux-threads:make-thread
	   #'(lambda ()
	       (follow-file-and-alert x pattern reformatter deliver)))
	  *mytasks*)))))

(defun log-ssh-logins ()
  (find-logs "sshd.log" "/var/log/hosts/" "Accepted publickey" #'ssh-reformat-line #'slack-log-ssh)
  (loop (format t "count:~A~%" *alert-count*)  (sleep 1)))

#+sbcl
(defun sbcl-entry()
  (handler-case (log-slack-api-errors)
    (sb-sys:interactive-interrupt ()
      (sb-ext:quit))))

(fare-memoization:define-memo-function get-hostname-by-ip (ip)
  #+sbcl
  (ignore-errors (sb-bsd-sockets:host-ent-name
		  (sb-bsd-sockets:get-host-by-address
		   (sb-bsd-sockets:make-inet-address ip))))
#+lispworks
(comm:get-host-entry ip :fields '(:name))
#+clozure
(ignore-errors (ccl:ipaddr-to-hostname (ccl:dotted-to-ipaddr ip))))

(defun main()
  (let ((i 0))
    (loop
       (if (>= *walk-time* i)
	   (setf i 0)
	   (find-me-new-files))
       (format t "alerts:~A myfiles:~A mytasks:~A i:~A~%" *alert-count* (length *myfiles*) (length *mytasks*) i)
       (inc i)
       (sleep 1))))


(defun get-size (f)
  #+allegro (progn
	      (require 'osi)
	      (with-open-file (x f)
		(excl.osi:stat-size (excl.osi:fstat x))))
  #+lispworks (progn
		(sys:file-stat-size (sys:get-file-stat "/etc/issue")))
  #+sbcl (sb-posix:stat-size (sb-posix:fstat (open f)))
  )

(defun get-inode (f)
  #+allegro (progn
	      (require 'osi)
	      (with-open-file (x f)
		(excl.osi:stat-ino (excl.osi:fstat x))))
  #+lispworks (progn
		(sys:file-stat-inode (sys:get-file-stat "/etc/issue")))
  #+sbcl (sb-posix:stat-ino (sb-posix:fstat (open f)))
  )

