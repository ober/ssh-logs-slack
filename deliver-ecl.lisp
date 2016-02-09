(load "load.lisp")

(compile-file "tail.lisp" :system-p t)

;; (defconstant +compound-fasl+ (compile-file-pathname "compound" :type :fasl))

;; (c::build-fasl +compound-fasl+
;; 	       :lisp-files
;; 	       (list (compile-file-pathname "tail.lisp" :type :object)))

;; (load +compound-fasl+)

(defconstant +standalone-exe+ (compile-file-pathname "tail-ecl" :type :program))

(c::build-program +standalone-exe+
		  :lisp-files
		  (list (compile-file-pathname "tail.lisp" :type :object))
		  :epilogue-code
		  '(log-ssh-logins))
