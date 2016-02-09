(load "load.lisp")

#+sbcl
(format t "XXX: cat deliver.lisp |sbcl --disable-debugger")

#+(or ccl clisp)
(ql:quickload "trivial-dump-core")

#+(or ccl clisp ecl ccl64)
(trivial-dump-core:save-executable "tail-ssh" #'log-ssh-logins)

#+sbcl
(sb-ext:save-lisp-and-die "tail-ssh" :compression 5 :executable t :toplevel 'sbcl-entry :save-runtime-options t)

#+lispworks
(deliver 'ctcl::main "dist/lispworks/tail-ssh" 0 :multiprocessing t :keep-eval t)
