(load "load.lisp")

#+(or ccl clisp ecl)
(ql:quickload "trivial-dump-core")

(format t "XXX: cat deliver.lisp |sbcl --dynamic-space-size 2048 --disable-debugger")

#+sbcl
(sb-ext:save-lisp-and-die "dist/sbcl/tail-ssh" :compression 5 :executable t :toplevel 'log-ssh-logins :save-runtime-options t)

#+(or ccl ccl64 )
(trivial-dump-core:save-executable "dist/ccl/tail-ssh" #'log-ssh-logins)
