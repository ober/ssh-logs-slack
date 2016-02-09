(load "~/quicklisp/setup.lisp")
(ql:quickload '(:fare-memoization :cl-fad :gzip-stream :cl-json :bordeaux-threads))

(defpackage :ctcl
  (:use :cl :fare-memoization :cl-fad :gzip-stream :cl-json :bordeaux-threads))
