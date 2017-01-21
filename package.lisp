;;;; package.lisp

(defpackage #:binary-utils
  (:use #:cl)
  (:export #:defdata
           #:defbinary
           #:read-value
           #:u1
           #:u2
           #:u3
           #:u4
           #:u20
           #:tstring))
