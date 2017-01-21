;;;; package.lisp

(defpackage #:binary-utils
  (:use #:cl
        #:peyton-utils)
  (:export #:defdata
           #:defbinary
           #:read-value
           #:read-bytes
           #:u1
           #:u2
           #:u3
           #:u4
           #:u8
           #:u20
           #:tstring))
