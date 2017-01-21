;;;; binary-utils.lisp

(in-package #:binary-utils)

(defun read-bytes (n stream &optional acc)
  "Reads n bytes from a stream"
  (aif (and (>= n 0) (read-byte stream ))
       (read-bytes (1- n) stream (cons it acc))
       (reverse acc)))

(defmacro defdata (name &key bytes terminator)
  "Defines a binary data class."
  (cond ((and bytes terminator)
         (error "Cannot specify both a byte count and a terminator."))
        (terminator
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defclass ,name ()
               ((terminator :reader terminator :initform ,terminator)
                (content :accessor content)))
             (defmethod read-value ((object ,name) stream)
               (read-until ,terminator stream))))

        (bytes
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defclass ,name ()
               ((bytes :reader bytes :initform ,bytes)
                (content :accessor content)))
             (defmethod read-value ((object ,name) stream)
               (read-bytes ,bytes stream))))))

(defmacro defbinary (name superclasses slots)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,superclasses
       ,(mapcar #'(lambda (slot)
                    (let ((name (first slot))
                          (type (second slot)))
                      `(,name :accessor ,name :initform (make-instance ',type))))
                slots))
     (defmethod read-value ((binary ,name) stream)
       ,@(mapcar #'(lambda (slot)
                     (let ((name (first slot))
                           (type (second slot)))
                       `(setf (content (,name binary))
                              (read-value ',type stream))))
                 slots))))

(defmethod read-value ((type symbol) stream)
  (let ((object (make-instance type)))
    (read-value object stream)
    object))

(defdata u1 :bytes 1)
(defdata u2 :bytes 2)
(defdata u3 :bytes 3)
(defdata u4 :bytes 4)
(defdata u8 :bytes 8)
(defdata u20 :bytes 20)
(defdata tstring :terminator 0)
