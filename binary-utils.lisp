;;;; binary-utils.lisp

(in-package #:binary-utils)

(defun read-bytes (n stream &optional acc)
  "Reads n bytes from a stream"
  (aif (and (> n 0) (read-byte stream nil))
       (read-bytes (1- n) stream (cons it acc))
       (reverse acc)))

(defmacro defdata (name &key bytes terminator function)
  "Defines a binary data class."
  (cond ((and bytes terminator)
         (error "Cannot specify both a byte count and a terminator."))
        (terminator
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defclass ,name ()
               ((terminator :reader terminator :initform ,terminator)
                (content :accessor content :initform nil)))
             (defmethod read-value ((object ,name) stream)
               (setf (content object)
                     (funcall ,function
                              (read-until ,terminator stream :read #'read-byte)))
               (read-until-not ,terminator stream :read #'read-byte)
               (content object))))

        (bytes
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defclass ,name ()
               ((bytes :reader bytes :initform ,bytes)
                (content :accessor content)))
             (defmethod read-value ((object ,name) stream)
                 (setf (content object)
                       (funcall ,function (read-bytes ,bytes stream))))))))

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
                       `(let ((content (read-value ',type stream)))
                          (if (null content)
                              (return-from read-value)
                              (setf (,name binary)
                                    (content content))))))
                 slots))))

(defmethod read-value ((type symbol) stream)
  (let ((object (make-instance type)))
    (if (read-value object stream)
        object)))

(defdata u1 :bytes 1 :function #'octets->integer)
(defdata u2 :bytes 2 :function #'octets->integer)
(defdata u3 :bytes 3 :function #'octets->integer)
(defdata u4 :bytes 4 :function #'octets->integer)
(defdata u8 :bytes 8 :function #'octets->integer)
(defdata u20 :bytes 20 :function #'octets->integer)
(defdata s4 :bytes 4 :function #'octets->string)
(defdata s8 :bytes 8 :function #'octets->string)
(defdata tstring :terminator 0 :function #'octets->string)
