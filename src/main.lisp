(in-package :cl-mediawiki)

(defclass mediawiki ()
  ((url :accessor url :initarg :url :initform nil)))

(defvar *mediawiki* nil
  "the current instance of media wiki we are dealing with (mostly for use with with-mediawiki)")

(defmacro with-mediawiki ((url) &body body)
  `(let ((*mediawiki* (make-instance 'mediawiki :url ,url)))
     ,@body
     ))

