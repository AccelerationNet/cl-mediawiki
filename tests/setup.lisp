(defpackage :net.acceleration.cl-mediawiki-test
  (:nicknames #:cl-mediawiki-test)
  (:use :common-lisp #:lisp-unit2))

(in-package :cl-mediawiki-test)

(defmacro define-wikipedia-test (name () &body body)
  `(lisp-unit2:define-test ,name ()
     (cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
       ,@body)))
