;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.cl-mediawiki.system)
    (defpackage :net.acceleration.cl-mediawiki.system
	(:use :common-lisp :asdf))))

(in-package :net.acceleration.cl-mediawiki.system)

(defsystem :cl-mediawiki
  :description "A tool to help talk to mediawiki's api."
  :components ((:module :src
			:components ((:file "packages")
				     (:file "main")
				     (:file "query" :depends-on ("packages" "main"))
				     (:file "edit" :depends-on ("query")))))
  :depends-on (:cxml :drakma :cl-unification :cl-ppcre))
