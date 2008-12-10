;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.cl-mediawiki.system)
    (defpackage :net.acceleration.cl-mediawiki.system
	(:use :common-lisp :asdf))))

(in-package :net.acceleration.cl-mediawiki.system)

(defsystem :cl-mediawiki
  :description "A tool to help talk to mediawiki's api."
  :components ((:module :src
			:serial T
			:components ((:file "packages")
				     (:file "util" )
				     (:file "main" )
				     (:file "query" )
				     (:file "edit"))))
  :depends-on (:cxml :drakma :cl-unification :cl-ppcre))
