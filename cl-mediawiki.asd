;; -*- lisp -*-
;; Copyright (c) 2008 Accelerated Data Works, Russ Tyndall

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.cl-mediawiki.system)
    (defpackage :net.acceleration.cl-mediawiki.system
	(:use :common-lisp :asdf))))

;; --------------------------------------------------------

(in-package :net.acceleration.cl-mediawiki.system)

;; --------------------------------------------------------

(defsystem :cl-mediawiki
  :description "A tool to help talk to mediawiki's api."
  :components ((:module :src
			:serial T
			:components ((:file "packages")
				     (:file "util" )
				     (:file "main" )
				     (:file "query" )
				     (:file "edit"))))
  :depends-on (:drakma :cl-json :log5))

;; EOF