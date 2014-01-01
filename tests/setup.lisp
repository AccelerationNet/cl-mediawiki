
(defpackage :net.acceleration.cl-mediawiki-test
    (:nicknames #:cl-mediawiki-test)
  (:use :common-lisp :cxml :lisp-unit2-asserts :cl-mediawiki)
  (:shadowing-import-from :cl-mediawiki :results)
  (:shadow :cdata :run-tests))

(in-package :cl-mediawiki-test)

;; make everythign in cl-mediawiki accessible here
(with-package-iterator (sym '(:cl-mediawiki) :internal :external)
  (let (more? symbol accessibility pkg)
    (loop do (multiple-value-setq (more? symbol accessibility pkg) (sym))
	     (when (eql (find-package :cl-mediawiki)
			pkg)
	       (ignore-errors
                (unintern symbol :cl-mediawiki-test)
                (import (list symbol) :cl-mediawiki-test)))
	  while more?
	  )))

(defmacro def-test (name (&rest args) &body body)
  `(lisp-unit2:define-test ,name (:tags '(,@args))
    ,@body
    ))

(defmacro def-test-wikipedia (name (&rest args) &body body)
  `(def-test ,name (,@args)
     (cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
       ,@body
       )))

(defun run-tests (&key suites tests)
  (let* ((*package* (find-package :cl-mediawiki-test)))
    (lisp-unit2:run-tests
     :tests tests
     :tags suites
     :name :cl-mediawiki
     :run-contexts #'lisp-unit2:with-summary-context)))

