
(defpackage :net.acceleration.cl-mediawiki-test
    (:nicknames #:cl-mediawiki-test)
  (:use :common-lisp :cxml :lisp-unit :cl-mediawiki)
  (:shadow :cdata :run-tests))

(in-package :cl-mediawiki-test)

(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defun cl-mediawiki-tests.info (message &rest args)
  (format *standard-output* "~&")
  (log-time (get-universal-time) *standard-output*)
  (apply #'format *standard-output* message args)
  (format *standard-output* "~%"))

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
  (loop for tag in args
	do (setf (get tag :tests)
		 (union (alexandria:ensure-list (get tag :tests))
			(list name))))
  `(lisp-unit:define-test ,name 
     (progn
       ,@body
       )))

(defmacro def-test-wikipedia (name (&rest args) &body body)
  `(def-test ,name (,@args)
     (cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
       ,@body
       )))

(defun run-tests (&key suites tests (use-debugger T))
  (let* ((*package* (find-package :cl-mediawiki-test))
	 (lisp-unit::*use-debugger* use-debugger)
	 (tests (append (alexandria:ensure-list tests)
			(loop for suite in (alexandria:ensure-list suites)
			      appending (get suite :tests))))
	 (out (with-output-to-string (*standard-output*)
		(lisp-unit::run-test-thunks
		 (lisp-unit::get-test-thunks
		  (if (null tests)
		      (get-tests *package*)
		      tests))))))
    
    (format *standard-output*
	    "~&~% ** TEST RESULTS: Cl-Mediawiki ** ~%-----------~%~A~%------ END TEST RESULTS ------~%"
	    out)))

