(in-package :cl-mediawiki)
;; This file has a few necessary random utility functions


(defun symbolize-string (str &optional (package :keyword))
  "Turns a string into a happy symbol

   ex: ''foo bar_bast'' -> FOO-BAR-BAST
  "
  (etypecase str
    (string (intern (nsubstitute
		     #\- #\_
		     (nsubstitute #\- #\space (string-upcase str) :test #'char=)
		     :test #'char=)
		    package))
    (symbol str)))

