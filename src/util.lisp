(in-package :cl-mediawiki)
;; This file has a few necessary random utility functions

(defun ensure-list (x)
  "just ensure that you have alist"
  (if (listp x) x (list x)))

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

(defun map-tree (fn &rest trees)
  "Do a depth first traversal of some set of trees calling fn on every non-nil element. "
  (when trees
    (dolist (n trees)
      (etypecase n
	(null)
	(atom (funcall fn n))
	(list
	   (funcall fn n)
	   (apply #'map-tree fn n))))))

(defun find-tree (pred &rest trees)
  "find a tree based on a predicate"
  (let ((results))
    (flet ((handler (node)
	     (when (funcall pred node)
	       (push node results))))
      (map-tree #'handler trees)
      (nreverse results))))

(defun find-nodes-by-name (name &rest trees)
  "find all sxml nodes with a given name "
  (find-tree (lambda (n)
	       (string-equal
		(when (and (listp n) (stringp (car n)))
		  (car n)) name)) trees))

