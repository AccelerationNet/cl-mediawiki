(in-package :cl-mediawiki)

(defclass mediawiki ()
  ((url :accessor url :initarg :url :initform nil)
   (auth :accessor auth :initarg :auth :initform nil)))

(defvar *mediawiki* nil
  "the current instance of media wiki we are dealing with (mostly for use with with-mediawiki)")

(defmacro with-mediawiki ((obj) &body body)
  `(let ((*mediawiki* ,(typecase obj
			 (list obj)
			 (string `(make-instance 'mediawiki :url ,obj)))))
     ,@body
     ))

(defun make-api-request (api-params &key (basic-authorization (auth *mediawiki* )) (force-ssl T))
  (let ((full-url
	 ;; The last bit of that format is a conditional
	 ;; http://gigamonkeys.com/book/a-few-format-recipes.html#conditional-formatting 
	 (format nil "~a/api.php?format=xml~@[&~a~]" (url *mediawiki*) api-params)))
    (multiple-value-bind (content status headers uri stream must-close status-word)
	(drakma:http-request full-url :basic-authorization basic-authorization :force-ssl force-ssl)
      (declare (ignore headers uri stream must-close status-word))
      (values content status))))

(defun parse-api-response-to-sxml (content)
  (cxml:parse content (cxml-xmls:make-xmls-builder) :validate nil))

(defun attribute-value (key alist)
  (cadr (assoc key alist :test #'equalp)))