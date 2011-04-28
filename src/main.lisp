(in-package :cl-mediawiki)
(defclass mediawiki ()
  ((url :accessor url :initarg :url :initform nil)
   (auth :accessor auth :initarg :auth :initform nil)
   (cookie-jar :accessor cookie-jar :initarg cookie-jar :initform (make-instance 'drakma:cookie-jar))))

(defvar *mediawiki* nil
  "the current instance of media wiki we are dealing with (mostly for use with with-mediawiki)")

(defmacro with-mediawiki ((obj) &body body)
  `(let ((*mediawiki* ,(typecase obj
			 (string `(make-instance 'mediawiki :url ,obj))
			 (T obj))))
     ,@body
     ))

(defun make-api-request (api-params &key (basic-authorization (auth *mediawiki* )) (force-ssl nil force-ssl-p) (method :get))
  "Calls the media wiki api providing the specified parameters"
  ;; force-ssl should either be whats passed in, or if nothing is passed in
  ;; check to see what protocol we used to connect to the server
  (let ((force-ssl (if force-ssl-p
		       force-ssl
		       (eq 0 (search "https://" (url *mediawiki*) :test #'char-equal ))
		       ))
	(full-url (format nil "~a/api.php" (url *mediawiki*))))
    (push '("format" . "xml") api-params)
    (multiple-value-bind (content status headers uri stream must-close status-word)
	(let ((drakma:*drakma-default-external-format* *default-external-format*))
	  (drakma:http-request
	   full-url
	   :method method
	   :basic-authorization basic-authorization
	   :force-ssl force-ssl
	   :parameters api-params
	   :cookie-jar (cookie-jar *mediawiki*)
	   ))
      (declare (ignore headers uri stream must-close status-word))
      (values content status))))

(defvar *default-external-format* :utf-8
  "sets as the drakma default coding system")

(defun make-parameters (params)
  "Takes a list of bindings (:key :val) and prepares them for transit
   by converting them to strings
   (if either the pair is nil or the value is nil, we drop that param)
  "
  (flet ((format-list-element (el)
	   (typecase el
	     (symbol (string-downcase (princ-to-string el)))
	     (T (princ-to-string el)))))
    (loop for binding in params
	  ;; only collect when we have a key and value
	  when (and binding (cadr binding))
	    collecting
	 (destructuring-bind (key val) binding
	   ;; grabs a downcased key and its value (downcased if symbol)
	   ;; as a pair of strings
	   (cons (format nil "~(~a~)" key)
		 (typecase val
		   ;;lists should be pipe delimited
		   (list (format nil "~{~a~^|~}" (mapcar #'format-list-element val)))
		   (symbol (format nil "~(~a~)" val))
		   (T (princ-to-string val))))))))

(defun parse-api-response-to-sxml (content)
  (cxml:parse content (cxml-xmls:make-xmls-builder) :validate nil))

(defun sxml-attribute-value (key alist)
  (cadr (assoc key alist :test #'equalp)))

(defun convert-sxml-attribs-to-alist (sxml-attribs)
  (loop for ((key val) . rest) = sxml-attribs then rest
	collecting (cons (symbolize-string key)  val)
	while rest))

(define-condition media-wiki-error (error)
  ((obj :accessor obj :initarg :obj :initform nil)
   (code :accessor code :initarg :code :initform nil)
   (message :accessor message :initarg :message :initform nil)))

(defmethod print-object ((err media-wiki-error) stream)
  (format stream "MEDIA-WIKI-ERROR: ~s ~a ~%~s"
	  (code err)
	  (message err)
	  (obj err)
	  ))

(define-condition match-error (error)
  ((obj :accessor obj :initarg :obj :initform nil)
   (match-against :accessor match-against :initarg :match-against :initform nil)
   (message :accessor message :initarg :message :initform nil)))

(defun check-for-xml-for-error (xml)
  "search the response for <api><error attribs></api>"
  (loop for kid in (cddr xml) ;; first node is api
	do (when (string-equal "error" (first kid))
	     (let ((err (second kid)))
	       (error 'media-wiki-error
		      :obj xml
		      :code (sxml-attribute-value "code" err)
		      :message (sxml-attribute-value "info" err))))))

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
