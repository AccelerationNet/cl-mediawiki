(in-package :cl-mediawiki)

(defclass mediawiki ()
  ((url :accessor url :initarg :url :initform nil)
   (auth :accessor auth :initarg :auth :initform nil)
   (cookie-jar :accessor cookie-jar :initarg cookie-jar :initform (make-instance 'drakma:cookie-jar))))

(defvar *mediawiki* nil
  "the current instance of media wiki we are dealing with (mostly for use with with-mediawiki)")

(defmacro with-mediawiki ((obj) &body body)
  `(let ((*mediawiki* ,(typecase obj
			 (list obj)
			 (string `(make-instance 'mediawiki :url ,obj)))))
     ,@body
     ))

(defun make-api-request (api-params &key (basic-authorization (auth *mediawiki* )) (force-ssl T) (method :get))
  (let ((full-url (format nil "~a/api.php" (url *mediawiki*))))
    (push '("format" . "xml") api-params)
    (multiple-value-bind (content status headers uri stream must-close status-word)
	(drakma:http-request
	 full-url
	 :method method
	 :basic-authorization basic-authorization
	 :force-ssl force-ssl
	 :parameters api-params
	 :cookie-jar (cookie-jar *mediawiki*)
	 )
      (declare (ignore headers uri stream must-close status-word))
      (values content status))))

(defun make-parameters (params)
  (loop for binding in params
	when binding
	  collecting
       (destructuring-bind (key val) binding
	 (cons (format nil "~(~a~)" key)
	       (typecase val
		 (symbol (format nil "~(~a~)" val))
		 (string val))))))

(defun parse-api-response-to-sxml (content)
  (cxml:parse content (cxml-xmls:make-xmls-builder) :validate nil))

(defun attribute-value (key alist)
  (cadr (assoc key alist :test #'equalp)))

(define-condition media-wiki-error (error)
  ((obj :accessor obj :initarg :obj :initform nil)
   (code :accessor code :initarg :code :initform nil)
   (message :accessor message :initarg :message :initform nil)))

(defmethod print-object ((err media-wiki-error) stream)
  (format stream "MEDIA-WIKI-ERROR: ~a ~a ~%~a"
	  (code err)
	  (message err)
	  (obj err)
	  ))

(define-condition match-error (error)
  ((obj :accessor obj :initarg :obj :initform nil)
   (match-against :accessor match-against :initarg :match-against :initform nil)
   (message :accessor message :initarg :message :initform nil)))

(defmacro match-response-with-error-reporting ((match-form object)&body body)
  "Attempts to unify body as (match (match-form obj) ,@body)

   will detect wiki errors and hand them back to us as reasonable CL signals
   if we canot match signal a matching-error
  "
  (let ((obj-sym (gensym "obj")))
    `(let ((,obj-sym ,object))
       (unify:match-case
	(,obj-sym)
	(`("api"
	   NIL
	   ("error" ?err))
	  (error 'media-wiki-error :obj ,obj-sym
				   :code (attribute-value "code" err)
				   :message (attribute-value "info" err)))
	(,match-form
	 ,@body)
	(T
	 (error 'match-error :message "Error matching"
			     :obj ,obj-sym
			     :match-against ,match-form))
	))))