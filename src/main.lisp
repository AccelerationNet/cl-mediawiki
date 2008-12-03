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
	(let ((drakma:*drakma-default-external-format* :utf-8))
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

(defun make-parameters (params)
  "Takes a list of bindings (:key :val) and prepares them for transit
   by converting them to strings
   (if either the pair is nil or the value is nil, we drop that param)
  "
  (loop for binding in params
	;; only collect when we have a key and value
	when (and binding (cadr binding))
	  collecting
       (destructuring-bind (key val) binding
	 ;; grabs a downcased key and its value (downcased if symbol)
	 ;; as a pair of strings
	 (cons (format nil "~(~a~)" key)
	       (typecase val
		 (symbol (format nil "~(~a~)" val))
		 (T (princ-to-string val)))))))

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