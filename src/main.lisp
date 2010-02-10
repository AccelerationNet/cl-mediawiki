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

;; (log5:start-sender 'warnings-and-worse  
;; 		   (log5:stream-sender :location *error-output*)  
;; 		   :category-spec '(log5:warn)  
;; 		   :output-spec '(log5:time log5:message log5:context))

(in-package :cl-mediawiki)

;; Cirqumvent restrictions put by strict Common Lisp implementations.
;; see <http://www.sbcl.org/manual/Defining-Constants.html>
(defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc))))

(defparameter *default-external-format* :UTF-8
  "Sets as the drakma default coding system.")

(define-constant +retry-after-http-status+ 503)

(define-constant +max-lag-retries+ 5
  "Number of retries when maxlag gives 503 error code.")

;; --------------------------------------------------------

(defclass mediawiki ()
  ((url
    :accessor url
    :initarg :url
    :initform (error "Missing MediaWiki URL.")
    :documentation "URL where to look for MediaWiki API; the 'api.php'
    is appended automatically. In order to work with WikiPedia use
    'http://LANG.wikipedia.org/w' where LANG is corresponding language
    code.")
   (user-name
    :accessor user-name
    :initarg :user-name
    :initform nil
    :documentation "Login name to authenticate with the server.")
   (password
    :accessor password
    :initarg :password
    :initform nil
    :documentation "Password to authenticate on the server.")
   (bot-p
    :accessor bot-p
    :initarg :bot-p
    :initform t
    :documentation "Toggles whether to mark edits as a bot.")
   (assert-edit-p
    :accessor assert-edit-p
    :initarg :assert-edit-p
    :initform t
    :documentation "Toggles whether to use 'Assert Edit' MediaWiki extensions.")
   (maxlag
    :accessor maxlag
    :initarg :maxlag
    :initform 0
    :documentation "Specify maxlag parameter for queries to the
    server. Higher values mean more aggressive behaviour, lower values
    are nicer. Whenever the maxlag is exceeded 503 HTTP code is
    returned and 'Retry-After' HTTP header is set with minimum number
    of seconds to wait before repeating the request. Using maxlag
    parameter is a best-practice recommended by WP:MKBOT.")
   (request-delay
    :accessor request-delay
    :initarg :request-delay
    :initform 6
    :documentation "Delay in seconds between consequtive
    requests. Used to avoid high and frequent traffic to the server.")
   (last-request-time
    :accessor last-request-time
    :initarg :last-request-time
    :initform 0
    :documentation "Time when the last request was made. See
    GET-UNIVERSAL-TIME.")
   (cookie-jar
    :accessor cookie-jar
    :initarg cookie-jar
    :initform (make-instance 'drakma:cookie-jar)))
  (:documentation "Main mediawiki information container."))

;; --------------------------------------------------------

(defvar *mediawiki* nil
  "the current instance of media wiki we are dealing with (mostly for
  use with with-mediawiki)")

;; --------------------------------------------------------

(defmethod initialize-instance :after ((mw mediawiki) &key)
  "If user name is supplied, login; if it is a bot, check whether it
  has new messages."
  (unless (null (user-name mw))
    (setf *mediawiki* mw)
    (login (user-name mw) (password mw))))

;; --------------------------------------------------------

(defmacro with-mediawiki ((obj) &body body)
  `(let ((*mediawiki* ,(typecase obj
			 (string `(make-instance 'mediawiki :url ,obj))
			 (T obj))))
     ,@body))

;; --------------------------------------------------------

(defun make-api-request (api-params &key (force-ssl nil force-ssl-p) (method :GET))
  "Calls the media wiki api providing the specified parameters.

API-PARAMS is an alist with CARs specifying keywords and CDRs
corresponding values."

  ;; force-ssl should either be whats passed in, or if nothing is passed in
  ;; check to see what protocol we used to connect to the server
  (let ((force-ssl (if force-ssl-p
		       force-ssl
		       (string-equal "https://" (url *mediawiki*)
				     :end2 (length "https://"))))
	(full-url (format nil "~a/api.php" (url *mediawiki*)))
	(requests-count 0)
	(result nil))
    ;; configure default JSON format
    (push '("format" . "json")
	  api-params)
    ;; add 'Maxlag parameter' to POST requests
    (when (and (not (= (maxlag *mediawiki*) 0))
	       (equal method :POST))
      (push `("maxlag" . ,(format nil "~A" (maxlag *mediawiki*)))
	    api-params))
    ;; check whether add assert parameter (Extension:Assert_Edit)
    (when (and (string-equal "edit"
			     (cdr (assoc "action" api-params
					 :test #'string-equal)))
	       (assert-edit-p *mediawiki*)
	       (bot-p *mediawiki*))
      (push '("assert" . "bot")
	    api-params))
    ;; check whether requests come too often
    (when (< (- (get-universal-time) (last-request-time *mediawiki*))
	     (request-delay *mediawiki*))
      (log5:log-for trace "request is to soon, sleeping")
      (sleep (- (request-delay *mediawiki*)
		(- (get-universal-time) (last-request-time *mediawiki*)))))
    ;; send request
    (tagbody send-request
       (multiple-value-bind (content status-code headers uri stream must-close status-word)
	   (let ((drakma:*drakma-default-external-format*
		  *default-external-format*)
		 (drakma:*text-content-types* '(("application" . "json")
						("text" . nil))))
	     (drakma:http-request full-url
				  :method method
				  :force-ssl force-ssl
				  :parameters api-params
				  :cookie-jar (cookie-jar *mediawiki*)))
	 (declare (ignore stream must-close status-word))
	 (log5:log-for trace "uri: ~A" uri)
	 ;; check whether maxlag signals to pause
	 (when (and (= status-code +retry-after-http-status+)
		    (drakma:header-value :retry-after headers))
	   (if (< (incf requests-count) +max-lag-retries+)
	       (progn
		 (log5:log-for warn "Sleeping for ~A seconds" (parse-integer (drakma:header-value :retry-after headers)))
		 (sleep (parse-integer (drakma:header-value :retry-after headers)))
		 (go send-request))
	       (error "Exhausted attempts to send request")))
	 (setf result (values content status-code))))
    (setf (last-request-time *mediawiki*) (get-universal-time))
    result))

;; --------------------------------------------------------

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

;; --------------------------------------------------------

(defun parse-api-response (content)
  "Parse JSON string and detect error response."
  (let ((resp (json:decode-json-from-string content)))
    (when (get-value resp :error)
      (error "have MediaWiki error"))
    resp))

;; --------------------------------------------------------

(defun get-value (js &rest keys)
  "JS is a list of key-value pairs as parsed by JSON:DECODE-JSON.

Keys are either keywords or function designators."
  ;; todo: probably REDUCE is a better approach
  (let ((ret js))
    (loop for key in keys
       do 
       (setf ret (if (functionp key)
		     (apply key (list ret))
		     (cdr (assoc key ret)))))
    ret))

;; --------------------------------------------------------

(define-condition media-wiki-error (error)
  ((obj :accessor obj :initarg :obj :initform nil)
   (code :accessor code :initarg :code :initform nil)
   (message :accessor message :initarg :message :initform nil)))

;; --------------------------------------------------------

(defmethod print-object ((err media-wiki-error) stream)
  (format stream "MEDIA-WIKI-ERROR: ~a ~a ~%~a"
	  (code err)
	  (message err)
	  (obj err)))

;; --------------------------------------------------------

(define-condition match-error (error)
  ((obj :accessor obj :initarg :obj :initform nil)
   (match-against :accessor match-against :initarg :match-against :initform nil)
   (message :accessor message :initarg :message :initform nil)))

;; EOF