(in-package :cl-mediawiki)


(defun get-page-content (title)
  (let ((parameters
	 (make-parameters
	  `((action query)
	    (prop revisions)
	    (rvprop content)
	    (titles ,title)))))
    (flet ((page-content (?title sxml)
	     "Accepts sxml and pulls out the page content from it"
	     (declare (ignorable ?title)) ;; supress compiler noise
	     (unify:match-case (sxml)
	      (`("api"
		 ()
		 ("query"
		  ()
		  ("pages"
		   ()
		   ("page" ,(unify:make-template 'list '(("title" ?title) &rest _))
			   ("revisions"
			    NIL
			    ("rev" NIL ?content))))))
		content
		)
	      (T nil)
	       
	       )))
      (page-content
       title 
       (parse-api-response-to-sxml (make-api-request parameters))))
    ))

(defclass token-bag ()
  ((timestamp :accessor timestamp :initarg :timestamp :initform nil)
   (tokens :accessor tokens :initarg :tokens :initform nil
	   :documentation "either a single token, or an
            alist mapping type to value" )))

(defmethod print-object ((token-bag token-bag) stream)
  (with-accessors ((timestamp timestamp)
		   (tokens tokens)) token-bag
    (format stream "#<Token-bag ~a ~a>" timestamp tokens)))

(defmethod edit-token ((token-bag token-bag))
  (cdr (assoc :edit (tokens token-bag))))

(defmethod move-token ((token-bag token-bag))
  (cdr (assoc :move (tokens token-bag))))

(defmethod delete-token ((token-bag token-bag))
  (cdr (assoc :delete (tokens token-bag) )))

(defun get-action-tokens (title &optional (tokens '(:edit)))
  "Returns a token bag which is how you get permission to perform
   edit/move/delete changes

   tokens, set of : :move :delete :edit"
  (let* (;; put pipes between the downcased tokens
	 (token-string (format nil "~{~(~a~)~^|~}" tokens))
	 (parameters
	  (make-parameters
	  `((action query)
	    (prop info)
	    (intoken ,token-string)
	    (titles ,title)))
	   ))
    (flet ((bind-token-&-ts (?title sxml)
	     "Accepts sxml and pulls out the page content from it"
	     (declare (ignorable ?title)) ;; supress compiler noise
	     (unify:match (`("api"
			     NIL
			     ("query"
			      NIL
			      ("pages"
			       NIL
			       ("page" ?alist))))
			    sxml)
	       (make-instance
		'token-bag
		:tokens
		(loop for token in tokens
		      collecting
		   (cons token
			 (attribute-value (format nil "~atoken" token) alist)))
		:timestamp (attribute-value "touched" alist))
	       )))
      (bind-token-&-ts
       title 
       (parse-api-response-to-sxml (make-api-request parameters))))
    ))