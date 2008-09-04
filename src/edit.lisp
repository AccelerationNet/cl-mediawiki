(in-package :cl-mediawiki)
(cl-interpol:enable-interpol-syntax)

(defun check-edit-response (title xml)
  "Checks for the expected 'success' message
   signals match-errors assertion-errors and media-wiki-errors
  "
  (match-response-with-error-reporting
      (`("api"
	 NIL
	 ("edit"
	  #T(list &rest ?alist)))
	xml)
    (assert (string-equal "success" (attribute-value "result" alist))
	    (alist) "Failed to create page: '~a' ~a  ~a ~%~%~A" title (attribute-value "result" alist) (string-equal "success "(attribute-value "result" alist)) alist)
    T
    )
  )

(defun create-page
    (title text &key
	   (summary  "cl-mediawiki:create-page")
	   (override nil))
  "Creates a new wiki page
   If override is true, replace the existing page with the text passed in
  "
  (let* ((tokens (get-action-tokens title))
	 (parameters
	  (make-parameters
	   `((action edit)
	     (token ,(edit-token tokens))
	     (title ,title)
	     (basetimestamp ,(timestamp tokens))
	     ,(when (not override)
		  '(createonly  true))
	     (summary ,summary)
	     (text ,text)))
	   ))
      (check-edit-response
       title
       (parse-api-response-to-sxml (make-api-request parameters :method :post))))
  
  )

(defun add-new-page-section (title section-title section-text &key no-create)
  "Creates a new == section-title ==  at the bottom of the page. followed by the specified text"
  (let* ((tokens (get-action-tokens title))
	 (parameters
	  (make-parameters
	   `((action edit)
	     (token ,(edit-token tokens))
	     (title ,title)
	     (section new)
	     (summary ,section-title)
	     (basetimestamp ,(timestamp tokens))
	     ,(when no-create
		'(nocreate T))
	     (text ,section-text)))
	   ))
      (check-edit-response
       title
       (parse-api-response-to-sxml
	(make-api-request parameters :method :post)))
    ))

(defun append-text-to-page
    (title text &key
	   no-create
	   (summary  "cl-mediawiki:append-text-to-page"))
  "appends the text the the end of the page (will create the page if neccessary)"
  (let* ((tokens (get-action-tokens title))
	 (parameters
	  (make-parameters
	   `((action edit)
	     (token ,(edit-token tokens))
	     (title ,title)
	     (summary ,summary)
	     (basetimestamp ,(timestamp tokens))
	     ,(when no-create
		'(nocreate T))
	     (appendtext ,text)))
	   ))
    (check-edit-response
     title
     (parse-api-response-to-sxml
      (make-api-request parameters :method :post)))
    ))

(defun prepend-text-to-page
    (title text &key
	   (summary  "cl-mediawiki:prepend-text-to-page")
	   no-create)
  "Adds the text to the beginning of the page
   (will create the page if neccessary unless no-create is true)"
  (let* ((tokens (get-action-tokens title)) 
	 (parameters
	  (make-parameters
	   `((action edit)
	     (token ,(edit-token tokens))
	     (title ,title)
	     (summary ,summary)
	     (basetimestamp ,(timestamp tokens))
	     ,(when no-create
		'(nocreate T))
	     (prependtext ,text)))
	   ))
    (check-edit-response
     title
     (parse-api-response-to-sxml
      (make-api-request parameters :method :post)))
    ))

(defun set-page-content
    (title text &key no-create
	   (summary  "cl-mediawiki:set-page-content"))
  "sets the text of a wiki page to the specified text"
  (let* ((tokens (get-action-tokens title))
	 (parameters
	  (make-parameters
	   `((action edit)
	     (token ,(edit-token tokens))
	     (title ,title)
	     (summary ,summary)
	     (basetimestamp ,(timestamp tokens))
	     ,(when no-create
		'(nocreate T))
	     (text ,text)))
	   ))
    (check-edit-response
     title
     (parse-api-response-to-sxml
      (make-api-request parameters :method :post)))
    ))