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


(in-package :cl-mediawiki)

;; --------------------------------------------------------

(defun check-edit-response (title js)
  (log5:log-for trace "completed page [[~A]] with result ~A" title (get-value js :edit :result)))

;; --------------------------------------------------------

(defun create-page (tokens title text &key
		    (summary  "cl-mediawiki:create-page")
		    (override nil))
  "Creates a new wiki page.

If override is true, replace the existing page with the text passed
in (if the page already exists)
  "
  (let* ((parameters
	  (make-parameters
	   `((action edit)
	     (token ,(get-value tokens :edittoken))
	     (title ,title)
	     (basetimestamp ,(get-value tokens :revisions #'first :timestamp))
	     ,(when (not override)
		    '(createonly  true))
	     (summary ,summary)
	     (text ,text)))))
    (check-edit-response title
			 (parse-api-response (make-api-request parameters
							       :method :POST)))))

;; --------------------------------------------------------

(defun add-new-page-section (tokens title section-title section-text &key
			     no-create
			     (bot NIL)
			     (timestamp NIL))
  "Creates a new == section-title ==  at the bottom of the page. followed by the specified text"
  (let* ((parameters
	  (make-parameters
	   `((action edit)
	     (token ,(get-value tokens :edittoken))
	     (title ,title)
	     (section new)
	     (summary ,section-title)
	     (bot ,bot)
	     (basetimestamp ,(if timestamp
				 timestamp
				 (get-value tokens :revisions #'first :timestamp)))
	     ,(when no-create
		    '(nocreate T))
	     (text ,section-text)))))
    (check-edit-response title
			 (parse-api-response
			  (make-api-request parameters
					    :method :POST)))))

;; --------------------------------------------------------

(defun append-text-to-page (tokens title text &key
			    no-create
			    (bot NIL)
			    (timestamp NIL)
			    (summary  "cl-mediawiki:append-text-to-page"))
  "appends the text the the end of the page (will create the page if neccessary, unless no-create is passed)"
  (let* ((parameters
	  (make-parameters
	   `((action edit)
	     (token ,(get-value tokens :edittoken))
	     (title ,title)
	     (summary ,summary)
	     (bot ,bot)
	     (basetimestamp ,(if timestamp
				 timestamp
				 (get-value tokens :revisions #'first :timestamp)))
	     ,(when no-create
		    '(nocreate T))
	     (appendtext ,text)))))
    (check-edit-response title
			 (parse-api-response
			  (make-api-request parameters
					    :method :POST)))))

;; --------------------------------------------------------

(defun prepend-text-to-page (tokens title text &key
			     no-create
			     (bot NIL)
			     (timestamp NIL)
			     (summary  "cl-mediawiki:prepend-text-to-page"))
  "Adds the text to the beginning of the page named title
   (will create the page if neccessary unless no-create is true)"
  (let* ((parameters
	  (make-parameters
	   `((action edit)
	     (token ,(get-value tokens :edittoken))
	     (title ,title)
	     (summary ,summary)
	     (bot ,bot)
	     (basetimestamp ,(if timestamp
				 timestamp
				 (get-value tokens :revisions #'first :timestamp)))
	     ,(when no-create
		    '(nocreate T))
	     (prependtext ,text)))))
    (check-edit-response title
			 (parse-api-response
			  (make-api-request parameters
					    :method :POST)))))

;; --------------------------------------------------------

(defun set-page-content (tokens title text &key
			 (no-create nil)
			 (summary  "cl-mediawiki:set-page-content")
			 (bot T)
			 (minor NIL)
			 (timestamp NIL))
  "sets the text of a wiki page 'title' to the specified 'text',

     title: The wiki page to set the content of
     text: The new content that the wiki page should have
     no-create:, do not create the wiki page if it does not exist
     summary: The comment associated with changing the page content
    "
  (let* ((parameters
	  (make-parameters
	   `((action edit)
	     (token ,(get-value tokens :edittoken))
	     (title ,title)
	     (summary ,summary)
	     (bot ,bot)
	     (minor ,minor)
	     (basetimestamp ,(if timestamp
				 timestamp
				 (get-value tokens :revisions #'first :timestamp)))
	     ,(when no-create
		    '(nocreate T))
	     (text ,text)))))
    (check-edit-response title
			 (parse-api-response
			  (make-api-request parameters
					    :method :POST)))))

;; EOF