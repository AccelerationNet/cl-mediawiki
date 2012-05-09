;; See ../LICENSE  for info 
(in-package :cl-mediawiki)

(eval-when  (:compile-toplevel :load-toplevel :execute)
  (defparameter +default-query-params+
    '(titles pageids revids prop list meta generator redirects indexpageids export exportnowrap  )
    "The parameters that are available for any action=query api call"))

(defmacro define-proxy (name &key core req based-on props doc (processor 'identity) (method :GET))
  "Defines a function with NAME with REQ required parameters. The
symbols in the BASED-ON and PROPS lists are concatenated with pairs
from the CORE list and passed to the MAKE-PARAMETERS function."
  ;; get the args that are not in req and are found in either based-on or props
  ;; Also add default params if necessary
  (let* ((kw-params (set-difference
		     (union props
			    (if (eq 'query (cadr (assoc 'action core)))
				(union based-on +default-query-params+)
				based-on))
		     req)))
    (let ((par-sym (gensym)))
      `(defun ,name (,@req &key ,@kw-params) ,(if doc doc "no documentation given")
	 (let ((,par-sym (make-parameters
			  (list ,@(mapcar #'(lambda (x) (if (listp x)
							    `(list ',(car x) ',(cadr x))))
					  core)
				,@(mapcar #'(lambda (x) (if (listp x)
							    `(list ',(car x) ,(car x))
							    `(list ',x ,x)))
					  (concatenate 'list req kw-params ))))))
					;(print ,par-sym)
	   (funcall #',processor
		    (parse-api-response-to-sxml (make-api-request ,par-sym :method ,method))))))))

(defun login (lgname lgpassword &key lgdomain)
  ;;login is a 2 step process.  http://www.mediawiki.org/wiki/API:Login
  (let* ((sxml (%login lgname lgpassword :lgdomain lgdomain))
	 (login-attrs (cadar (find-nodes-by-name "login" sxml)))
	 (result (cadr (find "result" login-attrs
			     :test #'string= :key #'car))))
    (if (string= result "NeedToken")
	(%login lgname lgpassword :lgdomain lgdomain
		:lgtoken (cadr (find "token" login-attrs
				     :test #'string= :key #'car)))
	sxml)))

(define-proxy %login
    :core ((action login))
    :req (lgname lgpassword)
    :props (lgdomain lgtoken)
    :method :POST
    :doc
    "
  This module is used to login and get the authentication tokens. 
  In the event of a successful log-in, a cookie will be attached
  to your session. In the event of a failed log-in, you will not 
  be able to attempt another log-in through this method for 5 seconds.
  This is to prevent password guessing by automated password crackers.

This module only accepts POST requests.
Parameters:
  lgname         - User Name
  lgpassword     - Password
  lgdomain       - Domain (optional)
  lgtoken        - Login token obtained in first request
Example:
  api.php?action=login&lgname=user&lgpassword=password ")


(define-proxy list-category-members
    :core ((action query)
	    (list categorymembers))
  :req (cmtitle)
  :based-on (version maxlag smaxage maxage requestid titles pageids revids prop
		     meta generator redirects indexpageids)
  :props (cmprop cmnamespace cmcontinue cmlimit cmsort cmdir cmstart cmend cmstartsortkey cmendsortkey)
  :processor
  (lambda (sxml)
    (let ((rows (find-nodes-by-name "cm" sxml)))  
      (loop for row in rows
	    collecting (loop for (attr val) in (second row)
			     collecting (list (symbolize-string attr) val) ))) )
  :doc
    "List all pages in a given category.

Parameters:
  cmtitle        - Which category to enumerate (required). Must include Category: prefix
  cmprop         - What pieces of information to include
                   Values (separate with '|'): ids, title, sortkey, timestamp
                   Default: ids|title
  cmnamespace    - Only include pages in these namespaces
                   Values (separate with '|'): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 100, 101
  cmcontinue     - For large categories, give the value retured from previous query
  cmlimit        - The maximum number of pages to return.
                   No more than 500 (5000 for bots) allowed.
                   Default: 10
  cmsort         - Property to sort by. One value: sortkey, timestamp,Default: sortkey
  cmdir          - In which direction to sort. One value: asc, desc Default: asc
  cmstart        - Timestamp to start listing from. Can only be used with cmsort=timestamp
  cmend          - Timestamp to end listing at. Can only be used with cmsort=timestamp
  cmstartsortkey - Sortkey to start listing from. Can only be used with cmsort=sortkey
  cmendsortkey   - Sortkey to end listing at. Can only be used with cmsort=sortkey

 Examples:
   Get first 10 pages in [[Category:Physics]]:
     (list-category-members \"Category:Physics\")

   Get page info about first 10 pages in [[Category:Physics]]:
     (list-category-members \"Category:Physics\" :prop 'info)

  Returns a list of alists, each representing a CategoryMember
    alist keys are: :title :ns :pageid
"
    )


#| Some experimenting indicates that, at least for queries
where ((action query) (prop revisions) (rvprop content)), the
parameters titles and revids are mutually exclusive. 

The following implementation is therefore broken. By requiring titles,
it prevents you from doing queries that specified a particular
revision id, for instance. |#

(define-proxy get-page-content
    :core ((action query)
	   (prop revisions)
	   (rvprop content))
  :req (titles)
  :props (rvsection)
  :processor
  (lambda (sxml)
    (let ((rows (find-nodes-by-name "rev" sxml)))  
      (third (first rows))))
  :doc
    "Get the content for a given page.

Does not accept revid. To get the content of older pages, use
get-revisions with the rvprop content tag.

Parameters:
  titles - the title of the page

  rvsection      - only retrieve the content of this section
                   (a number indicating which section, not the section name)

 Examples: (get-page-content \"Physics\")

 Returns: a string with the given page content
")

(define-proxy get-page-content-by-revid
    :core ((action query)
	   (prop revisions)
	   (rvprop content))
  :req (revids)
  :props (rvsection)
  :processor
  (lambda (sxml)
    (let ((rows (find-nodes-by-name "rev" sxml)))
      (third (first rows))))
  :doc
    "Get the content for a given revid

Parameters:
  revids - the revision id of the page

  rvsection      - only retrieve the content of this section
                   (a number indicating which section, not the section name)

 Examples: (get-page-content 446445813)

 Returns: a string with the given page content
")

(define-proxy %parse-text-sections
    :core ((action parse)
	   (prop sections))
  :req (text)
  :processor
  (lambda (sxml &aux (rvsection 0))
    (mapcar
     #'(lambda (s &aux (attrs (second s)))
	 ;; s looks like:
	 ;; ("s"
	 ;;  (("anchor" "HREF_ANCHOR") ("byteoffset" "")
	 ;;  ("fromtitle" "PAGE TITLE") ("index" "T-2") ("number" "1.1")
	 ;;  ("line" "SECTION TITLE") ("level" "3") ("toclevel" "2")))

	 (flet ((find-attr (name)
		  (second
		   (find name attrs :key #'first :test #'string-equal))))
	 (list
	  (find-attr "number")
	  (find-attr "line")
	  (find-attr "anchor")
	  (incf rvsection))))
     (find-nodes-by-name "s" sxml)))
  :doc "parses the given text and lists sections in that content.
returns list of (number name anchor rvsection)")

(defun list-page-sections (page-title)
  "lists sections in a page, returns list of (number name anchor rvsection)

rvsection is suitable to for the :rvsection param of get-page-content
"
  ;; ask wiki to parse some markup and show the text sections,
  ;; crafting the markup to return the desired page
  ;; see http://lists.wikimedia.org/pipermail/mediawiki-api/2008-March/000392.html
  (%parse-text-sections (format nil "{{:~a}}__TOC__" page-title)))

(defun find-page-section (page-title section-name)
  "searches the the given page for the given section name. returns nil or (number name anchor rvsection)"
  (find section-name (list-page-sections page-title)
	:key #'second
	:test #'string-equal))

(define-proxy pages-that-embed
    :core ((action query)
	   (list embeddedin))
  :req (eititle)
  :props (eicontinue einamespace eifilterredir eilimit)
  :processor
  (lambda (sxml)
    (let* ((rows (find-nodes-by-name "ei" sxml))
	   (c-blob (first (find-nodes-by-name
			   "embeddedin"
			   (first (find-nodes-by-name "query-continue" sxml)))))
 	   (continuation (when c-blob
 			   (destructuring-bind (_1 ((_2 continuation))) c-blob
 			     (declare (ignore _1 _2))
 			     continuation)))
	  titles)
      (loop for row in rows
	    do (destructuring-bind (_1 ((_2 title) &rest _3)) row
		   (declare (ignore _1 _2 _3))
		   (push title titles)))
      (values (nreverse titles) continuation)))
  :doc
    "List pages that embed a given template or other page

Parameters:
  eititle        - Title to search. If null, titles= parameter will be used instead, but will be obsolete soon.
  eicontinue     - When more results are available, use this to continue.
  einamespace    - The namespace to enumerate.
                   Values (separate with '|'): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 100, 101, 102, 103
  eifilterredir  - How to filter for redirects
                   One value: all, redirects, nonredirects
                   Default: all
  eilimit        - How many total pages to return.
                   No more than 500 (5000 for bots) allowed.
                   Default: 10

 Examples: (pages-that-embed \"Template:Client\")

 Returns: a list of pagetitles and a continuation (if there is one)
")

(defclass token-bag ()
  ((page-attributes :accessor page-attributes :initarg :page-attributes :initform nil
       :documentation "An alist of page attributes returned by the api")
   (timestamp :accessor timestamp :initarg :timestamp :initform nil)
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

(define-proxy get-action-tokens
    :core ((action query)
	   (rvprop timestamp)
	   (prop "info|revisions"))
  :req (titles)
  :props ((intoken :edit))
  :processor
  (lambda (sxml)
    (let ((pages (find-nodes-by-name "page" sxml)))
      (let ((result (loop for (page alist . children) in pages
			  collecting
		       (make-instance
			    'token-bag
			    :page-attributes (convert-sxml-attribs-to-alist alist)
			    :tokens
			    (loop for token in (ensure-list intoken)
				  collecting
			       (cons token
				     (sxml-attribute-value (format nil "~atoken" token) alist)))
			    ;; The timestamp on the tokens bag may be incorrect, its better to
			    ;; look at the revision history for the correct one
			    :timestamp (let ((rev (first (loop for child in children
							       for res = (find-nodes-by-name "rev" child)
							       until res
							       finally (return res)))))
					 (if rev
					   (sxml-attribute-value "timestamp" (second rev))
					   (sxml-attribute-value "touched" alist)))))))
	(if (eq 1 (length result)) (car result) result))))
  :doc
    "Gets the tokens necessary for perform edits.

Parameters:
  titles - the title of the page we wish to edit
  intoken - which tokens do we want (out of :edit :move :delete :block :unblock  or a list of those)

 Examples: (get-action-tokens \"Physics\")
           (get-action-tokens \"Physics\" :intoken '(:edit :move :delete))
           (get-action-tokens '(\"Main Page\" \"User:Russ\") :intoken '(:move :edit :delete :protect))

 Returns: a token bag (or list of them if you asked for multiple pages) 
")

(define-proxy get-page-info
    :core ((action query)
	   (prop info))
  :req (titles)
  :processor
  (lambda (sxml)
    (convert-sxml-attribs-to-alist
     (second (first (find-nodes-by-name "page" sxml))
    )))
  :doc
    "Gets the info for a given page as an alist

Parameters:
  titles - the title of the page we wish to retrieve the info of

 Returns: an alist of attributes about the page
")

(define-proxy get-revisions
    :core ((action query)
	   (prop revisions))
    :req (titles)
    :props ((rvprop "ids|flags|timestamp|user|comment|size") 
	    (rvlimit 550) rvstartid rvendid rvstart rvend rvdir rvuser 
	    rvexcludeuser rvcontinue rvdiffto)
    :processor
    (lambda (sxml)
;;    (format *debug-io* "~&get-revisions processing sxml== ~S" sxml) ; debug
      (flet ((parse-rawrev (rawrev)
               "Parses a rev tag, to handle possible tag contents.
Adds text from rvprop content or parameter rvdiffto as an attrib.
Possible rev structures:
| <rev attrs... />                             #normal rvprops
| <rev attrs...>CONTENT</rev>                  #rvprop content
| <rev attrs...><diff>DIFFCONTENT</diff></rev> #rvdiffto

Content is introduced by a xml:space attribute, which we dump.

This ad-hoc parsing makes me sad."
               (destructuring-bind (revstr attribs &optional revcontent) rawrev
                 (declare (ignore revstr))
                 (let* ((attribs-filtered
                         (remove-if-not ;removes any xml:space attrib
                          #'(lambda (attrib) (stringp (car attrib)))
                          attribs))
                        (attribs-with-content
                         (cond          ; <rev attrs... />
                           ((null revcontent) 
                            attribs-filtered)
                                        ; <rev attrs...>content</rev>
                           ((stringp revcontent) 
                             (cons (list "content" revcontent) attribs-filtered))
                                        ; <rev attrs...><diff>DIFFCONTENT</diff></rev>
                           ((and (listp revcontent) (equal (first revcontent) "diff"))
                             (cons (list "diffcontent" (elt revcontent 2)) attribs-filtered))
                           (t           ; error. no match. drop the contents
                            attribs-filtered))))
                   (convert-sxml-attribs-to-alist attribs-with-content)))))
        (let* ((rawrevs (find-nodes-by-name "rev" sxml))
               (revs (mapcar #'parse-rawrev rawrevs))
               (c-blob (first (find-nodes-by-name "query-continue" sxml))))
          (values revs (when c-blob
                         (destructuring-bind (_1 _2 (_3 ((_4 continuation)))) c-blob
			   (declare (ignore _1 _2 _3 _4))
			   continuation))))))
    :doc
    "Gets the revisions of a page.

Parameters:
  titles         - the title of the page we wish to retrieve the info of
  rvprop:        - Which properties to get for each revision
                   Possible values (separate with '|'): ids, flags, timestamp, 
                   user, comment, size, content. Default is all except content.
  rvcontinue: -    When more results are available, use this to continue
                   (This is different from the returned continuation.)
  rvlimit: 	 - The maximum number of revisions to return (enum)
  rvstartid: 	 - Revision ID to start listing from. (enum)
  rvendid: 	 - Revision ID to stop listing at. (enum)
  rvstart: 	 - Timestamp to start listing from. (enum)
  rvend: 	 - Timestamp to end listing at. (enum)
  rvdir: 	 - Direction to list in. (enum)
                   Possible values: older, newer.
                   Default: older
  rvuser: 	 - Only list revisions made by this user
  rvexcludeuser: - Do not list revisions made by this user
  rvdiffto:      - Revision ID to diff each revision to.
                   Possible values (an id, \"prev\", \"next\" or \"cur\"). 

 Examples: (get-revisions \"Pigment\" :rvprop \"ids|user|size\" :rvlimit 10)
           (get-revisions \"Physics\" :rvlimit 10)

 Returns: list of revisions as alists and (if there is one) a continuation, 
which is the rvstart id to pass in the next call to get more results.
")

(define-proxy get-links
    :core ((action query)
	   (prop links))
    :req (titles)
    :props ((pllimit 5000) 
	    (plnamespace nil)
	    plcontinue)
    :processor
    (lambda (sxml)
      (let ((links (mapcar #'(lambda (n) (convert-sxml-attribs-to-alist (second n)))
			  (find-nodes-by-name "pl" sxml)))
	    (c-blob (first (find-nodes-by-name "query-continue" sxml))))
	(values links (when c-blob
		       (destructuring-bind (_1 _2 (_3 ((_4 continuation)))) c-blob
			   (declare (ignore _1 _2 _3 _4))
			   continuation)))))
    :doc
    "Gets a list of all links on the provided pages.

Parameters:
  titles         - the title of the page we wish to retrieve the info of
  pllimit        - How many links to return. Default: 10. No more than 500 (5000 for bots) allowed.
  plcontinue     - When more results are available, use this to continue.
  plnamespace    - Only list links to pages in these namespaces.
                   (For example, set plnamespace to 0 to get only article links in Wikipedia.)

Examples: 
  ; gets 10 results
  (get-links \"Pigment\" :pllimit 10) 

  ; gets 10 results, then gets 10 more using a continuation token
  (multiple-value-bind (firstresults continuation-token) 
		  (get-links \"Pigment\" :pllimit 10)
		(let ((secondresults (get-links \"Pigment\" :pllimit 10 :plcontinue continuation-token)))
		  (list firstresults secondresults)))
")


(define-proxy recent-changes
    :core ((action query)
	   (list recentchanges))
  :req ()
  :props (rcstart  rcend rcdir rcnamespace  (rcprop "user|comment|title|timestamp|ids")  rcshow  rclimit  rctype)
  :processor
  (lambda (sxml)
    (mapcar #'(lambda (n)
		 (convert-sxml-attribs-to-alist
		  (cadr n)))
      (cddr (first (cddr (find "query" (cddr sxml)
			       :key #'first
			       :test #'string-equal)))))
    ;sxml
    )
  :doc
    "Enumerates the recent changes

Parameters:
  rcstart        - The timestamp to start enumerating from.
  rcend          - The timestamp to end enumerating.
  rcdir          - In which direction to enumerate.
                   One value: newer, older
                   Default: older
  rcnamespace    - Filter log entries to only this namespace(s)
                   Values (separate with '|'): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 100, 101, 102, 103
  rctitles       - Filter log entries to only these page titles
  rcprop         - Include additional pieces of information
                   Values (separate with '|'): user, comment, flags, timestamp, title, ids, sizes, redirect, patrolled
                   Default: title|timestamp|ids
  rcshow         - Show only items that meet this criteria.
                   For example, to see only minor edits done by logged-in users, set show=minor|!anon
                   Values (separate with '|'): minor, !minor, bot, !bot, anon, !anon, redirect, !redirect, patrolled, !patrolled
  rclimit        - How many total changes to return.
                   No more than 500 (5000 for bots) allowed.
                   Default: 10
  rctype         - Which types of changes to show.
                   Values (separate with '|'): edit, new, log

 Returns: 
")

(define-proxy user-contribs
    :core ((action query)
	   (list usercontribs))
  :req (ucuser)
  :props (uclimit ucstart ucend ucuserprefix ucdir ucnamespace (ucprop "comment|title|timestamp|ids") ucshow)
  :processor
  (lambda (sxml)
    (mapcar #'(lambda (n)
		 (convert-sxml-attribs-to-alist
		  (cadr n)))
      (cddr (first (cddr (find "query" (cddr sxml)
			       :key #'first
			       :test #'string-equal)))))
    ;sxml
    )
  :doc
    "  Get all edits by a user
Parameters:
  uclimit        - The maximum number of contributions to return.
                   No more than 500 (5000 for bots) allowed.
                   Default: 10
  ucstart        - The start timestamp to return from.
  ucend          - The end timestamp to return to.
  ucuser         - The user to retrieve contributions for.
  ucuserprefix   - Retrieve contibutions for all users whose names begin with this value. Overrides ucuser.
  ucdir          - The direction to search (older or newer).
                   One value: newer, older
                   Default: older
  ucnamespace    - Only list contributions in these namespaces
                   Values (separate with '|'): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 100, 101, 102, 103
  ucprop         - Include additional pieces of information
                   Values (separate with '|'): ids, title, timestamp, comment, flags
                   Default: ids|title|timestamp|flags|comment
  ucshow         - Show only items that meet this criteria, e.g. non minor edits only: show=!minor
                   Values (separate with '|'): minor, !minor

")

;;;; query-result and friends
;; a query-result encapsulates the response to a query, and allows
;; for repeated follow-up queries

(define-modify-macro
    appendf (&rest lists) append
    "Modify-macro for APPEND. Appends LISTS to the place designated by the first argument.")

(defclass query-result ()
  ((results :accessor results :initarg :results :initform '()) ; a sequence of revisions retrieved so far
   (closure :initarg :closure :initform nil))
  (:documentation "Accumulating result to a get-revisions-result query.

  Use has-more-results-p to check if there are more results available.
  Use get-more-results to get them through one or more queries."))

(defmethod has-more-results-p ((qr query-result))
  "Returns nil, or the closure used for a single follow-up query"
  (slot-value qr 'closure))

(defmethod get-more-results-once ((qr query-result))
  "Fetches more results with one follow-up query.

   Updates the query-result object with these new results.
   Returns the object, and the number of new items fetched."
  (multiple-value-bind (newrevs c-token new-closure)
      (funcall (slot-value qr 'closure))
    (declare (ignore c-token))
    (appendf (slot-value qr 'results) newrevs)
    (setf (slot-value qr 'closure) new-closure)
    (values qr (length newrevs))))

(defmethod get-more-results ((qr query-result) &key (at-least 0) (pause 1))
  "Fetches AT-LEAST more results, re-querying every PAUSE seconds if necessary.

   If AT-LEAST is nil, repeats until it gets all results.
   Updates the query-result object with these new results.
   Returns the object, and the number of new items fetched."
  (loop with fetched = 0
        while (and (has-more-results-p qr)
		   (if (numberp at-least) (< fetched at-least) 't))
        do (incf fetched (second (multiple-value-list (get-more-results-once qr))))
        do (sleep pause)
        finally (return (values qr fetched))))

;;;; get-revisions-result and friends
;; query functions for doing get-revisions queries that
;; return their results as query-result objects

(defun get-revisions-and-closure (&rest args)
  "Like get-revisions, but also returns a closure for a follow-up query.

   This closure can be called outside of a with-mediawiki form.

   Example:
    (multiple-value-list (with-mediawiki (\"http://en.wikipedia.org/w\")
       (get-revisions-and-closure \"Pigment\" :rvlimit 3)))
    (multiple-value-list (funcall (elt * 2)))
    (multiple-value-list (funcall (elt * 2)))
    etc.."
  (let ((mediawiki *mediawiki*))  ; capture *mediawiki* into lexical scope
    (multiple-value-bind (revs c-token) (apply #'get-revisions args)
      (values revs (when c-token c-token)
	      (when c-token
		(setf (getf (cdr args) :rvstartid) c-token) ;set next rvstartid
		(lambda ()
		  (with-mediawiki (mediawiki) ; use captured *mediawiki*
		    (apply #'get-revisions-and-closure args))))))))

(defun get-revisions-result (&rest args)
  "Like get-revisions, but returns a query-result object"
  (multiple-value-bind (revs c-token closure)
      (apply #'get-revisions-and-closure args)
    (declare (ignore c-token))
    (make-instance 'query-result :results revs :closure closure)))

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
