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

(defmacro define-proxy (name &key core req based-on props doc (processor 'identity) (method :GET))
  "Defines a function with NAME with REQ required parameters. The
symbols in the BASED-ON and PROPS lists are concatenated with pairs
from the CORE list and passed to the MAKE-PARAMETERS function."

  (let ((par-sym (gensym)))
    `(defun ,name (,@req &key ,@props ,@based-on) ,(if doc doc "no documentation given")
       (let ((,par-sym (make-parameters
			(list ,@(mapcar #'(lambda (x) (if (listp x)
							  `(list ',(car x) ',(cadr x))))
					core)
			      ,@(mapcar #'(lambda (x) (if (listp x)
							  `(list ',(car x) ,(car x))
							  `(list ',x ,x)))
					(concatenate 'list req props based-on ))))))
	 ;(print ,par-sym)
	 (funcall #',processor
		  (parse-api-response (make-api-request ,par-sym :method ,method)))))))

;; --------------------------------------------------------

(define-proxy login
    :core ((action login))
    :req (lgname lgpassword)
    :props (lgdomain)
    :method :POST
    :doc
    "This module is used to login and get the authentication tokens.
  In the event of a successful log-in, a cookie will be attached to
  your session. In the event of a failed log-in, you will not be able
  to attempt another log-in through this method for 5 seconds.  This
  is to prevent password guessing by automated password crackers.

This module only accepts POST requests.
Parameters:
  lgname         - User Name
  lgpassword     - Password
  lgdomain       - Domain (optional)
Example:
  (login \"user\" \"password\")
 ")

;; --------------------------------------------------------

(define-proxy userinfo
    :core ((action query)
	   (meta userinfo))
    :req ()
    :props (uiprop)
    :processor 
    (lambda (resp)
      (get-value resp :QUERY :USERINFO))
    :doc
    "Get information about the current user

This module requires read rights.
Parameters:
  uiprop         - What pieces of information to include
                     blockinfo  - tags if the current user is blocked, by whom, and for what reason
                     hasmsg     - adds a tag 'message' if the current user has pending messages
                     groups     - lists all the groups the current user belongs to
                     rights     - lists all the rights the current user has
                     changeablegroups - lists the groups the current user can add to and remove from
                     options    - lists all preferences the current user has set
                     editcount  - adds the current user's edit count
                     ratelimits - lists all rate limits applying to the current user
                   Values (separate with '|'): blockinfo, hasmsg, groups, rights, changeablegroups, options, preferencestoken, editcount, ratelimits, email
Examples:
  api.php?action=query&meta=userinfo
  api.php?action=query&meta=userinfo&uiprop=blockinfo|groups|rights|hasmsg")

;; --------------------------------------------------------

(define-proxy list-category-members
    :core ((action query)
	    (list categorymembers))
  :req (cmtitle)
  :based-on (version smaxage maxage requestid titles pageids revids prop
		     meta generator redirects indexpageids)
  :props (cmprop cmnamespace cmcontinue cmlimit cmsort cmdir cmstart cmend cmstartsortkey cmendsortkey)
  :processor
  (lambda (resp)
    (values-list (list
		  (get-value resp :query :categorymembers)
		  (get-value resp :query-continue :categorymembers :cmcontinue))))
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
     (list-category-members \"Category:Physics\" :cmprop 'title)

  Returns two values:

    1. a list of pages with keys (:PAGEID, :NS, :TITLE)

    2. a value to pass as CMCONTINUE parameter to continue retrieving
    list of categories
")

;; --------------------------------------------------------

(define-proxy get-page-content
    :core ((action query)
	   (prop revisions)
	   (rvprop "content|timestamp"))
    :req (titles)
    :props (rvsection)
    :processor
    (lambda (resp)
      (values-list (list
		    (get-value resp :query :pages #'first #'cdr :revisions #'first :*)
		    (get-value resp :query :pages #'first #'cdr :revisions #'first :timestamp)
		    )))
    :doc
    "Get the content for a given page

Parameters:
  titles - the title of the page

  rvsection      - only retrieve the content of this section
                   (a number indicating which section, not the section name)

 Examples: (get-page-content \"Physics\")

 Returns: a string with the given page content ")

;; --------------------------------------------------------

(define-proxy pages-that-embed
    :core ((action query)
	   (list embeddedin))
    :req (eititle)
    :props (eicontinue einamespace eifilterredir eilimit)
    :processor
    (lambda (resp)
      (values-list (list
		    (get-value resp :query :embeddedin)
		    (get-value resp :query-continue :embeddedin :eicontinue))))
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

 Examples: (pages-that-embed \"Template:Stub\")

 Returns two values:

   1. a list of pages with keys (:PAGEID :NS :TITLE)

   2. a value to pass to the EICONTINUE
")

;; --------------------------------------------------------

(defclass token-bag ()
  ((page-attributes
    :accessor page-attributes
    :initarg :page-attributes
    :initform nil
    :documentation "An alist of page attributes returned by the api")
   (timestamp
    :accessor timestamp
    :initarg :timestamp
    :initform nil)
   (tokens
    :accessor tokens
    :initarg :tokens
    :initform nil
    :documentation "either a single token, or an alist mapping type to value" )))

;; --------------------------------------------------------

(defmethod print-object ((token-bag token-bag) stream)
  (with-accessors ((timestamp timestamp)
		   (tokens tokens)) token-bag
    (format stream "#<Token-bag ~a ~a>" timestamp tokens)))

;; --------------------------------------------------------

(defmethod edit-token ((token-bag token-bag))
  (cdr (assoc :edit (tokens token-bag))))

;; --------------------------------------------------------

(defmethod move-token ((token-bag token-bag))
  (cdr (assoc :move (tokens token-bag))))

;; --------------------------------------------------------

(defmethod delete-token ((token-bag token-bag))
  (cdr (assoc :delete (tokens token-bag) )))

;; --------------------------------------------------------

(define-proxy get-action-tokens
    :core ((action query)
	   (rvprop timestamp)
	   (prop "info|revisions"))
    :req (titles)
    :props ((intoken :edit))
    :processor
    (lambda (res)
      (if (= 1 (length (get-value res :query :pages)))
	  (cdr (first (get-value res :query :pages)))
	  (map 'list #'cdr  (get-value res :query :pages))))
    :doc
    "Gets the tokens necessary to perform edits.

Parameters:
  titles - the title of the page we wish to edit
  intoken - which tokens do we want (out of :edit :move :delete :block :unblock  or a list of those)

 Examples: (get-action-tokens \"Physics\")
           (get-action-tokens \"Physics\" :intoken '(:edit :move :delete))
           (get-action-tokens '(\"Main Page\" \"User:Russ\") :intoken '(:move :edit :delete :protect))

 Returns: if one page is requested, returns an assoc list with page
 info; otherwise returns a list of assoc lists.
")

;; --------------------------------------------------------

(define-proxy get-page-info
    :core ((action query)
	   (prop info))
    :req (titles)
    :processor
    (lambda (res)
      (get-value res :query :pages))
    :doc
    "Gets the info for a given page as an alist

Parameters:
  titles - the title of the page we wish to retrieve the info of

 Returns: an alist of attributes about the pages. ")

;; --------------------------------------------------------

(define-proxy recent-changes
    :core ((action query)
	   (list recentchanges))
    :req ()
    :props (rcstart  rcend rcdir rcnamespace  rctitles  (rcprop "user|comment|title|timestamp|ids")  rcshow  rclimit  rctype)
    :processor
    (lambda (res)
      (get-value res :query :recentchanges))
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

;; --------------------------------------------------------

(define-proxy user-contribs
    :core ((action query)
	   (list usercontribs))
    :req (ucuser)
    :props (uclimit ucstart ucend ucuserprefix ucdir ucnamespace (ucprop "comment|title|timestamp|ids") ucshow)
    :processor
    (lambda (res)
      (get-value res :query :usercontribs))
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

;; EOF