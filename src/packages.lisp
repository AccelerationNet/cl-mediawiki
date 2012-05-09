;; -*- lisp -*-

(defpackage :net.acceleration.cl-mediawiki
    (:use :common-lisp )
  (:nicknames :cl-mediawiki)
  (:export
   ;; main
   #:mediawiki
   #:*mediawiki*
   #:*default-external-format*
   #:with-mediawiki
   ;; query
   #:login
   #:get-page-content
   #:get-action-tokens
   #:recent-changes
   #:user-contribs
   #:get-links
   #:get-revisions
   #:get-revisions-result
   #:list-page-sections
   #:find-page-section
   ;; query/query-result
   #:query-result
   #:has-more-results-p
   #:get-more-results
   #:results
   ;; EDIT
   #:set-page-content
   #:set-section-content
   #:append-text-to-page
   #:add-new-page-section
   #:create-page
   #:regex-replace-all
   #:get-page-info
   #:pages-that-embed
   #:upload)
  (:documentation 
   "This package provides a client to the mediawiki API, which is used 
by Wikipedia among others.

Exported functions fairly closely mirror the command structure of the
API, which is summarized here: http://en.wikipedia.org/w/api.php

 Usage example:
 ;; setup to use only Wikipedia
 CL-USER> (setf cl-mediawiki:*mediawiki* 
                (cl-mediawiki:with-mediawiki (\"http://en.wikipedia.org/w\") 
                 cl-mediawiki:*mediawiki*))

 ;; get content of article titled Pigment
 CL-USER> (cl-mediawiki:get-page-content \"Pigment\")

 ;; get the revids and sizes of its last 10 revisions
 CL-USER> (cl-mediawiki:get-revisions \"Pigment\" :rvprop \"ids|user|size\" :rvlimit 10)

Further documentation is in README.mediawiki."))
