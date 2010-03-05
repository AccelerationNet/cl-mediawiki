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
   #:get-page-history
   #:get-action-tokens
   #:recent-changes
   #:user-contribs
   ;; EDIT
   #:set-page-content
   #:append-text-to-page
   #:add-new-page-section
   #:create-page
   #:regex-replace-all
   #:get-page-info
   #:pages-that-embed))