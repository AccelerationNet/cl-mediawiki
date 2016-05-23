(in-package :cl-mediawiki-test)

(define-wikipedia-test get-content-test ()
  (lisp-unit2:assert-true
   (cl-mediawiki:get-page-content "Pigment")))

(define-wikipedia-test get-action-tokens-test ()
  (lisp-unit2:assert-true
   (cl-mediawiki:get-action-tokens "Pigment")))

(define-wikipedia-test pages-that-embed-test ()
  (lisp-unit2:assert-false
   (cl-mediawiki:pages-that-embed "Pigment"))
  (lisp-unit2:assert-true
   (cl-mediawiki:pages-that-embed "Template:Grateful_Dead" )))

(define-wikipedia-test get-page-info-test ()
  (lisp-unit2:assert-true
   (cl-mediawiki:get-page-info "Pigment" )))

(define-wikipedia-test recent-changes-test ()
  (lisp-unit2:assert-true
   (cl-mediawiki:recent-changes)))

(define-wikipedia-test user-contribs-test ()
  (lisp-unit2:assert-true
   (cl-mediawiki:user-contribs "bobbysmith007")))

(define-wikipedia-test get-revisions-test ()
  (lisp-unit2:assert-true
   (cl-mediawiki:get-revisions "Pigment" :rvlimit 10)))
