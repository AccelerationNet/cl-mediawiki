(in-package :cl-mediawiki-test)

(def-test-wikipedia get-content-test (query)
  (assert-true
   (cl-mediawiki:get-page-content "Pigment")))

(def-test-wikipedia get-action-tokens-test (query)  
  (assert-true
      (cl-mediawiki:get-action-tokens "Pigment")))

(def-test-wikipedia pages-that-embed-test (query)
  (assert-false
      (cl-mediawiki:pages-that-embed "Pigment"))
  (assert-true
      (cl-mediawiki:pages-that-embed "Template:Grateful_Dead" )))

(def-test-wikipedia get-page-info-test (query)
  (assert-true
      (cl-mediawiki:get-page-info "Pigment" )))

(def-test-wikipedia recent-changes-test (query)
  (assert-true
      (cl-mediawiki:recent-changes)))

(def-test-wikipedia user-contribs-test (query)
  (assert-true
      (cl-mediawiki:user-contribs "bobbysmith007")))

(def-test-wikipedia get-revisions-test (query)
  (assert-true
      (cl-mediawiki:get-revisions "Pigment" :rvlimit 10)))
