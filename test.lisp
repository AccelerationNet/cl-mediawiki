;; -*- lisp -*-
(in-package :cl-mediawiki)



(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:get-page-content "Pigment"))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:get-action-tokens "Pigment"))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:pages-that-embed "Template:Grateful_Dead" ))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:get-page-info "Pigment" ))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:user-contribs "bobbysmith007"))