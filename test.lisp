;; -*- lisp -*-
(in-package :cl-mediawiki)

(log5:start-sender 'warnings-and-worse  
 		   (log5:stream-sender :location *error-output*)  
 		   :category-spec '(log5:warn)  
 		   :output-spec '(log5:time log5:message log5:context))

(debugging 'dribble+) 

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:get-page-content "Pigment"))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:get-action-tokens "Pigment"))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:pages-that-embed "Template:Stub" ))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:get-page-info "Pigment" ))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:recent-changes))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:user-contribs "bobbysmith007"))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawiki:userinfo))

(cl-mediawiki:with-mediawiki ("http://en.wikipedia.org/w")
  (cl-mediawik:list-category-members "Category:Formal_methods"))


(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (wiki:get-action-tokens "Test_Page"))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (let ((tokens (wiki:get-action-tokens "Test_Page")))
    ;(format t "tokens: ~A~%" tokens)
    (wiki:set-page-content
     tokens "Test_Page"
     (format nil "This is a different {{coord dms|d|d|d|d|}} text with a link to the [[Main Page]] (~A)." (get-universal-time))
     :bot T
     :summary "action summary")))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (let ((tokens (wiki:get-action-tokens "Test_Page")))
    ;(format t "tokens: ~A~%" tokens)
    (wiki:add-new-page-section
     tokens "Test_Page" "New section"
     (format nil "This is a different {{coord dms|d|d|d|d|}} text with a link to the [[Main Page]] (~A)." (get-universal-time))
     :bot T)))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (let ((tokens (wiki:get-action-tokens "Test_Page")))
    ;(format t "tokens: ~A~%" tokens)
    (wiki:append-text-to-page
     tokens "Test_Page"
     (format nil "~%~%This is a different {{coord dms|||||}} text with a link to the [[Main Page]] (~A).~%" (get-universal-time))
     :bot T
     :summary "action summary")))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (let ((tokens (wiki:get-action-tokens "Test_Page")))
    ;(format t "tokens: ~A~%" tokens)
    (wiki:prepend-text-to-page
     tokens "Test_Page"
     (format nil "~%~%This is a different {{coord dms|||||}} text with a link to the [[Main Page]] (~A).~%~%" (get-universal-time))
     :bot T
     :summary "action summary")))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (let ((tokens (wiki:get-action-tokens "Test_Page")))
    (format t "tokens: ~A~%" tokens)
    (wiki:create-page
     tokens "Some_New_Page"
     (format nil "This is a different text with a link to the [[Main Page]] (~A)." (get-universal-time))
     :summary "action summary")))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (wiki:login "user" "123"))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (wiki:userinfo))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (wiki:get-page-info "Title_Page" ))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (wiki:get-page-content "Title_Page" ))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (wiki:recent-changes))

(wiki:with-mediawiki ("http://localhost:8080/mediawiki")
  (wiki:user-contribs "Victor"))
