(in-package #:asdf-user)

(defsystem :cl-mediawiki-test
  :description "The test suite for the cl-mediawiki system."
  :components ((:module :tests
                :serial T
                :components ((:file "setup")
                             (:file "query" )
                             (:file "edit"))))
  ;; Additional Functionality will be loaded if cl-ppcre is in
  ;; the features list during compilation
  :depends-on (:cl-mediawiki :lisp-unit2))
