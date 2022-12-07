;;;; proof-validator.asd

(asdf:defsystem #:proof-validator
  :description "Validates proofs of statement logic"
  :author "Kenneth Sun<kennett.sun@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:cl-ppcre #:uiop)
  :components ((:file "package")
               (:file "proof-validator")))
