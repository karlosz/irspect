;; -*- mode: lisp -*-

(in-package :cl-user)

(asdf:defsystem :irspect
    :serial t
    :depends-on (:mcclim :clouseau)
    :components
    ((:file "package")
     (:file "irspect")))
