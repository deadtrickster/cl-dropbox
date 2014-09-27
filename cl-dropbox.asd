;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(defpackage :cl-dropbox-system
  (:use :cl :asdf))

(in-package :cl-dropbox-system)

(defsystem :cl-dropbox
  :description "Dropbox Core SDK in Common Lisp"
  :license "MIT"
  :author "Ilya Khaprov"
  :version "0.0.1" ;; freeze version for now
  :depends-on (:drakma :log4cl :trivial-utf-8 :cl-interpo :ia-hash-table :yason)
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "conditions")
     (:file "ssl")
     (:file "package")
     (:file "constants")
     (:file "parameters")
     (:file "http")
     (:file "dropbox")))))
