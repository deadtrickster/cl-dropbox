;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(defpackage :cl-dropbox-test-system
  (:use :cl :asdf))

(in-package :cl-dropbox-test-system)

(defsystem :cl-dropbox-test
  :description "Tests for Dropbox Core SDK in Common Lisp"
  :license "MIT"
  :author "Ilya Khaprov"
  :version "0.1.0"
  :depends-on (:cl-dropbox
               :alexandria
               :fiveam)
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "util")
     (:file "suites")
     (:file "internals")
     (:file "api")))))
