;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(defmacro define-constant (name value &optional documentation)
  `(unless (boundp ',name)
     (defconstant ,name ,value documentation)))

(define-constant +api-server+ "api.dropbox.com")
(define-constant +api-content-server+ "api-content.dropbox.com")
(define-constant +web-server+ "www.dropbox.com")

(define-constant +api-version+ 1)

(define-constant +user-agent+ (concatenate 'string "CL-DROPBOX/" (asdf:component-version (asdf:find-system :cl-dropbox))))
