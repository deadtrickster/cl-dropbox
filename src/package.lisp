;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage :cl-dropbox
  (:use :common-lisp :cl-dropbox.ssl :cl-dropbox.conditions)
  (:nicknames :dropbox)
  (:shadow #:search
           #:delete)
  (:export #:dropbox
           #:put-file
           #:put-stream
           #:put-url
           #:get-file
           #:metadata
           #:delta
           #:latest-cursor
           #:longpool-delta
           #:revisions
           #:restore
           #:search
           #:get-share-link
           #:get-direct-link
           #:get-copy-ref
           #:get-thumbnail
           #:get-preview
           #:copy
           #:create-folder
           #:delete
           #:move))
