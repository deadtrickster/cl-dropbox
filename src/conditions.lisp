;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(define-condition cl-dropbox-error-base ()
  ()
  (:documentation "Base error class for all cl-dropbox errors"))

(define-condition cl-dropbox-api-error (cl-dropbox-error-base)
  ()
  (:documentation "Base error class for errors returned by Dropbox API servers"))

(define-condition cl-dropbox-error (cl-dropbox-error-base)
  ()
  (:documentation "Base error class for errors occured in cl-dropbox library itself"))
