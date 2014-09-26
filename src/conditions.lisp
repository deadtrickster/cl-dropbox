;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage :cl-dropbox.conditions
  (:use :cl)
  (:export #:trusted-certificates-file-not-found))

(in-package :cl-dropbox.conditions)

(define-condition cl-dropbox-error-base (error)
  ()
  (:documentation "Base error class for all cl-dropbox errors"))

(define-condition cl-dropbox-api-error (cl-dropbox-error-base)
  ()
  (:documentation "Base error class for errors returned by Dropbox API servers"))

(define-condition cl-dropbox-error (cl-dropbox-error-base)
  ()
  (:documentation "Base error class for errors occured in cl-dropbox library itself"))

(define-condition trusted-certificates-file-not-found (cl-dropbox-error)
  ((pathname :initarg :pathname))
  (:documentation "Unable to find trusted certificates. Used as cafile")
  (:report (lambda (condition stream)
             (format stream "Unable to find trusted certificates. Path: ~A" (slot-value condition 'pathname)))))
