;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage :cl-dropbox.ssl
  (:use :cl :cl-dropbox.conditions)
  (:export #:ssl-context))

(in-package :cl-dropbox.ssl)

(let ((ssl-ctx))
  (defun ssl-context ()
    "SSL context for Dropbox REST API connections"
    (unless ssl-ctx
      (log:debug "Creating new SSL_CTX")
      ;; calling this explicitly here since
      ;; we want to create context before any call to make-ssl-client-stream
      (handler-bind ((error #'(lambda (c) (setf ssl-ctx nil) (log:error "Error while creating SSL Context: ~A" c))))
        (cl+ssl::ensure-initialized)
        (setf ssl-ctx (cl+ssl:make-context :verify-location (merge-pathnames #p"data/trusted-certs.crt" (asdf:component-pathname (asdf:find-system :cl-dropbox)))))))
    ssl-ctx))
