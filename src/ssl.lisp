;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage :cl-dropbox.ssl
  (:use :cl)
  (:export #:ssl-context))

(in-package :cl-dropbox.ssl)

(define-condition ssl-error-call (cl+ssl::ssl-error)
  ((message :initarg :message))
  (:documentation
   "A failure in the SSL library occurred..")
  (:report (lambda (condition stream)
             (format stream "A failure in OpenSSL library occurred~@[: ~A~].~%" (slot-value condition 'message)) (cl+ssl::format-ssl-error-queue stream (cl+ssl::ssl-error-queue condition)))))

(defun ssl-signal-error (&optional message)
  (error 'ssl-error-call :queue (cl+ssl::read-ssl-error-queue) :message message))

(defun add-verify-files% (ssl-ctx files)
  (dolist (file files)
    (let ((namestring (if (pathnamep file) (namestring (truename file)) file)))
      (cffi:with-foreign-strings ((cafile namestring))
        (when (eql 0 (cl+ssl::ssl-ctx-load-verify-locations
                      ssl-ctx
                      cafile
                      (cffi:null-pointer)))
          (ssl-signal-error (format nil "Unable to load cafile ~A" namestring)))))))

(defun add-verify-files (ssl-ctx files)
  (cond
    ((stringp files)
     (add-verify-files% ssl-ctx (list files)))
    ((pathnamep files)
     (add-verify-files% ssl-ctx (list files)))
    (t nil)))

(cffi:defcfun ("SSL_CTX_set_cipher_list" ssl-ctx-set-cipher-list%)
    :int
  (ctx :pointer)
  (ciphers :pointer))

(defun ssl-ctx-set-cipher-list (ctx ciphers)
  (cffi:with-foreign-string (ciphers* ciphers)
    (when (eql 0 (ssl-ctx-set-cipher-list% ctx ciphers*))
      (ssl-signal-error))))


(let ((ssl-ctx))
  (defun ssl-context ()
    "SSL context for Dropbox REST API connections"
    (unless ssl-ctx
      (log:debug "Creating new SSL_CTX")
      ;; calling this explicitly here since
      ;; we want to create context before any call to make-ssl-client-stream
      (handler-bind ((error #'(lambda (c) (setf ssl-ctx nil) (log:error "Error while creating SSL Context: ~A" c))))
        (cl+ssl::ensure-initialized)
        (setf ssl-ctx (cl+ssl::ssl-ctx-new (cl+ssl::ssl-tlsv1-client-method)))
        (cl+ssl::ssl-ctx-set-session-cache-mode ssl-ctx cl+ssl::+ssl-sess-cache-client+)
        (add-verify-files ssl-ctx (merge-pathnames #p"data/trusted-certs.crt" (asdf:component-pathname (asdf:find-system :cl-dropbox))))
        (cl+ssl::ssl-ctx-set-verify-depth ssl-ctx 100)
        (cl+ssl::ssl-ctx-set-verify ssl-ctx cl+ssl::+SSL-VERIFY-PEER+ (cffi:callback cl+ssl::cb-ssl-verify))
        (ssl-ctx-set-cipher-list ssl-ctx (format nil
                                                 "ECDHE-RSA-AES256-GCM-SHA384:~
                                                  ECDHE-RSA-AES256-SHA384:~
                                                  ECDHE-RSA-AES256-SHA:~
                                                  ECDHE-RSA-AES128-GCM-SHA256:~
                                                  ECDHE-RSA-AES128-SHA256:~
                                                  ECDHE-RSA-AES128-SHA:~
                                                  ECDHE-RSA-RC4-SHA:~
                                                  DHE-RSA-AES256-GCM-SHA384:~
                                                  DHE-RSA-AES256-SHA256:~
                                                  DHE-RSA-AES256-SHA:~
                                                  DHE-RSA-AES128-GCM-SHA256:~
                                                  DHE-RSA-AES128-SHA256:~
                                                  DHE-RSA-AES128-SHA:~
                                                  AES256-GCM-SHA384:~
                                                  AES256-SHA256:~
                                                  AES256-SHA:~
                                                  AES128-GCM-SHA256:~
                                                  AES128-SHA256:~
                                                  AES128-SHA"
                                                 ))
        ;; TODO more session handling
        ))
    ssl-ctx))
