;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-user)

(defpackage :cl-dropbox.ssl
  (:use :cl)
  (:export #:ssl-context))

(in-package :cl-dropbox.ssl)

(define-condition ssl-load-vertify-location (cl+ssl::ssl-error)
  ((location :initarg :locations))
  (:report (lambda (condition stream)
             (format stream "Unable to load verify location ~A" (slot-value condition 'location)))))

(defun add-verify-files% (ssl-ctx files)
  (dolist (file files)
    (let ((namestring (if (pathnamep file) (namestring (truename file)) file)))
      (cffi:with-foreign-strings ((cafile namestring))
        (unless (eql 1 (cl+ssl::ssl-ctx-load-verify-locations
                        ssl-ctx
                        cafile
                        (cffi:null-pointer)))
          (error 'ssl-load-vertify-location :location cafile))))))

(defun add-verify-files (ssl-ctx files)
  (cond
    ((stringp files)
     (add-verify-files% ssl-ctx (list files)))
    ((pathnamep files)
     (add-verify-files% ssl-ctx (list files)))
    (t nil)))

(cffi:defcfun ("SSL_CTX_set_cipher_list" ssl-ctx-set-cipher-list)
    :int
  (ctx :pointer)
  (ciphers :pointer))


(let ((ssl-ctx))
  (defun ssl-context ()
    "SSL context for Dropbox REST API connections"
    (unless ssl-ctx
      ;; calling this explicitly here since
      ;; we want to create context before any call to make-ssl-client-stream
      (cl+ssl::ensure-initialized)
      (setf ssl-ctx (cl+ssl::ssl-ctx-new (cl+ssl::ssl-tlsv1-client-method)))
      (cl+ssl::ssl-ctx-set-session-cache-mode ssl-ctx cl+ssl::+ssl-sess-cache-client+)
      (add-verify-files ssl-ctx (merge-pathnames #p"data/trusted-certs.crt"))
      (cl+ssl::ssl-ctx-set-verify-depth ssl-ctx 100)
      (cl+ssl::ssl-ctx-set-verify ssl-ctx cl+ssl::+SSL-VERIFY-PEER+ (cffi:callback cl+ssl::cb-ssl-verify))
      (cffi:with-foreign-string (ciphers (format nil
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
                                                AES128-SHA"))
        (ssl-ctx-set-cipher-list ssl-ctx ciphers)        
        ;; TODO more session handling
        ))
    ssl-ctx))
