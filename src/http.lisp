;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(defun http-request-with-ssl (drakma-args)
  ;; do not replace cl+ssl global state, rebind
  (let ((cl+ssl::*ssl-global-context* (ssl-context))
        ;; hostname checks
        (cl+ssl::*ssl-check-verify-p* t))
    (apply #'drakma:http-request drakma-args)))
