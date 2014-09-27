;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(defun http-request-with-ssl (uri drakma-args)
  ;; do not replace cl+ssl global state, rebind
  (let* ((cl+ssl::*ssl-global-context* (ssl-context))
         (cl+ssl::*ssl-check-verify-p* t))
    (apply #'drakma:http-request uri drakma-args)))

(defun ensure-string (response)
  (if (stringp response)
      response
      (trivial-utf-8:utf-8-bytes-to-string response)))

(defun ensure-string-and-success (response status-code headers)
  (let ((response-string (ensure-string response))
        (ret))
    (cond
      ((and (>= 200 status-code)
            (< 300 status-code))
       (setf ret response))
      ((= status-code 400)
       (setq ret 'cl-dropbox-api-bad-input))
      ((= status-code 401)
       (setq ret 'cl-dropbox-api-unauthorized))
      ((= status-code 403)
       (setq ret 'cl-dropbox-api-forbidden))
      ((= status-code 404)
       (setq ret 'cl-dropbox-api-notfound))
      ((= status-code 405)
       (setq ret 'cl-dropbox-api-method-not-expected))
      ((= status-code 429)
       (setq ret 'cl-dropbox-api-too-many-requests))
      ((= status-code 503)
       (if (drakma:header-value :retry-after headers)
           (setq ret 'cl-dropbox-api-oauth1.0-rate-limited)
           (setq ret 'cl-dropbox-api-transient-server-error)))
      ((= status-code 507)
       (setq ret 'cl-dropbox-api-over-quota))
      ((and (>= status-code 500) (< status-code 600))
       (setq ret 'cl-dropbox-api-server-error))
      (t 'cl-dropbox-api-error-non-standard))
    (if (symbolp ret)
        (error ret :status-code status-code :response response-string :headers headers)
        response-string)))

(defun http-request% (uri drakma-args)
  ;; connections cache not implemented yet so :close t
  (multiple-value-bind (response status-code headers)
      (http-request-with-ssl uri (append drakma-args (list :close t)))
    (values (ensure-string-and-success response status-code headers) headers)))

(defun http-request (uri &rest drakma-args)
  (let ((retries 0))
    (tagbody
     :retry
       (handler-bind ((cl-dropbox-transient-server-error (lambda (c)
                                                           (log:error c)
                                                           (when (< retries *max-dropbox-server-error-retries*) (incf retries) (go :retry))))
                      (error (lambda (c)
                               (log:error c))))
         (http-request% uri drakma-args)))))
