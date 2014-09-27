;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(cl-interpol:enable-interpol-syntax)

(defclass request ()
  ((headers :initarg :headers :accessor request-headers)
   (body :initarg :body :accessor request-body)
   (method :initarg :method :accessor request-method)
   (path :initarg :uri :accessor request-path)
   (params :initarg :query :accessor request-params)))

(defgeneric request-url (request))

(defclass api-request (request)
  ())

(defmethod request-url ((request api-request))
  (make-instance 'puri:uri :scheme "https"
                           :host +api-server+
                           :path #?"#{+dropbox-api-version+}/#{request-path}"))

(defclass content-request (request)
  ())

(defmethod request-url ((request content-request))
  (make-instance 'puri:uri :scheme "https"
                           :host +api-content-server+
                           :path #?"#{+api-version+}/#{request-path}"))

(defun add-header (request name value)
  (setf (request-headers request) (acons name value (request-headers request))))

(defclass response ()
  ((status-code :initarg :status-code :reader response-status-code)
   (headers :initarg :headers :reader response-headers)
   (body :initarg :body :reader response-body)))

(defun response-header (response name)
  (drakma:header-value name (response-headers response)))

(defun http-request-with-ssl (request)
  ;; do not replace cl+ssl global state, rebind
  ;; connections cache not implemented yet so :close t
  (let* ((cl+ssl::*ssl-global-context* (ssl-context))
         (cl+ssl::*ssl-check-verify-p* t))
    (drakma:http-request (request-url request) :method (request-method request)
                                                       :parameters (request-params request)
                                                       :content (request-body request)
                                                       :additional-headers (request-headers request)
                                                       :user-agent +user-agent+)))

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

(defun http-request% (request)
  (multiple-value-bind (response status-code headers)
      (http-request-with-ssl request)
    (values (ensure-string-and-success response status-code headers) headers)))

(defun http-request (request)
  (let ((retries 0))
    (tagbody
     :retry
       (handler-bind ((cl-dropbox-transient-server-error (lambda (c)
                                                           (log:error c)
                                                           (when (< retries *max-dropbox-server-error-retries*) (incf retries) (go :retry))))
                      (error (lambda (c)
                               (log:error c))))
         (http-request% request)))))
