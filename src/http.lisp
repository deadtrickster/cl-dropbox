;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(cl-interpol:enable-interpol-syntax)

(defclass request ()
  ((headers :initform nil :initarg :headers :accessor request-headers)
   (body :initform nil  :initarg :body :accessor request-body)
   (method :initform :get  :initarg :method :accessor request-method)
   (path :initform nil  :initarg :path :accessor request-path)
   (params :initform nil  :initarg :params :accessor request-params)))

(defclass response ()
  ((status-code :initarg :status-code :reader response-status-code)
   (headers :initarg :headers :reader response-headers)
   (body :initarg :body :reader response-body)))


(defgeneric request-url (request))

(defclass api-request (request)
  ())

(defmethod request-url ((request api-request))
  (make-instance 'puri:uri :scheme :https
                           :host +api-server+
                           :path #?"/${+api-version+}/${(request-path request)}"))

(defclass content-request (request)
  ())

(defmethod request-url ((request content-request))
  (make-instance 'puri:uri :scheme :https
                           :host +api-content-server+
                           :path #?"/${+api-version+}/${(request-path request)}"))


(defun header-value (response name)
  (drakma:header-value name (response-headers response)))

(defun (setf header-value) (value request name)
  ;;check if header with name already presented, if true change its value otherwise add new header
  (let ((header (assoc name (request-headers request) :test #'string-equal)))
    (if header
        (rplacd header value)
        (setf (request-headers request) (acons name value (request-headers request))))))

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

(defun http-request% (request)
  (multiple-value-bind (response status-code headers)
      (http-request-with-ssl request)
    (make-instance 'response :body response :headers headers :status-code status-code)))


