;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(cl-interpol:enable-interpol-syntax)

(defclass request ()
  ((host :initarg :host)
   (headers :initform nil :initarg :headers :accessor request-headers)
   (body :initform nil  :initarg :body :accessor request-body)
   (method :initform :get  :initarg :method :accessor request-method)
   (path :initform nil  :initarg :path :accessor request-path)
   (params :initform nil  :initarg :params :accessor request-params)))

(defun escape (str &optional (safe "/"))
  "URI encodes/escapes the given string."
  (with-output-to-string (s)
    (loop for c across (flexi-streams:string-to-octets str :external-format :utf-8)
          do (if (or (find (code-char c) safe)
                     (<= 48 c 57)
                     (<= 65 c 90)
                     (<= 97 c 122)
                     (find c '(45 95 46 126)))
              (write-char (code-char c) s)
              (format s "%~2,'0x" c)))))

(defmethod request-url ((request request))
  (make-instance 'puri:uri :scheme :https
                           :host (slot-value request 'host)
                           :path (escape (if (listp (request-path request))
                                             (let ((cl-interpol:*list-delimiter* #\/))
                                               #?"/${+api-version+}/@{(request-path request)}")
                                             #?"/${+api-version+}/${(request-path request)}"))))

(defgeneric request-url (request))

(defclass api-request (request)
  ()
  (:default-initargs :host +api-server+))

(defclass api-content-request (request)
  ()
  (:default-initargs :host +api-content-server+))

(defclass api-notify-request (request)
  ()
  (:default-initargs :host +api-notify-server+ :method :get))

(defmethod request-requires-signature ((request request))
  t)

(defmethod request-requires-signature ((request api-notify-request))
  nil)


(defun header-value (response name)
  (drakma:header-value name (response-headers response)))

(defun (setf header-value) (value request name)
  ;;check if header with name already presented, if true change its value otherwise add new header
  (let ((header (assoc name (request-headers request) :test #'string-equal)))
    (if header
        (rplacd header value)
        (setf (request-headers request) (acons name value (request-headers request))))))

(defun http-request-with-ssl (request &key want-stream)
  ;; do not replace cl+ssl global state, rebind
  ;; connections cache not implemented yet so :close t
  (let* ((cl+ssl::*ssl-global-context* (ssl-context))
         (cl+ssl::*ssl-check-verify-p* t))
    (drakma:http-request (request-url request) :method (request-method request)
                                               :parameters (request-params request)
                                               :content (request-body request)
                                               :additional-headers (request-headers request)
                                               :user-agent +user-agent+
                                               :content-type nil
                                               :want-stream want-stream)))

(defun ensure-string (response)
  (if (stringp response)
      response
      (trivial-utf-8:utf-8-bytes-to-string response)))


(defclass response ()
  ((status-code :initarg :status-code :reader response-status-code)
   (headers :initarg :headers :reader response-headers)
   (body :initarg :body :reader response-body)))

(defun http-request% (request &key want-stream)
  (multiple-value-bind (response status-code headers)
      (http-request-with-ssl request :want-stream want-stream)
    (if (functionp response)
        (lambda (content &optional continue)
          (if continue
              (funcall response content t)
              (multiple-value-bind (response status-code headers)
                  (funcall response content nil)
                (process-response (make-instance 'response :body response :headers headers :status-code status-code)))))
        (make-instance 'response :body response :headers headers :status-code status-code))))
