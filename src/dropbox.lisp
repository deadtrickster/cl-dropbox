;;; Copyright (C) 2014  Ilya Khaprov https://github.com/deadtrickster
;;;
;;; See LICENSE for details.

(in-package :cl-dropbox)

(defclass Dropbox ()
  ((access-token :initarg :access-token :reader access-token)))

(defmethod dropbox-sign-request ((dropbox dropbox) request)
  (setf (header-value request "Authorization") (concatenate 'string "Bearer " (access-token dropbox))))


(defvar *dropbox*) ;; current dropbox session


(defun http-request (request)
  (let ((retries 0)
        (response))
    (dropbox-sign-request *dropbox* request)
    (tagbody
     :retry
       (handler-bind ((cl-dropbox-api-transient-server-error (lambda (c)
                                                               (log:error c)
                                                               (when (< retries *max-dropbox-server-error-retries*) (incf retries) (go :retry))))
                      (error (lambda (c)
                               (log:error c))))
         (setf response (process-response (http-request% request)))))
    response))

(defun process-response (response)
  (let* ((response-string (ensure-string (response-body response)))
         (response-json (json:parse response-string))
         (status-code (response-status-code response))
         (error-type))
    (setf (slot-value response 'body) (or response-json response-string))
    (cond
      ((and (>= 200 status-code)
            (< 300 status-code))
       (setf error-type response))
      ((= status-code 400)
       (setq error-type 'cl-dropbox-api-bad-input))
      ((= status-code 401)
       (setq error-type 'cl-dropbox-api-unauthorized))
      ((= status-code 403)
       (setq error-type 'cl-dropbox-api-forbidden))
      ((= status-code 404)
       (setq error-type 'cl-dropbox-api-notfound))
      ((= status-code 405)
       (setq error-type 'cl-dropbox-api-method-not-expected))
      ((= status-code 429)
       (setq error-type 'cl-dropbox-api-too-many-requests))
      ((= status-code 503)
       (if (header-value response :retry-after)
           (setq error-type 'cl-dropbox-api-oauth1.0-rate-limited)
           (setq error-type 'cl-dropbox-api-transient-server-error)))
      ((= status-code 507)
       (setq error-type 'cl-dropbox-api-over-quota))
      ((and (>= status-code 500) (< status-code 600))
       (setq error-type 'cl-dropbox-api-server-error))
      (t 'cl-dropbox-api-error-non-standard))
    (if error-type
        (error error-type :response response)
        (response-body response))))


(defun account-info (&optional (locale "en"))
  (http-request (make-instance 'api-request :path "account/info"
                                            :params `(("locale" . ,locale)))))
