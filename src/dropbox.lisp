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
  (let ((retries 0))
    (when (request-requires-signature request)
      (dropbox-sign-request *dropbox* request))
    (prog ()
     :retry
       (handler-bind ((cl-dropbox-api-transient-server-error (lambda (c)
                                                               (log:error c)
                                                               (when (< retries *max-dropbox-server-error-retries*) (incf retries) (go :retry))))
                      (error (lambda (c)
                               (log:error c))))
         (return (process-response (http-request% request)))))))

(defun process-response (response)
  (when (and (header-value response "content-type")
             (or
              (alexandria:starts-with-subseq "text/" (header-value response "content-type"))
              (equal "application/json" (header-value response "content-type"))))
    (setf (slot-value response 'body) (ensure-string (response-body response)))
    (if (or (equal "application/json" (header-value response "content-type"))
            (equal "text/javascript" (header-value response "content-type")))
        (setf (slot-value response 'body) (yason:parse (response-body response)))
        ;; still try to handle text/json and other zoo animals
        (alexandria:if-let ((json (ignore-errors (yason:parse (response-body response)))))
          (setf (slot-value response 'body) json))))
  (let* ((status-code (response-status-code response))
         (error-type))
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
        (alexandria:if-let ((x-metadata (header-value response "x-dropbox-metadata")))
          (values (response-body response) (yason:parse x-metadata))
          (response-body response)))))

(defun expand-parameters (parameters)
  (loop for parameter in parameters
        as parameter-name = (if (consp parameter) (car parameter) parameter)
        as parameter-default = (if (consp parameter) (cdr parameter) nil)
        collect `(,(symbol-munger:underscores->lisp-symbol parameter-name)
                  ,parameter-name
                  ,parameter-default
                  ,(symbol-munger:underscores->lisp-symbol (concatenate 'string parameter-name "-supplied-p")))))

(defun build-api-call-key-args (parameters)
  (loop for parameter in parameters
        collect `(,(first parameter)
                  ,(third parameter)
                  ,(fourth parameter))))

(defun generate-parameters-collector (key-args)
  (loop for key-arg in key-args
        collect
           `(if ,(fourth key-arg)
                (setf parameters (acons ,(second key-arg) ,(first key-arg) parameters)))))

(defmacro define-api-call ((name required-args parameters) &body body)
  (let* ((parameters (expand-parameters parameters))
         (key-args (build-api-call-key-args parameters)))
    `(defun ,name (,@required-args &key ,@key-args)
       (let ((parameters))
         ,(unless parameters '(declare (ignorable parameters)))
         ,@(generate-parameters-collector parameters)
         ,@body))))

(defun disable-access-token ()
  (http-request (make-instance 'api-request :path "disable_access_token"
                                            :method :post)))

(define-api-call (get-account-info () ("locale"))
  (http-request (make-instance 'api-request :path "account/info"
                                            )))

(define-api-call (get-file (path) ("rev"))
  (http-request (make-instance 'api-content-request :path (list "files/auto" path)
                                                    :params parameters)))

(define-api-call (metadata (path) ("file_limit"
                                   "hash"
                                   "list"
                                   "include_deleted"
                                   "rev"
                                   "locale"
                                   "include_media_info"))
  (http-request (make-instance 'api-request :path (list "metadata/auto" path)
                                            :params parameters)))

(define-api-call (delta (cursor) ("path_prefix"
                                  "locale"
                                  "include_media_info"))
  (http-request (make-instance 'api-request :path "delta"
                                            :method :post
                                            :params (acons "cursor" cursor parameters))))

(define-api-call (latest-cursor () ("path_prefix"
                                    "include_media_info"))
  (http-request (make-instance 'api-request :path "delta/latest_cursor"
                                            :method :post
                                            :params parameters)))

(define-api-call (longpool-delta (cursor) ("timeout"))
  (http-request (make-instance 'api-notify-request :path "longpool_delta"
                                                   :params (acons "cursor" cursor parameters))))

(define-api-call (revisions (path) ("rev_limit"
                                    "locale"))
  (http-request (make-instance 'api-request :path (list "revisions/auto" path)
                                            :params parameters)))

(define-api-call (restore (path) ("rev"
                                  "locale"))
  (http-request (make-instance 'api-request :path (list "restore/auto" path)
                                           :params parameters)))

(define-api-call (search (path query) ("file_limit"
                                       "include_deleted"
                                       "locale"))
  (http-request (make-instance 'api-request :path (list "search/auto" path)
                                           :method :post
                                           :params (acons "query" query parameters))))

(define-api-call (get-share-link (path) ("short_url"
                                         "locale"))
  (http-request (make-instance 'api-request :path (list "shares/auto" path)
                                            :method :post
                                            :params parameters)))

(define-api-call (get-direct-link (path) ("locale"))
  (http-request (make-instance 'api-request :path (list "media/auto" path)
                                            :method :post
                                            :params parameters)))

(defstruct copy-ref
  (ref)
  (expires))

(define-api-call (get-copy-ref (path) ())
  (let ((response
          (http-request (make-instance 'api-request :path (list "copy_ref/auto" path)))))
    (make-copy-ref :ref  (gethash "copy_ref" (response-body response))
                   :expires (gethash "expires" (response-body response)))))

(define-api-call (get-thumbnail (path) ("format" "size"))
  (http-request (make-instance 'api-content-request :path (list "thumbnails/auto" path)
                                                    :params parameters)))

(define-api-call (get-preview (path) ("rev"))
  (http-request (make-instance 'api-content-request :path (list "previews/auto" path)
                                                    :params parameters)))

(define-api-call (copy (from to-path) (("root" . "auto")
                                       "locale"))
  (http-request (make-instance 'api-request :path "fileops/copy"
                                            :method :post
                                            :params (acons "to_path" to-path
                                                           (if (copy-ref-p from)
                                                               (acons  "from_copy_ref" (copy-ref-ref from) parameters)
                                                               (acons "from_path" from parameters))))))

(define-api-call (create-folder (path) (("root" . "auto")
                                        "locale"))
  (http-request (make-instance 'api-request :path "fileops/create_folder"
                                            :method :post
                                            :params (acons "path" path parameters))))

(define-api-call (delete (path) (("root" . "auto")
                                 "locale"))
  (http-request (make-instance 'api-request :path "fileops/delete"
                                            :method :post
                                            :params (acons "path" path parameters))))

(define-api-call (move (from-path to-path) (("root" . "auto")
                                            "locale"))
  (http-request (make-instance 'api-request :path "fileops/move"
                                            :method :post
                                            :params (acons "to_path" to-path
                                                           (acons "from_path" from-path parameters)))))
