(in-package :cl-dropbox-test)

(defun uri-to-string (puri)
  (with-output-to-string  (stream)
    (puri:render-uri puri stream)))

(defmacro with-response ((&key status-code headers body) &body body1)
  `(let ((response  (make-instance 'cl-dropbox::response :status-code ,status-code
                                                        :headers ,headers
                                                        :body ,body)))
    (multiple-value-bind (body metadata) (cl-dropbox::process-response response)
     ,@body1)))
