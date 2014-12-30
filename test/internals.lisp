(in-package :cl-dropbox-test)

(in-suite :cl-dropbox.internals)

(test (escape-url-test :compile-at :definition-time)
  (is-true (equal (cl-dropbox::escape "https://github.com/deadtrickster/cl-dropbox")
                  "https%3A//github.com/deadtrickster/cl-dropbox"))
  (is-true (equal (cl-dropbox::escape "https://github.com/deadtrickster/cl-dropbox" "")
                  "https%3A%2F%2Fgithub.com%2Fdeadtrickster%2Fcl-dropbox"))  
  (is-true (equal (cl-dropbox::escape "https://github.com/deadtrickster/cl-dropbox" ":/")
                  "https://github.com/deadtrickster/cl-dropbox"))
  (is-true (equal (cl-dropbox::escape "&<>\"!@#$%^*[] ")
            "%26%3C%3E%22%21%40%23%24%25%5E%2A%5B%5D%20")))

(test (request-url-test :compile-at :definition-time)
  (is-true (equal (uri-to-string
                   (cl-dropbox::request-url (make-instance 'cl-dropbox::api-request :path "cl-plus-ssl/cl-plus-ssl/pull/8")))
                  "https://api.dropbox.com/1/cl-plus-ssl/cl-plus-ssl/pull/8"))

  (is-true (equal (uri-to-string
                   (cl-dropbox::request-url (make-instance 'cl-dropbox::api-request :path '("Backup" "My Music" "QEWQ_Wfwef!"))))
                  "https://api.dropbox.com/1/Backup/My%20Music/QEWQ_Wfwef%21"))

  (is-true (equal (uri-to-string
                   (cl-dropbox::request-url (make-instance 'cl-dropbox::api-content-request :path "cl-plus-ssl/cl-plus-ssl/pull/8")))
                  "https://api-content.dropbox.com/1/cl-plus-ssl/cl-plus-ssl/pull/8"))

  (is-true (equal (uri-to-string
                   (cl-dropbox::request-url (make-instance 'cl-dropbox::api-content-request :path '("Backup" "My Music" "QEWQ_Wfwef!"))))
                  "https://api-content.dropbox.com/1/Backup/My%20Music/QEWQ_Wfwef%21"))

 (is-true (equal (uri-to-string
                   (cl-dropbox::request-url (make-instance 'cl-dropbox::api-notify-request :path "cl-plus-ssl/cl-plus-ssl/pull/8")))
                  "https://api-notify.dropbox.com/1/cl-plus-ssl/cl-plus-ssl/pull/8"))

  (is-true (equal (uri-to-string
                   (cl-dropbox::request-url (make-instance 'cl-dropbox::api-notify-request :path '("Backup" "My Music" "QEWQ_Wfwef!"))))
                  "https://api-notify.dropbox.com/1/Backup/My%20Music/QEWQ_Wfwef%21")))

(test (response-conditions-test :compile-at :definition-time)
  (with-response (:status-code 200
                  :headers '(("content-type" . "text/json"))
                  :body "{qwe: \"qwe\"}")
    (is-true (null metadata))
    (is-true (equalp (hash-table-alist body) '(("qwe" . "qwe")))))

  (with-response (:status-code 200
                  :headers '(("content-type" . "text/json")
                             ("x-dropbox-metadata" . "{qwe: \"qwe\"}"))
                  :body "{qwe: \"qwe\"}")
    (is-true (equalp (hash-table-alist metadata) '(("qwe" . "qwe"))))
    (is-true (equalp (hash-table-alist body) '(("qwe" . "qwe")))))

  ;; errors
  (signals cl-dropbox.conditions:cl-dropbox-api-bad-input
    (with-response (:status-code 400)))
  (signals cl-dropbox.conditions:cl-dropbox-api-unauthorized
    (with-response (:status-code 401)))
  (signals cl-dropbox.conditions:cl-dropbox-api-forbidden
    (with-response (:status-code 403)))
  (signals cl-dropbox.conditions:cl-dropbox-api-notfound
    (with-response (:status-code 404)))
  (signals cl-dropbox.conditions:cl-dropbox-api-method-not-expected
    (with-response (:status-code 405)))
  (signals cl-dropbox.conditions:cl-dropbox-api-too-many-requests
    (with-response (:status-code 429)))
  (signals cl-dropbox.conditions:cl-dropbox-api-transient-server-error
    (with-response (:status-code 503)))
  (signals cl-dropbox.conditions:cl-dropbox-api-oauth1.0-rate-limited
    (with-response (:status-code 503
                    :headers '(("retry-after" . 200))))))
