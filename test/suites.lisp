(in-package :cl-dropbox-test)

(def-suite :cl-dropbox
  :description "Main test suite for CL-DROPBOX")

(def-suite :cl-dropbox.internals :in :cl-dropbox
  :description "CL-DROPBOX internals tests")

(def-suite :cl-dropbox.api :in :cl-dropbox
  :description "CL-DROPBOX API tests")
