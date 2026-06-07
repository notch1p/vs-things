(defpackage :bulk-updater.threading
  (:use :cl)
  (:import-from :sb-thread #:make-thread #:join-thread)
  (:export #:spawn #:await #:failed-p #:failed-condition))

(defpackage :bulk-updater
  (:use :cl :bulk-updater.threading)
  ;; SBCL's native threads. Swap for :bordeaux-threads if you want to build
  ;; on another implementation (the make-thread/join-thread names line up).
  (:import-from :sb-thread #:make-thread #:join-thread)
  (:import-from :zip #:with-zipfile #:get-zipfile-entry #:zipfile-entry-contents)
  (:import-from :uiop #:getcwd #:directory-files)
  (:import-from :sb-thread #:make-thread #:join-thread)
  (:import-from :quri #:uri)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export :main :entry-point))

(setf cl-ppcre:*USE-BMH-MATCHERS* t)
