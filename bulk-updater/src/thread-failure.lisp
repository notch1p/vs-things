(in-package :bulk-updater.threading)

(defstruct (failed
            (:constructor fail (condition)))
  condition)

(defun spawn (thunk &optional name)
  "Like MAKE-THREAD but captures conditions, marking the respective thread as FAILED."
  (make-thread
    (lambda ()
      (handler-case (funcall thunk)
        (serious-condition (c) (fail c))))
    :name name))

(defun await (thread)
  "Like JOIN-THREAD but resignals the condition captured by SPAWN in the *current* thread"
  (let ((result (join-thread thread)))
    (if (failed-p result)
        (error (failed-condition result))
        result)))