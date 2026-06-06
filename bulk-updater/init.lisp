#+sbcl (setq *block-compile-default* t)

(defun relative (path)
  (merge-pathnames path (uiop:getcwd)))

(defun try-load (paths sys)
  (if (null paths) (error "bulk-update.asd not found in all searchpaths")
      (let ((p (merge-pathnames sys (car paths))))
        (if (probe-file p)
            (asdf:load-asd p)
            (try-load (cdr paths) sys)))))

(try-load (mapcar 'relative '("" "bulk-updater/")) "bulk-updater.asd")
(ql:quickload :bulk-updater)
