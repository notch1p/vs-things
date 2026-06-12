(in-package :bulk-updater)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; mutually recursive
  (defun list-of-vsmod-p (l) (every #'vsmod-p l))

  (deftype list-of-vsmods ()
    `(and list (satisfies list-of-vsmod-p)))

  (defstruct (vsmod (:constructor mkmod (modid ver filename path mtime &key deps)))
    (modid "" :type string)
    (ver "" :type string)
    (filename "" :type string)
    (path #p"" :type (or pathname quri:uri-http))
    (mtime 0 :type integer)
    (deps nil :type list-of-vsmods)))
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-at (n l)
    (cond ((null l) (values l nil))
          ((zerop n) (values nil l))
          ((consp l) (multiple-value-bind (hd rst) (split-at (1- n) (cdr l))
                       (values (cons (car l) hd) rst)))))

  (defun m-reader (s _subchar _arg)
    (declare (ignore _subchar _arg))
      (multiple-value-bind (hd deps) (split-at 5 (read s t nil t))
        `(mkmod ,@hd :deps (list ,@deps))))

  (set-dispatch-macro-character #\# #\m #'m-reader))

(defmethod print-object ((m vsmod) stream)
  (with-slots (modid ver filename path mtime deps) m
    (format stream "#M(~s ~s ~s ~s ~s ~s)"
      modid ver filename path mtime deps)))