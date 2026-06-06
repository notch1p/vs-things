(in-package :bulk-updater)

(named-readtables:in-readtable :interpol-syntax)
(defun default-vs-datapath ()
  #+darwin
  (merge-pathnames "Library/Application Support/VintagestoryData/"
                   (user-homedir-pathname))
  #+linux
  (merge-pathnames ".config/VintagestoryData/"
                   (user-homedir-pathname))
  #+win32
  (merge-pathnames "VintagestoryData/"
                   (concatenate 'string (uiop:getenv "APPDATA") "/"))
  #-(or darwin linux win32)
  (getcwd))

(defparameter *vs-datapath* (default-vs-datapath))

(defun mods-path (&optional host)
  (merge-pathnames
    (if host
        (format nil "ModsByServer/~A/" (substitute #\- #\: host))
        "Mods/")
    *vs-datapath*))

(defparameter *cwd* (mods-path))

(defun get-mods (&optional (path *cwd*))
  (directory-files path "*.zip"))

(defparameter *metadata-file* "modinfo.json")

(defun mtime (path)
  (sb-posix:stat-mtime (sb-posix:stat path)))

(defun get-mod-info (path)
  (zip:with-zipfile
    (zip path)
    (let ((e (get-zipfile-entry *metadata-file* zip)))
      (when e (list
               (jzon:parse (zipfile-entry-contents e)
                           :allow-trailing-comma t
                           :key-fn 'string-downcase)
               (file-namestring path)
               path
               (mtime path))))))

(declaim (inline pick-keys await-map))
(defun pick-keys (hashmap &rest keys)
  (mapcar (lambda (k) (gethash k hashmap)) keys))

(defun await-map (f thread)
  (funcall f (await thread)))

(defun parse-version (s)
  "Parse a version string of the form X.Y.Z[suffix] into the list
  (MAJOR MINOR PATCH SUFFIX). MAJOR MINOR PATCH are integers; SUFFIX is the
  (possibly empty) arbitrary string appended to Z with no separator."
  (let* ((dot1 (position #\. s))
         (dot2 (position #\. s :start (1+ dot1)))
         (rest (subseq s (1+ dot2)))
         (patch-end (or (position-if-not #'digit-char-p rest) (length rest))))
    (list (parse-integer s :end dot1)
          (parse-integer s :start (1+ dot1) :end dot2)
          (parse-integer rest :end patch-end)
          (subseq rest patch-end))))

(defun version> (a b)
  "True when version string A is strictly greater than B. X.Y.Z compare
  numerically; a version with no suffix outranks one with a suffix
  (1.2.3 > 1.2.3-rc1), and two suffixes fall back to STRING>."
  (destructuring-bind (a-maj a-min a-pat a-suf) (parse-version a)
    (destructuring-bind (b-maj b-min b-pat b-suf) (parse-version b)
      (cond ((/= a-maj b-maj) (> a-maj b-maj))
            ((/= a-min b-min) (> a-min b-min))
            ((/= a-pat b-pat) (> a-pat b-pat))
            ((string= a-suf b-suf) nil)
            ((string= a-suf "") t) ; release > prerelease
            ((string= b-suf "") nil)
            (t (and (string> a-suf b-suf) t))))))

(declaim (inline await-modid-version))
(defun await-modid-version (thread) ; manual eta-expansion
  (await-map (lambda (res)
               (destructuring-bind (res filename filepath mtime) res
                 `(,@(pick-keys res "modid" "version") ,filename ,filepath ,mtime)))
             thread))

(defun await-mod-latest-release (thread) ; manual eta-expansion
  (await-map (lambda (res)
               (pick-keys
                   (reduce (lambda (acc x)
                             (let ((modversion-acc (gethash "modversion" acc))
                                   (modversion-x (gethash "modversion" x)))
                               (if (version> modversion-acc modversion-x)
                                   acc
                                   x)))
                       (gethash "releases"
                                (gethash "mod"
                                         (jzon:parse res))))
                 "modidstr" "modversion" "filename" "mainfile"))
             thread))

(defun update-api (mod)
  (declare (inline update-api))
  (uri #?"https://mods.vintagestory.at/api/mod/${mod}"))

(defun build-args (modid-version)
  (destructuring-bind
      (modid version)
      modid-version
    #?"${modid}@${version}"))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
    (loop for key being the hash-keys of object
          using (hash-value value)
          collect (cons key value))))

(defun ordered-map-difference (xs ys &key (test #'equal))
  "compute the map (set) difference of xs ys in key order, with respect to xs.
  That is, the lifted (into the list functor) projection `car' of xs ys must equal.
  Otherwise it does not function properly.
  O(n) for xs ys of the same length, not tailrec."
  (if (or (null xs) (null ys))
      (values nil nil)
      (destructuring-bind (x xs y ys) `(,(car xs) ,(cdr xs) ,(car ys) ,(cdr ys))
        (if (funcall test x y)
            (ordered-map-difference xs ys :test test)
            (multiple-value-bind (diff-xs diff-ys)
                (ordered-map-difference xs ys
                                        :test test)
              (values
                (cons x diff-xs)
                (cons y diff-ys)))))))

(defun diff-mods (xs ys)
  (ordered-map-difference
    xs ys
    :test (lambda (xs ys)
            (destructuring-bind
                ((_x xversion _file _download)
                 (_y yversion _filename _filepath _mtime))
                `(,xs ,ys)
              (declare (ignore _x _y _file _download _filename _filepath _mtime))
              (equal xversion yversion)))))

(defun read-list (file)
  (format t "Reading ~A~%" file)
  (force-output)
  (with-open-file (in file)
    (read in nil nil)))

(defun edit-recipe (recipe)
  (let ((editor (uiop:getenv "EDITOR")))
    (if recipe
        (if (null editor)
            (progn
             (format t "; Update recipe below. Remove an entry if not wish to upgrade that specific mod.~%; Upgradable Mods:~%; ~A~%"
               (mapcar #'car recipe))
             (force-output)
             recipe)
            (uiop:with-temporary-file (:stream s :pathname tmp :type "lisp")
              (format s "; Update recipe below. Remove an entry if not wish to upgrade that specific mod.~%; Upgradable Mods:~%; ~A~%"
                (mapcar #'car recipe))
              (print recipe s)
              (terpri s)
              (force-output s)
              (uiop:run-program (format nil "~A ~S" editor (namestring tmp))
                :input :interactive
                :output :interactive
                :error-output :interactive)
              (read-list tmp)))
        (progn (format t "everything up-to-date, you're good.~%")
               (force-output)))))


(defun lockfile-path (cwd)
  (merge-pathnames #p"package.lock" cwd))

;; only check mtime
(defun lockfilep (lockfile modlist)
  (labels ((iter (xs ys)
                 (cond
                  ((and (null xs) (null ys)) t)
                  ((or (null xs) (null ys)) nil)
                  (t (let ((x (car xs))
                           (xs (cdr xs))
                           (y (car ys))
                           (ys (cdr ys)))
                       (and (eql (fifth x) (mtime y))
                            (iter xs ys)))))))
    (when (probe-file lockfile)
          (let ((modsinfo (read-list lockfile)))
            (and (iter modsinfo modlist)
                 modsinfo)))))

(defun getopt-host (args)
  (loop for (flag value) on args
          when (string= flag "--host")
        do (return (or value (error "--host requires an argument")))))

;; disable debugger so deadlock introduced by a thread failing results in
;; the whole program to crash.
(defun entry-point ()
  (sb-ext:disable-debugger)
  (main :host (getopt-host (uiop:command-line-arguments))))

(defun main (&key host)
  (let* ((*vs-datapath* (default-vs-datapath))
         (*cwd* (mods-path host))
         (mods (get-mods))
         (lockfile (lockfile-path *cwd*))
         (lockfile-test (lockfilep lockfile mods))
         (modsinfo-threads (or lockfile-test (mapcar
                                                 (lambda (path)
                                                   (spawn
                                                     (lambda () (get-mod-info path))
                                                     (pathname-name path)))
                                                 mods)))
         (modsinfo (or lockfile-test (mapcar #'await-modid-version modsinfo-threads))))
    (when (and modsinfo (null lockfile-test))
          (with-open-file (s lockfile
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (format t "~%lockfile stale or nonexistent. Written it to ~A~%" lockfile)
            (prin1 modsinfo s)
            (force-output s)))
    ;    (return-from main)
    (let* ((releases-thread (mapcar
                                (lambda (modinfo)
                                  (spawn (lambda ()
                                           (dex:get (update-api (car modinfo)) :force-binary t))))
                                modsinfo))
           (releases (mapcar #'await-mod-latest-release releases-thread))
           (diff-original nil)
           ;(releases (with-open-file (test "test.txt") (read test)))
           (diff (multiple-value-bind (diff-new diff-old) (diff-mods releases modsinfo)
                   (setf diff-original diff-old)
                   (let ((recipe (edit-recipe diff-new)))
                     (if (and recipe (y-or-n-p "Will upgrade ~&~A~&continue?" (mapcar #'car recipe)))
                         recipe
                         (progn (format t "Aborted.~%") (return-from main))))))
           (downloads (mapcar (lambda (modinfo)
                                (destructuring-bind (modidstr modversion filename mainfile) modinfo ; bind in main
                                  (let ((path (merge-pathnames filename *cwd*)))
                                    (spawn
                                      (lambda ()
                                        (let* ((download-uri (uri mainfile))
                                               (query (quri:uri-query-params download-uri)))
                                          (setf (quri:uri-query-params download-uri) query)
                                          (dex:fetch download-uri path)
                                          (terpri)
                                          (format t "~&Downloaded ~26@A~10A -> ~A~&" modidstr modversion path)
                                          (force-output)
                                          modidstr))))))
                          diff)))
      (let ((inter (intersection diff-original diff
                                 :test (lambda (xs ys) (equal (car xs) (car ys))))))
        (format t "Will delete: ~%~A~%at ~A~%"
          (mapcar #'third inter)
          *cwd*)
        (force-output)
        (let ((res (loop for r in (mapcar #'join-thread downloads)
                           if (failed-p r) do (format *error-output* "~&Skipped: ~a~%" (failed-condition r))
                           else collect r)))
          (dolist (i (intersection inter res :test (lambda (x y) (equal (car x) y))))
            (uiop:delete-file-if-exists (fourth i))
            (format t "Deleted ~A~%" (fourth i))
            (force-output)))))))
