(in-package :bulk-updater)

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

(defmacro select-keys (hashmap &rest keys)
  (reduce (lambda (acc k) `(gethash ,k ,acc)) keys :initial-value hashmap))

(defun await-map (f thread)
  (funcall f (await thread)))

;; simplified semver ::= x.y.z[-(rc|dev|pre)[.w]]
(defparameter semver-regex (cl-ppcre:create-scanner
                             "^(\\d*)\\.(\\d*)\\.(\\d*)(?:-(rc|dev|pre)(?:\\.(\\d*))?)?$"
                             :single-line-mode t))

(defun parse-version (v)
  (cl-ppcre:register-groups-bind ((#'parse-integer x y z) pre (#'parse-integer prenum))
    (semver-regex v)
    :sharedp t
    (list x y z pre prenum)))

(defun version> (a b)
  "a release outranks a prerelease of the same
core (1.2.3 > 1.2.3-rc.1); same tag compares by its number (rc.2 > rc.1, a
missing number counting as 0); differing tags has ordering rc > pre > dev."
  (destructuring-bind (a-maj a-min a-pat a-pre a-prenum) (parse-version a)
    (destructuring-bind (b-maj b-min b-pat b-pre b-prenum) (parse-version b)
      (cond ((/= a-maj b-maj) (> a-maj b-maj))
            ((/= a-min b-min) (> a-min b-min))
            ((/= a-pat b-pat) (> a-pat b-pat))
            ((and (null a-pre) (null b-pre)) nil) ; equal releases
            ((null a-pre) t) ; release > prerelease
            ((null b-pre) nil) ; prerelease < release
            ((string= a-pre b-pre) (> (or a-prenum 0) (or b-prenum 0)))
            (t (and (string> a-pre b-pre) t)))))) ; r > p > d is handled directly

(declaim (inline await-modid-version))
(defun await-modid-version (thread) ; manual eta-expansion
  (await-map (lambda (res)
               (destructuring-bind (res filename filepath mtime) res
                 `(,@(pick-keys res "modid" "version") ,filename ,filepath ,mtime)))
             thread))

(defun await-mod-latest-release (thread) ; manual eta-expansion
  (await-map
    (lambda (res)
      (pick-keys
          (reduce (lambda (acc x)
                    (let ((modversion-acc (gethash "modversion" acc))
                          (modversion-x (gethash "modversion" x)))
                      (if (version> modversion-acc modversion-x) acc x)))
              (select-keys (jzon:parse res) "mod" "releases"))
        "modidstr" "modversion" "filename" "mainfile"))
    thread))

(declaim (inline (update-api build-args)))
(defun update-api (mod)
  (uri (format nil "https://mods.vintagestory.at/api/mod/~A" mod)))

(defun build-args (modid version)
  (format nil "~A@~A" modid version))

(defun build-args-modsinfo (modsinfo)
  (build-args (car modsinfo) (cadr modsinfo)))

#+nil
(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
    (loop for key being the hash-keys of object
          using (hash-value value)
          collect (cons key value))))

(defun ordered-difference (xs ys &key (test #'equal) (key #'identity))
  "compute the map (set) difference of xs ys in key order, with respect to xs.
  That is, the lifted (into the list functor) projection `nth' (i.e. the nth column) of xs ys must equal.
  Otherwise it does not function properly.
  O(n) for xs ys of the same length, not tailrec."
  (if (or (null xs) (null ys))
      (values nil nil)
      (destructuring-bind (x xs y ys) `(,(car xs) ,(cdr xs) ,(car ys) ,(cdr ys))
        (if (funcall test (funcall key x) (funcall key y))
            (ordered-difference xs ys :test test :key key)
            (multiple-value-bind (diff-xs diff-ys)
                (ordered-difference xs ys :test test :key key)
              (values
                (cons x diff-xs)
                (cons y diff-ys)))))))

(defun ordered-intersection (diff-old diff &key (test #'eql) (key #'identity))
  "intersection on ordered set with respect to DIFF-OLD. 
  Requirements same as UPDATE-ORDERED-MODSINFO."
  (cond ((or (null diff-old) (null diff)) nil)
        (t (let ((diff-old-1 (car diff-old))
                 (diff-1 (car diff)))
             (if (funcall test (funcall key diff-old-1) (funcall key diff-1))
                 (cons diff-old-1 (ordered-intersection (cdr diff-old) (cdr diff) :test test :key key))
                 (ordered-intersection (cdr diff-old) diff :test test :key key))))))

(declaim (inline diff-version inter-id))
(defun diff-version (xs ys)
  (ordered-difference xs ys :test #'equal :key #'cadr))
(defun inter-id (diff-old diff)
  (ordered-intersection diff-old diff :test #'equal :key #'car))

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
              (format s
                  "; Update recipe below. Remove an entry if not wish to upgrade that specific mod.~%")
              (format s
                  "; Do NOT change the ordering of list; New mods go to the bottom of the list.~%")
              (format s
                  "; Upgradable Mods:~%; ~{~A~^, ~}~%~%"
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
          return (or value (warn 'arg-not-fullfilled/applicable :arg "--host" :param value))))

(defun entry-point ()
  ; disable debugger so deadlock introduced by a thread failing results in
  ; the whole program to crash.
  (sb-ext:disable-debugger)
  (main :host (getopt-host (uiop:command-line-arguments))))

(defun update-ordered-modsinfo (modsinfo res-succeed)
  "updates entries according to RES-SUCCEED in order with respect to MODSINFO.
  Consider the strict poset consisting of MODSINFO's MODID (car modsinfo),
  we require that of RES-SUCCEED to be a subset of MODSINFO's.
  A valid usage looks like 
    - modsinfo    '(m1 m2 m3 m4 m5)
    - res-succeed '(m1 m3 m5)"
  (cond ((null modsinfo) res-succeed) ; res-succeed = nil/newly installed mods.
        ((null res-succeed) modsinfo)
        (t (let ((old-mod (car modsinfo))
                 (res-mod (car res-succeed)))
             (if (equal (car old-mod) (car res-mod))
                 (destructuring-bind (modidstr modversion filename path) res-mod
                   (format t "+~22@A ~12A -> ~A~&" modidstr (cadr old-mod) modversion)
                   (cons `(,modidstr ,modversion ,filename ,path ,(mtime path)) (update-ordered-modsinfo (cdr modsinfo) (cdr res-succeed))))
                 (cons (car modsinfo) (update-ordered-modsinfo (cdr modsinfo) res-succeed)))))))

(defun main (&key host)
  (let* ((*vs-datapath* (default-vs-datapath))
         (*cwd* (or (probe-file (mods-path host))
                    (error "path ~A does not exist" (mods-path host))))
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
            (format t "lockfile stale or nonexistent. Written it to ~A~%" lockfile)
            (prin1 modsinfo s)
            (force-output s)))
    ; (print (mapcar #'build-args-modsinfo modsinfo))
    ; (return-from main)
    (let* ((releases-thread (mapcar
                                (lambda (modinfo)
                                  (spawn (lambda ()
                                           (dex:get (update-api (car modinfo)) :force-binary t))))
                                modsinfo))
           (releases (mapcar #'await-mod-latest-release releases-thread))
           (diff-original nil)
           ;(releases (with-open-file (test "test.txt") (read test)))
           (diff (multiple-value-bind (diff-new diff-old) (diff-version releases modsinfo)
                   (setf diff-original diff-old)
                   (let ((recipe (edit-recipe diff-new)))
                     (if (and recipe (y-or-n-p "~%Will upgrade: ~%~{~A~^, ~}~%continue?" (mapcar #'car recipe)))
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
                                          (format t "G~22@A ~A~%" modidstr modversion)
                                          (force-output)
                                          (list modidstr modversion filename path)))))))
                          diff)))
      (let ((inter (inter-id diff-original diff)))
        (format t "~%Will delete: ~%~{~A~^, ~}~%at ~A~%"
          (mapcar #'caddr inter)
          *cwd*)
        (force-output)
        (let* ((res-succeed (loop for r in (mapcar #'await downloads)
                                    if (failed-p r) do (format *error-output* "~&Skipped: ~a~%" (car (failed-condition r)))
                                    else collect r))
               (res-old-succeed (inter-id inter res-succeed)))
          (dolist (i res-old-succeed)
            (uiop:delete-file-if-exists (cadddr i))
            (format t "-~22@A ~A~%" (car i) (cadr i))
            (force-output))
          (when res-succeed (with-open-file (s lockfile
                                               :direction :output
                                               :if-exists :supersede
                                               :if-does-not-exist :create)
                              (prin1 (update-ordered-modsinfo modsinfo res-succeed) s)
                              (format t "Updated lockfile at ~A~%" lockfile)
                              (force-output s))))))))

; v2 api testing

;(print (dex:get (quri:make-uri :defaults "https://mods.vintagestory.at/api/v2/mods/install-information"
;                  :query
;                  `(("gv" . "1.22.3")
;                    ("ids" . ,(format nil "~{~A~^,~}" (read-list "src/test.txt")))))))

;(print (with-open-file (in "src/test.json")
;         (gethash "fileUrl" (gethash "carryon" (gethash "data" (jzon:parse in))))))
;
;(print (read-list "bulk-updater/src/test.txt"))