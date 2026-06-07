(in-package :asdf-user)

(defsystem "bulk-updater"
  :author "Evan Gao"
  :license "WTFPL"
  :homepage "https://github.com/notch1p/vs-things"
  :source-control "https://github.com/notch1p/vs-things.git"
  :depends-on ("com.inuoe.jzon"
               "dexador"
               "zip"
               "cl-interpol"
               "cl-ppcre"
               "named-readtables")

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "thread-failure")
                                     (:file "bulk-updater"))))
  :build-operation "program-op"
  :build-pathname "vsmod-updater"
  :entry-point "bulk-updater:entry-point")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression 9))
