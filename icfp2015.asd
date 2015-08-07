(in-package :cl-user)

(asdf:defsystem :icfp2015
  :serial t
  :components ((:file "package")
               (:file "hextris" :depends-on ("package")))
  :depends-on (:iterate :alexandria :cl-json))
