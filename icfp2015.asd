(in-package :cl-user)

(asdf:defsystem :icfp2015
  :serial t
  :components ((:file "package")
               #+nil(:file "???" :depends-on ("package")))
  :depends-on (:iterate :alexandria))
