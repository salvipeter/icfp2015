(in-package :cl-user)

(require :asdf)
(asdf:load-system :icfp2015)
(sb-ext:save-lisp-and-die "play_icfp2015" :toplevel #'icfp2015:main :executable t)
