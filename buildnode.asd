;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.buildnode.system)
    (defpackage :net.acceleration.buildnode.system
      (:use :common-lisp :asdf))))

(in-package :net.acceleration.buildnode.system)

(defsystem :buildnode
  :description "Tool for building up an xml dom nicely."
  :components ((:module :src
								:components ((:file "packages")
												 (:file "xul" :depends-on ("packages"))
												 (:file "xultags" :depends-on ("packages" "xul")))))
  :depends-on (:cxml :iterate :yaclml))
