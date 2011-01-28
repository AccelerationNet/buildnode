;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.buildnode.system)
    (defpackage :net.acceleration.buildnode.system
	(:use :common-lisp :asdf))))

(in-package :net.acceleration.buildnode.system)

(defsystem :buildnode-xul
  :description "Tool for building up an xml dom of an excel spreadsheet nicely."
  :components
  ((:module :src
	    :serial T
	    :components
	    ((:file "js-packager")
	     (:module :tags
		      :serial T
		      :components
		      ((:file "xul-tags"))))))
  :depends-on (:buildnode))