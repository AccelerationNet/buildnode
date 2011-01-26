;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.buildnode.system)
    (defpackage :net.acceleration.buildnode.system
	(:use :common-lisp :asdf))))

(in-package :net.acceleration.buildnode.system)

(defsystem :buildnode-excel
  :description "Tool for building up an xml dom of an excel spreadsheet nicely."
  :components
  ((:module :src
	    :serial T
	    :components
	    ((:module :tags
		      :serial T
		      :components
		      ((:file "excel"))))))
  :depends-on (:buildnode))
