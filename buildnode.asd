;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.buildnode.system)
    (defpackage :net.acceleration.buildnode.system
	(:use :common-lisp :asdf))))

(in-package :net.acceleration.buildnode.system)

(defsystem :buildnode
  :description "Tool for building up an xml dom nicely."
  :components
  ((:module :src
	    :components
	    ((:file "packages")
	     (:file "buildnode" :depends-on ("packages"))
	     (:file "js-packager" :depends-on ("packages" :tags ))
	     (:module :tags
		      :components
		      ((:file "tags" )
		       (:file "xul-tags" :depends-on ("tags"))
		       (:file "custom-xul" :depends-on ( "xul-tags"))
		       (:file "xhtml-tags" :depends-on ("tags"))
		       (:file "custom-html" :depends-on ("xhtml-tags")))
		      :depends-on ("packages" "buildnode")))))
  :depends-on (:cxml :iterate :flexi-streams :arnesi
		     :swank ;; for setting tag-indentation
		     :adwcodebase :cl-interpol :closure-html))

(defsystem :buildnode-test
  :description ":buildnode-test: tests for buildnode library of code"
  :author "Acceleration.net"
  :licence "It's ours."
  :version "not so early"
  :components
  ((:module :tests
	    :serial t
	    :components ((:file "setup")
			 (:file "basic-tests"))))
  :depends-on (:buildnode :lisp-unit))