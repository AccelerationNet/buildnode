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
	    :serial T
	    :components
	    ((:file "packages")
	     (:file "buildnode")
	     (:module :tags
		      :serial T
		      :components ((:file "tags" ))))))
  :depends-on (:cxml :iterate :flexi-streams 
		     :swank ;; for setting tag-indentation
		     :cl-interpol :closure-html))

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
  :depends-on (:buildnode :buildnode-xhtml :lisp-unit :adwcodebase :arnesi))

(defsystem :buildnode-xhtml
  :description "Tool for building up an xml dom of an excel spreadsheet nicely."
  :components
  ((:module :src
	    :serial T
	    :components
	    ((:module :tags
		      :serial T
		      :components
		      ((:file "xhtml-tags")))
	     (:file "js-packager"))))
  :depends-on (:buildnode))

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