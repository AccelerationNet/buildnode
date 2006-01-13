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
												 (:file "buildnode" :depends-on ("packages"))
												 (:file "js-packager" :depends-on ("packages" :tags ))
												 (:module :tags
															 :components ((:file "tags" )
																			  (:file "xul-tags" )
																			  (:file "custom-xul" :depends-on ( "xul-tags"))
																			  (:file "xhtml-tags" )
																			  (:file "custom-html" :depends-on ("xhtml-tags")))
															 :depends-on ("packages" "buildnode")))))
  :depends-on (:cxml :iterate :flexi-streams :arnesi :kmrcl))
