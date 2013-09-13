;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :net.acceleration.buildnode.system)
    (defpackage :net.acceleration.buildnode.system
	(:use :common-lisp :asdf))))

(in-package :net.acceleration.buildnode.system)

(defsystem :buildnode
  :description "Tool for building up an xml dom nicely."
  :author "http://www.acceleration.net"
  :licence "BSD"
  :components
  ((:module :src
	    :serial T
	    :components
	    ((:file "packages")
	     ;; should be removed once a patch doing the same thing makes its
	     ;; way upstream
	     (:file "dom-walker")
	     (:file "buildnode")
	     (:module :tags
		      :serial T
		      :components ((:file "tags" ))))))
  :depends-on (:cxml :alexandria
               :iterate :flexi-streams :split-sequence
		     :swank ;; for setting tag-indentation
		     :cl-interpol
                     :collectors
		     ;; TODO:
		     ;; for html-generation - probably not a dependancy of the whole library
		     :closure-html
                     :cl-ppcre
                     :symbol-munger
		     ))

(defsystem :buildnode-test
  :description ":buildnode-test: tests for buildnode library of code"
  :author "http://www.acceleration.net"
  :licence "BSD"
  :components
  ((:module :tests
	    :serial t
	    :components ((:file "setup")
			 (:file "basic-tests"))))
  :depends-on (:buildnode :buildnode-xhtml :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :buildnode))))
  (asdf:oos 'asdf:load-op :buildnode-test)
  (funcall (intern "RUN-TESTS" :buildnode-test)
	   :use-debugger nil))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.