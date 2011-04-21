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
  :depends-on (:cxml :iterate :flexi-streams :split-sequence
		     :swank ;; for setting tag-indentation
		     :cl-interpol
		     ;; TODO:
		     ;; for html-generation - probably not a dependancy of the whole library
		     :closure-html 
		     ))

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

;;;; Copyright (C) 2011 Acceleration.net, Russ Tyndall
;;;;   email: bobbysmith007@gmail.com
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, under version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; Copyright (C) 2011 Acceleration.net, Russ Tyndall
;;;;   email: bobbysmith007@gmail.com
;;;; This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING.
;;;; This is free software, and you are welcome to redistribute it
;;;; under certain conditions; for details see COPYING.
