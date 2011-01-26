
(defpackage :net.acceleration.buildnode-test
    (:nicknames #:buildnode-test)
  (:use :common-lisp :cxml :arnesi :iterate :net.acceleration.utils
	:lisp-unit :buildnode)
  (:shadow :cdata))

(in-package :buildnode-test)
(cl-interpol:enable-interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-logger 'buildnode-tests)
    (arnesi::deflogger buildnode-tests ()
      :level arnesi:+debug+
      :appender (make-instance 'adwutils:useful-stream-log-appender
			       :stream *debug-io*))))

(with-package-iterator (sym '(:buildnode) :internal)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :buildnode)
		     pkg)
	    (ignore-errors (import (list symbol) :buildnode-test)))
	  (while more?))))

(defmacro buildnode-test (name (&rest args) &body body)
  (iter (for tag in args)
	(setf (get tag :tests)
	      (union (ensure-list (get tag :tests))
		     (list name))))
  `(lisp-unit:define-test ,name 
     (progn
       ,@body
       )))

(defmacro buildnode-w/doc-test (name (&rest args) &body body)
  `(buildnode-test ,name (,@args)
     (buildnode:with-html-document (progn ,@body nil))))


(defun run-tests-with-debugging (&key tests suites)
  (let* ((lisp-unit::*use-debugger* T)
	 (tests (append (ensure-list tests)
			(iter (for suite in (ensure-list suites))
			      (appending (get suite :tests)))))
	 (out (with-output-to-string (s)
		(let ((lisp-unit::*lisp-unit-stream*
		       (make-broadcast-stream
			s
			(if (eql t lisp-unit::*lisp-unit-stream*)
			    *standard-output*
			    lisp-unit::*lisp-unit-stream*))))
		  (lisp-unit::run-test-thunks
		   (lisp-unit::get-test-thunks
		    (if (null tests) (get-tests *package*) tests)))))))
    (buildnode-tests.info #?"\n ** TEST RESULTS ** \n-----------\n${out}\n------------\n")))

