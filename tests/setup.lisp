
(defpackage :net.acceleration.buildnode-test
    (:nicknames #:buildnode-test)
  (:use :common-lisp :cxml :iterate :lisp-unit :buildnode)
  (:shadow :cdata :run-tests))

(in-package :buildnode-test)

(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defun buildnode-tests.info (message &rest args)
  (format lisp-unit::*lisp-unit-stream* "~&")
  (log-time (get-universal-time) lisp-unit::*lisp-unit-stream*)
  (apply #'format lisp-unit::*lisp-unit-stream* message args)
  (format lisp-unit::*lisp-unit-stream* "~%"))

(with-package-iterator (sym '(:buildnode) :internal :external)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :buildnode)
		     pkg)
	    (ignore-errors
	      (unintern symbol :buildnode-test)
	      (import (list symbol) :buildnode-test)))
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


(defun run-tests (&key suites tests (use-debugger T))
  (let* ((*package* (find-package :buildnode-test))
	 (lisp-unit::*use-debugger* use-debugger)
	 (tests (append (ensure-list tests)
			(iter (for suite in (ensure-list suites))
			      (appending (get suite :tests)))))
	 (out (with-output-to-string (lisp-unit::*lisp-unit-stream*)
		(lisp-unit::run-test-thunks
		 (lisp-unit::get-test-thunks
		  (if (null tests)
		      (get-tests *package*)
		      tests))))))
    
    (format *standard-output*
	    "~&~% ** TEST RESULTS: Buildnode ** ~%-----------~%~A~%------ END TEST RESULTS ------~%"
	    out)))

