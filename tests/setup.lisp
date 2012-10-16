
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
  (format *standard-output* "~&")
  (log-time (get-universal-time) *standard-output*)
  (apply #'format *standard-output* message args)
  (format *standard-output* "~%"))

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
	      (union (alexandria:ensure-list (get tag :tests))
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
         (lisp-unit:*print-failures* t)
         (lisp-unit:*print-errors* t)
	 (lisp-unit::*use-debugger* use-debugger)
	 (tests (append (alexandria:ensure-list tests)
			(iter (for suite in (alexandria:ensure-list suites))
                          (appending (get suite :tests)))))
         (actual-std-out *standard-output*)
	 (out (with-output-to-string (s)
		(let ((*standard-output*
                        (make-broadcast-stream s actual-std-out)))
                  (if (null tests)
                      (lisp-unit::%run-all-thunks)
                      (lisp-unit::%run-thunks tests))))))
    (format *standard-output*
     "~&~% ** TEST RESULTS: BUILDNODE ** ~%-----------~%~A~%------ END TEST RESULTS ------~%"
     out)))


