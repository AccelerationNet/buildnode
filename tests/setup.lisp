
(defpackage :net.acceleration.buildnode-test
    (:nicknames #:buildnode-test)
  (:use :common-lisp :cxml :iterate :lisp-unit2-asserts :buildnode)
  (:shadow :cdata :run-tests))

(defpackage :net.acceleration.buildnode-test-objects)

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
  `(lisp-unit2:define-test ,name (:tags '(, args)
                                 :package :net.acceleration.buildnode-test-objects)
    ,@body
    ))

(defun with-document-context (body-fn)
  (buildnode:with-html-document (progn (funcall body-fn) nil)))

(defmacro buildnode-w/doc-test (name (&rest args) &body body)
  `(lisp-unit2:define-test ,name (:tags '(, args)
                                  :contexts #'with-document-context
                                  :package :net.acceleration.buildnode-test-objects)
    ,@body
    ))


(defun run-tests (&key suites tests)
  (let* ((*package* (find-package :net.acceleration.buildnode-test-objects)))
    (lisp-unit2:run-tests
     :tests tests
     :tags suites
     :name :buildnode
     :reintern-package :net.acceleration.buildnode-test-objects
     :run-contexts #'lisp-unit2:with-summary-context)))


