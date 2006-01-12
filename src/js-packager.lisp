(in-package :net.acceleration.javascript)

(defun make-js-depenecy-graph ()
  (make-hash-table :test #'equal))

(defstruct js-graph-node
  (url)
  (dependency-list))

(defvar *global-js-dependency-graph* (make-js-depenecy-graph))

(defclass js-collector ()
  ((urls
	 :initarg :urls
	 :accessor urls
	 :initform '()
	 :documentation "a list that stores what urls are needed")))

(defun find-graph-node (key js-dependency-graph)
  (gethash key js-dependency-graph ))

(defun add-graph-node (key val js-dependency-graph)
  (setf (gethash key js-dependency-graph) val))

(defun get-url-from-key (key js-dependency-graph)
  (if (stringp key) key
		(let ((it (find-graph-node key js-dependency-graph)))
		  (when it
			 (js-graph-node-url it)))))

(defun make-js-collector ()
  "small quicky alias for make-instance 'js-collector)"
  (make-instance 'js-collector))

(defmethod js-list ((js js-collector))
  (let ((return-list '()))
	 (mapc ;foreach key/url in the js-collector
	  (lambda (key-or-url)
		 (mapc ;foreach url get the dependency list
		  (lambda (elem)
			 (pushnew elem return-list :test #'string=))
		  (get-dependency-list key-or-url))
		 (pushnew (get-url-from-key key-or-url *global-js-dependency-graph*)
					 return-list :test #'string=))
	  (urls js))
	 return-list))

(defun get-dependency-list (key)
  "gets a list of all the files required for proper execution  of a javascript file
that is designated by the key (either a keyword in the *global-js-dependency-graph* or a url)"
  (let ((return-list '()))
	 (awhen (find-graph-node key *global-js-dependency-graph*)
		(mapc (lambda (key-dep-list-item)
				  (let ((dep-list (get-dependency-list key-dep-list-item)))
					 (mapc
					  (lambda (elem)
						 (pushnew elem return-list :test #'equal))
					  dep-list)
					 (push (get-url-from-key key-dep-list-item *global-js-dependency-graph*)
							 return-list)))
				(js-graph-node-dependency-list it)))
	 (nreverse return-list)))

(defun def-js-file (js-name url &key depends-on)
  "defines a named (using a keyword) javascript file and its dependencies"
  (let ((value (make-js-graph-node :url url :dependency-list depends-on)))
	 (add-graph-node js-name value *global-js-dependency-graph* )
	 (add-graph-node url value *global-js-dependency-graph* )))

(defun def-anon-js-file (url &key depends-on)
  "defines an js file and its dependencies using only a url"
	 (add-graph-node url (make-js-graph-node :url url :dependency-list depends-on) *global-js-dependency-graph*))

(defun js-defined-p (js-key-or-url)
  "has a js-file been defined either with or without a name"
	 (nth-value 1 (find-graph-node js-key-or-url *global-js-dependency-graph*)))

(defun use-js-file (url-or-key &key depends-on )
  "a function to add a js-file to the special variable  *js-collector* which will
be in scope inside of with-javascript-collector"
  (declare (special *js-collector*))
  (unless (js-defined-p url-or-key)
	 (if (stringp url-or-key)
		  (def-anon-js-file url-or-key :depends-on depends-on)
		  (error (format nil "js-file with name ~s is not defined" url-or-key))))
  (pushnew url-or-key (urls *js-collector*) :test #'equal)
  '())

(defmacro with-javascript-collector (&body body)
  "returnst a list of js file urls.
 There is a special-symbol use-js-file that is a function to add a js file to the collection.
 (use-js-file (url-or-JsName :depends-on '(a dependency list of urls and keyword names)) => has the side effect of adding a js-file to the js-collection
With js-collector also appends all (non-nil) elements in body to the document"
  `(let* ((*js-collector* (make-js-collector)))
	 (declare (special *js-collector*))
	 (let ((body-rtn-list (list ,@body)))
		(values
		 body-rtn-list
		 (js-list *js-collector*)))))

(defun with-script-tags-from-list (script-tag-function url-list children)
  "script tag function is a function that accepts a url and creates script tags out of them"
  (append
	(mapcar
	 (lambda (url)
		(funcall script-tag-function url))
	 url-list)
	children))


 
(def-js-file :JsHelper "/jscontrols/jshelper.js" :depends-on '())
(def-js-file :Control "/jscontrols/Control.js" :depends-on '(:JsHelper))
(def-js-file :Data "/jscontrols/Data.js" :depends-on '(:JsHelper))
(def-js-file :Http "/jscontrols/Http.js" :depends-on '(:JsHelper))
(def-js-file :Grid "/jscontrols/Grid/grid.js" :depends-on '(:JsHelper :Data :Http :Control :Hashtable))
(def-js-file :Collections "/jscontrols/Collections.js" :depends-on '(:JsHelper))
(def-js-file :Hashtable "/jscontrols/Collections.Hashtable.js" :depends-on '(:JsHelper :Collections))
(def-js-file :Behaviour "/jscontrols/Behaviour.js" :depends-on '(:Hashtable))


  



;(flet ((button-control ()
;			(declare (special use-js-file))
;			(use-js-file :behaviour)
;			(list (xul:button '(:label "Fuck"))
;					(xul:button '(:label "You")))))
  
;  (buildnode:with-document-to-file "test.xul"
;	 (xul:window '()
;					 (multiple-value-bind (chillins js-url-list)
;						  (with-javascript-collector
;							 (use-js-file :grid)
;							 (button-control)
;							 (use-js-file "js/initgrid.js"))
;						(with-script-tags-from-list
;							 (lambda (url) (xul:script (list :href url) ))
;						  js-url-list
;						  chillins)))))
