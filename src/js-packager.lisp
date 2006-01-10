(in-package :net.acceleration.javascript)

(defstruct js-dependency-graph
  (hash (make-hash-table :test #'equal)))

(defstruct js-graph-node
  (url)
  (dependency-list))

(defvar *global-js-dependency-graph* (make-js-depenecy-graph))

(defclass js-collector ()
  ((url-hash
	 :initarg :url-hash
	 :accessor url-hash
	 :initform (make-hash-table :test #'equal)
	 :documentation "a hash table that stores what urls are needed")
	))

(defun find-graph-node (key js-dependency-graph)
  (gethash key (js-dependency-graph-hash js-dependency-graph) ))

(defun add-graph-node (key val js-dependency-graph)
  (setf (gethash key (js-dependency-graph-hash js-dependency-graph)) val))


(defun make-js-collector ()
  "small quicky alias for make-instance 'js-collector)"
  (make-instance 'js-collector))

(defmethod js-list ((js js-collector))
  (let ((result '()))
	 (maphash
	  
	  (lambda (key val)
		 (mapc
		  
		  (lambda (elem)
			 (unless (member elem result :test #'string-equal)
				(nconc result (list elem))))
		  
		  (get-dependency-list key))
		 (unless (member val result :test #'string-equal)
			(nconc result (list key))))
	  
	  (url-hash js))
	 result))

(defun get-dependency-list (key)
  "gets a list of all the files required for proper execution  of a javascript file
that is designated by the key (either a keyword in the *global-js-dependency-graph* or a url)"
  (let ((return-list '()))
	 (kmrcl:awhen (find-graph-node key *global-js-dependency-graph*)
		(mapc (lambda (elem)
				  (nconc  
					(if (stringp elem)
						 (list elem)
						 (get-dependency-list elem))
					return-list))
				(js-graph-node-dependency-list it)))
	 return-list))

(defun def-js-file (js-name url &key depends-on)
  "defines a named (using a keyword) javascript file and its dependencies"
	 (add-graph-node js-name (make-js-graph-node :url url :dependency-list depends-on) *global-js-dependency-graph* )
	 (add-graph-node url (make-js-graph-node :url url :dependency-list depends-on) *global-js-dependency-graph* ))

(defun def-anon-js-file (js-name url &key depends-on)
  "defines an js file and its dependencies using only a url"
	 (add-graph-node (make-js-graph-node :url url :dependency-list depends-on) *global-js-dependency-graph*))

(defun js-defined-p (js-key-or-url)
  "has a js-file been defined either with or without a name"
	 (nth-value 1 (find-graph-node js-key-or-url *global-js-dependency-graph*)))

(defun use-js-file (js-collector url-or-key &key depends-on)
  (unless (js-defined-p url-or-key)
	 (if (stringp url-or-key)
		  (def-anon-js-file url-or-key :depends-on depends-on)
		  (error (format nil "js-file with name ~s is not defined" url-or-key))))
  (if (stringp url-or-key)
		(setf (gethash url-or-key (url-hash js-collector)) T)
		(setf (gethash
				 ;; Get the url for the name out of *global-js-dependency-graph*
				 (js-graph-node-url (find-graph-node url-or-key *global-js-dependency-graph*))
				 (url-hash js-collector)) T)))

(defmacro with-javascript-collector (&body body)
  "returnst a list of js file urls.  There is a special-symbol use-js-file that is a function to add a js file to the collection.
 (use-js-file (url-or-JsName :depends-on '(a dependency list of urls and keyword names)) => has the side effect of adding a js-file to the js-collection"
  `(let ((,variable ,js-collector)
			(use-js-file))
	 (declare (special ,variable))
	 (declare (special use-js-file))
	 (setf (symbol-function 'use-js-file)
	  (lambda (url-or-key &key depends-on )
		 (funcall #'net.acceleration.javascript::use-js-file ,variable url-or-key depends-on))
	 ,@body)))







(def-js-file :JsHelper "/jscontrols/jshelper.js" :depends-on '() )
(def-js-file :Control "/jscontrols/Control.js" :depends-on '(:JsHelper) )
(def-js-file :Data "/jscontrols/Data.js" :depends-on '(:JsHelper) )
(def-js-file :Http "/jscontrols/Http.js" :depends-on '(:JsHelper) )
(def-js-file :Grid "/jscontrols/Grid/grid.js" :depends-on '(:JsHelper :Data :Http :Control) )
(def-js-file :Collections "/jscontrols/Collections.js" :depends-on '(:JsHelper))
(def-js-file :Hashtable "/jscontrols/Collections.Hashtable.js" :depends-on '(:JsHelper :Collections))
(def-js-file :Behaviour "/jscontrols/Behaviour.js" :depends-on '(:Hashtable))


