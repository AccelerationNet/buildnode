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
    :documentation "a list that stores what urls are needed")
   (snippets :initarg :snippets
	     :accessor snippets
	     :initform nil
	     :documentation "A list of snippet strings that should be put in a script block.")))

(defun find-graph-node (key js-dependency-graph)
  "retrieves a graph-node from the js-dependency-graph based on a symbol or url"
  (gethash key js-dependency-graph ))

(defun get-url-from-key (key js-dependency-graph)
  "gets the url for a key reguardles of whether it is a symbol or url (assuming it is in the graph)"
  (if (stringp key) key
      (let ((it (find-graph-node key js-dependency-graph)))
	(when it
	  (js-graph-node-url it)))))

(defun creates-circular-dependency-p (key dep-list js-dependency-graph &optional visited-list)
  "checks if inserting into key into a js-dependency-list will cause a unseemly circular relationships to the beautiful dependency graph.  And you wouldnt want thatn now would you"
  (let ((key-url (get-url-from-key key js-dependency-graph)))
    (when key-url
      (or (find key-url visited-list :test #'equal)
	  (progn
	    (push key-url visited-list)
	    (some
	     (lambda (elem)
	       (let ((graph-node (find-graph-node elem js-dependency-graph)))
		 (when graph-node
		   (creates-circular-dependency-p (js-graph-node-url graph-node)
						  (js-graph-node-dependency-list graph-node)
						  js-dependency-graph
						  visited-list))))
	     dep-list))))))

(defun add-graph-node (key val js-dependency-graph)
  (if (creates-circular-dependency-p key (js-graph-node-dependency-list val) js-dependency-graph)
      (error "Insertion will lead to a circular dependency graph ~% key:~a~%graph:~a" key js-dependency-graph )
      (setf (gethash key js-dependency-graph) val)))

(defun make-js-collector ()
  "small quicky alias for make-instance 'js-collector)"
  (make-instance 'js-collector))

(defmethod js-list ((js js-collector))
  "gets the list of urls from a js collector (including dependencies)"
  (let ((return-list '()))
    (mapc				;foreach key/url in the js-collector
     (lambda (key-or-url)
       (mapc				;foreach url get the dependency list
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
  (unless (member url-or-key (urls *js-collector*) :test #'equal )
    (setf (urls *js-collector*) (nconc (urls *js-collector*) (list url-or-key))))
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

(defun make-script-tags-from-list (script-tag-function url-list)
  "script tag function is a function that accepts a url and creates script tags out of them"
  (mapcar
   (lambda (url)
     (funcall script-tag-function url))
   url-list))

(defmacro js-insertion-block ((make-script-fn &key (insertion-point-symbol 'insert-script-here)) &body body)
  "creates an environment in which there are a function: (use-js-file url &key depends-on) which will declare your usage of a script file and a symbol-macro: as defined by the keyword argument insertion-point-symbol (defaults to 'insert-script-here) that will be the final location of all of the collected script tags in the body"
  (arnesi:with-unique-names (chillins js-url-list script-tags replacement-node parent )
    `(let ((,replacement-node)) 
      (symbol-macrolet ((,insertion-point-symbol 
			 (if ,replacement-node
			     (error "More than one script tag insertion point in a document is not allowed")
			     (setf ,replacement-node (create-complete-element *document* "adw" "adw:replacement-node" '() '())))))
	(multiple-value-bind (,chillins ,js-url-list)
	    (with-javascript-collector
	      ,@body)
	  (let ((,script-tags (reverse
			       (make-script-tags-from-list
				,make-script-fn
				,js-url-list))))
	    (arnesi:if-bind ,parent (dom:parent-node ,replacement-node)
	      (progn
		(mapc
		 (lambda (elem)
		   (dom:insert-before ,parent elem ,replacement-node))
		 ,script-tags)
		(dom:remove-child ,parent ,replacement-node)
		,chillins)
	      (substitute ,script-tags ,replacement-node ,chillins ))))))))

(defun xhtml-script-tag (url)
  (make-script-fn #'xhtml:script url))

(defun xul-script-tag (url)  
  (make-script-fn #'xul:script url))

(defun xhtml-script-block (js)
  (make-script-block-fn #'xhtml:script js))
(defun xul-script-block (js)
  (make-script-block-fn #'xul:script js))

(defun make-script-fn (fn-script url)
  (funcall fn-script (list :src url :lang "javascript" :language "javascript" :type "text/javascript") ))

(defvar *cdata-script-blocks* T "Should script blocks have a cdata?")

(defun make-script-block-fn (fn-script js)
  (declare (special buildnode:*document*))
  (funcall fn-script
	   (list :lang "javascript" :language "javascript" :type "text/javascript")
	   (if *cdata-script-blocks*
	       (dom:create-cdata-section buildnode:*document* (format nil "~%~a~%" js))
	       (dom:create-text-node buildnode:*document* (format nil "~%~a~%" js)))))

(def-js-file 'JsHelper "/jscontrols/JSHelper.js" :depends-on '())
(def-js-file 'Controls "/jscontrols/Controls.js" :depends-on '(JsHelper))
(def-js-file 'Cookies "/jscontrols/Cookies.js" :depends-on '(JsHelper))
(def-js-file 'ADWSoap "/jscontrols/ADWSoap.js" :depends-on '(JsHelper))
(def-js-file 'Data "/jscontrols/Data.js" :depends-on '(JsHelper))
(def-js-file 'DetailObject "/jscontrols/DetailObject.js" :depends-on '(JsHelper))
(def-js-file 'DHTMLHelper "/jscontrols/DHTMLHelper.js" :depends-on '(JsHelper))
(def-js-file 'PartialPostBack "/jscontrols/PartialPostBack.js" :depends-on '(JsHelper Http))
(def-js-file 'Http "/jscontrols/Http.js" :depends-on '(JsHelper))
(def-js-file 'Grid "/jscontrols/Grid/grid.js" :depends-on '(JsHelper Data Http Controls Hashtable))
(def-js-file 'Collections "/jscontrols/Collections.js" :depends-on '(JsHelper))
(def-js-file 'Hashtable "/jscontrols/Collections.Hashtable.js" :depends-on '(JsHelper Collections))
(def-js-file 'Behaviour "/jscontrols/Behaviour.js" :depends-on '(Hashtable))
(def-js-file 'Tables "/jscontrols/Tables.js" :depends-on '(JsHelper))
(def-js-file 'Xul "/jscontrols/Xul.js" :depends-on '(JsHelper))


'(flet ((button-control ()
	 (declare (special use-js-file))
	 (use-js-file 'behaviour)
	 (list (xhtml:button '(:label "Fuck"))
	       (xhtml:button '(:label "You")))))
  (write-document
   (with-document
     (js-insertion-block ( #'xhtml-script-tag)
       (use-js-file 'grid)
       (xhtml:html
	'()
	(xhtml:head
	 '()
	 (xhtml:div '() "above")
	 insert-script-here
	 (xhtml:div '() "below")
	 )
	(xhtml:body
	 '()
	 (xhtml:div
	  '()
	  (use-js-file "js/initgrid.js")
	  (button-control))))
       (xul:window '(:id "fuck")
		   (js-insertion-block (#'xul-script-tag)
		     (xul:button '(:label "fuck"))
		     insert-script-here
		     (xul:button '(:label "you"))
		     (use-js-file "js/fuck your mom in her dirty ass")))))))

		