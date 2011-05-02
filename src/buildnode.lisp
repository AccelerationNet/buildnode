(in-package :net.acceleration.buildnode)
(cl-interpol:enable-interpol-syntax)


(defvar *document* ()
  "A variable that holds the current document that is being built. see
  with-document.")

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute),@body))

(defun ensure-list (l) (if (listp l) l (list l)))

(defun flatten-children (kids &optional (doc *document*))
  "Handles flattening nested lists and vectors of nodes
   into a single flat list of children
  "
  (iter (for kid in (ensure-list kids))
	(typecase kid
	  (string (collecting (if doc
				  (dom:create-text-node doc kid)
				  kid)))
	  ((or dom:element dom:node) (collecting kid))
	  (list (nconcing (flatten-children kid doc)))
	  (vector (nconcing
		   (flatten-children
		    (iter (for sub-kid in-sequence kid) (collect sub-kid))
		    doc)))
	  (T (collecting (let ((it (princ-to-string kid)))
			   (if doc
			       (dom:create-text-node doc it)
			       it)))))))

(defun %merge-conts (&rest conts)
  "Takes many continuations and makes a single continuation that
   iterates through each of the arguments in turn"
  (setf conts (remove-if #'null conts))
  (when conts
    (lambda ()
      (let ((rest (rest conts)))
	(multiple-value-bind (item new-cont) (funcall (first conts))
	  (when new-cont (push new-cont rest))
	  (values item (when rest
			 (apply #'%merge-conts rest))))))))

(defun %walk-dom-cont (tree)
  "calls this on a dom tree (or tree list) and you get back
   a node and a continuation function.

   repeated calls to the continuation each return the next node
   and the next walker continuation
  "
  (typecase tree
    (null nil)
    ((or vector list)
       (when (plusp (length tree))
	 (multiple-value-bind (item cont) (%walk-dom-cont (elt tree 0) )
	   (values
	     item (%merge-conts
		   cont (lambda () (%walk-dom-cont (subseq tree 1))))))))
    (dom:document (%walk-dom-cont (dom:child-nodes tree)))
    (dom:text tree)
    (dom:element
       (values
	 tree
	 (when (> (length (dom:child-nodes tree)) 0)
	   (lambda () (%walk-dom-cont (dom:child-nodes tree) )))))))

(defun depth-first-nodes (tree)
  "get a list of the nodes in a depth first traversal of the dom trees"
  (iter
    (with cont = (lambda () (%walk-dom-cont tree)))
    (if cont
	(multiple-value-bind (item new-cont) (funcall cont)
	  (when item (collect item))
	  (setf cont new-cont))
	(terminate))))

(iterate:defmacro-driver (FOR node in-dom tree)
  "A driver that will walk over every node in a set of dom trees"
  (let ((kwd (if generate 'generate 'for))
	(cont (gensym "CONT-"))
	(new-cont (gensym "NEW-CONT-"))
	(genned-node (gensym "GENNED-NODE-")))    
    `(progn
       (with ,cont = (lambda () (%walk-dom-cont ,tree)))
       (,kwd ,node next
	     (if (null ,cont)
		 (terminate)
		 (multiple-value-bind (,genned-node ,new-cont) (funcall ,cont)
		   (setf ,cont ,new-cont)
		   ,genned-node)))
       (unless ,node (next-iteration)))))

(iterate:defmacro-driver (FOR parent in-dom-parents node)
  "A driver that will return each parent node up from a starting node
   until we get to a null parent"
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       ;; (with ,cont = (lambda () (%walk-dom-cont ,tree)))
       (,kwd ,parent next
	     (if (first-iteration-p)
		 (dom:parent-node ,node)
		 (if (null ,parent)
		     (terminate)
		     (dom:parent-node ,parent))))
       (unless ,parent (next-iteration)))))

(iterate:defmacro-driver (FOR kid in-dom-children nodes)
  "iterates over the children of a dom node as per flatten-children"
  (let ((kwd (if generate 'generate 'for))
	(nl (gensym "NL-")))
    `(progn
       (with ,nl =
	     (flatten-children
	      (typecase ,nodes
		((or dom:element dom:document) (dom:child-nodes ,nodes))
		((or list vector) ,nodes))))
       (,kwd ,kid in ,nl))))


(defun xmls-to-dom-snippet ( sxml &key
			    (namespace "http://www.w3.org/1999/xhtml"))
  "Given a snippet of xmls, return a new dom snippet of that content"
  (etypecase sxml
    (string sxml)
    (list (destructuring-bind (tagname attrs . kids) sxml
	    (create-complete-element
	     *document* namespace
	     tagname
	     (iter (for (k v) in attrs) (collect k) (collect v))
	     (loop for node in kids
		   collecting
		   (xmls-to-dom-snippet node :namespace namespace)))))))

(defparameter *xhtml1-transitional-extid*
  (let ((xhtml1-transitional.dtd
	 (make-pathname :directory (append
				    (pathname-directory (truename (asdf:system-definition-pathname :buildnode)))
				    '("src"))
			:name "xhtml1-transitional"
			:type "dtd")))
   (cxml:make-extid "-//W3C//DTD XHTML 1.0 Transitional//EN"
		   (puri:uri #?|file://${xhtml1-transitional.dtd}|))))

(defmethod text-of-dom-snippet (el &optional splice stream)
  "get all of the textnodes of a dom:element and return that string
   with splice between each character"
  (flet ((body (s)
	   (iter
	     (with has-written = nil )
	     (for node in-dom el)
	     (when (dom:text-node-p node)
	       (when (and has-written splice)
		 (princ splice s))
	       (princ (dom:data node) s)
	       (setf has-written T)
	       ))))
    (if stream
	(body stream)
	(with-output-to-string (stream)
	  (body stream)))))

(defclass scoped-dom-builder (rune-dom::dom-builder)
  ()
  (:documentation
   "A dom builder that builds inside of another dom-node"))
(defmethod sax:start-document ((db scoped-dom-builder)))
(defmethod sax:end-document ((db scoped-dom-builder))
  (rune-dom::document db))

(defmethod sax:unescaped ((builder scoped-dom-builder) data)
  ;; TODO:  Is this the correct answer?  I have no idea how to handle
  ;; unescaped content in a dom (which is probably why this was not
  ;; implemented on dom-builder)
  ;; also not sure why returning this fails to work (tries to add it to node)
  ;; rather than teh current subnode we are in ...
  (buildnode:add-children
   (first (rune-dom::element-stack builder))
   (dom:create-text-node (rune-dom::document builder) data))
  (values))

;;;; I think we might be able to use this as a dom-builder for a more efficient
;;;; version of the inner-html function
(defun make-scoped-dom-builder (node)
  "Returns a new scoped dom builder, scoped to the passed in node.
   Nodes built with this builder will be added to the passed in node"
  (let ((builder (make-instance 'scoped-dom-builder)))
    (setf (rune-dom::document builder) (etypecase node
					 (dom:document node)
					 (dom:node (dom:owner-document node)))
 	  (rune-dom::element-stack builder) (list node))
    builder))

(defclass html-whitespace-remover (cxml:sax-proxy)
  ()
  (:documentation "a stream filter to remove nodes that are entirely whitespace"))

(defmethod sax:characters ((handler html-whitespace-remover) data)
  (unless (every #'cxml::white-space-rune-p (cxml::rod data)) (call-next-method)))

(defun inner-html (string &optional
		   (tag "div")
		   (namespace-uri "http://www.w3.org/1999/xhtml")
		   (dtd nil))
  "Parses a string containing a well formed html snippet
   into dom nodes inside of a newly created node.

   (Based loosely around the idea of setting the javascript innerHTML property)

   Will wrap the input in a tag (which is neccessary from CXMLs perspective)
   can validate the html against a DTD if one is passed, can use
   *xhtml1-transitional-extid* for example.
   "
  (handler-bind ((warning #'(lambda (condition)
			      (declare (ignore condition))
			      (muffle-warning))))
    (let ((node (dom:create-element *document* tag)))
      (cxml:parse #?|<${tag} xmlns="${ namespace-uri }">${string}</${tag}>|
		  (make-instance 'html-whitespace-remover
				 :chained-handler (make-scoped-dom-builder node))
		  :dtd dtd)
      (dom:first-child node))))

(defun document-of (el)
  "Returns the document of a given node (or the document if passed in)"
  (if (typep el 'rune-dom::document)
      el
      (dom:owner-document el)))

(defun add-children (elem &rest kids)
  "adds some kids to an element and return that element
    alias for append-nodes"
  (iter (for kid in (flatten-children kids (document-of elem)))
	(dom:append-child elem kid))
  elem)

(defun insert-children (elem idx &rest kids)
  " insert a bunch of dom-nodes (kids) to the location specified
     alias for insert-nodes"
  (setf kids (flatten-children kids (document-of elem)))
  (if (<= (length (dom:child-nodes elem)) idx )
      (apply #'add-children elem kids)
      (let ((after (elt (dom:child-nodes elem) idx)))
	(iter (for k in kids)
	      (dom:insert-before elem k after))))
  elem)

(defun append-nodes (to-location &rest chillins)
  "appends a bunch of dom-nodes (chillins) to the location specified
   alias of add-children"
  (apply #'add-children to-location chillins))

(defun insert-nodes (to-location index &rest chillins)
  "insert a bunch of dom-nodes (chillins) to the location specified
    alias of insert-children"
  (apply #'insert-children to-location index chillins))

(defvar *html-compatibility-mode* nil)
(defvar *cdata-script-blocks* T "Should script blocks have a cdata?")
(defvar *namespace-prefix-map*
  '(("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul" . "xul")
    ("http://www.w3.org/1999/xhtml" . "xhtml")))

(defun get-prefix (namespace &optional (namespace-prefix-map
					*namespace-prefix-map*))
  (when namespace-prefix-map
  (the (or null string)
    (cdr (assoc namespace namespace-prefix-map :test #'string=)))))

(defun get-namespace-from-prefix (prefix &optional (namespace-prefix-map
						    *namespace-prefix-map*))
  (when namespace-prefix-map
    (the (or null string)
      (car (find prefix namespace-prefix-map :key #'cdr :test #'string=)))))

(defun calc-complete-tagname (namespace base-tag namespace-prefix-map)
  (let ((prefix
	 (and namespace-prefix-map
	      (not (cxml::split-qname base-tag)) ;not already a prefix
	      (let ((prefix (get-prefix namespace namespace-prefix-map)))
		;;found the given namespace in the map
		(when (and prefix (> (length (the string prefix)) 0))
		  prefix)))))
    (if prefix
	#?"${prefix}:${base-tag}"
	base-tag)))

(defun prepare-attribute-name (attribute)
  "Prepares an attribute name for output to html by coercing to strings"
  (etypecase attribute
    (symbol (coerce (string-downcase attribute)
		    '(simple-array character (*))))
    (string attribute)))

(defun prepare-attribute-value (value)
  "prepares a value for html out put by coercing to a string"
  (typecase value
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (T (princ-to-string value))))

(defun attribute-uri (attribute)
  (typecase attribute
    (symbol nil)
    (string 
       (let ((list (cl-ppcre:split ":" attribute)))
	 (case (length list)
	   (2 (get-namespace-from-prefix (first list)))
	   ((0 1) nil)
	   (T (error "Couldnt parse attribute-name ~a into prefix and name" attribute)))))))

(defun get-attribute (elem attribute)
  "Gets the value of an attribute on an element
   if the attribute does not exist return nil
  "
  (let ((args (list elem
		    (attribute-uri attribute)
		    (prepare-attribute-name attribute))))
    (when (apply #'dom:has-attribute-ns args)
      (apply #'dom:get-attribute-ns args))))

(defun set-attribute (elem attribute value)
  "Sets an attribute and passes the elem through, returns the elem"
  (dom:set-attribute-ns
   elem
   (attribute-uri attribute)
   (prepare-attribute-name attribute)
   (prepare-attribute-value value))
  elem)

(defun remove-attribute (elem attribute)
  "removes an attribute and passes the elem through, returns the elem
   If the attribute does not exist, simply skip it
  "
  ;; throws errors to remove attributes that dont exist
  ;; dont care about that
  (let ((uri (attribute-uri attribute))
	(name (prepare-attribute-name attribute)))
    (when (dom:has-attribute-ns elem uri name)
      (dom:remove-attribute-ns elem uri name)))
  elem)

(defun remove-attributes (elem &rest attributes)
  "removes an attribute and passes the elem through, returns the elem"
  (iter (for attr in attributes)
	(remove-attribute elem attr))
  elem)

(defmethod css-classes ((el dom:element))
  "Returns a list of css classes (space separated names in the 'class' attribute)"
  (split-sequence:split-sequence
   #\space (get-attribute el :class)
   :remove-empty-subseqs t))

(defmethod add-css-class ((el dom:element) new-class)
  "Adds a new css class to the element and returns the element"
  (let ((css-classes (css-classes el)))
    (pushnew new-class css-classes :test #'string=)
    (set-attribute el :class (format nil "~{~a~^ ~}" css-classes)))
  el)

(defmethod remove-css-class ((el dom:element) new-class)
  "Removes a css class from the elements and returns the element"
  (let ((css-classes (remove
		      new-class
		      (css-classes el)
		      :test #'string=)))
    (if css-classes
	(set-attribute el :class (format nil "~{~a~^ ~}" css-classes))
	(remove-attribute el :class))
    el))

(defun push-new-attribute (elem attribute value)
  "if the attribute is not on the element then put it there with the specified value,
   returns the elem and whether or not the attribute was set"
  (values elem
   (when (null (get-attribute elem attribute))
     (set-attribute elem attribute value)
     T)))

(defun push-new-attributes (elem &rest attribute-p-list)
  "for each attribute in the plist push-new into the attributes list of the elem, returns the elem"
  (iter (for (attr val) on attribute-p-list by #'cddr)
	(push-new-attribute elem attr val))
  elem)

(defun set-attributes (elem &rest attribute-p-list)
  "set-attribute for each attribute specified in the plist, returns the elem"
  (iter (for (attr val) on attribute-p-list by #'cddr)
	(set-attribute elem attr val))
  elem)

(defun create-complete-element (document namespace tagname attributes children
					 &optional
				(namespace-prefix-map *namespace-prefix-map*))
  "Creates an xml element out of all the necessary components.
If the tagname does not contain a prefix, then one is added based on the namespace-prefix map."
  (declare (type list attributes))
  ;;if we don't already have a prefix and we do find one in the map.
  (let* ((tagname (if namespace-prefix-map
		      (calc-complete-tagname namespace tagname namespace-prefix-map)
		      tagname))
	 (elem (dom:create-element-ns document namespace tagname)))
    (when (oddp (length attributes))
      (error "Incomplete attribute-value list. Odd number of elements in ~a" attributes))
    (apply #'set-attributes elem attributes)
    ;;append the children to the element.
    (append-nodes elem children)
    elem))


(defun write-normalized-document-to-sink (document stream-sink)
  "writes a cxml:dom document to the given stream-sink,
passing the document through a namespace normalizer first, and
possibly a html-compatibility-sink if *html-compatibility-mode* is set"
  (dom:map-document
   (cxml:make-namespace-normalizer stream-sink)
   document
   :include-doctype :canonical-notations))

(defun make-output-sink (stream &key canonical indentation (char-p T))
  (apply
   (cond
     ((and char-p *html-compatibility-mode*)
      #'chtml:make-character-stream-sink)
     ((and (not char-p) *html-compatibility-mode*)
      #'chtml:make-octet-stream-sink)
     ((and char-p (not *html-compatibility-mode*))
      #'cxml:make-character-stream-sink)
     ((and (not char-p) (not *html-compatibility-mode*))
      #'cxml:make-octet-stream-sink))
   stream
   (unless *html-compatibility-mode*
     (list :canonical canonical :indentation indentation))))

(defun write-document-to-character-stream (document char-stream)
  "writes a cxml:dom document to a character stream"
  (let ((sink (make-output-sink char-stream)))
    (write-normalized-document-to-sink document sink)))

(defun write-document-to-octet-stream (document octet-stream)
  "writes a cxml:dom document to a character stream"
  (let ((sink (make-output-sink octet-stream :char-p nil)))
    (write-normalized-document-to-sink document sink)))

(defun write-document (document &optional (out-stream *standard-output*))
  "Write the document to the designated out-stream, or *standard-ouput* by default."
  (case (stream-element-type out-stream)
    ('character (write-document-to-character-stream document out-stream))
    (otherwise (write-document-to-octet-stream document out-stream))))

(defmacro with-document (&body chillins)
  "(with-document ( a bunch of child nodes of the document )) --> cxml:dom document
Creates an environment in which the special variable *document* is available
a document is necessary to create dom nodes and the document the nodes end up on
must be the document on which they were created.  At the end of the form, the
complete document is returned"
  `(let ((*document*  (cxml-dom:create-document)))
    (append-nodes *document* ,@chillins)
    *document*))

(defmacro with-document-to-file (filename &body chillins)
  "Creates a document block with-document upon which to add the chillins (southern for children).
  When the document is complete, it is written out to the specified file."
  `(write-doc-to-file (with-document ,@chillins) ,filename))

(defun write-doc-to-file (doc filename)
  "Binary write-out a document. will create/overwrite any existing file named the same."
  (let ((filename (merge-pathnames filename)) )
    (with-open-stream (fd (open filename :direction :output :element-type '(unsigned-byte 8)
							    :if-does-not-exist :create
							    :if-exists :supersede))
      (write-document doc fd))
    (values doc filename)))

(defun document-to-string (doc)
  "Return a string representation of a document."
  (with-output-to-string (fd)
    (write-document doc fd)))

(defmacro with-xhtml-document (&body chillins)
  "(with-xhtml-document ( a bunch of child nodes of the document )) --> cxml:dom document
Creates an environment in which the special variable *document* is available
a document is necessary to create dom nodes and the document the nodes end up on
must be the document on which they were created.  At the end of the form, the
complete document is returned.
This sets the doctype to be xhtml transitional."
  `(let ((*cdata-script-blocks* T)
	 (*document* (dom:create-document
		      'rune-dom:implementation
		      nil nil
		      (dom:create-document-type
		       'rune-dom:implementation
		       "html"
		       "-//W3C//DTD XHTML 1.0 Transitional//EN"
		       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"))))
    (append-nodes *document* ,@chillins)
    *document*))

(defmacro with-xhtml-frameset-document (&body chillins)
  "(with-xhtml-document ( a bunch of child nodes of the document )) --> cxml:dom document
Creates an environment in which the special variable *document* is available
a document is necessary to create dom nodes and the document the nodes end up on
must be the document on which they were created.  At the end of the form, the
complete document is returned.
This sets the doctype to be xhtml transitional."
  `(let ((*document* (dom:create-document
		      'rune-dom:implementation
		      nil nil
		      (dom:create-document-type
		       'rune-dom:implementation
		       "html"
		       "-//W3C//DTD XHTML 1.0 Frameset//EN"
		       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"))))
    (append-nodes *document* ,@chillins)
    *document*))

(defmacro with-xhtml-document-to-file (filename &body chillins)
  "Creates a document block with-document upon which to add the chillins (southern for children).  When the document is complete, it is written out to the specified file."
  `(write-doc-to-file (with-xhtml-document ,@chillins) ,filename))


(defmacro with-html-document-to-file ((filename) &body body)
  "Creates an html-document, writes out the results to filename"
  `(let ((*html-compatibility-mode* T))
    (write-doc-to-file (with-html-document ,@body)
		      ,filename)))



(defmacro with-html-document (&body body)
  "(with-html-document ( a bunch of child nodes of the document )) --> cxml:dom document
Creates an environment in which the special variable *document* is available
a document is necessary to create dom nodes and the document the nodes end up on
must be the document on which they were created.  At the end of the form, the
complete document is returned.
This sets the doctype to be xhtml transitional."
  `(let ((*namespace-prefix-map* nil)
	 (*document* (dom:create-document
		      'rune-dom:implementation
		      nil nil
		      (dom:create-document-type
		       'rune-dom:implementation
		       "html"
		       "-//W3C//DTD HTML 4.01//EN"
		       "http://www.w3.org/TR/html4/strict.dtd")
		      ))
	 (*html-compatibility-mode* T)
	 (*cdata-script-blocks* nil))
    (declare (special *document*))
    (append-nodes *document* ,@body)
    *document*))

(defmacro with-html-document-to-string (() &body body)
  "trys to output a string containing all "
  `(let ((*html-compatibility-mode* T))
     (document-to-string (with-html-document ,@body))))





