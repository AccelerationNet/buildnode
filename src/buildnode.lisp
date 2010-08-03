(in-package :net.acceleration.buildnode)
(cl-interpol:enable-interpol-syntax)

(defun xmls-to-dom-snippet ( sxml &key (namespace "http://www.w3.org/1999/xhtml"))
  (declare (special *document*))
  (etypecase sxml
    (string sxml)
    (list (destructuring-bind (tagname attrs . kids) sxml
	    (create-complete-element
	     *document* namespace
	     tagname
	     (flatten attrs)
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

(defmethod text-of-dom-snippet ((el T) &optional splice)
  (declare (ignorable el splice))
  ())

(defmethod text-of-dom-snippet ((el dom:document) &optional splice )
  "get all of the textnodes of a dom:document and return that string with splice between each character"
  (let ((strings (loop for kid across (dom:child-nodes el)
		       collecting (text-of-dom-snippet kid splice))))
    (join-strings
     (flatten (if splice
		  (insert-between strings splice)
		  strings)))))

(defmethod text-of-dom-snippet ((el dom:element) &optional splice)
  "get all of the textnodes of a dom:element and return that string with splice between each character"
  (let ((strings (loop for kid across (dom:child-nodes el)
		       collecting (text-of-dom-snippet kid splice))))
    (join-strings
     (flatten (if splice
		  (insert-between strings splice)
		  strings)))))

(defmethod text-of-dom-snippet ((el dom:text) &optional splice)
  (declare (ignorable splice))
  (dom:data el))

(defclass scoped-dom-builder (rune-dom::dom-builder)
  ())
(defmethod sax:start-document ((db scoped-dom-builder)))
(defmethod sax:end-document ((db scoped-dom-builder))
  (rune-dom::document db))

;;;; I think we might be able to use this as a dom-builder for a more efficient
;;;; version of the inner-html function
(defun make-scoped-dom-builder (node)
  (let ((builder (make-instance 'scoped-dom-builder)))
    (setf (rune-dom::document builder) (dom:owner-document node)
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
  "Will wrap the input in a tag (which is neccessary from CXMLs perspective)
can validate the html against a DTD if one is passed, can use
*xhtml1-transitional-extid* for example."
  (handler-bind ((warning #'(lambda (condition)
			      (declare (ignore condition))
			      (muffle-warning))))
    (let ((node (dom:create-element *document* tag)))
      (cxml:parse #?|<${tag} xmlns="${ namespace-uri }">${string}</${tag}>|
		  (make-instance 'html-whitespace-remover
				 :chained-handler (make-scoped-dom-builder node))
		  :dtd dtd)
      (dom:first-child node))))

(defun append-nodes (to-location &rest chillins)
  "appends a bunch of dom-nodes (chillins) to the location specified"
  (let ((doc (if (typep to-location 'rune-dom::document)
		 to-location
		 (dom:owner-document to-location))))
    (adwutils:map-tree-leaves
     #'(lambda (child)
	 (typecase child
	   (dom:node (dom:append-child to-location child))
	   (string (dom:append-child
		    to-location
		    (dom:create-text-node doc child)))
	   (array (iter (for elem in-sequence child)
			(append-nodes to-location elem)))
	   (T (dom:append-child
	       to-location
	       (dom:create-text-node doc (princ-to-string child))))))
     chillins)
    to-location))

(defun insert-nodes (to-location index &rest chillins)
  "insert a bunch of dom-nodes (chillins) to the location specified"
  (let ((doc (if (typep to-location 'rune-dom::document)
		 to-location
		 (dom:owner-document to-location))))
    (adwutils:map-tree-leaves
     #'(lambda (child)
	 (dom:insert-before
	  to-location
	  (typecase child
	    (dom:node child)
	    (string (dom:create-text-node doc child))
	    (T (dom:create-text-node doc (princ-to-string child))))
	  (elt (dom:child-nodes to-location) index)))
     chillins)
    to-location))

(defvar *html-compatibility-mode* nil)
(defvar *cdata-script-blocks* T "Should script blocks have a cdata?")
(defvar *namespace-prefix-map* '(("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul" . "xul")
				 ("http://www.w3.org/1999/xhtml" . "xhtml")))

(defun get-prefix (namespace &optional (namespace-prefix-map
					*namespace-prefix-map*))
  (when namespace-prefix-map
  (the (or null string)
    (cdr (assoc namespace namespace-prefix-map :test #'string=)))))

(defun calc-complete-tagname (namespace base-tag namespace-prefix-map)
  (let ((prefix (and namespace-prefix-map
		     (not (cxml::split-qname base-tag)) ;not already a prefix
		     (when-bind prefix (get-prefix namespace namespace-prefix-map)
		       ;;found the given namespace in the map
		       (when (> (length (the string prefix)) 0)
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

(defun get-attribute (elem attribute)
  "Gets the value of an attribute on an element"
  (dom:get-attribute-ns
   elem
   nil
   (prepare-attribute-name attribute)))

(defun set-attribute (elem attribute value)
  "Sets an attribute and passes the elem through, returns the elem"
  (dom:set-attribute-ns
   elem
   nil
   (prepare-attribute-name attribute)
   (prepare-attribute-value value))
  elem)

(defun remove-attribute (elem attribute)
  "removes an attribute and passes the elem through, returns the elem"
  (dom:remove-attribute
   elem
   (prepare-attribute-name attribute))
  elem)

(defmethod add-css-class ((el dom:element) new-class)
  (let ((css-classes (split-sequence:split-sequence #\space (get-attribute el :class)
						    :remove-empty-subseqs t)))
    (pushnew new-class css-classes :test #'string=)
    (set-attribute el :class (format nil "~{~a~^ ~}" css-classes))))

(defun push-new-attribute (elem attribute value)
  "if the attribute is not on the element then put it there with the specified value,
   returns the elem and whether or not the attribute was set"
  (values elem
   (when (null (get-attribute elem attribute))
     (set-attribute elem attribute value)
     T)))

(defun push-new-attributes (elem &rest attribute-p-list)
  "for each attribute in the plist push-new into the attributes list of the elem, returns the elem"
  (loop for (attr val . rest) = attribute-p-list then rest
	while attr
	do (push-new-attribute elem attr val))
  elem)

(defun set-attributes (elem &rest attribute-p-list)
  "set-attribute for each attribute specified in the plist, returns the elem"
  (loop for (attr val) on attribute-p-list by #'cddr
	do (set-attribute elem attr val))
  elem)

(defun add-children (elem &rest kids)
  "adds some kids to an element and return that element"
  (iter (for kid in kids)
	(dom:append-child elem kid))
  elem)

(defun create-complete-element (document namespace tagname attributes children
					 &optional (namespace-prefix-map *namespace-prefix-map*))
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
    (declare (special *document*))
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
  "Binary write-out a document. will create/overwrite any existing file named the same."
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
    (declare (special *document*))
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
    (declare (special *document*))
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

(defvar *document* ()
  "A variable for document building")

(defmacro with-html-document (&body body)
  "(with-html-document ( a bunch of child nodes of the document )) --> cxml:dom document
Creates an environment in which the special variable *document* is available
a document is necessary to create dom nodes and the document the nodes end up on
must be the document on which they were created.  At the end of the form, the
complete document is returned.
This sets the doctype to be xhtml transitional."
  `(let ((buildnode::*namespace-prefix-map* nil)
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