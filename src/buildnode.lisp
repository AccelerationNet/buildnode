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
  (cxml:make-extid "-//W3C//DTD XHTML 1.0 Transitional//EN"
		   (puri:uri #?|file://${(merge-pathnames "xhtml1-transitional.dtd" *load-truename* )}|)))

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

(defun inner-html (string &optional (tag "div"))
  "Will wrap the input in a tag (which is neccessary from CXMLs perspective)"
  (handler-bind ((warning #'(lambda (condition)
			      (declare (ignore condition))
			      (muffle-warning))))
    
    (let ((doc (cxml:parse #?|<${tag}>${string}</${tag}>| (cxml-dom:make-dom-builder)
			   :dtd *xhtml1-transitional-extid*)))
      (dom:import-node *document* (dom:first-child doc) T))))

(defun append-nodes (to-location &rest chillins)
  "appends a bunch of dom-nodes (chillins) to the location specified"
  (let ((doc (if (typep to-location 'rune-dom::document)
		 to-location
		 (dom:owner-document to-location))))
    (labels ((map-tree (children)
	       (when children
		 (iterate (for child in children)
			  (if (listp child)
			      (map-tree child)
			      (dom:append-child
			       to-location
			       (cond
				 ((stringp child)
				  (dom:create-text-node doc child))
				 ((numberp child)
				  (dom:create-text-node doc
							#?"${child}"))
				 (T child)
				 )))))))
      (map-tree chillins)
    )
    to-location))

(defvar *html-compatibility-mode* nil)
(defvar *namespace-prefix-map* '(("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul" . "xul")
				 ("http://www.w3.org/1999/xhtml" . "xhtml")))


(defun calc-complete-tagname (namespace base-tag namespace-prefix-map)
  (let ((prefix (and namespace-prefix-map
		     (not (cxml::split-qname base-tag)) ;not already a prefix
		     (when-bind namespace-entry (assoc namespace namespace-prefix-map :test #'string=)
		       ;;found the given namespace in the map
		       (let ((prefix (cdr namespace-entry)))
			 (declare (type string prefix))
			 (when (> (length prefix) 0)
			   prefix))))))
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
  (if (stringp value) value (princ-to-string value)))

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
  (loop for (attr val . rest) = attribute-p-list then rest
	while attr
	do (set-attribute elem attr val))
  elem)

(defun create-complete-element (document namespace tagname attributes children
					 &optional (namespace-prefix-map *namespace-prefix-map*))
  "Creates an xml element out of all the necessary components.
If the tagname does not contain a prefix, then one is added based on the namespace-prefix map."
  ;;if we don't already have a prefix and we do find one in the map.
  (let* ((tagname (calc-complete-tagname namespace tagname namespace-prefix-map))
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

(defun write-document-to-character-stream (document char-stream)
  "writes a cxml:dom document to a character stream"
  (write-normalized-document-to-sink
   document
      (if *html-compatibility-mode*
       (chtml:make-character-stream-sink char-stream)
       (cxml:make-character-stream-sink char-stream
				    :canonical nil
				    :indentation nil))))

(defun write-document-to-octet-stream (document octet-stream)
  "writes a cxml:dom document to a character stream"
  (write-normalized-document-to-sink
   document
   (if *html-compatibility-mode*
       (chtml:make-octet-stream-sink octet-stream)
       (cxml:make-octet-stream-sink octet-stream
				    :canonical nil
				    :indentation 2))))

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
  `(let ((*document* (dom:create-document
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
  `(let ((*document* (dom:create-document
		      'rune-dom:implementation
		      nil nil
		      (dom:create-document-type
		       'rune-dom:implementation
		       "html"
		       "-//W3C//DTD HTML 4.01//EN"
		       "http://www.w3.org/TR/html4/strict.dtd")
		      ))
	 (*html-compatibility-mode* T))
    (declare (special *document*))
    (append-nodes *document* ,@body)
    *document*))
