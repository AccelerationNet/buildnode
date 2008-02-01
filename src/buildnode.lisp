(in-package :net.acceleration.buildnode)

(cl-interpol:enable-interpol-syntax)

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


(defvar *namespace-prefix-map* '(("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul" . "xul")
				 ("http://www.w3.org/1999/xhtml" . "xhtml")))


(defun calc-complete-tagname (namespace base-tag namespace-prefix-map)
  (let ((prefix (and (not (cxml::split-qname base-tag)) ;not already a prefix
		     (when-bind namespace-entry (assoc namespace namespace-prefix-map :test #'string=)
		       ;;found the given namespace in the map
		       (let ((prefix (cdr namespace-entry)))
			 (declare (type string prefix))
			 (when (> (length prefix) 0)
			   prefix))))))
    (if prefix
	#?"${prefix}:${base-tag}"
	base-tag)))

(defun create-complete-element (document namespace tagname attributes children
					 &optional (namespace-prefix-map *namespace-prefix-map*))
  "Creates an xml element out of all the necessary components.
If the tagname does not contain a prefix, then one is added based on the namespace-prefix map."
  ;;if we don't already have a prefix and we do find one in the map.
  (let* ((tagname (calc-complete-tagname namespace tagname namespace-prefix-map))
	 (elem (dom:create-element-ns document namespace tagname)))
    (when (oddp (length attributes))
      (error "Incomplete attribute-value list. Odd number of elements in ~a" attributes))
    (iterate (for (name value . rest) first attributes then rest)
	     (when (and name)
	       (dom:set-attribute elem (etypecase name
					 (symbol (string-downcase name))
					 (string name))
				  (if (stringp value)
				      value
				      (format nil "~a" value))))
	     (while rest))
    ;;append the children to the element.
    (append-nodes elem children)

    elem))



(defun write-document-to-character-stream (document char-stream)
  "writes a cxml:dom document to a character stream"
  (dom:map-document (cxml:make-namespace-normalizer
		     (cxml:make-character-stream-sink
						     char-stream
						     :canonical nil
						     :indentation nil))
		    document
		    :include-doctype :canonical-notations
		    ))


(defun write-document-to-octet-stream (document octet-stream)
  "writes a cxml:dom document to a character stream"
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-octet-stream-sink
						     octet-stream
						     :canonical nil
						     :indentation 2))
		    document
		    :include-doctype :canonical-notations
		    ))

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
  (with-open-stream (fd (open filename :direction :output :element-type '(unsigned-byte 8)
			      :if-does-not-exist :create
			      :if-exists :supersede))
    (write-document doc fd)
    doc))


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
