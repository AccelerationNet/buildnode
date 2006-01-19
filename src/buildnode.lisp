(in-package :net.acceleration.buildnode)

(defvar *common-javascript*
  '((:JSHelper "/JSControls/JSHelper.js")))


(defun append-nodes (to-location &rest chillins)
  "appends a bunch of dom-nodes (chillins) to the location specified"
  (let ((children (kmrcl:flatten chillins))
		  (doc (if (subtypep (type-of to-location) 'rune-dom::document)
					  to-location
					  (dom:owner-document to-location))))
	 (iterate (for child in children)
				 (when child
					(dom:append-child
					 to-location
					 (if (stringp child)
						  (dom:create-text-node doc child)
						  child))))
	 to-location))

(defun create-complete-element (document namespace tagname attributes children)
  "Creates a fully qualified xml element"
  (let ((e (dom:create-element-ns document namespace tagname)))
	 (when (oddp (length attributes))
		(error "Incomplete attribute-value list. Odd number of elements in ~a" attributes))
	 (iterate (for name = (pop attributes))
				 (for value = (pop attributes))
				 (while name)
				 (dom:set-attribute e (string-downcase name) (string value)))
	 (apply #'append-nodes (append (list e) children))
	 e))



(defun write-document-to-character-stream (document char-stream)
  "writes a cxml:dom document to a character stream"
  (let ((buf (flex:with-output-to-sequence (stream)
					(write-document-to-octet-stream document stream))))
	 
	 (flex:with-input-from-sequence (input buf)
		;;echo the flexi stream to output
		(with-open-stream (echo (make-echo-stream (flex:make-flexi-stream input)
																char-stream))
		  (loop for line = (read-line echo nil)
				  while line)))))


(defun write-document-to-octet-stream (document octet-stream)
    "writes a cxml:dom document to a character stream"
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-octet-stream-sink
																	  octet-stream
																	  :canonical nil
																	  :indentation 2))
						  document
						  :include-doctype nil
						  ))

(defun write-document (document &optional (out-stream *standard-output*))
  "Write the document to the designated out-stream, or *standard-ouput* by default."
  (case (stream-element-type out-stream) 
	 ('character (write-document-to-character-stream document out-stream))
	 ('octet (write-document-to-octet-stream document out-stream))))


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
  "Creates a document block with-document upon which to add the chillins
(southern for children).  When the document is complete, it is written out to the specified file."
  `(with-output-to-file (stream ,filename :if-exists :supersede)
	 (write-document (with-document ,@chillins) stream)))
