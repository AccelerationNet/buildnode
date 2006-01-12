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
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-octet-stream-sink octet-stream))
						  document
						  :include-doctype :canonical-notations
						  :include-xmlns-attributes nil))

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
	 (append-nodes *document* ,@chillins)))

(defmacro with-document-to-file (filename &body chillins)
  "Creates a document block with-document upon which to add the chillins
(southern for children).  When the document is complete, it is written out to the specified file."
  `(with-output-to-file (stream ,filename :if-exists :supersede)
	 (write-document (with-document ,@chillins) stream)))



;;creating a xul file
;(write-dom-to-file "Admin-main.xul"
;  (xul:window-template
;		  :title "My window"
;		  :onload "foo()"
;		  :scripts (:JSHelper ("/script/MC.js" :depends-on :JSHelper))
;		  (xul:hbox :flex 1
;						(xhtml:a
;						 :href "http://google.com"
;						 "FGI")
;						(xul:labeled-groupbox "Main Menu"
;												(generic-node :ns :xhtml :node-name "hr"
;																  :attributes '(:width "50%"
;																					 :color "green")))
;						(xul:flashy-groupbox :label "Main Menu"
;									 (generic-node :node-name "xhtml:hr"
;														:attributes '((:class "foo" "bar" "bast") (:width . "70%") (:color . "green")))))))


;;;Defining a new xul control
;(def-control xulcontrol:flashy-groupbox (label &attributes :xul-core  caption-class
;													&body children)
;  :javascripts '(("/scripts/flashy-groupbox.js" :depends-on :jshelper "/scripts/flashy.js"))
;  (xul:groupbox :attributes xul:core
;					 :onmouseover "flash (this)"
;					 :children (list (xul:caption :label label :class caption-class)
;										  children)))



;;the expansion of the above control
;(defun xulcontrol:flashy-groupbox (&key id class label caption-class attributes &body children )
;  (let* ((xul:core (make-attr-collection 'xul:id id
;													  'xul:class class)))
;	 (add-javascript-requirement javascripts)
;	 (xul:groupbox :onmouseover "flash (this)"
;						:attributes xul:core
;						:children (append (list (xul:caption :label label :class caption-class))
;												children)) ))

;;;the macro to do that
;(defmacro def-control (conrol-name control-lambda-list &key javascripts &body children)
;  )