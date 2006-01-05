(in-package :net.acceleration.buildnode)

;(declaim (optimize (debug 3)))


(defparameter +buildnode-output-directory+ #P"/var/local/lisp/xul/"
				  "The root location to output XUL files.
Must be addressable by the lisp system. Read from command line as input parameter?")


(defvar *common-javascript*
  '((:JSHelper "/JSControls/JSHelper.js")))


(defun create-complete-element (document namespace tagname attributes children)
  (let ((e (dom:create-element-ns document namespace tagname)))
	 (when (oddp (length attributes))
		(error "Incomplete attribute-value list. Odd number of elements in ~a" attributes))
	 (iterate (for name = (pop attributes))
				 (for value = (pop attributes))
				 (while name)
				 (dom:set-attribute e (string-downcase name) (string value)))
	 (iterate (for child in children)
				 (dom:append-child e (if (stringp child)
												 (dom:create-text-node document child)
												 child)))
	 e))
;(create-complete-element *document* +xul-namespace+ "box" '(("id" "asdf")) '())

(defun <?xml-stylesheet (href &optional (type "txt/css" ))
  (declare (special *document*))
  (let (( attrib-string (format nil " type=~s href=~s  " type href)))
	 (dom:create-processing-instruction *document*  "xml-stylesheet" attrib-string)))



(defun write-document-to-character-stream (document char-stream)
  (let ((buf (flex:with-output-to-sequence (stream)
					(write-document-to-octet-stream document stream))))
	 
	 (flex:with-input-from-sequence (input buf)
		;;echo the flexi stream to output
		(with-open-stream (echo (make-echo-stream (flex:make-flexi-stream input)
																char-stream))
		  (loop for line = (read-line echo nil)
				  while line)))))


(defun write-document-to-octet-stream (document octet-stream)
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-octet-stream-sink octet-stream))
						  document
						  :include-doctype :canonical-notations
						  :include-xmlns-attributes nil))

(defun write-document (document &optional (out-stream *standard-output*))
  "Write the document to the designated out-stream, or *standard-ouput* by default."
  (case (stream-element-type out-stream) 
	 ('character (write-document-to-character-stream document out-stream))
	 ('octet (write-document-to-octet-stream document out-stream))))




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