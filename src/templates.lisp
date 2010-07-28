(in-package :net.acceleration.buildnode)
(cl-interpol:enable-interpol-syntax)

;;;; While this lays the ground work for templates inside of the dom, you 
;;;; will still need to register templates with some template system (see cl-emb:register-emb)
;;;; and define a sax:process-instruction method specialized on template-processing-sink
;;;; (see ucw-buildnode/src/app-modules/adw-application)

(defclass template-processing-sink () ())
(defclass html-template-processing-sink (template-processing-sink) ())

(defclass template-node (rune-dom::processing-instruction)
  ((dom:node-name :accessor dom:node-name :initarg :node-name :initform "template-node")))

(defmacro dom-template (attribs &body body)
  "Children should be a plist of values to fill in"
  `(let ((attribs (append ,attribs
			  (list :body (list ,@body)))))
     (unless (getf attribs :type)
       (let* ((name (merge-pathnames (getf attribs :name) "."))
	      (type (when name (pathname-type name))))
	 (setf (getf attribs :type) (or type "emb"))))
     (make-instance 'template-node
		    :owner *document*
		    :target (getf attribs :type)
		    :data attribs)))

