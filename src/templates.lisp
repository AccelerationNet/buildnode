(in-package :net.acceleration.buildnode)
(cl-interpol:enable-interpol-syntax)

;;;; While this lays the ground work for templates inside of the dom, you 
;;;; will still need to register templates with some template system (see cl-emb:register-emb)
;;;; and define a sax:process-instruction method specialized on template-processing-sink
;;;; (see ucw-buildnode/src/app-modules/adw-application)

(defclass template-processing-sink () ())

(defclass template-node (rune-dom::processing-instruction)
  ((dom:node-name :accessor dom:node-name :initarg :node-name :initform "template-node")))

(defmacro dom-template (attribs &body body)
  "Children should be a plist of values to fill in"
  `(let ((attribs (append ,attribs
			  (list :body (list ,@body)))))
     (unless (getf attribs :type)
       (setf (getf attribs :type) "emb"))
     (make-instance 'template-node
		    :owner *document*
		    :target (getf attribs :type)
		    :data attribs)))

(defgeneric %process-template-data (sink value)
  (:documentation "Turns a value into a string for inclusion in a template"))

(defmethod %process-template-data (sink (value dom:element))
  (with-output-to-string (str)
    (let ((parent-ystream (cxml::sink-ystream sink))
	  (new-ystream (runes::make-character-stream-ystream str)))
      (unwind-protect
	   (progn
	     (setf (runes:ystream-encoding new-ystream) (runes:ystream-encoding parent-ystream))
	     (setf (cxml::sink-ystream sink) new-ystream)
	     (dom:walk sink value)
	     (runes::flush-ystream (cxml::sink-ystream sink)))
	(setf (cxml::sink-ystream sink) parent-ystream)))))

(defmethod %process-template-data (sink (value list))
  (with-output-to-string (s)
    (iter (for v in value)
	  (write-sequence (%process-template-data sink v) s))))

(defmethod %process-template-data (sink value) (declare (ignore sink))
  (princ-to-string value))

(defun process-template-data (sink data)
  (iter (for (key value . rest) = data)
	(while key) (setf data rest)
	(collect key)
	(collect (%process-template-data sink value))))

