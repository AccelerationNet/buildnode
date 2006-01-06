(in-package :net.acceleration.buildnode)

;(declaim (optimize (debug 2)))

(defparameter +xul-namespace+ "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")
(defparameter +xhtml-namespace+ "http://www.w3.org/1999/xhtml")

;(defparameter +xul-core-attributes+
;  (mapcar #'(lambda (s) (intern (string-upcase s)))
;			 '("align" "allowevents" "allownegativeassertions" "class" 
;				"coalesceduplicatearcs" "collapsed" "container" "containment" 
;				"context" "contextmenu" "datasources" "dir" "empty" "equalsize" 
;				"flags" "flex" "height" "hidden" "id" "insertafter" "insertbefore" 
;				"left" "maxheight" "maxwidth" "menu" "minheight" "minwidth" 
;				"mousethrough" "observes" "ordinal" "orient" "pack" "persist" "popup" 
;				"position" "preference-editable" "ref" "removeelement" "sortDirection" 
;				"sortResource" "sortResource2" "statustext" "style" "template" 
;				"tooltip" "tooltiptext" "top" "uri" "wait-cursor" "width"))) 

(defun flatten (list)
  (labels ((helper (list &optional result)				 
				 (if (null list) result
					  (helper (cdr list)
								 (nconc result
										  (if (listp (car list))
												(helper (car list))
												(list (car list))))))))
	 (helper list)))


(defmacro def-tag-node (package name prefix namespace doc  )
  (let* ((evaled-name (eval name))
			(name (intern (string-upcase evaled-name) (eval package)))
			(tagname (string-downcase (concatenate 'string prefix ":" evaled-name))))
	 `(CL:defun ,name (&optional attributes &rest children )
		,doc
		(declare (special *document*))
		(create-complete-element *document*
		 ,namespace
		 ,tagname
		 attributes
		 (flatten children)))))

(defmacro def-xul-element (name doc &rest attributes)
  (declare (ignore attributes))
  `(def-tag-node :net.acceleration.xul ,name "xul" +xul-namespace+ ,doc))

(defmacro def-html-tag (name doc)
  `(def-tag-node :net.acceleration.xhtml ,name "xhtml" +xhtml-namespace+ ,doc))



(defmacro with-document (&body chillins)
  `(let ((*document*  (cxml-dom:create-document)))
	 (declare (special *document*))
	 (let ((children (flatten (list ,@chillins))))
		(iterate (for child in children)
					(dom:append-child *document* child))
		*document*)))


(defmacro with-document-to-file (filename &body chillins)
  `(with-output-to-file (stream ,filename :if-exists :supersede)
	 (write-document (with-document ,@chillins) stream)))