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
  "flattens a list
ex: (a (b (c d))) -> (a b c d)"
  (labels ((helper (list &optional result)				 
				 (if (null list) result
					  (helper (cdr list)
								 (nconc result
										  (if (listp (car list))
												(helper (car list))
												(list (car list))))))))
	 (helper list)))

(defmacro def-tag-node (package name prefix namespace doc  )
  "Defines a tag function in the package with the name and prefix specified
for example: :net.acceleration.xul \"box\" \"xul\" will create a function #'box in the :net.acceleration.xul
lisp namespace. When this function is called it wil create a 'xul:box' node in the xmlns provided in the namespace param"
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
  "defines a function that will build an xul node (on *document*) when called"
  (declare (ignore attributes))
  `(def-tag-node :net.acceleration.xul ,name "xul" +xul-namespace+ ,doc))

(defmacro def-html-tag (name doc)
  "defines a function that will build an xhtml node (on *document*) when called"
  `(def-tag-node :net.acceleration.xhtml ,name "xhtml" +xhtml-namespace+ ,doc))

(defun ?xml-stylesheet (href &optional (type "txt/css" ))
  "adds an xml-stylesheet processing instruction to the cxml:dom document bound to the
special variable *document*"
  (declare (special *document*))
  (let (( attrib-string (format nil " type=~s href=~s  " type href)))
	 (dom:create-processing-instruction *document*  "xml-stylesheet" attrib-string)))

(defun net.acceleration.xul:groupbox-with-caption (caption-label &optional attributes &rest children)
  "creates a group box that contains a caption element with the appropriate label"
  (xul:groupbox attributes
					 (append (list (xul:caption (list :label caption-label)))
								children)))

