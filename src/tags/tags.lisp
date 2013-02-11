(in-package :net.acceleration.buildnode)
(cl-interpol:enable-interpol-syntax)

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


;;get a ref to the swank::symbol-indentation in such a way that if this file
;; gets evaled multiple times, we always have a ref to the original function,
;; not one of our overrides
(defvar *tags-indentation-hints* (make-hash-table))
(when (boundp 'swank::*application-hints-tables*)
  (pushnew *tags-indentation-hints* swank::*application-hints-tables*))


(defmacro def-tag-node (package name  namespace docstring &optional fn-name )
  "Defines a tag function in the package with the name and prefix specified
for example: :net.acceleration.xul \"box\" \"xul\" will create a function #'box in the :net.acceleration.xul
lisp namespace. When this function is called it will create a 'xul:box' node in the xmlns provided in the namespace param"
  (let* ((evaled-name (eval name))
	 (name (or fn-name
		   (intern (string-upcase evaled-name) (eval package))))
	 (tagname evaled-name))
    `(progn
      (CL:defun ,name (&optional attributes &rest children )
	,@(when docstring (list docstring))
	(declare (special *document*))
	(create-complete-element *document*
				 ,namespace
				 ,tagname
				 attributes
				 children))
       (setf (gethash ',name *tags-indentation-hints*) 1)
      )))

(defun ?xml-stylesheet (href &optional (type "text/css" ))
  "adds an xml-stylesheet processing instruction to the cxml:dom document bound to the
special variable *document*"
  (let (( attrib-string (format nil " type=~s href=~s  " type href)))
    (?processing-instruction "xml-stylesheet" attrib-string)))

(defun ?processing-instruction (target data)
  (declare (special *document*))
  (dom:create-processing-instruction *document* target data))

(defun dom-comment (text)
  "Insert a dom comment into the current *document*"
  (declare (special *document*))
  (dom:create-comment *document* text))

(defun net.acceleration.buildnode:CDATA ( data )
  (declare (special *document*))
  (dom:create-cdata-section *document* data))

(defun stylesheet-block (list &optional (type "text/css"))
  "given a list of urls, will build a list of ?xml-stylesheet nodes pointing to the appropriate urls"
  (mapcar #'(lambda (sheet)
	      (?xml-stylesheet sheet type))
	  list))

(defun script-block (fn list-of-urls)
  "given a list of urls, will build a list of script nodes pointing to the appropriate urls.
Pass in #'xul:script or #'xhtml:script as the first argument"
  (mapcar #'(lambda (url)
	      (funcall fn (list :language "javascript" :type "text/javascript" :src url )))
	  list-of-urls))

(defun make-script-fn (fn-script url)
  (funcall fn-script `(:src ,url
			    "type" "text/javascript")))

(defun make-script-block-fn (fn-script js)
  (declare (special buildnode:*document*))
  (funcall fn-script
	   (list "type" "text/javascript")
	   (if buildnode:*cdata-script-blocks*
	       (dom:create-cdata-section buildnode:*document* js)
	       (dom:create-text-node buildnode:*document* js))))
