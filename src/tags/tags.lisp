(in-package :net.acceleration.buildnode)

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

(defvar *symbol-indentation-tag-cache* (make-hash-table :test #'eq)
  "Hash table to hold indenation information for swank for tags.")

;;get a ref to the swank::symbol-indentation in such a way that if this file
;; gets evaled multiple times, we always have a ref to the original function,
;; not one of our overrides
(defvar +old-swank-symbol-indentation+ nil)
(unless +old-swank-symbol-indentation+
  (setf +old-swank-symbol-indentation+ #'swank::symbol-indentation))

(defun swank::symbol-indentation (sym)
  (aif (gethash sym *symbol-indentation-tag-cache*)
       it
       (funcall +old-swank-symbol-indentation+ sym)))


(defmacro def-tag-node (package name  namespace doc  )
  "Defines a tag function in the package with the name and prefix specified
for example: :net.acceleration.xul \"box\" \"xul\" will create a function #'box in the :net.acceleration.xul
lisp namespace. When this function is called it will create a 'xul:box' node in the xmlns provided in the namespace param"
  (let* ((evaled-name (eval name))
	 (name (intern (string-upcase evaled-name) (eval package)))
	 (tagname (string-downcase evaled-name)))
    `(progn
      (CL:defun ,name (&optional attributes &rest children )
	,doc
	(declare (special *document*))
	(create-complete-element *document*
				 ,namespace
				 ,tagname
				 attributes
				 (kmrcl:flatten children)))
      (setf (gethash ',name *symbol-indentation-tag-cache*) 1))))

(defmacro def-xul-element (name doc &rest attributes)
  "defines a function that will build an xul node (on *document*) when called"
  (declare (ignore attributes))
  `(def-tag-node :net.acceleration.xul ,name  +xul-namespace+ ,doc))

(defmacro def-html-tag (name doc)
  "defines a function that will build an xhtml node (on *document*) when called"
  `(def-tag-node :net.acceleration.xhtml ,name  +xhtml-namespace+ ,doc))

(defun ?xml-stylesheet (href &optional (type "text/css" ))
  "adds an xml-stylesheet processing instruction to the cxml:dom document bound to the
special variable *document*"
  (let (( attrib-string (format nil " type=~s href=~s  " type href)))
    (?processing-instruction "xml-stylesheet" attrib-string)))

(defun ?processing-instruction (target data)
  (declare (special *document*))
  (dom:create-processing-instruction *document* target data))

(defun net.acceleration.buildnode:CDATA ( data )
  (declare (special *document*))
  (dom:create-cdata-section *document* data))

(defun script-block (fn list-of-urls)
  "given a list of urls, will build a list of script nodes pointing to the appropriate urls.
Pass in #'xul:script or #'xhtml:script as the first argument"
  (mapcar #'(lambda (url)
	      (funcall fn (list :language "javascript" :type "text/javascript" :src url )))
	  list-of-urls))

(defun stylesheet-block (list &optional (type "text/css"))
  "given a list of urls, will build a list of ?xml-stylesheet nodes pointing to the appropriate urls"
  (mapcar #'(lambda (sheet)
	      (?xml-stylesheet sheet type))
	  list))
