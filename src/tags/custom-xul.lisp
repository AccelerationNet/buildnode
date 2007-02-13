(in-package :net.acceleration.buildnode)

(defun net.acceleration.xul:groupbox-with-caption (caption-label group-box-attributes caption-attributes &rest children)
  "creates a group box that contains a caption element with the appropriate label"
  (xul:groupbox group-box-attributes
		(append (list (xul:caption (append caption-attributes (list :label caption-label))))
			children)))

(defun net.acceleration.xul:radiogroup-with-options (attributes options &optional selected-key)
  "takes a list of attributes for the radiogroup and then a list of options.  Options are specified as an alist (label . value) or string to use for both"
  (xul:radiogroup attributes
		  (xul:label-value-list #'xul:radio options selected-key)))

(defun net.acceleration.xul::_tabbox-with-tabs (&rest tab-defs)
  "creates the tabbox, tabs, tabpanels from a rest of (\"title\" body)  Usages:
 (_tabbox-with-tabs
    (list \"tab title\" (list contents))
    (list \"tab two\" (list contents)))"
  (let ((tabs (mapcar (lambda (tab-def)
			(xul:tab (list :label (car tab-def))))
		      tab-defs))
	(tabpanels (mapcar (lambda (tab-def)
			     (let ((tab-contents (cdr tab-def)))
			       (xul:tabpanel '(:orient "vertical" :style "overflow:auto;" :flex 1 :align "stretch") (list tab-contents))))
			   tab-defs)))
    (xul:tabbox '(:flex 1 :align "stretch")
		(xul:tabs nil tabs)
		(xul:tabpanels '(:flex 1 :align "stretch") tabpanels))))

(defmacro net.acceleration.xul:tabbox-with-tabs (&rest tab-defs)
  "creates the tabbox, tabs, tabpanels from a rest of (\"title\" body)  Usages:
 (tabbox-with-tabs
    (\"tab title\" (list contents))
    (\"tab two\" (list contents)))"
  `(xul::_tabbox-with-tabs
    ,@(mapcar
     (lambda (elem)
       (append (list 'list) elem))
     tab-defs)))

(defun net.acceleration.xul:label-value-list (xul-elem-fn items &optional selected-key)
  "takes a xul element function, and generates a list of them, one for each item.  An item is a string or a (label . value) cons"
  (flet ((make-attrs (label &optional value)
	   (append (list :label label)
		   (if (not (null value))
		       (list :value value))
		   (if (equal label selected-key)
		       '(:selected "true")))))
    (mapcar
     (lambda (item)
       (funcall xul-elem-fn (if (consp item)
				(make-attrs (car item) (cdr item))
				(make-attrs item))))
     items)))

(defun net.acceleration.xul:menulist-with-items (attributes items &optional selected-key)
  "Adds a menulist with the given items.  Items can be a list of labels, or a cons list of (label.value)"
  (xul:menulist attributes
		(xul:menupopup nil
			       (xul:label-value-list #'xul:menuitem items selected-key))))


(defun net.acceleration.xul:boolean-radio (attributes &key selected null-label (true-label "Yes") (false-label "No"))
  "Adds a menulist with the given items.  Items can be a list of labels, or a cons list of (label.value)"
  (xul:radiogroup attributes
		  (xul:radio (list :label true-label :value "true"))
		  (xul:radio (list :label false-label :value "false"))
		  (when null-label
		    (xul:radio (list :label null-label :value "null")))))
