(in-package :net.acceleration.buildnode)


(defun net.acceleration.xul:groupbox-with-caption (caption-label group-box-attributes caption-attributes &rest children)
  "creates a group box that contains a caption element with the appropriate label"
  (xul:groupbox group-box-attributes
					 (append (list (xul:caption (append caption-attributes (list :label caption-label))))
								children)))

(defun net.acceleration.xul:radiogroup-with-options (attributes options &optional selected-key)
  "takes a list of attributes for the radiogroup and then a list of options.  Options are specified as an alist (label . value) or string to use for both"
  (flet ((make-radio-attrs (label value)
			  (append
				(list :label label :value value)
				(if (equal label selected-key)
					 '(:selected "true")))))
	 (xul:radiogroup attributes
						  (mapcar
							(lambda (option)
							  (xul:radio 
								(if (consp option)
									 (make-radio-attrs (car option) (cdr option))
									 (make-radio-attrs option option))))
							options))))

(defun net.acceleration.xul::_tabbox-with-tabs (&rest tab-defs)
  "creates the tabbox, tabs, tabpanels from a rest of ('title' body)  Usages:
 (_tabbox-with-tabs
    (list 'tab title' (list contents))
    (list 'tab two' (list contents)))"
  (let ((tabs (mapcar (lambda (tab-def)
								(xul:tab (list :label (car tab-def))))
							 tab-defs))
		  (tabpanels (mapcar (lambda (tab-def)
									  (let ((tab-contents (cdr tab-def)))
										 (xul:tabpanel '(:orient "vertical") (list tab-contents))))
									tab-defs)))
	 (xul:tabbox nil
					 (xul:tabs nil tabs)
					 (xul:tabpanels nil tabpanels))))

(defmacro net.acceleration.xul:tabbox-with-tabs (&rest tab-defs)
  "creates the tabbox, tabs, tabpanels from a rest of ('title' body)  Usages:
 (tabbox-with-tabs
    ('tab title' (list contents))
    ('tab two' (list contents)))"
  `(xul::_tabbox-with-tabs ,@
	 (mapcar
	  (lambda (elem)
		 (append (list 'list) elem))
	  tab-defs)))