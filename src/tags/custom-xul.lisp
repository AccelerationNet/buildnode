(in-package :net.acceleration.buildnode)

(defun net.acceleration.xul:groupbox-with-caption (caption-label &optional attributes &rest children)
  "creates a group box that contains a caption element with the appropriate label"
  (xul:groupbox attributes
					 (append (list (xul:caption (list :label caption-label)))
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