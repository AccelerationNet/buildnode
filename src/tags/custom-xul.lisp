(in-package :net.acceleration.buildnode)

(defun net.acceleration.xul:groupbox-with-caption (caption-label &optional attributes &rest children)
  "creates a group box that contains a caption element with the appropriate label"
  (xul:groupbox attributes
					 (append (list (xul:caption (list :label caption-label)))
								children)))
