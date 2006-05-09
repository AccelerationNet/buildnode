(in-package :net.acceleration.buildnode)

(defun xhtml:tab-container (&rest tabs)
  (let ((tab-titles nil) (panels nil))
	 (flet ((make-tab (panel-id tab tab-title)
				 (dom:set-attribute tab "id" panel-id)
				 (setf tab-titles (append tab-titles (list tab-title)))
				 (setf panels (append panels (list tab)))
				 ))
		(loop for (title tab) in tabs
				for tab-num from 1
				for tab-left from 10 by 140
				for tab-title = (xhtml:tab-title title tab-num)
				for panel-id = (format nil "panel~a" tab-num)
				do (make-tab panel-id tab tab-title))
		(append tab-titles panels))))

(defun xhtml:tab-title (title tab-num)
  (let ((left-pos (+ 10 (* 140 (1- tab-num)))))
	 (xhtml:div (list :class "tab" :onmouseover "hover(this);"
							:style (format nil "left: ~a;" left-pos)
							:onclick (format nil "showPanel(~a);" tab-num)
							:id (format nil "tab~a" tab-num)
							:onmouseout (format nil "setState(~a)" tab-num))
					title)))

(defun xhtml:tab (title &rest body)
  (list
	title
	(xhtml:div '(:class "panel") body)))

(defun xhtml:iframe-tab (title src)
  (list
	title
	(xhtml:iframe (list :class "panel" :src src) "Loading...")))

