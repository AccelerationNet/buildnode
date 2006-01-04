(in-package :net.acceleration.buildnode)

(declaim (optimize (debug 3)))

(defparameter +xul-namespace+ "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")

(defparameter +xul-core-attributes+
  (mapcar #'(lambda (s) (intern (string-upcase s)))
			 '("align" "allowevents" "allownegativeassertions" "class" 
				"coalesceduplicatearcs" "collapsed" "container" "containment" 
				"context" "contextmenu" "datasources" "dir" "empty" "equalsize" 
				"flags" "flex" "height" "hidden" "id" "insertafter" "insertbefore" 
				"left" "maxheight" "maxwidth" "menu" "minheight" "minwidth" 
				"mousethrough" "observes" "ordinal" "orient" "pack" "persist" "popup" 
				"position" "preference-editable" "ref" "removeelement" "sortDirection" 
				"sortResource" "sortResource2" "statustext" "style" "template" 
				"tooltip" "tooltiptext" "top" "uri" "wait-cursor" "width"))) 


(defmacro def-xul-element (sname documentation &rest attributes  )
  (let* ((name (intern (string-upcase sname) :net.acceleration.xul))
			(attributes (append +xul-core-attributes+
									  (iterate (for (entry _) in attributes)
												  (unless (find (string-upcase entry)
																	 +xul-core-attributes+
																	 :test #'string=)
													 (collect (intern (string-upcase entry))))))))
	 
	 `(CL:defun ,name (&rest arguments )
		,documentation
		(declare (special *document*))
		(yaclml::attribute-bind (&attribute attributes ,@attributes &body children) arguments
			 (create-complete-element *document*
											  +xul-namespace+
											  ,sname
											  (CL:append attributes
															 (iterate (for key in '(,@attributes))
																		 (for value in (list ,@attributes))
																		 (when value
																			(collect (cons key value)))))
											  children)))))

;(DEF-XUL-ELEMENT "action"
;                 "Should be contained within a rule element. It is used to specify the generated  content for each matched node. Within the action, attributes are parsed for   resource and variable references .

;link:http://www.xulplanet.com/tutorials/xultu/advrules.html
;src:http://www.xulplanet.com/reference/elemref/ref_action.html")

(defmacro create-xul-document (filename &body chillins)
  `(let ((*document* (cxml-dom:create-document)))
	 (declare (special *document*))
	 (iterate (for child in (list ,@chillins))
				 (dom:append-child *document* child))
;	 (dom:set-attribute-ns (dom:document-element *document*)  "http://www.w3.org/2000/xmlns/" "xmlns:html" "http://www.w3.org/1999/xhtml")
	 (with-output-to-file (fd (merge-pathnames ,filename) :if-exists :supersede)
		(write-document *document* fd))))

(create-xul-document "foo.xul"
  (dom:CREATE-ATTRIBUTE-NS *document* "http://www.w3.org/1999/xhtml" "xmlns:html")
  (xul:window ;  :onload "MC.init ('/');"
				  :title "MC Administration"
				  (xul:script  :type "text/javascript" :src "/JSControls/JSHelper.js")
				  (xul:label :class "h1" "MobileCampus Administration")))