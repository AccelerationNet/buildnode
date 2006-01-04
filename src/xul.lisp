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

(defmacro with-xul-document ( &body chillins)
  `(let ((*document*  (cxml-dom:create-document)))
	 (declare (special *document*))
	 (iterate (for child in (list ,@chillins))
				 (dom:append-child *document* child))
	 *document*))

(defun <?xml-stylesheet (href &optional (type "txt/css" ))
  (declare (special *document*))
  (let (( attrib-string (format nil " type=~s href=~s  " type href)))
	 (dom:create-processing-instruction *document*  "xml-stylesheet" attrib-string)))

(setf test-doc
		(with-xul-document
		  (<?xml-stylesheet "chrome://global/skin/" )
		  (<?xml-stylesheet "/css/MC.css" )
		  (dom:create-element-ns *document* uri qname)
		  (xul:window
			:title "MC Administration"
			(xhtml:div (xhtml:div))
			(xul:script  :type "text/javascript" :src "/JSControls/JSHelper.js")
			(xul:label :class "h1" "MobileCampus Administration"))
		  (xul:vbox :flex "1"
						(xul:hbox :flex "1"))))

(buildnode::write-document
 test-doc
 *standard-output*)


;<?xml-stylesheet href="chrome://global/skin/" type="text/css"?>
;<?xml-stylesheet href="/css/MC.css" type="text/css"?>
;<window 
;   onload="MC.init('/');"
;   xmlns="http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul"
;   xmlns:html="http://www.w3.org/1999/xhtml"
;   title="MC Administration" >
;   <script language="javascript" type="text/javascript" src="/JSControls/JSHelper.js" />
;   <script language="javascript" type="text/javascript" src="/JSControls/ADWSoap.js" />
;   <script language="javascript" type="text/javascript" src="/JSControls/Xul.js" />
;   <script language="javascript" type="text/javascript" src="/JSControls/Http.js" />
;   <script language="javascript" type="text/javascript" src="/JSControls/Widgets/Xul/Tree.js" />
;   <script language="javascript" type="text/javascript" src="/JSControls/Controls.js" />   
;   <script language="javascript" type="text/javascript" src="/script/MC.js" />

;   <script language="javascript" type="text/javascript" src="/script/MC.Admin.js" />   
;   <script language="javascript" type="text/javascript" src="/script/MC.Events.js" />
;   <script language="javascript" type="text/javascript" src="/script/MC.Offers.js" />
;   <script language="javascript" type="text/javascript" src="/script/MC.Referrers.js" />
;   <script language="javascript" type="text/javascript" src="/script/MC.Messengers.js" />
;   <script language="javascript" type="text/javascript" src="/script/MC.FileUp.js" />         
   
   
   
;      <label class="errorText"></label>
;      <label class="h1">MobileCampus Administration</label>   
   
   
;   <vbox flex="1">
;      <hbox flex="1">


