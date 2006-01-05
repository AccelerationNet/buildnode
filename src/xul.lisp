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


(defmacro def-xul-element (name doc &rest attributes)
  (declare (ignore attributes))
  `(def-tag-node :net.acceleration.xul ,name "xul" +xul-namespace+ ,doc))

(defmacro def-html-tag (name doc)
  `(def-tag-node :net.acceleration.xhtml ,name "xhtml" +xhtml-namespace+ ,doc))

;(def-html-tag "a" "Defines an anchor")

(defmacro def-tag-node (package name prefix namespace doc  )
  (let* ((evaled-name (eval name))
			(name (intern (string-upcase evaled-name) (eval package)))
			(tagname (string-downcase (concatenate 'string prefix ":" evaled-name))))
	 `(CL:defun ,name (attributes &rest children )
		,doc
		(declare (special *document*))
		(create-complete-element *document*
		 ,namespace
		 ,tagname
		 attributes
		 children))))

(defmacro with-xul-document ( &body chillins)
  `(let ((*document*  (cxml-dom:create-document)))
	 (declare (special *document*))
	 (iterate (for child in (list ,@chillins))
				 (dom:append-child *document* child))
	 *document*))

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
