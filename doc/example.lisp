(in-package :buildnode)
(setf test-doc
		(with-xul-document
		  (<?xml-stylesheet "chrome://global/skin/" )
		  (<?xml-stylesheet "/css/MC.css" )
		  (xul:window '(:title "MC Administration"
									 :onload "asdf")
						  (xhtml:div '()  (xhtml:div))
						  (xul:script  '(:type "text/javascript" :src "/JSControls/JSHelper.js"))
						  (xul:label '(:class "h1") "MobileCampus Administration")
						  (xul:vbox '(:flex "1")
										(xul:hbox '(:flex "1"))))))

(with-document-to-file "foo.xul"
  (<?xml-stylesheet "chrome://global/skin/" )
  (<?xml-stylesheet "/css/MC.css" )
		  (xul:window '(:title "MC Administration"
							 :onload "asdf")
						  (xhtml:div '()  (xhtml:div))
						  (xul:script  '(:type "text/javascript" :src "/JSControls/JSHelper.js"))
						  (xul:label '(:class "h1") "MobileCampus Administration")
						  (xul:vbox '(:flex "1")
										(xul:hbox '(:flex "1")))))

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