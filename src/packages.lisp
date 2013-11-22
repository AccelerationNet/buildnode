(in-package :common-lisp-user)

(defpackage :net.acceleration.buildnode
	 (:nicknames :buildnode)
  (:use :common-lisp :cxml :iterate)
  (:shadow #:cdata :value :name :body)
  (:export
   #:dom-walk
   #:text-of-dom-snippet
   #:join-text
   #:set-attributes
   #:push-new-attribute
   #:push-new-attributes
   #:set-attribute
   #:get-attribute
   #:add-css-class
   #:add-css-classes
   #:remove-css-class
   #:remove-css-classes
   #:css-classes
   #:?xml-stylesheet
   #:?processing-instruction
   #:CDATA
   #:create-complete-element
   #:write-document
   #:inner-html
   #:insert-html-string
   #:with-xul-document
   #:with-document
   #:with-document-to-file
   #:with-xhtml-document
   #:with-xhtml-frameset-document
   #:with-xhtml-document-to-file
   #:with-html-document
   #:with-html-document-to-file
   #:with-html5-document
   #:with-html5-document-to-file
   #:with-html5-document-to-string
   #:append-nodes
   #:insert-nodes
   #:add-children
   #:insert-children
   #:script-block
   #:stylesheet-block
   #:*document*
   #:*html-compatibility-mode*
   #:*cdata-script-blocks*
   #:*namespace-prefix-map*
   #:dom-comment
   #:document-to-string
   #:scoped-dom-builder
   #:make-scoped-dom-builder
   #:remove-attribute
   #:remove-attributes
   #:insert-nodes
   #:with-html-document-to-string
   #:dom-template
   #:make-output-sink
   #:def-tag-node
   #:remove-all-children
   #:with-html-snippet
   #:with-xhtml-snippet
   #:write-doc-to-file))


