(in-package :net.acceleration.buildnode)
(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (asdf:oos 'asdf:load-op 'buildnode-xhtml))


(defun simple-html-test ( &key file )
  (let ((doc
	 ;; sets up document context (this form will return a dom:document
	 (buildnode:with-html-document 
	   ;; attributes are plists, if the att name is a string casing is preseverd
	   ;; if the att name is a symbol it is downcased to a string
	   (let ((title-tag (xhtml:h1 '(:class "title") "My Page Title")))
	     (flet ((form-ctl (name  &key type value)
		      (xhtml:input `(:type ,type :name ,name
					   ,@(when value (list :value value))))))
	       (xhtml:html '("ID" "my-html") 
		 (xhtml:head ()
		   (xhtml:title () "My Page Title"))
		 (xhtml:body ()
		   (xhtml:div '(:id "page")
		     (xhtml:div '(:id "header")
		       (xhtml:div '(:class "title-holder")
			 ;; functions that manipulate the dom, try to
			 ;; return the element, so that these manipulations
			 ;; can be done in line
			 (set-attribute title-tag :title "my page title"))
		       (xhtml:div '(:class"nav")

			 ;; functions can return dom snippets that can be manipulated
			 ;; and added to the dom
			 (set-attributes (form-ctl "chk" :type "checkbox") :checked :checked)
			 (xhtml:label ()
			   "Enter your name"
			   (add-css-class (form-ctl "txt" :value "foo bar" ) "lablebox"))
		
			 (xhtml:ul '(:class "nav-items")
			   ;; tags can be collected, and manipulated
			   ;; lists and vectors of dom nodes are handled appropriately
			   ;; ie: this UL will have 12 children 
			   (list (xhtml:li () "a test")
				 (xhtml:li () "a second test"))
			   (iter (for i from 0 to 9)
				 (collect
				     (xhtml:li () (format nil "Nav ~A" i)))))))
	    
		     (xhtml:div '(:id "content")
		       (xhtml:h2 `(:id "article-title" :title "totally the title of this article"
				       :name "article-title-name")
			 "What its about"))
		     (xhtml:div '(:id "footer" :class "layout box bottom-aligned"
				  :title "this-be-the-page-footer-yo")
		       (xhtml:div '(:class "contact-info")
		
			 ;; Text content can be spliced into the dom using the inner-html
			 ;; function.  This call will result in a div that contains span(dyn) being
			 ;; created and inserted into the contact-info span
			 (inner-html "<span id=\"dyn\"> Text content being spliced right in </span>")
		
			 (xhtml:span '(:class "name")
			   "Acceleration.net")
			 (xhtml:span '(:class "phone")
			   "352-335-6500x123")))))))))))
    (when file (write-doc-to-file doc file))
    (values (document-to-string doc) doc)))

