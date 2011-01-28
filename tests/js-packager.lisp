(in-package :buildnode-test)
(cl-interpol:enable-interpol-syntax)


'(flet ((button-control ()
	 (declare (special use-js-file))
	 (use-js-file 'behaviour)
	 (list (xhtml:button '(:label "Foo"))
	       (xhtml:button '(:label "Bar")))))
  (write-document
   (with-document
     (js-insertion-block ( #'xhtml-script-tag)
       (use-js-file 'grid)
       (xhtml:html
	'()
	(xhtml:head
	 '()
	 (xhtml:div '() "above")
	 insert-script-here
	 (xhtml:div '() "below")
	 )
	(xhtml:body
	 '()
	 (xhtml:div
	  '()
	  (use-js-file "js/initgrid.js")
	  (button-control))))
       (xul:window '(:id "fuck")
		   (js-insertion-block (#'xul-script-tag)
		     (xul:button '(:label "foo"))
		     insert-script-here
		     (xul:button '(:label "bar"))
		     (use-js-file "js/foo your mom in her dirty bam")))))))
