(in-package :net.acceleration.buildnode)

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (asdf:oos 'asdf:load-op 'buildnode-excel))

(defun simple-excel-test ( &optional file)
  (let ((doc (excel:with-excel-workbook () ;; declare a new excel workbook type document
	       ;; add a single worksheet
	       (ss:worksheet `("ss:Name" "First WorkSheet")
		 (ss:table ()
		   (ss:row ()
		     (set-attribute
		      (ss:string-cell "PAGE TITLE" "Title")
		      "ss:MergeAcross" "6"))
		   (excel:set-index 5
                    (ss:row ()
                      (excel:set-index 3 (ss:header-cell "String Data"))
                      (ss:header-cell "Currency Data")
                      (ss:header-cell "Double Currency Data")
                      (ss:header-cell "Date Data")))
		   (iter (for i from 0 to 1)
                     (collect
                         (ss:row ()
                           (excel:set-index 3 (ss:string-cell "Here is string data"))
                           (ss:currency-cell "42838.0311111111111111")
                           (ss:currency-cell 42838.0311111111111111d0)
                           (ss::date-cell "2010-08-10T00:00:00.000")))))))))
    (when file (write-doc-to-file doc file))
    (values (document-to-string doc) doc)))
