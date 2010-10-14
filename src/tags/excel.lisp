(in-package :net.acceleration.buildnode.excel)

;; XML-Spreadsheet Reference:
;;   http://msdn.microsoft.com/en-us/library/Aa140066
;;
;; XML in Excel and the Spreadsheet Component
;;   http://msdn.microsoft.com/en-us/library/aa140062%28office.10%29.aspx

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +excel-namespaces+
    '(("urn:schemas-microsoft-com:office:spreadsheet" . "ss")
      ("urn:schemas-microsoft-com:office:office" . "o")
      ("urn:schemas-microsoft-com:office:excel" . "x")
      ("urn:schemas-microsoft-com:office:component:spreadsheet" . "c")
      ("http://www.w3.org/TR/REC-html40" . "html"))
    "The namespaces used in excel spreadsheet xml"
    ))


(defmacro def-excel-tag (package name xmlns &optional (doc ""))
  "defines a function that will build an xhtml node (on *document*) when called"
  `(def-tag-node ,package ,name
     ,(car (find xmlns +excel-namespaces+ :key #'cdr :test #'string-equal))
     ,doc))

(defmacro excel-tag-package (package nick (&body tags) &optional other-symbols)
  (let ((pack-form
	 `(defpackage ,package
	      (:nicknames ,nick)
	    (:use )
	    (:export
	     ,@(mapcar (compose #'make-symbol #'string-upcase)
		       tags)
	     ,@other-symbols)))
	(snick (string-downcase nick)))
    (eval pack-form)
    ;; eval at macro-expand and execution
    ;; so I can intern and macro expanding looks right
    `(progn
       ,pack-form
       ,@(iter (for tag in tags)
	       (collect `(def-excel-tag
			     ,package
			     ,tag
			   ,snick))))))

(excel-tag-package :urn.schemas-microsoft-com.office.spreadsheet :ss
    ("Alignment" "Border" "Borders" "Cell" "Column" "Comment" 
		 "Data" "Font"  "Interior" "NamedCell" "NamedRange" "Names" 
		 "NumberFormat" "Protection" "Row" "Style" "Styles" "Table" 
		 "Workbook" "Worksheet" )
    (#:currency-cell #:date-cell))

(excel-tag-package :urn.schemas-microsoft-com.office.excel :x
    ("AutoFilter" "AutoFilterAnd" "AutoFilterColumn" "AutoFilterCondition"
		  "AutoFilterOr" "Footer" "Header" "Layout" "PageMargins" "PageSetup"
		  "PhoneticText" "WorksheetOptions") )

(excel-tag-package :urn.schemas-microsoft-com.office.office :o
    ("Smarts" "SmartType" "DocumentProperties" "Author" "LastAuthor"
	      "Created" "Company" "LastSaved" "Version"))

(excel-tag-package :urn.schemas-microsoft-com.office.component.spreadsheet :c
    ("ComponentOptions" "DisplayCustomHeaders" "HideOfficeLogo" "Toolbar"
			"WorksheetOptions"))



(defun ?mso-application (&optional (value "progid=\"Excel.Sheet\""))
  " <?mso-application progid=\"Excel.Sheet\"?> "
  (declare (special *document*))
  (dom:create-processing-instruction
   *document* "mso-application" value))





(defmacro with-excel-workbook (() &body chillins)
  `(let ((*namespace-prefix-map* +excel-namespaces+))
     (with-document ()
       (?mso-application)
       (ss:workbook () ,@chillins))))

(defmacro with-excel-workbook-string (() &body chillins)
  `(with-output-to-string (s)
     (let ((doc (buildnode::with-excel-workbook () ,@chillins)))
       (write-document doc s))))

(defparameter +valid-data-types+
  '("String" "Number" "DateTime")
  "Valid ss:Type values for Data nodes")

(defparameter +obvious-style+
  '(("Date" . "")
    ("DateTime" . "")))

(defun default-excel-styles ()
  (list 
   (ss:style '("ss:ID" "Default" "ss:Name" "Normal")
     (ss:alignment '("ss:Vertical" "Bottom" "ss:Horizontal" "Left"))
     (ss:font `("ss:FontName" "Arial" "x:Family" "Swiss" "ss:Size" "11" "ss:Color" "#000000")))
   (ss:style '("ss:ID" "ShortDate" "ss:Parent" "Default")
     (ss:numberformat '("ss:Format" "Short Date")))
   (ss:style '("ss:ID" "Currency" "ss:Parent" "Default")
     (ss:numberformat '("ss:Format" "_(\"$\"* #,##0.00_);_(\"$\"* \(#,##0.00\);_(\"$\"* \"-\"??_);_(@_)")))))

(defun ss::date-cell (v)
  (ss:cell `("ss:StyleID" "ShortDate")
    (ss:data '("ss:Type" "DateTime")
      v)))
(export 'ss::date-cell :ss)

(defun ss::currency-cell (v)
  (ss:cell `("ss:StyleID" "Currency")
    (ss:data '("ss:Type" "Number")
      v)))

(defun simple-excel-test ( &optional file)
  (let ((res (with-excel-workbook-string ()
	       (ss:styles ()
		 (%default-excel-styles))
	       (ss:worksheet `("ss:Name" "First WorkSheet")
		 (ss:table ()
		   (iter (for i from 0 to 5)
			 (collect 
			     (ss:row ()
			       (ss:cell ()
				 (ss:data '("ss:Type" "String")
				   "Here is string data"))
			       (ss:currency-cell "42838.0311111111111111")
			       (ss::date-cell "2010-08-10T00:00:00.000"))))))
	       )))
    (when file
      (write-string-to-file
       res file
       :if-exists :supersede
       :if-does-not-exist :create))
    res))