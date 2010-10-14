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
    (#:currency-cell #:date-cell #:string-cell #:header-cell #:title-cell))

(excel-tag-package :urn.schemas-microsoft-com.office.excel :x
    ("AutoFilter" "AutoFilterAnd" "AutoFilterColumn" "AutoFilterCondition"
		  "AutoFilterOr" "Footer" "Header" "Layout" "PageMargins" "PageSetup"
		  "PhoneticText" "WorksheetOptions"))

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

(defvar *workbook-node*)
(defvar *styles-node*)

(defmacro with-excel-workbook (() &body chillins)
  `(let ((*namespace-prefix-map* +excel-namespaces+))
     (with-document ()
       (?mso-application)
       (let* ((*workbook-node* (ss:workbook ()))
	      (*styles-node* (ss:styles ())))
	 (add-default-excel-styles)
	 (apply #'add-children *workbook-node*
		*styles-node* (list ,@chillins))))))

(defun remove-style (id)
  (iter (for kid in (dom:child-nodes *styles-node*))
	(when (string-equal id (get-attribute kid "ss:ID"))
	  (dom:remove-child *styles-node* kid))))

(defun add-style (id &key name parent styles)
  (remove-style id)  
  (add-children
   *styles-node*
   (ss:style (append
	      (when id
		(list "ss:ID" id))
	      (when name
		(list "ss:Name" name))
	      (when parent
		(list "ss:Parent" parent)))
     styles)))

(defmacro with-excel-workbook-file ((file) &body chillins)
  `(with-output-to-file (s ,file :if-exists :supersede :if-does-not-exist :create)
     (let ((doc (with-excel-workbook () ,@chillins)))
       (write-document doc s))))

(defmacro with-excel-workbook-string (() &body chillins)
  `(with-output-to-string (s)
     (let ((doc (with-excel-workbook () ,@chillins)))
       (write-document doc s))))

(defparameter +valid-data-types+
  '("String" "Number" "DateTime")
  "Valid ss:Type values for Data nodes")

(defparameter +obvious-style+
  '(("Date" . "")
    ("DateTime" . "")))

(defun add-default-excel-styles ()
  (add-style
   "Default" :name "Normal"
   :styles (list (ss:alignment '("ss:Vertical" "Bottom" "ss:Horizontal" "Left"))
		 (ss:font `("ss:FontName" "Arial" "x:Family" "Swiss"
					  "ss:Size" "11" "ss:Color" "#000000"))))
  (add-style
   "ShortDate" :parent "Default"
   :styles (list (ss:numberformat '("ss:Format" "Short Date"))))
  
  (add-style
   "Currency" :parent "Default"
   :styles (list (ss:numberformat
		     '("ss:Format" "_(\"$\"* #,##0.00_);_(\"$\"* \(#,##0.00\);_(\"$\"* \"-\"??_);_(@_)"))))
  
  (add-style
   "Title" :parent "Default"
   :styles (list (ss:alignment '("ss:Vertical" "Bottom" "ss:Horizontal" "Center"))
		 (ss:font '("ss:Bold" "1" "ss:Size" "13"))
		 (ss:borders ()
		   (ss:border '("ss:Position" "Bottom"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "2"
				"ss:Color" "#213B92")))))

  (add-style
   "LastTitle" :parent "Title"
   :styles (list (ss:borders ()
		   (ss:border '("ss:Position" "Bottom"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "2"
				"ss:Color" "#213B92")))))
  
  (add-style
   "Header" :parent "Default"
   :styles (list (ss:alignment '("ss:Vertical" "Bottom" "ss:Horizontal" "Center"))
		 (ss:interior '("ss:Color" "#213B92" "ss:Pattern" "Solid"))
		 (ss:font '("ss:Color" "#FFFFFF" "ss:Bold" "1"))
		 (ss:borders ()
		   (ss:border '("ss:Position" "Bottom"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "2"
				"ss:Color" "#FF6730"))
		   (ss:border '("ss:Position" "Left"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "2"
				"ss:Color" "#FF6730"))
		   (ss:border '("ss:Position" "Right"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "2"
				"ss:Color" "#FF6730"))
		   (ss:border '("ss:Position" "Top"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "2"
				"ss:Color" "#FF6730")))
		 (ss:numberformat '("ss:Format" "@"))))
  )

(defun build-excel-cell-reference (sheet-name row cell)
  (format nil "#~A!~A~A" sheet-name row cell))

(defun link-to (item reference)
  (set-attributes item "ss:HRef" reference))

(defun ss::date-cell (v &optional (style-id "ShortDate"))
  (ss:cell `("ss:StyleID" ,style-id)
    (ss:data '("ss:Type" "DateTime")
      v)))

(defun ss::currency-cell (v &optional (style-id "Currency"))
  (ss:cell `("ss:StyleID" ,style-id)
    (ss:data '("ss:Type" "Number")
      v)))

(defun ss::string-cell (v &optional (style-id "Default"))
  (ss:cell `("ss:StyleID" ,style-id)
    (ss:data '("ss:Type" "String")
      v)))

(defun ss::title-cell (v &optional (style-id "Title"))
  (ss:string-cell v style-id ))

(defun ss::header-cell (v &optional (style-id "Header"))
  (ss:string-cell v style-id ))

(defun set-index (val item)
  (set-attribute item "ss:Index" val))

(defun simple-excel-test ( &optional file)
  (let ((res (with-excel-workbook-string ()
	       (ss:worksheet `("ss:Name" "First WorkSheet")
		 (ss:table ()
		   (ss:row ()
		     (set-attribute
		      (ss:string-cell "PAGE TITLE" "Title")
		      "ss:MergeAcross" "6"))
		   (set-index 3
		    (ss:row ()
		      (set-index 3 (ss:header-cell "String Data"))
		      (ss:header-cell "Currency Data")
		      (ss:header-cell "Date Data")))
		   (iter (for i from 0 to 5)
			 (collect 
			     (ss:row ()
			       (set-index 3 (ss:string-cell "Here is string data"))
			       (ss:currency-cell "42838.0311111111111111")
			       (ss::date-cell "2010-08-10T00:00:00.000"))))))
	       )))
    (when file
      (write-string-to-file
       res file
       :if-exists :supersede
       :if-does-not-exist :create))
    res))

(defun validate-excel-output (node)
  "Tries to help validate that your output will be readable by excel"
  (let ((ht (make-hash-table :test #'equal)))
    (labels ((doit (node)
	       (cond
		 ((string-equal "ss:Worksheet" (dom:tag-name node))
		  (when (gethash ht (get-attribute node "ss:Name"))
		    (error "The worksheet names are not unique!"))
		  (setf (gethash ht (get-attribute node "ss:Name")) T))))
	     (walker (node)
	       (doit node)
	       (iter (for kid in (dom:child-nodes node))
		     (walker kid))))
      (walker node))))

