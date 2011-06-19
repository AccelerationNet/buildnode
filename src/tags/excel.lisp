(in-package :net.acceleration.buildnode)

(eval-always
  (unless (find-package :net.acceleration.buildnode.excel)
    (defpackage :net.acceleration.buildnode.excel
	(:nicknames :excel-xml :buildnode-excel :excel)
      (:use :common-lisp :buildnode :iterate)
      (:export
       #:with-excel-workbook
       #:with-excel-workbook-string
       #:with-excel-workbook-file
       #:default-excel-styles
       #:?mso-application
       #:def-excel-tag
       #:set-index
       #:link-to
       #:build-excel-cell-reference
       #:set-merge
       ))))

(in-package :net.acceleration.buildnode.excel)

;; XML-Spreadsheet Reference:
;;   http://msdn.microsoft.com/en-us/library/Aa140066
;;
;; XML in Excel and the Spreadsheet Component
;;   http://msdn.microsoft.com/en-us/library/aa140062%28office.10%29.aspx

(buildnode::eval-always
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
	     ,@(mapcar (lambda (x) (make-symbol (string-upcase x)))
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

(buildnode::eval-always
  (excel-tag-package :urn.schemas-microsoft-com.office.spreadsheet :ss
		     ("Alignment" "Border" "Borders" "Cell" "Column" "Comment" 
				  "Data" "Font"  "Interior" "NamedCell" "NamedRange" "Names" 
				  "NumberFormat" "Protection" "Row" "Style" "Styles" "Table" 
				  "Workbook" "Worksheet")
		     (#:currency-cell #:date-cell #:string-cell #:header-cell #:title-cell))

  (excel-tag-package :urn.schemas-microsoft-com.office.excel :x
		     ("AutoFilter" "AutoFilterAnd" "AutoFilterColumn" "AutoFilterCondition"
				   "AutoFilterOr" "Footer" "Header" "Layout" "PageMargins"
				   "PageSetup" "PhoneticText" "WorksheetOptions"
				   "TabColorIndex"))

  (excel-tag-package :urn.schemas-microsoft-com.office.office :o
		     ("Smarts" "SmartType" "DocumentProperties" "Author" "LastAuthor"
			       "Created" "Company" "LastSaved" "Version"))

  (excel-tag-package :urn.schemas-microsoft-com.office.component.spreadsheet :c
		     ("ComponentOptions" "DisplayCustomHeaders" "HideOfficeLogo" "Toolbar"
					 "WorksheetOptions")))



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
  `(buildnode::write-doc-to-file
    (with-excel-workbook () ,@chillins)
    ,file))

(defmacro with-excel-workbook-string (() &body chillins)
  `(buildnode::document-to-string
    (with-excel-workbook () ,@chillins)))

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
  (add-style "String" :parent "Default"
	     :styles (list (ss:numberformat '("ss:Format" "@"))))
  (excel::add-style
     "Notes" :parent "String"
     :styles (list (ss:font `("ss:FontName" "Arial" "x:Family" "Swiss"
					    "ss:Size" "9" "ss:Color" "#666666"))))
  (add-style "HyperLink" :parent "String"
	     :styles (list (ss:font `("ss:Color" "#0000FF" "ss:Underline" "Single"))))
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
		 (ss:numberformat '("ss:Format" "@"))))

  (add-style
   "LastTitle" :parent "Title"
   :styles (list (ss:borders ()
		   (ss:border '("ss:Position" "Bottom"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "2"
				"ss:Color" "#213B92")))))

  (add-style
   "TotalText" :parent "String"
   :styles (list (ss:interior '("ss:Color" "#E2E6F4" "ss:Pattern" "Solid"))
		 (ss:borders ()
		   (ss:border '("ss:Position" "Top"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "1"
				"ss:Color" "#213B92")))))
   (add-style
   "TotalAmount" :parent "Currency"
   :styles (list (ss:interior '("ss:Color" "#E2E6F4" "ss:Pattern" "Solid"))
		 (ss:borders ()
		   (ss:border '("ss:Position" "Top"
				"ss:LineStyle" "Continuous"
				"ss:Weight" "1"
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

(defun special-char-p (s)
  "Check if a string has any special characters"
  (loop for char across s
	thereis (find char '(#\space #\, #\. #\! #\@ #\# #\$ #\%
			     #\^ #\& #\* #\| #\( #\) #\- #\+))))

(defun build-excel-cell-reference (sheet-name row cell)
  (let (*print-pretty*)
    (format nil "#~A!~A~A"
	    (if (special-char-p sheet-name)
		(format nil "'~A'" sheet-name)
		sheet-name)
	    row cell)))

(defun link-to (item reference)
  (set-attributes item "ss:HRef" reference "ss:StyleID" "HyperLink"))

(defun ss::date-cell (v &optional (style-id "ShortDate"))  
  (ss:cell `("ss:StyleID" ,style-id)
    (ss:data '("ss:Type" "DateTime") v )))

(defun ss::currency-cell (v &optional (style-id "Currency"))
  (let ((sv (etypecase v
	      (number (format nil "~0,2F" v))
	      (string v))))
    (ss:cell `("ss:StyleID" ,style-id)
      (ss:data '("ss:Type" "Number") sv ))))

(defun ss::string-cell (v &optional (style-id "String"))
  (ss:cell `("ss:StyleID" ,style-id)
    (ss:data '("ss:Type" "String")
      v)))

(defun ss::title-cell (v &optional (style-id "Title"))
  (ss:string-cell v style-id ))

(defun ss::header-cell (v &optional (style-id "Header"))
  (ss:string-cell v style-id ))

(defun set-index (val item)
  (set-attribute item "ss:Index" val))

(defun set-merge (col-cnt item)
  (set-attribute item "ss:MergeAcross" (princ-to-string col-cnt)))

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

