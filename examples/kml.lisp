(in-package :net.acceleration.buildnode)
(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (asdf:oos 'asdf:load-op 'buildnode-kml))

(defun simple-kml-test (&key file)
  (let ((doc
	 ;; sets up document context (this form will return a dom:document
	 (kml:with-kml-document ()
	  (kml:kml ()
	    (kml:document ()
	      (kml:name () "Test KML Document")
	      (kml:placemark ()
		(kml:name () "Acceleration.net")
		(kml:address () "2831 NW 41ST ST
Gainesville, FL 32606")
		(kml:description () "http://www.acceleration.net")
		(kml:style ()
		  (kml:iconstyle ()
		    (kml:icon ()
		      (kml:href () "http://www.acceleration.net/favicon.ico"))))
		))))))
    (when file (write-doc-to-file doc file))
    (values (document-to-string doc) doc)))

(defun simple-kml-from-marker-test ( &key file )
  (let ((doc
	 ;; sets up document context (this form will return a dom:document
	 (kml:make-doc-from-markers
	  (list
	   (kml:make-marker :name "Acceleration.net"
			    :description "http://www.acceleration.net"
			    :icon-href "http://www.acceleration.net/favicon.ico"
			    :address
			    "2831 NW 41ST ST
Gainesville, FL 32606"))
	  "Test KML Document")))
    (when file (write-doc-to-file doc file))
    (values (document-to-string doc) doc)))

