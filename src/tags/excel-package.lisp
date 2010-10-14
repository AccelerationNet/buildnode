(in-package :common-lisp-user)

(defpackage :net.acceleration.buildnode.excel
    (:nicknames :excel-xml :buildnode-excel)
  (:use :common-lisp :buildnode :iterate :arnesi)
  (:export
   #:with-excel-workbook
   #:with-excel-workbook-string
   #:default-excel-styles
   #:?mso-application
   #:def-excel-tag
   #:set-index
   #:link-to
   ))