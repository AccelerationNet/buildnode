(in-package :net.acceleration.buildnode)
(defvar *html-compatibility-mode* nil "should we render as text/html compatibile?")

(defclass html-compatibility-sink (cxml:sax-proxy)
  ((add-comments-p :accessor add-comments-p
		   :initform nil))
  (:documentation "A SAX sink to make application/xml+html work as text/html"))

(defun make-html-compatibility-sink (chained-handler)
  "makes a new html-compatibility-sink with the given next handler"
  (make-instance 'html-compatibility-sink
		 :chained-handler chained-handler))

(defmethod sax:start-element ((handler html-compatibility-sink) uri lname qname attrs)
  "takes note when we enter a script block"
  (when (equal "script" lname)
    (setf (add-comments-p handler) T))
  (sax:start-element (cxml:proxy-chained-handler handler) uri lname qname attrs))

(defmethod sax:end-element ((handler html-compatibility-sink) uri lname qname)
  "ensures we never have a self-closing script block"
  (when (add-comments-p handler)
    (setf (add-comments-p handler) nil)
    ;;prevent self-closing script tags
    (cxml::maybe-close-tag (cxml:proxy-chained-handler handler)))
  (sax:end-element (cxml:proxy-chained-handler handler) uri lname qname))

(defmethod sax:characters ((handler html-compatibility-sink) data)
  "If we're in a script block, this adds HTML commented JS comments around the
CDATA bits, so all the parsers work.

I had to copy and modify from the (sax:character (handler sink) data) method,
does not call the base."
  (let ((sink (cxml:proxy-chained-handler handler)))
    (flet ((maybe-comment (comment)
	     "add the comment when we're configured to do so"
	     (when (add-comments-p handler)
	       (sax:comment sink comment))))
      (cxml::maybe-close-tag sink)
      (cond
	((and (eq (car (cxml::stack sink)) :cdata)
	      (not (cxml::canonical sink))
	      (not (search #"]]" data)))
	 (when (cxml::indentation sink)
	   (cxml::sink-fresh-line sink))
	 (maybe-comment "/*")
	 (cxml::%write-rod #"<![CDATA[" sink)
	 (maybe-comment "*/")
	 ;; XXX signal error if body is unprintable?
	 (map nil (lambda (c) (cxml::%write-rune c sink)) data)
	 (maybe-comment "/*")
	 (cxml::%write-rod #"]]>" sink)
	 (maybe-comment "*/"))
	(t
	 (if (cxml::indentation sink)
	     (cxml::unparse-indented-text data sink)
	     (let ((y (cxml::sink-ystream sink)))
	       (if (cxml::canonical sink)
		   (loop for c across data do (cxml::unparse-datachar c y))
		   (loop for c across data do (cxml::unparse-datachar-readable c y))))))))))