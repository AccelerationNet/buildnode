(in-package :buildnode)

(defun compute-attributes (element xmlnsp defaultp)
  (let ((results '()))
    (dom:do-node-list (a (dom:attributes element))
      (when (and (or defaultp (dom:specified a))
                 (or xmlnsp (not (cxml::xmlns-attr-p (cxml::rod (dom:name a))))))
        (push
         (sax:make-attribute :qname (dom:name a)
                             :value (dom:value a)
			     :local-name (dom:local-name a)
			     :namespace-uri (dom:namespace-uri a)
                             :specified-p (dom:specified a))
         results)))
    (reverse results)))

(defgeneric dom-walk (handler value &key
		     include-xmlns-attributes
		     include-doctype
		     include-default-values
		     recode)
  (:documentation "An in order traversal of a subset of the dom, that calls
   the appropriate sax events"))

(defmethod dom-walk (handler (document dom:document)
		     &key (include-xmlns-attributes sax:*include-xmlns-attributes*)
		     include-doctype
		     include-default-values
		     (recode (and #+rune-is-integer (typep document 'utf8-dom::node))))
  (declare (ignorable recode))
  #+rune-is-integer
  (when recode
    (setf handler (make-recoder handler #'utf8-string-to-rod)))
  
  (sax:start-document handler)
  (when include-doctype
    (let ((doctype (dom:doctype document)))
      (when doctype
	(sax:start-dtd handler
		       (dom:name doctype)
		       (dom:public-id doctype)
		       (dom:system-id doctype))
	(ecase include-doctype
	  (:full-internal-subset
	     (when (slot-boundp doctype 'dom::%internal-subset)
	       (sax:start-internal-subset handler)
	       (dolist (def (dom::%internal-subset doctype))
		 (apply (car def) handler (cdr def)))
	       (sax:end-internal-subset handler)))
	  (:canonical-notations
	     ;; need notations for canonical mode 2
	     (let* ((ns (dom:notations doctype))
		    (a (make-array (dom:length ns))))
	       (when (plusp (dom:length ns))
		 (sax:start-internal-subset handler)
		 ;; get them
		 (dotimes (k (dom:length ns))
		   (setf (elt a k) (dom:item ns k)))
		 ;; sort them 
		 (setf a (sort a #'cxml::rod< :key #'dom:name))
		 (loop for n across a do
		   (sax:notation-declaration handler
					     (dom:name n)
					     (dom:public-id n)
					     (dom:system-id n)))
		 (sax:end-internal-subset handler)))))
	(sax:end-dtd handler))))
  (dom:do-node-list (child (dom:child-nodes document))
    (dom-walk handler child
	      :include-xmlns-attributes include-xmlns-attributes 
	      :include-doctype include-doctype
	      :include-default-values include-default-values))
  (sax:end-document handler))

(defmethod dom-walk (handler (n dom:element)
		     &key (include-xmlns-attributes sax:*include-xmlns-attributes*)
		     include-doctype
		     include-default-values &allow-other-keys)
  (let ((attlist
	 (compute-attributes n
	  include-xmlns-attributes
	  include-default-values))
	(uri (dom:namespace-uri n))
	(lname (dom:local-name n))
	(qname (dom:tag-name n)))
    (sax:start-element handler uri lname qname attlist)
    (dom:do-node-list (child (dom:child-nodes n))
      (dom-walk handler child
		:include-xmlns-attributes include-xmlns-attributes 
		:include-doctype include-doctype
		:include-default-values include-default-values))
    (sax:end-element handler uri lname qname)))

(defmethod dom-walk (handler (n dom:cdata-section) &key &allow-other-keys)
  (sax:start-cdata handler)
  (sax:characters handler (dom:data n))
  (sax:end-cdata handler))

(defmethod dom-walk (handler (n dom:text)  &key &allow-other-keys)
  (sax:characters handler (dom:data n)))

(defmethod dom-walk (handler (n dom:comment)  &key &allow-other-keys)
  (sax:comment handler (dom:data n)))

(defmethod dom-walk (handler (n dom:processing-instruction)  &key &allow-other-keys)
  (sax:processing-instruction handler (dom:target n) (dom:data n)))

(defmethod dom-walk (handler (n dom:entity-reference)  &key &allow-other-keys)
  (sax:unescaped handler (format nil "&~A;" (dom:name n))))