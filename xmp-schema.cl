;; -*- mode: common-lisp; package: net.xmp -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

;; $Id: xmp-schema.cl,v 1.2 2003/12/11 05:38:48 layer Exp $

;; XML Schema support

(in-package :net.xmp)

(defpackage :net.xmp
  (:export
   #:schema-collected-part
   #:schema-simple-part
   #:schema-named-part
   #:schema-types
   #:schema-target
   #:schema-collect-target
   #:schema-file-connector
   #:schema-decode-attribute
   ))

(eval-when (compile load eval)
  (defpackage :net.xmp.schema
    (:use)
    (:export 
     "schema"
     "element"
     "complexType"
     "attribute"
     "simpleType"
     "attributeGroup"
     "group"
     "sequence"
     "choice"
     "any"
     "annotation"
     "anyAttribute"
     "simpleContent"
     "complexContent"
     "documentation"
     "restriction"
     "pattern"
     "list"
     "extension"
     "all"
     "base"

     ;; simpleType names (incomplete)

     "anyType"
     "ur-type"

     "string"
     "boolean"
     "float"
     "double"
     "decimal"
     "duration"
     "dateTime"
     "time"
     "date"
     "gYearMonth"
     "gYear"
     "gMonthDay"
     "gDay"
     "gMonth"
     "hexBinary"
     "base64Binary"
     "anyURI"
     "QName" 
     "NOTATION"
     "normalizedString"
     "token"
     "language"
     "IDREFS"
     "ENTITIES"
     "NMTOKEN"
     "NMTOKENS"
     "Name"
     "NCName"
     "ID"
     "IDREF"
     "ENTITY"
     "integer"
     "nonPositiveInteger"
     "negativeInteger"
     "long"
     "int"
     "short"
     "byte" 
     "nonNegativeInteger"
     "unsignedLong"
     "unsignedInt"
     "unsignedShort"
     "unsignedByte" 
     "positiveInteger"

     ))

  (defpackage :net.xmp.schema-instance
    (:use)
    (:export 
     "type"
     "base"
     "minOccurs"
     "maxOccurs"
     ))

  )

(eval-when (compile)
  (defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
  (defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
  )

(defun add-to-list (item place)
  (nconc place (list item)))


(defclass schema-file-connector (xmp-string-in-connector)
  (
   (protocol   :initarg :xml-schema)
   (source     :reader   schema-source :initarg :source)
   (target     :accessor schema-target :initform nil)
   (prefix     :accessor schema-prefix :initform "tns")
   (elements   :accessor schema-elements :initform (list nil))
   (types      :accessor schema-types    :initform (list nil))
   (attributes :accessor schema-attributes :initform (list nil))
   (groups     :accessor schema-groups     :initform (list nil))
   (a-groups   :accessor schema-a-groups   :initform (list nil))
   (context    :accessor schema-context    :initform nil)

   (message-dns     :initform
		    (list nil
			  (list :net.xmp.schema
				"xs"
				"http://www.w3.org/2001/XMLSchema")
			  (list :net.xmp.schema-instance
				"xsi"
				"http://www.w3.org/1999/XMLSchema-instance")
			  ))
   (trim-whitespace :initform t)
   ))

(defun decode-schema (file &key (verbose t))
  (let* ((conn (make-instance 'schema-file-connector :source file)))

    (xmp-decode-message conn (xmp-parse-message conn file))
    (when verbose
      (format t "~&~%Elements:~%~S~%" (schema-elements conn))
      (format t "~%Types:~%~S~%" (schema-types conn))
      (format t "~%Attributes:~%~S~%" (schema-attributes conn))
      (format t "~%Groups:~%~S~%" (schema-groups conn))
      (format t "~%AGroups:~%~S~2%" (schema-a-groups conn)))
    conn))


(defmethod xmp-parse-message ((conn schema-file-connector) source &key namespaces) 
  (with-open-file (s source)
		  (call-next-method conn s :namespaces namespaces)))

(defmethod xmp-begin-message ((conn schema-file-connector))
  (list :seq1 'xs:|schema|))

(defmethod xmp-end-message ((conn schema-file-connector) data
			    &key types &allow-other-keys)
  (values data types))


(define-xmp-element nil 'xs:|schema| '(:complex
				       (:set* xs:|element| xs:|complexType| 
					      xs:|attribute| xs:|simpleType| 
					      xs:|attributeGroup| xs:|group|
					      )))
(define-xmp-element nil 'xs:|group|       '(:complex (:set* xs:|sequence|)))
(define-xmp-element nil 'xs:|complexType|
  '(:complex
    (:set* xs:|sequence| xs:|any| xs:|annotation|
	   xs:|anyAttribute| xs:|attributeGroup| 
	   xs:|group| xs:|all|
	   xs:|simpleContent|
	   xs:|complexContent|
	   )))
(define-xmp-element nil 'xs:|attribute|      '(:complex
					       (:set* xs:|simpleType| xs:|annotation|)))
(define-xmp-element nil 'xs:|attributeGroup| '(:complex
					       (:set* xs:|attribute| xs:|anyAttribute| 
						      xs:|annotation|)))
(define-xmp-element nil 'xs:|annotation|     '(:complex (:set* xs:|documentation|)))


(define-xmp-element nil 'xs:|all|           
  '(:complex (:seq xs:|annotation| (:seq* xs:|element|))))
(define-xmp-element nil 'xs:|sequence|
  '(:complex (:seq xs:|annotation|
		   (:set* xs:|element| xs:|sequence| xs:|any| xs:|group| xs:|choice|))))
(define-xmp-element nil 'xs:|choice|
  '(:complex (:seq xs:|annotation| 
		   (:set* xs:|element| xs:|sequence| xs:|any| xs:|group| xs:|choice|))))
  
(define-xmp-element nil 'xs:|anyAttribute|   '(:complex (:set* xs:|annotation|)))
(define-xmp-element nil 'xs:|simpleType|     '(:complex
					       (:set* xs:|restriction|
						      xs:|annotation|
						      xs:|list|)))
(define-xmp-element nil 'xs:|restriction|
  '(:complex (:set* xs:|pattern| xs:|attribute|)))
(define-xmp-element nil 'xs:|simpleContent|  '(:complex (:set* xs:|extension|)))
(define-xmp-element nil 'xs:|extension|      '(:complex (:set* xs:|attributeGroup|)))

(define-xmp-element nil 'xs:|complexContent|
  '(:complex (:seq1 (:seq xs:|annotation|) (:or xs:|restriction| xs:|extension|))))



(defmethod xmp-decode-element :around ((conn schema-file-connector)
				      (elt (eql 'xs:|schema|)) (data t)
				      &rest options
				      &key attributes &allow-other-keys)
  (let ((found (schema-collect-target conn attributes)))
    (multiple-value-prog1
     (call-next-method)
     (when found (pop (schema-target conn))))))


(defmethod schema-collect-target ((conn schema-file-connector) attributes)
  (do ((att attributes (cddr att)) name found)
      ((atom att) nil)
    (setf name (first att))
    (when (equal (symbol-name name) "targetNamespace")
      (when (setf found (second att))
	(push found (schema-target conn)))
      (return found))))


(defmethod schema-begin-part ((conn schema-file-connector) 
			      tag1 attributes acc
			      &aux (place (funcall acc conn)))
  (let ((nameplus (schema-name-attr conn attributes)))
    (push (list* tag1 nameplus) (schema-context conn))
    (or (cdr (schema-context conn))
	(add-to-list nameplus place))))


(defmethod schema-update-content ((conn schema-file-connector) key data acc  
				  &aux result
				  (context (schema-context conn))
				  (place (funcall acc conn))
				  )
  (when data (setf result (list data key)))
  (if (null (cdr context))
      (let* ((item (assoc (second (first context)) (cdr place) :test #'string-equal-ex)))
	(dolist (d result) (push d (cdr item)))
	(setf result nil))
    (setf result (list (append (car context) (reverse result)))))
  (pop (schema-context conn))
  result)


(defmethod xmp-complex-content ((conn schema-file-connector) 
				(elt (eql 'xs:|annotation|))
				data &rest options &key &allow-other-keys)
  (list :annotation data))

(defmethod xmp-complex-content ((conn schema-file-connector) 
				(elt (eql 'xs:|documentation|))
				data &rest options &key &allow-other-keys)
  (list :documentation data))

(defun drop-two (list tail &aux head)
  (do ((tl list (cddr tl)))
      ((atom tl) (nreverse head))
    (when (eq tl tail)
	(return (nconc (nreverse head) (cddr tl))))
    (push (car tl) head)
    (push (cadr tl) head)))


(defmethod schema-decode-attribute ((conn schema-file-connector)
				    name value nss &aux attr)
  (values (setf attr (xmp-decode-qualified-name conn name nss))
	  (or (case (intern (symbol-name attr) :keyword)
		((:|base| :|type|)
		 (xmp-decode-qualified-name conn value nss))
		((:|minOccurs| :|maxOccurs|)
		 (parse-integer value))
		)
	      (case attr
		(otherwise value)))))
	  
(defmethod schema-decode-attributes ((conn schema-file-connector)
				     attributes nss)
  (do ((tail attributes (cddr tail)) res)
      ((atom tail) (nreverse res))
    (multiple-value-bind (name val)
	(schema-decode-attribute conn (first tail) (second tail) nss)
      (push name res)
      (push val res))))


(defmethod schema-name-attr ((conn schema-file-connector)
				 attributes &optional key data not-qname)
  (let* (tail
	 (name (second (or (setq tail (member
				       "name" attributes :test #'string-equal-ex))
			   (setq tail (member
				       "ref" attributes :test #'string-equal-ex)))))
	 (tail2 (if name (drop-two attributes tail) attributes))
	 tail3
	 (type (second 
		(setf tail3
		      (member
		       "type" (setf attributes tail2) :test #'string-equal-ex))))
	 (tail4 (if type
		    (drop-two attributes tail3)
		  attributes))
	 )
    (append
     (list
      (if not-qname
	  name
	(let* ((targets (schema-target conn))
	       (target (first targets))
	       (tp (xmp-uri-to-package conn target :in))
	       (*package* (resolve-package tp)))
	  (if tp
	      (xmp-decode-qualified-name conn name (cons (list target) :in))
	    (xmp-decode-qualified-name conn name :in)))))
     (when type
       (list :type (xmp-decode-qualified-name conn type :in)))
     (and key data (list key data))
     (when tail4 (list :attributes
		       (schema-decode-attributes conn tail4 :in))))))
  


(defmethod xmp-complex-content ((conn schema-file-connector) 
				(elt (eql 'xs:|element|)) data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (let ((nameplus (schema-name-attr conn attributes :content data :not-qname)))
    (cond ((cdr (schema-context conn))
	   (list (list* :element nameplus)))
	  (t (add-to-list nameplus (schema-elements conn))
	     (list (first nameplus))))))


(defmacro schema-collected-part (class elt tag acc)
  `(progn
     (defmethod xmp-begin-element :after ((conn ,class) 
					  (elt (eql ',elt))
					  &rest options
					  &key attributes &allow-other-keys)
       (declare (ignore options))
       (schema-begin-part conn ',tag attributes ',acc))
     (defmethod xmp-complex-content ((conn ,class) 
				     (elt (eql ',elt)) data
				     &rest options &key &allow-other-keys)
       (declare (ignore options))
       (schema-update-content conn ',tag data ',acc))))

(schema-collected-part schema-file-connector
		       xs:|complexType| :complex-type schema-types)
(schema-collected-part schema-file-connector
		       xs:|simpleType|  :simple-type  schema-types)
(schema-collected-part schema-file-connector
		       xs:|attribute|   :attribute    schema-attributes)
(schema-collected-part schema-file-connector
		       xs:|group|       :group        schema-groups)
(schema-collected-part schema-file-connector
		       xs:|attributeGroup| :a-group   schema-a-groups)
	

(defmacro schema-nested-part (class elt tag)
  `(progn
     (defmethod xmp-begin-element :after ((conn ,class) 
					  (elt (eql ',elt))
					  &rest options &key &allow-other-keys)
       (declare (ignore options))
       (push (list ',tag) (schema-context conn)))
     (defmethod xmp-complex-content ((conn ,class) 
				     (elt (eql ',elt))
				     data
				     &rest options &key attributes &allow-other-keys)
       (declare (ignore options))
       (pop (schema-context conn))
       (append (when attributes
		 (list :attributes (schema-decode-attributes conn attributes :in)))
	       (list :content (cons ',tag data))))
     ))

;; ??? minOccurs maxOccurs attributes may apply
;;     xs:|sequence|   min=0  max=Unbounded default=1
;;     xs:|all|        min=0  max=1         default=1
;;     xs:|choice|     min=0  max=Unbounded default=1
(schema-nested-part schema-file-connector xs:|sequence| :seq*) 
(schema-nested-part schema-file-connector xs:|all|      :set)
(schema-nested-part schema-file-connector xs:|choice|   :or)

(defmethod xmp-complex-content ((conn schema-file-connector) 
				(elt (eql 'xs:|complexContent|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list* :complex-content
	 (append (when attributes
		   (list :attributes (schema-decode-attributes conn attributes :in)))
		 data)))


(defun collect-schema-parts (conn tag attributes data)
  (list
   (list* 
    tag
    (append (schema-decode-attributes conn attributes :in)
	    (when data
	      (list :content
		    (cond ((cdr data) data)
			  ((atom (car data)) (car data))
			  (t data))))))))


(defmacro schema-named-part (class elt tag &optional not-qname)
  `(defmethod xmp-complex-content ((conn ,class) 
				   (elt (eql ',elt))
				   data
				   &rest options &key attributes &allow-other-keys)
     (declare (ignore options))
     (list
      (list* 
       ',tag
       (append
	(schema-name-attr conn attributes nil nil ,not-qname)
	(when data
	  (list :content
		(cond ((cdr data) data)
		      ((atom (car data)) (car data))
		      (t data)))))))
     ))

(defmacro schema-simple-part (class elt tag)
  `(defmethod xmp-complex-content ((conn ,class) 
				   (elt (eql ',elt))
				   data
				   &rest options &key attributes &allow-other-keys)
     (declare (ignore options))
     (list
      (list* 
       ',tag
       (append (schema-decode-attributes conn attributes :in)
	       (when data
		 (list :content
		       (cond ((cdr data) data)
			     ((atom (car data)) (car data))
			     (t data)))))))

     ))

(schema-simple-part schema-file-connector xs:|extension|     :extension)
(schema-simple-part schema-file-connector xs:|simpleContent| :simple)
(schema-simple-part schema-file-connector xs:|restriction|   :restriction)
(schema-simple-part schema-file-connector xs:|pattern|       :pattern)
(schema-simple-part schema-file-connector xs:|any|           :any)
(schema-simple-part schema-file-connector xs:|anyAttribute|  :any-attribute)
(schema-simple-part schema-file-connector xs:|list|          :list)



(defmethod xmp-complex-content ((conn schema-file-connector) 
				(elt (eql 'xs:|schema|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (when data (xmp-error conn :client :string "Unprocessed data"))
  nil)

(defmethod xmp-complex-content ((conn schema-file-connector) (elt t) data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (format t "~&;;xmp-complex-content ~S~%" elt)
  (list (list* (if attributes
		   (concatenate
		    'vector
		    (list* elt (schema-decode-attributes conn attributes :in)))
		 elt)
	       data)))


