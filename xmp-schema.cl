;; -*- mode: common-lisp; package: net.xmp -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2003-2004 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: xmp-schema.cl,v 2.5 2005/09/06 17:10:28 layer Exp $

;; XML Schema support

(in-package :net.xmp)

(defpackage :net.xmp
  (:export
   #:define-schema-collected-part
   #:define-schema-simple-part
   #:define-schema-named-part 
   #:define-schema-default-part
   #:schema-types
   #:schema-elements
   #:schema-target
   #:schema-collect-target
   #:schema-file-connector
   #:schema-decode-attribute
   #:schema-decoded-attribute
   #:schema-raw-attribute
   #:define-schema-ignored-part
   #:schema-component
   #:schema-text-component
   #:schema-simple-component
   #:schema-named-component
   #:schema-imports
   
   #:schema-raw-attribute
   #:schema-decoded-attribute
   #:schema-collected-component
   #:schema-collected-parts
   #:schema-single-part
   #:schema-parts-to-type
   #:schema-element-key
   #:schema-element-tag
   #:schema-component-content
   #:schema-component-name
   #:schema-lookup-type
   #:schema-lookup-element
   #:schema-merge-types 

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
     "import"
     "include"
     "minInclusive"
     "maxInclusive"

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

     "enumeration"
     "maxLength"
     "maxlength"
     "nillable"
     ))

  (defpackage :net.xmp.schema-instance
    (:use)
    (:export 
     "type"
     "base"
     "minOccurs"
     "maxOccurs"
     "nil"
     ))

  )

(eval-when (compile)
  (defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
  (defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
  )

(defun add-to-list (item place)
  (nconc place (list item)))


(defclass schema-file-connector (xmp-string-in-out-connector)
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
   (imports    :accessor schema-imports    :initform (list nil))

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

   (schema-component-stack :accessor schema-component-stack
			   :initarg :schema-component-stack)
   (schema-ignored-messages :accessor schema-ignored-messages :initform nil)
   ))

(defclass schema-component ()
  (
   (schema-element-tag :reader schema-element-tag
		       :initarg :schema-element-tag)
   (schema-element-key :reader schema-element-key
		       :initarg :schema-element-key)
   (schema-component-raw-attributes
    ;; raw attribute p-list
    :accessor schema-component-raw-attributes
    :initarg :schema-component-raw-attributes)
   (schema-component-decoded-attributes
    ;; decoded attribute p-list
    :accessor schema-component-decoded-attributes
    :initarg :schema-component-decoded-attributes)
   (schema-component-content
    ;; list of content instances
    :accessor schema-component-content :initarg :schema-component-content)
   ))

(defmethod schema-component-name ((c schema-component))
  (schema-raw-attribute c "name"))

(defvar *print-parts* nil)
(defmethod print-object ((c schema-component) s)
  (print-unreadable-object 
   (c s :type t :identity t)
   (format s "~S ~S" (schema-element-tag c) (schema-element-key c))
   (typecase c
     (schema-named-component (format s " ~A" (schema-component-name c))))
   (when *print-parts*
     (dolist (p (schema-component-content c))
       (let ((*print-parts* (typecase *print-parts*
			      ((member 1) nil)
			      (integer (1- *print-parts*))
			      (otherwise nil))))
	 (format s " ~S" p))))
   ))

(defclass schema-text-component (schema-component) ())

(defmethod schema-raw-attribute ((comp schema-component) name)
  (xmp-getf nil (schema-component-raw-attributes comp) name))

(defmethod schema-decoded-attribute ((comp schema-component) name)
  (xmp-getf nil (schema-component-decoded-attributes comp) name))

(defclass schema-simple-component (schema-component)
  (
   ))

(defclass schema-named-component (schema-component)
  (
   (schema-component-name :accessor schema-component-name
			  :initarg :schema-component-name
			  )
   (schema-component-type :accessor schema-component-type
			  :initarg :schema-component-type
			  )
   ))


(defclass schema-collected-component (schema-named-component)
  (
   (schema-component-accessor :accessor schema-component-accessor
			      :initarg :schema-component-accessor
			      :initform nil) 
   ))

(defmethod schema-collected-component ((conn schema-file-connector) 
				       collection-accessor key-accessor key-value
				       &optional ignore-case)
  (first (member key-value (cdr (funcall collection-accessor conn))
		 :test #'(lambda (x y) (xmp-match-name conn y x ignore-case))
		 :key key-accessor)))



(defmethod schema-single-collector ((comp schema-component))
  (schema-single-part comp #'(lambda (x) (xmp-collector-p nil x))))

(defmethod schema-collector-to-type ((sub schema-component) &key options)
  (list* :complex (schema-component-to-collector sub) options))

(defmethod schema-collector-to-type ((sub null) &key options)
  (list* :complex (list :seq) options))

(defmethod schema-error-p ((conn t) error-p &rest fmt)
  (if error-p
      (apply 'error fmt)
    (values nil (apply 'format nil fmt))))

(defmethod schema-merge-types ((conn t) base ext &key options error-p)
  (let* ((b-kind (when (consp base) (first base)))
	 (e-kind (when (consp ext) (first ext)))
	 (b-coll (when (consp base) (second base)))
	 (e-coll (when (consp ext) (second ext)))
	 (b-collector (first b-coll))
	 (e-collector (first e-coll))
	 (b-tail (cdr b-coll))
	 (e-tail (cdr e-coll))
	 )
    (cond ((and (eq b-kind e-kind)
		(case b-kind
		  (:complex
		   (cond ((xmp-collector-p conn b-collector)
			  (cond ((eq b-collector e-collector)
				 (list* b-kind
					(list* b-collector (append b-tail e-tail))
					(append (cddr base) (cddr ext) options)))
				((null b-tail)
				 (list* b-kind e-coll
					(append (cddr base) (cddr ext) options)))
				(t (schema-error-p
				    conn error-p "cannot merge (a) ~S ~S" base ext))))
			 (t (schema-error-p
			     conn error-p "cannot merge (b) ~S ~S" base ext))))
		  (otherwise (schema-error-p
			      conn error-p "cannot merge (c) ~S ~S" base ext)))))
	  (t (schema-error-p conn error-p "cannot merge (d) ~S ~S" base ext)))))
		 





(defmethod schema-parts-to-type ((comp schema-component) 
				 &key options (error-p t) conn
				 &aux part ext sub key base b-def b-type extype)
  (setf key (schema-element-key comp))
  (cond ((eq key :complex-type)
	 (cond ((setf sub (schema-single-collector comp))
		(schema-collector-to-type sub :options options))
	       ((null (schema-component-content comp))
		(schema-collector-to-type nil :options options))
	       ((setf part (schema-single-part comp :complex-content))
		(cond
		 ((setf ext (schema-single-part part :extension))
		  (cond
		   ((setf base (schema-decoded-attribute ext "base"))
		    (cond
		     ((setf b-def (schema-lookup-type conn base))
		      (cond
		       ((and (setf b-type (schema-parts-to-type b-def :error-p nil
								:conn conn))
			     (xmp-collection-type-p conn b-type))
			(cond
			 ((setf sub (schema-single-collector ext))
			  (cond
			   ((setf extype (schema-parts-to-type sub))
			    (cond
			     ((xmp-collection-type-p conn extype)
			      (cond
			       ((schema-merge-types conn b-type extype :error-p error-p))
			       (t (schema-error-p conn error-p 
						  "incompatible base and extension ~S ~S"
						  b-type extype))))
			     (t (schema-error-p conn error-p
						"unrecognized extension ~S" extype))))
			   (t (schema-error-p
			       conn error-p
			       "unrecognixed content in extension ~S" ext))))
			 ((null (schema-component-content ext))
			  b-type)))
		       (t (schema-error-p
			   conn error-p "base type is not collection ~S" b-def))))
		     (t (schema-error-p
			 conn error-p "undefined base type ~S" base))))
		   (t (schema-error-p
		       conn error-p "extension without a base type"))))
		 (t (schema-error-p
		     conn error-p "unrecognised complexContent"))))
	       (t (schema-error-p conn error-p "Unknown complexType."))))
	((xmp-collector-p nil key)
	 (schema-collector-to-type comp :options options))
	((or (setf part (schema-single-part comp :complex-type))
	     (setf part
		   (setf sub (schema-single-collector comp))))
	 (cond ((or sub
		    (setf sub (schema-single-collector part)))
		(schema-collector-to-type sub :options options))
	       ((null (schema-component-content part))
		(schema-collector-to-type nil :options options))
	       (t (schema-error-p conn error-p "Unknown complex-type."))))

	((eq key :simple-type)
	 (cond ((and (setf part (schema-single-part comp :restriction))
		     (setf base (schema-decoded-attribute part "base")))
		base)
	       ((schema-single-part comp :list)
		'xs:|string|)
	       (t (schema-error-p conn error-p "Unknown simple-type."))))

	((eq key :simple-content)
	 (cond ((and (setf part (schema-single-part comp :extension))
		     (setf base (schema-decoded-attribute part "base")))
		base)
	       ((and (setf part (schema-single-part comp :restriction))
		     (setf base (schema-decoded-attribute part "base")))
		base)
	  
	       ))

	(t (schema-error-p conn error-p "Unknown part type."))))


(defmethod schema-component-to-collector ((comp schema-component))
  (cond ((xmp-collector-p nil (schema-element-key comp))
	 (list* (schema-element-key comp)
		(mapcan #'schema-component-to-cpart (schema-component-content comp))))))


(defmethod schema-element-name ((comp schema-component))
  (let* ((raw (schema-raw-attribute comp "name"))
	 (qname (position #\: raw)))
    (cond (qname (schema-decoded-attribute comp "name"))
	  (raw   raw)
	  (t     (schema-component-name comp)))))
	 

(defmethod schema-component-to-cpart ((comp schema-component)
				      &aux n (key (schema-element-key comp)))
  (cond
    ((xmp-collector-p nil key)
     (list (schema-component-to-collector comp)))
    ((eq key :element)
     (list 
      (list* :element 
	    (list (schema-element-name comp))
	    (or (schema-decoded-attribute comp "type")
		(schema-parts-to-type comp :error-p nil)
		(xmp-any-type nil))
	    (and (setf n (schema-decoded-attribute comp "nillable"))
		 (or (equalp n "true") (equal n "1"))
		 (list :nillable t))
	    )))
    ((eq key :annotation) nil)
    ((eq key :documentation) nil)
    ((eq key :any) (list (list :any)))
    (t(error "Unknown cpart type."))))
    



(defun decode-schema (file &key (verbose t))
  (let* ((conn (make-instance 'schema-file-connector :source file)))

    (xmp-decode-file conn file)
    (when verbose
      (format t "~&~%Elements:~%~S~%" (schema-elements conn))
      (format t "~%Types:~%~S~%" (schema-types conn))
      (format t "~%Attributes:~%~S~%" (schema-attributes conn))
      (format t "~%Groups:~%~S~%" (schema-groups conn))
      (format t "~%AGroups:~%~S~2%" (schema-a-groups conn))
      (format t "~%Imports:~%~S~2%" (schema-imports conn))
      )
    conn))


(defmethod xmp-begin-message :before ((conn schema-file-connector))
  (setf (schema-context conn) nil
	(schema-component-stack conn) (list nil)
	(schema-ignored-messages conn) nil
	))

(defmethod xmp-begin-message ((conn schema-file-connector))
  (list :seq1 'xs:|schema|))


(defmethod xmp-end-message ((conn schema-file-connector) data
			    &key types &allow-other-keys)
  (values data types))

(defmethod xmp-begin-element :around ((conn schema-file-connector) elt
				     &rest options 
				     &key schema-component attributes tag 
				     &allow-other-keys)
  (push (or schema-component (schema-make-element
			      conn elt :attributes attributes :tag tag))
	(schema-component-stack conn))
  (call-next-method))

(defmethod schema-make-element ((conn schema-file-connector) elt
				&rest keys &key tag attributes class &allow-other-keys)
  (declare (ignore keys))
  (cond (tag)
	((member elt (schema-ignored-messages conn)) (setf tag :ignored))
	((eq :ignored (schema-element-key (first (schema-component-stack conn))))
	 (push elt (schema-ignored-messages conn))
	 (setf tag :ignored))
	(t (format t "~&;; Unexpected schema element ~S given tag :ignored.~%" elt)
	   (push elt (schema-ignored-messages conn))
	   (setf tag :ignored)))
  (make-instance (or class 'schema-component)
		 :schema-element-tag elt
		 :schema-element-key tag
		 :schema-component-raw-attributes attributes
		 :schema-component-decoded-attributes
		 (schema-decode-attributes conn attributes :in)
		 ))

(defmethod xmp-complex-content ((conn schema-file-connector) (elt t) data
				&rest options &key (warn t) &allow-other-keys
				&aux part
				)
  #+debug-schema
  (format t "~& stack: ~A~%" (length (schema-component-stack conn)))

  (setf part (pop (schema-component-stack conn)))
  (case (schema-element-key part)
    (:ignored nil)
    (otherwise
     (when warn (format t "~&;; Unexpected schema element ~S~%" elt))))

  ;; This default primary method simply returns the schema-component instance
  #+debug-schema
  (format t "~&~A parts: ~A stack: ~A~%"
	  elt (length data) (length (schema-component-stack conn))
	  )
  (setf (schema-component-content part) data)
  (list part))

(defmethod xmp-simple-content ((conn schema-file-connector) (elt t) data
				&rest options &key &allow-other-keys)
  data)


(define-xmp-element nil 'xs:|schema| '(:complex
				       (:set* xs:|element| xs:|complexType| 
					      xs:|attribute| xs:|simpleType| 
					      xs:|attributeGroup| xs:|group|
					      xs:|import| xs:|include|
					      )))
(define-xmp-element nil 'xs:|group|       '(:complex (:set* xs:|sequence|)))
(define-xmp-element nil 'xs:|complexType|
  '(:complex
    (:set* xs:|sequence| xs:|any| xs:|annotation|
	   xs:|attribute| xs:|anyAttribute| xs:|attributeGroup| 
	   xs:|group| xs:|all| xs:|choice| xs:|element|
	   xs:|simpleContent|
	   xs:|complexContent|
	   (:any)          ;;;wsdl:element  occurs in Agni Find MP3 on xmethods
	   )))
(define-xmp-element nil 'xs:|attribute|      '(:complex
					       (:set* xs:|simpleType| xs:|annotation|)))
(define-xmp-element nil 'xs:|attributeGroup| '(:complex
					       (:set* xs:|attribute| xs:|anyAttribute| 
						      xs:|annotation|)))
(define-xmp-element nil 'xs:|annotation|     (xmp-any-type nil))


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
						      xs:|list|
						      
						      ;;???
						      ;; This does not seem to be legal
						      ;; XML Schema syntax but it does
						      ;; occur in IHS Web Service def
						      xs:|simpleContent|
						      
						      )))
(define-xmp-element nil 'xs:|restriction|
  '(:complex (:set* xs:|pattern| xs:|attribute| xs:|sequence| xs:|enumeration|
		    xs:|maxLength| xs:|maxlength|
		    xs:|attributeGroup| xs:|minInclusive| xs:|maxInclusive|
		    )))
(define-xmp-element nil 'xs:|simpleContent|  '(:complex (:set* xs:|extension|)))
(define-xmp-element nil 'xs:|extension|
  '(:complex (:set* xs:|sequence| xs:|attribute| xs:|attributeGroup|)))

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
    (when (equal (string name) "targetNamespace")
      (when (setf found (second att))
	(push found (schema-target conn)))
      (return found))))


(defmacro define-schema-default-part (pclass elt tag &key (cclass 'schema-component))
  `(progn
     (defmethod schema-make-element ((conn ,pclass) (elt (eql ',elt))
				     &rest keys &key &allow-other-keys)
       (apply #'call-next-method conn elt :tag ',tag :class ',cclass keys))
     (defmethod xmp-complex-content ((conn ,pclass)
				     (elt (eql ',elt))
				     data
				     &rest options &key &allow-other-keys)
       (apply #'call-next-method conn elt data :warn nil options))))

(define-schema-default-part schema-file-connector xs:|schema| :schema)
(define-schema-default-part
  schema-file-connector xs:|complexContent| :complex-content)
(define-schema-default-part schema-file-connector xs:|annotation|
  :annotation :cclass schema-text-component)
(define-schema-default-part
  schema-file-connector xs:|documentation| :documentation :cclass  schema-text-component)



(defun drop-two (list tail &aux head)
  (do ((tl list (cddr tl)))
      ((atom tl) (nreverse head))
    (when (eq tl tail)
	(return (nconc (nreverse head) (cddr tl))))
    (push (car tl) head)
    (push (cadr tl) head)))


(defmethod schema-decode-attribute :around ((conn schema-file-connector)
					    name value nss)
  (call-next-method 
   conn
   (xmp-decode-qualified-name conn name nss :suppress-default t)
   value nss))

(defmethod schema-decode-attribute ((conn schema-file-connector)
				    name value nss &aux attr)
  (values (setf attr name)
	  (or (case (intern (string attr) :keyword)
		((:|base| :|type|)
		 (xmp-decode-qualified-name conn value nss :suppress-default t))
		(:|maxOccurs| (if (string-equal "unbounded" value)
				  most-positive-fixnum
				(parse-integer value)))
		(:|minOccurs| (parse-integer value))
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
			     attributes &optional (qname t))
  ;; extract a schema element of the form <elt name="nn" type="tt" ... />
  ;;
  ;; and return values: name type content decoded-attributes

  (let* ((name (or (xmp-getf conn attributes "name")
		   (xmp-getf conn attributes "ref")))
	 (type (xmp-getf conn attributes "type"))
	 )
    (values
     (if (null qname)
	 name
       (let* ((targets (schema-target conn))
	      (target (first targets))
	      (tp (when target (xmp-uri-to-package conn target :in)))
	      (*package* (resolve-package tp)))
	 (if tp
	     ;; if there is a targetNamaspace, set it as default and use it
	     (xmp-decode-qualified-name conn name (cons (list target) :in))
	   ;; otherwise there is no default that applies
	   (xmp-decode-qualified-name conn name :in :suppress-default t))))
     (when type
       (xmp-decode-qualified-name conn type :in :suppress-default t))
     (schema-decode-attributes conn attributes :in))))
  




(defmacro define-schema-collected-part (pclass elt tag acc
					       &optional
					       (cclass 'schema-collected-component)
					       )
  ;; pclass   - xmp connection to schema source
  ;; elt      - schema element name (symbol)
  ;; cclass   - class of schema component created for this element
  ;; acc      - keep a list of the outer-most instances of this component
  ;;             at this accessor in pclass
  `(progn
     (defmethod xmp-begin-element :around ((conn ,pclass) 
					  (elt (eql ',elt))
					  &rest options
					  &key attributes &allow-other-keys)
       (apply #'call-next-method conn elt
	      :schema-component
	      (schema-begin-part conn elt ',cclass attributes ',tag ',acc)
	      options))
     (defmethod xmp-complex-content ((conn ,pclass) 
				     (elt (eql ',elt)) data
				     &rest options &key &allow-other-keys)
       (pop (schema-context conn))
       (apply #'call-next-method conn elt data :warn nil options)
       )))

(defmethod schema-begin-part ((conn schema-file-connector) 
			      elt cclass attributes key acc
			      &aux part (place (funcall acc conn)))
  (multiple-value-bind (name type dattr)
      (schema-name-attr conn attributes)
    (setf part (make-instance cclass
			 :schema-component-raw-attributes attributes
			 :schema-component-decoded-attributes dattr
			 :schema-component-name name
			 :schema-component-type type
			 :schema-element-key key
			 :schema-element-tag elt
			 ))
    (push part (schema-context conn))
    (or (cdr (schema-context conn))
	(add-to-list part place))
    part))




(defmethod schema-collected-parts ((comp schema-component) tag &optional key
				   &aux
				   (all (schema-component-content comp))
				   subset)
  (or (eq tag  (schema-element-key comp)) 
      (error "Schema component mismatch"))
  (if key
      (dolist (a all (nreverse subset))
	(when (eq key (schema-element-key a))
	  (push a subset)))
    all))

(defmethod schema-single-part ((comp schema-component) key
			       &key (error-p t) more-p only-p ignored
			       &aux
			       (all (schema-component-content comp))
			       a found)
  ;; look for sub-elements that match key
  ;;  if more-p is nil, return nil or the only match
  ;;            otherwise return list of matches
  ;;   if only-p is nil ignore other elements
  ;;             otherwise ignore only elements that match ignored
  ;; if error-p is non-nil, signal error
  ;;               when more-p is nil AND second element found
  ;;               when only-p is t AND element cannot be ignored
  (dolist (one all found)
    (setf a (schema-element-key one))
    (cond ((typecase key
	     (cons (member a key))
	     (function (funcall key a))
	     (otherwise (equal key a)))
	   (cond (more-p  (push one found))
		 ((null found) (setf found one))
		 (error-p (error "Schema component has too many parts"))
		 (t       (return nil))))
	  ((null only-p))
	  ((typecase ignored
	    (null nil)
	    (cons (member a ignored))
	    (function (funcall ignored a))
	    (otherwise (equal ignored a)))
	   )
	  (error-p (error "Schema component has other parts"))
	  (t       (return nil)))))
	 

(define-schema-collected-part schema-file-connector
		       xs:|element|     :element      schema-elements)
(define-schema-collected-part schema-file-connector
		       xs:|complexType| :complex-type schema-types)
(define-schema-collected-part schema-file-connector
		       xs:|simpleType|  :simple-type  schema-types)
(define-schema-collected-part schema-file-connector
		       xs:|attribute|   :attribute    schema-attributes)
(define-schema-collected-part schema-file-connector
		       xs:|group|       :group        schema-groups)
(define-schema-collected-part
  schema-file-connector xs:|attributeGroup| :a-group   schema-a-groups)
(define-schema-collected-part
  schema-file-connector xs:|import| :import   schema-imports)
(define-schema-collected-part
  schema-file-connector xs:|include| :include   schema-imports)

(defmacro define-schema-nested-part (class elt tag 
					   &optional (cclass 'schema-component))
  `(progn
     (defmethod xmp-begin-element :around ((conn ,class) 
					  (elt (eql ',elt))
					  &rest options &key attributes 
					  &allow-other-keys
					  &aux part)
       (setf part (make-instance ',cclass
			 :schema-component-raw-attributes attributes
			 :schema-component-decoded-attributes
			 (schema-decode-attributes conn attributes :in)
			 :schema-element-tag ',elt
			 :schema-element-key ',tag
			 ))
       (push part (schema-context conn))
       (apply #'call-next-method conn elt :schema-component part options))
     (defmethod xmp-complex-content ((conn ,class) 
				     (elt (eql ',elt))
				     data
				     &rest options &key &allow-other-keys)
       (pop (schema-context conn))
       (apply #'call-next-method conn elt data :warn nil options))))

;; ??? minOccurs maxOccurs attributes may apply
;;     xs:|sequence|   min=0  max=Unbounded default=1
;;     xs:|all|        min=0  max=1         default=1
;;     xs:|choice|     min=0  max=Unbounded default=1
(define-schema-nested-part schema-file-connector xs:|sequence| :seq*) 
(define-schema-nested-part schema-file-connector xs:|all|      :set)
(define-schema-nested-part schema-file-connector xs:|choice|   :or)


(defmacro define-schema-named-part (pclass elt tag 
					   &key 
					   (class 'schema-named-component)
					   (qname t))
  `(progn
     (defmethod xmp-begin-element :around ((conn ,pclass) 
					  (elt (eql ',elt))
					  &rest options &key attributes 
					  &allow-other-keys
					  &aux part)
       (multiple-value-bind (name type dattr)
	   (schema-name-attr conn attributes ',qname)
	 (setf part (make-instance ',class
				   :schema-component-raw-attributes attributes
				   :schema-component-decoded-attributes dattr
				   :schema-component-name name
				   :schema-component-type type
				   :schema-element-key ',tag
				   :schema-element-tag ',elt
				   ))
	 (apply #'call-next-method conn elt :schema-component part options)))
     (defmethod xmp-complex-content ((conn ,pclass) 
				     (elt (eql ',elt))
				     data
				     &rest options &key &allow-other-keys)
       (apply #'call-next-method conn elt data :warn nil options)
       )))

(defmacro define-schema-simple-part (pclass elt tag
					    &optional
					    (cclass 'schema-simple-component)
					    )
  `(progn
     (defmethod xmp-begin-element :around ((conn ,pclass) 
					  (elt (eql ',elt))
					  &rest options &key attributes 
					  &allow-other-keys
					  &aux part)
       (setf part (make-instance ',cclass
				 :schema-component-raw-attributes attributes
				 :schema-component-decoded-attributes
				 (schema-decode-attributes conn attributes :in)
				 :schema-element-key ',tag
				 :schema-element-tag ',elt
				 ))
       (apply #'call-next-method conn elt :schema-component part options))
     (defmethod xmp-complex-content ((conn ,pclass) 
				     (elt (eql ',elt))
				     (data t)
				     &rest options &key &allow-other-keys)
       (apply #'call-next-method conn elt data :warn nil options))))


(defmacro define-schema-ignored-part (class elt)
  `(progn
     (defmethod xmp-begin-element :around ((conn ,class) 
					   (elt (eql ',elt))
					   &rest options &key &allow-other-keys)
       (declare (ignore options))
       (xmp-any-cpart conn))

     (defmethod xmp-decode-element ((conn ,class) 
				    (elt (eql ',elt))
				    (data t)
				    &rest options &key &allow-other-keys)
       (declare (ignore options))
       nil)))


(define-schema-simple-part schema-file-connector xs:|extension|     :extension)
(define-schema-simple-part schema-file-connector xs:|simpleContent| :simple-content)
(define-schema-simple-part schema-file-connector xs:|restriction|   :restriction)
(define-schema-simple-part schema-file-connector xs:|maxLength|     :max-length)
(define-schema-simple-part schema-file-connector xs:|maxlength|     :max-length)
(define-schema-simple-part schema-file-connector xs:|pattern|       :pattern)
(define-schema-simple-part schema-file-connector xs:|any|           :any)
(define-schema-simple-part schema-file-connector xs:|anyAttribute|  :any-attribute)
(define-schema-simple-part schema-file-connector xs:|list|          :list)
(define-schema-simple-part schema-file-connector xs:|enumeration|   :enumeration)



(defmethod schema-lookup-type ((conn schema-file-connector) name)
  (schema-collected-component conn #'schema-types #'schema-component-name name))

(defmethod schema-lookup-element ((conn schema-file-connector) name)
  (schema-collected-component conn #'schema-elements #'schema-component-name name))




