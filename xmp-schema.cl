;; -*- mode: common-lisp; package: net.xmp -*-
;;
;; See the file LICENSE for the full license governing this code.


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
   #:schema-groups
   #:schema-a-groups
   #:schema-target
   #:schema-source
   #:schema-context
   #:schema-collect-target
   #:schema-file-connector
   #:schema-decode-attribute
   #:schema-decode-attributes
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
   #:schema-lookup-component
   #:schema-collected-parts
   #:schema-single-part
   #:schema-parts-to-type
   #:schema-element-key
   #:schema-element-tag
   #:schema-component-content
   #:schema-component-name
   #:schema-component-raw-attributes
   #:schema-lookup-type
   #:schema-lookup-element
   #:schema-merge-types 
   #:schema-listify
   #:schema-targeted-name

   #:loose-type
   #:targeted-type

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
     "notation"
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
     "minExclusive"
     "maxExclusive"
     "redefine"
     "whiteSpace"
     "unique"
     "union"
     "key"
     "keyref"

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
     
     "appinfo"
     "enumeration"
     "length"
     "field"
     "selector"
     "minLength"
     "maxLength"
     "maxlength"
     "nillable"
     "totalDigits"
     "fractionDigits"
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
  (defmacro xsd (x) (list 'xmp-symbol x :net.xmp.schema))
  (defmacro xsi (x) (list 'xmp-symbol x :net.xmp.schema-instance))
  (defpackage :net.xmp (:export xsd xsi))
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
   (schema-source :reader schema-source :initarg :schema-source)
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
	 (format s " ~_~S" p))))
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

(defmethod schema-lookup-component ((conn schema-file-connector) 
				    collection-accessor key-accessor key-value
				    &optional ignore-case)
  (or (schema-collected-component
       conn collection-accessor key-accessor key-value ignore-case)
      (case (xmp-xml-syntax conn)
	(:strict nil)
	(otherwise
	 (typecase key-value
	   (string nil)
	   (symbol
	    (schema-collected-component
	     conn collection-accessor key-accessor (string key-value) ignore-case)))))))



(defmethod schema-single-collector ((comp schema-component))
  (or 
   (schema-single-part comp #'(lambda (x) (xmp-collector-p nil x)))
   ;; 2006-10-24 mm rev:
   ;; Follow a <group ref=> to its definition.
   (let* ((group (schema-single-part comp :group))
	  (def (when group (schema-lookup-group (schema-source group) group))))
     (when def (schema-single-collector def)))))
   

   

(defmethod schema-collector-to-type ((sub schema-component) &key options)
  (list* :complex (schema-component-to-collector sub) options))

(defmethod schema-collector-to-type ((sub null) &key options)
  (list* :complex (list :seq) options))

(defmethod schema-error-p ((conn t) error-p &rest fmtin &aux fmt)
  (case error-p
    ((nil)  (values nil))
    (otherwise
     (setf fmt
	   (mapcar #'(lambda (fm)
		       (typecase fm
			 (schema-component
			  (schema-listify fm))
			 (otherwise fm)))
		   fmtin))
     (case error-p
       (:report (values nil (apply 'format nil fmt)))
       (:cont (apply #'cerror "Continue" fmt))
       (otherwise (apply #'error fmt))))))

(defmethod schema-listify ((comp t)) comp)
(defmethod schema-listify ((comp schema-component))
  (list* (append (list (schema-element-tag comp) (schema-element-key comp))
		(schema-component-raw-attributes comp)
		(schema-component-decoded-attributes comp))
	 (mapcar #'schema-listify (schema-component-content comp))))

(defmethod schema-merge-types ((conn t) base ext &key options error-p)
  (let* ((b-kind (when (consp base) (first base)))
	 (e-kind (when (consp ext) (first ext)))
	 (b-coll (when (consp base) (second base)))
	 (e-coll (when (consp ext) (second ext)))
	 (b-collector (first b-coll))
	 (e-collector (first e-coll))
	 (b-tail (cdr b-coll))
	 (e-tail (cdr e-coll))
	 ;;2006-10-24 mm rev: options are the same in all emitted results
	 (newopt (append (cddr base) (cddr ext) options))
	 )
    (cond ((and (eq b-kind e-kind)
		(case b-kind
		  (:complex
		   (cond ((xmp-collector-p conn b-collector)
			  (cond ((eq b-collector e-collector)
				 (list* b-kind
					(list* b-collector (append b-tail e-tail))
					newopt))
				((null b-tail)
				 (list* b-kind e-coll newopt))
				((and (not (eq :or b-collector)) (eq :or e-collector))
				 ;; 2006-10-24 mm rev:
				 ;; Base is a collection of some kind,
				 ;; and extension is a <choice>.
				 ;; Add the choices to the collection.
				 (list* b-kind
					(list* b-collector (append b-tail 
								   (list e-coll)))
					newopt))
				(t (schema-error-p
				    conn error-p "cannot merge (a) ~S ~S" base ext))))
			 (t (schema-error-p
			     conn error-p "cannot merge (b) ~S ~S" base ext))))
		  (otherwise (schema-error-p
			      conn error-p "cannot merge (c) ~S ~S" base ext)))))
	  (t (schema-error-p conn error-p "cannot merge (d) ~S ~S" base ext)))))
		 



(defmethod schema-parts-to-type ((comp schema-component) 
				 &key options (error-p t) (conn (schema-source comp))
				 &aux part ext sub key base b-type extype)
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
		   ((and (setf b-type (schema-base-type conn ext))
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
					    "unrecognised extension type ~S" extype))))
		       (t (schema-error-p
			   conn error-p
			   "unrecognised content in extension ~S" ext))))
		     ((null
		       ;;2006-10-24 mm rev: If there are no structural
		       ;;  components, then map extension to base type.
		       (schema-single-part ext '(:complex-type) :more-p t
					   :ignored '(:attribute)))
		      b-type)
		     ;; 2006-10-24 mm rev: Avoid falling through with nil result.
		     (t (schema-error-p conn error-p 
					"unrecognised extension ~S" ext))
		     ))
		   (t (schema-error-p
		       conn error-p "base type is not collection: ~S ~S" ext b-type))))
		 ((setf ext (schema-single-part part :restriction))
		  ;; 2006-10-24 mm rev: Map a <restriction> to the base type.
		  (cond
		   ((setf b-type (schema-base-type conn ext))   b-type)
		   (t (schema-error-p
		       conn error-p "base type is not recognized in ~S" part))))
		 (t (schema-error-p
		     conn error-p "unrecognised complexContent ~S" part))))
	       ((setf part (schema-single-part comp :simple-content))
		(cond
		 ((setf ext (schema-single-part part :extension))
		  (cond
		   ((setf b-type (schema-base-type conn ext))
		    b-type ;;; 2006-10-24 mm rev: [bug16483] b-type is a typespec
		    )
		   (t (schema-error-p
		       conn error-p "base type is not recognized in ~S" part))))
		 ((setf ext (schema-single-part part :restriction))
		  (cond
		   ((setf b-type (schema-base-type conn ext))
		    b-type ;;; 2006-10-24 mm rev: [bug16483] b-type is a typespec
		    )
		   (t (schema-error-p
		       conn error-p "base type is not recognized in ~S" part))))
		 (t (schema-error-p
		       conn error-p "unrecognized simpleContent ~S" part))))
	       ((and (setf part (schema-single-part comp :group))
		     (setf sub (schema-component-to-collector part)))
		(list :complex sub))
	       ((null (schema-single-part
		       comp :element :more-p t :only-p t
		       :ignored '(:attribute :a-group :any-attribute)))
		(schema-collector-to-type nil :options options))
	       (t (schema-error-p conn error-p "Unknown complexType: ~S" comp))))
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
	       (t (schema-error-p conn error-p "Unknown complex-type: ~S" comp))))

	((eq key :simple-type)
	 (cond
	  ((setf part (schema-single-part comp :restriction))
	   ;; 2006-10-24 mm rev: There are several cases of restriction on simple-type.
	   (cond ((setf base (schema-decoded-attribute part "base"))
		  ;; Base type is a named type.
		  base)
		 ((and (setf base (schema-single-part part :simple-type))
		       (setf base (schema-single-part base :list)))
		  ;; Defined type is a list of base type items.
		  (list :simple (xsd "string")))
		 (t (schema-error-p conn error-p "Unknown restriction ~S in ~S"
				    part comp))))
	  ((schema-single-part comp :list) (list :simple (xsd "string")))
	  ((and (setf sub (schema-single-part comp :ignored))
		(eq :union (schema-element-key sub)))
	   (list :simple (xsd "string")))
	  (t (schema-error-p conn error-p "Unknown simple-type: ~S" comp))))

	((eq key :simple-content)
	 (cond ((and (setf part (schema-single-part comp :extension))
		     (setf base (schema-decoded-attribute part "base")))
		base)
	       ((and (setf part (schema-single-part comp :restriction))
		     (setf base (schema-decoded-attribute part "base")))
		base)
	       (t (schema-error-p conn error-p "Unknown simple-content: ~S" comp))
	       ))

	(t (schema-error-p conn error-p "Unknown part type: ~S" comp))))


(defmethod schema-component-to-collector ((comp schema-component) &aux exp)
  (cond ((xmp-collector-p nil (schema-element-key comp))
	 (list* (schema-element-key comp)
		(mapcan #'schema-component-to-cpart (schema-component-content comp))))
	((not (eq :group (schema-element-key comp))))
	((null (setf exp (schema-component-to-cpart comp))) nil)
	((not (consp exp)) (list :seq exp))
	((and (null (cdr exp)) (consp (first exp))
	      (xmp-collector-p nil (first (first exp))))
	 (first exp))
	(t (cons :seq exp))
	))


(defmethod schema-element-name ((comp schema-component))
  (let* ((raw (schema-raw-attribute comp "name"))
	 (qname (position #\: raw)))
    (cond (qname (schema-decoded-attribute comp "name"))
	  (raw   raw)
	  (t     (schema-component-name comp)))))
	 

(defmethod schema-component-to-cpart ((comp schema-component)
				      &aux ref rdef n part (key (schema-element-key comp)))
  (cond
    ((xmp-collector-p nil key)
     (list (schema-component-to-collector comp)))
    ((eq key :element)
     (list 
      (list* :element 
	    (list (schema-element-name comp))
	    (or (schema-decoded-attribute comp "type")
		(and (setq part (schema-single-part comp #'any-part-p))
		     (schema-parts-to-type part :error-p nil))
		(xmp-any-type nil))
	    (and (setf n (schema-decoded-attribute comp "nillable"))
		 ;; Element type maybe discribed in a simpleType content element [bug20703]
		 (or (equalp n "true") (equal n "1"))
		 (list :nillable t))
	    )))
    ((eq key :group)     ;;; [bug16201]
     (cond ((null (setf ref (schema-raw-attribute comp "ref")))
	    ;; must be a <group> definition collected earlier during parse
	    nil)
	   ((null (setf rdef (schema-lookup-group
			      (schema-source comp)
			      ;; 2006-10-24 mm rev:
			      ;; schema-lookup-group exects a component pointer.
			      comp)))
	    (error "Undefined group reference ~A" ref))
	   (t (list (schema-component-to-collector (schema-single-collector rdef))))))
    ((eq key :annotation) nil)
    ((eq key :documentation) nil)
    ((eq key :any) (list (list :any)))
    (t(error "Unknown cpart type."))))
    



(defun decode-schema (&key file string stream url (verbose t) syntax)
  (let* ((conn (make-instance 'schema-file-connector
			      :xml-syntax syntax :source file)))
    (when url (setf string (net.aserve.client:do-http-request url)))
    (cond (string (xmp-decode-string conn string))
	  (stream (xmp-decode-stream conn string))
	  (file   (xmp-decode-file conn file))
	  (verbose (format t "~&~%NO SOURCE SPECIFIED.~%")))
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
  (list :seq1 (xsd "schema")))


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
				&rest keys &key tag attributes class
				&allow-other-keys &aux outer)
  (declare (ignore keys))
  (cond (tag)
	((member elt (schema-ignored-messages conn)) (setf tag :ignored))
	((or (null (setf outer (first (schema-component-stack conn))))
	     (eq :ignored (schema-element-key outer)))
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
		 :schema-source conn
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


(defun define-schema-elements ()

;;; FROM http://www.w3.org/TR/2004/REC-xmlschema-1-20041028/structures.html

  ;; <schema
  ;;   attributeFormDefault = (qualified | unqualified) : unqualified
  ;;   blockDefault = (#all | List of (extension | restriction | substitution))  : ''
  ;;   elementFormDefault = (qualified | unqualified) : unqualified
  ;;   finalDefault = (#all | List of (extension | restriction | list | union))  : ''
  ;;   id = ID
  ;;   targetNamespace = anyURI
  ;;   version = token
  ;;   xml:lang = language
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: ((include | import | redefine | annotation)*,
  ;;             (((simpleType | complexType | group | attributeGroup)
  ;;               | element | attribute | notation), annotation*)*)
  ;; </schema>
  (define-xmp-element nil (xsd "schema")
    `(:complex
      (:seq? 
       (:set* ,(xsd "include") ,(xsd "import") 
	      ,(xsd "redefine") ,(xsd "annotation"))
       (:set* (:seq
	       (:or ,(xsd "simpleType") ,(xsd "complexType") 
		    ,(xsd "group") ,(xsd "attributeGroup"))
	       ,(xsd "element") 
	       ,(xsd "attribute") ,(xsd "notation") )
	      ,(xsd "annotation")))))

  ;; <element
  ;;   abstract = boolean : false
  ;;   block = (#all | List of (extension | restriction | substitution))
  ;;   default = string
  ;;   final = (#all | List of (extension | restriction))
  ;;   fixed = string
  ;;   form = (qualified | unqualified)
  ;;   id = ID
  ;;   maxOccurs = (nonNegativeInteger | unbounded)  : 1
  ;;   minOccurs = nonNegativeInteger : 1
  ;;   name = NCName
  ;;   nillable = boolean : false
  ;;   ref = QName
  ;;   substitutionGroup = QName
  ;;   type = QName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, ((simpleType | complexType)?, (unique | key | keyref)*))
  ;; </element>
  (define-xmp-element nil (xsd "element")
    `(:complex
      (:seq? ,(xsd "annotation")
	     (:seq
	      (:or ,(xsd "simpleType") ,(xsd "complexType"))
	      (:set*
	       ,(xsd "unique")
	       ,(xsd "key")   
	       ,(xsd "keyref"))))))

  ;; <group
  ;;   id = ID
  ;;   maxOccurs = (nonNegativeInteger | unbounded)  : 1
  ;;   minOccurs = nonNegativeInteger : 1
  ;;   name = NCName
  ;;   ref = QName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (all | choice | sequence)?)
  ;; </group>
  (define-xmp-element nil (xsd "group")
    `(:complex (:seq? ,(xsd "annotation")
		      (:or ,(xsd "all") ,(xsd "choice")
			   ,(xsd "sequence")))))

  ;; <complexType
  ;;   abstract = boolean : false
  ;;   block = (#all | List of (extension | restriction))
  ;;   final = (#all | List of (extension | restriction))
  ;;   id = ID
  ;;   mixed = boolean : false
  ;;   name = NCName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (simpleContent | complexContent
  ;;                            | ((group | all | choice | sequence)?,
  ;;                               ((attribute | attributeGroup)*, anyAttribute?)
  ;;                            )))
  ;; </complexType>
  (define-xmp-element nil (xsd "complexType")
    `(:complex
      (:seq? ,(xsd "annotation")
	     (:or ,(xsd "simpleContent") ,(xsd "complexContent")
		  (:seq (:or ,(xsd "group") ,(xsd "all")
			     ,(xsd "choice") ,(xsd "sequence"))
			(:set* ,(xsd "attribute") ,(xsd "attributeGroup"))
			,(xsd "anyAttribute")))

	     (:maybe 
	      ,(xsd "any")
	      ,(xsd "element")
	      (:any) ;;;wsdl:element  occurs in Agni Find MP3 on xmethods
	      ))))

  ;; <redefine
  ;;   id = ID
  ;;   schemaLocation = anyURI
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation | (simpleType | complexType | group | attributeGroup))*
  ;; </redefine>
  (define-xmp-element nil (xsd "redefine")
    `(:complex (:seq? ,(xsd "annotation")
		      (:or 
		       ,(xsd "simpleType") 
		       ,(xsd "complexType") 
		       ,(xsd "group")
		       ,(xsd "attributeGroup") 
		       ))))

  ;; <attribute
  ;;   default = string
  ;;   fixed = string
  ;;   form = (qualified | unqualified)
  ;;   id = ID
  ;;   name = NCName
  ;;   ref = QName
  ;;   type = QName
  ;;   use = (optional | prohibited | required) : optional
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, simpleType?)
  ;; </attribute>
  (define-xmp-element nil (xsd "attribute")
    `(:complex
      (:seq? ,(xsd "simpleType") ,(xsd "annotation"))))

  ;; <attributeGroup
  ;;   id = ID
  ;;   name = NCName
  ;;   ref = QName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, ((attribute | attributeGroup)*, anyAttribute?))
  ;; </attributeGroup>
  (define-xmp-element nil (xsd "attributeGroup") 
    `(:complex
      (:seq? ,(xsd "annotation")
	     (:set*
	      ,(xsd "attribute")
	      ,(xsd "attributeGroup"))
	     ,(xsd "anyAttribute") 
	     )))

  ;; <annotation
  ;;   id = ID
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (appinfo | documentation)*
  ;; </annotation>
  (define-xmp-element nil (xsd "annotation")
    `(:complex (:seq? (:set* ,(xsd "appinfo") ,(xsd "documentation"))
		      (:maybe ,(xmp-any-cpart nil)))))

  ;; <appinfo
  ;;   source = anyURI
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: ({any})*
  ;; </appinfo>
  (define-xmp-element nil (xsd "appinfo")        (xmp-any-type nil))

  ;; <documentation
  ;;   source = anyURI
  ;;   xml:lang = language
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: ({any})*
  ;; </documentation>
  (define-xmp-element nil (xsd "documentation")  (xmp-any-type nil))

  ;; <all
  ;;   id = ID
  ;;   maxOccurs = 1 : 1
  ;;   minOccurs = (0 | 1) : 1
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, element*)
  ;; </all>
  (define-xmp-element nil (xsd "all")           
    `(:complex (:seq? ,(xsd "annotation") (:seq* ,(xsd "element")))))

  ;; <sequence
  ;;   id = ID
  ;;   maxOccurs = (nonNegativeInteger | unbounded)  : 1
  ;;   minOccurs = nonNegativeInteger : 1
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (element | group | choice | sequence | any)*)
  ;; </sequence>
  (define-xmp-element nil (xsd "sequence")
    `(:complex (:seq? ,(xsd "annotation")
		      (:set* ,(xsd "element") ,(xsd "group")
			     ,(xsd "choice") ,(xsd "sequence") ,(xsd "any")))))

  ;; <choice
  ;;   id = ID
  ;;   maxOccurs = (nonNegativeInteger | unbounded)  : 1
  ;;   minOccurs = nonNegativeInteger : 1
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (element | group | choice | sequence | any)*)
  ;; </choice>
  (define-xmp-element nil (xsd "choice")
    `(:complex (:seq? ,(xsd "annotation") 
		      (:set* ,(xsd "element") ,(xsd "group")
			     ,(xsd "choice") ,(xsd "sequence") ,(xsd "any")))))
  
  ;; <any
  ;;   id = ID
  ;;   maxOccurs = (nonNegativeInteger | unbounded)  : 1
  ;;   minOccurs = nonNegativeInteger : 1
  ;;   namespace = ((##any | ##other)
  ;;                | List of (anyURI | (##targetNamespace | ##local)) )  : ##any
  ;;   processContents = (lax | skip | strict) : strict
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?)
  ;; </any>
  (define-xmp-element nil (xsd "any")         `(:complex (:seq? ,(xsd "annotation"))))

  ;; <anyAttribute
  ;;   id = ID
  ;;   namespace = ((##any | ##other)
  ;;                | List of (anyURI | (##targetNamespace | ##local)) )  : ##any
  ;;   processContents = (lax | skip | strict) : strict
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?)
  ;; </anyAttribute>
  (define-xmp-element nil (xsd "anyAttribute")   `(:complex (:set* ,(xsd "annotation"))))

  ;; <simpleType
  ;;   final = (#all | List of (list | union | restriction))
  ;;   id = ID
  ;;   name = NCName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (restriction | list | union))
  ;; </simpleType>
  (define-xmp-element nil (xsd "simpleType") 
    `(:complex
      (:seq?
       ,(xsd "annotation")
       (:or ,(xsd "restriction")
	    ,(xsd "list")
	    ,(xsd "union"))
       (:maybe
	;; This does not seem to be legal
	;; XML Schema syntax but it does
	;; occur in IHS Web Service def
	,(xsd "simpleContent")
	)
       )))


  ;; <restriction     --- in simpleType   (2006-10-24 mm rev:)
  ;;   base = QName
  ;;   id = ID
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: ( annotation?,
  ;;              ( simpleType?,
  ;;                ( minExclusive | minInclusive | maxExclusive | maxInclusive
  ;;                  | totalDigits | fractionDigits | length | minLength | maxLength
  ;;                  | enumeration | whiteSpace | pattern)*))
  ;; </restriction>
  ;;
  ;; <restriction     --- in simpleContent   (2006-10-24 mm rev:)
  ;;   base = QName
  ;;   id = ID
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: ( annotation?,
  ;;              ( simpleType?,
  ;;                ( minExclusive | minInclusive | maxExclusive | maxInclusive
  ;;                  | totalDigits | fractionDigits | length | minLength | maxLength
  ;;                  | enumeration | whiteSpace | pattern)*)?,
  ;;              ((attribute | attributeGroup)*, anyAttribute?))
  ;; </restriction>
  ;;

  
  (define-xmp-element nil (xsd "restriction")
    `(:complex (:seq? ,(xsd "annotation")
		      ,(xsd "simpleType")
		      (:set* ,(xsd "minExclusive") ,(xsd "minInclusive")
			     ,(xsd "maxExclusive") ,(xsd "maxInclusive")
			     ,(xsd "totalDigits") ,(xsd "fractionDigits")
			     ,(xsd "length") ,(xsd "minLength") ,(xsd "maxLength")
			     ,(xsd "enumeration")
			     ,(xsd "whiteSpace")
			     ,(xsd "pattern"))
		      (:set* ,(xsd "attribute") ,(xsd "attributeGroup"))
		      ,(xsd "anyAttribute")
		      (:maybe
		       ,(xsd "sequence")
		       ,(xsd "maxlength") ;;; mis-spelled in one WSDL at XMethods
		       ))))

  ;; <simpleContent
  ;;   id = ID
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (restriction | extension))
  ;; </simpleContent>
  (define-xmp-element nil (xsd "simpleContent")
    `(:complex (:seq? ,(xsd "annotation")
		      (:set* ,(xsd "restriction")
			     ,(xsd "extension")))))

  ;; <union
  ;;   id = ID
  ;;   memberTypes = List of QName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, simpleType*)
  ;; </union>
  (define-xmp-element nil (xsd "union")
    `(:complex (:seq? ,(xsd "annotation") 
		      (:seq* ,(xsd "simpleType")))))

  ;; <extension
  ;;   base = QName
  ;;   id = ID
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, ((attribute | attributeGroup)*, anyAttribute?))
  ;; </extension>
  ;; 
  ;; <extension
  ;;   base = QName
  ;;   id = ID
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: ( annotation?,
  ;;              ( (group | all | choice | sequence)?,
  ;;                ((attribute | attributeGroup)*, anyAttribute?)))
  ;; </extension>
  (define-xmp-element nil (xsd "extension")
    `(:complex (:seq? ,(xsd "annotation")
		      (:or ,(xsd "group") ,(xsd "all") ,(xsd "choice") ,(xsd "sequence"))
		      (:set* ,(xsd "attribute") ,(xsd "attributeGroup"))
		      ,(xsd "anyAttribute")
		      )))

  ;; <enumeration
  ;;   id = ID
  ;;   value = anySimpleType
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?)
  ;; </enumeration>
  (define-xmp-element nil (xsd "enumeration") `(:complex (:seq? ,(xsd "annotation"))))

  ;; <complexContent
  ;;   id = ID
  ;;   mixed = boolean
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (restriction | extension))
  ;; </complexContent>
  ;;
  ;; <restriction
  ;;   base = QName
  ;;   id = ID
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: ( annotation?,
  ;;             (group | all | choice | sequence)?,
  ;;             ((attribute | attributeGroup)*, anyAttribute?))
  ;; </restriction>

  (define-xmp-element nil (xsd "complexContent")
    `(:complex (:seq? ,(xsd "annotation")
		      (:or (:element
			    ,(xsd "restriction")
			    ;; 2006-10-24 mm rev: insert content def from spec.
			    (:complex
			     (:seq?
			      ,(xsd "annotation")
			      (:or ,(xsd "group") ,(xsd "all") ,(xsd "choice")
				   ,(xsd "sequence"))
			      (:set* ,(xsd "attribute") ,(xsd "attributeGroup"))
			      ,(xsd "anyAttribute"))))
			   ,(xsd "extension")))))

  ;; <field
  ;;   id = ID
  ;;   xpath = a subset of XPath expression, see below
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?)
  ;; </field>
  (define-xmp-element nil (xsd "field")   `(:complex (:seq? ,(xsd "annotation"))))

  (define-xmp-element nil (xsd "length")  `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "import")  `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "include") `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "pattern") `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "selector") `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "maxInclusive") `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "minInclusive") `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "maxExclusive") `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "minExclusive") `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "maxLength")    `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "maxlength")    `(:complex (:seq? ,(xsd "annotation"))))
  (define-xmp-element nil (xsd "minLength")    `(:complex (:seq? ,(xsd "annotation"))))

  ;; <key
  ;;   id = ID
  ;;   name = NCName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (selector, field+))
  ;; </key>
  (define-xmp-element nil (xsd "key")     `(:complex (:seq? ,(xsd "annotation")
							  (:seq1 ,(xsd "selector"))
							  (:seq+ ,(xsd "field"))
							  )))

  ;; <keyref
  ;;   id = ID
  ;;   name = NCName
  ;;   refer = QName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (selector, field+))
  ;; </keyref>
  (define-xmp-element nil (xsd "keyref")  `(:complex (:seq? ,(xsd "annotation")
							  (:seq1 ,(xsd "selector"))
							  (:seq+ ,(xsd "field"))
							  )))

  ;; <unique
  ;;   id = ID
  ;;   name = NCName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, (selector, field+))
  ;; </unique>
  (define-xmp-element nil (xsd "unique")  `(:complex (:seq? ,(xsd "annotation")
							  (:seq1 ,(xsd "selector"))
							  (:seq+ ,(xsd "field"))
							  )))

  ;; <list
  ;;   id = ID
  ;;   itemType = QName
  ;;   {any attributes with non-schema namespace . . .}>
  ;;   Content: (annotation?, simpleType?)
  ;; </list>
  (define-xmp-element nil (xsd "list")    `(:complex (:seq? ,(xsd "annotation")
							  ,(xsd "simpleType"))))
  )


(defmethod xmp-decode-element :around ((conn schema-file-connector)
				      (elt (eql (xsd "schema"))) (data t)
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
  (or (consp elt) (setf elt (list 'quote elt)))
  `(progn
     (defmethod schema-make-element ((conn ,pclass) (elt (eql ,elt))
				     &rest keys &key &allow-other-keys)
       (apply #'call-next-method conn elt :tag ',tag :class ',cclass keys))
     (defmethod xmp-complex-content ((conn ,pclass)
				     (elt (eql ,elt))
				     data
				     &rest options &key &allow-other-keys)
       (apply #'call-next-method conn elt data :warn nil options))))

(define-schema-default-part schema-file-connector (xsd "schema") :schema)
(define-schema-default-part schema-file-connector (xsd "complexContent")
  :complex-content)
(define-schema-default-part schema-file-connector (xsd "annotation")
  :annotation :cclass schema-text-component)
(define-schema-default-part schema-file-connector (xsd "documentation")
  :documentation :cclass  schema-text-component)



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
				    name value nss
				    &aux (attr name)
				    (attrkey (intern (string attr) :keyword))
				    attr-loose attr-targeted
				    )
  (cond 
    ((or (when (eq attrkey (xmp-symbol "base" :keyword))
	   (setf attr-loose 'base-loose attr-targeted 'base-targeted) t)
	 (when (eq attrkey (xmp-symbol "type" :keyword))
	   (setf attr-loose 'type-loose attr-targeted 'type-targeted) t)
	 ;; 2006-10-24 mm rev: ref attribute is one more for strict/loose treatment
	 (when (eq attrkey (xmp-symbol "ref" :keyword))
	   (setf attr-loose 'ref-loose attr-targeted 'ref-targeted) t)
	 )
     (let* ((strict (xmp-decode-qualified-name conn value nss :suppress-default t))
	    (not-strict (case (xmp-xml-syntax conn) (:strict nil) (otherwise t)))
	    (targeted (when not-strict (schema-targeted-name conn value :nss nss)))
	    (loose (when not-strict 
		     (xmp-decode-qualified-name conn value nss :suppress-default nil)))
	    )
       (values-list (list* attr strict
			   (when (or (and targeted (not (eq strict targeted)))
				     (and loose (not (eq strict loose))))
			     (cond ((eq targeted loose) (list attr-loose loose))
				   ((null loose) (list attr-targeted targeted))
				   ((null targeted) (list attr-loose loose))
				   ((eq strict targeted) (list attr-loose loose))
				   ((eq strict loose) (list attr-targeted targeted))
				   (t (list attr-targeted targeted
					    attr-loose loose))))))
       ))
    ((eq attrkey (xmp-symbol "maxOccurs" :keyword))
     (values attr (if (string-equal "unbounded" value)
		      most-positive-fixnum
		    (parse-integer value))))
    ((eq attrkey (xmp-symbol "minOccurs" :keyword)) (values attr (parse-integer value)))
    (t (values attr value))
    ))
	  
(defmethod schema-decode-attributes ((conn schema-file-connector)
				     attributes nss)
  (do ((tail attributes (cddr tail)) res)
      ((atom tail) res)
    (setf res (append
	       (multiple-value-list
		(schema-decode-attribute conn (first tail) (second tail) nss))
	       res))))

(defmethod schema-targeted-name ((conn schema-file-connector) tname
				 &key (nss :in) (suppress-default t))
  ;; Returns two values, like xmp-decode-qualified-name.
  (let* ((name (string tname))
	 (targets (schema-target conn))
	 (target (first targets))  ;;; Use the target at the top of the stack.
	 (tp (when target (xmp-uri-to-package conn target nss)))
	 (*package* (resolve-package tp)))
    (if tp
	;; if there is a targetNamespace, set it as default and use it
	(xmp-decode-qualified-name conn name (cons (list target) nss))
      ;; otherwise there is no default that applies
      (xmp-decode-qualified-name conn name nss :suppress-default suppress-default))))

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
       (schema-targeted-name conn name))
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
  (or (consp elt) (setf elt (list 'quote elt)))
  `(progn
     (defmethod xmp-begin-element :around ((conn ,pclass) 
					  (elt (eql ,elt))
					  &rest options
					  &key attributes &allow-other-keys)
       (apply #'call-next-method conn elt
	      :schema-component
	      (schema-begin-part conn elt ',cclass attributes ',tag ',acc)
	      options))
     (defmethod xmp-complex-content ((conn ,pclass) 
				     (elt (eql ,elt)) data
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
			 :schema-source conn
			 ))
    (push part (schema-context conn))
    (or (cdr (schema-context conn))
	(add-to-list part place))
    part))




(defmethod schema-collected-parts ((comp schema-component) this-key &optional sub-key
				   &aux
				   (all (schema-component-content comp))
				   subset)
  (or (eq this-key  (schema-element-key comp)) 
      (error "Schema component mismatch"))
  (if sub-key
      (dolist (a all (nreverse subset))
	(when (eq sub-key (schema-element-key a))
	  (push a subset)))
    all))

(defun any-part-p (x) (declare (ignore x)) t)

(defmethod schema-single-part ((comp schema-component) key
			       &key (error-p t) more-p only-p ignored
			       &aux
			       (all (schema-component-content comp))
			       a found)
  ;; look for sub-elements that match key
  ;;   key -> string | symbol | (symbol...) | predicate-function
  ;;   if more-p is nil, return nil or the only match
  ;;            otherwise return list of matches
  ;;   if only-p is nil ignore other elements
  ;;            otherwise ignore only elements that match ignored
  ;;   if error-p is non-nil, signal error
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
		 (t (return
		     (schema-error-p 
		      nil error-p
		      "Schema component ~S has too many parts with key ~S"
		      comp key)))))
	  ((null only-p))
	  ((typecase ignored
	     (null nil)
	     (cons (member a ignored))
	     (function (funcall ignored a))
	     (otherwise (equal ignored a)))
	   )
	  (t (return
	      (schema-error-p 
	       nil error-p
	       "Schema component ~S has parts other than ~S or ~S"
	       comp key ignored))))))
	 

(define-schema-collected-part
  schema-file-connector (xsd "element")     :element      schema-elements)
(define-schema-collected-part
  schema-file-connector (xsd "complexType") :complex-type schema-types)
(define-schema-collected-part
  schema-file-connector (xsd "simpleType")  :simple-type  schema-types)
(define-schema-collected-part
  schema-file-connector (xsd "attribute")   :attribute    schema-attributes)
(define-schema-collected-part
  schema-file-connector (xsd "group")       :group        schema-groups)
(define-schema-collected-part
  schema-file-connector (xsd "attributeGroup") :a-group   schema-a-groups)
(define-schema-collected-part
  schema-file-connector (xsd "import") :import   schema-imports)
(define-schema-collected-part
  schema-file-connector (xsd "include") :include   schema-imports)

(defmacro define-schema-nested-part (class elt tag 
					   &optional (cclass 'schema-component))
  (or (consp elt) (setf elt (list 'quote elt)))
  `(progn
     (defmethod xmp-begin-element :around ((conn ,class) 
					  (elt (eql ,elt))
					  &rest options &key attributes 
					  &allow-other-keys
					  &aux part)
       (setf part (make-instance ',cclass
			 :schema-component-raw-attributes attributes
			 :schema-component-decoded-attributes
			 (schema-decode-attributes conn attributes :in)
			 :schema-element-tag ,elt
			 :schema-element-key ',tag
			 :schema-source conn
			 ))
       (push part (schema-context conn))
       (apply #'call-next-method conn elt :schema-component part options))
     (defmethod xmp-complex-content ((conn ,class) 
				     (elt (eql ,elt))
				     data
				     &rest options &key &allow-other-keys)
       (pop (schema-context conn))
       (apply #'call-next-method conn elt data :warn nil options))))

;; ??? minOccurs maxOccurs attributes may apply
;;     xs:sequence   min=0  max=Unbounded default=1
;;     xs:all        min=0  max=1         default=1
;;     xs:choice     min=0  max=Unbounded default=1
(define-schema-nested-part schema-file-connector (xsd "sequence") :seq*) 
(define-schema-nested-part schema-file-connector (xsd "all")      :set)
(define-schema-nested-part schema-file-connector (xsd "choice")   :or)


(defmacro define-schema-named-part (pclass elt tag 
					   &key 
					   (class 'schema-named-component)
					   (qname t))
  (or (consp elt) (setf elt (list 'quote elt)))
  `(progn
     (defmethod xmp-begin-element :around ((conn ,pclass) 
					  (elt (eql ,elt))
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
				   :schema-element-tag ,elt
				   :schema-source conn
				   ))
	 (apply #'call-next-method conn elt :schema-component part options)))
     (defmethod xmp-complex-content ((conn ,pclass) 
				     (elt (eql ,elt))
				     data
				     &rest options &key &allow-other-keys)
       (apply #'call-next-method conn elt data :warn nil options)
       )))

(defmacro define-schema-simple-part (pclass elt tag
					    &optional
					    (cclass 'schema-simple-component)
					    )
  (or (consp elt) (setf elt (list 'quote elt)))
  `(progn
     (defmethod xmp-begin-element :around ((conn ,pclass) 
					  (elt (eql ,elt))
					  &rest options &key attributes 
					  &allow-other-keys
					  &aux part)
       (setf part (make-instance ',cclass
				 :schema-component-raw-attributes attributes
				 :schema-component-decoded-attributes
				 (schema-decode-attributes conn attributes :in)
				 :schema-element-key ',tag
				 :schema-element-tag ,elt
				 :schema-source conn
				 ))
       (apply #'call-next-method conn elt :schema-component part options))
     (defmethod xmp-complex-content ((conn ,pclass) 
				     (elt (eql ,elt))
				     (data t)
				     &rest options &key &allow-other-keys)
       (apply #'call-next-method conn elt data :warn nil options))))


(defmacro define-schema-ignored-part (class elt)
  (or (consp elt) (setf elt (list 'quote elt)))
  `(progn
     (defmethod xmp-begin-element :around ((conn ,class) 
					   (elt (eql ,elt))
					   &rest options &key &allow-other-keys)
       (declare (ignore options))
       (xmp-any-cpart conn))

     (defmethod xmp-decode-element ((conn ,class) 
				    (elt (eql ,elt))
				    (data t)
				    &rest options &key &allow-other-keys)
       (declare (ignore options))
       nil)))

;; 2006-10-24 mm rev: supress a few more warnings...
(define-schema-ignored-part schema-file-connector (xsd "whiteSpace"))
(define-schema-ignored-part schema-file-connector (xsd "key"))
(define-schema-ignored-part schema-file-connector (xsd "fractionDigits"))
(define-schema-ignored-part schema-file-connector (xsd "totalDigits"))
(define-schema-ignored-part schema-file-connector (xsd "notation"))


(define-schema-simple-part schema-file-connector (xsd "extension")     :extension)
(define-schema-simple-part schema-file-connector (xsd "simpleContent") :simple-content)
(define-schema-simple-part schema-file-connector (xsd "restriction")   :restriction)
(define-schema-simple-part schema-file-connector (xsd "maxLength")     :max-length)
(define-schema-simple-part schema-file-connector (xsd "maxlength")     :max-length)
(define-schema-simple-part schema-file-connector (xsd "minLength")     :min-length)
(define-schema-simple-part schema-file-connector (xsd "pattern")       :pattern)
(define-schema-simple-part schema-file-connector (xsd "any")           :any)
(define-schema-simple-part schema-file-connector (xsd "anyAttribute")  :any-attribute)
(define-schema-simple-part schema-file-connector (xsd "list")          :list)
(define-schema-simple-part schema-file-connector (xsd "union")         :union)
(define-schema-simple-part schema-file-connector (xsd "enumeration")   :enumeration)
(define-schema-simple-part schema-file-connector (xsd "minInclusive")  :min-inclusive)
(define-schema-simple-part schema-file-connector (xsd "maxInclusive")  :max-inclusive)
(define-schema-simple-part schema-file-connector (xsd "minExclusive")  :min-exclusive)
(define-schema-simple-part schema-file-connector (xsd "maxExclusive")  :max-exclusive)


(defmethod schema-base-type ((conn schema-file-connector) ext)
  (schema-maybe-loose-type conn ext 'base-loose 'base-targeted))

(defmethod schema-maybe-loose-type ((conn schema-file-connector) ext
                                    attr-loose attr-targeted)
  ;; Return a type-spec or nil
  (let* ((strict (schema-decoded-attribute ext "base"))
         (targeted (schema-decoded-attribute ext attr-targeted))
         (loose (schema-decoded-attribute ext attr-loose))
         (component
          (or (and strict  ;;; 2006-10-24 mm rev: [bug16483] avoid lookup of nil
		   (schema-lookup-type conn strict))
              (case (xmp-xml-syntax conn)
                (:strict nil)
                (otherwise
                 (or
		  (and targeted	;;; 2006-10-24 mm rev: [bug16483] avoid lookup of nil
		       (schema-lookup-type conn targeted))
		  (and loose ;;; 2006-10-24 mm rev: [bug16483] avoid lookup of nil
		       (schema-lookup-type conn loose))))))))
    (if component
        (schema-parts-to-type component)
      (flet ((find-type (conn name nss)
			;; 2006-10-24 mm rev: [bug16483] avoid lookup of nil type 
			;;  and keep named base types as names
                        (let ((def (when name (xmp-find-type conn name nss))))
                          (if (and (consp def)
                                   (eq :simple (first def))
                                   (null (second def)))
                              name
                            def))))
        (or (find-type conn strict :in)
            (case (xmp-xml-syntax conn)
              (:strict nil)
              (otherwise
               (or (find-type conn targeted :in)
                   (find-type conn loose :in)))))))))



(defmethod schema-lookup-type ((conn schema-file-connector) name)
  (schema-collected-component conn #'schema-types #'schema-component-name name))

(defmethod schema-lookup-element ((conn schema-file-connector) name)
  (schema-collected-component conn #'schema-elements #'schema-component-name name))

(defmethod schema-lookup-group ((conn schema-file-connector) comp &aux name)
  ;; 2006-10-24 mm rev: 
  ;; If comp is a <group> element that references a definition,
  ;;    then find the definition and return it.
  (or
   (and (setf name (schema-decoded-attribute comp "ref"))
	(schema-collected-component conn #'schema-groups #'schema-component-name name))
   (case (xmp-xml-syntax conn)
     (:strict nil)
     (otherwise
      (or
       (and (setf name (schema-decoded-attribute comp 'ref-targeted))
	    (schema-collected-component
	     conn #'schema-groups #'schema-component-name name))
       (and (setf name (schema-decoded-attribute comp 'ref-loose))
	    (schema-collected-component
	     conn #'schema-groups #'schema-component-name name)))))))


(define-schema-elements)

