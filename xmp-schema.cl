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

;; $Id: xmp-schema.cl,v 1.1.1.1 2003/07/24 00:49:45 layer Exp $

;; XML Schema support

(in-package :net.xmp)

(eval-when (compile) (pushnew :xmp-dev *features*))

(defpackage :net.xmp.schema
  #+xmp-dev(:nicknames :xs :xsd)
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
   "any"
   "annotation"
   "anyAttribute"
   "simpleContent"
   "documentation"
   "restriction"
   "pattern"
   "list"
   "extension"

   ;; simpleType names (incomplete)

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
  #+xmp-dev(:nicknames :xsi)
  (:use)
  (:export 
   "type"
   ))


(defclass xmp-schema-file-connector (xmp-string-in-connector)
  (
   (protocol   :initarg :xml-schema)
   (source     :reader   xmp-schema-source :initarg :source)
   (target     :accessor xmp-schema-target :initform nil)
   (prefix     :accessor xmp-schema-prefix :initform "tns")
   (elements   :accessor xmp-schema-elements :initform nil)
   (types      :accessor xmp-schema-types    :initform nil)
   (attributes :accessor xmp-schema-attributes :initform nil)
   (groups     :accessor xmp-schema-groups     :initform nil)
   (a-groups   :accessor xmp-schema-a-groups   :initform nil)
   (context    :accessor xmp-schema-context    :initform nil)

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

(defun xmp-schema (file &key (verbose t))
  (let* ((conn (make-instance 'xmp-schema-file-connector :source file)))

    ;; This prefix is reserved in XML schemas
    (push (list nil "xs") (xmp-defined-namespaces conn))

    (xmp-decode-message conn (xmp-parse-message conn nil))
    (when verbose
      (format t "~&~%Elements:~%~S~%" (xmp-schema-elements conn))
      (format t "~%Types:~%~S~%" (xmp-schema-types conn))
      (format t "~%Attributes:~%~S~%" (xmp-schema-attributes conn))
      (format t "~%Groups:~%~S~%" (xmp-schema-groups conn))
      (format t "~%AGroups:~%~S~2%" (xmp-schema-a-groups conn)))
    conn))


(defmethod xmp-parse-message ((conn xmp-schema-file-connector) string &key namespaces) 
  ;; namespaces???
  (declare (ignore string namespaces))
  (multiple-value-bind (xml ns)
      (with-open-file (s (xmp-schema-source conn))
		      (call-next-method conn s))
    (setf (xmp-message-pns conn) ns)
    xml))

(defmethod xmp-begin-message ((conn xmp-schema-file-connector))
  (list :seq1 'xs:|schema|))

(defmethod xmp-end-message ((conn xmp-schema-file-connector) data
			    &key types &allow-other-keys)
  (values data types))


(define-xmp-element nil 'xs:|schema| '(:complex
				       (:set* xs:|element| xs:|complexType| 
					      xs:|attribute| xs:|simpleType| 
					      xs:|attributeGroup| xs:|group|
					      )))
(define-xmp-element nil 'xs:|group|       '(:complex (:set* xs:|sequence|)))
(define-xmp-element nil 'xs:|complexType| '(:complex
					    (:set* xs:|sequence| xs:|any| xs:|annotation|
						   xs:|anyAttribute| xs:|attributeGroup| 
						   xs:|group| xs:|simpleContent|
						   )))
(define-xmp-element nil 'xs:|attribute|      '(:complex
					       (:set* xs:|simpleType| xs:|annotation|)))
(define-xmp-element nil 'xs:|attributeGroup| '(:complex
					       (:set* xs:|attribute| xs:|anyAttribute| 
						      xs:|annotation|)))
(define-xmp-element nil 'xs:|annotation|     '(:complex (:set* xs:|documentation|)))
(define-xmp-element nil 'xs:|sequence|       '(:complex (:set* xs:|element| xs:|any|)))
(define-xmp-element nil 'xs:|anyAttribute|   '(:complex (:set* xs:|annotation|)))
(define-xmp-element nil 'xs:|simpleType|     '(:complex
					       (:set* xs:|restriction|
						      xs:|annotation|
						      xs:|list|)))
(define-xmp-element nil 'xs:|restriction|    '(:complex (:set* xs:|pattern|)))
(define-xmp-element nil 'xs:|simpleContent|  '(:complex (:set* xs:|extension|)))
(define-xmp-element nil 'xs:|extension|      '(:complex (:set* xs:|attributeGroup|)))




#+ignore
(defmethod xmp-begin-element :after ((conn xmp-schema-file-connector)
				     (elt (eql 'xs:|schema|))
				     &rest options
				     &key attributes &allow-other-keys)
  (declare (ignore options))
  (do ((att attributes (cddr att)) name)
      ((atom att))
    (setf name (first att))
    (cond ((equal name xs:|targetNamespace|) 
	   (setf (xmp-schema-target conn) (second att)))))

  )

				     


(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|annotation|))
				data &rest options &key &allow-other-keys)
  (declare (ignore options data))
  nil)

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|documentation|))
				data &rest options &key &allow-other-keys)
  (declare (ignore options data))
  (values))

(defun drop-two (list tail &aux head)
  (do ((tl list (cddr tl)))
      ((atom tl) (nreverse head))
    (when (eq tl tail)
	(return (nconc (nreverse head) (cddr tl))))
    (push (car tl) head)
    (push (cadr tl) head)))


(defun xmp-schema-name-attr (attributes &optional key data)
  (let* (tail
	 (name (second (or (setq tail (member "name" attributes :test #'string-equal))
			   (setq tail (member "ref" attributes :test #'string-equal)))))
	 (new (list* name
		     (append
		      (when data (list key data))
		      (drop-two attributes tail))))
	 )
    new))

(defmacro xmp-update-part (conn key new acc)
  (let ((cv (gensym)) (kv (gensym)) (nv (gensym)))
    `(let ((,cv ,conn) (,kv ,key) (,nv ,new))
       (case (caar (xmp-schema-context ,cv))
	 ((nil) (push ,nv (,acc ,cv)) nil)
	 (otherwise (list (list* ,kv ,nv)))))))

(defmethod xmp-update-content ((conn xmp-schema-file-connector) key data acc  
			       &aux result (context (xmp-schema-context conn)))
  (when data (setf result (list data key)))
  (if (null (cdr context))
      (let* ((place (assoc (second (first context))
			   (funcall acc conn) :test #'string-equal)))
	(dolist (d result) (push d (cdr place)))
	(setf result nil))
    (setf result (list (append (car context) (reverse result)))))
  result)

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|element|)) data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (xmp-update-part 
   conn :element (xmp-schema-name-attr attributes data) xmp-schema-elements))

(defmethod xmp-begin-element :after ((conn xmp-schema-file-connector) 
				     (elt (eql 'xs:|complexType|))
				     &rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (let* ((new (xmp-schema-name-attr attributes)) 
	 (result (xmp-update-part conn :complex-type new xmp-schema-types)))
    (push (list* :complex-type new) (xmp-schema-context conn))
    result))

  

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|complexType|)) data
				&rest options &key &allow-other-keys
				&aux result)
  (declare (ignore options))
  (setf result (xmp-update-content conn :complex-type data 'xmp-schema-types))
  (pop (xmp-schema-context conn))
  result)

(defmethod xmp-begin-element :after ((conn xmp-schema-file-connector) 
				     (elt (eql 'xs:|simpleType|))
				     &rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (let* ((new (xmp-schema-name-attr attributes)) 
	 (result (xmp-update-part conn :simple-type new xmp-schema-types)))
    (push (list* :simple-type new) (xmp-schema-context conn))
    result))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|simpleType|))
				data
				&rest options &key &allow-other-keys
				&aux result)
  (declare (ignore options))
  (setf result (xmp-update-content conn :simple-type data 'xmp-schema-types))
  (pop (xmp-schema-context conn))
  result)

	      
(defmethod xmp-begin-element :after ((conn xmp-schema-file-connector) 
				     (elt (eql 'xs:|attribute|))
				     &rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (let* ((new (xmp-schema-name-attr attributes)) 
	 (result (xmp-update-part conn :attribute new xmp-schema-attributes)))
    (push (list* :attribute new) (xmp-schema-context conn))
    result))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|attribute|))
				data
				&rest options &key &allow-other-keys
				&aux result)
  (declare (ignore options))
  (setf result (xmp-update-content conn :data data 'xmp-schema-attributes))
  (pop (xmp-schema-context conn))
  result)

(defmethod xmp-begin-element :after ((conn xmp-schema-file-connector) 
				     (elt (eql 'xs:|group|))
				     &rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (let* ((new (xmp-schema-name-attr attributes)) 
	 (result (xmp-update-part conn :group new xmp-schema-groups)))
    (push (list* :group new) (xmp-schema-context conn))
    result))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|group|)) data
				&rest options &key &allow-other-keys
				&aux result)
  (declare (ignore options))
  (setf result (xmp-update-content conn :data data 'xmp-schema-groups))
  (pop (xmp-schema-context conn))
  result)
	
(defmethod xmp-begin-element :after ((conn xmp-schema-file-connector) 
				     (elt (eql 'xs:|attributeGroup|))
				     &rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (let* ((new (xmp-schema-name-attr attributes)) 
	 (result (xmp-update-part conn :a-group new xmp-schema-a-groups)))
    (push (list* :a-group new) (xmp-schema-context conn))
    result))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|attributeGroup|)) data
				&rest options &key &allow-other-keys
				&aux result)
  (declare (ignore options))
  (setf result (xmp-update-content conn :data data 'xmp-schema-a-groups))
  (pop (xmp-schema-context conn))
  result)
	

(defmethod xmp-begin-element :after ((conn xmp-schema-file-connector) 
				     (elt (eql 'xs:|sequence|))
				     &rest options &key &allow-other-keys)
  (declare (ignore options))
  (push (list :sequence) (xmp-schema-context conn)))


(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|sequence|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (pop (xmp-schema-context conn))
  (append (when attributes
	    (list (list* :attributes attributes)))
	  data))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|extension|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list
   (list* :extension
	  (append attributes (if (null (cdr data)) (car data) data)))))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|simpleContent|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list
   (list* :simple
	  (append attributes (if (null (cdr data)) (car data) data)))))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|restriction|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list
   (list* :restriction
	  (append (when attributes
		    (list (list* :attributes attributes)))
		  data))))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|pattern|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list
   (list* :pattern
	  (append (when attributes
		    (list (list* :attributes attributes)))
		  data))))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|any|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list
   (list* :any
	  (append (when attributes
		    (list (list* :attributes attributes)))
		  data))))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|anyAttribute|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list
   (list* :any-attribute
	  (append (when attributes
		    (list (list* :attributes attributes)))
		  data))))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|list|))
				data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (list
   (list* :list
	  (append (when attributes
		    (list (list* :attributes attributes)))
		  data))))

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) 
				(elt (eql 'xs:|schema|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (when data (xmp-error conn :client :string "Unprocessed data"))
  nil)

(defmethod xmp-complex-content ((conn xmp-schema-file-connector) (elt t) data
				&rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  (format t "~&;;xmp-complex-content ~S~%" elt)
  (list (list* (concatenate 'vector (list* elt attributes)) data)))

