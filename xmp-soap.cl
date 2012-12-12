;; -*- mode: common-lisp; package: net.xmp.soap -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2003-2012 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: xmp-soap.cl,v 2.17 2009/05/01 16:03:53 layer Exp $

;; SOAP support

(defpackage :net.xmp.soap)
(in-package :net.xmp.soap)

(defpackage :net.xmp.soap (:use :common-lisp :excl :net.xmp))

(eval-when (compile eval)
  (defmacro soap-case-mode () *current-case-mode*))

(defvar *soap-server-debug* nil)
(defvar *soap-client-debug* nil)

(defun soap-version (&rest args)
  (apply 'xmp-version args))
    

(defpackage :net.xmp.soap
  
  (:export
   
   define-namespace
   delete-namespace
   define-namespace-map

   soap-version
   soap-message-client
   soap-message-server
   soap-export-method
   start-soap-server
   enable-soap-server
   disable-soap-server
   stop-soap-server

   soap-element
   soap-header
   soap-headers
   soap-add-header
   make-soap-header
   soap-must-understand

   soap-sub-element-content
   soap-alist-to-plist
   soap-result-part 
   soap-result-only
   soap-result-pair
   soap-result-string
   soap-result-typed

   define-soap-type
   define-soap-element
   call-soap-method
   *soap-server*
   soap-invoke-method
   soap-encode-object

   soap-port-name
   soap-binding-name
   soap-service-name

   soap-connector

   soap-client-connector
   soap-server-connector

   soap-string-out-connector
   soap-stream-out-connector

   soap-string-in-connector
   soap-event-in-connector

   soap-string-in-out-connector
   soap-event-string-connector
   soap-string-stream-connector
   soap-event-stream-connector
   
   soap-client-string-out-connector
   soap-client-stream-out-connector
   soap-client-string-in-connector
   soap-client-event-in-connector
   soap-client-string-in-out-connector
   soap-client-event-string-connector
   soap-client-string-stream-connector
   soap-client-event-stream-connector

   soap-server-string-out-connector
   soap-server-stream-out-connector
   soap-server-string-in-connector
   soap-server-event-in-connector
   soap-server-string-in-out-connector
   soap-server-event-string-connector
   soap-server-string-stream-connector
   soap-server-event-stream-connector

   soap-aserve-connector

   soap-aserve-client-connector
   soap-aserve-server-connector

   soap-aserve-string-out-connector
   soap-aserve-stream-out-connector

   soap-aserve-string-in-connector
   soap-aserve-event-in-connector

   soap-aserve-string-in-out-connector
   soap-aserve-event-string-connector
   soap-aserve-string-stream-connector
   soap-aserve-event-stream-connector
   
   soap-aserve-client-string-out-connector
   soap-aserve-client-stream-out-connector
   soap-aserve-client-string-in-connector
   soap-aserve-client-event-in-connector
   soap-aserve-client-string-in-out-connector
   soap-aserve-client-event-string-connector
   soap-aserve-client-string-stream-connector
   soap-aserve-client-event-stream-connector

   soap-aserve-server-string-out-connector
   soap-aserve-server-stream-out-connector
   soap-aserve-server-string-in-connector
   soap-aserve-server-event-in-connector
   soap-aserve-server-string-in-out-connector
   soap-aserve-server-event-string-connector
   soap-aserve-server-string-stream-connector
   soap-aserve-server-event-stream-connector

   soap-server-message-method 
   soap-server-message-return
   soap-server-message-signature
   soap-server-message-action 
   soap-message-body
   soap-message-headers
   soap-get-attribute 
   soap-get-attributes 
   soap-make-fault
   soap-new-environment

   *soap-server-debug*
   *soap-client-debug*

   ))


(eval-when (compile load eval)
  (defpackage :net.xmp.soap.none
    (:use)
    (:export 
     "faultcode"
     "faultstring"
     "faultactor"
     "detail"
     )
    )

  (defpackage :net.xmp.soap.envelope
    (:use)
    (:export 
     "Envelope"
     "Header"
     "Body"
     "mustUnderstand"
     "actor"
     "Fault"
     "faultcode"
     "faultstring"
     "faultactor"
     "detail"
     "VersionMismatch"
     "MustUnderstand"
     "Client"
     "Server"
     "encodingStyle"
     ))

  (defpackage :net.xmp.soap.encoding
    (:use)
    (:export
     "arrayCoordinate"
     "arrayType"
     "offset"
     "position"
     "Array"
     "Struct"
     "duration"
     "dateTime"
     "NOTATION"
     "time"
     "date"
     "gYearMonth"
     "gYear"
     "gMonthDay"
     "gDay"
     "gMonth"
     "boolean";; simple-content enc:boolean
     "base64"
     "base64Binary";; simple-content enc:base64Binary
     "hexBinary"
     "float";; simple-content enc:float
     "double";; simple-content enc:double
     "anyURI"
     "QName";; simple-content enc:QName
     "string";; simple-content enc:string
     "normalizedString"
     "token"
     "language"
     "Name"
     "NMTOKEN"
     "NCName"
     "NMTOKENS"
     "ID"
     "IDREF"
     "ENTITY"
     "IDREFS"
     "ENTITIES"
     "decimal"
     "integer"
     "nonPositiveInteger"
     "negativeInteger"
     "long"
     "int";; simple-content enc:int
     "short"
     "byte"
     "nonNegativeInteger"
     "unsignedLong"
     "unsignedInt"
     "unsignedShort"
     "unsignedByte"
     "positiveInteger"
     ))

  )

(eval-when (compile)
  (defpackage :net.xmp.soap.none (:use) (:nicknames :none))
  (defpackage :net.xmp.soap.envelope (:use) (:nicknames :env))
  (defpackage :net.xmp.soap.encoding (:use) (:nicknames :enc))
  (defmacro none (x) (list 'xmp-symbol x :net.xmp.soap.none))
  (defmacro env  (x) (list 'xmp-symbol x :net.xmp.soap.envelope))
  (defmacro enc  (x) (list 'xmp-symbol x :net.xmp.soap.encoding))
  )

 
(def-xmp-sub-classes ("soap" "connector") (("xmp" "connector"))

  (nil
   (
    (envelope-uri    :accessor soap-envelope-uri
		     :allocation :class 
		     :initform "http://schemas.xmlsoap.org/soap/envelope/")
    (schema-inst-uri :accessor soap-schema-inst-uri 
		     :allocation :class
		     :initform "http://www.w3.org/1999/XMLSchema-instance")
    (schema-def-uri  :accessor soap-schema-def-uri 
		     :allocation :class
		     :initform "http://www.w3.org/1999/XMLSchema")
    (default-actor   :accessor soap-default-actor
      :allocation :class
      :initform "http://schemas.xmlsoap.org/soap/actor/next")

    (encoding-style  :accessor soap-encoding-style
		     ;; Sent out as encodingStyle attribute.
		     ;; URIs separated by spaces, most specific first.
		     :initarg :encoding-style
		     :initform "http://schemas.xmlsoap.org/soap/encoding/")
    (actor           :accessor soap-actor
		     :initarg :actor
		     :initform "http://schemas.xmlsoap.org/soap/actor/next")
    (message-dns     :initform nil)
    (base-dns        :initform :soap)

    (soap-headers    :accessor soap-headers :initform nil)
    (soap-body-form  :accessor soap-body-form 
		     ;; values:   :one   :many   [rfe6217]
		     :initarg :body-form :initform :one)
    (lisp-package    :initform :net.xmp.soap.none)
    (trim-whitespace :initform t)
    (must-understand :accessor soap-must-understand-flag
		     :initform :warn :initarg :must-understand)
    (soap-send-type  :accessor soap-send-type :initarg :send-type :initform t)
    (decode-flag     :accessor soap-decode-flag
		     ;; :strict - all elements must be defined 
		     ;;               type attribute must match if present
		     ;; :warn   - warn if undefined element or type mismatch
		     ;;   nil   - accept anything silently
		     :initform :warn :initarg :decode-flag)
    (null-element    :accessor soap-null-element
		     ;; What to send when lisp content is nil:
		     ;; :default-value  send  number->0  string->"" 
		     ;; :empty          send empty element
		     :initarg :null-element :initform :default-value)
    (empty-element   :accessor soap-empty-element
		     ;; How to decode an empty element:
		     ;; :default-value  return  number->0  string->"" 
		     ;; nil             return nil
		     :initarg :empty-element :initform :default-value)
    (string-type     :initform (enc "string")) 
    (undef-list      :accessor soap-undef-list :initform nil)

    (port-name       :accessor soap-port-name    :initarg :port-name    :initform nil)
    (binding-name    :accessor soap-binding-name :initarg :binding-name :initform nil)
    (service-name    :accessor soap-service-name :initarg :service-name :initform nil)

    (outer-element :accessor soap-outer-element :initarg nil :initform nil)

    (soap-debug      :accessor soap-debug :initarg :soap-debug)

    ;; THESE SLOTS ARE NOT COPIED by xmp-copy methods
    (soap-message-body   :accessor soap-message-body   :initform nil
			 :documentation "no-xmp-copy")
    (soap-message-headers :accessor soap-message-headers   :initform nil
			  :documentation "no-xmp-copy")
    (soap-message-arrays :accessor soap-message-arrays :initform nil
			 :documentation "no-xmp-copy")

    ))

  ("client"
   ((soap-debug :initform *soap-client-debug*)
    ))

  ("server"
   (
    (soap-debug :initform *soap-server-debug*)

    ;; THESE SLOTS ARE NOT COPIED by xmp-copy methods
    (action :accessor soap-server-action :initarg :action :initform nil
	    :documentation "no-xmp-copy")
    
    (wsdl  :accessor soap-server-wsdl :initarg :wsdl :initform nil)

    ;; these slots are set before calling soap-invoke-method
    (soap-server-message-method :accessor soap-server-message-method :initform nil
				:documentation "no-xmp-copy")
    (soap-server-message-return :accessor soap-server-message-return :initform nil
				:documentation "no-xmp-copy")
    (soap-server-message-signature :accessor soap-server-message-signature :initform nil
				   :documentation "no-xmp-copy")
    (soap-server-message-action :accessor soap-server-message-action :initform nil
				:documentation "no-xmp-copy")

    ))

  )

(def-xmp-sub-classes ("soap-aserve" "connector")
  (("soap" "connector") ("xmp-aserve" "connector"))

  (nil
   ((method        :initform :post)
    (http-protocol :initform :http/1.0)
    (content-type  :initform "text/xml")
    (xml-leader    :initform "xml version=\"1.0\"")
    ))

  )

(defclass soap-element (xmp-element) ())
(defclass soap-header  (soap-element) ())
(defclass soap-fragment (xmp-element) ())



(define-condition soap-client-error (xmp-client-condition) 
  ((xmp-error-code :initform (env "Client"))))
(define-condition soap-server-error (xmp-server-condition)
  ((xmp-error-code :initform (env "Server"))))
(define-condition soap-client-fault (soap-client-error)
  ((xmp-error-code :initform :client-fault)))
(define-condition soap-server-fault (soap-server-error)
  ((xmp-error-code :initform :server-fault)))

(define-condition soap-mismatch (xmp-condition)
  ((xmp-error-code :initform (env "VersionMismatch"))))
(define-condition soap-must-understand (xmp-condition)
  ((xmp-error-code :initform (env "MustUnderstand"))))

(defmethod soap-client-error ((conn soap-connector) &rest keys)
  (apply 'xmp-error conn 'soap-client-error keys))
(defmethod soap-server-error ((conn soap-connector) &rest keys)
  (apply 'xmp-error conn 'soap-server-error keys))






(defun soap-message-client (&rest options
				  &key (class
					'soap-aserve-client-string-in-out-connector)
				  &allow-other-keys
				  &aux (*xmp-warning-leader* "SOAP"))
  (apply 'make-instance class (remove-keys '(:class) options)))

(defmethod soap-add-header ((conn soap-connector) (header t)
			    &key parts after before reset)
  (soap-add-header conn (apply #'make-soap-header header parts)
		   :after after :before before :reset reset))

(defmethod soap-add-header ((conn soap-connector) (header soap-header)
			    &key parts after before reset)
  (when parts 
    (error "~A~A"
	   "soap-add-header :parts argument makes sense only "
	   "if header argument is a type or typespec"))
  (let ((all (soap-headers conn)))
    (when reset
      (setf (soap-headers conn) (setf all nil)))
    (cond ((null all)
	   (setf (soap-headers conn) (setf all (list header))))
	  (t
	   (do ((tail all (cdr tail)))
	       ((atom tail))
	     (when (cond ((null (cdr tail))
			  ;; 2006-10-24 mm rev: add new header at the end 
			  ;;   if before or after is not found
			  t)
			 (after   (eq (car tail) after))
			 (before  (cond ((eq (car tail) before)
					 (setf (soap-headers conn) 
					       (setf all (cons header all)))
					 (return))
					((eq (cadr tail) before) t))))
	       (setf (cdr tail) (cons header (cdr tail)))
	       (return)))))
    header))
      

(defmethod xmp-object-class  ((conn soap-connector) (data t) (type t)
			      &rest options 
			      &key (class 'soap-element) &allow-other-keys)
  (declare (ignore options))
  class)



   

(defmethod xmp-object-class  ((conn soap-connector) (data t) (type (eql (env "Header")))
			      &rest options 
			      &key (class 'soap-header) &allow-other-keys)
  (declare (ignore options))
  class)

(defmethod make-soap-header ((conn soap-connector) def &rest args)

  ;; code is modified copy of xmp-encode-object

  (let ((tc (xmp-copy conn))
	(data (make-instance 'soap-header))
	)
    (setf (xmp-message-string tc) nil)
    (xmp-message-begin tc)
    (soap-encode-element tc (xmp-normal-element-spec conn def :out) args)
    (setf (xmp-element-type data) (env "Header")
	  (xmp-element-tag1 data) (env "Header"))
    (setf (xmp-element-content data) (xmp-message-string tc))
    data))


(defmethod soap-find-type ((conn t) name nss)
  (let* ((def-name (xmp-decode-qualified-name conn name nss))
	 (def (xmp-find-type conn def-name nss)))
    (values def def-name)))

(defmethod soap-find-element ((conn t) name nss)
  (typecase name
    (cons (case (first name)
	    (:element (values (third name) (xmp-pick-name conn name)))))
    ((or string symbol)
     (let* ((def-name (xmp-decode-qualified-name conn name nss))
	    (def (xmp-find-element conn def-name nss)))
       (values def def-name)))))

(defmethod soap-resolve-type ((conn t) type nss &aux name def)
  ;; returns nil  or
  ;; 3 values: type-def, outermost name of def, innermost name
  (loop
   (setf def (if (consp type)
		 type
	       (when type
		 (or name (setf name type))
		 (soap-find-type conn type nss))))
   (cond ((null def) (return nil))
	 ((consp def)  (or (ecase (first def)
			     (:simple (when (second def)
					(setf type (second def))
					(or name (setf name type))))
			     (:array nil)
			     (:complex nil))
			   (return (values def name type))))
	 (t (setf type def)  (or name (setf name type)))
	 )))

(defmethod soap-known-type-p ((conn t) type nss &rest packages)
  ;; nss -> :top    -- type must be name of globally defined type
  ;;        :outer  -- type may be type-spec, outermost named type is known
  ;;        nss     -- type may be type-spec, innermost named type is known
  (flet ((known-p (sym)
		  (and sym (symbolp sym)
		       (if packages
			   (dolist (p packages nil)
			     (typecase p
			       (package nil)
			       ((or string symbol) (setf p (find-package p))))
			     (when (eq (symbol-package sym) p)
			       (return t)))
			 (eq (symbol-package sym) (find-package :net.xmp.schema)))
		       sym)))
    (case nss
      (:top (and (known-p type) (soap-find-type conn type :dns) type))
      (otherwise
       (multiple-value-bind (def outer inner)
	   (soap-resolve-type conn type (case nss (:outer :dns) (otherwise nss)))
	 (declare (ignore def))
	 (case nss
	   (:outer (known-p outer))
	   (otherwise (known-p inner))))))))

(defmethod soap-array-elements ((conn t) type-def)
  (if type-def
      `(:seq* (:element nil ,(second type-def)))
    (xmp-any-cpart conn)))


(defmethod define-soap-type ((conn t) name type-def &rest options
			     &aux (*xmp-warning-leader* "SOAP"))
  (apply 'define-xmp-type conn name type-def options))

(defmethod define-soap-element ((conn t) elt-name-spec type-spec &rest options
				&aux (*xmp-warning-leader* "SOAP"))
  (apply 'define-xmp-element conn elt-name-spec type-spec options))


(defvar *soap-server* nil)
(defmethod soap-encode-object ((conn null) name type data
			       &aux (*xmp-warning-leader* "SOAP"))
  (if *soap-server*
      (soap-encode-object *soap-server*  name type data)
    (xmp-error nil :client "Cannot encode an object without a server context.")))

(defmethod soap-encode-object ((conn soap-connector) name type data)

  ;; code is modified copy of xmp-encode-object

  (let ((tc (xmp-copy conn))
	(new (make-instance 'soap-element :name name :type type :lisp-value data))
	)
    (setf (xmp-message-string tc) nil)
    (xmp-message-begin tc)
    (soap-encode-element tc (xmp-normal-element-spec conn name :out) data :type type)
     (setf (xmp-element-content new) (xmp-message-string tc))
    new))

(defmethod soap-encode-parts-to-object ((conn soap-connector) type data)

  ;; code is modified copy of soap-encode-object

  (let ((tc (xmp-copy conn))
	(new (make-instance 'soap-fragment :type type :lisp-value data))
	)
    (setf (xmp-message-string tc) nil)
    (xmp-message-begin tc)
    (soap-encode-parts tc data nil type)
     (setf (xmp-element-content new) (xmp-message-string tc))
    new))

(defmethod soap-encoded-array-type ((conn xmp-connector) type nss
				   &key rank dimensions parts
				   &aux open close)
  ;; type is element type of array
  (cond ((null type) (error "Array type cannot be nil."))
	((and (stringp type)
	      (setf open (position #\[ type))
	      (setf close (position #\] type)))
	 (cond ((not (eql close (1- (length type))))
		(error "Ill-formed :send-atype option ~S" type))
	       ((eql (1+ open) close) (setf type (subseq type 0 open)))
	       ((or rank dimensions)
		(error
		 ":send-atype specified with dimensions, but :send-asize present."))
	       (t (return-from soap-encoded-array-type type))))
	)
  (concatenate
   'string
   (xmp-encoded-qualified-name conn type nss :suppress-default t)
   "["
   (cond ((numberp rank) (let ((s ""))
			   (dotimes (i (1- (truncate rank)) s)
			     (setf s (concatenate 'string s ",")))))
	 ((numberp dimensions) (format nil "~A" (truncate dimensions)))
	 ((consp dimensions) (format
			      nil "~A~{, ~A~}" (first dimensions) (cdr dimensions)))
	 (dimensions (format nil "~A" (length parts)))
	 (t ""))
   "]"))


(defmethod soap-encode-element ((conn soap-connector)
				element parts
				&key type name
				&aux dname type-def st std option-def type-name
				send-atype type-res empty nilled)
  (etypecase element
    (cons (ecase (first element) (:element nil))
	  (when type (error "type argument not allowed"))
	  (setf type-def (third element)
		option-def element
		name (or name (xmp-pick-name conn element))))
    ((or string symbol)
     ;; always do this step - even if type is specified -
     ;;  in order to compute dname correctly
     (multiple-value-setq (type-def dname)
       (soap-find-element conn element :out))
     (or name (setf name dname))
     (cond (type (setf type-def type option-def type-def))
	   (type-def (setf option-def element))
	   (t    (setf option-def type-def)))))
  (or type-def (xmp-error conn :def :string (list "Cannot find ~A" element)))
  (or name (xmp-error conn :def :string
		      (list "Cannot encode un-named element ~A" element)))
  (setf type-res type-def)
  (setf send-atype (xmp-getf-in-def conn option-def :send-atype))
  (typecase type-def
    ((or string symbol) (setf type-name type-def
			      type-res (soap-resolve-type conn type-def :out)))
    (cons (when (eq :simple (first type-def)) (setf type-name (second type-def))))
    (otherwise nil))
  (when (null parts)
    (case (soap-null-element conn)
      (:empty (setf empty t))
      (:none (return-from soap-encode-element (values name type-def)))
      (:nilled (setf empty t
		     nilled (list  (intern "nil" :net.xmp.schema-instance) "true")))
      ((:nilled-or-default :nilled-or-empty :nilled-or-none)
       (cond ((xmp-getf-in-def conn option-def :nillable)
	      (setf empty t
		    nilled (list  (intern "nil" :net.xmp.schema-instance) "true")))
	     (t (case (soap-null-element conn)
		  (:nilled-or-empty (setf empty t))
		  (:nilled-or-none
		   (return-from soap-encode-element (values name type-def)))
		  (otherwise nil)))))
      (otherwise nil)))
  (xmp-encode-begin conn name
		    :namespaces (xmp-getf-in-def conn option-def :namespaces)
		    :empty empty
		    :attributes 
		    (merge-plists 
		     nilled
		     (when (setf st (xmp-getf-in-def conn option-def :encoding))
		       ;; env:encodingStyle can appear in any element
		       (list (env "encodingStyle") st))
		     (when (and (setf st (xmp-getf-in-def
					  conn option-def :send-type -1))
				(if (eql st -1)
				    (setf st (soap-send-type conn))
				  st)
				(if send-atype
				    (setf st (enc "Array"))
				  (typecase st
				    ((member t) (setf st type-name))
				    ((or string symbol) st)
				    (otherwise
				     (error ":send-type option must be a name.")))))
		       (list (xsi "type") st))
		     (when send-atype
		       (list (enc "arrayType")
			     (soap-encoded-array-type
			      conn
			      (case send-atype
				((t) 
				 ;; type-def must be (:array ...)
				 (and (consp type-res) (eq :array (first type-res))
				      (second type-res)))
				(otherwise send-atype))
			      :out
			      :dimensions (xmp-getf-in-def conn option-def :send-asize)
			      :parts parts)))
		     (when (and (setf st (xmp-getf-in-def
					  conn option-def :must-understand
					  (setf std (list nil))))
				(not (eq st std)))
		       (list (env "mustUnderstand") (if st "1" "0")))
		     (let ((a (xmp-getf-in-def conn option-def :computed-attributes)))
		       (when a (funcall a conn element)))
		     (xmp-getf-in-def conn option-def :attributes)))
  (when (not empty)
    (soap-encode-parts conn parts name type-def)
    (xmp-encode-end conn name))
  (values name type-def))


(defun merge-plists (plist1 plist2 &rest more)
  (if more
      (merge-plists plist1 (apply 'merge-plists plist2 more))
    (do ((ptl plist1 (cddr ptl)) copied (none '#:none))
	((atom ptl) plist2)
      (if (eq none (getf plist2 (car ptl) none))
	  (setf plist2 (list* (first ptl) (second ptl)
			      (if copied
				  plist2
				(append plist2 nil))))
	(setf (getf plist2 (car ptl)) (second ptl))))))

(defmethod soap-encode-parts ((conn soap-connector)
			      parts name type-def
			      &aux
			      type-class cdef type-res res item-type
			      aparts elt pattern attr send pname ptype pattr parg)
  ;; return type of encoded part
  (setq type-res (soap-resolve-type conn type-def :out))
  (cond ((consp type-res)
	 (setf type-class (first type-res)))
	(t (setf type-class :simple type-res type-def)
	   (setf type-def nil)))
  (ecase type-class
    (:simple
     (if (consp parts)
	 (dolist (part parts res)
	   (setf res (xmp-encode conn part type-res)))
       (xmp-encode conn parts type-res)))
    (:array
     (setf aparts (xmp-getf-in-part conn type-res :array-item))
     (setf item-type (second type-res))
     (setf elt (or (getf aparts :element)
		   (or (and (symbolp item-type) item-type) 
		       (and (symbolp name) name)
		       "item")))
     (setf pattern (getf aparts :argument))
     (setf attr (getf aparts :attributes))
     (setf send (getf aparts :send-type))
     (dolist (part parts res)
       (setf pname elt ptype nil pattr nil)
       (or
	(typecase part
	  (xmp-element (setf res (xmp-encode conn part nil)) t)
	  (cons
	   (case pattern
	     ((nil :arg-only) (setf parg part))
	     (otherwise 
	      (setf parg (second part))
	      (ecase pattern
		(:type-and-arg
		 (setf ptype (first part)))
		(:attributes-and-arg
		 (setf pattr (first part)))
		(:element-and-arg 
		 (when (first part) (setf pname (first part))))
		)))
	   nil)
	  (otherwise (setf parg part) nil)
	  )

	(cond
	 ((and (or (null pname) (symbolp pname) (stringp pname))
	       (null pattr) (null send))
	  (setf res (soap-encode-element conn pname parg
					 :type (or ptype item-type))))
	 ((and (consp pname) (third pname) (null ptype) (null pattr) (null send))
	  (setf res (soap-encode-element conn pname parg)))
	 (t
	  (etypecase pname
	    (cons (setf pname (append pname nil)))
	    ((member nil) (setf pname (list :element nil nil)))
	    ((or string symbol) (setf pname (list :element pname nil))))
	  (or (second pname)
	      (setf (second pname) (if (symbolp item-type) item-type "item")))
	  (or (third pname)
	      (setf (third pname) (or ptype item-type)))
	  (when (or attr pattr send)
	    (let* ((pt (third pname))
		   (ptl (if (consp pt)
			    pt
			  (setf (third pname)
				(list :simple (third pname)))))
		   (pa (getf (cddr ptl) :attributes)))
	      (when (or attr pattr)
		(setf (getf (cddr ptl) :attributes)
		      (merge-plists pattr attr pa)))
	      (when (and send (typecase (setf ptype (or ptype item-type))
				((or string symbol) ptype)))
		(setf (getf (cddr ptl) :send-type) ptype))))
	  (setf res (soap-encode-element conn pname parg))
	  )))))
    (:complex
     ;; preserve the order of elements in the definition
     ;;  but allow arguments in any order
     ;; undefined elements in input will be ignored
     ;; elements must be unique
     (setf cdef (second type-res))
     (labels ((propagate (name arg key defs opts)
			 (multiple-value-bind (n2 a2)
			     (encode key defs opts)
			   (cond ((and n2 a2) (values n2 a2))
				 ((and name arg) (values name arg))
				 (n2 (values n2 nil))
				 (t (values name arg)))))	      
	      (encode (key defs opts &aux name arg (default '#:none) prop)
		      ;;  key      defs
		      ;;  nil      (:any) | (:element ...) | complex-def
		      ;; collector complex-def-tail
		      ;; :element  name | 
		      (ecase key
			;; return two values:
			;;    the name of an element or an element-def
			;;    t if the element was actually encoded
			((nil) (etypecase defs
				 (cons
				  (case (first defs)
				    (:any (encode :any nil opts))
				    (:element (encode :element defs opts))
				    (otherwise
				     (encode (first defs) (cdr defs) opts))))
				 ((member nil) nil)
				 (string (encode :element defs opts))
				 (symbol (encode :element defs opts))))
			(:or (dolist (def defs)
			       (multiple-value-setq (name arg)
				 (encode nil def opts))
			       (when arg (return (values name t)))))
			((:seq :set)
			 (when defs
			   (multiple-value-setq (name arg)
			     (encode nil (first defs) opts))
			   (if (cdr defs)
			       (propagate name arg key (cdr defs) opts)
			     (values name arg))))
			((:seq1 :set1)
			 (when defs
			   (multiple-value-setq (name arg)
			     (encode nil (first defs) opts))
			   (or arg (soap-encode-element conn (first defs) nil))
			   (if (cdr defs)
			       (propagate name arg key (cdr defs) opts)
			     (values name t))))
			((:seq+ :set+)
			 (when defs
			   (multiple-value-setq (name arg)
			     (encode nil (first defs) opts))
			   (or arg (soap-encode-element conn (first defs) nil))
			   (loop
			    (multiple-value-setq (name arg)
			      (encode nil (first defs) opts))
			    (or arg (return)))
			   (if (cdr defs)
			       (propagate name arg key (cdr defs) opts)
			     (values name t))))
			((:seq* :set*)
			 (when defs
			   (loop
			    (multiple-value-bind (n2 a2)
			      (encode nil (first defs) opts)
			      (when (null a2)
				(return))
			      (setf name n2 arg a2)))
			   (if (cdr defs)
			       (propagate name arg key (cdr defs) opts)
			    (values name arg))))
			(:any
			 (when parts 
			   (setf arg t)
			   (typecase parts
			     (xmp-element (xmp-encode conn parts nil)
					  (setf parts nil))
			     (atom  ;;; [bug15783]
			      (xmp-encode conn parts (enc "string"))
			      (setf parts nil))
			     (otherwise
			      ;; a list may be an alternating list of
			      ;; (elt-name content elt-name...)
			      (multiple-value-bind (type-def name)
				  (soap-find-element conn (first parts) :out)
				(cond
				 ((and name type-def)
				  (soap-encode-element
				   conn name (second parts) :name name)
				  (setf parts (cddr parts)))
				 (name
				  (soap-encode-element
				   conn name (second parts) :name name
				   :type (xsd "string"))
				  (setf parts (cddr parts)))
				 (t (error
				     "Cannot encode a random list as xsd:anyType ~S"
				     parts))))))
			   )
			 (values (or name :any) arg))
			(:element
			 (cond
			  ((and (consp defs) (null (second defs)))
			   ;; This is an un-named element def,
			   ;; take the first argument name and value.
			   (when parts 
			     (setf arg t)
			     (setf name
				   (soap-encode-element
				    conn (first parts) (second parts)
				    :name (first parts)
				    :type (when (consp defs) (third defs))
				    ))
			     (setf parts (cddr parts)))
			   (values name arg))
			  (t 
				
			   ;;???
			   ;; if element is named "num"
			   ;; and "num" is defined as (:or "num1" "num2")
			   ;; then we should look for "num1" or "num2" in the
			   ;; argument list - not "num"
			   ;; bug20672 was looking for (:complex (:or ...))
			   ;;   BUT (:element foo (:complex (:or ...))) denotes a nested element structure
			   ;;   WHILE (:element foo (:or ...)) denotes alternate element names???
			   ;;   BUT BUT (:element foo (:or ...)) is not well formed!
				
			   (let* ((edef defs)
				  (type (if (consp edef)
					    (soap-resolve-type
					     conn (third edef) :out)
					  (soap-resolve-type
					   conn 
					   (or (soap-find-element conn edef :out)
					       (error
						"Encoding undefined element ~S"
						edef))
					   :out)))
				  ctail)
			     (if (and (consp (setf ctail type))
				      (eq :or (pop ctail))
				      (or (atom (first ctail))
					  (eq :element (first (first ctail))))
				      )
				 (encode :or ctail opts)
			       (progn
				 (typecase edef
				   (cons (setf name (xmp-pick-name conn edef)))
				   (otherwise (setf name edef)))
				 (setf arg default)
				 (when (setf prop (xmp-elt-getf-name conn parts edef))
				   (setf name prop)
				   (setf arg (getf parts prop default))
				   (when (not (eq arg default))
				     (soap-encode-element
				      conn edef arg :name prop)
				     (drop prop)))
				 (values name (when name (not (eq arg default))))))))))
			))
	      (drop (name)
		    (if (eq name (first parts))
			(setf parts (cddr parts))
		      (do ((tl parts (cddr tl)))
			  ((atom (cddr tl)))
			(when (eq name (third tl))
			  (setf (cddr tl) (cddddr tl))
			  (return)))))			
	      )
       (encode (first cdef) (cdr cdef) (cddr type-res)))



     )))


	  
(defmethod soap-xml-leader ((conn soap-connector))
  (format nil
	  "<?~A~A?>" 
	  (xmp-destination-leader conn)
	  (if (xmp-xml-encoding conn)
	      (format nil " encoding=\"~A\""
		      (etypecase (xmp-xml-encoding conn)
			(cons (second (xmp-xml-encoding conn)))
			(symbol (xmp-xml-encoding conn))))
	    "")))


(defmethod soap-encode-headers ((conn soap-connector))
  ;; 2006-10-24 mm rev: [bug16470] Combine header list into a single Header element
  (let ((hs (soap-headers conn)))
    (cond ((null  hs) nil)
	  ((null (cdr hs)) (xmp-encode conn (car hs) nil))
	  (t (let ((new (make-instance 'soap-header))
		   (content ""))
	       (dolist (h hs)
		 (setf content (concatenate 'string content
					    (xmp-element-content h))))
	       (setf (xmp-element-type new) (env "Header")
		     (xmp-element-tag1 new) (env "Header")
		     (xmp-element-content new) content)
	       (xmp-encode conn new nil))))))

(defmethod soap-encode-message ((conn soap-connector) method args &aux def)
  (xmp-message-begin conn)
  (xmp-encode-content conn (soap-xml-leader conn)
		      :sanitize nil)
  (xmp-encode-begin conn (env "Envelope") :dns t
		    :attributes 
		    (when (soap-encoding-style conn)
		      (list (env "encodingStyle") (soap-encoding-style conn)))
		    )
  (soap-encode-headers conn) ;;; 2006-10-24 mm rev: [bug16470] Combine header list
  (xmp-encode-begin conn (env "Body"))
  (cond 
    ((typep method 'xmp-element)
     (xmp-encode conn method nil)
     (setf def (xmp-element-type method)
	   method (xmp-element-name method)
	   ))
    ((or (and (or (symbolp method) (stringp method))
	      (soap-find-element conn method :dns))
	 (and (consp method) (eq :element (first method))))
     (multiple-value-setq (method def)
       (soap-encode-element
	conn (xmp-normal-element-spec conn method :out) args)))
    ;; Look for a type def last
    ;; (in case an element and a type have the same name)
    ;; A type def may be used to specify a sequence of body elements.
    ((soap-type-spec-p method)    
     (soap-encode-parts conn args nil method)
     (setf def (soap-resolve-type conn method :out)))
    (t (error "Ill-formed method spec ~S" method)))
  (xmp-encode-end conn (env "Body"))
  (xmp-encode-end conn (env "Envelope"))
  (values method def (xmp-message-string conn)))

(defun soap-type-spec-p (s)
  (typecase s
    (cons (case (first s) ((:simple :complex :array) t)))
    ((or string symbol) (soap-find-type nil s :dns))))

(defmethod call-soap-method ((conn soap-client-connector)
			    method &rest args
			    &aux (*xmp-warning-leader* "SOAP"))

  ;; method -> element-name | element-def
  ;;           args -> [sub-element-name value]...
  ;; method -> type-spec
  ;;           args -> [element-name value]...

  ;; Always returns two values: body-or-body-list header-list
  (let ((res (apply 'xmp-call-method conn method args)))
    (typecase res
      (cons (case (first res)
	      (:envelope
	       (let* ((blist (cdr (assoc :body (cdr res))))
		      (hlist (cdr (assoc :headers (cdr res))))
		      (body (first blist)))
		 (setf (soap-message-body conn) blist
		       (soap-message-headers conn) hlist)		 
		 
		 ;; Update content of arrays now
		 (soap-update-arrays conn)

		 (if (and (consp body)
			  (not (eq :fault (first body)))
			  )
		     (ecase (soap-body-form conn)
		       (:one (values body hlist))
		       (:many (values blist hlist)))
		   (let* ((code (soap-result-string
				 conn res  nil :body :fault "faultcode"))
			  (string (soap-result-string
				   conn res nil :body :fault "faultstring"))
			  (actor  (soap-result-string
				   conn res nil :body :fault "faultactor"))
			  ec esub)
		     (typecase code
		       (null (setf ec :client esub nil))
		       ((or string symbol) (let* ((str (string code))
						  (dot (position #\. str))
						  (hd (if dot
							  (subseq str 0 dot)
							str))
						  (tl (if dot
							  (subseq str (1+ dot))
							nil)))
					     (cond ((equal hd "Client") (setf ec :client
									      esub tl))
						   ((equal hd "Server") (setf ec :server
									      esub tl))
						   (t (setf ec (if (symbolp code)
								   (intern
								    hd
								    (symbol-package 
								     code))
								 hd)
							    esub tl)))))
		       (otherwise (setf ec :client esub code)))
		     (xmp-error conn 
				(case ec
				  (:server 'soap-server-fault)
				  (otherwise 'soap-client-fault))
				:sub-code esub
				:string string
				:factor actor
				:detail res)
		     ))))
	      (otherwise (values res nil))))
      (otherwise (values res nil)))))

(defmethod soap-debug-p ((conn soap-client-connector) &key var)
  (declare (ignore var))
  (call-next-method conn :var '*soap-client-debug*))
(defmethod soap-debug-p ((conn soap-server-connector) &key var)
  (declare (ignore var))
  (call-next-method conn :var '*soap-server-debug*))
(defmethod soap-debug-p ((conn soap-connector) &key var &aux v bp)
  (multiple-value-setq (v bp)
    (mp:symeval-in-process var mp:*current-process*))
  (if (case bp
	(:unbound nil)
	((nil) nil)
	(otherwise t))
      v
    (or (soap-debug conn) (symbol-value var))))



(defun soap-print-xml (xml)
  (multiple-value-bind (v e)
      (ignore-errors
	(progn (net.xml.dom:dom-print
		(net.xml.dom:parse-to-dom xml) t)
	       (format t " ~% ~%")
	       t))
    (if v t (format t "~&DOM error: ~A~%Raw string:~%~A ~% ~%" e xml)))
  )

(defun xmp-call-method-debug (conn where data)
  (ecase where
    (:before
     (format t "~&Calling SOAP method:~%")
     (format t "~&   destination URL: ~A~%" (xmp-destination-url conn))
     (format t "~&       HTTP method: ~A~%" (net.xmp::xmp-destination-method conn))
     (format t "~&     HTTP protocol: ~A~%"
	     (net.xmp::xmp-destination-http-protocol conn))
     (format t "~&      content-type: ~A~%"
	     (net.xmp::xmp-destination-content-type conn))
     (format t "~&        user agent: ~A~%" (net.xmp::xmp-destination-agent conn))
     (format t "~& SOAPAction header: ~A~%" data)
     (format t "~&       Host header: ~A~%" (net.xmp::xmp-destination-host conn))
     (format t "~&   external format: ~S~%" (net.xmp::xmp-external-format conn))
     (format t "~&additional headers: ~S~%" (xmp-more-headers conn))
     (format t "~&   additional args: ~S~%" (xmp-client-start conn))
     (format t "~&      SOAP message:~%~A~%" (soap-xml-leader conn))
     (soap-print-xml (xmp-message-string conn)))
    (:after
     (format t "~& SOAP reply:~%")
     (soap-print-xml data))))

(defun xmp-peek-at-reply (reply)
  ;; Before parsing check if it looks like XML and SOAP Envelope
  ;;  do not require xmldecl  [bug17614]
  (multiple-value-bind (r se)
      (match-re "^ *<[?]xml [^>]*[?]>\\s*<" reply :case-fold t :return :index)
    (setf se (if r (1- (cdr se)) 0))
    (match-re "^\\s*<[^<>: ]*:?Envelope " reply :case-fold t :return nil :start se)))

(defmethod xmp-call-method ((conn soap-client-connector)
			    method &rest args)
  (multiple-value-bind (name def)
      (soap-encode-message conn method args)
    (declare (ignore name))
    (xmp-decode-message 
     conn
     (let* ((action (xmp-getf-in-def conn def :action))
	    reply parsed)
       (when (soap-debug-p conn)
	 (xmp-call-method-debug conn :before action))
	    
       (case (soap-debug-p conn)
	 (:stop
	  ;; [bug16004] skip the call to xmp-decode-message in this case
	  (return-from xmp-call-method nil))
	 (otherwise
	  (setf reply (xmp-message-send
		       conn
		       :headers 
		       (when action
			 ;; The soapAction value stored in Lisp defs must be an undecorated
			 ;; string to allow various equal tests to pass but the actual
			 ;; HTTP header value needs the quotes inserted in the header
			 ;; that goes out on the wire.   [rfe8861]
			 (setf action (format nil "~A" action))
			 (when (or (equal "" action) ;; this one needs quotes
				   ;; this one does not (caller must be using early patch)
				   (not (eql #\" (elt action 0))) 
				   )
			   (setf action (format nil "\"~A\"" action)))
			 `((,(xmp-symbol "SOAPAction" :keyword) . ,action)))
		       ))
	  (when (soap-debug-p conn)
	    (xmp-call-method-debug conn :after reply))

	  (if (xmp-peek-at-reply reply)
	      (setf parsed (xmp-parse-message
			    conn reply
			    :namespaces 
			    (xmp-decode-namespaces
			     :pns (xmp-extract-namespaces :string reply)
			     :known (list
				     nil
				     (net.xmp::xmp-normal-nse
				      (xmp-getf-in-part conn def :namespaces))
				     (net.xmp::xmp-normal-nse (xmp-message-dns conn))
				     (net.xmp::xmp-normal-nse (xmp-base-dns conn))
				     :all))
			    ))
	    (soap-client-error conn :sub-code :reply
			       :string "Reply is not a SOAP XML document." :detail reply)
	    )))
       
       parsed))))


(defmethod xmp-begin-message ((conn soap-string-in-connector))
  (setf (soap-undef-list conn) nil
	(soap-message-body conn) nil
	(soap-message-headers conn) nil
	(soap-message-arrays conn) nil
	)
  (list :seq1 (env "Envelope")))

(defmethod xmp-end-message ((conn soap-string-in-connector) data
			    &key types &allow-other-keys)
  (cond ((cdr data)
	 (xmp-error conn :client :string "Too many elements in message"))
	(data (values (car data) (car types)))
	(t (values))))


(defun soap-array-size (x start end)
  (let* ((p (position #\, x :start start :end end)))
    (if p
	(list* (or (ignore-errors (parse-integer x :start start :end p))
		   0)
	       (soap-array-size x (1+ p) end))
      (list (or (ignore-errors (parse-integer x :start start :end end))
		0)))))

(defmethod soap-parse-array-type (conn s nss)

  ;; values:  element-type-name rank dimensions

  (let* (x
	 (pk (typecase s
	       (symbol (setf x (symbol-name s))
		       (symbol-package s))
	       (otherwise (setf x s) nil)))
	 (b0 (position #\[ x))
	 (b1 (when b0 (position #\] x :start b0)))
	 (b2 (when b1 (position #\[ x :start b1)))
	 (b3 (when b2 (position #\] x :start b2)))
	 (name (when (and b0 (< 0 b0))
		 (if pk
		     (intern (subseq x 0 b0) pk)
		   (xmp-decode-qualified-name
		    conn (subseq x 0 b0) nss :suppress-default t))))
	 )
    (cond ((and b0 b1 (null b2))
	   (values name nil (soap-array-size x (1+ b0) b1)))
	  ((and b0 b1 b2 b3)
	   (values name
		   (1+ (count #\, x :start b0 :end b1))
		   (soap-array-size x (1+ b2) b3)))
	  (t (soap-client-error conn :string "Ill-formed arrayType")))))

(defmethod soap-decode-type ((conn soap-string-in-connector) attributes nss
			     &aux type atype res x rank length)
  ;; Result is 3 values:
  ;;   Type          (as declared in a xsi:type attribute
  ;;   arrayType     derived from Type or declared
  ;;   res           type-def of Type or arrayType

  (and (setf type (getf attributes (xsi "type")))
       (setf type (xmp-decode-qualified-name conn type nss :suppress-default t))
       (setf res (soap-resolve-type conn type nss)))
  (when (setf x (getf attributes (enc "arrayType")))
    (multiple-value-setq (atype rank length) (soap-parse-array-type conn x nss))
    (setf res (list :array atype :rank rank :length length))
    )
  (when (and atype (setf x (getf attributes (enc "offset"))))
    (multiple-value-setq (x x x)
      (soap-parse-array-type conn x nss))
    (when (and x (not (member nil x)))
      (setf res (append (list (first res) (second res)
			      :offset x)
			(cddr res)))))    
  (values type atype res))


(defmethod xmp-warning-leader ((conn soap-connector)) "SOAP")

(define-condition soap-decode-warning (xmp-warning) ())
(defmethod soap-decode-note ((conn t) bool fmt &rest args)
  (or conn (setf conn "SOAP"))
  (or bool
      (case (soap-decode-flag conn)
	(:strict (soap-client-error conn :string 
				    (if args (list* fmt args) fmt)))
	(:warn (apply 'xmp-class-warning conn 'soap-decode-warning fmt args)))))



(defmethod soap-match-types ((conn soap-connector) t1 t2)
  (or (null t1)
      (null t2)
      (equal t1 t2)
      (xmp-any-cpart conn t1)
      (xmp-any-cpart conn t2)
      (eq t1 (xsd "ur-type"))
      (eq t2 (xsd "ur-type"))
      (eq t1 (xsd "anyType"))
      (eq t2 (xsd "anyType"))
      (and (or (and (consp t1) (eq :simple (first t1)))
	       (and (consp t2) (eq :simple (first t2))))
	   (soap-match-types conn 
			     (if (consp t1) (second t1) t1)
			     (if (consp t2) (second t2) t2)))
      (and (consp t1) (consp t2) (equal (first t1) (first t2))
	   (case (first t1)
	     (:element (and (equal (second t1) (second t2))
			    (soap-match-types conn (third t1) (third t2))
			    (equal (cdddr t1) (cdddr t2))))
	     (otherwise (and (soap-match-types conn (second t1) (second t2))
			   (equal (cddr t1) (cddr t2))))))
      ))

(defmethod xmp-begin-element ((conn soap-string-in-connector) (elt t)
			      &rest options &key attributes &allow-other-keys
			      &aux exel dt defined-p
			      derived-exel derived-type effective-type skip-undef)
  (multiple-value-setq (exel dt defined-p) (xmp-defined-element-def conn elt :in 0))
  (setf derived-type dt)
  (and exel defined-p (setf effective-type dt))
  (let (type atype res)
    (multiple-value-setq (type atype res) (soap-decode-type conn attributes :in))
    (cond (atype (setf derived-type (list :array atype)
		       derived-exel (soap-array-elements conn res)))
	  (type (setf derived-type type)
		(when res
		  (setf effective-type type)
		  (setf derived-exel
			(ecase (first res)
			  (:simple (xmp-simple-exel conn res))
			  (:array (soap-array-elements conn res))
			  (:complex (second res))))))
	  (t (setf derived-exel exel))))

  ;; SOAP 1.1:
  ;; The name of the return value accessor is not significant.
  ;; Likewise, the name of the struct is not significant. 
  ;; However, a convention is to name it after the method name 
  ;;    with the string "Response" appended.
  (cond ((eq (env "Body") (first (second (xmp-inside-elements conn))))
	 ;; This is the outermost body element, ie the "Response" struct
	 (setf (soap-outer-element conn) (xmp-inside-elements conn)
	       skip-undef t))
	((eq  (soap-outer-element conn) (cdr (xmp-inside-elements conn)))
	 ;; This is the first element in the "Response" struct,
	 ;;  ie the return value accessor.
	 (setf (soap-outer-element conn) nil
	       skip-undef t)))

  (cond (effective-type)
	(skip-undef)
	((null exel)) ;;; If in :any context, accept anything ???
	((member elt (soap-undef-list conn)))
	(t (push elt (soap-undef-list conn))
	   (soap-decode-note conn defined-p "Undefined element ~S" elt)))
  
  (cond (derived-exel
	 (when (null (soap-match-types conn exel derived-exel))
	   (soap-decode-note conn nil "Type mismatch in element ~S:" elt)
	   (soap-decode-note conn nil "     expected ~S" exel)
	   (soap-decode-note conn nil "        found ~S." derived-exel)
	   ))
	((null exel))
	(derived-type 
	 (cond ((member derived-type (soap-undef-list conn)))
	       (t (push derived-type (soap-undef-list conn))
		  (soap-decode-note conn nil
				    "Unknown derived type ~S in ~S."
				    derived-type elt))))
	(t (soap-decode-note conn nil "Cannot determine type in ~S." elt)))
  (or derived-exel (call-next-method)))

(defmethod xmp-begin-element :after ((conn soap-string-in-connector) elt
				     &rest options &key attributes &allow-other-keys
				     &aux (depth (xmp-in-depth conn)) 
				     actor this-actor
				     )
  (declare (ignore options))
  (when (eql 1 depth)
    (if (equal "Envelope" (string elt))
	(or (eq elt (env "Envelope"))
	    (xmp-error conn 'soap-mismatch))
      (soap-client-error conn :string "Ill-formed message.")))
  (when (and (eql 3 depth)
	     (or (equal "1" (getf attributes (env "mustUnderstand")))
		 (equal "true" (getf attributes (env "mustUnderstand"))))
	     (equal (env "Header") (first (second (xmp-inside-elements conn))))
	     (or (null (setf actor (getf attributes (env "actor"))))
		 (and (setf this-actor (soap-actor conn))
		      (same-uri actor this-actor))
		 (same-uri actor (soap-default-actor conn))))
    (soap-must-understand conn elt))
  )



(defmethod xmp-simple-content ((conn soap-connector) 
			       (elt (eql :any)) data
			       &rest options &key &allow-other-keys)
  (declare (ignore options))
  data)

(defmethod xmp-simple-content :around ((conn soap-connector) 
				       (elt t) (data null)
				       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (case (soap-empty-element conn)
    (:default-value (call-next-method))
    (otherwise nil)))



(eval-when (compile load eval) (defvar *soap-deftypes* nil))
(defmacro def-soap-simple-content (elt &key class keywords
				       (decode-class class) (decode-keys keywords)
				       decode
				       (encode-class class) (encode-keys keywords)
				       encode
				       check-type
				       &aux lastelt eltref)
  ;; If elt argument is a form, it must always be wrapped!
  (or class
      (and (if decode decode-class t) (if encode encode-class t))
      (error "def-soap-simple-content missing class"))
  (if (consp elt)
      (setf lastelt (first (last elt)))
    (setf lastelt elt))
  (when (symbolp lastelt) (setf lastelt (list 'quote lastelt)))
  `(progn
     ,@(when (consp elt)
	 (do ((tl elt (cdr tl)) res)
	     ((atom tl) (reverse res))
	   (setf elt (first tl))
	   (if (symbolp elt)
	       (setf eltref (list 'quote elt))
	     (setf eltref elt))
	   (when (cdr tl)
	     (when check-type
	       (push `(setf (get ,eltref :soap-check-type) ',check-type)
		     res))
	     (push `(define-soap-type nil ,eltref (list :simple ,lastelt))
		   res)
	     (push (first res) *soap-deftypes*)
	     )))
     ,@(when check-type
	 `((setf (get ,eltref :soap-check-type) ',check-type)))
     ,(let ((def `(define-soap-type
		    nil ,eltref (list :simple  nil :simple-content-key ,eltref))))
	(push def *soap-deftypes*)
	def)
     ,@(when decode

	 ;; These methods assume that the entire content will be presented
	 ;; in one string - xmp-decode-body collects fragments from the parser.

	 `((defmethod xmp-simple-content ((conn ,decode-class) (elt (eql ,eltref)) data
					  &rest options 
					  &key ,@decode-keys &allow-other-keys)
	     ,@decode)))
     ,@(when encode
	 `((defmethod xmp-encode ((conn ,encode-class) data (elt (eql ,eltref))
				  &rest options 
				  &key ,@encode-keys &allow-other-keys)
	     ,@encode)))))



(def-soap-simple-content ((enc "QName") (xsd "QName"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (xmp-decode-qualified-name conn data :in :suppress-default t))
  :encode ((declare (ignore options))
	   (xmp-encode-qualified-name conn data :out :suppress-default t))
  :check-type (or string symbol)
  )


(def-soap-simple-content ((enc "string")
			      (enc "duration") (enc "dateTime") (enc "time") (enc "date")
			      (enc "gYearMonth") (enc "gYear") (enc "gMonthDay")
			      (enc "gDay") (enc "gMonth")
			      (enc "anyURI") (enc "NOTATION")
			      (enc "token") (enc "language")
			      (enc "IDREFS") (enc "ENTITIES")
			      (enc "NMTOKEN") (enc "NMTOKENS")
			      (enc "Name") (enc "NCName") (enc "ID")
			      (enc "IDREF") (enc "ENTITY")
			      (enc "normalizedString")

			      (xsd "normalizedString")
			      (xsd "duration") (xsd "dateTime") (xsd "time") (xsd "date")
			      (xsd "gYearMonth") (xsd "gYear") (xsd "gMonthDay")
			      (xsd "gDay") (xsd "gMonth")
			      (xsd "anyURI") (xsd "NOTATION") (xsd "token") (xsd "language")
			      (xsd "Name") (xsd "NMTOKEN") (xsd "NCName") (xsd "NMTOKENS")
			      (xsd "ID")  (xsd "IDREF")  (xsd "ENTITY")   (xsd "IDREFS")
			      (xsd "ENTITIES")  

			      (xsd "string"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (or data ""))
  :encode ((declare (ignore options))
	   (xmp-encode-content conn
			       (typecase data
				 (string data)
				 (null   "")
				 (otherwise (format nil "~A" data))))
	   (xsd "string"))
  :check-type t
  )
  
#+ignore
(def-soap-simple-content ((enc "dateTime") (xsd "dateTime"))
  :class soap-connector
  ;; '-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  )
#+ignore
(def-soap-simple-content ((enc "duration") (xsd "duration"))
  :class soap-connector
  ;; [-]P[nY][nM][nD][T[nH][nM][n[.m]S]]
  )

(def-soap-simple-content ((enc "decimal") (xsd "decimal"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-decimal data :fraction t))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (typecase data
	     (null (xmp-encode-content conn "0"))
	     (integer (xmp-encode-content conn (format nil "~D" data)))
	     (float (xmp-encode conn data (xsd "double")))
	     (ratio (xmp-encode-content conn (format-ratio data)))
	     (otherwise (error "Cannot encode ~S as ~S" data (xsd "decimal"))))
	   (xsd "decimal"))
  :check-type number
  )

(defun format-ratio (data &optional (decimals 10))
  (let* ((num (numerator data))
	 (den (denominator data))
	 fract
	 (int (multiple-value-bind (i f)
		  (truncate num den)
		(setf fract f)
		i))
	 (ld (log den 10d0))
	 (ldi (truncate ld))
	 (up (- (if (< decimals ldi) ldi (- decimals ldi)) ld))
	 (fac (round (expt 10 up)))
	 d*
	 fdigits
	 f*)
    (loop (multiple-value-bind (f r)
	      (truncate fac 10)
	    (or (eql r 0)
		(return))
	    (setf fac f)))
    (setf d* (* den fac)
	  fdigits (truncate (log d* 10))
	  f* (* fract fac))
    (values
     (format nil "~D.~V,'0D" int fdigits f*)
     (list num den fract int ld ldi up fac d* fdigits f*))))

(def-soap-simple-content ((enc "long")  (xsd "long"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")
	     (let ((v (truncate data)))
	       (if (< -9223372036854775809 v 9223372036854775808)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as long"))))
	   (xsd "long"))
  :check-type number
  )

(def-soap-simple-content ((enc "unsignedLong")  (xsd "unsignedLong"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")
	     (let ((v (truncate data)))
	       (if (< -1 v 18446744073709551616)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as unsignedLong"))))
	   (xsd "unsignedLong"))
  :check-type number
  )

(def-soap-simple-content ((enc "int") (xsd "int"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")
	     (let ((v (truncate data)))
	       (if (< -2147483649 v 2147483648)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as int"))))
	   (xsd "int"))
  :check-type number
  )

(def-soap-simple-content ((enc "unsignedInt") (xsd "unsignedInt"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")
	     (let ((v (truncate data)))
	       (if (< -1 v 4294967296)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as unsignedInt"))))
	   (xsd "unsignedInt"))
  :check-type number
  )

(def-soap-simple-content ((enc "integer") (xsd "integer"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")
	     (let ((v (truncate data)))
	       (xmp-encode-content conn (format nil "~A" v))))
	   (xsd "int"))
  :check-type number
  )

(def-soap-simple-content ((enc "nonPositiveInteger") (xsd "nonPositiveInteger"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (<= v 0)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as nonPositiveInteger"))))
	   (xsd "nonPositiveInteger"))
  :check-type number
  )

(def-soap-simple-content ((enc "nonNegativeInteger") (xsd "nonNegativeInteger"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (<= 0 v)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as nonNegativeInteger"))))
	   (xsd "nonNegativeInteger"))
  :check-type number
  )

(def-soap-simple-content ((enc "negativeInteger") (xsd "negativeInteger"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when data (ignore-errors (parse-integer data)))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (< v 0)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as negativeInteger"))))
	   (xsd "negativeInteger"))
  :check-type number
  )

(def-soap-simple-content ((enc "positiveInteger") (xsd "positiveInteger"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when data (ignore-errors (parse-integer data)))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (< 0 v)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as positiveInteger"))))
	   (xsd "positiveInteger"))
  :check-type number
  )

(def-soap-simple-content ((enc "short") (xsd "short"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (if data (ignore-errors (parse-integer data)) 0)
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (< -32769 v 32768)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as short"))))
	   (xsd "short"))
  :check-type number
  )

(def-soap-simple-content ((enc "byte") (xsd "byte"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (if data (ignore-errors (parse-integer data)) 0)
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (< -129 v 128)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as byte"))))
	   (xsd "byte"))
  :check-type number
  )

(def-soap-simple-content ((enc "unsignedShort") (xsd "unsignedShort"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (if data (ignore-errors (parse-integer data)) 0)
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (< -1 v 65536)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as unsignedShort" v))))
	   (xsd "unsignedShort"))
  :check-type number
  )

(def-soap-simple-content ((enc "unsignedByte") (xsd "unsignedByte"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (if data (ignore-errors (parse-integer data)) 0)
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (if (null data)
	       (xmp-encode-content conn "0")  ;;; [bug16260]
	     (let ((v (truncate data)))
	       (if (< -1 v 256)
		   (xmp-encode-content conn (format nil "~A" v))
		 (error "Cannot encode ~A as unsignedByte"))))
	   (xsd "unsignedByte"))
  :check-type number
  )


(def-soap-simple-content ((enc "boolean") (xsd "boolean"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (let ((content (string-trim 
			   ;; bug17461 - Must collapse whitespace before decoding.
			   '(#\space #\tab #\newline #\return #\linefeed)
			   data)))
	     (cond ((equal content "0") nil)
		   ((equal content "false") nil)
		   ((equal content "1") t)
		   ((equal content "true") t)
		   (t (soap-client-error conn :string "Boolean value is not valid.")))))
  :encode ((declare (ignore options))
	   (xmp-encode-content conn (if data "true" "false"))
	   (xsd "boolean"))
  :check-type t
  )

(defun soap-decode-float (data)
  (if (null data)
      0.0d0
    (prog ((num (string-trim '(#\space #\tab #\newline #\return #\linefeed 
			       #\rubout #\page #\backspace)
			     data)) e v l)
	  (cond ((equal num "") (return 0d0))
		((or (setf e (position #\e num))
		     (setf e (position #\E num)))
		 (setf (elt num e) #\D))
		((or (position #\d num) (position #\D num)))
		(t (setf num (concatenate 'string num "d0"))))
	  (setf v (ignore-errors (multiple-value-setq (v l) (read-from-string num)) v))
	  (or (typep v 'double-float)
	      (error "Cannot decode ~S to an xsd:float or xsd:double value." data))
	  (when (< l (length num))
	    (error
	     "Decoding ~S  to an xsd:float or xsd:double value leaves residue at position ~A."
	     num l))
	  (return v))))

(def-soap-simple-content ((enc "float") (xsd "float"))
  :class soap-connector
  :decode ((declare (ignore options))
	   
	   ;; Parse the input as a double float to retain excess precision 
	   ;;  if it is there (some clients/servers ie soapware.org/validator1
	   ;;  seem to send and expect double precision for xsd:float )
	   (values (soap-decode-float data)
		   (xsd "float")))
  :encode ((declare (ignore options))
	   (let (f d)
	     (typecase data
	       (null (setf f 0.0 d 12))
	       (double-float (setf f data d 22))
	       (single-float (setf f data d 12))
	       (otherwise (setf f (coerce data 'single-float) d 12)))
	     (xmp-encode-content
	      conn
	      (if (or (eql f 0e1) (eql f 0d1))
		  ;; A true float zero causes an error in the 
		  ;; format used below.
		  (concatenate 'string "0.0")   ;;; bug13943
		(string-trim
		 " "
		 (format nil "~VF" 
			 (+ 
			  ;; This is conservatively the most significant digits
			  ;; in a single-float number, plus room for a leading or
			  ;; trailing zero.
			  d
			  ;; This is conservatively the most leading or trailing
			  ;; zeroes that will be printed.
			  (abs (truncate (log (abs f) 10))))
			 f))))
	     (xsd "float")))
  :check-type number
  )

(def-soap-simple-content ((enc "double") (xsd "double"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (soap-decode-float data)
		   (xsd "double")))
  :encode ((declare (ignore options))
	   (let ((f (coerce (or data 0) 'double-float)))
	     (xmp-encode-content
	      conn
	      (if (or (eql f 0e1) (eql f 0d1))
		  ;; A true float zero causes an error in the 
		  ;; format used below.   [bug21405]
		  (concatenate 'string "0.0")  
		(string-trim
		 " "
		 (format nil "~VF" 
			 (+ 
			  ;; This is conservatively the most significant digits
			  ;; in a double-float number, plus room for a leading or
			  ;; trailing zero.
			  22
			  ;; This is conservatively the most leading or trailing
			  ;; zeroes that will be printed.
			  (abs (truncate (log (abs f) 10))))
			 f))))
	     (xsd "double")))
  :check-type number
  )

(def-soap-simple-content ((enc "base64") (enc "base64Binary") (xsd "base64Binary"))
  :class soap-connector
  :decode ((declare (ignore options))
	   (decode-base64-string data))
  :encode ((declare (ignore options))
	   (xmp-encode-content conn (encode-base64-string data))
	   (xsd "base64Binary"))
  :check-type (or string (array (signed-byte 8)) (array (unsigned-byte 8)))
  )

;;;??? enc:hexBinary  xsd:?
#+ignore
(def-soap-simple-content ((enc "hexBinary") (xsd "hexBinary"))
  :class soap-connector
  :decode ((declare (ignore options))
	   
	   )
  :encode ((declare (ignore options))
	   ;; accept: number (array (unsigned-byte 8)) (array (signed-byte 8))

	   (xsd "hexBinary")))

(defun parse-decimal (string &key (start 0) (end (length string))
			     min max digits (fraction  0) whitespace
			     &aux fract tail)
  (typecase whitespace
    (list nil)
    (character (setf whitespace (list whitespace)))
    (otherwise (setf whitespace (list #\space))))
  (typecase fraction
    ((member nil) nil)
    (integer nil)
    (otherwise (setf fraction end)))
      
  (flet ((white-p (string start end white)
		  (do ((i start (1+ 1)))
		      ((eql i end) t)
		    (or (member (elt string i) white)
			(return nil))))
	 (dcount (string start end white)
		(do ((i start (1+ i)) (n 0) c)
		    ((eql i end) n)
		  (setf c (elt string i))
		  (or (member c white)
		      (when (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) 
			(incf n))
		      (return n)))))

    (multiple-value-bind (int pos)
	(parse-integer string :start start :end end :junk-allowed t)
      (or (when (and int
		     (or (eql pos end)
			 (when (eql #\. (elt string pos))
			   (incf pos)
			   (multiple-value-setq (fract tail)
			     (parse-integer string :start pos :end end
					    :junk-allowed t))
			   (and fract
				(white-p string tail end whitespace)
				(eql tail end)))))
	    (let* ((fdigits (when fract (dcount string pos tail whitespace)))
		   (res (if fract
			    (+ int (/ fract (expt 10 fdigits)))
			  int)))
	      (and digits (< digits (+ (dcount string start pos whitespace)
				       (if fract fdigits 0)))
		   (error "Out of range decimal."))
	      (and min (< res min) (error "Out of range decimal."))
	      (and max (< max res) (error "Out of range decimal."))
	      (and fract (< fraction fdigits) (error "Out of range decimal."))
	      res))))))


(defmethod xmp-complex-content ((conn soap-string-in-connector) 
				(elt (eql (env "Envelope"))) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (let (h b tl)
    (dolist (d data)
      (cond (b (push d tl))
	    ((eq :body (first d))
	     (when b
	       (soap-client-error conn :string "More than one Body."))
	     (setf b d))
	    ((eq :header (first d)) (push d h))
	    (t (soap-client-error conn :string "Not Header or Body."))))
    (or b (soap-client-error conn :string "No Body."))
    (list (list* :envelope
		 (list* :headers (reverse h))
		 b 
		 (when tl (list :tail (reverse tl)))))))

(defmethod xmp-complex-content ((conn soap-string-in-connector) 
				(elt (eql (env "Header"))) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (list (list* :header data)))

(defmethod xmp-complex-content ((conn soap-string-in-connector) 
				      (elt (eql (env "Body"))) data
				      &rest options &key &allow-other-keys)
  (declare (ignore options))

  (cond 
   ((eq :many (soap-body-form conn)))
   ((null data) (soap-client-error conn :string "Empty Body"))
   ((cdr data)  (soap-client-error conn :string "Too many elements in Body")))

  (list (list* :body data)))

(defmethod xmp-complex-content ((conn soap-string-in-connector) 
				      (elt (eql (env "Fault"))) (data t)
				      &rest options &key &allow-other-keys)
  (declare (ignore options))
  (list (list* :fault data)))


(defmethod soap-fill-array ((conn soap-connector) array indices dims data &aux d)
  (loop
   (cond ((null data) (return))
	 ((do ((i indices (cdr i)) (j dims (cdr j)))
	      ((atom i) t)
	    (or (< (first i) (first j)) (return nil)))
	  (setf d (pop data))
	  (setf (apply #'aref array indices) 
		(if (consp d)
		    (soap-result-string conn d nil)
		  d))
	  (bump-index indices dims))
	 (t (return)))))

(defun bump-index (indices dims &optional (j (1- (length dims))))
  ;; this is called only if every index is less than 
  ;;  the corresponding dims element
  (incf (elt indices j))
  (when (eql (elt indices j) (elt dims j))
    (when (> j 0)
      (setf (elt indices j) 0)
      (bump-index indices dims (1- j)))))


(defmethod xmp-complex-content ((conn soap-string-in-connector) 
				(elt t) data
				&rest options &key attributes &allow-other-keys
				&aux type atype outer res length array a2)
  (multiple-value-setq (type atype res) (soap-decode-type conn attributes :in))
  (cond ((or atype (xmp-defined-array-def conn elt :in 1))
	 
	 ;; this code ignores position attributes???
	 ;;  maybe pos attributes could be saved in array element?

	 (when (member nil (setf length (xmp-getf-in-part conn res :length)))
	   (setf length nil))

	 ;; We need to delay array creation until all the body has been collected
	 ;; because array elements may be empty multiRef references.
	 ;; 
	 (or length (setf length (length data)))
	 (setf array (make-array length))
	 (push (list length array data) (soap-message-arrays conn))
	 (values (list (list elt array)) elt))
	(type
	 (if (equal elt type)
	     ;; There was no method specialized on the declared type
	     ;;  so return the data as accepted.
	     data
	   (values (list
		    (list* elt (apply 'xmp-complex-content conn type data options)))
		   elt)))
	((and (setq outer (second (xmp-inside-elements conn)))
	      (progn (multiple-value-setq (res a2 res)
		       (soap-decode-type conn (cdr outer) :in))
		     a2)
	      res
	      (not (eq elt a2)))
	 (values (apply 'xmp-complex-content conn a2 data options)
		 elt))
	(t (list (list* elt data)))))


(defmethod soap-update-arrays ((conn soap-connector))
  (let* ((arrays (soap-message-arrays conn)))
    (dolist (a arrays)
      (let* ((length (first a))
	     (array (second a))
	     (data (third a)))
	(etypecase length
	  (null nil)
	  (cons nil)
	  (integer (setf length (list length))))
	(soap-fill-array conn
			 array
			 (mapcar #'(lambda (x) (declare (ignore x)) 0) length)
			 length
			 data)))))

(defmethod soap-must-understand ((conn soap-connector) (elt t))

  ;; The application must supply a soap-must-understand method
  ;;  for any header element that must be understood.

  (case (soap-must-understand-flag conn)
    (:warn
     (xmp-warning conn "Accepting mustUnderstand on ~S" elt))
    ((nil) nil)
    (otherwise
     (xmp-error conn 'soap-must-understand :string elt))))



(defmethod xmp-encode ((dest soap-connector) data (type cons)
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (if (and (eq :simple (first type)) (atom data))
      (call-next-method)
    (soap-encode-parts dest data nil type)))


(defmethod xmp-encode ((dest soap-connector) data (type t)
		       &rest options &key &allow-other-keys &aux res def)

  ;; A default method to encode anything unknown as a string.

  (typecase data
    (xmp-element (call-next-method))
    (otherwise
     (cond ((and (setf res (soap-resolve-type dest type :out))
		 (eq :simple (first res))
		 (setf def (second res))
		 (not (eq type def)))
	    (apply 'xmp-encode dest data def options))
	   ((and res
		 (eq :simple (first res))
		 (null def)
		 (setf def (getf (cddr res) :simple-content-key))
		 (not (eq type def)))
	    (apply 'xmp-encode dest data def options))
	   (t (when data (xmp-encode-content dest (format nil "~A" data)))
	      (xsd "string"))))))

(defmethod xmp-encode ((dest soap-connector) (data integer) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data (xsd "int") options))

(defmethod xmp-encode ((dest soap-connector) (data single-float) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data (xsd "double") options))

(defmethod xmp-encode ((dest soap-connector) (data double-float) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data (xsd "double") options))

(defmethod xmp-encode ((dest soap-connector) (data string) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data (xsd "string") options))





;;; SOAP Server


(defun soap-message-server (&rest keys &key start 
				  (enable :start)
				  publish 
				  (class 
				   'soap-aserve-server-string-in-out-connector)
				  &allow-other-keys
				  &aux (*xmp-warning-leader* "SOAP")
				  )
  ;; keys passed to make-instance:
  ;;   action lisp-package
  (or (member :path publish) (setq publish (list* :path "/SOAP" publish)))
  (when (or (member :function publish) (member :content-type publish))
    (xmp-error nil 'soap-server-argument :sub-code :soap-message-server
	   :string "does not allow :function or :content-type arguments"))
  (let ((server (apply 'make-instance class :start start :parameters publish
		       (remove-keys '(:start :enable :publish :class) keys))))
    (case enable 
      ((nil) nil)
      (:start-new (xmp-start-server server :new t :enable t))
      (:start (xmp-start-server server :enable t))
      (otherwise (xmp-enable-server server)))
    server))

(defmethod start-soap-server ((server soap-server-connector) &rest args
			      &key new start enable &allow-other-keys)
  (declare (ignore new start enable))
  (apply #'xmp-start-server server args))

(defmethod stop-soap-server ((server soap-server-connector) &rest args
			     &key disable &allow-other-keys)
  (declare (ignore disable))
  (apply #'xmp-stop-server server args))

(defmethod enable-soap-server ((server soap-server-connector) &rest args
			       &key &allow-other-keys)
  (apply #'xmp-enable-server server args))

(defmethod disable-soap-server ((server soap-server-connector) &rest args
				&key &allow-other-keys)
  (apply #'xmp-disable-server server args))



(defmethod xmp-export-standard-methods ((server soap-server-connector)
					&key &allow-other-keys)
  t)


(defmethod xmp-server-response ((server soap-aserve-server-connector)
				&key request entity &allow-other-keys
				&aux action)
  (setf action (xmp-header-slot request (xmp-symbol "SOAPAction" :keyword)))
  (call-next-method server :request request :entity entity 
		    :options (list :action action)))  

(defmethod soap-invoke-method :around ((server soap-server-connector) (name t) (args t)
				       &key headers)
  (declare (ignore headers))
  (let ((*soap-server* server))
    (call-next-method)))

(defmethod soap-invoke-method ((server soap-server-connector) (name t) (args t)
			       &key headers
			       &aux (*xmp-warning-leader* "SOAP"))
  (declare (ignore headers))
  (xmp-invoke-method server name args))



(defvar *soap-last-server* nil)
(defmethod xmp-server-implementation ((server soap-aserve-server-connector) body
				      &rest options &key action &allow-other-keys)
  (declare (ignore options))
  
  (if* (eq :get (net.aserve:request-method (net.xmp::aserve-request server)))
     then (if* (net.aserve::request-query-value "wsdl"
		(net.xmp::aserve-request server))
		      
	     then (setf (net.xmp::xmp-destination-content-type server) 
		    "text/xml")
		  (let ((wsdl (soap-server-wsdl server)))
		    (if* wsdl
		       then (setf (net.xmp::xmp-destination-content-type server) 
			      "text/xml")
			    (setf (net.xmp::xmp-message-string server)
			      wsdl)
			    
		       else (setf (net.xmp::xmp-destination-content-type server)
				  "text/html")
			    (setf (net.xmp::xmp-message-string server)
				  "<html><body>No wsdl specified</body></html>"))
		    (return-from xmp-server-implementation nil)))
	  
	  (setf (net.xmp::xmp-destination-content-type server) "text/html")
	  (setf (net.xmp::xmp-message-string server) "<html><body>Server Running</body></html>")
	  (return-from xmp-server-implementation nil))
  
  ;; parse an rpc call and pass it to the exported function
  (when (soap-debug-p server)
    (format t "~&Received SOAP message:~%")
    (soap-print-xml body))
  (when (xmp-server-enabled server)
    (setf *soap-last-server* server)
    (when (soap-debug-p server)
      (format t "~&   Server is Enabled.~%"))
    (let* ((code (env "Client")) rval vals body-error (body-done (list nil)))
      (flet ((body
	      ()
	      (let* ((r (xmp-decode-message
			 server
			 ;; xmp-parse-message automatically includes
			 ;;  message-dns namespaces
			 (xmp-parse-message server body)))
		     (blist (cdr (assoc :body (cdr r))))
		     (headers (assoc :headers (cdr r)))
		     body method signature params)
		;; Save pointer to body and extract all parts looking for multiRef's
		(setf (soap-message-body server) blist
		      (soap-message-headers server) (cdr headers))
		(setf body (soap-result-pair server blist 0))

		;; Update content of arrays now
		(soap-update-arrays server)
		
		(setf method (first body))
		(setf signature (list* action nil
				       ;; if body is not complex, then signature is nil
				       (when (consp (first (cdr body)))
					 (mapcar #'car (cdr body)))))
		(setf params (mapcar
			      #'(lambda (x) (soap-result-string server x nil))
			      (cdr body)))

		(when (soap-debug-p server)
		  (format t "~&   method: ~S~%" method)
		  (format t "~&   signature: ~S~%" signature)
		  )

		;; signature -> element names collected from incoming message
		;;    params -> (e1 v1 e2 v2 ... )
		;;               ei name in signature   vi content from message
		;; [bug16269]
		;;   export def has mapping from export signature to keyword list
		;;   modified params from xmp-accept-method -> (k1 v1 k2 v2 ... )
		;;              ki is keyword associated with element ei

		(setf params (mapcan #'list (cddr signature) params))
		(multiple-value-bind (fn rt modargs)
		    (xmp-accept-method server method signature params)
		  (if (null fn)
		      (xmp-error server :client
				 :string (setf body-error
					       (format nil "Undefined method ~S~S"
						       method signature)))
		    (let ()
		      (when (soap-debug-p server)
			(format t "~&   calling fn: ~S~%" fn)
			(format t "~{~&   argument: ~S~%~}" modargs)
			)
		      (setf (soap-server-message-method server) method
			    (soap-server-message-return server) rt
			    (soap-server-message-signature server) (cddr signature)
			    (soap-server-message-action server) action
			    )
		      
		      (setf vals (multiple-value-list
				  (soap-invoke-method server fn modargs
						      :headers headers)))
		      (if (null vals)
			  (xmp-error server :client :string "Call refused")
			(let (relt rtype)
			  (etypecase rt
			    (symbol
			     (cond ((setf rtype (soap-find-element server rt :out))
				    (setf relt rt))
				   ((setf rtype (soap-find-type server rt :out)))
				   (t (error "Ill-defined method return.")))
			     )
			    (cons (ecase (first rt)
				    (:element (setf relt (xmp-pick-name server rt)
						    rtype (third rt)))
				    ((:complex :simple) (setf rtype rt))
				    )))
			  (if relt
			      (setf rval (soap-encode-object
					  server relt rtype (first vals)))
			    (setf rval (soap-encode-parts-to-object
					server rtype (first vals)))
			    ))))
		    ))
		body-done)))
	(multiple-value-bind (v e)
	    (case (soap-debug-p server)
	      (:break
		(block body
		  (let (done)
		    (unwind-protect (progn
				      (body)
				      (setf done t))
		      (return-from body
			(if done
			    (values nil nil)
			  (values nil (or body-error "Debugged error."))))))))
	      (otherwise (ignore-errors (body))))
	  (when (soap-debug-p server)
	    (if (or e (not (eq v body-done)))
		(format t "~&   call failed: ~S ~A~%" v e)
	      (format t "~&   call returned: ~S~%" (first vals))))
	  (typecase e
	    (null nil)
	    (xmp-condition (setf rval (soap-make-fault
				       server
				       (xmp-fault-code e)
				       (xmp-fault-string e)
				       :sub-code (xmp-fault-sub-code e)
				       :factor (xmp-fault-factor e)
				       :detail (xmp-fault-detail e))))
	    (otherwise
	     (setf rval (soap-make-fault server code (format nil "~A" e)
					 :sub-code "LispError"))))
	  (setf rval (multiple-value-list (soap-encode-message server rval nil)))
	  (when (soap-debug-p server)
	    (format t "~&   encoded result element: ~S~%" (first rval))
	    (format t "~&       encoded resulttype: ~S~%" (second rval))
	    (format t "~&       encoded result XML:~%")
	    (soap-print-xml (third rval))
	    )
	  (values-list rval))))))

(defmethod xmp-signature-equal ((conn soap-connector) ss1 ss2
				&aux (s1 ss1) (s2 ss2) k1 k2 kt lswap)
  ;; SOAP  sig -> (action collector elt-name ... )
  ;;    action -> string
  ;; collector -> nil  :seq  :seq1  :set  :set1
  ;;  elt-name -> symbol  string  (:any-case string) (:any)
  ;; pattern is (action-or-nil collector ...)
  ;;  target is (action-or-nil nil ...)
  ;; When defining exports, both signatures have non-nil collector.
  (cond
   ((or (atom ss1) (atom ss2)) nil)
   ((and (second ss1) (null (second ss2))) (xmp-signature-equal conn ss2 ss1))
   ((and (first ss2) (not (equal (first ss1) (first ss2)))) nil)
   ((null (second ss2))
    (xmp-error conn :internal :string "Comparing two candidate signatures 1."))
   (t (pop s1) (pop s2)
      (setf k1 (pop s1) k2 (pop s2))
      (case k1
	((nil) (setf kt k2))
	(:seq (case k2
		(:seq  (setf lswap t kt :seq))
		(:seq1 (setf lswap t kt :seq))
		(:set  (setf lswap t kt :set))
		(:set1 (setf lswap t kt :set))
		(otherwise
		 (xmp-error conn :internal :string "Ill-formed signature 1."))))
	(:seq1 (case k2
		 (:seq  (setf lswap t kt :seq))
		 (:seq1 (setf kt :seq1))
		 (:set  (setf lswap t kt :set))
		 (:set1 (setf lswap t kt :set1))
		 (otherwise
		  (xmp-error conn :internal :string "Ill-formed signature 2."))))
	(:set  (case k2
		 (:seq  (setf lswap t kt :set))
		 (:seq1 (setf lswap t kt :set))
		 (:set  (setf lswap t kt :set))
		 (:set1 (setf lswap t kt :set))
		 (otherwise
		  (xmp-error conn :internal
			     :string "Ill-formed signature 3.")))  )
	(:set1  (case k2
		  (:seq  (setf lswap t kt :set))
		  (:seq1 (setf lswap t kt :set1))
		  (:set  (setf lswap t kt :set))
		  (:set1 (setf lswap t kt :set1))
		  (otherwise
		   (xmp-error conn :internal
			      :string "Ill-formed signature 4."))) )
	(otherwise
	 (xmp-error conn :internal :string "Ill-formed signature 5.")))
      (when lswap
	(when (< (length s2) (length s1)) (rotatef s1 s2)))
      (flet ((eeq (e1 e2)
		  (or (eq e1 e2)
		      (typecase e1
			(symbol (typecase e2
				  (string (equal (string e1) e2))
				  (cons   (string-equal
					   (string e1) (second e2)))))
			(string (typecase e2
				  (symbol (equal e1 (string e2)))
				  (string (equal e1 e2))
				  (cons   (string-equal e1 (second e2)))))
			(cons   (typecase e2
				  (symbol (string-equal (second e1) (string e2)))
				  (string (string-equal (second e1) e2))
				  (cons   (string-equal
					   (second e1) (second e2)))))))))
	(when (ecase kt
		(:seq
		 (dolist (e1 s1
			     ;; Match is good if s1 is a sub-sequence of s2.
			     t)
		   (or (setf s2 (member e1 s2 :test #'eeq))
		       (return nil))
		   (pop s2)))
		(:seq1
		 ;; We need an exact match.
		 (every #'eeq s1 s2))
		(:set
		 (let (found i)
		   (dolist (e1 s1)
		     (setf i 0)
		     (or
		      (dolist (e2 s2)
			(when (eeq e1 e2)
			  (if (member i found)
			      (return nil)
			    (return (push i found))))
			(incf i))
		      (return nil)))
		   ;; Match is good if every element in s1 was found
		   ;;  exactly once in s2.
		   (equal (length found) (length s1))))
		(:set1
		 (let (found)
		   (dotimes (i (length s2)
			       ;; Match is good if s1 and s2 contain the same
			       ;;  elements but for order.
			       (equal (length found) (length s1))
			       )
		     (if (member (elt s2 i) s1 :test #'eeq)
			 (push i found)
		       (return nil)))))
		)
	  (cond ((null k1) t)
		((and (eq k1 k2) (eql (length s1) (length s2))) t)
		(t (xmp-error conn :def :string "Conflicting signatures.")))
	  )))))



(defmethod soap-make-fault ((server soap-connector) code string
			    &key factor detail sub-code)
  (setf code (case code
	       (:server (env "Server"))
	       (:client (env "Client"))
	       (:version-mismatch (env "VersionMismatch"))
	       (:must-understand  (env "MustUnderstand"))
	       (otherwise code)))
  (soap-encode-object server 
		      (env "Fault")
		      nil
		      `(,(xmp-symbol "faultcode" :keyword)
			 ,(if sub-code
			     (intern
			      (format nil "~A.~A" code sub-code)
			      (if (symbolp code) (symbol-package code) :keyword))
			   code)
			 ,(xmp-symbol "faultstring" :keyword) ,string
			 ,@(when factor (list (xmp-symbol "faultactor" :keyword) factor))
			 ,@(when detail (list (xmp-symbol "detail" :keyword) detail)))
		      ))



(defmethod soap-export-method ((conn soap-server-connector) name sig
			       &rest keys
			       &key lisp-name (enable t) return help
			       (action nil a-p) ordered exact
			       &allow-other-keys
			       &aux (*xmp-warning-leader* "SOAP")
			       )
  ;; if action is specified as nil then ignore message action
  ;; if action is omitted, then use soap-server-action as default
  ;;                            :none -> nil
  ;; return -> element-name | element-def 
  (case action 
    (:none (setf action nil a-p t))
    (:default (setf action nil a-p nil)))
  (or action a-p
      (typecase (setf action (soap-server-action conn))
	((member :none) (setf action nil a-p t))
	((member nil) nil)))
  (typecase action
    ((member nil) nil)
    (string nil)
    (symbol (setf action (symbol-name action)))
    (otherwise (xmp-error conn :def :string "Action must be symbol or string.")))
  (or action a-p
      (xmp-error conn :def :string "Action must be specified."))      
  (xmp-export-method conn name
		     (list* action
			    (if ordered
				(if exact
				    :seq1
				  :seq)
			      (if exact
				  :set1
				:set))
			    sig)
		     :lisp-name lisp-name
		     :enable enable
		     :return return
		     :help help))

(defun soap-element-content (v)
  (cond ((atom v) v)
	((and (atom (first v)) (null (cdr v))) (first v))
	(t v)))

(defun soap-sub-element-content (v name &rest more)
  (apply #'soap-result-part nil v name more))

(defmethod soap-result-pair ((conn t) r name &rest more)
  (soap-result-internal conn r name more))

;; rfe6196
(defmethod soap-get-attributes ((conn soap-connector) (data t))
  (xmp-get-attributes conn data))
;; rfe6196
(defmethod soap-get-attribute ((conn soap-connector) (data t) (name t) &optional default)
  (xmp-get-attribute conn data name default))

(defmethod soap-result-internal ((conn soap-connector) r name more 
				 &key (top nil t-p) refp)
  (declare (ignore refp))
  ;; default calls get the default top-level message body
  ;; but caller may specify another context if needed.
  (if (or t-p top)
      (call-next-method)
    (call-next-method conn r name more :top (soap-message-body conn))))


(defmethod soap-result-internal ((conn t) r name more &key top refp
				 &aux v href ref idref id)
  ;; Input must be first value returned by call-soap-method
  ;;    - body-form=:one  -> (top-elt-name part...)
  ;;    - body-form=:many -> ((elt-name part...)...)
  ;; each name may be string  symbol posint
  ;; returns the list that begins with the last named sub-element
  ;;   this list is a suitable argument to get-attributes
  ;; return nil if sub-element is not found
  (cond ((atom r) nil)
	((atom (first r))
	 (when (or (null name) (eql name 0) (xmp-match-name nil (first r) name))

	   ;; resolve multiRef here  [rfe6169]
	   (when (and (null (cdr r))
		      (or 
		       ;; SOAP 1.1 uses href="#idstring"
		       (and (setf href (xmp-get-attribute conn r "href"))
			       (setf href (string href))
			       (not (equal href ""))
			       (eql #\# (elt href 0))
			       (setf idref (subseq href 1))
			       (not (equal idref "")))
		       ;; SOAP 1.2 uses enc:ref=???
		       (and (setf ref (xmp-get-attribute conn r "ref"))
			       (setf ref (string ref))
			       (not (equal ref ""))
			       (eql #\# (elt ref 0))
			       (setf idref (subseq ref 1))
			       (not (equal idref "")))))
	     (when refp (return-from soap-result-internal t))
	     (dolist (e top)
	       (when (and (consp e)
			  (xmp-match-name conn (first e) "multiRef")
			  (setf id (xmp-get-attribute conn e "id"))
			  (equal idref id))
		 (setf r e)
		 (return))))

	   (cond (more (soap-result-internal
			conn (cdr r) (first more) (cdr more) :refp refp :top top))
		 (refp nil)
		 (t r))))
	((typecase name
	   ((integer 0) (setf v (nth name r)))
	   (null (setf v (first r)))
	   (otherwise (setf v (xmp-assoc nil name r))))
	 (soap-result-internal conn v nil more :refp refp :top top))
	(t nil)))

(defmethod soap-result-part ((conn t) r name &rest more &aux v)
  ;; return nil if sub-element is not found
  (when (setf v (apply #'soap-result-pair conn r name more))
    (soap-element-content (cdr v))))

(defmethod soap-result-string ((conn t) r name &rest more &aux v)
  ;; return a single string if list of strings  (or chars)
  ;;  otherwise return the result of soap-element-content
  (when (setf v (apply #'soap-result-pair conn r name more))
    (typecase (setf v (soap-element-content (cdr v)))
      (cons  (do ((tl v (cdr tl)) (len 0))
		 ((atom tl)
		  (if tl
		      v
		    ;; Element content is all strings and chars, concatenate.
		    (do ((t2 v (cdr t2)) l2 (i 0) (res (make-string len)))
			((atom t2) res)
		      (typecase (first t2)
			(character (setf (elt res i) (first t2))
				   (incf i))
			(string  (setf l2 (length (first t2)))
				 (setf (subseq res i (+ i l2)) (first t2))
				 (incf i l2)))
		      )))
	       (typecase (first tl)
		 (character (incf len))
		 (string (incf len (length (first tl))))
		 (otherwise (return v))))
	     )
      (otherwise v))))
    
(defmethod soap-result-only ((conn t) r error-p name &rest more &aux v)
  ;; signal error if sub-element is not found
  (if (setf v (apply #'soap-result-pair conn r name more))
      (soap-element-content (cdr v))
    (case error-p
      (:error
       (error "Cannot find sub-element ~S~{ ~S~} in ~S" name more r))
      (otherwise error-p))))

(defmethod soap-result-typed ((conn t) r type error-p name &rest more
			      &aux (rtype (soap-resolve-type conn type nil)))
  (cond
   ((eq rtype (xsd "string"))
    (or (apply #'soap-result-string conn r name more)
	(case error-p
	  (:error
	   (error "Cannot find sub-element ~S~{ ~S~} in ~S" name more r))
	  (otherwise error-p))))
   ;;??? more conversions
   (t (apply #'soap-result-only conn r error-p name more))))
  


	 

(defun soap-alist-to-plist (l &optional recursive)
  (if (consp l)
      (mapcan #'(lambda (x)
		  (when (consp x)
		    (list (first x)
			  (let ((y (soap-element-content (cdr x))))
			    (if recursive
				(soap-alist-to-plist y)
			      y)))))
	      l)
    l))


(defun define-soap-names ()

  (xmp-define-namespace-map 
   :schema1999
   t  
   (list
    nil
    (list :net.xmp.schema
	  "xsd"
	  "http://www.w3.org/1999/XMLSchema")
    (list :net.xmp.schema
	  "xsd"
	  "http://www.w3.org/1999/XMLSchema/")
    (list :net.xmp.schema-instance
	  "xsi"
	  "http://www.w3.org/1999/XMLSchema-instance")))

  (xmp-define-namespace-map
   :schema2000
   t  
   (list
    nil
    (list :net.xmp.schema
	  "xsd"
	  "http://www.w3.org/2001/XMLSchema")
    (list :net.xmp.schema
	  "xsd"
	  "http://www.w3.org/2001/XMLSchema/")
    (list :net.xmp.schema-instance
	  "xsi"
	  "http://www.w3.org/2001/XMLSchema-instance")))

  (xmp-define-namespace-map
   :schema t  
   (list nil :schema2000 :schema1999))

  (xmp-define-namespace-map
   :schema1 t  
   (list nil :schema1999 :schema2000))

  (xmp-define-namespace-map
   :soap1.2 t  
   (list
    nil
    (list :net.xmp.soap.envelope
	  "SOAP-ENV"
	  "http://schemas.xmlsoap.org/soap/envelope/")
    (list :net.xmp.soap.encoding
	  "SOAP-ENC"
	  "http://schemas.xmlsoap.org/soap/encoding/")
    ))

  (xmp-define-namespace-map
   :soap1.1 t  
   (list
    nil
    (list :net.xmp.soap.envelope
	  "SOAP-ENV"
	  "http://schemas.xmlsoap.org/soap/envelope/")
    (list :net.xmp.soap.encoding
	  "SOAP-ENC"
	  "http://schemas.xmlsoap.org/soap/encoding/")
    ))

  (xmp-define-namespace-map :soap  t (list nil :soap1.2 :soap1.1 :schema))
  (xmp-define-namespace-map :soap1 t (list nil :soap1.1 :soap1.2 :schema))

  (define-xmp-element nil (env "Envelope") `(:complex
					    (:seq (:seq* ,(env "Header"))
						  ,(env "Body")
						  (:seq* (:any)))))
  (define-xmp-element nil (env "Header")   (xmp-any-type nil))
  (define-xmp-element nil (env "Body")     (xmp-any-type nil))
  (define-xmp-element nil (env "Fault")    `(:complex
					    (:set
					     (:element "faultcode"   ,(enc "QName"))
					     (:element "faultstring" ,(enc "string"))
					     (:element "faultactor" ,(enc "string"))
					     (:element "detail"
						       (:complex (:seq* (:any))))
					     (:seq* (:any))
					     )))
  (define-xmp-element nil "multiRef"    '(:complex (:seq* (:any))))
  (define-xmp-type    nil (enc "Array")  '(:complex (:seq* (:any))))

  (define-soap-type nil (xsd "ur-type") (xmp-any-type nil))
  (define-soap-type nil (xsd "anyType") (xmp-any-type nil))
  (macrolet ((types () (list* 'progn (reverse *soap-deftypes*))))
    (types))
  
  )
(define-soap-names)

