;; -*- mode: common-lisp; package: net.xmp.soap -*-
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

;; $Id: xmp-soap.cl,v 2.2 2004/02/13 05:35:28 layer Exp $

;; SOAP support

(defpackage :net.xmp.soap)
(in-package :net.xmp.soap)

(defpackage :net.xmp.soap (:use :common-lisp :excl :net.xmp))

(eval-when (compile eval)
  (defmacro soap-case-mode () *current-case-mode*))

(eval-when (compile eval load)
(defparameter *soap-raw-version* #.(sys::version-from-file "ChangeLog"))
)
(defparameter *soap-version*
    '#.(sys::version-string-to-list *soap-raw-version*))

(defun soap-version (&optional v1-or-s v2 v3 error-p &aux (v1 v1-or-s))
  (typecase v1
    (integer (if (or (< (first *soap-version*) v1)
		     (and v2
			  (or 
			   (and (= (first *soap-version*) v1)
				(< (second *soap-version*) v2))
			   (and v3
				(= (second *soap-version*) v2)
				(< (third *soap-version*) v3)))))
		 (if error-p
		     (error
		      "SOAP Module Version ~A.~A.~A needed, but ~{~A.~A.~A~} is loaded."
		      v1 (or v2 0) (or v3 0) *soap-version*)
		   nil)
	       *soap-version*))
    (otherwise (format v1-or-s "SOAP Module Version ~{~A.~A.~A~}" *soap-version*))))
    
(eval-when (compile load eval)
  (defparameter *soap-case-mode* (soap-case-mode))
  (or (eq *soap-case-mode* *current-case-mode*)
      (error "Expected *current-case-mode*=~S" *soap-case-mode*))

  #+ignore
  (or (position :soapaction net.aserve::*header-keyword-array*
		:test #'string-equal)
      (cerror
       "Continue, ignoring SOAPAction headers."
       "This version of AllegroServe does not recognize HTTP SOAPAction headers."))

  (or (multiple-value-bind (v0 v1 v2)
	  (values-list net.aserve:*aserve-version*)
	(or (< 1 v0)
	    (and (eql 1 v0)
		 (or (< 2 v1)
		     (and (eql 2 v1)
			  (< 29 v2))))))
      (cerror
       "Continue, ignoring SOAPAction headers."
       "This version of AllegroServe does not recognize HTTP SOAPAction headers."))
	

  )



(defpackage :net.xmp.soap
  (:export

   soap-version
   soap-message-client
   soap-message-server
   soap-export-method

   soap-element
   soap-header
   soap-headers
   soap-add-header
   make-soap-header
   soap-must-understand

   soap-sub-element-content
   soap-alist-to-plist

   define-soap-type
   define-soap-element
   call-soap-method
   *soap-server*
   soap-invoke-method
   soap-encode-object

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
     "boolean";; simple-content enc:|boolean|
     "base64"
     "base64Binary";; simple-content enc:|base64Binary|
     "hexBinary"
     "float";; simple-content enc:|float|
     "double";; simple-content enc:|double|
     "anyURI"
     "QName";; simple-content enc:|QName|
     "string";; simple-content enc:|string|
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
     "int";; simple-content enc:|int|
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
  )

 
(defclass soap-connector (xmp-connector) 
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
   (message-dns     :initform
		    (list nil
		     (list :net.xmp.schema-instance
			   "xsi"
			   "http://www.w3.org/2001/XMLSchema-instance")
		     (list :net.xmp.schema
			   "xsd"
			   "http://www.w3.org/2001/XMLSchema")
		     (list :net.xmp.soap.envelope
			   "soap"
			   "http://schemas.xmlsoap.org/soap/envelope/")
		     (list :net.xmp.soap.encoding
			   "SOAP-ENC"
			   "http://schemas.xmlsoap.org/soap/encoding/")
		     )

		    #+ignore
		    (list nil
			  (list :net.xmp.soap.envelope
				"SOAP-ENV"
				"http://schemas.xmlsoap.org/soap/envelope/")
			  (list :net.xmp.soap.encoding
				"SOAP-ENC"
				"http://schemas.xmlsoap.org/soap/encoding/")
			  (list :net.xmp.schema
				"xsd"
				"http://www.w3.org/1999/XMLSchema")
			  (list :net.xmp.schema-instance
				"xsi"
				"http://www.w3.org/1999/XMLSchema-instance")
			  )

		    )

   (soap-headers    :accessor soap-headers :initform nil)
   (lisp-package    :initform :net.xmp.soap.none)
   (trim-whitespace :initform t)
   (must-understand :accessor soap-must-understand-flag
		    :initform :warn :initarg :must-understand)
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
   (string-type     :initform 'enc:|string|) 
   (undef-list      :accessor soap-undef-list :initform nil)

   (port-name       :accessor soap-port-name    :initarg :port-name :initform nil)
   (binding-name    :accessor soap-binding-name :initarg :binding-name :initform nil)
   (service-name    :accessor soap-service-name :initarg :service-name :initform nil)

   ))

(defmethod xmp-copy ((object soap-connector)
		     &key &allow-other-keys
		     &aux (new (call-next-method)))
  (setf (soap-encoding-style new) (soap-encoding-style object)
	(soap-actor new) (soap-actor object)
	(soap-headers new) (soap-headers object)
	(soap-must-understand-flag new) (soap-must-understand-flag object)
	(soap-decode-flag new) (soap-decode-flag object)
	)
  new)

(defclass soap-client-connector  (soap-connector xmp-client-connector)  ())
(defclass soap-server-connector  (soap-connector xmp-server-connector)
  (
   (action :accessor soap-server-action :initarg :action :initform nil)
   ))

(defclass soap-string-out-connector 
  (soap-connector xmp-string-out-connector) ())
(defclass soap-stream-out-connector  
  (soap-connector xmp-stream-out-connector) ())
(defclass soap-string-in-connector   
  (soap-connector xmp-string-in-connector)  ())
(defclass soap-event-in-connector
  (soap-connector xmp-event-in-connector)   ())

(defclass soap-string-in-out-connector 
  (soap-string-out-connector 
   soap-string-in-connector 
   xmp-string-in-out-connector)  ())
(defclass soap-event-string-connector 
  (soap-string-out-connector 
   soap-event-in-connector 
   xmp-event-string-connector)   ())
(defclass soap-string-stream-connector
  (soap-stream-out-connector  
   soap-string-in-connector 
   xmp-string-stream-connector)  ())
(defclass soap-event-stream-connector
  (soap-stream-out-connector 
   soap-event-in-connector 
   xmp-event-stream-connector)   ())
   
(defclass soap-client-string-out-connector
  (soap-client-connector 
   soap-string-out-connector 
   xmp-client-string-out-connector) ())
(defclass soap-client-stream-out-connector
  (soap-client-connector 
   soap-stream-out-connector 
   xmp-client-stream-out-connector) ())
(defclass soap-client-string-in-connector
  (soap-client-connector 
   soap-string-in-connector
   xmp-client-string-in-connector)  ())
(defclass soap-client-event-in-connector
  (soap-client-connector 
   soap-event-in-connector 
   xmp-client-event-in-connector)   ())

(defclass soap-client-string-in-out-connector
  (soap-client-string-out-connector 
   soap-client-string-in-connector
   soap-string-in-out-connector
   xmp-client-string-in-out-connector) ())
(defclass soap-client-event-string-connector
  (soap-client-string-out-connector 
   soap-client-event-in-connector
   soap-event-string-connector
   xmp-client-event-string-connector)  ())
(defclass soap-client-string-stream-connector
  (soap-client-stream-out-connector 
   soap-client-string-in-connector
   soap-string-stream-connector
   xmp-client-string-stream-connector) ())
(defclass soap-client-event-stream-connector
  (soap-client-stream-out-connector
   soap-client-event-in-connector
   soap-event-stream-connector
   xmp-client-event-stream-connector) ())

(defclass soap-server-string-out-connector
  (soap-server-connector 
   soap-string-out-connector
   xmp-server-string-out-connector) ())
(defclass soap-server-stream-out-connector
  (soap-server-connector
   soap-stream-out-connector
   xmp-server-stream-out-connector) ())
(defclass soap-server-string-in-connector
  (soap-server-connector
   soap-string-in-connector
   xmp-server-string-in-connector)   ())
(defclass soap-server-event-in-connector
  (soap-server-connector
   soap-event-in-connector
   xmp-server-event-in-connector)   ())

(defclass soap-server-string-in-out-connector 
  (soap-server-string-out-connector 
   soap-server-string-in-connector
   soap-string-in-out-connector 
   xmp-server-string-in-out-connector) ())
(defclass soap-server-event-string-connector
  (soap-server-string-out-connector 
   soap-server-event-in-connector
   soap-event-string-connector
   xmp-server-event-string-connector) ())
(defclass soap-server-string-stream-connector
  (soap-server-stream-out-connector 
   soap-server-string-in-connector
   soap-string-stream-connector
   xmp-server-string-stream-connector) ())
(defclass soap-server-event-stream-connector
  (soap-server-stream-out-connector 
   soap-server-event-in-connector
   soap-event-stream-connector
   xmp-server-event-stream-connector) ())




(defclass soap-aserve-connector (soap-connector xmp-aserve-connector)
  ((method        :initform :post)
   (http-protocol :initform :http/1.0)
   (content-type  :initform "text/xml")
   (xml-leader    :initform "xml version=\"1.0\"")
   ))

(defclass soap-aserve-client-connector
  (soap-aserve-connector 
   xmp-aserve-client-connector
   soap-client-connector)  ())
(defclass soap-aserve-server-connector
  (soap-aserve-connector
   xmp-aserve-server-connector
   soap-server-connector)  ())

(defclass soap-aserve-string-out-connector
  (soap-aserve-connector
   xmp-aserve-string-out-connector
   soap-string-out-connector) ())
(defclass soap-aserve-stream-out-connector
  (soap-aserve-connector
   xmp-aserve-stream-out-connector
   soap-stream-out-connector)  ())
(defclass soap-aserve-string-in-connector
  (soap-aserve-connector
   xmp-aserve-string-in-connector
   soap-string-in-connector)   ())
(defclass soap-aserve-event-in-connector 
  (soap-aserve-connector
   xmp-aserve-event-in-connector
   soap-event-in-connector)   ())

(defclass soap-aserve-string-in-out-connector 
  (soap-aserve-string-out-connector
   soap-aserve-string-in-connector
   xmp-aserve-string-in-out-connector
   soap-string-in-out-connector) ())
(defclass soap-aserve-event-string-connector 
  (soap-aserve-string-out-connector
   soap-aserve-event-in-connector
   xmp-aserve-event-string-connector
   soap-event-string-connector)  ())
(defclass soap-aserve-string-stream-connector
  (soap-aserve-stream-out-connector
   soap-aserve-string-in-connector
   xmp-aserve-string-stream-connector
   soap-string-stream-connector) ())
(defclass soap-aserve-event-stream-connector
  (soap-aserve-stream-out-connector
   soap-aserve-event-in-connector
   xmp-aserve-event-stream-connector
   soap-event-stream-connector)  ())
   
(defclass soap-aserve-client-string-out-connector
  (soap-aserve-client-connector
   soap-aserve-string-out-connector
   xmp-aserve-client-string-out-connector
   soap-client-string-out-connector) ())
(defclass soap-aserve-client-stream-out-connector
  (soap-aserve-client-connector
   soap-aserve-stream-out-connector
   xmp-aserve-client-stream-out-connector
   soap-client-stream-out-connector) ())
(defclass soap-aserve-client-string-in-connector
  (soap-aserve-client-connector
   soap-aserve-string-in-connector
   xmp-aserve-client-string-in-connector
   soap-client-string-in-connector)  ())
(defclass soap-aserve-client-event-in-connector
  (soap-aserve-client-connector
   soap-aserve-event-in-connector
   xmp-aserve-client-event-in-connector
   soap-client-event-in-connector)   ())

(defclass soap-aserve-client-string-in-out-connector
  (soap-aserve-client-string-out-connector 
   soap-aserve-client-string-in-connector
   soap-aserve-string-in-out-connector
   xmp-aserve-client-string-in-out-connector
   soap-client-string-in-out-connector) ())
(defclass soap-aserve-client-event-string-connector
  (soap-aserve-client-string-out-connector 
   soap-aserve-client-event-in-connector
   soap-aserve-event-string-connector
   xmp-aserve-client-event-string-connector
   soap-client-event-string-connector) ())
(defclass soap-aserve-client-string-stream-connector
  (soap-aserve-client-stream-out-connector 
   soap-aserve-client-string-in-connector
   soap-aserve-string-stream-connector
   xmp-aserve-client-string-stream-connector
   soap-client-string-stream-connector) ())
(defclass soap-aserve-client-event-stream-connector
  (soap-aserve-client-stream-out-connector
   soap-aserve-client-event-in-connector
   soap-aserve-event-stream-connector
   xmp-aserve-client-event-stream-connector
   soap-client-event-stream-connector) ())

(defclass soap-aserve-server-string-out-connector
  (soap-aserve-server-connector 
   soap-aserve-string-out-connector
   xmp-aserve-server-string-out-connector
   soap-server-string-out-connector) ())
(defclass soap-aserve-server-stream-out-connector
  (soap-aserve-server-connector
   soap-aserve-stream-out-connector
   xmp-aserve-server-stream-out-connector
   soap-server-stream-out-connector) ())
(defclass soap-aserve-server-string-in-connector
  (soap-aserve-server-connector
   soap-aserve-string-in-connector
   xmp-aserve-server-string-in-connector
   soap-server-string-in-connector)   ())
(defclass soap-aserve-server-event-in-connector
  (soap-aserve-server-connector
   soap-aserve-event-in-connector
   xmp-aserve-server-event-in-connector
   soap-server-event-in-connector)   ())

(defclass soap-aserve-server-string-in-out-connector 
  (soap-aserve-server-string-out-connector 
   soap-aserve-server-string-in-connector
   soap-aserve-string-in-out-connector 
   xmp-aserve-server-string-in-out-connector 
   soap-server-string-in-out-connector) ())
(defclass soap-aserve-server-event-string-connector
  (soap-aserve-server-string-out-connector 
   soap-aserve-server-event-in-connector
   soap-aserve-event-string-connector
   xmp-aserve-server-event-string-connector
   soap-server-event-string-connector) ())
(defclass soap-aserve-server-string-stream-connector
  (soap-aserve-server-stream-out-connector 
   soap-aserve-server-string-in-connector
   soap-aserve-string-stream-connector
   xmp-aserve-server-string-stream-connector
   soap-server-string-stream-connector) ())
(defclass soap-aserve-server-event-stream-connector
  (soap-aserve-server-stream-out-connector 
   soap-aserve-server-event-in-connector
   soap-aserve-event-stream-connector
   xmp-aserve-server-event-stream-connector
   soap-server-event-stream-connector) ())





(define-xmp-element nil 'env:|Envelope| '(:complex
					  (:seq (:seq* env:|Header|)
						env:|Body|
						(:seq* (:any)))))
(define-xmp-element nil 'env:|Header|   '(:complex (:seq* (:any))))
(define-xmp-element nil 'env:|Body|     '(:complex(:seq* (:any))))
(define-xmp-element nil 'env:|Fault|    '(:complex
					  (:set
					   (:element "faultcode"   enc:|QName|)
					   (:element "faultstring" enc:|string|)
					   (:element "faultactor" enc:|string|)
					   (:element "detail" (:complex (:seq* (:any))))
					   )))


(define-condition soap-client-error (xmp-client-condition) 
  ((xmp-error-code :initform 'env:|Client|)))
(define-condition soap-server-error (xmp-server-condition)
  ((xmp-error-code :initform 'env:|Server|)))
(define-condition soap-mismatch (xmp-condition)
  ((xmp-error-code :initform 'env:|VersionMismatch|)))
(define-condition soap-must-understand (xmp-condition)
  ((xmp-error-code :initform 'env:|MustUnderstand|)))

(defmethod soap-client-error ((conn soap-connector) &rest keys)
  (apply 'xmp-error conn 'soap-client-error keys))
(defmethod soap-server-error ((conn soap-connector) &rest keys)
  (apply 'xmp-error conn 'soap-server-error keys))






(defun soap-message-client (&rest options)
  (apply 'xmp-make-connector :soap :aserve :client :string :string options))

(defmethod xmp-make-connector ((protocol  (eql :soap)) 
			       (transport (eql :aserve))
			       (role      (eql :client))
			       (receiver  (eql :string)) 
			       (sender    (eql :string)) 
			       &rest options  &key &allow-other-keys)
  (apply 'make-instance 'soap-aserve-client-string-in-out-connector 
	 options))

(defmethod soap-add-header ((conn soap-connector) header &key after before reset)
  (let ((all (soap-headers conn)))
    (when reset
      (setf (soap-headers conn) (setf all nil)))
    (cond ((null all)
	   (setf (soap-headers conn) (setf all (list header))))
	  (t
	   (do ((tail all (cdr tail)))
	       ((atom tail))
	     (when (cond (after   (eq (car tail) after))
			 (before  (cond ((eq (car tail) before)
					 (setf (soap-headers conn) 
					       (setf all (cons header all)))
					 (return))
					((eq (cadr tail) before) t)))
			 ((null (cdr tail)) t))
	       (setf (cdr tail) (cons header (cdr tail)))
	       (return)))))
    header))
      

(defmethod xmp-object-class  ((conn soap-connector) (data t) (type t)
			      &rest options 
			      &key (class 'soap-element) &allow-other-keys)
  (declare (ignore options))
  class)

(defclass soap-element (xmp-element) ())
(defclass soap-header  (soap-element) ())
(defclass soap-fault   (soap-element) 
  ()  ;;;???
  )

   

(defmethod xmp-object-class  ((conn soap-connector) (data t) (type (eql 'env:|Header|))
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
    (setf (xmp-element-type data) 'env:|Header|
	  (xmp-element-tag1 data) 'env:|Header|)
    (setf (xmp-element-content data) (xmp-message-string tc))
    data))


(defmethod soap-find-type ((conn t) name nss)
  (let* ((def-name (xmp-decode-qualified-name conn name nss))
	 (def (xmp-find-type conn def-name nss)))
    (values def def-name)))

(defmethod soap-find-element ((conn t) name nss)
  (let* ((def-name (xmp-decode-qualified-name conn name nss))
	 (def (xmp-find-element conn def-name nss)))
    (values def def-name)))

(defmethod soap-resolve-type ((conn t) type nss &aux def)
  ;; argument must be type name
  ;; returns nil or type-def and name of def
  (loop
   (setf def (soap-find-type conn type nss))
   (cond ((null def) (return nil))
	 (t  (or (ecase (first def)
		   (:simple (when (second def) (setf type (second def))))
		   (:array nil)
		   (:complex nil))
		 (return (values def type)))))))

(defmethod soap-array-elements ((conn t) type-def)
  (if type-def
      `(:seq* (:element nil ,(second type-def)))
    '(:seq* (:any))))


(defmethod define-soap-type ((conn t) &rest options)
  (apply 'define-xmp-type conn options))

(defmethod define-soap-element ((conn t) &rest options)
  (apply 'define-xmp-element conn options))


(defvar *soap-server* nil)
(defmethod soap-encode-object ((conn null) name type data)
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

(defmethod soap-encode-element ((conn soap-connector)
				element parts
				&key type name
				&aux dname type-def st std as)
  (etypecase element
    (cons (ecase (first element) (:element nil))
	  (setf type-def (third element)
		name (or name (xmp-pick-name conn element))))
    ((or string symbol)
     (multiple-value-setq (type-def dname)
       (soap-find-element conn element :out))
     (or name (setf name dname))
     (when type (setf type-def type)))
    )
  (or type-def (xmp-error conn :def :string (list "Cannot find ~A" element)))
  (or name (xmp-error conn :def :string
		      (list "Cannot encode un-named element ~A" element)))

  (xmp-encode-begin conn name
		    :namespaces (xmp-getf-in-part conn type-def :namespaces)
		    :attributes 
		    (merge-plists 

		     (when (setf st (xmp-getf-in-part conn type-def :encoding))
		       ;; env:|encodingStyle| can appear in any element
		       (list 'env:|encodingStyle| st))
		     (when (setf st (xmp-getf-in-part conn type-def :send-type))
		       (list 'xsi:|type| st))
		     (when (setf st (xmp-getf-in-part conn type-def :send-atype))
		       (list 'enc:|arrayType| 
			     (if (setf as (xmp-getf-in-part conn type-def :send-asize))
				 (concatenate
				  'string
				  (xmp-encoded-qualified-name conn st :out)
				  "["
				  (format nil "~A"
					  (typecase as
					    (number (truncate as))
					    ;; (cons "(dim ...) ???")
					    (otherwise (length parts))))
				  "]")
			       st)))
		     (when (and (setf st (xmp-getf-in-part
					  conn type-def :must-understand
					  (setf std (list nil))))
				(not (eq st std)))
		       (list 'env:|mustUnderstand| (if st "1" "0")))
		     (xmp-getf-in-part conn type-def :attributes)))

  (soap-encode-parts conn parts name type-def)
  (xmp-encode-end conn name)
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
			      type-class cdef v type-res res
			      aparts elt pattern attr send pname ptype pattr parg)
  ;; return type of encoded part
  (cond ((consp type-def)
	 (setf type-class (first type-def)
	       type-res (or (second type-def) name)))
	((setf v (soap-resolve-type conn type-def :out))
	 (setf type-class (first v)
	       type-res (or (second v) type-def))
	 (setf type-def v))
	(t (setf type-class :simple type-res type-def)
	   (setf type-def nil)))
  (ecase type-class
    (:simple
     (if (consp parts)
	 (dolist (part parts res)
	   (setf res (xmp-encode conn part type-res)))
       (xmp-encode conn parts type-res)))
    (:array
     (setf aparts (xmp-getf-in-part conn type-def :array-item))
     (setf elt (or (getf aparts :element) (if (symbolp type-res) type-res "item")))
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
					 :type (or ptype type-res))))
	 ((and (consp pname) (third pname) (null ptype) (null pattr) (null send))
	  (setf res (soap-encode-element conn pname parg)))
	 (t
	  (etypecase pname
	    (cons (setf pname (append pname nil)))
	    ((member nil) (setf pname (list :element nil nil)))
	    ((or string symbol) (setf pname (list :element pname nil))))
	  (or (second pname)
	      (setf (second pname) (if (symbolp type-res) type-res "item")))
	  (or (third pname)
	      (setf (third pname) (or ptype type-res)))
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
	      (when send
		(setf (getf (cddr ptl) :send-type) (or ptype type-res)))))
	  (setf res (soap-encode-element conn pname parg))
	  )))))
    (:complex
     ;; preserve the order of elements in the definition
     ;;  but allow arguments in any order
     ;; undefined elements in input will be ignored
     ;; elements must be unique
     (setf cdef type-res)
     (labels ((propagate (name arg key defs opts)
			 (multiple-value-bind (n2 a2)
			     (encode key defs opts)
			   (cond ((and n2 a2) (values n2 a2))
				 ((and name arg) (values name arg))
				 (n2 (values n2 nil))
				 (t (values name arg)))))	      
	      (encode (key defs opts &aux name arg (def '#:none) prop)
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
			   (multiple-value-bind (type-def name)
			       (soap-find-element conn (first parts) :out)
			     (if type-def
				 (soap-encode-element
				  conn name (second parts) :name name)
			       (soap-encode-element
				conn name (second parts) :name name
				:type 'enc:|string|)))
			   (setf parts (cddr parts)))
			 (values (or name :any) arg))
			(:element
			 (cond ((and (consp defs) (null (second defs)))
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
			       (t (typecase defs
				    (cons (setf name (xmp-pick-name conn defs)))
				    (otherwise (setf name defs)))
				  (setf arg def)
				  (when (setf prop (xmp-elt-getf-name conn parts defs))
				    (setf name prop)
				    (setf arg (getf parts prop def))
				    (when (not (eq arg def))
				      (soap-encode-element conn defs arg :name prop)
				      (drop prop)))
				  (values name (when name (not (eq arg def)))))))
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
       (encode (first cdef) (cdr cdef) (cddr type-def)))



     )))


	  



(defmethod soap-encode-message ((conn soap-connector) method args &aux def)
  (xmp-message-begin conn)
  (xmp-encode-content conn
		      (format nil
			      "<?~A~A?>" 
			      (xmp-destination-leader conn)
			      (if (xmp-xml-encoding conn)
				  (format nil " encoding=\"~A\""
					  (etypecase (xmp-xml-encoding conn)
					    (cons (second (xmp-xml-encoding conn)))
					    (symbol (xmp-xml-encoding conn))))
				""))
		      :sanitize nil)
  (xmp-encode-begin conn 'env:|Envelope|
		    :namespaces (xmp-message-dns conn)
		    :attributes 
		    (when (soap-encoding-style conn)
		      (list 'env:|encodingStyle| (soap-encoding-style conn)))
		    )
  (dolist (h (soap-headers conn))
    (xmp-encode conn h nil))
  (xmp-encode-begin conn 'env:|Body|)
  (etypecase method
    (xmp-element
     (xmp-encode conn method nil)
     (setf def (xmp-element-type method)
	   method (xmp-element-name method)
	   ))
    ((or string symbol cons)
     (multiple-value-setq (method def)
       (soap-encode-element
	conn (xmp-normal-element-spec conn method :out) args))))
  (xmp-encode-end conn 'env:|Body|)
  (xmp-encode-end conn 'env:|Envelope|)
  (values method def (xmp-message-string conn)))

(defmethod call-soap-method ((conn soap-client-connector)
			    method &rest args)

  ;; method -> element-name | element-def
  ;; args -> [sub-element-name value]...
  (let ((res (apply 'xmp-call-method conn method args)))
    (typecase res
      (cons (case (first res)
	      (:envelope
	       (values (second (assoc :body (cdr res)))
		       (cdr (assoc :headers (cdr res)))))
	      (otherwise res)))
      (otherwise res))))

(defmethod xmp-call-method ((conn soap-client-connector)
			    method &rest args)
  (multiple-value-bind (name def)
      (soap-encode-message conn method args)
    (declare (ignore name))
    (xmp-decode-message 
     conn
     (let* ((action (xmp-getf-in-part conn def :action))
	    (reply (xmp-message-send
		    conn
		    :headers 
		    (when action
		      `((:|SOAPAction| . ,action)))
		    )))
       (xmp-parse-message conn reply
			  :namespaces (xmp-getf-in-part conn def :namespaces)
			  )))))


(defmethod xmp-begin-message ((conn soap-string-in-connector))
  (setf (soap-undef-list conn) nil)
  (list :seq1 'env:|Envelope|))

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
		   (xmp-decode-qualified-name conn (subseq x 0 b0) nss))))
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
  ;;   Type          (as declared in a xsi:|type| attribute
  ;;   arrayType     derived from Type or declared
  ;;   res           type-def of Type or arrayType

  (and (setf type (getf attributes 'xsi:|type|))
       (setf type (xmp-decode-qualified-name conn type nss))
       (setf res (soap-resolve-type conn type nss)))
  (when (setf x (getf attributes 'enc:|arrayType|))
    (multiple-value-setq (atype rank length) (soap-parse-array-type conn x nss))
    (setf res (list :array atype :rank rank :length length))
    )
  (when (and atype (setf x (getf attributes 'enc:|offset|)))
    (multiple-value-setq (x x x)
      (soap-parse-array-type conn x nss))
    (when (and x (not (member nil x)))
      (setf res (append (list (first res) (second res)
			      :offset x)
			(cddr res)))))    
  (values type atype res))


(defmethod xmp-warning-leader ((conn soap-connector)) "SOAP Warning")
(defmethod soap-decode-note ((conn t) bool fmt &rest args)
  (or conn (setf conn "SOAP Warning"))
  (or bool
      (case (soap-decode-flag conn)
	(:strict (soap-client-error conn :string 
				    (if args (list* fmt args) fmt)))
	(:warn (apply 'xmp-warning conn fmt args)))))

(defmethod soap-exel-and-type ((conn soap-connector) elt attributes
			       &aux dt exel type atype res nx tx tn dn ad)
  (multiple-value-setq (exel dt dn ad) (xmp-defined-element-defs conn elt :in 0))
  (setf tn dt)
  (multiple-value-setq (type atype res) (soap-decode-type conn attributes :in))
  (cond (atype (setf tn (list :array atype) nx (soap-array-elements conn res)))
	(type (setf tn type)
	      (when res
		(setf nx (ecase (first res)
			   (:simple (setf tx type) (xmp-simple-exel conn res))
			   (:array (soap-array-elements conn res))
			   (:complex (second res))))))
	(t (setf nx exel tx elt)))
  (values exel nx (or dt tx) tn dn ad))

(defmethod soap-match-types ((conn soap-connector) t1 t2)
  (or (null t1)
      (null t2)
      (equal t1 t2)
      (equal t1 '(:seq* (:any)))
      (equal t2 '(:seq* (:any)))
      (eq t1 'xsd:|ur-type|)
      (eq t2 'xsd:|ur-type|)
      (eq t1 'xsd:|anyType|)
      (eq t2 'xsd:|anyType|)
      ))

(defmethod xmp-begin-element ((conn soap-string-in-connector) (elt t)
			      &rest options &key attributes &allow-other-keys
			      &aux exel nx dt tn dn)
  (multiple-value-setq (exel nx dt tn dn) (soap-exel-and-type conn elt attributes))
  (cond (dn)
	((member elt (soap-undef-list conn)))
	(t (push elt (soap-undef-list conn))
	   (soap-decode-note conn dn "Undefined element ~S" elt)))
  (cond (nx
	 (soap-decode-note
	  conn (soap-match-types conn exel nx)
	  "Type mismatch in element ~S: def=~S attr=~S."
	  elt exel nx))
	((eq dt elt)
	 (cond ((null dn)
		;; If element was undefined, dont emit a 
		;;    redundant warning.
		)
	       (tn (soap-decode-note conn nil
				     "Unknown type (a) ~S in ~S."
				     tn elt))
	       (t  (soap-decode-note conn nil
				     "Unspecified type in ~S." elt))))
	((and dt tn)
	 (if (eq dt tn)
	     (soap-decode-note conn nil "Unknown type (b) ~S in ~S." tn elt)
	   (soap-decode-note conn nil "Unknown type (c) ~S/~S in ~S." tn dt elt)))
	(tn
	 (soap-decode-note conn nil "Unknown type (d) ~S in ~S." tn elt))
	(t (soap-decode-note conn nil "Unknown type (e) ~S in ~S." dt elt)))
  (or nx (call-next-method)))

(defmethod xmp-begin-element :after ((conn soap-string-in-connector) elt
				     &rest options &key attributes &allow-other-keys
				     &aux (depth (xmp-in-depth conn)) 
				     actor this-actor
				     )
  (declare (ignore options))
  (when (eql 1 depth)
    (if (equal "Envelope" (string elt))
	(or (eq elt 'env:|Envelope|)
	    (xmp-error conn 'soap-mismatch))
      (soap-client-error conn :string "Ill-formed message.")))
  (when (and (eql 3 depth)
	     (or (equal "1" (getf attributes 'env:|mustUnderstand|))
		 (equal "true" (getf attributes 'env:|mustUnderstand|)))
	     (equal 'env:|Header| (first (second (xmp-inside-elements conn))))
	     (or (null (setf actor (getf attributes 'env:|actor|)))
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

(defmethod xmp-encode :around ((conn soap-connector) (data t) (elt null)
			       &rest options 
			       &key &allow-other-keys)
  (declare (ignore options))
  (case (soap-null-element conn)
    (:empty nil)
    (otherwise (call-next-method))))


(defmacro def-soap-simple-content (elt &key class keywords
				       (decode-class class) (decode-keys keywords)
				       decode
				       (encode-class class) (encode-keys keywords)
				       encode)
  (or class
      (and (if decode decode-class t) (if encode encode-class t))
      (error "def-soap-simple-content missing class"))
  `(progn
     ,@(when (consp elt)
	 (do ((tl elt (cdr tl)) res)
	     ((atom tl) (reverse res))
	   (setf elt (first tl))
	   (when (cdr tl)
	     (push `(define-soap-type nil ',elt '(:simple ,(second tl)))
		   res))))
     (define-soap-type nil ',elt '(:simple  nil :simple-content-key ,elt))
     ,@(when decode
	 `((defmethod xmp-simple-content ((conn ,decode-class) (elt (eql ',elt)) data
					  &rest options 
					  &key ,@decode-keys &allow-other-keys)
	     ,@decode)))
     ,@(when encode
	 `((defmethod xmp-encode ((conn ,encode-class) data (elt (eql ',elt))
				  &rest options 
				  &key ,@encode-keys &allow-other-keys)
	     ,@encode)))))

(define-soap-type nil 'xsd:|ur-type| '(:complex (:seq* (:any))))
(define-soap-type nil 'xsd:|anyType| '(:complex (:seq* (:any))))

(def-soap-simple-content enc:|QName|
  :class soap-connector
  :decode ((declare (ignore options))
	   (xmp-decode-qualified-name conn data :in)))


(def-soap-simple-content (enc:|string| xsd:|string|)
  :class soap-connector
  :decode ((declare (ignore options))
	   data)
  :encode ((declare (ignore options))
	   (xmp-encode-content conn
			       (typecase data
				 (string data)
				 (null   "")
				 (otherwise (format nil "~A" data))))
	   'xsd:|string|))
  

(def-soap-simple-content (enc:|decimal| xsd:|decimal|)
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-decimal data :fraction t))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (xmp-encode-content 
	    conn 
	    (let ((int (truncate data)))
	      (typecase data
		(null "0")
		(integer (format nil "~D" int))
		(float (xmp-encode conn data 'xsd:|double|))
		(ratio (format-ratio data))
		))
	    'xsd:|decimal|)))

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

(def-soap-simple-content (enc:|int| xsd:|int|)
  :class soap-connector
  :decode ((declare (ignore options))
	   (values (or (when (null data) 0)
		       (ignore-errors (parse-integer data))
		       (call-next-method))))
  :encode ((declare (ignore options))
	   (xmp-encode-content conn
			       (if (null data)
				   "0"
				 (format nil "~A" (truncate data))))
	   'xsd:|int|))

(def-soap-simple-content (enc:|boolean| xsd:|boolean|)
  :class soap-connector
  :decode ((declare (ignore options))
	   (cond ((equal data "0") nil)
		 ((equal data "false") nil)
		 ((equal data "1") t)
		 ((equal data "true") t)
		 (t (soap-client-error conn :string "Boolean value is not valid."))))
  :encode ((declare (ignore options))
	   (xmp-encode-content conn (if data "true" "false"))
	   'xsd:|boolean|))

(def-soap-simple-content (enc:|float| xsd:|float|)
  :class soap-connector
  :decode ((declare (ignore options))
	   
	   ;; Parse the input as a double float to retain excess precision 
	   ;;  if it is there (some clients/servers ie soapware.org/validator1
	   ;;  seem to send and expect double precision for xsd:float )
	   (values (if (null data)
		       0
		     (read-from-string (case (length data)
					 ((0 1 2 3 4 5 6 7 8 9 10)
					  data)
					 (otherwise		    
					  (concatenate 'string data "d0")))
				       nil nil))
		   'xsd:|float|))
  :encode ((declare (ignore options))
	   (let (f d)
	     (typecase data
	       (null (setf f 0.0 d 12))
	       (double-float (setf f data d 22))
	       (single-float (setf f data d 12))
	       (otherwise (setf f (coerce data 'single-float) d 12)))
	     (xmp-encode-content
	      conn
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
		       f)))
	     'xsd:|float|)))

(def-soap-simple-content (enc:|double| xsd:|double|)
  :class soap-connector
  :decode ((declare (ignore options))
	   ;; Make sure that the data will be parsed as a double-float
	   (values 
	    (if (null data)
		0
	      (read-from-string (concatenate 'string data "d0") nil nil))))
  :encode ((declare (ignore options))
	   (let ((f (coerce (or data 0) 'double-float)))
	     (xmp-encode-content
	      conn
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
		       f)))
	     'xsd:|double|)))

(def-soap-simple-content (enc:|base64| enc:|base64Binary| xsd:|base64Binary|)
  :class soap-connector
  :decode ((declare (ignore options))
	   (decode-base64-string data))
  :encode ((declare (ignore options))
	   (xmp-encode-content conn (encode-base64-string data))
	   'xsd:|base64Binary|))


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
				(elt (eql 'env:|Envelope|)) data
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
				(elt (eql 'env:|Header|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (list (list* :header data)))

(defmethod xmp-complex-content ((conn soap-string-in-connector) 
				      (elt (eql 'env:|Body|)) data
				      &rest options &key &allow-other-keys)
  (declare (ignore options))

  (cond ((null data) (soap-client-error conn :string "Empty Body"))
	((cdr data)  (soap-client-error conn :string "Too many elements in Body")))

  (list (list* :body data)))


(defun fill-array (array indices rank data)
  (loop
   (cond ((null data) (return))
	 ((do ((i indices (cdr i)) (j rank (cdr j)))
	      ((atom i) t)
	    (or (< (first i) (first j)) (return nil)))
	  (setf (apply #'aref array indices) (soap-element-content (cdr (pop data))))
	  (bump-index indices rank))
	 (t (return)))))

(defun bump-index (indices rank &optional (j (1- (length rank))))
  ;; this is called only if every index is less than 
  ;;  the corresponding rank element
  (incf (elt indices j))
  (when (eql (elt indices j) (elt rank j))
    (when (> j 0)
      (setf (elt indices j) 0)
      (bump-index indices rank (1- j)))))


(defmethod xmp-complex-content ((conn soap-string-in-connector) 
				(elt t) data
				&rest options &key attributes &allow-other-keys
				&aux type atype outer res length array a2)
  (multiple-value-setq (type atype res) (soap-decode-type conn attributes :in))
  (cond ((or atype
	     (nth-value 3 (xmp-defined-element-defs conn elt :in 1)))
	 
	 ;; this code ignores position attributes???
	 ;;  maybe pos attributes could be saved in array element?

	 (when (member nil (setf length (xmp-getf-in-part conn res :length)))
	   (setf length nil))
	 (if length
	     (fill-array (setf array (make-array length))
			 (mapcar #'(lambda (x) (declare (ignore x)) 0) length)
			 length
			 data)
	   (setf array (concatenate 'vector (mapcar #'second data))))
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
  (soap-encode-parts dest data nil type))


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
	      'xsd:|string|)))))

(defmethod xmp-encode ((dest soap-connector) (data integer) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data 'xsd:|int| options))

(defmethod xmp-encode ((dest soap-connector) (data single-float) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data 'xsd:|double| options))

(defmethod xmp-encode ((dest soap-connector) (data double-float) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data 'xsd:|double| options))

(defmethod xmp-encode ((dest soap-connector) (data string) (type null)
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest data 'xsd:|string| options))





;;; SOAP Server


(defun soap-message-server (&rest keys &key start 
				  (enable :start)
				  publish 
				  (class 
				   'soap-aserve-server-string-in-out-connector)
				  &allow-other-keys)
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
      (:start (xmp-start-server server :enable t))
      (otherwise (xmp-enable-server server)))
    server))

(defmethod xmp-export-standard-methods ((server soap-server-connector)
					&key &allow-other-keys)
  t)


(defmethod xmp-server-response ((server soap-aserve-server-connector)
				&key request entity &allow-other-keys
				&aux action)
  (setf action (xmp-header-slot request :|SOAPAction|))
  (call-next-method server :request request :entity entity 
		    :options (list :action action)))  

(defmethod soap-invoke-method :around ((server soap-server-connector) (name t) (args t)
				       &key headers)
  (declare (ignore headers))
  (let ((*soap-server* server))
    (call-next-method)))

(defmethod soap-invoke-method ((server soap-server-connector) (name t) (args t)
			       &key headers)
  (declare (ignore headers))
  (xmp-invoke-method server name args))



(defvar *soap-last-server* nil)
(defmethod xmp-server-implementation ((server soap-aserve-server-connector) body
				      &rest options &key action &allow-other-keys)
  (declare (ignore options))
  ;; parse an rpc call and pass it to the exported function
  (when (xmp-server-enabled server)
    (setf *soap-last-server* server)
    (let* (code string rval)
      (multiple-value-bind (v e)
	  (ignore-errors
	    (let* ((r (xmp-decode-message
		       server
		       ;; xmp-parse-message automatically includes
		       ;;  message-dns namespaces
		       (xmp-parse-message server body)))
		   (body (second (assoc :body (cdr r))))
		   (headers (assoc :headers (cdr r)))
		   (method (first body))
		   (signature (list* action nil (mapcar #'car (cdr body))))
		   (params (mapcar
			    #'(lambda (x) (soap-element-content (cdr x)))
			    (cdr body)))
		   )

	      (setf params (mapcan #'list (cddr signature) params))
	      (multiple-value-bind (fn rt)
		  (xmp-accept-method server method signature params)
		(if (null fn)
		    (xmp-error server :client :string "Undefined method")
		  (let ((vals (multiple-value-list
			       (soap-invoke-method server fn params
						   :headers headers))))
		    (if (null vals)
			(xmp-error server :client :string "Call refused")
		      (let (relt rtype)
			(etypecase rt
			  (symbol
			   ;; must be element name
			   (setf relt rt rtype (soap-find-element server rt :out)))
			  (cons (ecase (first rt)
				  (:element (setf relt (xmp-pick-name server rt)
						  rtype (third rt))))))
			(setf rval (soap-encode-object
				    server relt rtype (first vals))))))
		  ))
	      nil))
	(declare (ignore v))
	(typecase e
	  (xmp-condition (setf rval (soap-make-fault
				 server
				 (xmp-fault-code e)
				 (xmp-fault-string e)
				 :sub-code (xmp-fault-sub-code e)
				 :factor (xmp-fault-factor e)
				 :detail (xmp-fault-detail e))))
	  ((member nil)
	   (when code
	     (setf rval (soap-make-fault server code string))))
	  (otherwise
	   (setf rval (soap-make-fault server :LispError (format nil "~A" e)))))
	(soap-encode-message server rval nil)
	))))

(defmethod xmp-signature-equal ((conn soap-connector) ss1 ss2
				&aux (s1 ss1) (s2 ss2) k1 k2 lswap pc)
  ;; SOAP  sig -> (action collector elt-name ... )
  ;;    action -> string
  ;; collector -> nil  :seq  :seq1  :set  :set1
  ;;  elt-name -> symbol  string  (:any-case string) (:any)
  (cond
   ((or (atom ss1) (atom ss2)) nil)
   ((and (second ss1) (null (second ss2))) (xmp-signature-equal conn ss2 ss1))
   ((and (first ss2) (not (equal (first ss1) (first ss2)))) nil)
   ((null (second ss2))
    (xmp-error conn :internal :string "Comparing two candidate signatures 1."))
   (t (pop s1) (pop s2)
      (setf k1 (pop s1) k2 (pop s2))
      (cond ((and k1 (null k2)) (xmp-signature-equal conn ss2 ss1))
	    ((null k2)
	     (xmp-error conn :internal :string "Comparing two candidate signatures 2."))
	    ((case k1
	       ((nil) t)
	       (:seq (case k2
		       (:seq (setf lswap t pc t))
		       (:seq1 (rotatef k1 k2) (setf lswap t pc t))
		       (:set (setf lswap t pc t))
		       (:set1 (rotatef k1 k2) (setf lswap t pc t))
		       (otherwise
			(xmp-error conn :internal :string "Ill-formed signature 1."))))
	       (:seq1 (case k2
			(:seq (setf lswap t pc t))
			(:seq1 t)
			(:set (setf lswap t pc t))
			(:set1 t)
			(otherwise
			 (xmp-error conn :internal :string "Ill-formed signature 2."))))
	       (:set  (case k2
			(:seq (rotatef k1 k2) (setf lswap t pc t))
			(:seq1 (rotatef k1 k2) (setf lswap t pc t))
			(:set (setf lswap t pc t))
			(:set1 (rotatef k1 k2) (setf lswap t pc t))
			(otherwise
			 (xmp-error conn :internal
				    :string "Ill-formed signature 3.")))  )
	       (:set1  (case k2
			 (:seq (setf lswap t pc t))
			 (:seq1 (rotatef k1 k2) t)
			 (:set (rotatef k1 k2) (setf lswap t pc t))
			 (:set1 t)
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
	       (when (ecase k2
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
		 (cond ((null pc) t)
		       ((and (eq k1 k2)
			     (eql (length s1) (length s2)))
			t)
		       (t (xmp-error conn :def
				     :string "Conflicting signatures.")))
		 )))))))



(defmethod soap-make-fault ((server soap-server-connector) code string
			    &key factor detail sub-code)
  (soap-encode-object server 
		      'env:|Fault|
		      nil
		      `(:|faultcode| 
			 ,(if sub-code
			     (intern
			      (format nil "~A.~A" code sub-code)
			      (symbol-package code))
			   code)
			 :|faultstring| ,string
			 ,@(when factor (list :|faultactor| factor))
			 ,@(when detail (list :|detail| detail)))
		      ))



(defmethod soap-export-method ((conn soap-server-connector) name sig
			       &rest keys
			       &key lisp-name (enable t) return help
			       (action nil a-p) ordered exact
			       &allow-other-keys)
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

(defun soap-sub-element-content (v name)
  (and (consp v)
       (consp (first v))
       (setf v (assoc name v))
       (soap-element-content (cdr v))))

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


