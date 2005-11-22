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

;; $Id: xmp-wsdl.cl,v 2.9 2005/11/22 22:25:26 mm Exp $

;; WSDL support

;;; targetNamespace attribute in wsdl:definitions
;;;    applies to message port and service names
;;; namespace attribute in soap:body
;;;    applies to operation names     
;;; elements in message are not in any namespace

;;; style=rpc  operation name is method name
;;;            each part has name and type attrs, represents one parameter element
;;;            use="encoded" encodingStyle="required"     .NET
;;;                SOAPScope gives warning but still works
;;;
;;; style=document there should be only one part
;;;            each part has name and element attrs, represents one Body element
;;;            use="literal"

(in-package :net.xmp.soap)

(defpackage :net.xmp.soap 
  (:export 
   #:decode-wsdl-file
   #:decode-wsdl-string
   #:decode-wsdl-at-uri
   #:decode-wsdl-source
   #:decode-wsdl-namespaces
   #:encode-wsdl-file
   #:wsdl-service-names
   #:make-client-interface
   #:make-server-interface
   #:describe-soap-method
   #:describe-soap-element
   #:describe-soap-type

   #:*wsdl-debug*

   #:wsdl-post-process
   #:wsdl-props
   #:wsdl-maybe-conflicts
   #:wsdl-compose-name
   #:writer-prefix
   #:reader-prefix
   #:soap-object-class
   #:wsdl-file-connector
   #:wsdl-object-classes
   #:items
   #:wsdl-generate-code

   ))

(eval-when (compile load eval)
  (defpackage :net.xmp.wsdl
    (:use)
    (:export 
     #:|definitions|
     #:|documentation|
     #:|include|
     #:|import|
     #:|types|
     #:|message|
     #:|interface|
     #:|portType|     ;;; WSDL 1.1 (deprecated in 1.2, use interface?)
     #:|binding|
     #:|service|
     #:|operation|
     #:|input|
     #:|output|
     #:|part|
     #:|arrayType|
     #:|style|
     #:|transport|
     #:|soapAction|
     #:|use|
     #:|encodingStyle|
     #:|namespace|
     #:|port|
     #:|name|
     #:|location|
     #:|fault|
     ;; #:||
 
     ))

  (defpackage :net.xmp.wsdl.soap
    (:use)
    (:export
     #:|binding|
     #:|operation|
     #:|body|
     #:|address|
     #:|fault|
     #:|header|
     ;; #:||
     ))

  (defpackage :net.xmp.wsdl.mime
    (:use))

  (defpackage :net.xmp.wsdl.http
    (:use))

  (defpackage :net.xmp.wsdl.msstk
    (:use))

  )

(eval-when (compile)
  (defpackage :net.xmp.wsdl (:use) (:nicknames :wsdl))
  (defpackage :net.xmp.wsdl.soap (:use) (:nicknames :wsoap))
  )

(defvar *wsdl-debug* nil)

(defun define-wsdl-names ()
  (define-namespace :net.xmp.wsdl "wsdl" "http://schemas.xmlsoap.org/wsdl/")
  (xmp-define-namespace-map 
   :wsdl1.1 t
   (list
    nil ;;; no default
    ;;
    ;; From: Web Services Description Language (WSDL) 1.1
    ;;       W3C Note 15 March 2001
    ;;       This version: http://www.w3.org/TR/2001/NOTE-wsdl-20010315
    ;;

    '(:net.xmp.wsdl
      "wsdl"
      "http://schemas.xmlsoap.org/wsdl/"
      ;; WSDL namespace for WSDL framework.
      )
    '(:net.xmp.wsdl.soap
      "soap"
      "http://schemas.xmlsoap.org/wsdl/soap/"
      ;; WSDL namespace for WSDL SOAP binding.
      )
    ;; http http://schemas.xmlsoap.org/wsdl/http/ 
    ;;      WSDL namespace for WSDL HTTP GET & POST binding.
    '(:net.xmp.wsdl.http "http" "http://schemas.xmlsoap.org/wsdl/http/")

    ;; mime http://schemas.xmlsoap.org/wsdl/mime/
    ;;      WSDL namespace for WSDL MIME binding.
    '(:net.xmp.wsdl.mime "mime" "http://schemas.xmlsoap.org/wsdl/mime/")
    
    :soap1

    ;; tns (various)
    ;; The 'this namespace' (tns) prefix is used as a convention
    ;;     to refer to the current document.
    ;; (other) (various)
    ;; All other namespace prefixes are samples only. 
    ;;     In particular, URIs starting with 'http:// example.com'
    ;;     represent some application- dependent or context-dependent URI [4].
    ))

  ;;???(define-namespace :net.xmp.wsdl "wsdl" "http://www.w3.org/2003/06/wsdl")
  (xmp-define-namespace-map
   :wsdl1.2 t
   (list
    nil
    ;;
    ;; From: Web Services Description Language (WSDL)
    ;;       Version 1.2 Part 1: Core Language
    ;;       W3C Working Draft 11 June 2003
    ;;       This version: http://www.w3.org/TR/2003/WD-wsdl12-20030611
    ;;

    '(:net.xmp.wsdl
      "wsdl"
      "http://www.w3.org/2003/06/wsdl"
      ;; A normative XML Schema [XML Schema: Structures], 
      ;;   [XML Schema: Datatypes] document for the
      ;;   "http://www.w3.org/2003/06/wsdl" namespace can be 
      ;;   found at http://www.w3.org/2003/06/wsdl.
      ;;   WSDL documents that do NOT conform to this schema are
      ;;   not valid WSDL documents. WSDL documents that DO conform to
      ;;   this schema and also conform to the other constraints defined
      ;;   in this specification are valid WSDL documents.
      )
    '(:net.xmp.wsdl.soap
      "soap12"
      "http://www.w3.org/2003/06/wsdl/soap12"
      ;; Defined by WSDL 1.2: Bindings [WSDL 1.2 Bindings].
      )
    ;; http "http://www.w3.org/2003/06/wsdl/http"
    '(:net.xmp.wsdl.http "http" "http://www.w3.org/2003/06/wsdl/http")

    ;; mime "http://www.w3.org/2003/06/wsdl/mime"
    '(:net.xmp.wsdl.mime "mime" "http://www.w3.org/2003/06/wsdl/mime")
    
    :soap

    ))

  (xmp-define-namespace-map
   :wsdl-namespaces t
   (list
    nil
    :wsdl1.2
    :wsdl1.1
    '(:net.xmp.wsdl.msstk
      "stk"
      "http://schemas.microsoft.com/soap-toolkit/wsdl-extension")
    ))
  (xmp-define-namespace-map
   :wsdl1-namespaces t
   (list
    nil
    :wsdl1.1
    :wsdl1.2
    '(:net.xmp.wsdl.msstk
      "stk"
      "http://schemas.microsoft.com/soap-toolkit/wsdl-extension")
    ))

  (xmp-define-namespace-map :wsdl-combine t
			    '(nil :wsdl-namespaces :all (:wsdl-def "tns" :any)))
  (xmp-define-namespace-map :wsdl-keyword t
			    '(nil :wsdl-namespaces :all (:keyword "tns" :any)))
  (xmp-define-namespace-map :wsdl-prefix  t
			    '(nil :wsdl-namespaces :all ("wsdl-" "tns" :prefix)))
  (xmp-define-namespace-map :wsdl1-combine t
			    '(nil :wsdl1-namespaces :all (:wsdl-def "tns" :any)))
  (xmp-define-namespace-map :wsdl1-keyword t
			    '(nil :wsdl1-namespaces :all (:keyword "tns" :any)))
  (xmp-define-namespace-map :wsdl1-prefix  t
			    '(nil :wsdl1-namespaces :all ("wsdl-" "tns" :prefix)))


  (define-xmp-element nil 'wsdl:|definitions|
    '(:complex
      (:seq
       (:set* wsdl:|include| "import")
       wsdl:|documentation|
       (:element wsdl:|types| (:complex (:seq* xsd:|schema| (:any))))
       (:set* wsdl:|message|
	      (:or wsdl:|interface| wsdl:|portType|)
	      wsdl:|binding|
	      wsdl:|service|)
       (:seq* (:any)))))

  )
(define-wsdl-names)

(defun wsdl-base-namespaces (argnse)
  (if (and argnse (atom argnse))
      argnse
    (do ((tl argnse (cdr tl)) nsd (nse (list nil)))
	((atom tl) (append nse (list :wsdl1-prefix)))
      (etypecase (setf nsd (first tl))
	(null nil)
	(symbol (setf nse (append nse (list nsd))))
	(cons (case (third nsd)
		((:any :other :prefix)
		 (return (append nse (list :wsdl1-namespaces nsd))))
		(otherwise (setf nse (append nse (list nsd))))))))))


(defclass wsdl-file-connector (schema-file-connector)

  (
   (message-dns     :initform nil)
   (base-dns        :initform :wsdl1-namespaces)
   (trim-whitespace :initform t)
   (messages :accessor wsdl-messages :initform (list nil))
   (interfaces :accessor wsdl-interfaces :initform (list nil))
   (port-types :accessor wsdl-port-types :initform (list nil))
   (bindings :accessor wsdl-bindings :initform (list nil))
   (services :accessor wsdl-services :initform (list nil))
   
   (client-options :accessor wsdl-client-options
		   ;; combined client options from make-wsdl-interface args
		   ;; lisp-package null-element empty-element body-form
		   ;; connect-class->:class
		   :initform nil)

   (wsdl-options   :accessor wsdl-options
		   ;; a property list of additional options controlling
		   ;; code generation and/or wsdl decoding
		   :initform nil)

   (soap-style :accessor wsdl-soap-style :initform nil)
   (soap-address :accessor wsdl-soap-address :initform nil)
   (operations   :accessor wsdl-operations   :initform nil)
   (expand-singleton :accessor wsdl-expand-singleton :initform nil)

   (undef-ns   :accessor wsdl-undef-ns   :initform nil)
   (server-exports :accessor wsdl-server-exports :initform nil)

   (file-stream :accessor wsdl-file-stream :initform nil)
   (xml-leader    :initform "xml version=\"1.0\"")
   (wsdl-targets  :accessor wsdl-targets  :initform nil)
   (wsdl-warnings :accessor wsdl-warnings :initform nil)

   (defined-elements :accessor wsdl-defined-elements :initform nil)
   (defined-types    :accessor wsdl-defined-types    :initform nil)
   (target-package   :accessor wsdl-target-package    :initform nil)
   (counters         :accessor wsdl-counters         :initform nil)
   (name-prefix      :accessor wsdl-name-prefix      :initform nil)
   (file-package     :accessor wsdl-file-package     :initform :user)
   (def-list         :reader wsdl-def-list :accessor wsdl-wdef-list
                     ;; List of entries like (defining-form comment ... [symbol])
                     ;;  If symbol is present in the list (at the end), it is the 
                     ;;  name of the message handled by the defining form.
                     :initform nil)
   (sub-list         :accessor wsdl-sub-list
		     ;; This list is always updated in tandem with the 
		     ;; def-list slot, but it may be reset to nil intermittently
		     ;; in order to collect intermediate results.
		     :initform nil)
   (def-index        :accessor wsdl-def-index  
                     ;; An alist ((definer key ...) ...)
                     :initform nil)
   (map-name         :accessor wsdl-map-name         :initform nil)
   (url-name         :accessor wsdl-url-name         :initform nil)
   (maybe-conflicts  :accessor wsdl-maybe-conflicts  :initform :unknown)
   (props            :accessor wsdl-props            :initform nil)
   (compose-strategy :accessor wsdl-compose-strategy :initform nil)
   (encoder-name     :accessor wsdl-encoder-name     :initform nil)
   (object-classes   :accessor wsdl-object-classes
		     ;; collect defclass names generated in wsdl-emit-object-classes
		     :initform nil)
   (overloaded-ops   :accessor wsdl-overloaded-ops   :initform nil)
   ))


(defmethod wsdl-option ((conn wsdl-file-connector) name)
  (getf (wsdl-options conn) name))

(defmethod xmp-warning-leader ((conn wsdl-file-connector)) "WSDL")

(defun decode-wsdl-file (file &rest keys)
  (apply #'decode-wsdl-source :file file keys))
(defun decode-wsdl-string (string &rest keys)
  (apply #'decode-wsdl-source :string string keys))
(defun decode-wsdl-at-uri (uri &rest keys)
  (apply #'decode-wsdl-source :uri uri keys))

(defun decode-wsdl-source (&key uri string stream file 
				(namespaces :decode)
				(base :wsdl1-prefix)
				(lisp-package :keyword)
				xml-syntax init (class 'wsdl-file-connector)
				&aux (*xmp-warning-leader* "WSDL"))
  

  ;; namespaces -> :decode | namespace-map

  (when uri 
    (multiple-value-bind (body rc h ruri)
	(net.aserve.client:do-http-request uri)
      (if (and body (eql rc 200))
	  (setf string body)
	(error "URI ~A returned error ~S ~S ~S"
	       uri rc h ruri))))
  (or file string stream (error "No source specified."))      
  (let* ((base-dns (wsdl-base-namespaces base))
	 (dns (case namespaces
		(:decode
		 (or
		  file string
		  (error
		   "When source is a stream, namespaces argument cannot be :decode."))
		 (multiple-value-bind (ns other ambi missing)
		     (decode-wsdl-namespaces
		      :file file :string string :map base-dns)
		   (if (and (null other) (null ambi) (null missing))
		       ns
		     (return-from decode-wsdl-source
		       (values 
			nil
			(when other
			  (cons "Unrecognized namespaces:" other))
			(when ambi
			  (cons "Ambiguous URIs containing:" ambi))
			(when missing
			  (cons "Missing namespaces:" missing))
			)))))
		(otherwise namespaces)))
	 (conn (apply #'make-instance class
		      :message-dns dns
		      :base-dns base-dns
		      :xml-syntax xml-syntax
		      :source file :lisp-package lisp-package
		      init)))
    (or (typep conn 'wsdl-file-connector)
	(error "Class of ~S is not sub-class of wsdl-file-connector" conn))
    (values-list
     (append
      (list conn)
      (multiple-value-list 
       (cond (string (xmp-decode-string conn string))
	     (file   (xmp-decode-file conn file))
	     (stream (xmp-decode-stream conn stream))))))
    ))

(defmethod schema-decode-attribute ((conn wsdl-file-connector)
				    name value nss &aux attr kwd)
  (setf attr name)
  (setf kwd (intern (string attr) :keyword))
  (case attr
    ((wsdl:|arrayType|)
     (values attr (xmp-decode-qualified-name conn value nss :suppress-default t)))
    (otherwise 
     (case kwd
       ((:|binding| :|message| :|element|) 
	(values attr (xmp-decode-qualified-name conn value nss :suppress-default t)))
       (otherwise
	(call-next-method conn attr value nss))))))


(defmethod xmp-begin-message ((conn wsdl-file-connector))
  (list :seq1 'wsdl:|definitions|))

(defmethod xmp-end-message ((conn wsdl-file-connector) data
			    &key types &allow-other-keys)
  (values data types))






(defmethod xmp-simple-content ((conn wsdl-file-connector)
			       (elt (eql 'wsdl:|documentation|)) data
			       &rest options 
			       &key &allow-other-keys)
  (declare (ignore options))
  (list :documentation data))

(define-schema-default-part
  wsdl-file-connector wsdl:|documentation| :documentation :cclass schema-text-component)

(define-schema-default-part wsdl-file-connector wsdl:|definitions| :definitions)

(define-schema-default-part wsdl-file-connector wsdl:|types| :types)
  

     
(defmethod schema-collect-target :around ((conn wsdl-file-connector) attributes)
  (declare (ignore attributes))
  (let ((found (call-next-method)))
    (when found
      (pushnew found (wsdl-targets conn) :test #'same-uri)
      found)))

(defmethod xmp-decode-element :around ((conn wsdl-file-connector)
				      (elt (eql 'wsdl:|definitions|)) (data t)
				      &rest options
				      &key attributes &allow-other-keys)
  (let ((found (schema-collect-target conn attributes)))
    (multiple-value-prog1
     (call-next-method)
     (when found (pop (schema-target conn))))))


(define-schema-collected-part
  wsdl-file-connector wsdl:|message|   :message   wsdl-messages)
(define-schema-collected-part
  wsdl-file-connector wsdl:|interface| :interface wsdl-interfaces)
(define-schema-collected-part
  wsdl-file-connector wsdl:|portType|  :port-type wsdl-port-types)
(define-schema-collected-part
  wsdl-file-connector wsdl:|binding|   :binding   wsdl-bindings)
(define-schema-collected-part
  wsdl-file-connector wsdl:|service|   :service   wsdl-services)

(define-schema-collected-part
  schema-file-connector wsdl::|import| :import   schema-imports)
(define-schema-collected-part
  schema-file-connector wsdl::|include| :include   schema-imports)


(define-schema-simple-part wsdl-file-connector wsdl:|input|      :input)
(define-schema-simple-part wsdl-file-connector wsdl:|output|     :output)
(define-schema-simple-part wsdl-file-connector wsoap:|binding|   :soap-binding)
(define-schema-simple-part wsdl-file-connector wsoap:|operation| :soap-operation)
(define-schema-simple-part wsdl-file-connector wsoap:|body|      :soap-body)
(define-schema-simple-part wsdl-file-connector wsoap:|address|   :soap-address)
(define-schema-simple-part wsdl-file-connector wsoap:|header|    :soap-header) ;;???

(define-schema-named-part wsdl-file-connector wsdl:|operation|  :operation)
(define-schema-named-part wsdl-file-connector wsdl:|part|       :part)
(define-schema-named-part wsdl-file-connector wsdl:|port|       :port)
(define-schema-named-part wsdl-file-connector wsdl:|fault|      :fault)
(define-schema-named-part wsdl-file-connector wsoap:|fault|     :soap-fault)

(define-schema-ignored-part wsdl-file-connector xsd:|annotation|)

(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.mime::|multipartRelated|)
(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.mime::|binding|)
(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.mime::|mimeXml|)
(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.mime::|content|)

(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.http::|binding|)
(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.http::|operation|)
(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.http::|urlEncoded|)
(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.http::|address|)

(define-schema-ignored-part wsdl-file-connector net.xmp.wsdl.msstk::|binding|)

;;; We also ignore any alements that are in application namespaces
(defmethod net.xmp::schema-make-element ((conn wsdl-file-connector) elt &rest keys
				&key tag &allow-other-keys)
  (let ((pk (symbol-package elt)))
    (cond ((or (eq pk (find-package :net.xmp.soap))
	       (eq pk (find-package :net.xmp.wsdl))
	       (eq pk (find-package :net.xmp.wsdl.soap)))
	   (call-next-method))
	  (tag (call-next-method))
	  ((member elt (net.xmp::schema-ignored-messages conn)) (call-next-method))
	  ((eq :ignored (schema-element-key
			 (first (net.xmp::schema-component-stack conn))))
	   (call-next-method))
	  (t (apply #'call-next-method conn elt :tag :ignored keys)))))
       




;;; Allow some common errors:
(define-schema-collected-part schema-file-connector
  wsdl::|element|     :element      schema-elements)



(defmethod wsdl-service-names ((conn wsdl-file-connector) &optional and-ports)
  (if and-ports
      (let (services ports address)
	(dolist (sdef (cdr (wsdl-services conn)))
	  (dolist (spart (schema-collected-parts sdef :service))
	    (case (schema-element-key spart)
	      (:port (when (setf address (schema-single-part spart :soap-address))
		       (push 
			(case and-ports
			  (:verbose
			   (list (schema-component-name spart)
				 (schema-decoded-attribute spart "binding")
				 (schema-decoded-attribute address "location")))
			  (otherwise (schema-component-name spart)))
			ports)))))
	  (push (cons (schema-component-name sdef) (reverse ports)) services))
	(reverse services))
      (mapcar 'schema-component-name (cdr (wsdl-services conn)))))


(defmethod wsdl-define-content ((conn wsdl-file-connector) item)
  (schema-parts-to-type item :error-p nil))

(defun schema-true-p (v)
  (or (equal v "1") (eql v 1) (equalp v "true")))

(defmethod wsdl-parts-to-type ((conn wsdl-file-connector) msg &key options)
  `(:complex
    (:seq
     ,@(mapcar #'(lambda (part 
			  &aux 
			  (name (schema-decoded-attribute part "name"))
			  (type (schema-decoded-attribute part "type"))
			  (elt  (schema-decoded-attribute part "element"))
			  (nillable (schema-true-p
				     (schema-decoded-attribute part "nillable")))
			  )
		   (cond 
		    ((and name type (null elt))
		     `(:element (,name) ,type 
				,@(and nillable (list :nillable t))
				))
		    ((and elt (null type))
		     (if (schema-lookup-element conn elt)
			 elt
		       (error "cannot find message element def")))
		    (t (error "strange message part"))
		    ))
	       (schema-collected-parts msg :message :part)))
    ,@options))


(defmethod wsdl-define-element  ((conn wsdl-file-connector) edef &aux ctype)
	    
  ;; <element><complexType>...</complexType></element>
  ;; edef -> (name :element ((:complex-type type-name-or-nil
  ;;                            :complex-type (:content e-type-def))
  ;;
  ;; <element type=   />
  ;;      -> (name :type type :attributes attrs)
  ;;
  ;; <element    />
  ;;      -> (name :attributes attrs)            ANY content
  ;; e-type-def (collector e-type-def-part ... )
  ;; e-type-def-part -> (:element name :type type :attributes attrs
  ;;                                   :content content)
  ;;                 -> (:any . attributes)
  (or (setf ctype (schema-parts-to-type edef :error-p nil))
      (setf ctype (schema-decoded-attribute edef "type"))
      (setf ctype (xmp-any-type conn)))
  (wsdl-index-form conn
		   `(define-soap-element nil
		      ,(qt (schema-component-name edef))
		      ,(qt ctype)
		      ,@(when (schema-true-p (schema-decoded-attribute
					      edef "nillable"))
			  (list :nillable t))
		      ))
  )

(defmethod wsdl-define-type ((conn wsdl-file-connector) (typedef schema-component))
  (let* ((name (schema-component-name typedef))
	 x y z complex-content item
	 (simple (case (schema-element-key typedef)
		   (:simple-type (or (schema-component-content typedef) :null))))
	 (complex (case (schema-element-key typedef)
		    (:complex-type (or (schema-component-content typedef) :null))))
	 (aopts (append
		 (when (wsdl-option conn :send-atype)
		   (list :send-atype t))
		 (when (wsdl-option conn :send-asize)
		   (list :send-asize t))))
	 new-def err)
    
    (multiple-value-setq (new-def err)
      (schema-parts-to-type typedef :conn conn :error-p nil))
    
    (cond (new-def)
	  ((eq complex :null)
	   (setf new-def (xmp-any-type conn)))
	  ((consp complex)
	   (cond
	    ((setf complex-content (schema-single-part typedef :complex-content))
	     (or (when (setf item (schema-single-part complex-content :restriction))
		  (let* ((r-base (schema-decoded-attribute item "base"))
			 (r-attr (schema-single-part item :attribute))
			 (r-atype (when r-attr
				    (schema-decoded-attribute r-attr "arrayType")))
			 
			 )

		    (when *wsdl-debug*
		      (format
		       t "~&wsdl-define-type: r-base=~S   r-attr=~S  r-atype=~S ~%"
		       r-base r-attr r-atype))

		    (cond
		     ((and (eq r-base 'enc:|Array|)
			   r-attr
			   (eq (schema-component-name r-attr) 'enc:|arrayType|)
			   r-atype)
		      (setf new-def
			    `(:array ,(soap-parse-array-type conn r-atype :dns) ,@aopts))
		      t)


		     ;;     <restriction base="soapenc:Array">
		     ;;        <sequence>
		     ;;          <element minOccurs="0" maxOccurs="unbounded"   ???
		     ;;             name="String" type="s:string" /> 
		     ;;       </sequence></restriction>
		     ;;
		     ;; in xmethods.com InterFAX FAX Web Service 
		     ((and (eq r-base 'enc:|Array|)
			   (setf r-atype (schema-parts-to-type item :error-p nil)))
		      (setf new-def `(:array ,r-atype ,@aopts))
		      t)

		     ;;   <restriction wsdl:base SOAP-ENC:Array>
		     ;;     <element or type/>
		     ;;     </restriction>
		     ;;
		     ;; in xmethods "Muse.Net Client Service"
		     ((and (setf x (schema-component-content item))
			   (eql 1 (length x))
			   (setf x (schema-parts-to-type (first x) :error-p nil)))
		      (setf new-def `(:array ,x ,@aopts))
		      t)

		     (t 
		      (wsdl-warn
		       conn
		       "wsdl-define-type - unknown restriction in complex-content ~S"
		       complex-content)
		      t))))

		 (when (setf item (schema-single-part complex-content :extension))
		   (let* ((e-base (schema-decoded-attribute item "base"))
			  (e-parts (schema-component-content item))
			  )
		     (cond ((and e-base 
				 (dolist (p e-parts t)
				   (case (schema-element-key p)
				     ((:attribute) nil)
				     (otherwise (return nil)))))
			    (setf new-def `(:simple ,e-base))
			    t)
			   (t 
			    (wsdl-warn
			     conn
			     "wsdl-define-type - unknown extension in ~S with parts ~S"
			     complex-content e-parts)
			    t)
			   )))
	     
		 (wsdl-warn
		  conn
		  "wsdl-define-type - unknown item(s) in complex-content ~{~S~} of ~S"
		  (schema-component-content complex-content) typedef)))
	    
	    ;;<complexType><element.../>... </complexType>
	    ;;
	    ;;   in xmethods "Xara NavBar Generator"
	    ((setf x (schema-collected-parts typedef :complex-type :element))
	     (setf new-def
		   `(:complex (:seq1 ,@(mapcar
					#'(lambda (e)
					    `(:element
					      ,(string (schema-component-name e))
					      (:simple
					       ,(schema-decoded-attribute e "type"))
					      ,@(when (schema-true-p
						       (schema-decoded-attribute 
							e "nillable"))
						  (list :nillable t))		      
					      ))
					x)))))
	    
	    ;; <complexType><element type="" maxOccurs=n/></complexType>
	    ;; apparently sloppy def in xmethods "Agni Find MP3"
	    ((and (null (cdr complex)) 
		  (setf x (first complex))
		  (null (schema-component-content x))
		  (equal "element" (string (schema-element-tag x))))
	     (setf new-def
		   `(:array ,(or (setf y (schema-decoded-attribute x "type"))
				 (list :any))
			    ,@aopts)))

	    ((and (setf x (schema-single-part typedef :simple-content))
		  (setf y (schema-single-part x :extension))
		  (setf z (schema-decoded-attribute y "base"))
		  )
	     (setf new-def `(:simple ,z)))

	    ((schema-single-part typedef :attribute :error-p nil :more-p t)
	     ;; ignored ???
	     nil)

	    (t (wsdl-warn conn "wsdl-define-type - unknown complex type parts ~S in ~S"
			  complex typedef))
	    ))

	  ((eq simple :null)
	   (setf new-def `(:simple xsd:|string|)))
	  ((consp simple)
	   (or
	    (let* ((r (schema-single-part typedef :restriction))
		   (base (when r (schema-decoded-attribute r "base"))))
	      (when base

		;; look for parts <enumeration "value" val> 
		;;   and collect values  - need extension to :simple ???
		;; look for part <maxLength value=n/>   ???

		(setf new-def `(:simple ,base))
		t))
	    (let* ((sp (schema-single-part typedef :simple-content))
		   (ex (when sp (schema-single-part sp :extension)))
		   (base (when ex (schema-decoded-attribute ex "base")))
		   )
	      (when base (setf new-def `(:simple ,base)) t))
	    (wsdl-warn conn "wsdl-define-type - unknown simple type ~S"
		       simple)))


	  (t (wsdl-warn
	      conn "wsdl-define-type - unknown form ~S err was ~A" typedef err)))
    (when new-def
      (wsdl-index-form conn `(define-soap-type nil ,(qt name) ,(qt new-def))))
      
    ))

(defun qt (val)
  (typecase val
    (number val)
    (string val)
    (keyword val)
    (otherwise (list 'quote val))))

(defun maybe-second (form) 
  (cond ((consp form)
	 (if (eq 'quote (first form))
	     (second form)
	   (error "Not a qoted value ~S" form)))
	((constantp form) form)
	(t (error "Not a constant ~S" form))))
	 

(defmethod wsdl-resolve-type ((conn wsdl-file-connector) type &aux def name)
  ;; returns nil  or  type-def and outermost name of def
  (loop
   (setf def (if (consp type)
		 type
	       (when type
		 (or name (setf name type))
		 (or (wsdl-lookup conn :type type nil)
		     (soap-find-type conn type :dns)))))
   (cond ((null def) (return nil))
	 (t  (or (ecase (first def)
		   (:simple (when (second def)
			      (setf type (second def))
			      (or name (setf name (second def)))))
		   (:array nil)
		   (:complex nil))
		 (return (values def name type)))))))

(defmethod wsdl-resolve-element ((conn wsdl-file-connector) elt)
  ;; returns nil  or  type-def 
  (if (consp elt)
      (third elt)
    (when elt
      (or (wsdl-lookup conn :element elt nil)
	  (soap-find-element conn elt :dns)))))
      

(defmethod wsdl-lookup ((conn wsdl-file-connector) kind name recursive)
  ;; returnd 2 values: v1 v2
  ;; kind -> :element | :type 
  ;; recursive -> nil | t | :any            v1=type def     v2=defining form
  ;;           -> :complex        
  ;;                  if def is :complex    v1=type def     v2=defining form
  ;;                  if def is other       v1=nil          v2=defining form
  ;;                  if def is named, follow name chain
  (flet ((lookup (conn name recursive d &optional plist &aux tp v1 v2) 
		 (when (xmp-match-name conn  (maybe-second (third d)) name)
		   (when (setf tp (maybe-second (fourth d)))
		     (cond ((null recursive) (setf v1 tp v2 d))
			   ((case recursive ((t :any) t)) (setf v1 tp v2 d))
			   ((consp tp)
			    (if (eq recursive (first tp))
				(setf v1 tp v2 d)
			      (setf v1 nil v2 d)))
			   (t (multiple-value-bind (t2 d2)
				  (wsdl-lookup conn :type tp recursive)
				(cond (t2 (setf v1 t2 v2 d2))
				      (d2 (setf v1 nil v2 d2))
				      (t (setf v1 nil v2 d)))))))
		   (return-from wsdl-lookup (values v1 v2 plist)))))
    (dolist (d (wsdl-def-list conn))
      (case (first (setf d (first d)))
	(define-soap-element
	  (case kind (:element (lookup conn name recursive d (cddddr d)))))
	(define-soap-type
	  (case kind (:type    (lookup conn name recursive d))))
	))))
		   
(defmethod wsdl-message-part-element ((conn wsdl-file-connector) part)
  (or (schema-decoded-attribute part "element")
      (schema-component-name part)))

(defmethod wsdl-do-element
  (conn style method-name action intern-name bind msg parts
	&aux
	(soap-body (schema-single-part bind :soap-body))
	(estyle (when soap-body
		  (schema-decoded-attribute soap-body "encodingStyle")))
			   
	;;??? This seems to be use-less?
	;;(use    (schema-decoded-attribute soap-body "use"))
			   
	(ns     (when soap-body (schema-decoded-attribute soap-body "namespace")))
	(pk (when ns (net.xmp::xmp-uri-to-package conn ns :dns)))
	(pktag "tns")
	def-tail tail-props msg-type part-type elt def nillable)

  (or pk
      (when (null ns)
	(let* ((tg (wsdl-targets conn)) pk2)

	  ;; Look in the list of all targetNamespace attributes
	  ;; in the definition.

	  (cond
	   ;; If there is only one targetNamespace
	   ;;    and it has a package, make these two the same
	   ((and (eql 1 (length tg))
		 (setf pk2 (net.xmp::xmp-uri-to-package
			    conn (first tg) :dns)))
	    (setf pk pk2)))))			   
      (when ns (pushnew ns (wsdl-undef-ns conn) :test #'string=)))
  (setf def-tail
	`(,@(when ns
	      `(:namespaces
		(nil (,(when pk (intern (package-name pk) :keyword))
		      ,pktag
		      ,ns))))
	    ,@(when estyle `(:encoding ,estyle))
	    ,@(when action `(:action ,action))
	    ))
  (setf tail-props (append (when ns (list :namespaces))
			   (when estyle (list :encoding))
			   (when action (list :action))))
  (when intern-name
    (setf method-name (etypecase method-name
			(symbol method-name)
			(string (if pk
				    (intern method-name pk)
				  method-name)))))
  (when (schema-true-p (schema-decoded-attribute msg "nillable"))
    (setf nillable (list :nillable t)))

  (values
   (cond
    ((string-equal style "rpc")
     
     ;; Operation name is method name, each part is a parameter
     ;; SOAP method call and response are always structs.

     (let ((mname method-name)
	   (mtype (wsdl-parts-to-type conn msg :options def-tail)))
       (if (wsdl-lookup conn :element method-name nil)
	   (let ((i 1)
		 (pk (when (symbolp method-name) (symbol-package method-name)))
		 mn)
	     ;; This element was already defined once - this is an
	     ;; overloaded operation.  We need to define a new type
	     ;; name for the overloaded operations.
	     (loop (setf mn (format nil "~A_~A" method-name i))
		   (when pk (setf mn (intern mn pk)))
		   (if (wsdl-lookup conn :type mn nil)
		       (incf i)
		     (return (setf method-name mn))))
	     (push method-name (wsdl-overloaded-ops conn))
	     `(define-soap-type nil ,(qt method-name)
		'(:complex (:seq (:element ,mname ,mtype ,@nillable)) ,@def-tail))
	     )

	 `(define-soap-element nil ,(qt method-name) ,(qt mtype) ,@nillable))
       ))

    ((string-equal style "document")

     ;; Operation name is the element in the part
     ;;  and it was already defined in the schema.

     (setf elt (schema-decoded-attribute (first parts) "element"))
     (if (eq :doc intern-name)
	 (setf method-name elt)
       (setf method-name (string elt)))

     ;; We need to update the :namespaces option in the def
     ;;    def is in list of forms to be evaluated...

     ;; usual document  style part has the form
     ;;   <part name="xxx" element="defined-element" />
     (multiple-value-setq (msg-type def)
       (wsdl-lookup conn :element method-name :complex))

     ;; 14-Jan-05  mm: sometimes document style part has the form
     ;;     <part element="ignored-name" type="defined-type" />
     (or msg-type def
	 (let ()
	   (setf part-type (schema-decoded-attribute (first parts) "type"))
	   (when part-type
	     (multiple-value-setq (msg-type def)
	       (wsdl-lookup conn :type part-type :complex)))))
	     
     (flet ((update (props place tail case)
		    (dolist (p props (nconc place tail))
		      (when (getf (cddr place) p)
			(error "Multiple definitions (~A) of ~S on ~S"
			       case p method-name)))))
       (cond (msg-type (update tail-props msg-type def-tail 1))
	     (def (cond ((consp (maybe-second (fourth def)))
			 (update tail-props (maybe-second (fourth def)) def-tail 2)
			 (setf msg-type (maybe-second (fourth def))))
			(t (setf (fourth def)
				 (list 'quote
				       (list* :simple (maybe-second (fourth def))
					      def-tail)))
			   (setf msg-type (maybe-second (fourth def))))))
	     (t (error "Cannot determine message type"))))
     nil)
    (t (error "Unknown message format."))
    )

   method-name msg-type))

(defmethod wsdl-bind-operation ((conn wsdl-file-connector) b mode eval prefix suffix)
  (declare (ignore eval))
  (flet ((part-name (part) (wsdl-message-part-element conn part))
	 )
    (let* ((op-name (schema-component-name b))
	   (soap-op  (schema-single-part b :soap-operation))
	   (action   (schema-raw-attribute soap-op "soapAction"))
	   (messages (cdr (wsdl-messages conn)))
	   (style    (let ((s (or (schema-raw-attribute soap-op "style")
				  (wsdl-soap-style conn)))
			   p e enames)
		       (when (string-equal s "document")
			 ;; True document style defines all messages
			 ;;    with exactly one part???
			 ;;    and with unique element names???
			 (dolist (m messages)
			   (setf p (schema-collected-parts m :message :part))
			   (or (and p (null (cdr p))
				    (null (member (setf e (schema-decoded-attribute
							   (first p) "element"))
						  enames)))
			       (return (setf s "rpc")))
			   (push e enames)
			   ))
		       ;;(format t "~&~S~%" enames)
		       s))
	   (bind-in (schema-single-part b :input))
	   (bind-out (schema-single-part b :output))

	   ;; <binding>
	   ;;  <in-or-out> <:soap-body
	   ;;                     wsdl:use= encoded|literal
	   ;;                     wsdl:namespace= URIstring
	   ;;                     wsdl:encodingStyle= URIstring
	   ;;                     wsdl:parts=  nmtokens          ???
	   ;;                     />
	   ;;        soap:header  ???
	   ;;        soap:fault   ???
	   ;;    </in-or-out> </binding>

	   (opdefs   (let (all)
		       (dolist (op (wsdl-operations conn) all)
			 (when (equal op-name (schema-component-name op))
			   (push op all)))))

	   (opdef  (cond ((null opdefs) (error "Cannot find operation ~A" op-name))
			 ((null (cdr opdefs)) (first opdefs))
			 (t
			  ;; more than one operation - overloaded op name
			  ;; find the one with matching input or output
			  (or
			  (cond (bind-in
				 (dolist (op opdefs)
				   (let ((op-in (schema-single-part op :input)))
				     (and op-in
					  (equal (schema-component-name bind-in)
						 (schema-component-name op-in))
					  (return op)))))
				(bind-out
				 (dolist (op opdefs)
				   (let ((op-out (schema-single-part op :output)))
				     (and op-out
					  (equal (schema-component-name bind-out)
						 (schema-component-name op-out))
					  (return op))))))
			  (error "Cannot find overloaded operation ~A" op-name)))))
	   (op-in    (schema-single-part opdef :input))
	   (in-msg-name   (when op-in (schema-decoded-attribute op-in "message")))
	   (op-out   (schema-single-part opdef :output))
	   (out-msg-name  (when op-out (schema-decoded-attribute op-out "message")))
	   (in-msg (when in-msg-name
		     (schema-collected-component
		      conn #'wsdl-messages #'schema-component-name in-msg-name)))
	   (in-parts (when in-msg
		       (schema-collected-parts in-msg :message :part)))
	   (out-msg (when out-msg-name
		      (schema-collected-component
		       conn #'wsdl-messages #'schema-component-name out-msg-name)))
	   (out-parts (when out-msg
			(schema-collected-parts out-msg :message :part)))
	   (cx (when in-msg (position in-msg messages)))
	   comment done-form doc-type doc-ret ret-name)

      (when *wsdl-debug*
	(format t "~&;WSDL op=~S  in-msg=~S ~S  out-msg=~S ~S~%" 
		op-name in-msg-name (when in-msg (length in-parts)) 
		out-msg-name (when out-msg (length out-parts))))

      (let (op-rename)
	(when in-msg
	  (multiple-value-setq (done-form op-rename doc-type)
	    (wsdl-do-element conn style op-name action
			     (if (and (string-equal style "document")
				      (eql 1 (length in-parts)))
				 :doc
			       t)
			     bind-in in-msg in-parts))
	  (when done-form (wsdl-index-form conn done-form)))
	(when out-msg
	  (multiple-value-setq (done-form ret-name doc-ret)
	    (wsdl-do-element
	     conn style (concatenate 'string (string op-rename) "Response")
	     nil nil bind-out out-msg out-parts))
	  (when done-form (wsdl-index-form conn done-form)))
	(when (and in-msg out-msg);;???  in-out messages notification messages...
	  (wsdl-index-form
	   conn
	   (let* ((def-name (wsdl-compose-name conn nil prefix (ecase suffix
								 (:index (incf cx))
								 (:message op-rename))))
		  (one-part (and in-parts
				 (null (cdr in-parts))
				 (first in-parts)))
		  (one-type (when one-part
			      (or
			       (and (string-equal style "rpc")
				    (schema-decoded-attribute one-part "type"))
			       (and (string-equal style "document")
				    doc-type))))
		  (one-def  (and one-type
				 (cond
				  ((string-equal style "document")
				   (setf one-type doc-type))
				  ((wsdl-expand-singleton conn)
				   (and
				    (setf one-type
					  (schema-lookup-type conn	one-type))
				    (setf one-type
					  (schema-parts-to-type
					   one-type :error-p nil)))))
				 (eq :complex (first one-type))
				 (case (first (second one-type))
				   ((:seq* :seq :seq1 :set :set1) one-type)
				   (otherwise nil))))
		  (in-elts    (if one-def
				  (mapcar #'(lambda (p) (xmp-pick-name nil p))
					  (cdr (second  one-def)))
				(mapcar #'part-name in-parts)))
		  (key-args   (mapcar #'(lambda (part)
					  (wsdl-intern-name conn (string part)))
				      in-elts))
		  (arglist (mapcan #'(lambda (elt arg) (list (string elt) arg))
				   in-elts key-args))
		  )
	     (or (string-equal style "document")
		 (equal op-name op-rename)
		 ;; if we have an overloaded rpc message name, we need
		 ;;  to access through a defined type
		 (setf arglist `(,(qt op-name) (list ,@arglist))))
	     (ecase mode
	       (:client
		(setf comment
		      (list (format nil "Send client message ~A " op-name)
			    op-name))
		(wsdl-generate-code 
		 conn mode :top-level
		 'defun def-name 
		 `(&key ,@key-args)
		 `(let ((,(wsdl-intern-name conn "conn")
			 ,(apply #'wsdl-generate-code 
				 conn mode nil 
				 'soap-message-client 
				 (list*
				  :url (wsdl-url-name conn)
				  :message-dns (qt (wsdl-map-name conn))
				  (wsdl-client-options conn)
				  ))))
		    (multiple-value-call
		     #'values
		     ,(if (and one-def (not (string-equal style "document")))
			  `(call-soap-method ,(wsdl-intern-name conn "conn")
					     ,(qt op-rename)
					     ,(string (part-name one-part))
					     (list ,@arglist))
			`(call-soap-method
			  ,(wsdl-intern-name conn "conn")
			  ,(qt op-rename)
			  ,@arglist))
		     ,(wsdl-intern-name conn "conn")))))
	       (:server
		(let* ((one-ret (and out-parts
				     (null (cdr out-parts))
				     (first out-parts)))
		       (ret-type (when one-ret
				   (or
				    (and (string-equal style "rpc")
					 (schema-decoded-attribute one-ret "type"))
				    (and (string-equal style "document")
					 doc-ret))))
		       (ret-def  (and ret-type
				      (cond
				       ((string-equal style "document")
					(setf ret-type doc-ret))
				       ((wsdl-expand-singleton conn)
					(and
					 (setf ret-type
					       (schema-lookup-type conn ret-type))
					 (setf ret-type
					       (schema-parts-to-type
						ret-type :error-p nil))
					 (eq :complex (car ret-type))
					 ret-type)))))
		       (ret-keys   (if ret-def
				       (mapcar #'(lambda (part)
						   (wsdl-intern-name
						    conn (xmp-pick-name nil part)
						    :preserve))
					       (cdr (second ret-def)))
				     (mapcar
				      #'(lambda (part)
					  (wsdl-intern-name
					   conn (part-name part) :preserve))
				      out-parts)))
		       (ret-vars (mapcar #'(lambda (r)
					     (wsdl-intern-name conn (string r)))
					 ret-keys))		       
		       (ret-parts (if ret-def
				      (mapcan (lambda (part var)
						(list 
						 (list 'quote (xmp-pick-name nil part))
						 var))
					      (cdr (second  ret-def))
					      ret-vars
					      )
				    (mapcan 
				     #'(lambda (part var)
					 (list (list 'quote (part-name part)) var))
				     out-parts ret-vars)
				    ))
		       (key-list (mapcar #'(lambda (v r)
					     (list (list r v)))
					 key-args in-elts))
		       )
		  (push `(,(qt op-name) ,(qt in-elts)
				    ,@(when action (list :action action))
				    :lisp-name ,(qt def-name)
				    :return ,(string ret-name))
			(wsdl-server-exports conn))
		  (setf comment (format nil "Handler for message ~A" op-name))

		  (if (and one-def (not (string-equal style "document")))

		      (let* ((one-part-name (part-name one-part))
			     (one-part-var (wsdl-intern-name
					    conn (string one-part-name))))
			(wsdl-generate-code 
			 conn mode :top-body
			 'defun def-name `(&key ((,one-part-name ,one-part-var)))
			 `(apply #'(lambda (&key ,@key-list)
				     (let ,ret-vars
				       "INSERT BODY HERE"
				       ,(if (and ret-def
						 (not (string-equal style "document")))
					    `(list
					      ,(string (part-name (first out-parts)))
					      (list ,@ret-parts))
					  `(list ,@ret-parts))))
				 ,one-part-var)))

		    (wsdl-generate-code 
		     conn mode :top-body
		     'defun def-name `(&key ,@key-list)
		     `(let ,ret-vars
			"INSERT BODY HERE"
			,(if (and ret-def (not (string-equal style "document")))
			     `(list ,(string (part-name (first out-parts)))
				    (list ,@ret-parts))
			   `(list ,@ret-parts))))

		    )



		  ))
	       ))
	   comment)))
      )))

(defmethod wsdl-generate-code ((conn wsdl-file-connector) (mode t) (info t)
			       (op t) &rest args)
  (cons op args))


(defmethod wsdl-service-binding-names ((conn wsdl-file-connector) sdef port
				       &optional error-p)
  (let* (pref bname url (i 0)
	      )
    (dolist (spart (schema-collected-parts sdef :service :port))
      (if (typecase port 
	    (integer (eql port i))
	    (otherwise (eq port (schema-component-name spart))))
	  (return (setf pref spart))
	(incf i)))
    (when pref
      (setf bname (schema-decoded-attribute pref "binding"))
      (setf url  (schema-decoded-attribute 
		  (schema-single-part pref :soap-address)
		  "location")))

    (if (and sdef pref bname url)
	(values bname url)
      (if error-p
	  (error "Cannot find binding name in service ~S" sdef)
	nil))))



(defmethod make-client-interface ((conn wsdl-file-connector) service destination
				  &rest args)
  (apply 'wsdl-make-interface conn service destination :client args))

(defmethod make-server-interface ((conn wsdl-file-connector) service destination
				  &rest args)
  (apply 'wsdl-make-interface conn service destination :server args))
				  
(defmethod wsdl-intern-name ((conn wsdl-file-connector) form &optional preserve
			     &aux (file-package (wsdl-file-package conn)) pos)
  (etypecase form
    ((or string symbol)
     (setf form (string form))
     
     ;; This function is used only to make Lisp symbols for generated code.
     (when (setf pos (position #\: form))
       ;; Drop namespace qualifier if there.
       (setf form (subseq form (1+ pos))))

     (etypecase preserve
       ((member :preserve) (intern form file-package))
       (null (let ((*package* file-package))
	       (read-from-string form)))
       ))))


(defun break-at-case-shifts (s &aux c h maybe-long-upper out)
  (dotimes (i (length s)
	      (when (if maybe-long-upper
			(setf h maybe-long-upper)
		      h)
		(setf out (concatenate 'string
					     (or out "") (if out "-" "")
					     (subseq s h i))))
	      )
    (setf c (elt s i))
    (cond ((upper-case-p c)
	   (cond ((null h))
		 ((null out) (setf out (subseq s h i)))
		 (t (setf out (concatenate 'string
					   out "-" (subseq s h i)))))
	   (setf h nil)
	   (cond (maybe-long-upper)
		 (t (setf maybe-long-upper i))))
	  ((null maybe-long-upper) (or h (setf h i)))
	  (t (cond
	      ((eql (1- i) maybe-long-upper) 
	       ;;   aaaAx  --> aaa-Ax
	       (setf h maybe-long-upper))
	      ((null out)
	       ;;   AABx  --> AA-Bx 
	       (setf out (subseq s maybe-long-upper (1- i)))
	       (setf h (1- i)))
	      (t
	       ;;   ...AABx  --> ...-AA-Bx
	       (setf out (concatenate
			  'string out "-" (subseq s maybe-long-upper (1- i))))
	       (setf h (1- i))))
	     (setf maybe-long-upper nil)))
    )
  (string-downcase out))
	  
					   



(defmethod wsdl-compose-part ((conn wsdl-file-connector) part
			      &aux
			      (comp (wsdl-compose-strategy conn))
			      (conflict (wsdl-maybe-conflicts conn))
			      )
  (case comp
    (:hyphen
     ;; break string at down-up case shift, insert hyphen, downcase all
     (break-at-case-shifts part)
     )
    (:hyphen-if-ok
     ;; apply only if maybe-conflicts is nil
     (if (null conflict)
	 (break-at-case-shifts part)
       part))
    (:downcase  (string-downcase part))
    (:downcase-if-ok
     (if (null conflict)
	 (string-downcase part)
       part))
    (otherwise
     ;; keep the case
     part)))

(defmethod wsdl-compose-name ((conn wsdl-file-connector) package &rest parts)
  (let (mode (file-package (wsdl-file-package conn)))
    (case *current-case-mode* 
      (:case-sensitive-lower (setf mode :modern))
      (:case-insensitive-upper (setf mode :ansi)))
    (let ((*package* (typecase package
		       (null file-package)
		       (package package)
		       (otherwise (or (find-package package) 
				      (error "Cannot find the package ~S" package))))))
      (read-from-string (format nil "~{~A~}"
				(mapcar #'(lambda (part)
					    (typecase part
					      (null "")
					      (string (wsdl-compose-part conn part))
					      (symbol
					       (case mode
						 (:ansi (string-downcase part))
						 (otherwise part)))
					      (otherwise part)))
					parts))))))
    


(defmethod wsdl-index-form ((conn wsdl-file-connector) form &rest comments)
  (with-slots
   (def-list sub-list def-index) conn
   (etypecase form
     (cons
      (let* ((key (first form))
	     (place (assoc key def-index))
	     (name (case key
		     (defun (second form))
		     ((define-soap-element define-soap-type)
		      (maybe-second (third form)))
		     (otherwise (second form))))
	     )
	(or place (push (setf place (list key)) def-index))
	(and comments (consp (car comments)) (null (cdr comments))
	     (setf comments (car comments)))
	(push (if comments
		  (cons name comments)
		name)
	      (cdr place)))
      (push (cons form comments) def-list)
      (push (first def-list) sub-list)
      form))))

(defmethod wsdl-walk-form ((conn wsdl-file-connector) form pl
			   &aux all-names maybe-conflicts change)
  (case (setf maybe-conflicts (wsdl-maybe-conflicts conn))
    (:unknown (setf maybe-conflicts nil)))
  (labels ((same-string (s1 s2) (typecase s1
				  (string (typecase s2
					    (string (equal s1 s2))))
				  (symbol (typecase s2
					    (symbol (eq s1 s2))))))
	   (walk (form) (typecase form
			  (symbol (pushnew (symbol-package form) pl)
				  (dolist (name all-names)
				    (cond ((eq form name))
					  ((equal form name))
					  ((string-equal form name)
					   (pushnew form maybe-conflicts
						    :test #'same-string))))
				  )
			  (string (dolist (name all-names)
				    (cond ((equal form name))
					  ((string-equal form name)
					   (pushnew form maybe-conflicts
						    :test #'same-string))))
				  )
			  (cons (walk (car form))
				(walk (cdr form))))))
    (or (equal maybe-conflicts (wsdl-maybe-conflicts conn))
	(setf change t))
    (walk form)
    (setf (wsdl-maybe-conflicts conn) maybe-conflicts)
    (values pl change)))

(defmethod wsdl-post-process ((conn t) (defs t)) nil)

(defun wsdl-print-lines (stream prefix &rest lines)
  (or prefix (format stream "~&#|~%"))
  (dolist (l lines)
    (typecase l
      ((integer 1 9) (format stream "~&~V%" l))
      (string (format stream "~&~A~A~%" (or prefix "") l))
      (otherwise (format stream "~&~A~S~%" (or prefix "") l))))
  (or prefix (format stream "~&|#~%"))
  nil)

(defmethod wsdl-describe ((conn wsdl-file-connector) defs stream depth)
  (dolist (def defs)
    (cond
     ((eq 'defun (getf (cdr (first def)) :key))
      (format stream "~A~%" (first (first def)))
      (dolist (entry (cdr def))
	(typecase entry
	  (cons
	   (format stream "~&~%     ~A  ~A~%" 
		   (first entry) (second entry))
	   (when (third entry)
	     (describe-soap-method 
	      (third entry) :wsdl conn :stream stream :depth depth)))
	  (otherwise (format stream "~&~%     ~S~%" entry))))
      (format stream "~%"))
     ((and (cdr def) (null (cddr def)) (atom (second def)))
      (format stream "~A ~S~2%" (first (first def)) (second def)))
     ((and (cdr def) (consp (second def)))
      (format stream "~A~%~{     ~{~S ~}~%~}~%"
	      (first (first def)) (cdr def)))
     (t
      (format stream "~A~%~{     ~S~%~}~%" 
	      (first (first def)) (cdr def))))))

(defmethod wsdl-make-interface ((conn wsdl-file-connector) service destination
				mode
				&key
				port verbose
				eval
				(lisp-package :keyword)
				(file-package :user)
				(expand-singleton t)
				null-element (empty-element nil eep)
				(prefix (format nil "~A-" mode))
				(suffix :index)
				(compose :hyphen-if-ok)
				action (if-missing-package :warn)
				message-dns
				(if-exists :supersede)
				body-form
				map
				object-class message-method-prefix
				(text-file :txt) ;;; stream|path|:insert|:txt|nil
				(class-file :insert)
				(post-file  :insert)
				connect-class built-in-arrays defined-arrays
				send-atype send-asize sequence

				&aux
				w open dstream tstream dfile tfile topen
				cstream copen cfile pstream popen pfile
				head-forms tail-forms class-forms wrap-forms post-forms
				sdef bname port-name pdef opdefs bdef
				binding btype url
				(*xmp-warning-leader* "WSDL"))
  (or service (setf service 0))
  (or port (setf port 0))
  (and (numberp service)
       (setf w (nth service (wsdl-service-names conn)))
       (setf service w))
  (or (eq service :none)
      (setf sdef (schema-collected-component
		  conn #'wsdl-services #'schema-component-name service t))
      (error "Cannot find service ~S" service))
  (and verbose (cdr (schema-imports conn))
       (warn "WSDL definition contains <include> or <import> elements."))
  (setf (wsdl-warnings conn) 0)
  (when sdef
    (multiple-value-setq (bname url)
      (wsdl-service-binding-names conn sdef port t))
    (setf bdef (or (schema-collected-component
		    conn #'wsdl-bindings #'schema-component-name 
		    
		    ;; xmethods.com QueryInterfaceService uses the wrong namespace
		    (string bname)
		    
		    )
		   (error "Cannot find binding ~S" bname)))
    (setf binding bdef)
    (setf btype   (schema-decoded-attribute bdef "type"))
    (setf port-name btype)
    (setf pdef (or (schema-collected-component
		    conn #'wsdl-port-types #'schema-component-name port-name)
		   (error "Cannot find portType ~S" port-name)))
    (setf opdefs (schema-collected-parts pdef :port-type :operation))
    )
  (etypecase destination
    (stream (setf dstream destination))
    ((member nil t) (setf dstream destination tstream destination))
    (string (setf dfile destination))
    (pathname (setf dfile destination))
    )

  (labels
      ((get-stream (stream file openp)
		   (cond (stream (values stream openp))
			 (file   (values (open file
					   :direction :output
					   :if-exists if-exists)
					 t))
			 (t (values stream openp))
			 ))
       (get-file  (infile base &aux stream file)
		  (etypecase infile
		    (null (setf stream nil file nil))
		    ((member :insert) (setf stream base))
		    ((member :txt)
		     (setf file (namestring
				 (make-pathname :type "txt" :defaults base))))
		    (stream (setf stream infile))
		    (string (setf file infile))
		    (pathname (setf file infile))
		    )
		  (values stream file))
       (dstream () (or dstream
		       (multiple-value-setq (dstream open)
			 (get-stream dstream dfile open)))
		dstream)
       (tstream ()
		(cond (tstream)
		      (t (multiple-value-setq (tstream tfile)
			   (get-file text-file (dstream)))
		       (or tstream
			   (multiple-value-setq (tstream topen)
			     (get-stream tstream tfile topen)))
		       tstream)))
       (cstream ()
		(cond (cstream)
		      (t (multiple-value-setq (cstream cfile)
			   (get-file class-file (dstream)))
		       (or cstream
			   (multiple-value-setq (cstream copen)
			     (get-stream cstream cfile copen)))
		       cstream)))
       (pstream ()
		(cond (pstream)
		      (t (multiple-value-setq (pstream pfile)
			   (get-file post-file (dstream)))
		       (or pstream
			   (multiple-value-setq (pstream popen)
			     (get-stream pstream pfile popen)))
		       pstream)))
       (do-file (item &optional (stream nil s-p)
		      &aux (form (first item)) (comments (cdr item)))
		(or s-p (setf stream (dstream)))
		(when stream
		  (when comments
		    (format stream "~&~%~%")
		    (dolist (c comments)
		      (or (symbolp c) (wsdl-print-lines stream ";; " c))))
		  (let ((*package* file-package))
		    (format stream "~&~%~S~%" form))))
       (trim-defs (conn &aux (forms (reverse (wsdl-sub-list conn))))
		  (setf (wsdl-sub-list conn) nil)
		  forms)
       )

    (unwind-protect
	(let (soap-binding x)

	  (setf (wsdl-undef-ns conn) nil
		(wsdl-props conn) nil
		(wsdl-maybe-conflicts conn) :unknown
		(wsdl-soap-address conn) url
		(wsdl-operations conn) opdefs
		(wsdl-expand-singleton conn) expand-singleton
		(wsdl-wdef-list conn) nil
		(wsdl-sub-list conn) nil
		(wsdl-def-index conn) nil
		(wsdl-compose-strategy conn) compose
		(wsdl-map-name conn) (or map (wsdl-compose-name
					      conn :keyword prefix "namespaces"))
		(wsdl-client-options conn)
		`( ,@(when lisp-package (list :lisp-package lisp-package))
		     ,@(when null-element (list :null-element null-element))
		     ,@(when eep (list :empty-element empty-element)) 
		     ,@(when body-form (list :body-form body-form))
		     ,@(when connect-class (list :class (list 'quote connect-class)))
		     )
		(wsdl-options conn) (list :built-in-arrays built-in-arrays
					  :defined-arrays defined-arrays 
					  :message-method-prefix message-method-prefix
					  :send-atype send-atype :send-asize send-asize
					  :prefix prefix :sequence sequence
					  )
		)

	  (cond ((null file-package) (setf file-package *package*))
		((packagep file-package))
		((setf x (find-package file-package))
		 (setf file-package x))
		(t (error "Cannot find package ~S" file-package)))
	  (wsdl-index-form
	   conn `(in-package ,(make-symbol (package-name file-package))))
	  (setf (wsdl-file-package conn) file-package)
	  (setf (wsdl-url-name conn)
		(wsdl-compose-name conn nil "*" prefix "service-url*"))
	  (wsdl-index-form
	   conn `(defpackage ,(make-symbol (package-name file-package))
		   (:use ,@(mapcar #'(lambda (p)
				       (make-symbol (package-name p)))
				   (package-use-list file-package)))))
	  
	  (setf head-forms (trim-defs conn))

	  (dolist (typedef (cdr (schema-types conn)))
	    (wsdl-define-type conn typedef))
	  (dolist (edef (cdr (schema-elements conn)))
	    (wsdl-define-element conn edef))
	    
	  (and binding
	       (setf soap-binding (schema-single-part binding :soap-binding))

	    ;; (:soap-binding style rpc|document transport URIstring) ???

	    (setf (wsdl-soap-style conn)
		  (schema-raw-attribute soap-binding "style")))

	  (when binding
	    (dolist (b (schema-component-content binding))
	    (case (schema-element-key b)
	      (:operation
	       (wsdl-bind-operation conn b mode eval prefix suffix)))))

	  (case mode
	    (:server
	     (wsdl-index-form
	      conn
	      (wsdl-generate-code
	       conn mode :top-level
	       'defun (wsdl-compose-name conn nil prefix "make-server")
	       `(&optional (,(wsdl-intern-name conn "port") 8080))
	       `(let ((,(wsdl-intern-name conn "s")
		       ,(apply #'wsdl-generate-code
			       conn mode nil
			       'soap-message-server
			       :start (list 'list :port (wsdl-intern-name conn "port"))
			       :enable :start
			       :publish (list 'list :path
					      (xmp-resolve-uri (wsdl-soap-address conn)
							       :path))
				
			       :lisp-package :keyword
			       (append 
				(when action (list :action action))
				(list :message-dns
				      (list 'quote
					    (or message-dns (wsdl-map-name conn))))
				(when body-form (list :body-form body-form)))
			       )))
		  ,@(mapcar #'(lambda (ex)
				(list* 'soap-export-method
				       (wsdl-intern-name conn "s")
				       ex))
			    (wsdl-server-exports conn))
		  ,(wsdl-intern-name conn "s"))))))
						   
	  (when (wsdl-undef-ns conn)
	    (case if-missing-package
	      (:error (error "There are no Lisp packages defined for namespaces ~S"
			     (wsdl-undef-ns conn)))
	      (:warn
	       (dolist (ns (wsdl-undef-ns conn))
		 (warn "There is no Lisp package defined for namespace ~S"
		       ns)))))

	    
	  (setf tail-forms (trim-defs conn))

	  (let (pl px pn)
	    
	    ;; do this last to pick up any packages created during 
	    ;; interface generation

	    (setq pl (wsdl-walk-form conn (wsdl-def-list conn) pl))
	    (wsdl-emit-object-classes conn object-class)
	    (setf class-forms (trim-defs conn))

	    (setq pl (wsdl-walk-form conn (wsdl-def-list conn) pl))
	    (wsdl-emit-wrappers conn (reverse (wsdl-def-list conn)))
	    (setf wrap-forms (trim-defs conn))
	    
	    (setq pl (wsdl-walk-form conn (wsdl-def-list conn) pl))
	    (wsdl-post-process conn (reverse (wsdl-def-list conn)))
	    (setf post-forms (trim-defs conn))
	    
	    (dolist (nsd (nse-defs (or message-dns
				       (xmp-message-dns conn))
				   :one :package))
	      (or (eq (find-package :keyword)
		      (nsd-package nsd))
		  (when (member (nsd-package nsd) pl)
		    (push (list 'quote
				(list (setf pn (make-symbol
						(package-name (nsd-package nsd))))
				      (nsd-prefix nsd)
				      (nsd-uri nsd)
				      ))
			  px)
		    (wsdl-index-form
		     conn `(defpackage ,pn (:use))))))
	    (wsdl-index-form
	     conn `(define-namespace-map ,(qt (wsdl-map-name conn))
		     ,(qt (nse-default (or message-dns (xmp-message-dns conn))))
		     ,@px))
	    (wsdl-index-form
	     conn `(defvar ,(wsdl-url-name conn)
		     ,(wsdl-soap-address conn)))
	    )
	    
	  (let (pdefs)
	    (dolist (def (wsdl-def-index conn))
	      (push
	       (cons
		(append
		 (case (first def)
		   (defun (list 1 "Defined functions:"))
		   (define-soap-element (list 2 "Defined SOAP elements:"))
		   (define-soap-type    (list 3 "Defined SOAP types:"))
		   (defparameter        (list 4 "Defined parameters:"))
		   (defpackage          (list 5 "Defined packages:"))
		   (in-package          (list 6 "Lisp package of generated file:"))
		   (otherwise           (list 7 "Other entries:"))
		   )
		 (list :key (first def)))
		(cdr def))
	       pdefs))
	    (push (list (list 0 "Output in file:" :key :file) destination) pdefs)
	    (setf pdefs (sort pdefs #'< :key #'caar))
	    (mapc #'(lambda (def) (setf (car def) (cdr (car def)))) pdefs)

	    (when (dstream)
	      (do-file (first head-forms))
	      (do-file 
	       `((eval-when (compile load eval)
		   (or (eq *current-case-mode* ',*current-case-mode*)
		       (error " ~A~S.~% ~A ~A"
			      "This file was generated in case mode "
			      ',*current-case-mode*
			      "Reading this file in a different case mode"
			      "will almost certainly fail.")))
		 "WARNING"))

	      (when (wsdl-overloaded-ops conn)
		(multiple-value-call
		 #'wsdl-print-lines (dstream) ";;; " 1
		 "The following types represent overloaded message names:"
		 (values-list (mapcar #'(lambda (n) (format nil "   ~S" n))
				      (wsdl-overloaded-ops conn)))
		 1))		 

	      (when (tstream)
		(format (tstream) "~&~%#|~%")
		(wsdl-describe conn (cdr pdefs) (tstream) 3)
		(format (tstream) "~&|#~2%"))
	      (dolist (item (cdr head-forms)) (do-file item))
	      (dolist (item (trim-defs conn))
		(do-file item))
	      (dolist (item tail-forms) (do-file item))
	      (when (and class-forms (cstream))
		(or (eq (dstream) (cstream))
		    (do-file (first head-forms) cstream))
		(wsdl-print-lines cstream ";;; " 2 "*** Class definitions" 1)
		(dolist (item class-forms) (do-file item cstream)))
	      (when (and wrap-forms (cstream))
		(or (eq (dstream) (cstream))
		    (do-file (first head-forms) cstream))
		(wsdl-print-lines cstream ";;; " 2 "*** Wrapper definitions" 1)
		(dolist (item wrap-forms) (do-file item cstream)))
	      (when (and post-forms (pstream))
		(or (eq (dstream) (pstream))
		    (do-file (first head-forms) pstream))
		(wsdl-print-lines pstream ";;; " 2 "*** More definitions" 1)
		(dolist (item post-forms) (do-file item pstream)))
	      )
	    (when verbose
	      (format t "~&~2%")
	      (wsdl-describe conn pdefs t nil)
	      (format t "~2%"))

	    (when (and (wsdl-warnings conn)
		       (not (eql 0 (wsdl-warnings conn))))
	      (error "Serious warnings in WSDL processing: ~A"
		     (wsdl-warnings conn)))
	      
	    pdefs))
      (when open (close dstream))
      (when topen (close tstream))
      (when copen (close cstream))
      (when popen (close pstream))
      )))

(defclass soap-object-class ()
  ((reader-prefix :allocation :class :initform :soap-get-)
   (writer-prefix :allocation :class :initform :soap-set-)))

(defclass soap-array-mixin ()
  ((items :initform nil :initarg :items)))

(defmethod print-object ((x soap-array-mixin) s)
  (print-unreadable-object 
   (x s :type t :identity t)
   (format s "[~A]" (length (slot-value x 'items)))))

(defmethod wsdl-emit-object-classes ((conn wsdl-file-connector) (class null))
  nil)

(defmethod wsdl-emit-object-classes ((conn wsdl-file-connector) (class symbol))
  (wsdl-emit-object-classes conn (make-instance class)))

(defmethod wsdl-emit-object-classes ((conn wsdl-file-connector)
				     (instance soap-object-class)
				     &aux
				     (prefix (wsdl-option conn :prefix))
				     (class (type-of instance))
				     (reader-prefix (slot-value instance 'reader-prefix))
				     (writer-prefix (slot-value instance 'writer-prefix))
				     form encoder defclass-names
				     (client-options (wsdl-client-options conn))
				     (cclass (getf client-options :class))
				     (defs (reverse (wsdl-def-list conn)))
				     (iname (wsdl-compose-name
					     conn nil prefix "array-items"))
				     new-name)
  (dolist (def defs)
    ;; First, collect all defined structs.
    (setf form (first def))
    (with-tree-match
     (form (define-soap-type :?any '(:? type-name)
	     '(:complex (:seq* (:?each nil (:element . :?any))) . :?any) . :?any))
     ;;(format t "~&; register complex ~A ~%" type-name)
     (pushnew type-name defclass-names)))
  (dolist (def defs)
    ;; Then, collect all defined arrays.
    (setf form (first def))
    (with-tree-match
     (form (define-soap-type :?any '(:? type-name) '(:array (:? atype) . :?any) . :?any))
     ;;(format t "~&; identify array ~A   elt type ~A~%" type-name atype)
     (when (or (member atype defclass-names)
	       (soap-known-type-p conn atype :dns))
       ;;(format t "~&; register array ~A   elt type ~A~%" type-name atype)
       (pushnew type-name defclass-names)
       t)))
  (setf (wsdl-object-classes conn) defclass-names)
  ;;(format t "~&; defclass-names = ~S~%" defclass-names)
  (dolist (def defs)
    (setf form (first def))
    (with-tree-match
     (form (define-soap-type :?any '(:? type-name) '(:? type) . :?any))
     ;;(format t "~&; defined type ~A ~%" type-name)
     (when (null encoder)
       (setf encoder (wsdl-compose-name conn nil prefix :encode))
       (setf (wsdl-encoder-name conn) encoder))
     (let* ((class-name type-name)
	    (decode-name (wsdl-compose-name conn nil :decode- (string type-name)))
	    elt elts readers writers keys names etypes)
       (setf new-name (wsdl-compose-name conn nil "new-" (string type-name)))
       (or
	(with-tree-match
	 (type (:complex (:seq* (:?each slots (:element . :?any))) . :?any))

	 ;; Emit defclass, functions new-xxx decode-xxx, method pppencode.

	 (dolist (slot slots)
	   (push (setf elt (first (second slot))) elts)
	   (push (third slot) etypes)
	   (push (wsdl-compose-name conn :keyword elt) keys)
	   (push (wsdl-compose-name conn nil elt) names)
	   (push (wsdl-compose-name conn nil reader-prefix elt)
		 readers)
	   (push (wsdl-compose-name conn nil writer-prefix elt)
		 writers)
	   )
	 (wsdl-index-form
	  conn
	  (wsdl-generate-code
	   conn :object-class :top-level
	   'defclass type-name `(,class)
	   `( ,@(mapcar #'(lambda (name key reader writer)
			    (list name
				  :reader reader
				  :writer writer
				  :initarg key
				  :initform nil))
			names keys readers writers)
		)))
	 (net.xmp.soap::wsdl-index-form
	  conn
	   (wsdl-generate-code
	    conn :object-new :top-level
	    'defun new-name `(&key ,@names)
	    `(make-instance ,(qt class-name)
			    ,@(mapcan #'(lambda (key name) (list key name))
				      keys names))))
	 (wsdl-index-form
	  conn
	  (let ((cvar (wsdl-intern-name conn "conn"))
		(fvar (wsdl-intern-name conn "soap-result")))
	    (wsdl-generate-code
	     conn :object-decoder :top-level
	     'defmethod decode-name
	     (if cclass
		 (list (list cvar cclass) (list fvar t))
	       (list (list cvar t) (list fvar t)))
	     `(make-instance
	       ,(qt class-name)
	       ,@(mapcan #'(lambda (key elt etype
					&aux
					(part (list 'soap-result-typed
						    cvar fvar
						    (list 'quote etype)
						    (case (wsdl-option conn :sequence)
						      (:set* nil)
						      (otherwise :error))
						    (string elt)))
					)
			     (when (member etype defclass-names)
			       (setf part (list (wsdl-compose-name
						 conn nil :decode- (string etype))
						cvar part)))
			     (list key part))
			 keys elts etypes)))))
	 (net.xmp.soap::wsdl-index-form
	  conn
	  (let ((fvar (wsdl-intern-name conn "instance")))
	    (wsdl-generate-code
	     conn :object-encoder :top-level
	     'defmethod encoder `((,fvar ,type-name))
	     `(list ,@(mapcan #'(lambda (elt acc etype 
					     &aux (part (list acc fvar)))
				  (when (member etype defclass-names)
				    (setf part (list encoder part)))
				  (list (string elt) part))
			      elts readers etypes)))))
	 t) ;;; end match :complex
	(with-tree-match
	 (type (:array (:? atype) . :?any))
	   
	 ;;(format t "~&; match array ~A   elt type ~A~%" type-name atype)

	 (let (array-of-built-in-type)
	   (when (or (member atype defclass-names)
		     (setf array-of-built-in-type (soap-known-type-p conn atype :dns)))
	     ;;(format t "~&; find array elt type ~A~%" atype)
	     ;; Emit defclass, function new-xxx, decode-xxx, method pppencode.

	     (net.xmp.soap::wsdl-index-form
	      conn
	      (wsdl-generate-code
	       conn :object-type :top-level
	       'defclass type-name `(,class soap-array-mixin)
	       `((items :accessor ,iname))))

	     (setf new-name (wsdl-compose-name conn nil "new-" (string type-name)))
	     (net.xmp.soap::wsdl-index-form
	      conn
	      (let ((ivar (wsdl-intern-name conn "items")))
		(wsdl-generate-code
		 conn :object-new :top-level
		 'defun new-name `(&rest ,ivar)
		 `(when (and ,ivar (null (cdr ,ivar))
			     (typep (first ,ivar) 'sequence))
		    (setf ,ivar (concatenate 'list (first ,ivar))))
		 `(make-instance ,(qt class-name) :items ,ivar))))
	     (net.xmp.soap::wsdl-index-form
	      conn
	      (let ((cvar (wsdl-intern-name conn "conn"))
		    (aname (when (null array-of-built-in-type)
			     (wsdl-compose-name conn nil :decode- (string atype))))
		    (fvar (wsdl-intern-name conn "soap-result"))
		    (ivar (wsdl-intern-name conn "items"))
		    (tvar (wsdl-intern-name conn "tail"))
		    )
	     
		(wsdl-generate-code
		 conn :object-decoder :top-level
		 'defmethod decode-name
		 (if cclass
		     (list (list cvar cclass) (list fvar t))
		   (list (list cvar t) (list fvar t)))
		 `(make-instance ,(qt class-name)
				 :items
				 (let ((,ivar (concatenate 'list ,fvar nil)))
				   ,(if array-of-built-in-type
					ivar
				      `(do ((,tvar ,ivar (cdr ,tvar)))
					   ((null ,tvar) ,ivar)
					 (setf (car ,tvar)
					       (,aname ,cvar (car ,tvar))))))
				 ))))
	     (net.xmp.soap::wsdl-index-form
	      conn
	      (let ((fvar (wsdl-intern-name conn "instance"))
		    (ivar (wsdl-intern-name conn "item")))
		(wsdl-generate-code
		 conn :object-encoder :top-level
		 'defmethod encoder `((,fvar ,type-name))
		 (if array-of-built-in-type
		     `(,iname ,fvar)
		   `(mapcar #'(lambda (,ivar) (,encoder ,ivar)) 
			    (,iname ,fvar)))
		 )))
	     t))) ;;; end match :array
	)
       )))
  nil)


(defmethod wsdl-emit-wrappers ((conn wsdl-file-connector) defs
			       &aux msg message reply)

  (when (and (wsdl-encoder-name conn)
	     (wsdl-option conn :message-method-prefix))

    (dolist (def defs)
      
      ;; Do one (separate) pass to generate call wrappers
      ;;    BEFORE collapsing array types.

      (or
       (with-tree-match
	(def ((:? form (define-soap-element :?any 
			 (:?or (quote (:? name)) (:? name))
			 . :?any))
	      . :?any))
	;; save element defs for use in defun
	(typecase name
	  (symbol
	   ;; must be name of outgoing message
	   (setf message form reply nil))
	  (string
	   ;; must be name of reply
	   (if reply (setf message nil reply nil) (setf reply form))))
	t)

       (with-tree-match
	(def ((defun (:? defun-name) . :?any) . (:? comments)))

	(when (and (setf msg (first (last comments)))
		   (symbolp msg) message reply)
	  (with-tree-match
	   (message (define-soap-element :?any :?any 
		      '(:complex
			(:seq (:?each parts (:element . :?any)))
			. :?any)
		      . :?any ))
	   (with-tree-match
	    (reply  (define-soap-element :?any 
		      (:? out-name) '(:? out-type) . :?any ))
	    
	    (wsdl-emit-wrapper conn msg defun-name parts out-name out-type)
	    
	    ))
	  (setf message nil reply nil)
	  t))
       )))
  
  (when (or (wsdl-option conn :built-in-arrays)
	    (wsdl-option conn :defined-arrays))
	    
    (dolist (def defs)

      ;; Do one (separate) pass to collapse array types.

      (with-tree-match
       (def (((:?or define-soap-type define-soap-element)
	      :?any
	      :?any 
	      '(:complex ((:?or :seq :seq*)
			  (:?each elts (:element . :?any)))
			 . :?any)
	      . :?any)
	     . :?any))
       (wsdl-collapse-array-types conn elts)
       )
      ))

  (when (eq :set* (wsdl-option conn :sequence))
    (dolist (def defs)
      (with-tree-match
       (def ((define-soap-type
	       :?any
	       :?any 
	       '(:complex (:? seq (:seq* . :?any)) . :?any)
	       . :?any)
	     . :?any))
       (setf (first seq) :set*))))

  )

(defmethod wsdl-collapse-array-types ((conn wsdl-file-connector) elts)
  (dolist (e elts)
    (when (symbolp (third e))
      (let (btype 
	    (aopts (append
		    (when (wsdl-option conn :send-atype)
		      (list :send-atype t))
		    (when (wsdl-option conn :send-asize)
		      (list :send-asize t))))
	    (def (wsdl-resolve-type conn (third e))))
	(with-tree-match 
	 (def (:array (:? atype) . :?any))
	 (cond
	  ((setf btype (soap-known-type-p conn atype :dns))
	   (case (wsdl-option conn :built-in-arrays)
	     (:collapse

	      ;; Replace type name with explicit type declaration.

	      (setf (third e) (list* :array btype aopts))))
	   )
	  (t (case (wsdl-option conn :defined-arrays)
	       (:collapse

		;; Replace type name with explicit type declaration.

		(setf (third e) (list* :array atype aopts)))))

	  ))))))
  

(defmethod wsdl-emit-wrapper ((conn wsdl-file-connector)
			      msg defun-name parts out-name out-type
			      &aux
			      call res
			      (structs (wsdl-object-classes conn))
			      (encoder (wsdl-encoder-name conn))
			      (bvar (wsdl-compose-name conn nil "body"))
			      (cvar (wsdl-compose-name conn nil "conn"))
			      (hvar (wsdl-compose-name conn nil "headers"))
			      (mprefix (wsdl-option conn :message-method-prefix))
			      )

	      
  ;; emit wrapper methods for wsdl-generated defun-s
  ;; methods take positional args specialized on decoded object classes

  (setf call 
	`(,defun-name
	   ,@(mapcan
	      #'(lambda (elt &aux (ename (first (second elt))))
		  (list (wsdl-compose-name conn :keyword ename)
			(if (member (third elt) structs)
			    (list encoder (wsdl-compose-name conn nil ename))
			  (wsdl-compose-name conn nil ename)))) 
	      parts)))
  (or
   (with-tree-match
    (out-type  (:complex
		(:seq
		 (:? out-elt (:element . :?any)))
		. :?any))
    (setf res (list 'soap-result-only
		    cvar bvar :error
		    (list 'quote out-name)
		    (list 'quote 
			  (first (second out-elt)))))
    (when (member (third out-elt) structs)
      (setf res (list (wsdl-compose-name
		       conn nil
		       "decode-" (string (third out-elt)))
		      cvar res)))
    t)
   (setf res bvar))
		    
  (wsdl-index-form
   conn
   (wsdl-generate-code
    conn :object-wrapper :top-level
    'defmethod (wsdl-compose-name conn nil mprefix (string  msg))
    `(,@(mapcar #'(lambda (elt)
		    (list (wsdl-compose-name conn nil (first (second elt)))
			  (if (member (third elt) structs)
			      (third elt)
			    t)))
		parts))
    `(multiple-value-bind (,bvar ,hvar ,cvar) 
	 ,call
       (values ,res ,hvar ,cvar))

    ))
  )



(defun describe-soap-method (elt &key wsdl (stream t) depth)
  (let* ((type (or (when wsdl (wsdl-lookup wsdl :element elt :complex))
		   (soap-find-element wsdl elt :dns)))
	 (parts (when (and (consp type) (eq :complex (car type)))
		  (cdr (second type))))
	 (ret (concatenate 'string (string elt) "Response"))
	 (rtype (or (when wsdl (wsdl-lookup wsdl :element ret :complex))
		    (soap-find-element wsdl ret :dns)))
	 (rparts (when (and (consp rtype) (eq :complex (car rtype)))
		  (cdr (second rtype))))
	 (indent 6) l)
    (format stream "~&~VAMessage ~S takes ~A argument~A~%"
	    indent "" elt (setf l (length parts))
	    (case l (0 "s") (1 ":") (otherwise "s:")))
    (dolist (part parts)
      (describe-soap-element part :wsdl wsdl :stream stream :depth depth))
    (format stream "~&~VAand returns a result ~A containing the element(s):~%"
	    indent "" ret)
    (dolist (part rparts)
      (describe-soap-element part :wsdl wsdl :stream stream :depth depth))
    stream))

(defun describe-soap-element (eltspec &key wsdl (stream t) depth (indent 8))
  (let (name type)
    (cond ((atom eltspec)
	   (or (when wsdl (setf type (wsdl-lookup wsdl :element eltspec nil))
		     (setf name eltspec))
	       (multiple-value-setq (type name)
		 (soap-find-element wsdl eltspec :dns))))
	  ((and (consp eltspec) (eq :element (first eltspec)))
	   (setf name (xmp-pick-name nil eltspec)
		 type (third eltspec))))
    (cond (stream (format stream "~&~VAThe element ~S of type "
			  indent "" name)
		  (describe-soap-type
		   type :wsdl wsdl :stream stream :depth depth :indent indent)
		  (format stream "~&"))
	  (t (format stream "~&~VAThe element ~S of type ~A~%"
		     indent "" name 
		     (describe-soap-type type :wsdl wsdl :stream nil :depth nil))))
    stream))

(defun describe-soap-type (type &key wsdl (stream t) depth (indent 8) &aux res)
  (setf res
	(cond ((symbolp type) (format stream "~S" type))
	      ((consp type)
	       (case (first type)
		 (:simple (format stream "~S" (setf type (second type))))
		 (:array (format stream "array of ~S" (setf type (second type))))
		 (:complex
		  (cond
		   ((or (null stream) (null depth) (eql depth 0))
		    (format stream "composite with ~A sub-element(s)"
			    (length (cdr (second type)))))
		   (t (format stream "composite with ~A sub-element(s):~%"
			      (length (cdr (second type))))
		      (dolist (elt (cdr (second type)))
			(describe-soap-element elt 
					       :wsdl wsdl
					       :stream stream 
					       :depth (when depth (1- depth))
					       :indent (+ indent 2)
					       )))))))))
  (when (and stream type (symbolp type) depth (not (eql depth 0)))
    (incf indent 2) (decf depth)
    (let ((def (or (when wsdl (wsdl-lookup wsdl :type type nil))
		   (soap-find-type nil type :dns))))
      (cond ((null def)
	     (format stream "~&~VAThe type ~S is " indent "" type)
	     (format stream "undefined."))
	    ((and (consp def) (eq :simple (first def)) (null (second def))))
	    (t (format stream "~&~VAThe type ~S is " indent "" type)
	       (describe-soap-type
		def :wsdl wsdl :stream stream :depth depth :indent indent)))))

  res)

		    



;;;
;;; XMP output methods to generate WSDL file from Lisp server def.
;;;

(defvar *wsdl-file-depth* nil)

(defun encode-wsdl-file (file &key
			      (namespaces '(:net.xmp.wsdl))
			      (base (list nil :wsdl1-namespaces :all))
			      servers types elements
			      (target "urn:ThisWebServiceNamespace" t-p)
			      target-package
			      name
			      (if-exists :supersede)
			      (transport "http://schemas.xmlsoap.org/soap/http")
			      &aux (*xmp-warning-leader* "WSDL"))
  
  
  (let* (dns
	 (server-dns (define-namespace-map nil))
	 (server-base (define-namespace-map nil))
	 (server-tail (define-namespace-map nil))
	 (message-dns (apply 'define-namespace-map nil 
			     (nse-default namespaces)
			     (append
			      ;; insert a place where each server
			      ;; message-dns can be spliced 
			      ;; in front of all others
			      (list server-dns)
			      (nse-defs namespaces :one :level))))
	 (base-dns    (apply 'define-namespace-map nil 
			     nil
			     (append 
			      ;; insert a place where each server
			      ;; base-dns can be spliced 
			      ;; in front of generic base
			      (list server-base)
			      (nse-defs base :one :level)
			      ;; insert a place where each server
			      ;; base tail can be spliced 
			      ;; in front of generic tail
			      (list server-tail)
			      (list (nse-tail base)))))
	 (conn (make-instance 'wsdl-file-connector
			      :message-dns message-dns
			      :base-dns    base-dns 
			      ))
	 (wpk (net.xmp::xmp-package-to-prefix conn :net.xmp.wsdl :dns))
	 type-defs messages ports bindings services
	 (*wsdl-file-depth* 1)
	 seen error-messages target-uri used-namespaces)

    (typecase target-package
      (null nil)
      (package nil)
      ((or string symbol) (if (find-package target-package)
			      (setf target-package (find-package target-package))
			    (error "target-package not found: ~S" target-package)))
      (otherwise (error "target-package must denote a package: ~S" target-package)))

    (setf (wsdl-defined-elements conn) nil
	  (wsdl-defined-types conn) nil
	  (wsdl-counters conn) nil
	  (wsdl-name-prefix conn) (if name (string name) "SOAPServer")
	  )
    (setf dns (xmp-normal-nse (xmp-message-dns conn)))
    (cond
     ((nse-default dns))
     (wpk (setf (xnm-default dns)
		(xnd-uri (xmp-find-namespace :net.xmp.wsdl nil nil))))
     (t (error "Cannot determine default namespace.")))

    (cond ((null t-p) (setf target-uri target))
	  ((null target) (error "Target namespace is required."))
	  (t (typecase target
	       (symbol nil)
	       (string (setf target-uri target))
	       (xmp-namespace-declaration (setf target-uri (xnd-uri target)))
	       (net.uri:uri (setf target-uri (format nil "~A" target)))
	       (otherwise (error "Ill-formed :target argument ~S" target)))))


    (labels ((scan
	      (item &aux pk nsd)
	      (typecase item
		(null nil)
		(cons (mapc #'scan item))
		(symbol (when (setf pk (symbol-package item))
			  (setf pk (intern (package-name pk) :keyword))
			  (or (assoc pk used-namespaces)
			      (setf nsd (xmp-search-map message-dns :package pk))
			      (setf nsd (xmp-search-map base-dns :package pk))
			      (error "Undefined namespace for package of ~S" item))
			  (when nsd
			    ;;(format t "~&;~S ~S ~S~%" item pk nsd)
			    (pushnew 
			     (list (xnd-package nsd) (xnd-prefix nsd)
				   (xnd-uri nsd))
			     used-namespaces :test #'equal)))))))
      (dolist (e elements) (wsdl-add-element-def conn e))
      (dolist (d types) (wsdl-add-type conn d))
      (dolist (s (etypecase servers
		   (null nil)
		   (xmp-connector (list servers))
		   (cons (etypecase (first servers)
			   (string (list servers))
			   (xmp-connector servers)
			   (cons servers)))))
	(let* ((service-name (when (consp s) (first s)))
	       (server (if (consp s) (second s) s))
	       (port-name (or (soap-port-name server) (wsdl-make-name conn "Port")))
	       (binding-name (or (soap-binding-name server)
				 (wsdl-make-name conn "Binding")))
	       (tables (xmp-server-exports server))
	       port-ops bind-ops service-ports tns
	       target-prefix target-pk)
	  
	  (setf (xnm-default server-dns) (nse-default (xmp-message-dns server))
		(xnm-entries server-dns) (nse-defs (xmp-message-dns server))
		(xnm-entries server-base) (nse-defs (xmp-base-dns server))
		(xnm-tail server-tail) (nse-tail (xmp-base-dns server)))

	  ;; Revised target handling [bug15509]
	  ;; Must be done for each server.
	  (cond ((symbolp target)
		 (cond ((setf tns (xmp-search-map message-dns :package target))
			(cond ((null target-uri)
			       (setf target-uri (xnd-uri tns)))
			      ((equal target-uri (xnd-uri tns)))
			      (t (error
				  "More than on targetNamespace in wsdl:definition")))
			(setf target-prefix (xnd-prefix tns)
			      target-pk (xnd-package tns)))
		       (t (error "Cannot find targetNamespace from package ~A" 
				 target))))
		((setf tns (xmp-search-map message-dns :uri target-uri))
		 (setf target-prefix (xnd-prefix tns)
		       target-pk (xnd-package tns))))
	  (or target-prefix
	      ;; if prefix is given, assume it is unique
	      ;; otherwise make up a new one
	      (loop (setf target-prefix (symbol-name (gensym "tns")))
		    (or (xmp-search-map message-dns :prefix target-prefix)
			(xmp-search-map base-dns :prefix target-prefix)
			(return))))
	  ;; If target-pk is nil, all top-level elements are mapped to 
	  ;; target namespace.
	  (setf (wsdl-target-package conn) target-pk)
	  (when target-pk
	    (pushnew (list target-pk target-prefix target-uri)
		     used-namespaces :test #'equal))

	  (or service-name
	      (setf service-name (or (soap-service-name server)
				     (wsdl-make-name conn "Service"))))
	  (push `((wsdl:|port| "name" ,port-name "binding"
			,(wsdl-make-tname conn binding-name))
		  ((wsoap:|address| "location" 
			  ,(or (xmp-destination-url server) "???")))
		  )
		service-ports)
	  (when (null (xmp-destination-url server))
	    (push
	     (format nil "The service ~A does not specify a URI - using ??? "
		     service-name)
	     error-messages))

	  ;; Look through the tables normally accessed with xmp-lookup
	  (dotimes (i 3)
	    (maphash #'(lambda (op-elt v)
			 ;; scan signatures and generate message elements
			 (dolist (def v)
			   (let* ((sig (first def))
				  (action (first sig))
				  (op-name (string op-elt))
				  (res (second def))
				  (inmsg (string op-elt))
				  (inmsgq (wsdl-make-tname conn op-elt))
				  (rpos (search "Request" inmsg))
				  (outmsg (cond ((and rpos
						      (eql (length inmsg)
							   (+ rpos
							      (length "Request"))))
						 (concatenate 'string
							      (subseq inmsg 0 rpos)
							      "Response"))
						(t (concatenate
						    'string inmsg "Response"))))
				  (outmsgq (wsdl-make-tname conn outmsg))
				  (tns (list "namespace" target-uri))
				  (body `((wsoap:|body|
						 "use" "encoded"
						 ,@tns
						 "encodingStyle" 
						 ,(soap-encoding-style server)
						 )))
				  )
			     (when (or 
				    ;; default behavior
				    (null target-package)
				    ;; no package
				    (not (symbolp op-elt))
				    ;; skip elements not in target-package
				    (eq target-package (symbol-package op-elt)))
			       (when (symbolp op-elt)
				 (or target-pk
				     ;; If target package was not specified, then
				     ;;  just verify that all elements are in the
				     ;;  same package.
				     (pushnew
				      (list target-pk target-prefix target-uri)
				      used-namespaces :test #'equal)
				     (setf target-pk (symbol-package op-elt)))
				 (or (eq target-pk (symbol-package op-elt))
				     ;; [bug15509]
				     (error
				      "Message element is not in targetNamespace: ~S"
				      op-elt)))
			       (push `((wsdl:|operation| "name" ,op-name)
				       ((wsoap:|operation| "soapAction" ,(or action "")))
				      
				       (wsdl:|input| ,body)
				       (wsdl:|output| ,body)
				       )
				     bind-ops)
			       (push `((wsdl:|operation| "name" ,op-name)
				       ((wsdl:|input| "message" ,inmsgq))
				       ((wsdl:|output| "message" ,outmsgq)))
				     port-ops)
			       (push `((wsdl:|message| "name" ,inmsg)
				       ,@(wsdl-message-parts conn op-elt))
				     messages)
			       (push `((wsdl:|message| "name" ,outmsg)
				       ,@(wsdl-message-parts conn res))
				     messages)

			       ))))
		     (aref tables i)))
	  (push `((wsdl:|portType| "name" ,port-name) ,@(reverse port-ops))
		ports)
	  (push `((wsdl:|binding|
			"name" ,binding-name
			"type" ,(wsdl-make-tname conn port-name))
		  ((wsoap:|binding|
			  "style" "rpc"
			  "transport" ,transport
			  ))
		  ,@(reverse bind-ops))
		bindings)
	  (push `((wsdl:|service| "name" ,service-name)
		  ,@(reverse service-ports))
		services)

	  ;; Need to do the scan after each iteration because message-dns
	  ;;  and base-dns may change for each service.
	  (scan type-defs)
	  (scan messages)
	  (scan ports)
	  (scan bindings)
	  (scan services)
	  ;;(format t "~&;~S~%" used-namespaces)
	  ))

      (setf (xnm-default server-dns) nil
	    (xnm-entries server-dns) nil
	    (xnm-entries server-base) nil
	    (xnm-tail server-tail) nil)

      ;; scan defined-elements and defined-types to build type-defs
      ;;  defined-elements -> ((elt-name type-name [attributes]) ...)
      ;;  defined-types    -> ({ type-name | (type-name type-def) } ...)
      (dolist (def (wsdl-defined-elements conn))
	(push `((xsd:|element| "name" ,(string (first def)) "type" ,(second def)
		     ,@(let (props)
			 (do ((tl (third def) (cddr tl)))
			     ((atom tl) (reverse props))
			   (case (first tl)
			     (:nillable (when (second tl)
					  (push 'xsd:|nillable| props)
					  (push "true" props))))))
		     ))
	      type-defs))
      (setf seen nil)
      (loop
       (with-slots
	(defined-types) conn
	(let ((old defined-types))
	  (setf defined-types nil)
	  (dolist (def old)
	    (or
	     (member def seen)
	     (let* ((name (if (consp def) (first def) def))
		    (tdef (if (consp def)
			      (second def)
			    (or
			     (wsdl-lookup conn :type name nil)
			     (soap-find-type conn name :dns))))
		    )
	       (cond ((soap-known-type-p
		       ;; do not emit declarations for pre-defined types [bug15454]
		       conn name :top
		       :net.xmp.schema :net.xmp.soap.encoding))
		     ((null tdef) (error "Undefined type ~S" name))
		     (t (push def seen)
			(push (wsdl-encode-type-def conn name tdef) type-defs)
			)))))
	  (or defined-types (return)))))

      ;; Scan again since some of these may have changed.
      (scan type-defs)
      (scan messages)
      (scan ports)
      (scan bindings)
      (scan services)
      )
   
    (with-open-file
     (s file :direction :output :if-exists if-exists)

     ;; save s in conn
     (setf (wsdl-file-stream conn) s)

     (xmp-message-begin conn)
     (xmp-encode-content conn
			 (format nil
				 "<?~A?>" 
				 (xmp-destination-leader conn))
			 :sanitize nil)
     (format s "~&~%")

     (when (not t-p)
       (push "A target namespace was not specified" error-messages)
       (push (format nil "  - using the default URI '~A'." target) error-messages))
     (dolist (m (reverse error-messages))
       (xmp-encode-content conn (format nil "<!-- ~A -->~%" m) :sanitize nil))
     (format s "~&~%")

     

     (xmp-encode-begin
      conn 'wsdl:|definitions| 
      :dns nil
      ;; [bug15509] we have verified that target namespace is in message-dns
      ;;  so it does not need to be mentioned here
      :namespaces (list* (nse-default namespaces)
			 (append used-namespaces
				 (nse-defs namespaces :one :level)))

      :attributes (list "targetNamespace" target-uri)
      )

     (when type-defs
       (xmp-encode conn 
		   `(wsdl:|types|
			  ((xsd:|schema| "targetNamespace" ,target-uri)
			   ,@type-defs))
		   nil))
     (dolist (m (reverse messages))
       ;; (wsdl:|message| message-def)*
       (xmp-encode conn m nil))
     
     (dolist (p (reverse ports))
       ;; (wsdl:|portType| port-def)*
       (xmp-encode conn p nil))

     (dolist (b (reverse bindings))
       ;; (wsdl:|binding| bdef)*
       (xmp-encode conn b nil))

     (dolist (s (reverse services))
       ;; (wsdl:|service| sdef)*
       (xmp-encode conn s nil))

     (format s "~&")
     (xmp-encode-end conn 'wsdl:|definitions|)
     (format s "~&"))
    file
    ))

(defmethod wsdl-message-parts ((conn wsdl-file-connector) elt
			       &aux
			       def)
  ;; find definition and make list of parts
  (setf def (soap-find-element conn elt :dns))
  (cond
   ((null def)
    (error "Cannot find element definition ~S" elt))

;;; If message is defined as a sequence of elements with named types
;;;    then emit parts <part name=eltname type=elttype />

;;; ??? ONLY in <choice> case ???
;;; If message is defined as a sequence of named elements with complicated types
;;;    then emit parts <part name=Partxxx element=eltname />

;;; If message element is defined some other way
;;;    then emit one <part name=Partxxx type=elttype />

   (t (let* (kind
	     (body (typecase def
		     (atom (setf kind :simple) def)
		     (cons (setf kind (first def))
			   (second def))))
	     (collector (when (consp body) (first body)))
	     (cparts (when (consp body) (cdr body)))
	     elements ctypes named cprops
	     )
	(cond
	 ((and (eq kind :complex)
	       (member collector '(:seq :seq1 :set :set1))
	       (let (ctype cprop)
		 (setf named t)
		 (dolist (cpart cparts t)
		   (or (and (consp cpart)
			    (or (eq (first cpart) :element)
				(return nil))
			    (setf ctype (third cpart))
			    (symbolp ctype))
		       (and (atom cpart)
			    (setf ctype 
				  (or (multiple-value-bind (v1 v2 pl)
					  (wsdl-lookup conn cpart :element nil)
					(declare (ignore v2))
					(when v1 (setf cprop pl) v1))
				      (multiple-value-bind (v1 v2 v3 pl)
					  (soap-find-element conn cpart :dns)
					(declare (ignore v2 v3))
					(when v1 (setf cprop pl) v1)))))
		       (setf named nil))
		   (push (xmp-pick-name conn cpart) 
			 elements)
		   (push ctype ctypes)
		   (push cprop cprops)
		   )))
	  (reverse
	   (mapcar #'(lambda (elt type props &aux tname)
		       (if* named
			    then
			    (setf tname (wsdl-add-type conn type))
			    `((wsdl:|part|
				    "name" ,(string elt)
				    "type" ,tname))
			    else
			    (wsdl-add-element conn elt type props)
			    `((wsdl:|part|
				    "name" ,(wsdl-make-name conn "Part")
				    "element" ,(typecase elt
						 (symbol elt)
						 (string (wsdl-make-tname conn elt)))
				    ))))
		   elements ctypes cprops)))
	 ;; Method call must be a struct of named parameters
	 ;;  and reply must be a struct of result and
	 ;;   named in/out parameters.
	 (t (error 
	     "SOAP message is not seq of named parameters ~S ~S"
	     elt def))
	 )))))

(defmethod wsdl-add-element-def ((conn wsdl-file-connector) elt &aux def)
  (setf def (soap-find-element conn elt :dns))
  (cond
   ((null def)
    (error "Cannot find element definition ~S" elt))
   (t    (wsdl-add-element conn elt def nil) )))

(defmethod wsdl-make-name ((conn wsdl-file-connector) prefix &aux place)
  (with-slots
   (counters name-prefix) conn
   (setf place (assoc prefix counters :test #'string-equal))
   (or place (push (setf place (cons prefix 0)) counters))
   (incf (cdr place))
   (if (eql 1 (cdr place))
       (typecase prefix
	 (symbol    (format nil "~A~(~A~)" name-prefix prefix))
	 (otherwise (format nil "~A~A" name-prefix prefix)))
     (typecase prefix
       (symbol
	(format nil "~A~(~A~)~3,'0D"
		name-prefix prefix (cdr place)))
       (otherwise (format nil "~A~A~3,'0D"
			  name-prefix prefix (cdr place))))
     )))

(defmethod wsdl-make-tname ((conn wsdl-file-connector) name)
  (with-slots (target-package) conn
	      (if target-package
		  (intern (format nil "~A" name) target-package)
		(format nil "~A" name))))

(defmethod wsdl-add-element ((conn wsdl-file-connector) elt type props)
  (with-slots
   (defined-elements defined-types) conn
   (let ((def (assoc elt defined-elements :test #'equal)) def2)
     (if def
	 (or (equal type (second def))
	     (and (atom (second def))
		  (setq def2 (member (second def) defined-types
				     :test #'equal
				     :key #'(lambda (x) (if (consp x) (car x) x))
				     ))
		  (or (equal type (first def2))
		      (and (consp (first def2))
			   (equal type (second (first def2))))))
	     (error "Two defs of the same element ~S ~S ~S ~S"
		    elt (second def) (second def2) type))
       (push (list* elt (wsdl-add-type conn type) (when props (list props)))
	     defined-elements))
     nil)))

(defmethod wsdl-add-type ((conn wsdl-file-connector) type)
  (with-slots
   (defined-types) conn
   (let (name)
     (when (consp type)
       (case (first type)
	 (:simple (when (second type) (setf type (second type))))))
     (when (not (symbolp type))
       (setf name (wsdl-make-tname conn (wsdl-make-name conn "Type")))
       (setf type (list name type)))
     (or (and (symbolp type)
	      (eq (symbol-package type)
		  (find-package :net.xmp.schema)))
	 (pushnew type defined-types :test #'equal))
     (or name type))))

(defmethod wsdl-encode-type-def ((conn wsdl-file-connector) name indef
				 &key attributes
				 &aux tdef min max schema)
  (setf tdef indef)
  (and tdef (atom tdef)
       ;; In this case, it may be an element name [bug15508]
       (setf tdef (wsdl-resolve-element conn tdef))
       (setf tdef (wsdl-resolve-type conn tdef))
       )
  (case (first tdef)

    ;; If element type is not named, then we can emit
    ;;    an anonymous type in the body of the element.

    (:element
     `((xs:|element|
	   ,@(let ((name (xmp-pick-name conn tdef)))
	       (when name
		 (list
		  "name"
		  ;; The name attribute seems to be always unqualified ???
		  (string name))))
	   "type" ,(wsdl-add-type conn (third tdef))
	   ,@(when (getf (cdddr tdef) :nillable) (list :nillable t))
	   ,@attributes)))
    (:any
     `((xs:|element|
	   "type" xsd:|anyType|
	   ,@attributes)))
    (:complex
     (or (case (first (second tdef))
	   ;;  :seq  ->  sequence min=0 max=1
	   (:seq (setf schema 'xs:|sequence| min 0))
	   ;;  :seq1 ->  sequence min=1 max=1
	   (:seq1 (setf schema 'xs:|sequence|))
	   ;;  :seq+ ->  sequence min=1
	   (:seq+ (setf schema 'xs:|sequence| max "unbounded"))
	   ;;  :seq* ->  sequence min=0
	   (:seq* (setf schema 'xs:|sequence| min 0 max "unbounded"))
	   ;;  :set  ->  all   min=0 max=1
	   (:set  (setf schema 'xs:|all| min 0))
	   ;;  :set1 ->  all   min=1 max=1
	   (:set1 (setf schema 'xs:|all|))
	   ;;  :set+ ->  NA
	   ;;  :set* ->  NA
	   ;;  :or ->  choice
	   (:or (setf schema 'xs:|choice|))
	   )
	 (error "Unrecognized type def ~S" tdef))
     `(,(if name
	    `(xs:|complexType| "name" ,(string name))
	  'xs:|complexType|)
       (,schema 
	,@(mapcar #'(lambda (part) (wsdl-encode-type-def 
				    conn nil part
				    :attributes
				    (append
				     (when min
				       (list "minOccurs" min))
				     (when max
				       (list "maxOccurs" max)))))
		  (cdr (second tdef))))))
    (:array
     `(,(if name
	    `(xs:|complexType| "name" ,(string name))
	  'xs:|complexType|)
       (xs:|complexContent|
	   ((xs:|restriction| "base" enc:|Array|)
	    ((xs:|attribute|
		 "ref" enc:|arrayType|
		 wsdl:|arrayType|
		 ,(soap-encoded-array-type
		   conn (wsdl-add-type conn (second tdef)) :dns)))))))
    (:simple
     `((xs:|simpleType| ,@(when name `("name" ,(string name))) 
	   "type" ,(second tdef))))
    (otherwise
     (error "Unrecognized type def ~S ~S" indef tdef)))
  )
	     

(defmethod xmp-encode ((conn wsdl-file-connector) (item t) (type t)
		       &rest options &key &allow-other-keys
		       &aux elt attrs parts (s (wsdl-file-stream conn))
		       )
  ;; item -> element-name
  ;;      -> (element-name [item] ... )
  ;;      -> ((element-name [attr-name value] ... ) [item] ... )
  (cond ((null item))
	((atom item) (setf elt item))
	(t (setf elt (first item) parts (cdr item))
	   (when (consp elt)
	     (setf attrs (cdr elt))
	     (setf elt (car elt)))))
  (when elt
    (when *wsdl-file-depth*
      (format s "~&~VA" (* 2 *wsdl-file-depth*) ""))
    (cond (parts
	   (let ((*wsdl-file-depth* (when *wsdl-file-depth* (1+ *wsdl-file-depth*))))
	     (xmp-encode-begin conn elt :attributes attrs)
	     (dolist (part parts) (xmp-encode conn part nil)))
	   (when *wsdl-file-depth*
	     (format s "~&~VA" (* 2 *wsdl-file-depth*) ""))
	   (xmp-encode-end conn elt))
	  (t (xmp-encode-begin conn elt :attributes attrs :empty t)))))


(defmethod xmp-content-string ((conn wsdl-file-connector) data 
			       &key (sanitize t) string
			       &allow-other-keys)
  (if (and string (eq string (xmp-message-string conn)))
      (let ((s (call-next-method conn data :sanitize sanitize))
	    (stream (wsdl-file-stream conn))
	    )
	(format stream "~A" s)
	s
	)
    (call-next-method)))


  
(defmethod xmp-encode-begin :around ((conn wsdl-file-connector) (elt t) &rest options 
			      &key namespaces attributes &allow-other-keys)
  (let ((*wsdl-file-depth*
	 (if (or (eq elt 'wsdl:|definitions|)
		 (> (+
		     (if (first namespaces) 2 0)
		     (if namespaces (* 2 (length namespaces)) 0)
		     (if attributes (length attributes) 0))
		    5))
	     (+ 2 *wsdl-file-depth*)
	   nil)))
    (call-next-method)))

(defmethod xmp-encode-attribute :before ((conn wsdl-file-connector)
					 (prefix t) (suffix t) (name t)
					 (value t) (qname t))
  (when *wsdl-file-depth*
    (format (wsdl-file-stream conn) "~&~VA" (* 2 *wsdl-file-depth*) "")))

(defparameter *wsdl-needed-namespaces*
  '(
    ;; (url-sub-string package-name nsprefix)

    ("schemas.xmlsoap.org/wsdl" :net.xmp.wsdl "wsdl")

    ))

(defparameter *wsdl-optional-namespaces*
  '(
    ("http://www.w3.org/1999/XMLSchema"    :net.xmp.schema "xsd")
    ("http://www.w3.org/1999/XMLSchema/"   :net.xmp.schema "xsd")
    ("http://www.w3.org/2000/10/XMLSchema" :net.xmp.schema "xsd")
    ("http://www.w3.org/2001/XMLSchema"    :net.xmp.schema "xsd")
    ("http://www.w3.org/2001/XMLSchema/"   :net.xmp.schema "xsd")

    ("/wsdl/soap" :net.xmp.wsdl.soap "wsdl-soap")
    ("/wsdl/soap12" :net.xmp.wsdl.soap "wsdl-soap12")

    ("/XMLSchema-instance" :net.xmp.schema-instance "xsi")
    ("org/soap/encoding" :net.xmp.soap.encoding "SOAP-ENC")
    ("org/soap/envelope" :net.xmp.soap.envelope "SOAP-ENV")
    ))

(defun decode-wsdl-namespaces (&key string file uri (map :wsdl1-prefix))
  (xmp-decode-namespaces :pns (xmp-extract-namespaces
				  :string string :file file :uri uri)
			    :needed *wsdl-needed-namespaces*
			    :optional *wsdl-optional-namespaces*
			    :known map))


(defmethod wsdl-warn ((conn wsdl-file-connector) &rest fmt)
  (if (wsdl-warnings conn)
      (incf (wsdl-warnings conn))
    (setf (wsdl-warnings conn) 1))
  (apply 'warn fmt))


;;; This is defined here instead of in xmp-soap.cl 
;;;  because we want to call define-wsdl-names as well.
(defun soap-new-environment ()
  (setf *soap-server* nil
	*soap-last-server* nil
	)
  (xmp-new-environment)

  (net.xmp::define-schema-elements)
  (define-soap-names)
  (define-wsdl-names)
  nil)
