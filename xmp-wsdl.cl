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

;; $Id: xmp-wsdl.cl,v 2.4 2004/04/23 22:07:05 mm Exp $

;; WSDL support

;;; targetNamespace attribute in wsdl:definitions
;;;    applies to message port and service names
;;; namespace attribute in soap:body
;;;    applies to operation names     
;;; elements in message are not in any namespace

(in-package :net.xmp.soap)

(defpackage :net.xmp.soap 
  (:export 
   #:*wsdl-default-namespaces*
   #:*wsdl-1.1-namespaces*
   #:*wsdl-1.2-namespaces*
   #:decode-wsdl-file
   #:decode-wsdl-string
   #:decode-wsdl-at-uri
   #:decode-wsdl-source
   #:decode-wsdl-namespaces
   #:encode-wsdl-file
   #:wsdl-service-names
   #:make-client-interface
   #:make-server-interface
   #:*application-namespaces*
   #:describe-soap-method
   #:describe-soap-element
   #:describe-soap-type
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
     #:|targetNamespace|
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
     ;; #:||
     ))

  )

(eval-when (compile)
  (defpackage :net.xmp.wsdl (:use) (:nicknames :wsdl))
  (defpackage :net.xmp.wsdl.soap (:use) (:nicknames :wsoap))
  )

(defparameter *wsdl-1.1-namespaces*
  ;;
  ;; From: Web Services Description Language (WSDL) 1.1
  ;;       W3C Note 15 March 2001
  ;;       This version: http://www.w3.org/TR/2001/NOTE-wsdl-20010315
  ;;
  '(nil
    (:net.xmp.wsdl
     "wsdl"
     "http://schemas.xmlsoap.org/wsdl/"
     ;; WSDL namespace for WSDL framework.
     )
    (:net.xmp.wsdl.soap
     "soap"
     "http://schemas.xmlsoap.org/wsdl/soap/"
     ;; WSDL namespace for WSDL SOAP binding.
     )
    ;; http http://schemas.xmlsoap.org/wsdl/http/ 
    ;;      WSDL namespace for WSDL HTTP GET & POST binding.
    ;; mime http://schemas.xmlsoap.org/wsdl/mime/
    ;;      WSDL namespace for WSDL MIME binding.
    (:net.xmp.soap.encoding
     "soapenc"
     "http://schemas.xmlsoap.org/soap/encoding/"
     ;; Encoding namespace as defined by SOAP 1.1 [8].
     )
    (:net.xmp.soap.envelope
     "soapenv"
     "http://schemas.xmlsoap.org/soap/envelope/"
     ;; Envelope namespace as defined by SOAP 1.1 [8].
     )
    (:net.xmp.schema-instance
     "xsi"
     "http://www.w3.org/2000/10/XMLSchema-instance"
     ;; Instance namespace as defined by XSD [10].
     )
    (:net.xmp.schema
     "xsd"
     "http://www.w3.org/2000/10/XMLSchema"
     ;; Schema namespace as defined by XSD [10].
     )
    ;; tns (various)
    ;; The 'this namespace' (tns) prefix is used as a convention
    ;;     to refer to the current document.
    ;; (other) (various)
    ;; All other namespace prefixes are samples only. 
    ;;     In particular, URIs starting with 'http:// example.com'
    ;;     represent some application- dependent or context-dependent URI [4].
    ))

(defparameter *wsdl-1.2-namespaces*
  ;;
  ;; From: Web Services Description Language (WSDL)
  ;;       Version 1.2 Part 1: Core Language
  ;;       W3C Working Draft 11 June 2003
  ;;       This version: http://www.w3.org/TR/2003/WD-wsdl12-20030611
  ;;
  '(nil
    (:net.xmp.wsdl
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
    (:net.xmp.wsdl.soap
     "soap12"
     "http://www.w3.org/2003/06/wsdl/soap12"
     ;; Defined by WSDL 1.2: Bindings [WSDL 1.2 Bindings].
     )
    ;; http "http://www.w3.org/2003/06/wsdl/http"
    ;; mime "http://www.w3.org/2003/06/wsdl/mime"
    (:net.xmp.schema
     "xs"
     "http://www.w3.org/2001/XMLSchema"
     ;; Defined in the W3C XML Schema specification
     ;;   [XML Schema: Structures], [XML Schema: Datatypes].
     )
    (:net.xmp.schema-instance
     "xsi"
     "http://www.w3.org/2001/XMLSchema- instance"
     )
    ))

(defparameter *wsdl-default-namespaces* *wsdl-1.1-namespaces*)

(defclass wsdl-file-connector (schema-file-connector)
  (
   (message-dns     :initform *wsdl-default-namespaces*)
   (trim-whitespace :initform t)
   (messages :accessor wsdl-messages :initform (list nil))
   (interfaces :accessor wsdl-interfaces :initform (list nil))
   (port-types :accessor wsdl-port-types :initform (list nil))
   (bindings :accessor wsdl-bindings :initform (list nil))
   (services :accessor wsdl-services :initform (list nil))
   
   (client-options :accessor wsdl-client-options :initform nil)

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
   ))


(defmethod xmp-warning-leader ((conn wsdl-file-connector)) "WSDL")

(defun decode-wsdl-file (file &rest keys)
  (apply #'decode-wsdl-source :file file keys))
(defun decode-wsdl-string (string &rest keys)
  (apply #'decode-wsdl-source :string string keys))
(defun decode-wsdl-at-uri (uri &rest keys)
  (apply #'decode-wsdl-source :uri uri keys))

(defun decode-wsdl-source (&key file
				(namespaces :guess)
				(lisp-package :keyword)
				string uri
				&aux (*xmp-warning-leader* "WSDL")
				guess other-pk added-ns default
				tail
				)

  ;; namespaces -> ns-tail | (default-uri-or-nil [ ns-spec ]... . ns-tail)
  ;;    ns-spec -> package-name  ;; map all other URIs to this package
  ;;            -> (:prefix symbol) ;; map all other URIs to packages 
  ;;                                ;;     net.xmpns.symbolnn
  ;;            -> (package-name prefix uri)   ;; explicit namespace def
  ;;    ns-tail -> :none         ;; add nothing
  ;;            -> :guess        ;; deduce namespaces from scan of XML
  ;;            -> :wsdl-1.1     ;; *wsdl-1.1-namespaces*
  ;;            -> :wsdl-1.2     ;; add *wsdl-1.2-namespaces*
  ;;            -> nil           ;; add *wsdl-default-namespaces*

  (when uri 
    (multiple-value-bind (body rc h ruri)
	(net.aserve.client:do-http-request uri)
      (if (and body (eql rc 200))
	  (setf string body)
	(error "URI ~A returned error ~S ~S ~S"
	       uri rc h ruri))))

  (let* ((dns (progn
		(etypecase namespaces
		  (null nil)
		  ((member :guess) (setf guess namespaces))
		  ((or string symbol) (setf guess t other-pk namespaces))
		  (cons (setf default (car namespaces))
			(do ((tl (cdr namespaces) (cdr tl)))
			    ((atom tl)
			     (ecase tl
			       (:guess (setf guess t))
			       (:wsdl-1.1 (setf tail *wsdl-1.1-namespaces*))
			       (:wsdl-1.2 (setf tail *wsdl-1.2-namespaces*))
			       ((nil)  (setf tail *wsdl-default-namespaces*))
			       (:none nil)))
			  (typecase (first tl)
			    (atom (if other-pk
				      (error "More than one other-package")
				    (setf other-pk (first tl))))
			    (cons (case (first (first tl))
				    (:prefix
				     (if other-pk
					 (error "More than one other-package")
				       (setf other-pk (first tl))))
				    (otherwise
				     (push (first tl) added-ns))))))))
		(when guess
		  (multiple-value-bind (ns other ambi missing)
		      (decode-wsdl-namespaces
		       :file file :string string :other-pk other-pk 
		       :base-ns (xmp-merge-nses (cons default added-ns) tail))
		    (if (and (null other) (null ambi) (null missing))
			(setf tail ns)
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
		(xmp-merge-nses (cons default added-ns) tail)))
				   
	 (conn (make-instance 'wsdl-file-connector
			      :message-dns dns
			      :source file :lisp-package lisp-package)))

    (values-list
     (append
      (list conn)
      (multiple-value-list 
       (cond (string (xmp-decode-string conn string))
	     (file   (xmp-decode-file conn file))))))
    ))

(defmethod schema-decode-attribute ((conn wsdl-file-connector)
				    name value nss &aux attr kwd)
  (setf attr (xmp-decode-qualified-name conn name nss))
  (setf kwd (intern (string attr) :keyword))
  (case attr
    ((wsdl:|message| wsdl:|arrayType| wsdl:|binding|)
     (values attr (xmp-decode-qualified-name conn value nss)))
    (otherwise 
     (case kwd
       ((:binding) 
	(values attr (xmp-decode-qualified-name conn value nss)))
       (otherwise
	(call-next-method conn attr value nss))))))


(defmethod xmp-begin-message ((conn wsdl-file-connector))
  (list :seq1 'wsdl:|definitions|))

(defmethod xmp-end-message ((conn wsdl-file-connector) data
			    &key types &allow-other-keys)
  (values data types))




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

(defmethod xmp-simple-content ((conn wsdl-file-connector)
			       (elt (eql 'wsdl:|documentation|)) data
			       &rest options 
			       &key &allow-other-keys)
  (declare (ignore options))
  (list :documentation data))

(defmethod xmp-complex-content ((conn wsdl-file-connector) 
				(elt (eql 'wsdl:|documentation|))
				data &rest options &key &allow-other-keys)
  (apply #'call-next-method conn elt data :warn nil options))

(defmethod xmp-complex-content ((conn wsdl-file-connector) 
				(elt (eql 'wsdl:|definitions|))
				data &rest options &key &allow-other-keys)
  (apply #'call-next-method conn elt data :warn nil options))
  
(defmethod xmp-complex-content ((conn wsdl-file-connector)
				(elt (eql 'wsdl:|types|))
				data &rest options &key &allow-other-keys)
  (apply #'call-next-method conn elt data :warn nil options))
  

     
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


(schema-collected-part wsdl-file-connector wsdl:|message|   :message   wsdl-messages)
(schema-collected-part wsdl-file-connector wsdl:|interface| :interface wsdl-interfaces)
(schema-collected-part wsdl-file-connector wsdl:|portType|  :port-type wsdl-port-types)
(schema-collected-part wsdl-file-connector wsdl:|binding|   :binding   wsdl-bindings)
(schema-collected-part wsdl-file-connector wsdl:|service|   :service   wsdl-services)

(schema-simple-part wsdl-file-connector wsdl:|input|      :input)
(schema-simple-part wsdl-file-connector wsdl:|output|     :output)
(schema-simple-part wsdl-file-connector wsoap:|binding|   :soap-binding)
(schema-simple-part wsdl-file-connector wsoap:|operation| :soap-operation)
(schema-simple-part wsdl-file-connector wsoap:|body|      :soap-body)
(schema-simple-part wsdl-file-connector wsoap:|address|   :soap-address)

(schema-named-part wsdl-file-connector wsdl:|operation|  :operation  :not-qname)
(schema-named-part wsdl-file-connector wsdl:|part|       :part       :not-qname)
(schema-named-part wsdl-file-connector wsdl:|port|       :port)
(schema-named-part wsdl-file-connector wsdl:|fault|      :fault)
(schema-named-part wsdl-file-connector wsoap:|fault|     :soap-fault)


(defmethod wsdl-service-names ((conn wsdl-file-connector) &optional and-ports)
  (if and-ports
      (let (services ports)
	(dolist (sdef (cdr (wsdl-services conn)))
	  (dolist (spart (schema-collected-parts conn sdef :service))
	    (case (first spart)
	      (:port (push 
		      (case and-ports
			(:verbose
			 (list (second spart)
			       (xmp-getf conn 
					 (getf (cddr spart) :attributes)
					 "binding")
			       (xmp-getf conn
					 (cdr (assoc :soap-address
						     (getf (cddr spart) :content)))		
					 "location")))
			(otherwise (second spart)))
		      ports))))
	  (push (cons (first sdef) (reverse ports)) services))
	(reverse services))
      (mapcar 'first (cdr (wsdl-services conn)))))


(defmethod wsdl-define-content ((conn wsdl-file-connector) item)
  (when (consp item)
    (case (first item)
      ((:set :seq :set1 :set+ :set* :seq1 :seq+ :seq*)
       `(:complex
	 (,(first item)
	  ,@(mapcar
	     #'(lambda (elt)
		 (case (first elt)
		   (:element
		    `(:element ,(second elt)
			       ,(getf (cddr elt) :type)))
		   (:any `(:any))
		   (otherwise
		    (wsdl-warn conn
			       "wsdl-define-type - unknown sub-form ~S"
			       elt))))
	     (cdr item)))))
      )))

(defmethod wsdl-define-type ((conn wsdl-file-connector) typedef do-form)
  (let* ((name (first typedef))
	 (props (cdr typedef))
	 x 
	 complex content complex-content
	 (simple  (case (setf x (getf props :simple-type :none))
		    (:none nil)
		    ((nil) :null)
		    (otherwise x)
		    ))
	 new-def)
    (cond ((null (setf complex (getf props :complex-type :none)))
	   (setf new-def `(:complex (:seq* (:any)))))
	  ((consp complex)
	   (cond
	    ((setf complex-content (cdr (assoc :complex-content complex)))
	     (case (first complex-content)
	       (:attributes (pop complex-content) (pop complex-content)))
	     (dolist (item complex-content)
	       (case (first item)

		 ;; (:extension wsdl:base base-type
		 ;;             :content ((:content collected-parts)))
		 ;; xmeth014 looks like extension slots are added to base slots
		 ;;          need extension to :complex ???

		 (:restriction
		  (let* ((r (cdr item))
			 (r-base (xmp-getf conn r "base"))
			 (r-cont (getf r :content))
			 (r-attr (assoc :attribute r-cont))
			 (r-suba (getf r-attr :attributes))
			 (r-atype (xmp-getf conn r-suba "arrayType")))
		    (cond
		     ((and (eq r-base 'enc:|Array|)
			   (eq (second r-attr) 'enc:|arrayType|)
			   r-atype)
		      (setf new-def
			    `(:array ,(soap-parse-array-type conn r-atype :dns))))

		     ;; complex-content -> ((:restriction wsdl:base SOAP-ENC:Array
		     ;;                         :content
		     ;;                         ((:content collector-list))))
		     ;; in xmethods "Muse.Net Client Service"
		     ((and (setf x (assoc :content r-cont))
			   (eql 2 (length x))
			   (setf x (wsdl-define-content conn (second x))))
		      (setf new-def `(:array ,x)))

		     (t 
		      (wsdl-warn
		       conn
		       "wsdl-define-type - unknown restriction in complex-content~S"
		       complex-content)))))
		 (otherwise
		  (wsdl-warn conn
			     "wsdl-define-type - unknown item in complex-content ~S"
			     complex-content))
		 ))
	     )
	    ((setf content (cdr (assoc :content complex)))
	     (setf x nil)
	     (dolist (item content)
	       (when (setf new-def (wsdl-define-content conn item))
		 (return))
	       (or x  ;; warn only once
		   (wsdl-warn
		    conn "wsdl-define-type - unknown content in ~S" content))
	       (setf x t)))

	    ;; complex -> ((:element ...) ...)
	    ;;   in xmethods "Xara NavBar Generator"
	    ((dolist (x complex t)
	       (or (and (consp x) (eq :element (first x)))
		   (return nil)))
	     (setf new-def
		   `(:complex (:seq1 ,@(mapcar
					#'(lambda (e)
					    `(:element
					      ,(string (second e))
					      (:simple
					       ,(xmp-getf conn (cddr e) "type"))))
					complex)))))
		
	    ;; complex -> ((#("element" "type" type "maxOccurs" max)))
	    ;; apparently sloppy def in xmethods "Agni Find MP3"
	    ((and (null (cdr complex))
		  (consp (setf complex (first complex)))
		  (null (cdr complex))
		  (setf complex (concatenate 'list (first complex)))
		  (equal "element" (string (first complex))))
	     (setf new-def
		   `(:array ,(if (setf x (xmp-getf conn (cdr complex) "type"))
				 (xmp-decode-qualified-name conn x :dns)
			       (list :any)))))

	    ;; complex -> (:simple :content
	    ;;              ((:extension wsdl:base type :content attributes-def)))
	    ;;   in xmethods "Alan Bush Compositions"
	    ((and (eq :simple (first complex))
		  (setf content (getf (cdr complex) :content))
		  (setf x (assoc :extension content)))
	     (setf new-def `(:simple ,(xmp-getf conn (cdr x) "base"))))

	    (t (wsdl-warn conn "wsdl-define-type - unknown complex type ~S"
			  complex))
	    ))

	  ((null (setf simple (getf props :simple-type :none)))
	   (setf new-def `(:simple xsd:|string|)))
	  ((consp simple)
	   (or
	    (let* ((r (cdr (assoc :restriction simple)))
		   (base (when r (xmp-getf conn r "base"))))
	      (when base

		;; look for :content ((:enumeration "value" val) ...)
		;;   and collect values  - need extension to :simple ???

		(setf new-def `(:simple ,base))
		t))
	    (wsdl-warn conn "wsdl-define-type - unknown simple type ~S"
		       simple)))


	  (t (wsdl-warn conn "wsdl-define-type - unknown form ~S" typedef)))
    (when new-def (funcall do-form `(define-soap-type nil ',name ',new-def)))
      
    ))

(defmethod wsdl-message-part-element ((conn wsdl-file-connector) part)
  (or (getf (cddr part) :element)
      (xmp-getf conn
		(getf (cddr part) :attributes)
		"element")
      (second part)))

(defmethod wsdl-bind-operation ((conn wsdl-file-connector) b mode do-form 
				eval prefix suffix)
  (flet ((do-form (&rest x) (apply do-form x))
	 (part-name (part) (wsdl-message-part-element conn part))
	 )
    (let* ((op-name (second b))
	   (body (cddr b))
	   (content  (getf body :content))
	   (soap-op  (assoc :soap-operation content))
	   (action   (xmp-getf conn (cdr soap-op) "soapAction"))
	   (style    (or (xmp-getf conn (cdr soap-op) "style")
			 (wsdl-soap-style conn)))
	   (messages (cdr (wsdl-messages conn)))

	   (bind-in  (assoc :input content))
	   (bind-out (assoc :output content))
	   ;;   (in-or-out :content ((:soap-body
	   ;;                            wsdl:use encoded|literal
	   ;;                            wsdl:namespace     URIstring
	   ;;                            wsdl:encodingStyle URIstring
	   ;;                            wsdl:parts  nmtokens          ???
	   ;;                            )
	   ;;        soap:header  ???
	   ;;        soap:fault   ???

	   (opdef    (first (member op-name (wsdl-operations conn)
				    :test #'equal
				    :key #'second)))
	   (opcont   (getf opdef :content))
	   (op-in    (assoc :input opcont))
	   (in-msg-name   (xmp-getf conn (cdr op-in) "message"))
	   (op-out   (assoc :output opcont))
	   (out-msg-name  (xmp-getf conn (cdr op-out) "message"))
	   (in-msg (assoc in-msg-name messages))
	   (in-parts (when in-msg
		       (schema-collected-parts conn in-msg :message :key :part)))
	   (out-msg (assoc out-msg-name messages))
	   (out-parts (when out-msg
			(schema-collected-parts conn out-msg :message :key :part)))
	   (cx (when in-msg (position in-msg messages)))
	   comment done-form)

      (or (string-equal style "rpc")
	  (error "SOAP style=~S is not implemented." style) ;;;???
	  )

      (flet ((do-element
	      (method-name msg-name action binding intern-name
			   &aux
			   (parts (schema-collected-parts
				   conn (assoc msg-name messages) :message :key :part))
			   (content (getf (cdr binding) :content))
			   (soap-body (assoc :soap-body content))
			   (estyle (xmp-getf conn (cdr soap-body) "encodingStyle"))
			   (use (xmp-getf conn (cdr soap-body) "use"))
			   (ns (xmp-getf conn (cdr soap-body) "namespace"))
			   (pk (when ns (net.xmp::xmp-uri-to-package
					 conn ns :dns)))
			   (pktag "tns"))
	      (or (null use) (string-equal use "encoded")
		  (error "SOAP use=~S is not implemented." use) ;;;???
		  )
	      (when ns
		(or pk
		    (let* ((mp (wsdl-undef-ns conn))
			   (mpc (and (consp mp)
				     (eq :if-missing-package (first mp))
				     (second mp)))
			   (made (when mpc (cddr mp)))
			   nsp pkn nsd)
		      (when mpc
			(typecase mpc
			  ((or string symbol)
			   (setf nsp (string
				      (read-from-string
				       (format nil ":~A~A"
					       mpc (1+ (length made))))))
			   (setf pkn (string
				      (read-from-string
				       (format nil ":net.xmpns.~A" nsp))))
			   (setf nsd (make-nsd pkn nsp ns))
			   (push nsd (cddr mp))
			   (setf pktag nsp)
			   (setf pk (or (find-package pkn)
					(make-package pkn :use nil)))
			   (xmp-add-dns-entry conn nsd)
			   t))))


		    (let* ((tg (wsdl-targets conn)) pk2)

		      ;; Look in the list of all targetNamespace attributes
		      ;; in the definition.

		      (cond
		       ;; If there is only one targetNamespace
		       ;;    and it has a package, make these two the same
		       ((and (eql 1 (length tg))
			     (setf pk2 (net.xmp::xmp-uri-to-package
					conn (first tg) :dns)))
			(setf pk pk2))))			   
		    (pushnew ns (wsdl-undef-ns conn) :test #'string=)))

	      ;; part -> (:part name [:type type] [:attributes attrs] [:content content])
	      (values
	       (let (type elt attrs
			  (def-tail
			    `(,@(when ns
				  `(:namespaces
				    (nil (,(when pk (intern (package-name pk) :keyword))
					  ,pktag
					  ,ns))))
				,@(when estyle `(:encoding ,estyle))
				,@(when action `(:action ,action))
				))
			  )
		 ;; SOAP method call and response are always structs.
		 `(define-soap-element
		    nil
		    ',(if intern-name
			  (setf method-name (etypecase method-name
					      (symbol method-name)
					      (string (if pk
							  (intern method-name pk)
							method-name))))
			method-name)
		    '(:complex
		      (:seq1    
		       ,@(mapcar #'(lambda (part)
				     (setf type (getf (cddr part) :type))
				     (setf attrs (getf (cddr part) :attributes))
				     (setf elt (xmp-getf conn attrs "element"))
				     (cond ((and elt (null type))
					    elt)
					   ((and elt type)
					    `(:element ,elt ,type))
					   (t `(:element ,(second part)
							 ,type))))
				 parts))
		      ,@def-tail)))
	       method-name)))
	(when in-msg
	  (multiple-value-setq (done-form op-name)
	    (do-element op-name in-msg-name action bind-in t))
	  (do-form done-form))
	(when out-msg
	  (do-form (do-element (concatenate 'string (string op-name) "Response")
			       out-msg-name nil bind-out nil)))
	(when (and in-msg out-msg);;???  in-out messages notification messages...
	  (do-form
	   (let* ((def-name (read-from-string (format nil "~A~A"
						      prefix
						      (case suffix
							(:index (incf cx))
							(:message op-name)))))
		  (one-part (and in-parts
				 (null (cdr in-parts))
				 (first in-parts)))
		  (one-type (getf (cddr one-part) :type))
		  (one-def  (and one-type 
				 (wsdl-expand-singleton conn)
				 eval
				 (setf one-type (soap-find-type nil one-type :dns))
				 (eq :complex (first one-type))
				 (case (first (second one-type))
				   ((:seq :seq1 :set :set1) one-type)
				   (otherwise nil))))
		  (in-elts (mapcar #'part-name in-parts))
		  (in-keys (mapcar #'(lambda (part) (do-form part :preserve))
				   in-elts))
		  (key-args   (if one-def
				  (mapcar #'(lambda (part)
					      (do-form (xmp-pick-name nil part)
						       :preserve))
					  (cdr (second  one-def)))
				in-keys)
			      )
		  )
	     (ecase mode
	       (:client
		(setf comment
		      (list (format nil "Send client message ~A " op-name)
			    op-name))
		`(defun ,def-name 
		   (&rest ,(do-form "args") &key ,@key-args)
		   (declare (ignore ,@key-args))
		   (let ((,(do-form "conn") (soap-message-client 
					     :url ,(wsdl-soap-address conn)
					     :message-dns *application-namespaces*
					     ,@(wsdl-client-options conn)
					     )))
		     (values
		      ,(if one-def
			   `(apply 'call-soap-method ,(do-form "conn")
				   ',op-name 
				   ,(part-name one-part)
				   ,(do-form "args") nil)
			 `(apply 'call-soap-method
				 ,(do-form "conn") ',op-name ,(do-form "args")))
		      ,(do-form "conn")))))
	       (:server
		(let* ((one-ret (and out-parts
				     (null (cdr out-parts))
				     (first out-parts)))
		       (ret-type (getf (cddr one-ret) :type))
		       (ret-def  (and one-ret
				      (wsdl-expand-singleton conn)
				      eval
				      (setf ret-type
					    (soap-find-type nil ret-type :dns))
				      (eq :complex (car ret-type))
				      ret-type))
		       (ret-keys   (if ret-def
				       (mapcar #'(lambda (part)
						   (do-form (xmp-pick-name nil part)
							    :preserve))
					       (cdr (part-name  ret-def)))
				     (mapcar
				      #'(lambda (part)
					  (do-form (part-name part) :preserve))
				      out-parts)))
		       (ret-name (when ret-def (part-name (first out-parts))))
		       (ret-parts (if ret-def
				      (mapcan (lambda (part var)
						(list (xmp-pick-name nil part)
						      var))
					      (cdr (second  ret-def))
					      ret-keys
					      )
				    (mapcan 
				     #'(lambda (part var)
					 (list (part-name part) var))
				     out-parts ret-keys)
				    ))
		       )
		  (push `(',op-name ',in-elts
				    ,@(when action (list :action action))
				    :lisp-name ',def-name :return ',out-msg-name)
			(wsdl-server-exports conn))
		  (setf comment (format nil "Handler for message ~A" op-name))
		  (cond ((and one-def ret-def)
			 `(defun ,def-name (&key ,@in-keys)
			    ((lambda (&key ,@key-args)
			       (let ,ret-keys
				 "INSERT BODY HERE"
				 (list ,ret-name (list ,@ret-parts))))
			     ,(first in-keys)))
			 )
			(one-def
			 `(defun ,def-name (&key ,@in-keys)
			    ((lambda (&key ,@key-args)
			       (let ,ret-keys
				 "INSERT BODY HERE"
				 (list ,@ret-parts)))))
			 )
			(ret-def
			 `(defun ,def-name
			    (&key ,@key-args)
			    (let ,ret-keys
			      "INSERT BODY HERE"
			      (list ,ret-name (list ,@ret-parts))))
			 )
			(t `(defun ,def-name
			      (&key ,@key-args)
			      (let ,ret-keys
				"INSERT BODY HERE"
				(list ,@ret-parts)))
			   ))))
	       ))
	   comment)))
      )))


(defmethod wsdl-service-binding-names ((conn wsdl-file-connector) sdef port
				       &optional error-p)
  (let* (pref bname url (i 0)
	      )
    (dolist (spart (schema-collected-parts conn sdef :service))
      (case (first spart)
	(:port (if (typecase port 
		     (integer (eql port i))
		     (otherwise (eq port (second spart))))
		   (return (setf pref spart))
		 (incf i)))))
    (when pref
      (setf bname (xmp-getf conn (getf (cddr pref) :attributes) "binding"))
      (setf url (xmp-getf conn (cdr (assoc :soap-address (getf (cddr pref) :content)))
		      "location")))

    (if (and sdef pref bname url)
	(values bname url)
      (if error-p
	  (error "Cannot find binding name in service ~S" sdef)
	nil))))



(defmethod make-client-interface ((conn wsdl-file-connector) service destination
				  &rest args
				  &key
				  port verbose
				  (eval t)
				  (lisp-package :keyword)
				  (file-package :user)
				  (expand-singleton t)
				  (if-missing-package :warn)
				  null-element (empty-element nil)
				  (prefix :client-) (suffix :index))
  (declare (ignore port eval lisp-package file-package null-element empty-element
		   expand-singleton prefix suffix if-missing-package verbose))
  (apply 'wsdl-make-interface conn service destination :client args))

(defmethod make-server-interface ((conn wsdl-file-connector) service destination
				  &rest args
				  &key
				  port verbose
				  (eval t)
				  (lisp-package :keyword)
				  (file-package :user)
				  (expand-singleton t)
				  null-element (empty-element nil)
				  (prefix :server-) (suffix :index)
				  action (if-missing-package :warn)
				  message-dns
				  )
  (declare (ignore port eval lisp-package file-package null-element empty-element
		   expand-singleton prefix suffix action message-dns if-missing-package 
		   verbose))
  (apply 'wsdl-make-interface conn service destination :server args))
				  

(defmethod wsdl-make-interface ((conn wsdl-file-connector) service destination
				mode
				&key
				port verbose
				(eval t evp)
				(lisp-package :keyword)
				(file-package :user)
				(expand-singleton
				 (if evp
				     (if eval t nil)
				   t))
				null-element (empty-element nil eep)
				(prefix (format nil "~A-" mode))
				(suffix :index)
				action (if-missing-package :warn)
				message-dns
				(if-exists :supersede)
				&aux w open defs dstream dfile 
				head-forms forms tail-forms
				sdef bname port-name pdef opdefs bdef
				binding btype url
				(*xmp-warning-leader* "WSDL"))
  (or service (setf service 0))
  (or port (setf port 0))
  (and (numberp service)
       (setf w (nth service (wsdl-service-names conn)))
       (setf service w))
  (or (setf sdef (assoc service (cdr (wsdl-services conn)) :test #'string-equal))
      (error "Cannot find service ~S" service))
  (setf (wsdl-warnings conn) 0)
  (multiple-value-setq (bname url)
    (wsdl-service-binding-names conn sdef port t))
  (setf bdef (or (assoc bname (cdr (wsdl-bindings conn)))
		 (error "Cannot find binding ~S" bname)))
  (setf binding (getf (cdr bdef) :binding))
  (setf btype   (getf (cdr bdef) :type))
  (setf port-name btype)
  (setf pdef (or (assoc port-name (cdr (wsdl-port-types conn)))
		 (error "Cannot find portType ~S" port-name)))
  (setf opdefs (getf (cdr pdef) :port-type))

  (etypecase destination
    (stream (setf dstream destination))
    ((member nil t) (setf dstream destination))
    (string (setf dfile destination)
	    )
    )
  (when (and expand-singleton (null eval))
    (warn "expand-singleton=t is ignored when eval=nil"))

  (flet ((dstream ()
		  (cond (dstream)
			(dfile 
			 (setf open t
			       dstream (open destination
					     :direction :output
					     :if-exists if-exists))))))

    (flet ((do-form
	    (form &rest comments)
	    (etypecase form
	      ((or string symbol)
	       (setf form (string form))
	       (etypecase (first comments)
		 ((member :preserve) (intern form file-package))
		 (null (let ((*package* file-package))
			 (read-from-string form)))
		 ))
	      (cons
	       (let* ((key (first form))
		      (place (assoc key defs))
		      (name (case key
			      (defun (second form))
			      ((define-soap-element define-soap-type)
			       (second (third form)))
			      (otherwise (second form))))
		      )
		 (or place (push (setf place (list key)) defs))
		 (and comments (consp (car comments)) (null (cdr comments))
		      (setf comments (car comments)))
		 (push (if comments
			   (cons name comments)
			 name)
		       (cdr place)))
	       (push (cons form comments) forms)
	       (when eval (eval form))
	       form)))
	   (do-file (item &aux (form (first item)) (comments (cdr item)))
		    (when (dstream)
		      (when comments
			(format (dstream) "~&~%~%")
			(dolist (c comments)
			  (or (symbolp c) (format (dstream) ";; ~A~%" c))))
		      (let ((*package* file-package))
			(format (dstream) "~&~%~S~%" form))))
	   (desc (defs stream depth)
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
			    (describe-soap-method (third entry) stream depth)))
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
	   )

      (unwind-protect
	  (let (soap-binding x ctype stype found-ctype)

	    (setf (wsdl-undef-ns conn) (case if-missing-package
					 ((:error :warn) nil)
					 (otherwise (list :if-missing-package
							  if-missing-package)))
		  (wsdl-soap-address conn) url
		  (wsdl-operations conn) opdefs
		  (wsdl-expand-singleton conn) expand-singleton
		  (wsdl-client-options conn)
		  `( ,@(when lisp-package (list :lisp-package lisp-package))
		       ,@(when null-element (list :null-element null-element))
		       ,@(when eep (list :empty-element empty-element)) 
		       )
		  )

	    (cond ((null file-package) (setf file-package *package*))
		  ((packagep file-package)
		   (do-form `(in-package ,(make-symbol (package-name file-package)))))
		  ((setf x (find-package file-package))
		   (setf file-package x)
		   (do-form `(in-package ,(make-symbol (package-name file-package)))))
		  (t (error "Cannot find package ~S" file-package)))
	    (do-form `(defpackage ,(make-symbol (package-name file-package))
			(:use ,@(mapcar #'(lambda (p)
					    (make-symbol (package-name p)))
					(package-use-list file-package)))))

	    (setf head-forms (reverse forms))
	    (setf forms nil)

	    (dolist (typedef (cdr (schema-types conn)))
	      (wsdl-define-type conn typedef  #'do-form))
	    (dolist (edef (cdr (schema-elements conn)))
	    
	      ;; edef -> (name :element ((:complex-type type-name-or-nil
	      ;;                            :complex-type (:content e-type-def))
	      ;;      -> (name :type type :attributes attrs)
	      ;;      -> (name :attributes attrs)            ANY content
	      ;; e-type-def (collector e-type-def-part ... )
	      ;; e-type-def-part -> (:element name :type type :attributes attrs
	      ;;                                   :content content)
	      ;;                 -> (:any . attributes)
	      (setf found-ctype nil)
	      (do-form `(define-soap-element nil ',(first edef)
			  ',(cond ((setf ctype (getf (cdr edef) :element))
				   (do ((tail (assoc :complex-type ctype) (cddr tail)) x)
				       ((atom tail)
					(if found-ctype
					    (list :complex (list :seq))
					  (error "Element without a type ~S" edef)))
				     (case (first tail)
				       (:complex-type
					(setf found-ctype t)
					(cond
					 ((and (consp (second tail))
					       (setf x (second
							(assoc :content (second tail)))))
					  (let* ((collector (first x))
						 (elements
						  (mapcar
						   #'(lambda (e)
						       (ecase (first e)
							 (:element
							  (list :element
							     (list (second e))
							     (getf (cddr e) :type)))
							 (:any (list :any)))
						       )
						   (cdr x))))
					    (return
					     (list :complex
						   (list* collector elements)))))
					 ((and (atom (second tail)) (second tail))
					  (return (second tail))))))))
				  ((setf stype (getf (cdr edef) :type))
				   stype)
				  (t `(:complex (:seq* (:any)))))
			  ))
	      ) ;;; end (dolist (edef))

	    (dolist (b binding)
	      (case (first b)
		(:soap-binding

		 ;; (:soap-binding style rpc|document transport URIstring) ???

		 (when soap-binding (warn "wsdl:binding - Found second ~S" b))
		 (setf soap-binding b)
		 (setf (wsdl-soap-style conn)
		       (xmp-getf conn (cdr soap-binding) "style"))
		 )
		(:operation nil)
		(otherwise (warn "wsdl:binding - Found sub-element ~S" b))))
	    (dolist (b binding)
	      (case (first b)
		(:operation
		 (wsdl-bind-operation conn b mode #'do-form eval prefix suffix))))

	    (case mode
	      (:server
	       (do-form
		`(defun ,(do-form "make-server") (&optional (,(do-form "port") 8080))
		   (let ((,(do-form "s")
			  (soap-message-server
			   :start (list :port ,(do-form "port")) :enable :start
			   :publish (list :path
					  ,(xmp-resolve-uri (wsdl-soap-address conn) t))
			   ,@(when action (list :action action))
			   :lisp-package :keyword
			   ,@(if message-dns
				 (list :message-dns (list 'quote message-dns))
			       (list :message-dns '*application-namespaces*)
			       )
			   )))
		     ,@(mapcar #'(lambda (ex)
				   (list* 'soap-export-method (do-form "s") ex))
			       (wsdl-server-exports conn))
		     ,(do-form "s"))))))
						   
	    (when (wsdl-undef-ns conn)
	      (case if-missing-package
		(:error (error "There are no Lisp packages defined for namespaces ~S"
			       (wsdl-undef-ns conn)))
		(:warn
		 (dolist (ns (wsdl-undef-ns conn))
		   (warn "There is no Lisp package defined for namespace ~S"
			 ns)))))

	    
	    (setf tail-forms (reverse forms))
	    (setf forms nil)
	    ;; do this last to pick up any packages created during 
	    ;; interface generation
	    (dolist (nsd (nse-defs (xmp-message-dns conn)))
	      (or (eq (find-package :keyword)
		      (find-package (nsd-package nsd)))
		  (do-form `(defpackage ,(nsd-package nsd) (:use)))))
	    (do-form `(defparameter *application-namespaces* ',(xmp-message-dns conn)))
	    
	    (let (pdefs)
	      (dolist (def defs)
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
		(format (dstream) "~&~%#|~%")
		(desc (cdr pdefs) (dstream) 3)
		(format (dstream) "~&|#~2%")
		(dolist (item (cdr head-forms))
		  (do-file item))
		(dolist (item (reverse forms))
		  (do-file item))
		(dolist (item tail-forms)
		  (do-file item))
		)
	      (when verbose
		(format t "~&~2%")
		(desc pdefs t nil)
		(format t "~2%"))

	      (when (and (wsdl-warnings conn)
			 (not (eql 0 (wsdl-warnings conn))))
		(error "Serious warnings in WSDL processing: ~A"
		       (wsdl-warnings conn)))
	      
	      pdefs))
	(when open (close dstream))))))

(defun describe-soap-method (elt &optional (stream t) depth)
  (let* ((type (soap-find-element nil elt :dns))
	 (parts (when (and (consp type) (eq :complex (car type)))
		  (cdr (second type))))
	 (ret (concatenate 'string (string elt) "Response"))
	 (rtype (soap-find-element nil ret :dns))
	 (rparts (when (and (consp rtype) (eq :complex (car rtype)))
		  (cdr (second rtype))))
	 (indent 6) l)
    (format stream "~&~VAMessage ~S takes ~A argument~A~%"
	    indent "" elt (setf l (length parts))
	    (case l (0 "s") (1 ":") (otherwise "s:")))
    (dolist (part parts) (describe-soap-element part stream depth))
    (format stream "~&~VAand returns a result ~A containing the element(s):~%"
	    indent "" ret)
    (dolist (part rparts) (describe-soap-element part stream depth))
    stream))

(defun describe-soap-element (eltspec &optional (stream t) depth (indent 8))
  (let (name type)
    (cond ((atom eltspec)
	   (multiple-value-setq (type name)
	     (soap-find-element nil eltspec :dns)))
	  ((and (consp eltspec) (eq :element (first eltspec)))
	   (setf name (xmp-pick-name nil eltspec)
		 type (third eltspec))))
    (cond (stream (format stream "~&~VAThe element ~S of type "
			  indent "" name)
		  (describe-soap-type type stream depth indent)
		  (format stream "~&"))
	  (t (format stream "~&~VAThe element ~S of type ~A~%"
		     indent "" name 
		     (describe-soap-type type nil nil))))
    stream))

(defun describe-soap-type (type &optional (stream t) depth (indent 8) &aux res)
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
			(describe-soap-element elt stream 
					       (when depth (1- depth))
					       (+ indent 2)
					       )))))))))
  (when (and stream type (symbolp type) depth (not (eql depth 0)))
    (incf indent 2) (decf depth)
    (let ((def (soap-find-type nil type :dns)))
      (cond ((null def)
	     (format stream "~&~VAThe type ~S is " indent "" type)
	     (format stream "undefined."))
	    ((and (consp def) (eq :simple (first def)) (null (second def))))
	    (t (format stream "~&~VAThe type ~S is " indent "" type)
	     (describe-soap-type def stream depth indent)))))

  res)

		    



;;;
;;; XMP output methods to generate WSDL file from Lisp server def.
;;;

(defvar *wsdl-file-depth* nil)

(defun encode-wsdl-file (file &key
			      namespaces
			      servers
			      (target "ThisWebServiceNamespace" t-p)
			      name
			      (if-exists :supersede)
			      &aux (*xmp-warning-leader* "WSDL"))
  (let* ((dns (xmp-merge-nses
	       namespaces *wsdl-default-namespaces*))
	 (conn (make-instance 'wsdl-file-connector :message-dns dns))
	 (wpk (net.xmp::xmp-package-to-prefix conn :net.xmp.wsdl :dns))
	type-defs messages ports bindings services
	(*wsdl-file-depth* 1)
	(target-prefix (and target
			    (xmp-uri-to-prefix conn target :dns)))
	(name-prefix (if name (string name) "SOAPServer"))
	counters defined-elements defined-types seen
	error-messages)
    (setf dns (xmp-message-dns conn))
    (cond
     ((first dns))
     (wpk (setf (first dns)
		(third
		 (first
		  (member wpk (cdr dns) :key #'second :test #'equal)))))
     (t (xmp-error conn :wsdl :string "Cannot determine default namespace.")))

    (labels ((make-name (prefix &aux
				(place (assoc prefix counters :test #'string-equal))
				)
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
			  ))
	     (make-tname (name) (if target-prefix
				    (format nil "~A:~A" target-prefix name)
				  (format nil "~A" name)))
	     (add-element (elt type)
			  (let ((def (assoc elt defined-elements)))
			    (if def
				(or (equal type (second def))
				    (error "Two defs of the same element ~S ~S ~S"
					   elt (second def) type))
			      (push (list elt (add-type type)) defined-elements))
			    nil))
	     (add-type (type)
		       (let (name)
			 (when (not (symbolp type))
			   (setf name (make-tname (make-name "Type")))
			   (setf type (list name type)))
			 (or (and (symbolp type)
				  (eq (symbol-package type)
				      (find-package :net.xmp.schema)))
			     (pushnew type defined-types))
			 (or name type)))
	     (encode-type-def
	      (conn name tdef &key attributes
		    &aux min max schema)
	      (case (first tdef)

		;; If element type is not named, then we can emit
		;;    an anonymous type in the body of the element.

		(:element
		 `((xs:|element|
		       "name"
		       ;; The name attribute seems to be always unqualified ???
		       ,(string (xmp-pick-name conn tdef))
		       "type" ,(add-type (third tdef))
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
		       ,@(mapcar #'(lambda (part) (encode-type-def 
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
			       conn (add-type (second tdef)) :dns)))))))
		(:simple
		 `((xs:|simpleType| ,@(when name `("name" ,(string name))) 
		       "type" (second tdef))))
		(otherwise
		 (error "Unrecognized type def ~S" tdef)))


	      )

	     (message-parts
	      (elt &aux def)
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
			 elements ctypes named 
			 )
		    (cond
		     ((and (eq kind :complex)
			   (member collector '(:seq :seq1 :set :set1))
			   (let (ctype)
			     (setf named t)
			     (dolist (cpart cparts t)
			       (or (and (consp cpart)
					(or (eq (first cpart) :element)
					    (return nil))
					(setf ctype (third cpart))
					(symbolp ctype))
				   (setf named nil))
			       (push (xmp-pick-name conn cpart) 
				     elements)
			       (push ctype ctypes))))
		      (reverse
		       (mapcar #'(lambda (elt type)
				   (if* named
					then
					(add-type type)
					`((wsdl:|part|
						"name" ,elt
						"type" ,type))
					else
					(add-element elt type)
					`((wsdl:|part|
						"name" ,(make-name "Part")
						"element" ,elt))))
			       elements ctypes)))
		     ;; Method call must be a struct of named parameters
		     ;;  and reply must be a struct of result and
		     ;;   named in=out parameters.
		     (t (error 
			 "Cannot use definition of element ~S ~S"
			 elt def))
		     )))))
	     )

      (dolist (s (if (and servers (atom servers)) (list servers) servers))
	(let* ((service-name (when (consp s) (first s)))
	       (server (if (consp s) (second s) s))
	       (port-name (or (soap-port-name server) (make-name "Port")))
	       (binding-name (or (soap-binding-name server) (make-name "Binding")))
	       (tables (xmp-server-exports s))
	       port-ops bind-ops service-ports)
	  (or service-name
	      (setf service-name (or (soap-service-name server)
				     (make-name "Service"))))
	  (push `((wsdl:|port| "name" ,port-name "binding" ,(make-tname binding-name))
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
				  (inmsgq (typecase op-elt
					   (symbol op-elt)
					   (string (make-tname op-elt))))
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
				  (outmsgq (make-tname outmsg))
				  (tns (when target (list "namespace" target)))
				  (body `((wsoap:|body|
						  "use" "encoded"
						  ,@tns
						  "encodingStyle" 
						  ,(soap-encoding-style server)
						  )))
				  )
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
				     ,@(message-parts op-elt))
				   messages)
			     (push `((wsdl:|message| "name" ,outmsg)
				     ,@(message-parts res))
				   messages)

			     )))
		     (aref tables i)))
	  (push `((wsdl:|portType| "name" ,port-name) ,@(reverse port-ops))
		ports)
	  (push `((wsdl:|binding|
			"name" ,binding-name
			"type" ,(make-tname port-name))
		  ((wsoap:|binding|
			  "style" "rpc"
			  ;; transport=???
			  ))
		  ,@(reverse bind-ops))
		bindings)
	  (push `((wsdl:|service| "name" ,service-name)
		  ,@(reverse service-ports))
		services)
	  ))

      ;; scan defined-elements and defined-types to build type-defs
      ;;  defined-elements -> ((elt-name type-name) ...)
      ;;  defined-types    -> ({ type-name | (type-name type-def) } ...)
      (dolist (def defined-elements)
	(push `((xsd:|element| "name" ,(first def) "type" ,(second def)))
	      type-defs))
      (setf seen nil)
      (loop
       (let ((old defined-types))
	 (setf defined-types nil)
	 (dolist (def old)
	   (or
	    (member def seen)
	    (let* ((name (if (consp def) (first def) def))
		   (tdef (if (consp def) (second def) (soap-find-type nil name :dns)))
		   )
	      (or (consp tdef)
		  (error "Undefined type ~S" name))
	      (push def seen)
	      (push (encode-type-def conn name tdef) type-defs)
	      )))
	 (or defined-types (return)))) 
   
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
	 (push "  - using the default URI 'ThisWebServiceNamespace'." error-messages))
       (dolist (m (reverse error-messages))
	 (xmp-encode-content conn (format nil "<!-- ~A -->~%" m) :sanitize nil))
       (format s "~&~%")

       (xmp-encode-begin
	conn 'wsdl:|definitions| 
	:namespaces (append (xmp-message-dns conn)
			    (when target
			      (list (list nil target-prefix target))))
	:attributes (when target (list 'wsdl:|targetNamespace| target))
	)

       (when type-defs
	 (xmp-encode conn 
		     `(wsdl:|types| (xsd:|schema| ,@type-defs))
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
      )))
	     

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
  (let (values)
    (let ((*wsdl-file-depth*
	   (if (> (+
		   (if (first namespaces) 2 0)
		   (if namespaces (* 2 (length namespaces)) 0)
		   (if attributes (length attributes) 0))
		  5)
	       (+ 2 *wsdl-file-depth*)
	     nil)))
      (setf values (multiple-value-list (call-next-method))))
    (values-list values)))

(defmethod xmp-encode-attribute :before ((conn wsdl-file-connector)
					 (prefix t) (suffix t) (name t)
					 (value t) (qname t))
  (when *wsdl-file-depth*
    (format (wsdl-file-stream conn) "~&~VA" (* 2 *wsdl-file-depth*) "")))



(defun decode-wsdl-namespaces (&key string file uri other-pk base-ns &aux ambi)
  (when uri 
    (multiple-value-bind (body rc h ruri)
	(net.aserve.client:do-http-request uri)
      (if (and body (eql rc 200))
	  (setf string body)
	(error "URI ~A returned error ~S ~S ~S"
	       uri rc h ruri))))
  (multiple-value-bind (xml pns)
      (cond ((and string file) (error "Ambiguous call."))
	    (string (net.xml.parser:parse-xml string :content-only t))
	    (file   (with-open-file 
		     (s file)
		     (net.xml.parser:parse-xml s :content-only t))))
    (declare (ignore xml))
    (let ((needed '(("/XMLSchema" :net.xmp.schema "xsd")
		    ("/XMLSchema-instance" :net.xmp.schema-instance "xsi")
		    ("org/soap/envelope" :net.xmp.soap.envelope "SOAP-ENV")
		    ("org/soap/encoding" :net.xmp.soap.encoding "SOAP-ENC")
		    ("schemas.xmlsoap.org/wsdl" :net.xmp.wsdl "wsdl")
		    ("/wsdl/soap" :net.xmp.wsdl.soap "wsdl-soap")
		    ("/wsdl/soap12" :net.xmp.wsdl.soap "wsdl-soap12")))
	  found nse nse-defs uri other missing (j 0))
      (dolist (ns pns)
	(setf uri (format nil "~A" (first ns)))
	(dolist (n needed (push uri other))
	  (let* ((item (first n)) (string uri)
		 (k (length item)) 
		 (l (length string))
		 (i (search item string))
		 m)
	    (when (cond ((null i) nil)
			((eql l (+ i k)) t)
			((and (eql l (+ i k 1))
			      (eql #\/ (elt string (1- l))))
			 t))
	      (cond ((setf m (assoc (first n) found :test #'equal))
		     (push (first (last m)) ambi)
		     (push uri ambi)
		     (return))
		    (t (setf needed (remove n needed))
		       (push (append n (list uri)) found)
		       (return))))))
	)

      (setf nse-defs
	    (append
	     (etypecase other-pk
	       (null nil)
	       (atom
		 (or (find-package other-pk)
		     (make-package other-pk :use nil))
		 (mapcar #'(lambda (uri)
			     (make-nsd other-pk 
				       (format nil "tns~A" (incf j))
				       uri))
			 other))
	       (cons ;; must be (:prefix prefix)
		(setf other-pk (string (second other-pk)))
		(mapcar #'(lambda (uri &aux tns pkn)
			    (setf tns (string
				       ;; to get the case right
				       (read-from-string
					(format nil ":~A~A" other-pk (incf j)))))
			    (setf pkn (string
				       (read-from-string
					(format nil ":net.xmpns.~A" tns))))
			    (or (find-package pkn) (make-package pkn :use nil))
			    (make-nsd pkn tns uri))
			other)))
	     (mapcar #'(lambda (f) (make-nsd (second f) (third f) (fourth f))) found)
	     ))

      (dolist (n needed)
	(if (member (second n) found :key #'second)
	    nil
	  (push n missing)))

      (dolist (nse (append (when (and base-ns (nse-defs base-ns))
			     (list base-ns))
			   (list *soap-namespaces-a* *soap-namespaces-b*
				 *soap-namespaces*
				 *wsdl-1.1-namespaces*
				 *wsdl-1.2-namespaces*
				 *wsdl-default-namespaces*)))
	(when (null missing) (return))
	(dolist (m missing)
	  (let ((n (member (second m) (nse-defs nse) :key #'nsd-package)))
	    (when n
	      (push (make-nsd (nsd-package (first n))
			      (nsd-prefix (first n))
			      (nsd-uri (first n)))
		    nse-defs)
	      (setf missing (remove m missing))))))

      (setf nse (make-nse* nil nse-defs))

      (values nse (if other-pk nil other) ambi missing))))


(defmethod wsdl-warn ((conn wsdl-file-connector) &rest fmt)
  (if (wsdl-warnings conn)
      (incf (wsdl-warnings conn))
    (setf (wsdl-warnings conn) 1))
  (apply 'warn fmt))
