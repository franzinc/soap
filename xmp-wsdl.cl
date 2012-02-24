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

;; $Id: xmp-wsdl.cl,v 2.17 2007/06/25 19:31:30 mm Exp $

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
   #:wsdl-add-form
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
   #:soap-array-mixin
   #:wsdl-include-file 
   #:wsdl-include-url
   #:wsdl-option
   ))

(eval-when (compile load eval)
  (defpackage :net.xmp.wsdl
    (:use)
    (:export 
     "definitions"
     "documentation"
     "import"
     "types"
     "message"
     "interface"
     "portType"     ;;; WSDL 1.1 (deprecated in 1.2, use interface?)
     "binding"
     "service"
     "operation"
     "input"
     "output"
     "part"
     "arrayType"
     "style"
     "transport"
     "soapAction"
     "use"
     "encodingStyle"
     "namespace"
     "port"
     "name"
     "location"
     "fault"
 
     ))

  (defpackage :net.xmp.wsdl.soap
    (:use)
    (:export
     "binding"
     "operation"
     "body"
     "address"
     "fault"
     "header"
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
  (defmacro wsdl (s) `(xmp-symbol ,s :net.xmp.wsdl))
  (defmacro wsoap (s) `(xmp-symbol ,s :net.xmp.wsdl.soap))
  (defmacro wmime (s) `(xmp-symbol ,s :net.xmp.wsdl.mime))
  (defmacro whttp (s) `(xmp-symbol ,s :net.xmp.wsdl.http))
  (defmacro wmsstk (s) `(xmp-symbol ,s :net.xmp.wsdl.msstk))
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


  (define-xmp-element nil (wsdl "definitions")
    `(:complex
      (:seq
       (:seq* ,(wsdl "import"))
       ,(wsdl "documentation")
       (:element ,(wsdl "types") (:complex (:seq* ,(xsd "schema") (:any))))
       (:set* ,(wsdl "message")
	      (:or ,(wsdl "interface") ,(wsdl "portType"))
	      ,(wsdl "binding")
	      ,(wsdl "service"))
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
		   :initarg :wsdl-options :initform nil)

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
   (all-names        :accessor wsdl-all-names        :initform nil)
   (props            :accessor wsdl-props            :initform nil)
   (compose-strategy :accessor wsdl-compose-strategy :initform nil)
   (encoder-name     :accessor wsdl-encoder-name     :initform nil)
   (object-classes   :accessor wsdl-object-classes
		     ;; collect defclass names generated in wsdl-emit-object-classes
		     :initform nil)
   (overloaded-ops   :accessor wsdl-overloaded-ops   :initform nil)
   ))

(defclass wsdl-included-connector (wsdl-file-connector)
  ())


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
				include import verbose
				&aux source vals (*xmp-warning-leader* "WSDL"))
  

  ;; namespaces -> :decode | namespace-map

  (cond (uri 
	 (when (or string stream file) (error "Ambiguous source."))
	 (multiple-value-bind (body rc h ruri)
	     (net.aserve.client:do-http-request uri)
	   (if (and body (eql rc 200))
	       (setf string body)
	     (error "URI ~A returned error ~S ~S ~S"
		    uri rc h ruri))
	   (setf source (list :uri uri string))
	   ))
	(stream
	 (when (or string uri file) (error "Ambiguous source."))
	 (setf source (list :stream (ignore-errors (namestring stream)))))
	(string
	 (when (or uri stream file) (error "Ambiguous source."))
	 (setf source (list :string "..." string))   ;;; bug16611
	 )
	(file
	 (when (or string stream uri) (error "Ambiguous source."))
	 (setf source (list :file file)))
	(t (error "No source specified.")))
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
		      :source source :lisp-package lisp-package
		      :wsdl-options (list :import import :include include)
		      init)))
    (or (typep conn 'wsdl-file-connector)
	(error "Class of ~S is not sub-class of wsdl-file-connector" conn))
    (setf vals (multiple-value-list 
       (cond (string (xmp-decode-string conn string))
	     (file   (xmp-decode-file conn file))
	     (stream (xmp-decode-stream conn stream)))))
    (wsdl-report-imports conn :verbose verbose)
    (values-list (append (list conn) vals))))

(defmethod wsdl-report-imports ((conn wsdl-file-connector) &key verbose)
  (and verbose
       (dolist (s (cdr (schema-imports conn)))
	 (when (eq (xsd "include") (schema-element-tag s))
	   (if (schema-raw-attribute s :included)
	       (format t "~&; schemaLocation=~A included with ~A~%"
		       (schema-raw-attribute s "schemaLocation")
		       (schema-raw-attribute s :included))
	     (format t "~&; schemaLocation=~A was not included.~%"
		     (schema-raw-attribute s "schemaLocation"))))
	 (when (eq (xsd "import") (schema-element-tag s))
	   (if (schema-raw-attribute s :imported)
	       (format t "~&; schemaLocation=~A imported with ~A~%"
		       (schema-raw-attribute s "schemaLocation")
		       (schema-raw-attribute s :imported))
	     (format t "~&; schemaLocation=~A was not imported.~%"
		     (schema-raw-attribute s "schemaLocation"))))
	 )))
  

(defun local-case-keyword (x)
  (etypecase x
    ((or string symbol)
     (let (pos (y (string-downcase (string x))))
       (when (setf pos (position #\: y)) (setf y (subseq y (1+ pos))))
       (read-from-string 
	(concatenate 'string ":" y " ")
	nil nil)))))

(defmethod schema-decode-attribute ((conn wsdl-file-connector)
				    name value nss &aux attr)
  (setf attr name)
  (cond
    ((eq attr (wsdl "arrayType"))
     (values attr (xmp-decode-qualified-name conn value nss :suppress-default t)))
    (t 
     (case (local-case-keyword attr)
       ((:binding :message :element) 
	(values attr (schema-targeted-name conn value :nss nss :suppress-default t)))
       (otherwise
	(call-next-method conn attr value nss))))))


(defmethod xmp-begin-message ((conn wsdl-file-connector))
  (list :seq1 (wsdl "definitions")))

(defmethod xmp-begin-message ((conn wsdl-included-connector))
  (list :seq1 (xsd "schema")))

(defmethod xmp-end-message ((conn wsdl-file-connector) data
			    &key types &allow-other-keys)
  (values data types))

(defmethod xmp-begin-element :around ((conn wsdl-file-connector)
				      (elt (eql (xsd "include")))
				      &rest options
				      &key attributes &allow-other-keys)

  (let* ((dattr (schema-decode-attributes conn attributes :in))
	 (from (xmp-getf conn dattr "schemaLocation"))
	 (filter (wsdl-option conn :include))
	 (data (when filter
		 (ignore-errors
		   (if (consp filter)
		       (apply (first filter) conn from (cdr filter))
		     (funcall filter conn from)))))
	 vals done x)
    (when data
      (let* ((schema (make-instance 'wsdl-included-connector
				    :message-dns (xmp-message-dns conn)
				    :base-dns    (xmp-base-dns conn)
				    :lisp-package (xmp-lisp-package conn)
				    :trim-whitespace (xmp-trim-whitespace conn)
				    :xml-syntax      (xmp-xml-syntax conn)
				    :wsdl-options    (wsdl-options conn)
				    )))
	
	(multiple-value-bind (contents types)
	    (xmp-decode-string schema data)
	  (declare (ignore contents))
	  (cond ((eq (xsd "schema") (first types))
		 (macrolet ((combine 
			     (slot)
			     `(setf (cdr (,slot conn))
				    (append (cdr (,slot schema)) (cdr (,slot conn))))))
		   (combine schema-elements)
		   (combine schema-types)
		   (combine schema-groups)
		   (combine schema-a-groups)
		   (combine schema-imports))
		 (setf done t)
		 )
		(t (warn "Included object is not a Schema: ~A" from))))

	))
    (setf vals (multiple-value-list (call-next-method)))
    (when done
      (setf x (first (schema-context conn)))
      (setf (schema-component-raw-attributes x)
	    (list* :included filter (schema-component-raw-attributes x))))
    (values-list vals)))

(defmethod xmp-begin-element :around ((conn wsdl-file-connector)
				      (elt (eql (xsd "import")))
				      &rest options
				      &key attributes &allow-other-keys)

  (let* ((dattr (schema-decode-attributes conn attributes :in))
	 (from (xmp-getf conn dattr "schemaLocation"))
	 (filter (wsdl-option conn :import))
	 (data (when filter
		 (ignore-errors
		   (if (consp filter)
		       (apply (first filter) conn from (cdr filter))
		     (funcall filter conn from)))))
	 vals done x)
    (when data
      (let* ((schema (make-instance 'wsdl-included-connector
				    :message-dns (xmp-message-dns conn)
				    :base-dns    (xmp-base-dns conn)
				    :lisp-package (xmp-lisp-package conn)
				    :trim-whitespace (xmp-trim-whitespace conn)
				    :xml-syntax      (xmp-xml-syntax conn)
				    :wsdl-options    (wsdl-options conn)
				    )))
	
	(multiple-value-bind (contents types)
	    (xmp-decode-string schema data)
	  (declare (ignore contents))
	  (cond ((eq (xsd "schema") (first types))
		 (macrolet ((combine 
			     (slot)
			     `(setf (cdr (,slot conn))
				    (append (cdr (,slot schema)) (cdr (,slot conn))))))
		   (combine schema-elements)
		   (combine schema-types)
		   (combine schema-groups)
		   (combine schema-a-groups)
		   (combine schema-imports))
		 (setf done t)
		 )
		(t (warn "Imported object is not a Schema: ~A" from))))

	))
    (setf vals (multiple-value-list (call-next-method)))
    (when done
      (setf x (first (schema-context conn)))
      (setf (schema-component-raw-attributes x)
	    (list* :imported filter (schema-component-raw-attributes x))))
    (values-list vals)))

(defmethod wsdl-include-file ((conn wsdl-file-connector) from)
  (when (probe-file from)
    (ignore-errors (file-contents from))))

(defmethod wsdl-include-url ((conn wsdl-file-connector) from)
  (mp:with-timeout 
   (300 nil)
   (ignore-errors (net.aserve.client:do-http-request :url from))))
  


(defmethod xmp-simple-content ((conn wsdl-file-connector)
			       (elt (eql (wsdl "documentation"))) data
			       &rest options 
			       &key &allow-other-keys)
  (declare (ignore options))
  (list :documentation data))

(define-schema-default-part
  wsdl-file-connector (wsdl "documentation") :documentation :cclass schema-text-component)

(define-schema-default-part wsdl-file-connector (wsdl "definitions") :definitions)

(define-schema-default-part wsdl-file-connector (wsdl "types") :types)
  

     
(defmethod schema-collect-target :around ((conn wsdl-file-connector) attributes)
  (declare (ignore attributes))
  (let ((found (call-next-method)))
    (when found
      (pushnew found (wsdl-targets conn) :test #'same-uri)
      found)))

(defmethod xmp-decode-element :around ((conn wsdl-file-connector)
				      (elt (eql (wsdl "definitions"))) (data t)
				      &rest options
				      &key attributes &allow-other-keys)
  (let ((found (schema-collect-target conn attributes)))
    (multiple-value-prog1
     (call-next-method)
     (when found (pop (schema-target conn))))))


(define-schema-collected-part
  wsdl-file-connector (wsdl "message")   :message   wsdl-messages)
(define-schema-collected-part
  wsdl-file-connector (wsdl "interface") :interface wsdl-interfaces)
(define-schema-collected-part
  wsdl-file-connector (wsdl "portType")  :port-type wsdl-port-types)
(define-schema-collected-part
  wsdl-file-connector (wsdl "binding")   :binding   wsdl-bindings)
(define-schema-collected-part
  wsdl-file-connector (wsdl "service")   :service   wsdl-services)

(define-schema-collected-part
  schema-file-connector (wsdl "import") :import   schema-imports)
;;
;; <wsdl:import namespace="nn" location="uri" />  uri points to a <schema> instance
;; <xsd:include id="nn" schemaLocation="uri" />  uri points to a <schema> instance
;; <xsd:import  id = ID  namespace = anyURI   schemaLocation = anyURI />
;;              used only to identify a namespace used with xmlns attributes


(define-schema-simple-part wsdl-file-connector (wsdl "input")      :input)
(define-schema-simple-part wsdl-file-connector (wsdl "output")     :output)
(define-schema-simple-part wsdl-file-connector (wsoap "binding")   :soap-binding)
(define-schema-simple-part wsdl-file-connector (wsoap "operation") :soap-operation)
(define-schema-simple-part wsdl-file-connector (wsoap "body")      :soap-body)
(define-schema-simple-part wsdl-file-connector (wsoap "address")   :soap-address)
(define-schema-simple-part wsdl-file-connector (wsoap "header")    :soap-header) ;;???

(define-schema-named-part wsdl-file-connector (wsdl "operation")  :operation)
(define-schema-named-part wsdl-file-connector (wsdl "part")       :part)
(define-schema-named-part wsdl-file-connector (wsdl "port")       :port)
(define-schema-named-part wsdl-file-connector (wsdl "fault")      :fault)
(define-schema-named-part wsdl-file-connector (wsoap "fault")     :soap-fault)

(define-schema-ignored-part wsdl-file-connector (xsd "annotation"))

(define-schema-ignored-part wsdl-file-connector (wmime "multipartRelated"))
(define-schema-ignored-part wsdl-file-connector (wmime "binding"))
(define-schema-ignored-part wsdl-file-connector (wmime "mimeXml"))
(define-schema-ignored-part wsdl-file-connector (wmime "content"))

(define-schema-ignored-part wsdl-file-connector (whttp "binding"))
(define-schema-ignored-part wsdl-file-connector (whttp "operation"))
(define-schema-ignored-part wsdl-file-connector (whttp "urlEncoded"))
(define-schema-ignored-part wsdl-file-connector (whttp "address"))

(define-schema-ignored-part wsdl-file-connector (wmsstk "binding"))

;;; We also ignore any alements that are in application namespaces
(defmethod net.xmp::schema-make-element ((conn wsdl-file-connector) elt &rest keys
				&key tag &allow-other-keys)
  (let (outer (pk (symbol-package elt)))
    (cond ((or (eq pk (find-package :net.xmp.schema))
	       (eq pk (find-package :net.xmp.soap))
	       (eq pk (find-package :net.xmp.wsdl))
	       (eq pk (find-package :net.xmp.wsdl.soap)))
	   (call-next-method))
	  (tag (call-next-method))
	  ((member elt (net.xmp::schema-ignored-messages conn)) (call-next-method))
	  ((or (null (setf outer (first (net.xmp::schema-component-stack conn))))
	       (eq :ignored (schema-element-key outer)))
	   (call-next-method))
	  (t (apply #'call-next-method conn elt :tag :ignored keys)))))
       




;;; Allow some common errors:
(define-schema-collected-part schema-file-connector
  (wsdl "element")     :element      schema-elements)



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



(defun schema-true-p (v)
  (or (equal v "1") (eql v 1) (equalp v "true")))

(defmethod wsdl-parts-to-type ((conn wsdl-file-connector) msg &key options)
  `(:complex
    (:seq
     ,@(mapcar #'(lambda (part &aux name type
			       (nillable (schema-true-p
					  (schema-decoded-attribute part "nillable"))))
		   (multiple-value-setq (name type)
		     (wsdl-message-part-parts conn part))
		   (if type
		       `(:element (,name) ,type 
				  ,@(and nillable (list :nillable t))
				  )
		     name))
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
      (setf ctype (wsdl-lookup-type conn edef nil))
      (setf ctype (schema-decoded-attribute edef "type"))
      (setf ctype (xmp-any-type conn)))
  (wsdl-index-form
   conn
   ;; Need to ensure that there is no shared structure among definitions.
   `(define-soap-element nil
      ,(qt (schema-component-name edef))
      ,(qt (copy-tree ctype))
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
      (schema-parts-to-type typedef :conn conn :error-p :report))
    
    (cond (new-def)
	  ((eq complex :null)
	   (setf new-def (xmp-any-type conn)))
	  ((consp complex)
	   (cond
	    ((setf complex-content (schema-single-part typedef :complex-content))
	     (or (when (setf item (schema-single-part complex-content :restriction))
		  (let* ((r-base (schema-decoded-attribute item "base"))
			 (r-attrs (schema-single-part item :attribute :more-p t))
			 r-attr r-atype)
		    ;; 2006-10-24 mm rev: There may be multiple <attribute> elements.
		    (dolist (r r-attrs)
		      (when (setf r-atype (schema-decoded-attribute r "arrayType"))
			(setf r-attr r)
			(return)))

		    (when *wsdl-debug*
		      (format
		       t "~&wsdl-define-type: r-base=~S   r-attr=~S  r-atype=~S ~%"
		       r-base r-attr r-atype))

		    (cond
		     ((and (eq r-base (enc "Array"))
			   r-attr
			   (eq (schema-component-name r-attr) (enc "arrayType"))
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
		     ((and (eq r-base (enc "Array"))
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
		       (schema-listify complex-content))
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
		  (mapcar #'schema-listify (schema-component-content complex-content))
		  typedef)))
	    
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

	    ((schema-parts-to-type typedef :conn conn))

	    (t (wsdl-warn conn "wsdl-define-type - unknown complex type parts ~S in ~S"
			  complex typedef))
	    ))

	  ((eq simple :null)
	   (setf new-def `(:simple ,(xsd "string"))))
	  ((consp simple)
	   (or
	    (let* ((r (schema-single-part typedef :restriction))
		   (base (when r (schema-decoded-attribute r "base"))))
	      (when base

		;; look for parts <enumeration "value" val> 
		;;   and collect values  - need extension to :simple ???
		;; look for part <maxLength value=n/>   ???

		(setf new-def `(:simple ,base))))
	    (let* ((sp (schema-single-part typedef :simple-content))
		   (ex (when sp (schema-single-part sp :extension)))
		   (base (when ex (schema-decoded-attribute ex "base")))
		   )
	      (when base (setf new-def `(:simple ,base)) t))
	    (when (setf x (schema-single-part typedef :union))
	      ;; extend to a :union type ???
	      (setf new-def `(:simple ,(xsd "string"))))
	    (wsdl-warn conn "wsdl-define-type - unknown simple type ~S in ~S"
		       simple typedef)))

	  (t (wsdl-warn
	      conn "wsdl-define-type - unknown form ~S err was ~A" typedef err)))
    (when new-def
      (wsdl-index-form conn
		       `(define-soap-type nil ,(qt name) ,(qt (copy-tree new-def)))))
      
    ))

(defun qt (val)
  (typecase val
    (number val)
    (string val)
    (keyword val)
    (otherwise 
     (if (and (consp val) (eq 'quote (first val)))
	 val
       (list 'quote val)))))

;; 2006-10-24 mm rev: Sometimes we need to add quote, and sometimes we don't.
;;    Sometimes quote must not be there.
(defun unquote (val)
  (if (and (consp val) (eq 'quote (first val)))
      (second val)
    val))

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
	 ((atom def) (return (values def (or name def) type)))
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


(defmethod wsdl-lookup-type ((conn wsdl-file-connector) (of schema-component) recursive
			     &aux name)
  (or (when (setf name (schema-decoded-attribute of "type"))
	(wsdl-lookup conn :type name recursive))
      (case (xmp-xml-syntax conn)
	(:strict nil)
	(otherwise
	 (or (when (setf name (schema-decoded-attribute of 'loose-type))
	       (wsdl-lookup  conn :type name recursive))
	     (when (setf name (schema-decoded-attribute of 'targeted-type))
	       (wsdl-lookup conn :type name recursive)))))))

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
		   
(defmethod wsdl-message-part-parts ((conn wsdl-file-connector) part)
  (let* ((name (schema-decoded-attribute part "name"))
	 (type (schema-decoded-attribute part "type"))
	 (elt  (schema-decoded-attribute part "element"))
	 )
    (cond 
     ((and name type (null elt)) (values name type))
     ((and elt (null type))(if (schema-lookup-element conn elt)
			       (values elt nil)
			     (error "cannot find message element def")))
     (t (error "strange message part")))))


(defmethod wsdl-message-part-element ((conn wsdl-file-connector) part)
  (wsdl-message-part-parts conn part))

(defmethod wsdl-do-element
  (conn style method-name-spec action intern-name bind msg
	&aux
	(method-wsdl-name (if (consp method-name-spec)
			      (first method-name-spec)
			    method-name-spec))
	(method-rename (and (consp method-name-spec)
			    (not (eq method-wsdl-name (second method-name-spec)))
			    (second method-name-spec)))
	(method-name method-wsdl-name)
	(mpk (when (symbolp method-name) (symbol-package method-name)))
	(parts (when msg (schema-collected-parts msg :message :part)))
	(soap-body (schema-single-part bind :soap-body))
	(estyle (when soap-body
		  (schema-decoded-attribute soap-body "encodingStyle")))
			   
	;;??? This seems to be use-less?
	;;(use    (schema-decoded-attribute soap-body "use"))
			   
	(ns  (when soap-body (schema-raw-attribute soap-body "namespace")))
	(npk (when ns (net.xmp::xmp-uri-to-package conn ns :dns)))
	pk
	(pktag "tns")
	def-tail tail-props msg-type elt def nillable)

  (or (let* ((tg (wsdl-targets conn)) pk2)

	;; Look in the list of all targetNamespace attributes
	;; in the definition.

	(cond

	 ;; If there is only one targetNamespace
	 ;;    and it has a package, it is the package of this namespace
	 ((and (eql 1 (length tg))
	       (setf pk2 (net.xmp::xmp-uri-to-package conn (first tg) :dns)))
	  (setf pk pk2))

	 ;; If the method name is a symbol it must be in the right package.
	 ((setf pk mpk))

	 ;; As a last resort, try to locate the namespace uri, but in that
	 ;;  case signal a warning.
	 (t (when ns (setf pk (net.xmp::xmp-uri-to-package conn ns :dns))) nil)
	 ))			   
      (when (and ns (null npk)) (pushnew ns (wsdl-undef-ns conn) :test #'string=)))
  (setf def-tail
	`(,@(when (and ns npk)
	      `(:namespaces
		(nil (,(when npk (intern (package-name npk) :keyword))
		      ,pktag
		      ,ns))))
	    ,@(when estyle `(:encoding ,estyle))
	    ,@(when action `(:action ,action))
	    ))
  (setf tail-props (append (when ns (list :namespaces))
			   (when estyle (list :encoding))
			   (when action (list :action))))
  (when intern-name
    (setf method-name (etypecase method-wsdl-name
			(symbol method-wsdl-name)
			(string (if pk
				    (wsdl-make-symbol conn method-wsdl-name nil pk)
				  method-wsdl-name)))))
  (when (schema-true-p (schema-decoded-attribute msg "nillable"))
    (setf nillable (list :nillable t)))

  (values
   (cond
    ((string-equal style "rpc")
     
     ;; Operation name is method name, each part is a parameter
     ;; SOAP method call and response are always structs.

     (let ((mname method-name)
	   (mtype (wsdl-parts-to-type conn msg :options def-tail))
	   overload)
       (when (wsdl-lookup conn :element mname nil)
	 (let ((i 1)
	       (pk (when (symbolp mname) (symbol-package mname)))
	       mn)
	   ;; This element was already defined once - this is an
	   ;; overloaded operation.  We need to define a new type
	   ;; name for the overloaded operations.
	   (or (when (consp method-name-spec)
		 ;; Try to use the message name as an alternate name
		 ;;  for reply elements
		 (setf mn (schema-component-name msg))
		 (when pk (setf mn (wsdl-make-symbol conn mn nil pk)))
		 (if (wsdl-lookup conn :type mn nil)
		     nil
		   (setf method-name mn)))
	       (when (setf mn method-rename)
		 (when pk (setf mn (wsdl-make-symbol conn mn nil pk)))
		 (if (wsdl-lookup conn :type mn nil)
		     nil
		   (setf method-name mn)))
	       (loop (setf mn (format nil "~A_~A" method-wsdl-name i))
		     (when pk (setf mn (wsdl-make-symbol conn mn nil pk)))
		     (if (wsdl-lookup conn :type mn nil)
			 (incf i)
		       (return (setf method-name mn)))))
	   (when (atom method-name-spec)
	     (push method-name (wsdl-overloaded-ops conn))
	     (setf overload t)
	     )))
       (if overload
	   `(define-soap-type nil ,(qt method-name)
	      '(:complex
		(:seq (:element ,mname ,(copy-tree mtype) ,@nillable))
		,@(copy-tree def-tail)))
	 `(define-soap-element nil ,(qt method-name) ,(qt (copy-tree mtype)) ,@nillable))
       ))

    ((or (string-equal style "document")
	 (and (null style) parts (null (cdr parts))))

     ;; Operation name is the element in the part
     ;;  and it was already defined in the schema.

     (cond 
      ((cdr parts)
       (error "Document style message with more than one part: ~S"
	      (schema-listify msg)))

      ;; usual document  style part has the form
      ;;   <part name="xxx" element="defined-element" />
      ((and (setf elt (schema-decoded-attribute (first parts) "element"))
	    (progn
	      (if (eq :doc intern-name)
		  (setf method-name elt)
		(setf method-name (string elt)))
	      (multiple-value-setq (msg-type def)
		(wsdl-lookup conn :element method-name :any))
	      msg-type)
	    def)
       )
      
      ;; 14-Jan-05  mm: sometimes document style part has the form
      ;;     <part element="ignored-name" type="defined-type" />
      ((and (schema-decoded-attribute (first parts) "type")
	    (progn
	      (multiple-value-setq (msg-type def)
		(wsdl-lookup-type conn (first parts) :complex))
	      (or (consp msg-type) def)))
       )

      ((cdr (schema-imports conn))
       (error "~A~S~A"
	      "Cannot determine message type in "
	      (schema-listify msg)
	      " - type may be defined in included Schema."))
      (t (error "Cannot determine message type in ~S" (schema-listify msg))))



     ;; We need to update the :namespaces option in the def
     ;;    def is in list of forms to be evaluated...

     (flet ((update (props place tail case)
		    (dolist (p props (nconc place tail))
		      (when (getf (cddr place) p)
			(error "Multiple definitions (~A) of ~S on ~S in ~S + ~S"
			       case p method-name place tail)))))
       (cond ((consp msg-type) (update tail-props msg-type def-tail 1))
	     (def (cond ((consp (maybe-second (fourth def)))
			 (update tail-props (maybe-second (fourth def)) def-tail 2)
			 (setf msg-type (maybe-second (fourth def))))
			(t (setf (fourth def)
				 (qt (list* :simple (maybe-second (fourth def))
					    def-tail)))
			   (setf msg-type (maybe-second (fourth def))))))))
     nil)
    (t (error "Unknown message style ~S in ~S." style (schema-listify msg)))
    )

   method-name msg-type))

(defmethod wsdl-response-name ((conn wsdl-file-connector) op-name
			       &aux
			       (resp (concatenate 'string (string op-name) "Response")))
  ;; rfe6555   30    8 wsdl code gen should allow qualified response elements
  (typecase op-name 
    (null   (error "Nil may not be an element name"))
    (string resp)
    (symbol (case (wsdl-option conn :response)
	      (:symbol (wsdl-make-symbol conn resp nil (symbol-package op-name)))
	      (otherwise resp)))
    (otherwise (error "Element name must be symbol or string: ~S" op-name))))

(defmethod wsdl-bind-operation ((conn wsdl-file-connector) b mode eval prefix suffix)
  (declare (ignore eval))
  (flet ((part-name (part) (wsdl-message-part-element conn part))
	 )
    (let* ((op-name (schema-component-name b))
	   (soap-op  (schema-single-part b :soap-operation))
	   (action   (schema-raw-attribute soap-op "soapAction"))
	   (messages (cdr (wsdl-messages conn)))
	   style-key
	   (style    (let ((s (or (schema-raw-attribute soap-op "style")
				  (wsdl-soap-style conn)))
			   rpcelt enames)
		       #+ignore
		       (when (string-equal s "document")
			 ;; True document style defines all messages
			 ;;    with exactly one part???
			 ;;    and with unique element names???
			 ;;mm: but we must only look at input messages, and the
			 ;; loop below looks at all -- just skip this check for now...
			 (dolist (m messages)
			   (let (e p)
			     (setf p (schema-collected-parts m :message :part))
			     (or (and p (null (cdr p))
				      (null (member (setf e (schema-decoded-attribute
							     (first p) "element"))
						    enames)))
				 (return (setf rpcelt m s "rpc")))
			     (push e enames)
			     )))
		       ;;(format t "~&~S~%" enames)
		       (cond ((string-equal s "document") (setf style-key :document))
			     (t (setf style-key :rpc)))
		       (when *wsdl-debug* 
			 (format
			  t "~&;WSDL style=~S raw=~S conn=~S rpcelt=~S enames=~S ~%"
			  s (schema-raw-attribute soap-op "style")
			  (wsdl-soap-style conn) rpcelt enames))
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
		     (or
		      (schema-lookup-component
		       conn #'wsdl-messages #'schema-component-name in-msg-name)
		      (error "Cannot find input message ~A" in-msg-name))))
	   (in-parts (when in-msg
		       (schema-collected-parts in-msg :message :part)))
	   (out-msg (when out-msg-name
		      (or
		       (schema-lookup-component
			conn #'wsdl-messages #'schema-component-name out-msg-name)
		       (error "Cannot find output message ~A" out-msg-name))))
	   (out-parts (when out-msg
			(schema-collected-parts out-msg :message :part)))
	   (cx (when in-msg (position in-msg messages)))
	   comment done-form doc-type doc-ret ret-name)

      (when *wsdl-debug*
	(format t "~&;WSDL op=~S  in-msg=~S ~S  out-msg=~S ~S~%" 
		op-name in-msg-name (when in-msg (length in-parts)) 
		out-msg-name (when out-msg (length out-parts))))

      (let (op-rename info)
	(when in-msg
	  (multiple-value-setq (done-form op-rename doc-type)
	    (wsdl-do-element conn style op-name action
			     (if (and (eq style-key :document) (eql 1 (length in-parts)))
				 :doc
			       t)
			     bind-in in-msg))
	  (when done-form (wsdl-index-form conn done-form))
	  (when *wsdl-debug*
	    (format t "~&;WSDL in-msg done-form=~S  op-rename=~S  doc-type=~S ~%"
		    done-form op-rename doc-type))
	  )
	(when out-msg
	  (multiple-value-setq (done-form ret-name doc-ret)
	    (wsdl-do-element
	     conn style 
	     (list (wsdl-response-name conn op-name) (wsdl-response-name conn op-rename))
	     nil nil bind-out out-msg))
	  (when done-form (wsdl-index-form conn done-form))
	  (when *wsdl-debug*
	    (format t "~&;WSDL out-msg done-form=~S  ret-name=~S  doc-ret=~S  ~%"
		    done-form ret-name doc-ret))
	  )
	(when (and in-msg out-msg);;???  in-out messages notification messages...
	  (wsdl-index-form
	   conn
	   (let* ((def-name (let ((context :call))
			      (wsdl-compose-parts conn nil (list prefix
							       (ecase suffix
								 (:index (incf cx))
								 (:compose
								  (setf context nil)
								  (string op-rename))
								 (:message op-rename)))
						  :export t
						  :context context)))
		  (one-part (and (eq mode :client)
				 in-parts
				 (null (cdr in-parts))
				 (first in-parts)))
		  (one-type (when one-part
			      (case style-key
				(:rpc (schema-decoded-attribute one-part "type"))
				(:document doc-type))))
		  (one-def  (and one-type
				 (cond
				  ((eq style-key :document)
				   (case mode (:client (setf one-type doc-type))))
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
				  (mapcar #'(lambda (p)
					      (or (xmp-pick-name nil p) (gensym)))
					  (cdr (second  one-def)))
				(mapcar #'part-name in-parts)))
		  (key-args   (mapcar #'(lambda (part)
					  (wsdl-file-local-name conn (string part)))
				      in-elts))
		  (arglist (mapcan #'(lambda (elt arg) (list (string elt) arg))
				   in-elts key-args))
		  )
	     (when (and (eq style-key :rpc)
			(not (equal op-name op-rename)))
		 ;; if we have an overloaded rpc message name, we need
		 ;;  to access through a defined type
		 (setf arglist `(,(qt op-name) (list ,@arglist))))
	     (ecase mode
	       (:client
		(setf comment
		      (list (format nil "Send client message ~A " op-name)
			    op-name))
		(setf info :top-level)
		`(defun ,def-name 
		   (&key ,@key-args)
		   (let ((,(wsdl-file-local-name conn "conn")
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
		      ,(if (and one-def (eq style-key :rpc))
			   `(call-soap-method ,(wsdl-file-local-name conn "conn")
					      ,(qt op-rename)
					      ,(string (part-name one-part))
					      (list ,@arglist))
			 `(call-soap-method
			   ,(wsdl-file-local-name conn "conn")
			   ,(qt op-rename)
			   ,@arglist))
		      ,(wsdl-file-local-name conn "conn")))))
	       (:server
		(let* ((one-ret (and out-parts
				     (null (cdr out-parts))
				     (first out-parts)))
		       (ret-type (when one-ret
				   (case style-key
				     (:rpc (schema-decoded-attribute one-ret "type"))
				     (:document doc-ret))))
		       (ret-def  (and ret-type
				      (cond
				       ((eq style-key :document) (setf ret-type doc-ret))
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
						   (wsdl-file-local-name
						    conn 
						    (or (xmp-pick-name nil part)
							(gensym))
						    :preserve))
					       (cdr (second ret-def)))
				     (mapcar
				      #'(lambda (part)
					  (wsdl-file-local-name
					   conn (part-name part) :preserve))
				      out-parts)))
		       (ret-vars (mapcar #'(lambda (r)
					     (wsdl-file-local-name conn (string r)))
					 ret-keys))		       
		       (ret-parts (if ret-def
				      (mapcan (lambda (part var)
						(list 
						 (qt (xmp-pick-name nil part))
						 var))
					      (cdr (second  ret-def))
					      ret-vars
					      )
				    (mapcan 
				     #'(lambda (part var)
					 (list (qt (part-name part)) var))
				     out-parts ret-vars)
				    ))
		       (key-list (let (r) 
				   ;; generate w args for server function [bug16269] 
				   (dotimes (i (length in-elts) (reverse r))
				     (push (read-from-string (format nil ":k~A " i))
					   r))))
		       )
		  (push `(,(qt op-name) ,(qt in-elts)
			  ,@(when action (list :action action))
			  :lisp-name (list ,(qt def-name) ,@key-list)
			  :return ,(if (eq :symbol (wsdl-option conn :response))
				       ret-name
				     (string ret-name))
			  )
			(wsdl-server-exports conn))
		  (setf comment
			(list* (format nil "Handler for message ~A" op-name)
			      (mapcar 
			       #'(lambda (key elt)
				   (format
				    nil
				    "    Keyword argument ~A represents element ~S"
				    key elt))
			       key-list in-elts)))
		  (setf info :top-body)
		  `(defun ,def-name (&key ,@(mapcar #'(lambda (k) 
							(wsdl-file-local-name
							 conn (string k)))
						    key-list))
		     (let ,ret-vars
		       ,(wsdl-generate-code conn mode :method-body op-name)   
		       ,(if (and ret-def (eq style-key :rpc))
			    `(list ,(string (part-name (first out-parts)))
				   (list ,@ret-parts))
			  `(list ,@ret-parts))))

		  ))
	       ))
	   :mode mode :info info
	   :comment comment)))
      )))

(defmethod wsdl-generate-code ((conn wsdl-file-connector) (mode t) (info t)
			       (op t) &rest args)
  ;; Called mostly from wsdl-index-form, but sometimes for inner forms as well
  (let ((form
	 (cond ((and (null op) (null (cdr args))) (first args))
	       ((eq info :method-body) "INSERT BODY HERE")
	       (t (cons op args)))))
    (if (wsdl-option conn :generate-comments)
	(list 'if nil
	      (list* 'wsdl-generate-code '<conn> mode info op args)
	      form)
      form)))




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
				  
(defmethod wsdl-file-local-name ((conn wsdl-file-connector) form &optional preserve
			     &aux (file-package (wsdl-file-package conn)) pos)
  (etypecase form
    ((or string symbol)
     (setf form (string form))
     
     ;; This function is used only to make Lisp symbols for generated code.
     (when (setf pos (position #\: form))
       ;; Drop namespace qualifier if there.
       (setf form (subseq form (1+ pos))))

     (etypecase preserve
       ((member :preserve) (wsdl-make-symbol conn form nil file-package))
       (null (wsdl-make-symbol conn nil form file-package))
       ))))



	  
					   


(defmethod wsdl-constructor ((conn wsdl-file-connector) type-name &key prototype)
  (wsdl-accessor conn type-name :new- :prototype prototype))

(defmethod wsdl-decoder ((conn wsdl-file-connector) type-name &key prototype)
  (wsdl-accessor conn type-name :decode- :prototype prototype))

(defmethod wsdl-encoder ((conn wsdl-file-connector) &key prototype)
  (wsdl-accessor conn :encode 
		 (case (wsdl-option conn :object-access)
			(:object-class nil)
			(otherwise (wsdl-option conn :prefix)))
		 :prototype prototype :keep-symbol t))

(defmethod wsdl-array-items ((conn wsdl-file-connector) &key prototype)
  (wsdl-accessor conn :array-items 
		 (case (wsdl-option conn :object-access)
			(:object-class nil)
			(otherwise (wsdl-option conn :prefix)))
		 :prototype prototype :keep-symbol t))

(defmethod wsdl-accessor ((conn wsdl-file-connector) type-name prefix
			  &key prototype keep-symbol)
  (wsdl-compose-parts conn
		      (case (wsdl-option conn :object-access)
			(:object-class 
			 (cond (prototype (symbol-package prototype))
			       ((symbolp type-name) (symbol-package type-name))
			       (t (error "Cannot determine package for accessor."))))
			(otherwise nil))
		      (list prefix 
			    (if keep-symbol
				type-name
			      (string type-name)))
		      :export t
		      :context :accessor
		      ))

(defmethod wsdl-compose-name ((conn wsdl-file-connector) package &rest parts)
  (wsdl-compose-parts conn package parts :export t))

(defmethod wsdl-case-mode ((conn wsdl-file-connector))
  (case *current-case-mode*
    (:case-sensitive-lower :modern)
    (otherwise             :ansi)))

(defmethod wsdl-compose-parts ((conn wsdl-file-connector) package parts
			       &key export context)
  ;; In :modern mode, we always keep the case of all the parts.
  ;; In :ansi mode, if any parts have a mixed component, then we keep 
  ;;    the case of each component and intern the string, otherwise
  ;;    read-from-string
  (let* ((mode (wsdl-case-mode conn)) string keep retry sym)
    (loop
     (setf string (format
		   nil "~{~A~}"
		   (mapcar #'(lambda (part &aux composed)
			       (setf composed
				     (case context
				       (:call (format nil "~A" part))
				       (otherwise
					(typecase part
					  (null "")
					  (string (wsdl-compose-part
						   conn part context :retry retry))
					  (symbol
					   (setf part (string part))
					   (case mode
					     (:ansi
					      (if (equal part (string-upcase part))
						  (string-downcase part)
						part))
					     (otherwise part)))
					  (otherwise (format nil "~A" part))))))
			       (or (equal composed (string-downcase composed))
				   (setf keep t))
			       composed)
			   (cond ((listp parts) parts)
				 (t (list parts))))))
     (setf sym
	   (if keep
	       (wsdl-make-symbol
		conn string nil package
		:from parts :export export :nil-if-conflict (null retry))
	     (wsdl-make-symbol
	      conn nil string package
	      :from parts :export export :nil-if-conflict (null retry))))
     (cond (sym (return sym))
	   (retry (error "failure during retry")))
     (setf retry t))))
    
(defmethod wsdl-compose-part ((conn wsdl-file-connector) inpart context &key retry
			      &aux
			      (part (string inpart))
			      (comp (wsdl-compose-strategy conn))
			      exceptions
			      ;;(conflict (wsdl-maybe-conflicts conn))
			      sub)
  (when (consp comp) (setf exceptions (cdr comp)) (setf comp (car comp)))
  (dolist (e exceptions)
    (cond ((equal part e) (return-from wsdl-compose-part part))
	  ((atom e))
	  ((equal part (first e)) (return-from wsdl-compose-part (second e)))))
  (case comp
    (:hyphen
     ;; break string at down-up case shift, insert hyphen, downcase all
     (break-at-case-shifts part)
     )
    (:hyphen-if-ok
     ;; apply only if maybe-conflicts is nil
     (if (null retry)
	 (break-at-case-shifts part)
       part))
    (:downcase  (string-downcase part))
    (:downcase-if-ok
     (if (null retry)
	 (string-downcase part)
       part))
    (:capitalize-all
     (cond ((equal part "") part)
	   ((eql (setf sub (subseq part 0 1)) (setf sub (string-upcase sub))) part)
	   (t (concatenate 'string sub (subseq part 1)))))
    (:capitalize-accessor
     (cond ((equal part "") part)
	   ((not (eq context :accessor)) part)
	   ((equal (setf sub (subseq part 0 1)) (setf sub (string-upcase sub))) part)
	   (t (concatenate 'string sub (subseq part 1)))))
    (otherwise
     ;; keep the case
     part)))

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
  (if out (string-downcase out) ""))

(defun to-string (x)
  (typecase x
    (string x)
    (null "")
    (symbol (string x))
    (otherwise (format nil "~A" x))))

(defmethod wsdl-make-symbol ((conn wsdl-file-connector) pname string package
			     &key from export nil-if-conflict)
  ;; if pname is specified, then intern the string as given
  ;; if string is given, 
  (let* ((all-names (wsdl-all-names conn))
	 (file-package (wsdl-file-package conn))
	 (symbol (let ((*package*
			(typecase package
			  (null file-package)
			  (package package)
			  (otherwise 
			   (or (find-package package) 
			       (error "Cannot find the package ~S" package))))))
		   (cond (pname (intern pname))
			 (string (or (ignore-errors (read-from-string string))
				     (error "Cannot make symbol from ~A" string)))
			 (t (error "Cannot make symbol from nothing")))))
	 (name (when symbol (string symbol)))
	 (key (when name (string-downcase name)))
	 (known (gethash key all-names))
	 x)
    (and (consp from) (null (cdr from)) (setf from (first from)))
    (typecase from
      (null (setf from (or pname
			   (case (wsdl-case-mode conn)
			     (:modern name)
			     (otherwise (if (equal name (string-upcase key))
					    key
					  name))))))
      (string nil)
      (symbol (setf from (string from)))
      (cons (setf from (mapcar #'to-string from))))
    (cond ((null key) (error "Cannot make symbol without a name"))
	  ((null known)
	   (setf (gethash key all-names) (list key (list symbol from))))
	  ((setf x (assoc symbol (cdr known)))
	   (when (not (member from (cdr x) :test #'equal))
	     ;; We are making one symbol from more than one (different) strings
	     (when nil-if-conflict (return-from wsdl-make-symbol nil))
	     (pushnew key (wsdl-maybe-conflicts conn) :test #'equal)
	     (push from (cdr x))))
	  (t (push (list symbol from) (cdr known)))
	  )
    (wsdl-export-symbol conn symbol export)))

(defmethod wsdl-export-symbol ((conn wsdl-file-connector) symbol from)
  (let* ((spk (symbol-package symbol)) (epk spk) pk)
    (when (typecase from
	    (null nil)
	    (package (setf epk from))
	    ((or string symbol) (cond ((eq from t))
				      ((setf pk (find-package from)) (setf epk pk))))
	    (otherwise t))
      (when (and epk (if (eq epk (find-package :keyword))
			 (or (eq spk epk)
			     (error "Importing symbol ~S to keyword package." symbol))
		       t))	
	(or (eq spk epk) (import symbol epk))
	(let* ((elist (wsdl-option conn :exported-symbols))
	       (pkname (intern (package-name epk) :keyword))
	       (pklist (assoc pkname (cdr elist))))
	  (if pklist
	      (pushnew symbol (cdr pklist))
	    (push (list pkname symbol) (cdr elist))))
	(export symbol epk)))
    symbol))



(defmethod wsdl-add-form  ((conn wsdl-file-connector) form &rest comments)
  ;; exported function to add code
  ;; skip call to wsdl-generate-codde for these forms
  (wsdl-index-form conn form :mode :user :comment comments))

(defmethod wsdl-index-form ((conn wsdl-file-connector) form &key mode info op comment
			    &aux comm2 (comments (when comment
						   (if (consp comment)
						       comment
						     (list comment)))))
  (case mode
    (:user nil)
    (otherwise
     (multiple-value-setq (form comm2)
       ;; [rfe6553]
       (apply #'wsdl-generate-code conn mode info
	      (or op (when (consp form) (first form)))
	      (if (consp form) (cdr form) (list form))))
     (when comm2
       (setf comments (append comments (if (consp comm2)
					   comm2
					 (list comm2)))))
     (when (and (wsdl-option conn :generate-comments)
		(consp form) (eq 'if (first form)) (null (second form)))
       (let* ((*print-pretty* nil)
	      (w (third form))
	      (ws (format nil "~S" w))
	      (out (fourth form))
	      (ln (length ws)))
	 (cond ((< ln 100))
	       (t (setf ws (format nil "(~S <conn> ~S ~S ~S ...)" 
				   (first w) (third w) (fourth w) (fifth w)))
		  (setf ln (length ws))
		  (cond ((< ln 100))
			( t (setf ws (format nil "(w-g-c <conn> ~S ~S ~S ...)" 
					     (third w) (fourth w) (fifth w)))))))
	 (setf comments (append comments (list ws)))
	 (setf form out)))))
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
			   &aux maybe-conflicts change pk name)
  ;; Called on content of wsdl-def-list
  (case (setf maybe-conflicts (wsdl-maybe-conflicts conn))
    (:unknown (setf maybe-conflicts nil)))
  (labels ((walk (form)
		 (typecase form
		   (symbol (when (setf pk (symbol-package form))
			     (pushnew pk pl)))
		   (cons (walk (car form))
			 (walk (cdr form))))))
    (dolist (def form)
      (when (consp def)
	(when (consp (setf def (first def)))
	  (case (first def)
	    ((define-soap-type define-soap-element)
	     (when (symbolp (setf name (maybe-second (third def)))) 
	       (wsdl-export-symbol conn name t)))
	    ((defun defmethod defvar defparameter)
	     (wsdl-export-symbol conn (second def) t))
	    )
	  (walk def))))
    (or (equal maybe-conflicts (wsdl-maybe-conflicts conn))
	(setf change t))
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

(defmethod wsdl-print-env ((conn wsdl-file-connector) thunk)
  (let ((*package* (wsdl-file-package conn))
	(*print-case*
	 (case (wsdl-case-mode conn)
	   (:modern *print-case*)
	   (otherwise :downcase))))
    (funcall thunk)))

(defmethod wsdl-describe ((conn wsdl-file-connector) defs stream depth)
  ;; defs -> (def ...)
  ;; def  -> ((comment :key key) entry ...)
  ;; entry -> fn-name | (fn-name comment-string [more-comments...] [element-name])
  (dolist (def defs)
    (cond
     ((eq 'defun (getf (cdr (first def)) :key))
      (format stream "~A~%" (first (first def)))
      (dolist (entry (cdr def))
	(typecase entry
	  (cons
	   (format stream "~&~%     ~A  ~A~%" 
		   (first entry) (second entry))
	   (do ((tl (cddr entry) (cdr tl))) ((atom tl))
	     (cond ((and (symbolp (first tl)) (null (cdr tl)))
		    (describe-soap-method 
		     conn (first tl) :wsdl conn :stream stream :depth depth))
		   (t (format stream "~&~%     ~A~%" (first tl))))))
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
				(expand-singleton
				 (if (and object-class message-method-prefix)
				     nil
				   t))
				(text-file :txt) ;;; stream|path|:insert|:txt|nil
				(class-file :insert)
				(post-file  :insert)
				connect-class built-in-arrays defined-arrays
				send-atype send-asize sequence response redef
				object-access xml-syntax generate-comments

				&aux
				input-args
				w open dstream tstream dfile tfile topen
				cstream copen cfile pstream popen pfile
				head-forms tail-forms class-forms wrap-forms post-forms
				sdef bname port-name pdef opdefs bdef
				binding btype url
				(*xmp-warning-leader* "WSDL"))
  (setf input-args (list service port))
  (or service (setf service 0))
  (or port (setf port 0))
  (and (numberp service)
       (setf w (nth service (wsdl-service-names conn)))
       (setf service w))
  (cond ((eq service :none) (push nil input-args))
	((setf sdef (schema-collected-component
		     conn #'wsdl-services #'schema-component-name service t))
	 (multiple-value-setq (bname url)
	   (wsdl-service-binding-names conn sdef port t))
	 (setf bdef (or
		     ;; xmethods.com QueryInterfaceService uses the wrong namespace
		     (schema-lookup-component
		      conn #'wsdl-bindings #'schema-component-name bname)
		     (error "Cannot find binding ~A" bname)))
	 (setf binding bdef)
	 (setf btype   (schema-decoded-attribute bdef "type"))
	 (setf port-name btype)
	 (setf pdef (or
		     ;; Some WSDLs seem to have sloppy namespace tagging 
		     ;;  ie "dConverterSOAP" in XMethods
		     (schema-lookup-component
		      conn #'wsdl-port-types #'schema-component-name port-name)
		     (error "Cannot find portType ~A" port-name)))
	 (push (list service (schema-component-name sdef) bname port-name 
		     (schema-component-name pdef))
	       input-args)
	 (setf opdefs (schema-collected-parts pdef :port-type :operation))
	 )
	(t (error "Cannot find service ~S" service)))
  (wsdl-report-imports conn :verbose verbose)
  (setf (wsdl-warnings conn) 0)
  
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
       (dstream ()
		(or dstream
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
       
       (trim-defs (conn &aux (forms (reverse (wsdl-sub-list conn))))
		  (setf (wsdl-sub-list conn) nil)
		  forms)
       )

    (unwind-protect
	(let (soap-binding x)

	  ;; Determine destination streams early because 
	  ;;  some outputs depend on file names.
	  (dstream)
	  (tstream)
	  (when object-class (cstream))
	  (pstream)

	  (cond ((null file-package) (setf file-package *package*))
		((packagep file-package))
		((setf x (find-package file-package))
		 (setf file-package x))
		(t (error "Cannot find package ~S" file-package)))

	  (setf (xmp-xml-syntax conn) xml-syntax
		(wsdl-undef-ns conn) nil
		(wsdl-props conn) nil
		(wsdl-maybe-conflicts conn) :unknown
		(wsdl-all-names conn) (make-hash-table :test #'equalp)
		(wsdl-soap-address conn) url
		(wsdl-operations conn) opdefs
		(wsdl-expand-singleton conn) expand-singleton
		(wsdl-wdef-list conn) nil
		(wsdl-sub-list conn) nil
		(wsdl-def-index conn) nil
		(wsdl-compose-strategy conn) compose
		(wsdl-client-options conn)
		`( ,@(when lisp-package (list :lisp-package lisp-package))
		     ,@(when null-element (list :null-element null-element))
		     ,@(when eep (list :empty-element empty-element)) 
		     ,@(when body-form (list :body-form body-form))
		     ,@(when connect-class (list :class (qt connect-class)))
		     )
		(wsdl-options conn) (list :built-in-arrays built-in-arrays
					  :defined-arrays defined-arrays 
					  :message-method-prefix message-method-prefix
					  :send-atype send-atype :send-asize send-asize
					  :prefix prefix :sequence sequence
					  :response response :redef redef
					  :cfile (or cfile
						     (when cstream
						       (namestring cstream)))
					  :object-class object-class
					  :object-access object-access
					  :exported-symbols (list nil)
					  :generate-comments generate-comments
					  )
		;; MUST call wsdl-compose-name after options are pre-set
		(wsdl-map-name conn) (or map (wsdl-compose-name
					      conn :keyword prefix "namespaces"))
		)
	  (wsdl-index-form
	   conn `(in-package ,(make-symbol (package-name file-package))))
	  (setf (wsdl-file-package conn) file-package)
	  (setf (wsdl-url-name conn)
		(wsdl-compose-name conn nil "*" prefix :service-url*))
	  (wsdl-index-form
	   conn `(defpackage ,(make-symbol (package-name file-package))
		   (:use ,@(mapcar #'(lambda (p)
				       (make-symbol (package-name p)))
				   (package-use-list file-package)))
		   ,@(when object-class 
		       (let ((opk (symbol-package object-class)))
			 (when (not (eq opk file-package))
			   (import object-class file-package)
			   `((:import-from ,(intern (package-name opk) :keyword)
					   ,(make-symbol (string object-class)))))))
		   ))
	  
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
	      `(defun ,(wsdl-compose-name conn nil prefix :make-server)
		 (&optional (,(wsdl-file-local-name conn "port")
			     (or (ignore-errors
				   (net.uri:uri-port   ;;; rfe6782 
				    (net.uri:parse-uri
				     ,(wsdl-url-name conn))))
				 8080)))
		 (let ((,(wsdl-file-local-name conn "s")
			,(apply #'wsdl-generate-code
				conn mode nil
				'soap-message-server
				:start (list 'list :port
					     (wsdl-file-local-name conn "port"))
				:enable :start
				:publish `(list :path
						(net.uri:uri-path  ;;; rfe6782 
						 (net.uri:parse-uri
						  ,(wsdl-url-name conn))))
				
				:lisp-package :keyword
				(append 
				 (when action (list :action action))
				 (list :message-dns
				       (qt (or message-dns (wsdl-map-name conn))))
				 (when body-form (list :body-form body-form)))
				)))
		   ,@(mapcar #'(lambda (ex)
				 (list* 'soap-export-method
					(wsdl-file-local-name conn "s")
					ex))
			     (wsdl-server-exports conn))
		   ,(wsdl-file-local-name conn "s")))
	      :mode mode :info :top-level)))
						   
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
	    (setq pl (wsdl-walk-form conn (wsdl-def-list conn) pl))
	    (setf post-forms (trim-defs conn))
	    
	    (dolist (nsd (nse-defs (or message-dns
				       (xmp-message-dns conn))
				   :one :package))
	      (or (eq (find-package :keyword)
		      (nsd-package nsd))
		  (when (member (nsd-package nsd) pl)
		    (push (qt (list (setf pn (make-symbol
					      (package-name (nsd-package nsd))))
				    (nsd-prefix nsd)
				    (nsd-uri nsd)
				    ))
			  px)
		    (wsdl-index-form
		     conn `(defpackage ,pn (:use))))))
	    (dolist (exp (cdr (wsdl-option conn :exported-symbols)))
	      (setf exp (car exp))
	      (wsdl-index-form
	       conn `(defpackage ,exp
		       (:use ,@(mapcar #'(lambda (p)
					   (make-symbol (package-name p)))
				       (package-use-list (find-package exp)))))))
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
	    (wsdl-walk-form conn (wsdl-def-list conn) nil)
	    (flet ((do-file
		    (item &optional (stream nil s-p)
			  &aux (form (first item)) (comments (cdr item)))
		    (or s-p (setf stream (dstream)))
		    (when stream
		      (wsdl-print-env 
		       conn
		       #'(lambda (&aux
				  (defpk (and (consp form)
					      (eq 'defpackage (first form))
					      (second form)))
				  (exp (when defpk (assoc :export (cddr form))))
				  (elist (wsdl-option conn :exported-symbols))
				  (exports (when defpk
					     (assoc defpk (cdr elist))))
				  )
			   (when exports
			     (dolist (esym (cdr exports))
			       (or exp
				   (push (setf exp (list :export)) (cddr form)))
			       (pushnew esym (cdr exp)))
			     (setf (cdr exp)
				   (sort
				    (mapcar #'(lambda (x)
						(make-symbol (string x)))
					    (cdr exp))
				    #'string-lessp)))
			   (when comments
			     ;; Print comments after all package munging is done
			     ;;  so that symbols in comments will show
			     ;;  like in form.
			     (format stream "~&~%~%")
			     (dolist (c comments)
			       (when (typecase c 
				       (symbol nil)
				       (otherwise t))
				 (wsdl-print-lines stream ";; " c))))
			   (format stream "~&~%~S~%" form)))))
		   (print-source
		    (stream)
		    ;; print source info in outfile [rfe6554]
		    (wsdl-print-lines
		     stream ";;; " 1
		     (wsdl-print-env
		      conn
		      #'(lambda ()
			  (format nil "WSDL source: ~{~S ~A~}"
				 (if (consp (schema-source conn))
				     (list (first (schema-source conn))
					   (second (schema-source conn)))
				   (list (schema-source conn) "")))))

		     (format nil "    Service: ~A" (second (first input-args)))
		     (format nil "    Binding: ~A" (third (first input-args)))
		     (format nil "   portType: ~A" (fifth (first input-args)))
		     1))
		   )

	      (dolist (c (wsdl-maybe-conflicts conn))
		(let ((k (gethash c (wsdl-all-names conn))))
		  (warn "Possible name conflicts: ~S" (cdr k))))
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

		
		(print-source (dstream))
		
		(dolist (c (wsdl-maybe-conflicts conn))
		  (let ((k (gethash c (wsdl-all-names conn))))
		    (multiple-value-call
		     #'wsdl-print-lines (dstream) ";;; " 
		     "Possible name conflicts:" (cdr k))))

		(when (wsdl-overloaded-ops conn)
		  (multiple-value-call
		   #'wsdl-print-lines (dstream) ";;; " 1
		   "The following types represent overloaded message names:"
		   (values-list (mapcar #'(lambda (n)
					    (wsdl-print-env 
					     conn
					     #'(lambda () (format nil "   ~S" n))))
					(wsdl-overloaded-ops conn)))
		   1))		 

		(when (tstream)
		  (when (not (eq (dstream) (tstream))) (print-source (tstream)))
		  (when (eq (dstream) (tstream)) (format (tstream) "~&~%#|~%"))
		  (wsdl-describe conn (cdr pdefs) (tstream) 3)
		  (when (eq (dstream) (tstream)) (format (tstream) "~&|#~2%")))
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
		))
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


(defmethod wsdl-query-redef ((conn wsdl-file-connector) (name t) (type t)) t)

(defmethod wsdl-query-redef ((conn wsdl-file-connector) (name symbol) type)
  ;; Return t if we should proceed with code generation.
  ;; Return nil to skip code generation for this symbol.
  (let* ((sources (ignore-errors (source-file name (if (consp type) type (list type)))))
	 (source (if (consp type)
		     (dolist (x type) (when (setf x (assoc x sources)) (return (cdr x))))
		   (cdr (assoc type sources))))
	 (redef (wsdl-option conn :redef))
	 (cfile (wsdl-option conn :cfile)))
    (when source (setf source (ignore-errors (namestring (truename source)))))
    (when cfile (if (probe-file cfile)
		    (setf cfile (ignore-errors (namestring (truename cfile))))
		  (namestring cfile)))
    (if (and source cfile)
	(case redef
	  (:warn  
	   (or (equal source cfile) (warn "Type ~S is also defined in ~A" name source))
	   t)
	  (:skip
	   (if (equal source cfile)
	       t
	     nil))
	  (:skip-and-warn
	   (or (equal source cfile) (warn "Type ~S is defined in ~A" name source))
	   (if (equal source cfile)
	       t
	     nil))
	  )
      t)))

(defmethod wsdl-emit-object-classes ((conn wsdl-file-connector) (class null))
  nil)

(defmethod wsdl-emit-object-classes ((conn wsdl-file-connector) (class symbol))
  (wsdl-emit-object-classes conn (make-instance class)))


(defmethod wsdl-type-of-type-def ((conn wsdl-file-connector) form)
  (with-tree-match
   (form (define-soap-type :?any '(:? type-name) '(:? type) . :?any))
   (values type-name type)))

(defmethod wsdl-slots-of-type-spec ((conn wsdl-file-connector) spec)
  (with-tree-match
   (spec (:complex ((:? collector (:?or :seq :seq* :set :set* :seq+ :set+))
		    (:?each slots (:element . :?any))) . :?any))
   (values t slots collector)))

(defmethod wsdl-atype-of-type-spec ((conn wsdl-file-connector) spec)
  (with-tree-match
   (spec (:array (:? atype) . :?any))
   (values t atype)))

(defmethod wsdl-emit-object-classes ((conn wsdl-file-connector)
				     (instance soap-object-class)
				     &aux
				     (class (type-of instance))
				     (reader-prefix (slot-value instance 'reader-prefix))
				     (writer-prefix (slot-value instance 'writer-prefix))
				     encoder defclass-names
				     (client-options (wsdl-client-options conn))
				     (cclass (getf client-options :class))
				     (defs (reverse (wsdl-def-list conn)))
				     iname new-name)
  (dolist (def defs)
    ;; First, collect all defined structs.
    (multiple-value-bind (type-name type)
	(wsdl-type-of-type-def conn (first def))
      (multiple-value-bind (r slots)
	  (wsdl-slots-of-type-spec conn type)
	(when (and type-name type r slots)
	  (when (wsdl-query-redef conn type-name :type)
	    (pushnew type-name defclass-names))))))
  (dolist (def defs)
    ;; Then, collect all defined arrays.
    (multiple-value-bind (type-name type)
	(wsdl-type-of-type-def conn (first def))
      (when (and type-name type) 
	(multiple-value-bind (r atype) (wsdl-atype-of-type-spec conn type)
	  (when (and r atype
		     (or (member atype defclass-names)
			 (soap-known-type-p conn atype :dns))
		     (wsdl-query-redef conn type-name :type))
	    (pushnew type-name defclass-names))))))
  (setf (wsdl-object-classes conn) defclass-names)
  ;;(format t "~&; defclass-names = ~S~%" defclass-names)
  (dolist (def defs)

    (multiple-value-bind (type-name type)
	(wsdl-type-of-type-def conn (first def))

      (when (and type-name type (wsdl-query-redef conn type-name :type)
		 ;; Generate code only for the types collected in the 
		 ;;  previous loops over the defs.
		 (member type-name defclass-names))
	(when (null encoder)
	  (setf encoder (wsdl-encoder conn :prototype type-name))
	  (setf (wsdl-encoder-name conn) encoder))
	(let* ((class-name type-name)
	       (decode-name (wsdl-decoder conn type-name))
	       elt elts readers writers keys names etypes)
	  (setf new-name (wsdl-constructor conn type-name))
	  (or
	   (multiple-value-bind (r slots collector)
	       (wsdl-slots-of-type-spec conn type)
	     (when (and r slots)

	       ;; Emit defclass, functions new-xxx decode-xxx, method pppencode.

	       (dolist (slot slots)
		 (push (setf elt (first (second slot))) elts)
		 (push (third slot) etypes)
		 (push (wsdl-compose-parts conn :keyword elt) keys)
		 (push (wsdl-compose-parts conn nil elt) names)
		 (push (wsdl-accessor conn elt reader-prefix :prototype type-name)
		       readers)
		 (push (wsdl-accessor conn elt writer-prefix :prototype type-name)
		       writers)
		 )
	       (wsdl-index-form
		conn
		`(defclass 
		   ,type-name
		   (,class)
		   ( ,@(mapcar #'(lambda (name key reader writer)
				   (list name
					 :reader reader
					 :writer writer
					 :initarg key
					 :initform nil))
			       names keys readers writers)
		       ))
		:mode :object-class :info :top-level)
	       (wsdl-index-form
		conn
		`(defun ,new-name (&key ,@names)
		   ,@(mapcan #'(lambda (name etype &aux ltype)
				 (when (setf ltype (wsdl-type-to-lisp-type conn etype))
				   (list (list 'check-type name
					       ;; 2006-10-24 mm rev: [bug16488] 
					       ;; make sure type spec is not quoted
					       (unquote ltype)
					       ))))
			     names etypes)
		   (make-instance ,(qt class-name)
				  ,@(mapcan #'(lambda (key name) (list key name))
					    keys names)))
		:mode :object-new   :info :top-level)
	       (wsdl-index-form
		conn
		(let ((cvar (wsdl-file-local-name conn "conn"))
		      (fvar (wsdl-file-local-name conn "soap-result")))
		  `(defmethod ,decode-name
		     ,(if cclass
			  (list (list cvar cclass) (list fvar t))
			(list (list cvar t) (list fvar t)))
		     (make-instance
		      ,(qt class-name)
		      ,@(mapcan
			 #'(lambda (key elt etype)
			     (list key (wsdl-type-to-decoder
					conn etype cvar
					(list 'soap-result-typed
					      cvar fvar
					      (qt etype)
					      (case (wsdl-option conn :sequence)
						(:set* nil)
						(otherwise
						 (case collector
						   ((:set+ :seq+) :error)
						   (otherwise nil))))
					      (string elt)))))
			 keys elts etypes))))
		:mode :object-decoder :info :top-level)
	       (wsdl-index-form
		conn
		(let ((fvar (wsdl-file-local-name conn "instance")))
		  `(defmethod ,encoder ((,fvar ,type-name))
		     (list ,@(mapcan #'(lambda (elt acc etype)
					 (list (string elt)
					       (wsdl-type-to-encoder
						conn etype (list acc fvar))))
				     elts readers etypes))))
		:mode :object-encoder :info :top-level)
	       t)) ;;; end match :complex

	   (multiple-value-bind (r atype)
	       (wsdl-atype-of-type-spec conn type)
	     (when (and r atype)
	   
	       ;;(format t "~&; match array ~A   elt type ~A~%" type-name atype)

	       (let (array-of-built-in-type)
		 (when (or (member atype defclass-names)
			   (setf array-of-built-in-type
				 (soap-known-type-p conn atype :dns)))
		   ;;(format t "~&; find array elt type ~A~%" atype)
		   ;; Emit defclass, function new-xxx, decode-xxx, method pppencode.

		   (setf iname (wsdl-array-items conn :prototype type-name))
		   (wsdl-index-form
		    conn
		    `(defclass ,type-name (,class soap-array-mixin)
		       ((items :accessor ,iname)))
		    :mode :object-type :info :top-level)

		   (setf new-name (wsdl-constructor conn type-name))
		   (wsdl-index-form
		    conn
		    (let ((ivar (wsdl-file-local-name conn "items")))
		      `(defun ,new-name (&rest ,ivar)
			 (when (and ,ivar (null (cdr ,ivar))
				    (typep (first ,ivar) 'sequence))
			   (setf ,ivar (concatenate 'list (first ,ivar))))
			 (make-instance ,(qt class-name) :items ,ivar)))
		    :mode :object-new :info :top-level)
		   (wsdl-index-form
		    conn
		    (let ((cvar (wsdl-file-local-name conn "conn"))
			  (aname (when (null array-of-built-in-type)
				   (wsdl-decoder conn atype)))
			  (fvar (wsdl-file-local-name conn "soap-result"))
			  (ivar (wsdl-file-local-name conn "items"))
			  (tvar (wsdl-file-local-name conn "tail"))
			  )
	     
		      `(defmethod ,decode-name
			 ,(if cclass
			      (list (list cvar cclass) (list fvar t))
			    (list (list cvar t) (list fvar t)))
			 (make-instance ,(qt class-name)
					:items
					(let ((,ivar (concatenate 'list ,fvar nil)))
					  ,(if array-of-built-in-type
					       ivar
					     `(do ((,tvar ,ivar (cdr ,tvar)))
						  ((null ,tvar) ,ivar)
						(setf (car ,tvar)
						      (,aname ,cvar (car ,tvar))))))
					)))
		    :mode :object-decoder :info :top-level)
		   (wsdl-index-form
		    conn
		    (let ((fvar (wsdl-file-local-name conn "instance"))
			  (ivar (wsdl-file-local-name conn "item")))
		      `(defmethod ,encoder ((,fvar ,type-name))
			 ,(if array-of-built-in-type
			      `(,iname ,fvar)
			    `(mapcar #'(lambda (,ivar) (,encoder ,ivar)) 
				     (,iname ,fvar)))
			 ))
		    :mode :object-encoder :info :top-level)
		   t)))) ;;; end match :array
	   )
	  ))))
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
	(def ((:? form
		  (:?or
		   (define-soap-element :?any 
		     (:?or (quote (:? name)) (:? name))
		     . :?any)
		   (define-soap-type :?any :?any 
		     (quote (:complex (:seq (:element (:? tname) . :?any))
				      . :?any))
		     . :?any))
		  )
	      . :?any))
	;; save element defs for use in defun
	;; Assume the sequence: message-def reply-def defun
	(or (cond (tname
		   (cond ((null message) (setf message form reply nil) t)))
		  ((symbolp name)
		   (cond ((null message) (setf message form reply nil) t)
			 (reply nil)
			 ((eq :symbol (wsdl-option conn :response))
			  (setf reply form))))
		  ((stringp name)
		   (cond ((eq :symbol (wsdl-option conn :response)) nil)
			 (message (setf reply form)))))
	    (setf message nil reply nil))
	t)

       (with-tree-match
	(def ((defun (:? defun-name) . :?any) . (:? comments)))

	(when (and (setf msg (first (last comments)))
		   (symbolp msg) message reply)
	  (with-tree-match
	   (message (:?or
		     (define-soap-element :?any :?any 
		       '(:complex
			 (:seq (:?each parts (:element . :?any)))
			 . :?any)
		       . :?any )
		     (define-soap-type :?any '(:? mtype)
		       '(:complex 
			 (:seq 
			  (:element :?any 
				    (:complex
				     (:seq (:?each parts (:element . :?any)))
				     . :?any)
				    . :?any))
			 . :?any)
		       . :?any)
		     ))
	   (with-tree-match
	    (reply  (define-soap-element :?any 
		      (:? out-name) '(:? out-type) . :?any ))
	    
	    (wsdl-emit-wrapper conn msg defun-name parts out-name out-type mtype)
	    
	    ))
	  (setf message nil reply nil)))
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
  
(defmethod wsdl-type-to-lisp-type ((conn wsdl-file-connector) type &aux ltype)
  ;; Return a Lisp type suitable for check-type, or nil.
  (cond ((member type (wsdl-object-classes conn)) type)
	((and (case (wsdl-option conn :redef) ((:skip :skip-and-warn) t))
	      (find-class type nil))
	 type)
	((and (symbolp type) (setf ltype (get type :soap-check-type)))
	 (if (eq ltype t)
	     ;; Skip type check for string args since anything is encoded
	     ;;  to string by default.  Skip for boolean as well.
	     nil
	   ltype))
	))


(defmethod wsdl-type-to-specializer ((conn wsdl-file-connector) type)
  (cond ((member type (wsdl-object-classes conn)) type)
	((and (case (wsdl-option conn :redef) ((:skip :skip-and-warn) t))
	      (find-class type nil))
	 type)
	((eq type (xsd "string")) 'string)
	(t 't)))

(defmethod wsdl-type-to-encoder ((conn wsdl-file-connector) type var)
  (cond ((member type (wsdl-object-classes conn)) (list (wsdl-encoder-name conn) var))
	((and (case (wsdl-option conn :redef) ((:skip :skip-and-warn) t))
	      (find-class type nil))
	 (list (wsdl-encoder-name conn) var))
	((eq type (xsd "string")) var)
	(t var)))

(defmethod wsdl-type-to-decoder ((conn wsdl-file-connector) type cvar res)
  (cond ((member type (wsdl-object-classes conn))
	 (list (wsdl-decoder conn type) cvar res))
	((and (case (wsdl-option conn :redef) ((:skip :skip-and-warn) t))
	      (find-class type nil))
	 (list (wsdl-decoder conn type) cvar res))
	((eq type (xsd "string")) res)
	(t res)))

(defmethod wsdl-emit-wrapper ((conn wsdl-file-connector)
			      msg defun-name parts out-name out-type mtype
			      &aux
			      call res
			      (bvar (wsdl-file-local-name conn "body"))
			      (cvar (wsdl-file-local-name conn "conn"))
			      (hvar (wsdl-file-local-name conn "headers"))
			      (mprefix (wsdl-option conn :message-method-prefix))
			      comments check-types)
  (declare (ignore out-name))

  ;; emit wrapper methods for wsdl-generated defun-s
  ;; methods take positional args specialized on decoded object classes

  (setf call 
	`(,defun-name
	   ,@(mapcan
	      #'(lambda (elt &aux (ename (first (second elt))))
		  (list (wsdl-compose-parts conn :keyword ename)
			(wsdl-type-to-encoder
			 conn (third elt) (wsdl-compose-name conn nil ename)))) 
	      parts)))
  (setf comments
	(mapcar #'(lambda (elt &aux (ename (first (second elt))) 
			       (etype (third elt)))
		    (format nil "Argument ~A is Schema type ~S"
			  ename
			  (cond ((symbolp etype) etype)
				((and (consp etype) (eq :simple (car etype))
				      (second etype) (symbolp (second etype)))
				 (second etype))
				(t "anonymous complex"))))
		parts))	    
  (or
   (with-tree-match
    (out-type  (:complex
		(:seq
		 (:? out-elt (:element . :?any)))
		. :?any))
    (setf res (wsdl-type-to-decoder
	       conn (third out-elt) cvar
	       (list 'soap-result-only
		     cvar bvar :error
		     nil ;;; always ignore the element name at the top-level
		     ;; ignore the namespace at the next level
		     (qt (string (first (second out-elt))))
		     ))))
   (setf res bvar))
		    
  (wsdl-index-form
   conn
   `(defmethod ,(wsdl-compose-name conn nil mprefix (string  (or mtype msg)))
      (,@(mapcar #'(lambda (elt &aux ltype
				(var (wsdl-compose-parts conn nil (first (second elt))))
				(spec (wsdl-type-to-specializer conn (third elt))))
		     (and (eq spec t)
			  (setf ltype (wsdl-type-to-lisp-type conn (third elt)))
			  (push (list 'check-type var
				      ;; 2006-10-24 mm rev: [bug16488] 
				      ;; make sure type spec is not quoted
				      (unquote ltype)
				      )
				check-types))
		     (list var spec))
		 parts))
      ,@check-types
      (multiple-value-bind (,bvar ,hvar ,cvar) 
	  ,call
	(values ,res ,hvar ,cvar))
      )
   :mode :object-wrapper :info :top-level :comment comments)
  )



(defmethod describe-soap-method ((conn wsdl-file-connector) elt
				 &key wsdl (stream t) depth)
  (let* ((type (or (when wsdl (wsdl-lookup wsdl :element elt :complex))
		   (soap-find-element wsdl elt :dns)))
	 (parts (when (and (consp type) (eq :complex (car type)))
		  (cdr (second type))))
	 (ret (wsdl-response-name conn elt))
	 (rtype (or (when wsdl (wsdl-lookup wsdl :element ret :complex))
		    (soap-find-element wsdl ret :dns)))
	 (rparts (when (and (consp rtype) (eq :complex (car rtype)))
		  (cdr (second rtype))))
	 (indent 6) l)
    (wsdl-print-env
     conn
     #'(lambda ()
	 (format stream "~&~VAMessage ~S takes ~A argument~A~%"
		 indent "" elt (setf l (length parts))
		 (case l (0 "s") (1 ":") (otherwise "s:")))
	 (dolist (part parts)
	   (describe-soap-element part :wsdl wsdl :stream stream :depth depth))
	 (format stream "~&~VAand returns a result ~A containing the element(s):~%"
		 indent "" ret)
	 (dolist (part rparts)
	   (describe-soap-element part :wsdl wsdl :stream stream :depth depth))))
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
    (cond (stream
	   (format stream "~&~VAThe element ~S of type "
			indent "" name)
	   (describe-soap-type
	    type :wsdl wsdl :stream stream :depth depth :indent indent)
	   (format stream "~&"))
	  (t (format stream "~&~VAThe element ~S of type ~A~%"
			  indent "" name 
			  (describe-soap-type
			   type :wsdl wsdl :stream nil :depth nil))))
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
	  (wsdl-all-names conn) (make-hash-table :test #'equalp)
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
	       (symbol
		(or (find-package target)
		    (error
		     ":target argument must specify a package defined as a namespace.")))
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
		 ;; then target-uri is nil
		 (cond ((setf tns (xmp-search-map message-dns :package target))
			(setf target-uri (xnd-uri tns)
			      target-prefix (xnd-prefix tns)
			      target-pk (xnd-package tns)))
		       (t (error "Cannot find targetNamespace from package ~A" 
				 target))))
		;; otherwise, target-uri is set
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
	  (cond (target-pk
		 (pushnew (list target-pk target-prefix target-uri)
			  used-namespaces :test #'equal))
		(t (error "targetNamespace must be represemted by a Lisp package.")))


	  (or service-name
	      (setf service-name (or (soap-service-name server)
				     (wsdl-make-name conn "Service"))))
	  (push `((,(wsdl "port") "name" ,port-name "binding"
			,(wsdl-make-tname conn binding-name))
		  ((,(wsoap "address") "location" 
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
	    (maphash
	     #'(lambda (op-elt v)
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
			  (body `((,(wsoap "body")
				   "use" "encoded"
				   ,@tns
				   "encodingStyle" 
				   ,(soap-encoding-style server)
				   )))
			  )
		     (and (null target-package)
			  (not (symbolp op-elt))
			  (error
			   "Message element ~S must be in package of targetNamespace"
			   op-elt))
		     (when (or 
			    ;; default behavior
			    (null target-package)
			    ;; skip elements with no package
			    (and (symbolp op-elt)
				 ;; skip elements not in target-package
				 (eq target-package (symbol-package op-elt))))
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
		       (push `((,(wsdl "operation") "name" ,op-name)
			       ((,(wsoap "operation") "soapAction" ,(or action "")))
				      
			       (,(wsdl "input") ,body)
			       (,(wsdl "output") ,body)
			       )
			     bind-ops)
		       (push `((,(wsdl "operation") "name" ,op-name)
			       ((,(wsdl "input") "message" ,inmsgq))
			       ((,(wsdl "output") "message" ,outmsgq)))
			     port-ops)
		       (push `((,(wsdl "message") "name" ,inmsg)
			       ,@(wsdl-message-parts conn op-elt))
			     messages)
		       (push `((,(wsdl "message") "name" ,outmsg)
			       ,@(wsdl-message-parts conn res))
			     messages)

		       ))))
	     (aref tables i)))
	  (push `((,(wsdl "portType") "name" ,port-name) ,@(reverse port-ops))
		ports)
	  (push `((,(wsdl "binding")
			"name" ,binding-name
			"type" ,(wsdl-make-tname conn port-name))
		  ((,(wsoap "binding")
			  "style" "rpc"
			  "transport" ,transport
			  ))
		  ,@(reverse bind-ops))
		bindings)
	  (push `((,(wsdl "service") "name" ,service-name)
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
	(push `((,(xsd "element") "name" ,(string (first def)) "type" ,(second def)
		     ,@(let (props)
			 (do ((tl (third def) (cddr tl)))
			     ((atom tl) (reverse props))
			   (case (first tl)
			     (:nillable (when (second tl)
					  (push (xsd "nillable") props)
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
      conn (wsdl "definitions") 
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
		   `(,(wsdl "types")
			  ((,(xsd "schema") "targetNamespace" ,target-uri)
			   ,@type-defs))
		   nil))
     (dolist (m (reverse messages))
       ;; (wsdl:message message-def)*
       (xmp-encode conn m nil))
     
     (dolist (p (reverse ports))
       ;; (wsdl:portType port-def)*
       (xmp-encode conn p nil))

     (dolist (b (reverse bindings))
       ;; (wsdl:binding bdef)*
       (xmp-encode conn b nil))

     (dolist (s (reverse services))
       ;; (wsdl:service sdef)*
       (xmp-encode conn s nil))

     (format s "~&")
     (xmp-encode-end conn (wsdl "definitions"))
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
			    `((,(wsdl "part")
				    "name" ,(string elt)
				    "type" ,tname))
			    else
			    (wsdl-add-element conn elt type props)
			    `((,(wsdl "part")
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
		  (wsdl-make-symbol conn (format nil "~A" name) nil target-package)
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
     `((,(xsd "element")
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
     `((,(xsd "element")
	   "type" ,(xsd "anyType")
	   ,@attributes)))
    (:complex
     (or (case (first (second tdef))
	   ;;  :seq  ->  sequence min=0 max=1
	   (:seq (setf schema (xsd "sequence") min 0))
	   ;;  :seq1 ->  sequence min=1 max=1
	   (:seq1 (setf schema (xsd "sequence")))
	   ;;  :seq+ ->  sequence min=1
	   (:seq+ (setf schema (xsd "sequence") max "unbounded"))
	   ;;  :seq* ->  sequence min=0
	   (:seq* (setf schema (xsd "sequence") min 0 max "unbounded"))
	   ;;  :set  ->  all   min=0 max=1
	   (:set  (setf schema (xsd "all") min 0))
	   ;;  :set1 ->  all   min=1 max=1
	   (:set1 (setf schema (xsd "all")))
	   ;;  :set+ ->  NA
	   ;;  :set* ->  NA
	   ;;  :or ->  choice
	   (:or (setf schema (xsd "choice")))
	   )
	 (error "Unrecognized type def ~S" tdef))
     `(,(if name
	    `(,(xsd "complexType") "name" ,(string name))
	  (xsd "complexType"))
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
	    `(,(xsd "complexType") "name" ,(string name))
	  (xsd "complexType"))
       (,(xsd "complexContent")
	   ((,(xsd "restriction") "base" ,(enc "Array"))
	    ((,(xsd "attribute")
		 "ref" ,(enc "arrayType")
		 ,(wsdl "arrayType")
		 ,(soap-encoded-array-type
		   conn (wsdl-add-type conn (second tdef)) :dns)))))))
    (:simple
     `((,(xsd "simpleType") ,@(when name `("name" ,(string name))) 
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
	 (if (or (eq elt (wsdl "definitions"))
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

(defun decode-wsdl-namespaces (&key string file uri (map :wsdl1-prefix) show)
  (multiple-value-bind (ns other ambi missing)
      (xmp-decode-namespaces :pns (xmp-extract-namespaces
				   :string string :file file :uri uri)
			     :needed *wsdl-needed-namespaces*
			     :optional *wsdl-optional-namespaces*
			     :known map)		
    (when show
      (let ((nsx (xmp-export-namespace-map ns)))
	(format show "~&#|")
	(format show "~&Namespace map:~%  (~S ~{~%    ~S~})~%"
		(first nsx) (cdr nsx))
	(if (or other ambi missing)
	    (let ()
	      (when other
		(format show "~&Other namespaces:~{~%    ~S~})~%"
			other))
	      (when ambi
		(format show "~&Ambiguous namespaces:~{~%    ~S~})~%"
			ambi))
	      (when missing
		(format show "~&Missing namespaces:~{~%    ~S~})~%"
			missing)))
	  (format show "~&No other namespaces~%"))
	(format show "~&|#")
	))
    (values ns other ambi missing)))


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
