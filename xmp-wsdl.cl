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

;; $Id: xmp-wsdl.cl,v 2.1 2004/01/16 19:37:23 layer Exp $

;; WSDL support

(in-package :net.xmp.soap)

(defpackage :net.xmp.soap 
  (:export 
   #:*wsdl-default-namespaces*
   #:*wsdl-1.1-namespaces*
   #:*wsdl-1.2-namespaces*
   #:decode-wsdl-file
   #:wsdl-service-names
   #:make-client-interface
   #:make-server-interface
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
     ;; #:||

     ))

  (defpackage :net.xmp.wsdl.soap
    (:use)
    (:export
     #:|binding|
     #:|operation|
     #:|body|
     #:|address|
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
     "http://www.w3.org/ 2003/06/wsdl"
     ;; A normative XML Schema [XML Schema: Structures], 
     ;;   [XML Schema: Datatypes] document for the
     ;;   "http://www.w3.org/2003/06/wsdl" namespace can be 
     ;;   found at http:// www.w3.org/2003/06/wsdl.
     ;;   WSDL documents that do NOT conform to this schema are
     ;;   not valid WSDL documents. WSDL documents that DO conform to
     ;;   this schema and also conform to the other constraints defined
     ;;   in this specification are valid WSDL documents.
     )
    (:net.xmp.wsdl.soap
     "soap12"
     "http://www.w3.org/ 2003/06/wsdl/ soap12"
     ;; Defined by WSDL 1.2: Bindings [WSDL 1.2 Bindings].
     )
    ;; http "http://www.w3.org/ 2003/06/wsdl/http"
    ;; mime "http://www.w3.org/ 2003/06/wsdl/ mime"
    (:net.xmp.schema
     "xs"
     "http://www.w3.org/ 2001/XMLSchema"
     ;; Defined in the W3C XML Schema specification
     ;;   [XML Schema: Structures], [XML Schema: Datatypes].
     )
    (:net.xmp.schema-instance
     "xsi"
     "http://www.w3.org/ 2001/XMLSchema- instance"
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
 
   ))


(defun decode-wsdl-file (file &key namespaces lisp-package)
  (let* ((conn (make-instance 'wsdl-file-connector
			      :message-dns (xmp-merge-nses
					    namespaces *wsdl-default-namespaces*)
			      :source file :lisp-package lisp-package)))

    (values-list
     (append
      (list conn)
      (multiple-value-list
       (xmp-decode-message
	conn (xmp-parse-message conn file)))))
    ))

(defmethod schema-decode-attribute ((conn wsdl-file-connector)
				    name value nss &aux attr)
  (setf attr (xmp-decode-qualified-name conn name nss))
  (case attr
    ((wsdl:|message| wsdl:|arrayType| wsdl:|binding|)
     (values attr (xmp-decode-qualified-name conn value nss)))
    (otherwise (call-next-method conn attr value nss))))


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

(schema-named-part wsdl-file-connector wsdl:|operation|  :operation)
(schema-named-part wsdl-file-connector wsdl:|part|       :part       :not-qname)
(schema-named-part wsdl-file-connector wsdl:|port|       :port)


(defmethod wsdl-service-names ((conn wsdl-file-connector))
  (mapcar 'first (cdr (wsdl-services conn))))


(defmethod wsdl-define-type ((conn wsdl-file-connector) typedef do-form)
  (let* ((name (first typedef))
	 (props (cdr typedef))
	 (complex (getf props :complex-type))
	 (simple  (getf props :simple-type))
	 )
    (flet ((do-form (&rest x) (apply do-form x)))
      (cond (complex
	     (let* ((content (getf complex :content))
		    (complex-content (getf complex :complex-content))		  
		    )
	       (cond
		(content
		 (case (first content)
		   ((:set :seq :set1 :set+ :set* :seq1 :seq+ :seq*)
		    (do-form
		     `(define-soap-type
			nil
			',name
			'(:complex
			  (,(first content)
			   ,@(mapcar
			      #'(lambda (elt)
				  (case (first elt)
				    (:element
				     `(:element ,(second elt)
						,(getf (cddr elt) :type)))
				    (otherwise
				     (warn
				      "wsdl-define-type - unknown sub-form ~S"
				      elt))))
			      (cdr content)))))))
		   (otherwise (warn
			       "wsdl-define-type - unknown content ~S"
			       content))
		   ))
		(complex-content
		 (case (first complex-content)
		   (:restriction
		    (let* ((r (cdr complex-content))
			   (r-base (xmp-getf conn r "base"))
			   (r-cont (getf r :content))
			   (r-attr (assoc :attribute r-cont))
			   (r-suba (getf r-attr :attributes))
			   (r-atype (xmp-getf conn r-suba "arrayType")))
		      (cond
		       ((and (eq r-base 'enc:|Array|)
			     (eq (second r-attr) 'enc:|arrayType|)
			     r-atype)
			(do-form
			 `(define-soap-type
			    nil
			    ',name
			    '(:array ,(soap-parse-array-type conn r-atype :dns)))))
		       (t (warn
			   "wsdl-define-type - unknown restriction ~S"
			   complex-content)))))
		   (otherwise (warn
			       "wsdl-define-type - unknown complex-content ~S"
			       complex-content))
		   )
		 )
		(t (warn "wsdl-define-type - unknown complex type ~S"
			 complex))
		)))

	    (simple (warn "wsdl-define-type - unknown simple type ~S"
			  simple)

		    )
	    (t (warn "wsdl-define-type - unknown form ~S" typedef)))

      )))

(defmethod wsdl-bind-operation ((conn wsdl-file-connector) b mode do-form 
				eval prefix suffix)
  (flet ((do-form (&rest x) (apply do-form x)))
    (let* ((op-name (second b))
	   (body (cddr b))
	   (content  (getf body :content))
	   (soap-op  (assoc :soap-operation content))
	   (action   (getf (cdr soap-op) 'wsdl:|soapAction|))
	   (style    (or (getf (cdr soap-op) 'wsdl:|style|)
			 (wsdl-soap-style conn)))
	   (messages (cdr (wsdl-messages conn)))

	   (bind-in  (assoc :input content))
	   (bind-out (assoc :output content))
	   ;;   (in-or-out :content ((:soap-body
	   ;;                            wsdl:use encoded|literal
	   ;;                            wsdl:namespace     URIstring
	   ;;                            wsdl:encodingStyle URLstring
	   ;;                            wsdl:parts  nmtokens          ???
	   ;;                            )
	   ;;        soap:header  ???
	   ;;        soap:fault   ???

	   (opdef    (first (member op-name (wsdl-operations conn) :key #'second)))
	   (opcont   (getf opdef :content))
	   (op-in    (assoc :input opcont))
	   (in-msg-name   (getf (cdr op-in) 'wsdl:|message|))
	   (op-out   (assoc :output opcont))
	   (out-msg-name  (getf (cdr op-out) 'wsdl:|message|))
	   (in-msg (assoc in-msg-name messages))
	   (in-parts (third in-msg))
	   (out-msg (assoc out-msg-name messages))
	   (out-parts (third out-msg))
	   (cx (position in-msg messages))
	   comment)

      (or (string-equal style "rpc")
	  (error "SOAP style=~S is not implemented." style) ;;;???
	  )

      (flet ((do-element
	      (msg-name action binding
			&aux
			(parts (third (assoc msg-name messages)))
			(content (getf (cdr binding) :content))
			(soap-body (assoc :soap-body content))
			(estyle (getf (cdr soap-body) 'wsdl:|encodingStyle|))
			(use (getf (cdr soap-body) 'wsdl:|use|))
			(ns (getf (cdr soap-body) 'wsdl:|namespace|))
			(pk (when ns (net.xmp::xmp-uri-to-package
				      conn ns :dns)))
			)
	      (or (null use) (string-equal use "encoded")
		  (error "SOAP use=~S is not implemented." use) ;;;???
		  )
	      (when ns
		(or pk (pushnew ns (wsdl-undef-ns conn) :test #'string=)))
	      `(define-soap-element
		 nil
		 ',msg-name 
		 '(:complex
		   (:seq1    
		    ,@(mapcar #'(lambda (part)
				  `(:element ,(second part)
					     ,(getf (cddr part) :type)))
			      parts))
		   ,@(when ns `(:namespaces
				(nil (,(when pk (package-name pk))
				      "tns"
				      ,ns))))
		   ,@(when estyle `(:encoding ,estyle))
		   ,@(when action `(:action ,action))
		   )))
	     )
	(do-form (do-element in-msg-name action bind-in))
	(do-form (do-element out-msg-name nil bind-out))
	(do-form
	 (let* ((def-name (read-from-string (format nil "~A~A"
						    prefix
						    (case suffix
						      (:index (incf cx))
						      (:message in-msg-name)))))
		(one-part (and in-parts
			       (null (cdr in-parts))
			       (first in-parts)))
		(one-type (getf (cddr one-part) :type))
		(one-def  (and one-type 
			       (wsdl-expand-singleton conn)
			       eval
			       (setf one-type (soap-find-type nil one-type :dns))
			       (eq :complex (car one-type))
			       one-type))
		(user (find-package :user))
		(in-elts (mapcar #'second in-parts))
		(in-keys (mapcar #'(lambda (part) (intern part user))
				 in-elts))
		(key-args   (if one-def
				(mapcar #'(lambda (part)
					    (intern (xmp-pick-name nil part)
						    user))
					(cdr (second  one-def)))
			      in-keys)
			    )
		)
	   (ecase mode
	     (:client
	      (setf comment (format nil "Send client message ~A " in-msg-name))
	      `(defun ,def-name 
		 (&rest ,(do-form "args") &key ,@key-args)
		 (declare (ignore ,@key-args))
		 (let ((,(do-form "conn") (soap-message-client 
					   :url ,(wsdl-soap-address conn)
					   ,@(wsdl-client-options conn)
					   )))
		   (values
		    ,(if one-def
			 `(apply 'call-soap-method ,(do-form "conn")
				 ',in-msg-name 
				 ,(second one-part)
				 ,(do-form "args") nil)
		       `(apply 'call-soap-method
			       ,(do-form "conn") ',in-msg-name ,(do-form "args")))
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
						 (intern (xmp-pick-name nil part)
							 user))
					     (cdr (second  ret-def)))
				   (mapcar
				    #'(lambda (part)
					(intern (second part) user))
				    out-parts)))
		     (ret-name (when ret-def (second (first out-parts))))
		     (ret-parts (if ret-def
				    (mapcan (lambda (part var)
					      (list (xmp-pick-name nil part)
						    var))
					    (cdr (second  ret-def))
					    ret-keys
					    )
				  (mapcan 
				   #'(lambda (part var)
				       (list (second part) var))
				   out-parts ret-keys)
				  ))
		     )
		(push `(',in-msg-name ',in-elts
				      ,@(when action (list :action action))
				      :lisp-name ',def-name :return ',out-msg-name)
		      (wsdl-server-exports conn))
		(setf comment (format nil "Handler for message ~A" in-msg-name))
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
	 comment))
      )))

(defmethod wsdl-service ((conn wsdl-file-connector) service &optional error-p)
  (or (when (consp service) service)
      (assoc service (cdr (wsdl-services conn)) :test #'string-equal)
      (if error-p
	  (error "Cannot find service ~S" service)
	nil)))
  
(defmethod wsdl-service-port-name ((conn wsdl-file-connector) service &optional error-p)
  (let ((sdef (wsdl-service conn service error-p)))
    (when sdef
      (or (second (assoc :port (third sdef)))
	  (if error-p
	      (error "Cannot find port name in service ~S" sdef)
	    nil)))))

(defmethod wsdl-service-binding-name ((conn wsdl-file-connector) service
				      &optional error-p)
  (let* ((sdef (wsdl-service conn service error-p))
	 (pref (assoc :port (third sdef)))
	 )
    (when sdef
      (or (getf (getf (cddr pref) :attributes) 'wsdl:|binding|)
	  (if error-p
	      (error "Cannot find binding name in service ~S" sdef)
	    nil)))))

(defmethod wsdl-service-location ((conn wsdl-file-connector) service
				  &optional error-p)
  (let* ((sdef (wsdl-service conn service error-p))
	 (pref (assoc :port (third sdef)))
	 )
    (when sdef
      (or (getf (cdr (assoc :soap-address (getf (cddr pref) :content)))
		'wsdl:|location|)
	  (if error-p
	      (error "Cannot find binding name in service ~S" sdef)
	    nil)))))


(defmethod make-client-interface ((conn wsdl-file-connector) service destination
				  &rest args
				  &key
				  (eval t)
				  (lisp-package :keyword)
				  (file-package :user)
				  (expand-singleton t)
				  null-element (empty-element nil)
				  (prefix :client-) (suffix :index))
  (declare (ignore eval lisp-package file-package null-element empty-element
		   expand-singleton prefix suffix))
  (apply 'wsdl-make-interface conn service destination :client args))

(defmethod make-server-interface ((conn wsdl-file-connector) service destination
				  &rest args
				  &key
				  (eval t)
				  (lisp-package :keyword)
				  (file-package :user)
				  (expand-singleton t)
				  null-element (empty-element nil)
				  (prefix :server-) (suffix :index)
				  action
				  message-dns
				  )
  (declare (ignore eval lisp-package file-package null-element empty-element
		   expand-singleton prefix suffix action message-dns))
  (apply 'wsdl-make-interface conn service destination :server args))
				  

(defmethod wsdl-make-interface ((conn wsdl-file-connector) service destination
				  mode
				  &key
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
				  action
				  message-dns
				  &aux open defs)
  (or service (setf service 0))
  (when (numberp service) (setf service (elt (wsdl-service-names conn) service)))
  (etypecase destination
    (stream nil)
    ((member nil t) nil)
    (string (setf open t destination (open destination :direction :output)))
    )
  (when (and expand-singleton (null eval))
    (warn "expand-singleton=t is ignored when eval=nil"))
  (flet ((do-form
	  (form &rest comments)
	  (etypecase form
	    (string (let ((*package* file-package))
		      (read-from-string form)))
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
	       (push (if comments
			 (cons name comments)
		       name)
		     (cdr place)))		    
	     (when destination
	       (when comments
		 (format destination "~&~%~%")
		 (dolist (c comments) (format destination ";; ~A~%" c)))
	       (let ((*package* file-package))
		 (format destination "~&~%~S~%" form)))
	     (when eval (eval form))
	     form))))
    (unwind-protect
	(let* ((sdef (wsdl-service conn service t))
	       (port-name (wsdl-service-port-name conn sdef t))
	       (bname (wsdl-service-binding-name conn sdef t))
	       (pdef (or (assoc port-name (cdr (wsdl-port-types conn)))
			 (error "Cannot find portType ~S" port-name)))
	       (opdefs (getf (cdr pdef) :port-type))
	       (bdef (or (assoc bname (cdr (wsdl-bindings conn)))
			 (error "Cannot find binding ~S" bname)))
	       (types (cdr (schema-types conn)))
	       (binding (getf (cdr bdef) :binding))
	       (btype   (getf (cdr bdef) :type))
	       soap-binding x
	       )

	  (or (eq port-name btype)
	      (error "service.port=~S binding.type=~S" port-name btype))
	  (setf (wsdl-undef-ns conn) nil
		(wsdl-soap-address conn) (wsdl-service-location conn sdef t)
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

	  (dolist (typedef types) (wsdl-define-type conn typedef  #'do-form))

	  (dolist (b binding)
	    (case (first b)
	      (:soap-binding

	       ;; (:soap-binding wsdl:style rpc|document wsdl:transport URLstring) ???

	       (when soap-binding (warn ":binding - Found second ~S" b))
	       (setf soap-binding b)
	       (setf (wsdl-soap-style conn) (getf (cdr soap-binding) 'wsdl:|style|))
	       )
	      (:operation nil)
	      (otherwise (warn ":binding - Found ~S" b))))
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
			   :publish (list :path ,(wsdl-soap-address conn))
			   ,@(when action (list :action action))
			   :lisp-package :keyword
			   ,@(when message-dns
			       (list :message-dns (list 'quote message-dns)))
			   )))
		   ,@(mapcar #'(lambda (ex)
				 (list* 'soap-export-method (do-form "s") ex))
			     (wsdl-server-exports conn))
		   ,(do-form "s"))))))
						   
	  (when (wsdl-undef-ns conn)
	    (dolist (ns (wsdl-undef-ns conn))
	      (warn "There is no Lisp package defined for namespace ~S"
		    ns)))

	  defs)
      (when open (close destination)))))





