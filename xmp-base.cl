;; -*- mode: common-lisp; package: net.xmp -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2003-2014 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: xmp-base.cl,v 2.13 2007/04/17 21:50:41 layer Exp $

;; Common XML Message Protocol support for SOAP, XMLRPC, and others...

(defpackage :net.xmp)
(in-package :net.xmp)

;; FEATURES
;;   :soap-sax   - use native SAX parser API
;;   :soap-lxml  - use LXML-on-SAX parser

;; pick a default if no features present at all
#-(or soap-sax soap-lxml)
  (eval-when (compile load eval) (push :soap-lxml *features*))

;; make sure only one of the two is present
#+(or (and soap-sax soap-lxml))
  (eval-when (compile load eval) (error "TOO MANY XML PARSER FEATURES"))



(eval-when (compile load eval) 
  (require :sax)
  (require :aserve)
  )

(defparameter *xmp-version*
  (list 
   ;; 1.3.1 was version released with 9.0 and earlier
   ;; 1.3.3 was patch release, includes SOAP 2.31.0
   ;;       rfe7514  SOAP print-object methods should be print-pretty friendly
   ;;       bug17411 soap gf arglist doesn't agree with documentation
   ;;       bug17461 SOAP decoder whitespace handling re XML-schema
   ;;       bug21405 SOAP fails to encode double-float zero
   ;;       bug17705 wsdl-include-url is defective
   ;;       rfe7512  decode-wsdl-source should accept do-http-request's :protocol kw
   3 1 ;;; 3.1.x delete *soap-version*, soap-version calls xmp-version
   ;;.1   ;;; bug21565 wsdl decoder assumes soap-operation is always present
   ;;.2   ;;; bug21863 fix WSDL gen of SOAP call with zero args  
   ;;.3   ;;; bug17080 wsdl document style reply may be a simple type
   ;;      rfe6769  document style WSDL may specify messages with multiple parts
   ;;      bug21752 WSDL decoder is confused when SOAP message is a simple type
   ;;      add make-client-interface &key op-is-action
   ;;4   ;;; rfe12678  allow SOAP application to see raw XML string
   ;;        rfe7502   discover the difference between encoded and literal
   ;;        bug20691  call-soap-method modifies argument data destructively
   5   ;;; bug21649  encode-wsdl-file cannot handle nested collectors
   ;;      bug20703  restriction on xsd:decimal translates to :any
   ))
(defun xmp-version (&optional v1-or-s v2 v3 error-p &aux (v1 v1-or-s))
  (typecase v1
    (integer
     (if (or (< (first *xmp-version*) v1)
	     (and v2
		  (or 
		   (and (= (first *xmp-version*) v1)
			(< (second *xmp-version*) v2))
		   (and v3
			(= (second *xmp-version*) v2)
			(< (third *xmp-version*) v3)))))
	 (if error-p
	     (error "XMP/SOAP/WSDL Version ~A.~A.~A needed, but ~{~A.~A.~A~} is loaded."
		    v1 (or v2 0) (or v3 0) *xmp-version*)
	   nil)
       *xmp-version*))
    (otherwise (format v1-or-s "XMP/SOAP/WSDL Version ~{~A.~A.~A~}" *xmp-version*))))

#+soap-lxml (defun net.xml.sax::same-iri (x y) (net.xmp::same-uri x y))
    


(defpackage :net.xmp
  (:export

   ;; Classes
   xmp-connector

   xmp-client-connector
   xmp-server-connector

   xmp-string-out-connector
   xmp-stream-out-connector

   xmp-string-in-connector
   xmp-event-in-connector

   xmp-string-in-out-connector
   xmp-event-string-connector
   xmp-string-stream-out-connector
   xmp-event-stream-connector
   
   xmp-client-string-out-connector
   xmp-client-stream-out-connector
   xmp-client-string-in-connector
   xmp-client-event-in-connector
   xmp-client-string-in-out-connector
   xmp-client-event-string-connector
   xmp-client-string-stream-out-connector
   xmp-client-event-stream-connector

   xmp-server-string-out-connector
   xmp-server-stream-out-connector
   xmp-server-string-in-connector
   xmp-server-event-in-connector
   xmp-server-string-in-out-connector
   xmp-server-event-string-connector
   xmp-server-string-stream-out-connector
   xmp-server-event-stream-connector

   xmp-element

   xmp-condition
   xmp-client-condition
   xmp-server-condition
   xmp-namespace-declaration
   xmp-namespace-map

   ;; Slot names 
   protocol
   transport
   message-dns
   lisp-package
   xmp-error-code
   trim-whitespace
   string-type
   xml-leader
   xml-encoding
   base-dns

   ;; Accessors
   xmp-message-string
   xmp-received-string
   xmp-expected-elements
   xmp-element-name
   xmp-element-type
   xmp-element-content
   xmp-element-tag1
   xmp-element-tag2
   xmp-lisp-value 
   xmp-destination-leader
   xmp-xml-encoding
   xmp-server-lock
   xmp-server-exports
   xmp-server-enabled
   xmp-server-start
   xmp-client-start
   xmp-message-dns
   xmp-base-dns
   xmp-lisp-package
   xmp-trim-whitespace
   xmp-inside-elements
   xmp-normal-element-spec
   xmp-normal-type-spec 
   xmp-fault-code
   xmp-fault-sub-code
   xmp-fault-string
   xmp-fault-factor
   xmp-fault-detail
   xmp-xml-syntax

   nsd-package
   nsd-prefix
   nsd-uri 
   make-nsd
   nse-default
   nse-defs
   nse-tail
   make-nse
   make-nse* 

   ;; Generic functions
   define-xmp-type
   define-xmp-element

   xmp-string-type
   xmp-struct-type
   xmp-call-method
   xmp-message-begin
   xmp-encode
   xmp-copy
   xmp-encode-object
   xmp-object-class 
   xmp-encode-begin
   xmp-encode-attribute 
   xmp-encode-content
   xmp-encode-end
   xmp-message-send
   xmp-decode-message
   xmp-decode-string
   xmp-parse-message
   xmp-parse-file
   xmp-decode
   xmp-begin-message
   xmp-begin-element
   xmp-decode-element
   xmp-complex-content
   xmp-simple-content
   xmp-end-element
   xmp-end-message
   xmp-start-server
   xmp-stop-server
   xmp-enable-server
   xmp-disable-server
   xmp-accept-method
   xmp-invoke-method
   xmp-enable-method
   xmp-server-implementation
   xmp-server-message
   xmp-export-method
   xmp-export-standard-methods
   xmp-error
   xmp-warning
   xmp-warning-class
   xmp-class-warning
   xmp-list-methods
   xmp-method-signature
   xmp-method-help
   xmp-slot-value
   xmp-run-one-file
   xmp-decode-file
   xmp-decode-string
   xmp-decode-stream
   xmp-signature-equal
   xmp-in-depth
   xmp-out-depth
   xmp-decode-qualified-name
   xmp-encode-qualified-name
   xmp-encoded-qualified-name
   xmp-content-string
   xmp-find-type
   xmp-find-element
   xmp-elt-getf-name
   xmp-pick-name
   xmp-getf-in-part
   xmp-getf-in-def
   xmp-simple-exel
   xmp-defined-element-def
   xmp-defined-array-def
   xmp-warning-leader
   xmp-match-name
   xmp-getf
   xmp-assoc
   xmp-uri-to-prefix
   xmp-uri-to-package
   xmp-add-dns-entry
   xmp-any-type
   xmp-any-cpart
   xmp-collector-p
   xmp-collection-p
   xmp-collection-type-p
   xmp-export-namespace
   xmp-export-namespace-map
   xmp-find-namespace
   xmp-normal-nse
   xnd-package xnd-prefix xnd-uri  
   xnm-name xnm-read-only xnm-default xnm-entries xnm-tail
   xmp-get-attribute
   xmp-get-attributes
   xmp-search-map

   ;; Ordinary functions
   encode-base64-string
   decode-base64-string
   same-uri
   remove-keys
   string-equal-ex
   xmp-version
   xmp-resolve-uri
   xmp-decode-namespaces
   xmp-extract-namespaces
   define-namespace
   delete-namespace
   define-namespace-map
   xmp-define-namespace-map
   match-tree 
   xmp-new-environment

   ;; Macros
   with-tree-match
   def-xmp-sub-classes 
   xmp-symbol
   
   ;; Variables
   *xmp-server*
   *xmp-debug*
   *xmp-warning-leader*
   *xmp-compare-uri*
   *xmp-redef-default*

   ))

;; A package for reserved XML names
(defpackage :net.xmp.xml (:use))


(defvar *xmp-debug* nil)
(defvar *xmp-redef-default* :warn)


;;; Load-time symbol BEGIN
;;;
;;; These macros and functions provide a facility for interning 
;;; fixed-case symbols at load or eval time to allow a case-portable
;;; fasl file.
;;; 
;;; *xmp-symbols* -> #(package-array symbol-array)
;;; 
;;; (xmp-symbol name-string pkg)
;;;       ==> (xmp-symbol-ref name-string 'pkg xx yy)
;;; 
;;; The function xmp-symbol-ref interns the name in the package and caches
;;;  the result in the above vectors.

(eval-when (compile load eval)
  (defvar *xmp-symbols*
    ;; There will be a compile-time error if we run out of space in these tables
    (vector (make-array 20) (make-array 1000))
    )
  (defun xmp-symbol-ref (name pk-name pk-index name-index
			      &aux
			      (both *xmp-symbols*)
			      (svec (svref both 1))
			      )
    (or (svref svec name-index)
	(let* ((pvec (svref both 0))
	       (pksv (svref pvec pk-index))
	       (pk (or pksv (find-package pk-name) (error "Cannot find ~S" pk-name)))
	       new)
	  (or pksv (setf (svref pvec pk-index) pk))
	  (setf (svref svec name-index) (setf new (intern name pk)))
	  new)))
  )

(defmacro xmp-symbol (name package)
  (let* ((packages (svref *xmp-symbols* 0))
	 (symbols  (svref *xmp-symbols* 1))
	 (pk (or (find-package package) (error "not a package")))
	 (sym (intern name pk))
	 (px (dotimes (i (length packages)
			 (error "increase package array in *xmp-symbols*"))
	       (cond ((eq pk (aref packages i)) (return i))
		     ((null (aref packages i))
		      (setf (aref packages i) pk)
		      (return i)))))
	 (sx (dotimes (i (length symbols)
			 (error "increase symbol array in *xmp-symbols*"))
	       (cond ((eq sym (aref symbols i)) (return i))
		     ((null (aref symbols i))
		      (setf (aref symbols i) sym)
		      (return i))))))
    `(xmp-symbol-ref ,name ',package ,px ,sx)))

	 
;;; Load-time symbol END



;;; composite class names:  xml-protocol-transport-role-sendstyle-receivestyle-connector
;;;               protocol -> xmp  soap
;;;              transport -> aserve
;;;                   role -> client  server
;;; sendstyle-receivestyle -> string-in   string-out   string-in-out
;;;                        -> event-in    stream-out   event-stream
;;;
;;; For any subclass of P (and P-*) and Q (and Q-*) we need  
;;; 27 subclasses:           with precedence list:
;; S                               P              Q
;; S-string-out               S    P-string-out   Q-...
;; S-stream-out               S    P-stream-out   Q-...
;; S-string-in                S    P-string-in    Q-...
;; S-event-in                 S    P-event-in     Q-...
;; S-string-in-out            S-string-out S-string-in  P-string-in-out      Q-...
;; S-event-string             S-string-out S-event-in   P-event-string       Q-...
;; S-string-stream-out        S-stream-out S-string-in  P-string-stream-out  Q-...
;; S-event-stream             S-stream-out S-event-in   P-event-stream       Q-...
;;
;; S-client                   S                         P-client             Q-...
;; S-client-string-out        S-client S-string-out     P-client-string-out  Q-...
;; S-client-stream-out        S-client S-stream-out     P-client-stream-out  Q-...
;; S-client-string-in         S-client S-string-in      P-client-string-in   Q-...
;; S-client-event-in          S-client S-event-in       P-client-event-in    Q-...
;; S-client-string-in-out     S-client-string-out S-client-string-in S-string-in-out
;;                            P-client-string-in-out      Q-...
;; S-client-event-string      S-client-string-out S-client-event-in S-event-string
;;                            P-client-event-string       Q-...
;; S-client-string-stream-out S-client-stream-out S-client-string-in S-string-stream-out
;;                            P-client-string-stream-out  Q-...
;; S-client-event-stream      S-client-stream-out S-client-event-in S-event-stream
;;                            P-client-event-stream       Q-...
;;
;; S-server                   S                         P-server             Q-...
;; S-server-string-out        S-server S-string-out     P-server-string-out  Q-...
;; S-server-stream-out        S-server S-stream-out     P-server-stream-out  Q-...
;; S-server-string-in         S-server S-string-in      P-server-string-in   Q-...
;; S-server-event-in          S-server S-event-in       P-server-event-in    Q-...
;; S-server-string-in-out     S-server-string-out S-server-string-in S-string-in-out
;;                            P-server-string-in-out      Q-...
;; S-server-event-string      S-server-string-out S-server-event-in S-event-string
;;                            P-server-event-string       Q-...
;; S-server-string-stream-out S-server-stream-out S-server-string-in S-string-stream-out
;;                            P-server-string-stream-out  Q-...
;; S-server-event-stream      S-server-stream-out S-server-event-in S-event-stream
;;                            P-server-event-stream       Q-...
;;
;;

(defmacro def-xmp-sub-classes ((prefix suffix) supers &rest slots-and-options)
  ;; supers -> ((prefix suffix) ...)
  ;; slots-and-options ((middle slots . options) ...)
  (let* (out place tail)
    (flet ((name (prefix role middle suffix)
		 (when (or prefix role middle suffix)
		   (read-from-string
		    (format nil "~A~A~A~A~A~A~A"
			    (if prefix prefix "")
			    (if (and prefix (or role middle)) "-"    "")
			    (if role   role   "")
			    (if (and role middle)  "-"    "")
			    (if middle middle "")
			    (if suffix "-" "")
			    (if suffix suffix ""))))))

      (dolist (role '(nil "client" "server"))
	(dolist (mid '( (nil)
			("string-out" nil)
			("stream-out" nil)
			("string-in" nil)
			("event-in" nil)
			("string-in-out" "string-out" "string-in")
			("event-string"  "string-out"  "event-in")
			("string-stream-out" "stream-out" "string-in")
			("event-stream"  "stream-out" "event-in")
			))

	  (setf place (name nil role (first mid) nil))
	
	  (push `(defclass ,(name prefix role (first mid) suffix)
		   ,(append (mapcar #'(lambda (required)
					(name prefix role required suffix))
				    (cdr mid))
			    (when role
			      (if (null (cdr mid))
				  (list (name prefix nil nil suffix))
				(list (name prefix nil (first mid) suffix))))
			    (mapcar #'(lambda (added &aux (pre (first added))
						     (suff (second added)))
					(name pre role (first mid) suff))
				    supers))
		   ,@(if (setf tail (assoc place slots-and-options
					   :test #'string-equal))
			 (cdr tail)
		       (list nil))
		   )
		out)
	  ))
      `(progn ,@(reverse out)))))




(def-xmp-sub-classes
  ("xmp" "connector") ()
  (nil 
   (

   ;;; CLASS SLOTS

    (send-style    :reader xmp-send-style
		   ;; values -> :message  :stream
		   :allocation :class)
    (receive-style :reader xmp-receive-style
		   ;; values -> :message  :stream
		   :allocation :class)
    (protocol    :reader xmp-protocol  
		 ;; values -> :xmlrpc  :soap  :xml-schema
		 :allocation :class)
    (role        :reader xmp-role      
		 ;; values -> :client  :server
		 :allocation :class)
    (transport   :reader xmp-transport 
		 ;; values -> :aserve
		 :allocation :class)

    (string-type :reader xmp-string-type :initform (xmp-symbol "string" :keyword)
		 :allocation :class)


   ;;; INSTANCE SLOTS COPIED by xmp-copy

    (xml-leader   :accessor xmp-destination-leader :initarg :xml-leader   :initform "")
    (xml-encoding :accessor xmp-xml-encoding       :initarg :xml-encoding :initform nil)

    (message-dns  
     ;; Pre-defined namespaces and prefixes
     ;; value -> namespace-map | xmp-namespace-map instance
     ;;OLD
     ;; value -> (default-namespace-uri
     ;;           (package-or-name prefix uri) ... )
     :accessor xmp-message-dns :initform nil :initarg :message-dns)
    (base-dns
     ;; value -> namespace-map | xmp-namespace-map instance 
     :accessor xmp-base-dns :initform nil :initarg :base-dns)

    (out-nss
     ;; Namespace context stack
     ;; value -> (ns-entry ... )
     :accessor xmp-out-nss :initform nil)
    (in-nss
     ;; Namespace context stack
     ;; value -> (ns-entry ... )
     :accessor xmp-in-nss :initform nil)
    (expected-elements :accessor xmp-expected-elements :initform nil)
    (inside-elements   :accessor xmp-inside-elements :initform nil)

    (lisp-package    :accessor xmp-lisp-package :initarg :lisp-package :initform nil)
    (trim-whitespace
     ;; nil    -> no action
     ;;  t     -> drop leading and trailing #\space #\newline and #\tab
     ;; string -> drop leading and trailing elements of string
     :accessor xmp-trim-whitespace :initarg :trim-whitespace :initform nil)

    (debug            :accessor xmp-debug :initarg :debug :initform *xmp-debug*)
    (nillable         :accessor xmp-nillable
		      ;;  :ignore    -> ignore nillable and nil attributes
		      ;;  :strict    -> must be declared in element def
		      ;;  :accept    -> do not look for declaration
		      :initarg :nillable
		      :initform :ignore ;;; for compatibility with older versions
		      )
    (xml-syntax       :accessor xmp-xml-syntax
		      ;;   nil   -> :seq? is :set*, :maybe spliced in
		      ;; :strict -> :seq? is :seq, :maybe ignored
		      :initarg :xml-syntax :initform nil)

   ;;; INSTANCE SLOTS NOT-copied by xmp-copy

    (message-xml :accessor xmp-message-xml :documentation "no-xmp-copy")
    (message-dtd :accessor xmp-message-dtd :documentation "no-xmp-copy")
    (message-pns :accessor xmp-message-pns
		 ;; parser namespace alist ((uri . pkg) ...)
		 :documentation "no-xmp-copy") 
    (message-ns
     ;; Namespace mapping returned by XML parser
     ;; value -> (default-namespace-uri
     ;;           (package prefix uri) ... )
     :accessor xmp-message-ns :initform nil :documentation "no-xmp-copy")
    (message-attributes
     ;; hashtable to hold attributes of message elements
     :accessor xmp-message-attributes :initform nil :documentation "no-xmp-copy")

    ))
  ("client"
   ((role :initform :client)
    (client-start :accessor xmp-client-start :initform nil :initarg :start
		  :documentation "no-xmp-copy")
    ))
  ("server"
   ((role :initform :server)
    (server-lock :reader xmp-server-lock :initform (mp:make-process-lock)
		 :documentation "no-xmp-copy")

   ;;; INSTANCE SLOTS COPIED by xmp-copy

    (server-enabled :accessor xmp-server-enabled :initform nil)
    (server-start   :accessor xmp-server-start   :initarg :start :initform nil)
    (exports     
     ;; key is symbol or string that names an exported method
     ;; value is list of hash-tables
     ;; hash value -> ((signature return-type lisp-function enabled) ... )
     :accessor xmp-server-exports :initform (xmp-make-tables nil))
    ))
  ("string-out"
   ((message-string :accessor xmp-message-string
		    :initform nil :initarg :message-string
		    :documentation "no-xmp-copy")
    (message-init   :accessor xmp-message-init
		    ;; [rfe6307] allow user to specify adjust-array strategy
		    :initform 500 :initarg :message-init)
    (received-string :accessor xmp-received-string
		     ;; [rfe12678] A place to stash the arriving string.
		     :initform nil :initarg :received-string
		     :documentation "no-xmp-copy")
    ))
  )


(defclass xmp-element () 
  ((name :accessor xmp-element-name :initarg :name :initform nil)
   (tag1 :accessor xmp-element-tag1 :initarg :tag1 :initform nil)
   (tag2 :accessor xmp-element-tag2 :initarg :tag2 :initform nil)
   (lisp-value    :accessor xmp-lisp-value      :initarg :lisp-value :initform nil)
   (content       :accessor xmp-element-content :initarg :content :initform nil)
   (type          :accessor xmp-element-type    :initarg :type :initform nil)
   ))

(defmethod print-object ((x xmp-element) s)
  (print-unreadable-object 
   (x s :type t :identity t)
   (format s "~S ~_name=~S ~_type= ~S ~_~A"
	   (xmp-lisp-value x)
	   (xmp-element-name x)
	   (xmp-element-type x)
	   (if (xmp-element-content x)
	       (format nil "'~A'" (xmp-element-content x))
	     "nil")
	   )))



(define-condition xmp-condition (error)
  ((xmp-error-code    
    ;; :version-mismatch
    ;; :must-understand
    ;; :client   :client.xxx
    ;; :server   :server.xxx
    :reader xmp-fault-code   :initarg :code   :initform nil)
   (sub-code :reader xmp-fault-sub-code :initarg :sub-code :initform nil)
   (string  :reader xmp-fault-string :initarg :string :initform nil)
   (factor  :reader xmp-fault-factor :initarg :factor :initform nil)
   (detail  :reader xmp-fault-detail :initarg :detail :initform nil)))

(define-condition xmp-client-condition (xmp-condition) ())
(define-condition xmp-server-condition (xmp-condition) ())

(defmethod print-object ((e xmp-condition) s)
  (print-unreadable-object 
   (e s :type nil :identity t)
   (let* ((sub (xmp-fault-sub-code e))
	  (str (xmp-fault-string e))
	  (fac (xmp-fault-factor e))
	  (det (xmp-fault-detail e)))
     (format s "xmp~S" (xmp-fault-code e))
     (when sub (format s ".~A" sub))
     (when fac (format s " ~_factor=~S" fac))
     (cond (str (format s " ~_~A" str))
	   (det (format s " ~_detail=~S" det)))
     )))



(defgeneric xmp-encode (connector data type &rest options &key &allow-other-keys))
(defgeneric xmp-begin-message (connector))
(defgeneric xmp-end-message (conn data &key &allow-other-keys))
(defgeneric xmp-message-send (conn &key &allow-other-keys))


(defmethod xmp-copy ((object xmp-connector) &optional destination)
  (let* ((class (class-of object))
	 (slots (mop:compute-slots class))
	 (new (or destination (make-instance class)))
	 name alloc)
    (dolist (slot slots)
      (setf name (mop:slot-definition-name slot))
      (setf alloc (mop:slot-definition-allocation slot))
      (cond ((eq alloc :class)
	     ;; do not copy class slots
	     ;; (format t "~&; slot ~S is a class slot~%" name)
	     )
	    ((search "no-xmp-copy" (or (documentation slot t) ""))
	     ;; do not copy slots with marker
	     ;; (format t "~&; slot ~S is no-xmp-copy~%" name)
	     )
	    ((slot-boundp object name)
	     ;; (format t "~&; slot ~S is copied~%" name)
	     (setf (slot-value new name) (slot-value object name)))
	    (t
	     ;; do not copy unbound slots
	     ;; (format t "~&; slot ~S is unbound~%" name)
	     )))
    new))



(defmethod xmp-decode-message ((conn xmp-string-in-connector) data)
  ;; Analyze a parsed XML document: data -> list of LXML elements from a parser.
  ;; 
  ;; xmp-begin-message is called to note the start of the analysis;
  ;;                   returns spec of expected top element in LXML.
  ;; xmp-decode-body is called to perform the analysis;
  ;;                   returns list of data collected by analysis functions,
  ;;                       and list of types collected by analysis functions.
  ;; xmp-end-message is called to compute final result of decode call.
  (setf (xmp-in-nss conn) (xmp-initial-nss conn nil))
  (setf (xmp-message-attributes  conn) nil)
  (setf (xmp-expected-elements conn)
	(list (xmp-new-complex-def conn (xmp-begin-message conn))))
  (multiple-value-bind (dt tp)
      (xmp-decode-body conn data)
    (xmp-end-message conn dt :types tp)))

(defmethod xmp-note-attributes ((conn xmp-connector) vals attrs)
  ;; called from xmp-decode-body with value list returned by xmp-decode-element
  ;; data is always a list
  ;; only the conses in the list represent element/content pairs

  (when attrs
    (let ((table (xmp-message-attributes  conn)))
      (or table 
	  (setf (xmp-message-attributes  conn)
		(setf table (make-hash-table :test #'eq))))
      (dolist (data vals)
	(when (consp data)
	  (setf (gethash data table) attrs))))))


(defmethod xmp-get-attributes ((conn t) (data t)) nil)

(defmethod xmp-get-attributes ((conn xmp-connector) data)
  (let ((table (xmp-message-attributes  conn)))
    (when table (gethash data table))))

(defmethod xmp-get-attribute ((conn t) (data t) (name t) &optional default)
  (declare (ignore default))
  nil)

(defmethod xmp-get-attribute ((conn xmp-connector) data name &optional default)
  (let ((table (xmp-message-attributes  conn)))
    (when table
      (xmp-getf conn (gethash data table) name default))))
  
  

(defmethod xmp-update-prefix-map ((conn xmp-string-in-connector) attributes
				  &aux default defs)
  ;; build prefix -> package and uri mapping that may be needed to decode
  ;; some attribute values correctly.
  (do ((tl attributes (cddr tl)))
      ((atom tl) (push (when (or default defs) (make-nse* default defs))
		       (xmp-in-nss conn)))
    (let* ((name (string (first tl)))
	   (uri (second tl))
	   )
      (cond ((not (eql 0 (search "xmlns" name))))
	    ((eql 5 (length name)) 
	     (when default
	       (error "multiple default namespace declarations ~S ~S"
		      default uri))
	     (setf default uri))
	    ((eql 6 (length name)))
	    (t (push (make-nsd (xmp-package-of-uri conn uri)
			       (subseq name 6)
			       uri)
		     defs)))
      )))

(defun xmp-make-tables (fourth-p) 
  (vector (make-hash-table :test #'eq)
	  (make-hash-table :test #'equal)
	  (make-hash-table :test #'equalp)

	  (when fourth-p (make-hash-table :test #'eq))

	  ))

(defvar *defined-xmp-elements*
  ;; Fourth element is Defined Types
  (xmp-make-tables t))
    

(defmethod xmp-decode-body ((conn xmp-string-in-connector) data-list
			    &rest options &key &allow-other-keys
			    &aux content-list type-list
			    trim cdi exel simple
			    (strt (xmp-string-type conn))
			    )
  (declare (ignore options))
  ;; data is a list of elements
  ;; 
  ;; If we expect an empty element, then top of expected-elements is (:seq)

  (typecase (setf trim (xmp-trim-whitespace conn))
    ((member nil) nil)
    (string t)
    (otherwise (setf (xmp-trim-whitespace conn)
		     (setf trim
			   (concatenate
			    'string (list #\return #\space #\tab #\newline))))))
  (typecase (setf exel (first (xmp-expected-elements conn)))
    (xmp-complex-def-instance (setf cdi exel) (setf exel nil)))
  (flet ((collect-simple
	  (simple data)
	  (let ((res (multiple-value-list 
		      (xmp-simple-content conn simple data))))
	    (cond ((null res))
		  ((null (cdr res)) (setf content-list (nconc content-list res)))
		  ((cddr res) 
		   (xmp-error 
		    conn :client
		    :string (list "Too many values from xmp-simple-content ~S" res)))
		  (t (setf content-list (nconc content-list (list (first res))))
		     (push (second res) type-list))
		  ))))
    (cond
     ((and (null data-list)
	   exel (atom exel) (setf simple exel)
	   #+ignore
	   (when exel (if (atom exel)
			  (setf simple exel)
			(when (xmp-any-cpart conn exel) (setf simple strt)))))
      ;; Make sure that xmp-simple-content gets called for empty elements.
      (collect-simple simple nil))
     (t
      (loop
       (let (data elt attr type vals)
	 (when (null data-list)
	   (when (and cdi (eq :incomplete (xmp-cdi-state cdi)))
	     (xmp-error conn :client 
			:string (list "Expected: ~S  Found only: ~S" 
				      (xmp-cdi-def cdi) (xmp-cdi-found cdi))))
	   (return))
	 ;; data is a single element
	 (when (consp data-list)
	   (setf data (pop data-list))
	   (multiple-value-setq (elt attr) (xmp-element-parts conn data)))

	 (cond 
	  ((and elt (null (cdr data)) (xmp-nillable-p conn elt attr)))
	  ((and elt (or cdi (xmp-any-cpart conn exel)))
	   (cond ((if cdi (xmp-test-complex-def conn cdi elt) t)

		  ;; Update the context first, then get the expected elements
		  (push (cons elt attr) (xmp-inside-elements conn))
		  (xmp-update-prefix-map conn attr)

		  (push (xmp-new-complex-def
			 conn (xmp-begin-element conn elt :attributes attr))
			(xmp-expected-elements conn))

		  (multiple-value-setq (vals type)
		    (xmp-decode-element conn elt data :attributes attr))
		  (xmp-note-attributes conn vals attr)
		  (xmp-end-element conn elt)
		  (pop (xmp-inside-elements conn))
		  (pop (xmp-expected-elements conn))

		  (when cdi (xmp-step-complex-def conn cdi elt))

		  (when vals (setf content-list (nconc content-list vals))
			(when type (push type type-list))))
		 (cdi  ;;; [bug15502] refine the error message
		  (xmp-error conn :client 
			     :string (list "Expected: ~S  Found: ~S" 
					   (xmp-cdi-def cdi)
					   (append (xmp-cdi-found cdi) (list elt)))))
		 ((and (stringp data) trim 
		       (when (equal "" (string-trim trim data))
			 (setf data nil)
			 t))
		  (when data (collect-simple (setf simple strt) data)))
		 (t (xmp-error
		     conn :client 
		     :string (list "Non-blank string data in complex context ~S" data)))
		 ))
	  ((and (or cdi (xmp-any-cpart conn exel))
		(stringp data) trim (equal "" (string-trim trim data))))
	  ((and (or (null data) (stringp data))
		(when exel (if (atom exel)
			       (setf simple exel)
			     (when (xmp-any-cpart conn exel) (setf simple strt)))))
	   (when data
	     ;; collect all the string data into one string
	     (loop (let ((d (first data-list)))
		     (or (stringp d) (return))
		     (setf data (concatenate 'string data d))
		     (pop data-list))))
	   (collect-simple simple data))
	  (t (xmp-error conn :client :string (list "Expected: ~S  Found content: ~S" 
						   exel data))))))
      )))
  (values content-list (nreverse type-list))

  )

(defmethod xmp-nillable-p ((conn xmp-connector) elt attr &aux strict)
  (and attr
       (case (xmp-nillable conn)
	 (:accept t)
	 (:ignore nil)
	 (:strict
	  (setf strict t)
	  (when elt
	    (xmp-lookup *defined-xmp-elements* elt elt nil :attribute :nillable))))
       ;; [bug15528] 
       (do ((tl attr (cddr tl)))
	   ((atom tl) nil)
	 (when (or (eq (first tl)
		       (intern "nil" :net.xmp.schema-instance))
		   (and (not strict)
			(equal "nil" (string (first tl)))))
	   (return (or (equalp "true" (second tl))
		       (equal "1" (second tl))
		       (eql 1 (second tl)))	
		   )))))


(defmethod xmp-element-parts ((conn xmp-connector) elt &aux attr)
  (when (consp elt)
    (if (consp (setf elt (car elt)))
	(setf attr (cdr elt) elt (first elt))
      (setf attr nil))
    (values elt attr)))


(defmethod xmp-decode-element ((conn xmp-string-in-connector) elt data
			       &rest options &key attributes &allow-other-keys)
  ;; this method is to allow specialized context handling between
  ;;  call to xmp-begin-element and xmp-end-element
  
  ;; If contents is nil, check for nillable element 
  ;;  skip call if element is truly ignored
  (if (and (null (cdr data)) (xmp-nillable-p conn elt attributes))
      (values)
    (multiple-value-bind (vals types)
	(xmp-decode-body conn (cdr data) :attributes attributes)
      (xmp-complex-content conn elt vals :types types :attributes attributes))))

(defmethod xmp-any-type ((conn t) &optional (type nil t-p))
  (if t-p
      (and (consp type) (eq (first type) :complex) (xmp-any-cpart conn (second type)))
    (list :complex (xmp-any-cpart conn))))

(defmethod xmp-any-cpart ((conn t) &optional (type nil t-p))
  (if t-p
      (equal type '(:seq* (:any)))
    (list :seq* (list :any))))




(defmethod xmp-defined-element-def ((conn t) elt nss depth
				     &aux edef defined-p type-name exel)
  ;; return 3 values:
  ;;    expected-element = nil or a collector
  ;;    type-name        = nil or element (when exel is nil) or type name
  ;;    defined-p        = nil or elt
  (when (setf edef (xmp-defined-element-p conn elt nss depth))
    (setf defined-p elt))
  (typecase edef
    ((member nil) nil)
    (cons (ecase (first edef)
	    (:element (multiple-value-setq (exel type-name)
			(xmp-element-exdef conn edef nss)))
	    (:complex (setf exel (second edef)))
	    (:array (setf exel `(:seq* (:element nil ,(second edef)))))
	    (:simple (or (setf exel (second edef))
			 (setf exel (xmp-simple-exel conn edef))))))
    (symbol (multiple-value-setq (exel type-name)
	      (xmp-element-exdef conn edef nss)))
    )
  (values exel type-name defined-p))

(defmethod xmp-defined-array-def ((conn t) elt nss depth &aux edef)
  (typecase (setf edef (xmp-defined-element-p conn elt nss depth))
    ((member nil) nil)
    (cons (ecase (first edef)
	    (:element (nth-value 2
			(xmp-element-exdef conn edef nss)))
	    (:complex nil)
	    (:array edef)
	    (:simple nil)))))

(defmethod xmp-defined-element-p ((conn xmp-connector) (elt t) (nss t) depth
				  &aux prev)
  (or (typecase (setf prev
		      (ecase depth
			(0 (first (xmp-expected-elements conn)))
			(1 (second (xmp-expected-elements conn)))))
	(xmp-complex-def-instance (xmp-cdi-inner prev)))
      (call-next-method)))

(defmethod xmp-defined-element-p ((conn t) elt nss (depth t))
  (xmp-find-element conn elt nss))



(defmethod xmp-begin-element ((conn xmp-string-in-connector) (elt t)
			      &rest options &key &allow-other-keys)
  (declare (ignore options))
  (or (xmp-defined-element-def conn elt :in 0)
      ;; The default is to accept anything
      (xmp-any-cpart conn)))


(defmethod xmp-begin-element :before ((conn xmp-string-in-connector) 
				      (elt t)
				      &rest options &key attributes &allow-other-keys)
  (declare (ignore options   attributes))
  nil)


(defun string-equal-ex (x y &optional ignored-last-char)
  (typecase x
    ((or string symbol)
     (typecase y
       ((or string symbol)
	(or (string-equal x y)
	    (when ignored-last-char
	      (let* ((s1 (string x))
		     (l1 (length x))
		     (s2 (string y))
		     (l2 (length y)))
		(cond ((eql l1 (1- l2))
		       (and (eql ignored-last-char (elt s2 (1- l2)))
			    (string-equal s1 s2 :end2 (1- l2))))
		      ((eql l2 (1- l1))
		       (and (eql ignored-last-char (elt s1 (1- l1)))
			    (string-equal s1 s2 :end1 (1- l1))))

		      ))))
	)))))

(defun uri-to-string (x)
  (etypecase x
    (null nil)
    (net.uri:uri (or (net.uri::uri-string x)
		     (format nil "~A" x)))
    (string x)))


(defun normal-uri (x &aux xu)
  (etypecase x 
    (null nil)
    (net.uri:uri x)
    (string (if (setf xu (ignore-errors (net.uri:parse-uri x)))
		xu
	      x))))

(defparameter *xmp-compare-uri* nil)   ;;; or #\/
(defun same-uri (x y)
  (string-equal-ex (uri-to-string x) (uri-to-string y) *xmp-compare-uri*))

(defun equal-uri (x y)
  (string-equal (uri-to-string x) (uri-to-string y)))

(defmethod xmp-package-of-uri ((conn xmp-connector) uri &aux e canonical)
  (setf canonical (uri-to-string uri))
  (when (setf e (assoc canonical (xmp-message-pns conn) :test #'equal-uri))
    (cdr e)))


(defmethod xmp-end-element :after ((conn xmp-string-in-connector) 
				      (elt t)
				      &rest options &key &allow-other-keys)
  (declare (ignore options))
  (pop (xmp-in-nss conn)))

(defmethod xmp-in-depth ((conn xmp-connector)) (length (xmp-inside-elements conn)))

(defmethod xmp-out-depth ((conn xmp-connector) &aux (len (length (xmp-out-nss conn))))
  (case len (0 0) ((1 2) 1) (otherwise (- len 2))))


(defmethod xmp-simple-content :around ((conn xmp-string-in-connector) 
				       (elt t) (data t) &rest options 
				       &key &allow-other-keys)
  (declare (ignore options))
  (let ((v (multiple-value-list (call-next-method))))
    (cond ((null v) (values))
	  ((cdr v) (values (car v) (cadr v)))
	  (t       (values (car v) (first (first (xmp-inside-elements conn))))))))

(defmethod xmp-simple-content ((conn xmp-string-in-connector) 
			       (elt t) data &rest options &key &allow-other-keys
			       &aux def key)
  (or
   (when (or (setf def (xmp-find-element conn elt :in))
	     (setf def (xmp-find-type conn elt :in)))
     (etypecase def
       ((or string symbol)
	(return-from xmp-simple-content
	  (apply 'xmp-simple-content conn def data options)))
       (cons
	(case (first def)
	  (:simple
	   (cond ((second def)
		  (return-from xmp-simple-content
		    (apply 'xmp-simple-content conn (second def) data options)))
		 ((setf key (getf (cddr def) :simple-content-key))
		  (when (not (eq key elt))
		    (return-from xmp-simple-content
		      (apply 'xmp-simple-content conn key data options))))
		 ))
	  (otherwise 
	   (xmp-error conn :client :string (list "Expected: ~S  Found simple string: ~S" 
						 def data)))
	  ))))
	
   (if (null data)
       ;; Null data can only mean an empty element.
       (list elt)
     ;; default content is the parsed XML string content
     ;;(format t "~&;;xmp-simple-content ~S~%" elt)
     (list elt data))))

(defmethod xmp-complex-content :around ((conn xmp-string-in-connector) 
					(elt t) (data t) &rest options 
					&key &allow-other-keys)
  (declare (ignorable options))
  (let ((v (multiple-value-list (call-next-method))))
    (cond ((null v) (values))
	  ((cdr v) (values (car v) (cadr v)))
	  (elt     (values (car v) elt))
	  (t       (values (car v) nil)))))

(defmethod xmp-complex-content ((conn xmp-string-in-connector) 
				      (elt t) data
				      &rest options &key types &allow-other-keys)
  (declare (ignore options))
  ;; this default method is needed to propagate the results of 
  ;;  xmp-simple-content
  ;;(format t "~&;;xmp-complex-content ~S~%" elt)
  (if (and data (null (cdr data)) types (null (cdr types)))
      (values data (car types))
    data))

(defmethod xmp-end-element ((conn xmp-string-in-connector) 
			    (elt t)
			    &rest options &key &allow-other-keys)
  (declare (ignore options))
  nil)


(defmethod xmp-encode ((conn xmp-connector) (data t) (type t)
		       &rest options &key &allow-other-keys &aux v)
  (declare (ignore options))
  (setf v (call-next-method))
  (or type v))

(defmethod xmp-encode ((conn xmp-connector) (data cons) (type null)
		       &rest options &key &allow-other-keys &aux str)
  (declare (ignore options))
  (if (eq (setf str (xmp-struct-type conn)) (car data))
      (xmp-encode conn (cdr data) str)
    (call-next-method)))



(defmethod xmp-encode ((conn xmp-connector) (data xmp-element) type
		       &rest options &key &allow-other-keys
		       &aux string)
  (declare (ignore options))
  (or (null type)
      (eq type (xmp-element-type data))
      (xmp-error 
       conn :client 
       :string (list "Cannot encode ~S as ~S" (xmp-element-type data) type)))
  (or (setf string (xmp-element-content data))
      (progn
	(xmp-encode-object conn data type)
	(setf string (xmp-element-content data)))
      (xmp-error conn :client :string (list "Cannot encode ~S" data)))
  (xmp-encode-content conn string
		      :sanitize nil
		      :tag1 (xmp-element-tag1 data)
		      :tag2 (xmp-element-tag2 data))
  (xmp-element-type data))

(defmethod xmp-encode-object ((conn xmp-string-out-connector) (data xmp-element) type
			      &rest options &key &allow-other-keys)
  (if (xmp-element-content data)
      (if (or (null type) (equal type (xmp-element-type data)))
	  data
	(xmp-error conn :encode
		   :string
		   (list "Attempt to encode ~S as ~S."
			 (xmp-element-type data) type)))
      (let ((tc (xmp-copy conn)))
	(setf (xmp-message-string tc) nil)
	(xmp-message-begin tc)
	(setf (xmp-element-type data) (xmp-encode tc (xmp-lisp-value data) type))
	(setf (xmp-element-content data) (xmp-message-string tc))
	data))
  data)


(defmethod xmp-object-class  ((conn xmp-connector) (data t) (type t)
			      &rest options 
			      &key (class 'xmp-element) &allow-other-keys)
  (declare (ignore options))
  class)

(defmethod xmp-encode-object ((conn xmp-connector) data type
			      &rest options 
			      &key
			      (class (apply 'xmp-object-class
					    conn data type options))
			      (name type)
			      &allow-other-keys)
  (apply 'xmp-encode-object
	 conn
	 (make-instance class :name name :lisp-value data :type type)
	 type
	 options))

(defvar *xmp-server* nil)
(defmethod xmp-encode-object ((conn null) data type
			      &rest options &key &allow-other-keys)
  (if *xmp-server*
      (apply 'xmp-encode-object
	     *xmp-server*
	     data
	     type
	     options)
    (xmp-error nil :context :string "Cannot find a default XMP connection.")))


(defmethod xmp-message-begin ((conn xmp-string-out-connector)
			      &key namespaces)
  (let ((s (xmp-message-string conn)))
    (if s
	(setf (fill-pointer s) 0)
      (let* ((init (xmp-message-init conn))
	     (size (etypecase init
		     ((integer 100 1000000) init)
		     (null 500)
		     (cons (first init))
		     )))
	(setf (xmp-message-string conn)
	      (make-array size :element-type 'character
			  :adjustable t :fill-pointer 0))))
    (setf (xmp-out-nss conn) (xmp-initial-nss conn namespaces))
    ))

(defmethod xmp-content-string ((conn xmp-string-out-connector) data
			       &key (sanitize t) 
			       (string (make-array 100 :element-type 'character
						   :adjustable t :fill-pointer 0))
			       &allow-other-keys)
  (let* ((s string)
	 (string (if (stringp data)
		     data
		   (format nil "~A" data)))
	 (need (length string))
	 (init (xmp-message-init conn))
	 (extend (typecase init
		   (cons (typecase (second init)
			   ((integer 0) (+ need (second init)))
			   ((float 0 0.999999) 
			    (max need (truncate (* (array-total-size s)
							    (+ 1.0 (second init))))))
			   ((float 1.0)
			    (max need (truncate (* (array-total-size s)
						   (second init)))))))))
	 c e)
    (dotimes (i need)
      (setf c (elt string i))
      (if sanitize
	  (when (setf e (case c
			  (#\< "&lt;")
			  (#\> "&gt;")
			  (#\& "&amp;")
			  (otherwise (vector-push-extend c s extend) nil)))
	    (dotimes (j (length e))
	      (vector-push-extend (elt e j) s extend)))
	(vector-push-extend c s extend)))
    s))

(defmethod xmp-encode-content ((conn xmp-string-out-connector) data
			       &key tag1 tag2 (sanitize t) &allow-other-keys)
  (when tag1 (xmp-encode-begin conn tag1))
  (when tag2 (xmp-encode-begin conn tag2))
  (xmp-content-string conn data :sanitize sanitize :string (xmp-message-string conn))
  (when tag2 (xmp-encode-end conn tag2))
  (when tag1 (xmp-encode-end conn tag1)))



(defclass xmp-namespace-declaration ()
  ((xnd-package :accessor xnd-package :initarg :xnd-package :initform nil)
   (xnd-prefix  :accessor xnd-prefix  :initarg :xnd-prefix  :initform nil)
   (xnd-uri     :accessor xnd-uri     :initarg :xnd-uri     :initform nil)))

(defmethod print-object ((x xmp-namespace-declaration) s)
  (print-unreadable-object 
   (x s :identity t)
   (format s "XND ~A ~A ~_~A" (when (xnd-package x) (package-name (xnd-package x)))
	   (xnd-prefix x) (xnd-uri x))))

(defmethod xmp-export-namespace ((nm xmp-namespace-declaration))
  (list (package-name (xnd-package nm))
	(xnd-prefix nm)
	(xnd-uri nm)))

(defmethod xnd-equal-package (a b)
  (flet ((convert (x) (typecase x
			(package x)
			(null nil)
			((or string symbol) (find-package x))
			(xmp-namespace-declaration (xnd-package x)))))
    (and (setf a (convert a))
	 (setf b (convert b))
	 (eq a b))))

(defmethod xnd-equal-uri (a b)
  (flet ((convert (x) (typecase x
			(xmp-namespace-declaration (xnd-uri x))
			(string x)
			(net.uri:uri x))))
    (and (setf a (convert a))
	 (setf b (convert b))
	 (equal-uri a b))))

(defmethod xnd-same-uri (a b)
  (flet ((convert (x) (typecase x
			(xmp-namespace-declaration (xnd-uri x))
			(string x)
			(net.uri:uri x))))
    (and (setf a (convert a))
	 (setf b (convert b))
	 (same-uri a b))))

(defmethod xnd-equal-prefix (a b)
  (flet ((convert (x) (typecase x
			(xmp-namespace-declaration (xnd-prefix x))
			(string x))))
    (and (setf a (convert a))
	 (setf b (convert b))
	 (equal a b))))


(defclass xmp-namespace-map ()        
  ((xnm-name :accessor xnm-name    :initarg :xnm-name)
   (xnm-read-only :reader xnm-read-only :initarg :xnm-read-only :initform nil)
   (xnm-default :accessor xnm-default
		;; nil | :none | uri-string
		:initarg :xnm-default :initform nil)
   (xnm-entries :accessor xnm-entries :initarg :xnm-entries :initform nil)
   (xnm-tail   :accessor xnm-tail   :initarg :xnm-tail  :initform nil)))

(defmethod print-object ((x xmp-namespace-map) s)
  (print-unreadable-object 
   (x s :identity t)
   (format s "XNMap ~A" (xnm-name x))))

(defvar *xmp-package-to-xnd*
  ;; package --> (xmp-namespace-declaration ...)
  (make-hash-table :test #'eq))

(defvar *xmp-uri-to-xnd*
  ;; uri-string --> (xmp-namespace-declaration ...)
  (make-hash-table :test #'equal))

(defvar *xmp-namespace-maps*
  ;; keyword --> xmp-namespace-map
  (make-hash-table :test #'eq))


(defmethod xmp-export-namespace-map ((map t) &aux m2)
  (if (and map (setf m2 (gethash map *xmp-namespace-maps*)))
      (xmp-export-namespace-map m2)
    map))

(defmethod xnm-exported-default ((map xmp-namespace-map)
				 &aux (d (xnm-default map)))
  (case d (:none "") (otherwise d)))

(defmethod xmp-export-namespace-map ((map xmp-namespace-map))
  (append (list (xnm-exported-default map))
	  (mapcar #'(lambda (e)
		      (typecase e
			(null e)
			(symbol e)
			(cons e)
			(xmp-namespace-declaration (xmp-export-namespace e))
			(xmp-namespace-map 
			 (or (xnm-name e)
			     (error "cannot export un-named map")))))
		  (xnm-entries map))
	  (when (xnm-tail map) (list (xnm-tail map)))))


(defun xmp-hash-table-lock (key table &aux (val (gethash key table)))
  (typecase val
    (null (setf (gethash key table) (setf val (mp:make-process-lock))) val)
    (mp:process-lock val)
    (otherwise nil)))

(defmacro with-possible-lock ((lock &rest keys) &body body
			      &aux (lock-body (gensym)) (lockvar (gensym)))
  `(flet ((,lock-body () ,@body))
     (let ((,lockvar ,lock))
       (if ,lockvar
	   (mp:with-process-lock (,lockvar ,@keys) (,lock-body))
	 (,lock-body)))))

(defun normal-package (p)
  (typecase p
    (null nil)
    (package p)
    ((or string symbol) (find-package p))))

(defun xmp-find-namespace (package prefix uri)

  (let* ((pk (normal-package package))
	 (pf (when prefix (string prefix)))
	 (nur (when uri (uri-to-string uri)))
	 def1
	 def2)
    
    (or (when pk 
	  (with-possible-lock
	   ((xmp-hash-table-lock :lock *xmp-package-to-xnd*))
	   (dolist (p (gethash pk *xmp-package-to-xnd*) def1)
	     (when (or (null nur) (xnd-equal-uri nur p))
	       (if pf
		   (if (xnd-equal-prefix pf p)
		       (return p)
		     (when (null (xnd-prefix p))
		       (setf def1 p)))
		 (return p))))))
	(when nur
	  (with-possible-lock
	   ;; USE THE SAME LOCK for both tables!
	   ((xmp-hash-table-lock :lock *xmp-package-to-xnd*))
	   (dolist (p (gethash nur *xmp-uri-to-xnd*) def2)
	     (when (or (null pk) (xnd-equal-package pk p)) 
		  (if pf
		      (if (xnd-equal-prefix pf p)
			  (return p)
			(when (null (xnd-prefix p))
			  (setf def2 p)))
		    (return p)))))))))

(defun xmp-make-namespace (package prefix uri)
  (let* ((pk (etypecase package
	       (null (error "Package name should not be nil."))
	       (package package)
	       ((or string symbol) (or (find-package package)
				       (make-package package :use nil)))))
	 (pf (when prefix (string prefix)))
	 (nur (uri-to-string uri)))
    (make-instance 'xmp-namespace-declaration
		   :xnd-package pk
		   :xnd-prefix pf
		   :xnd-uri nur)))
    

(defun define-namespace (package prefix uri &optional primary-p)
  (with-possible-lock
   ((xmp-hash-table-lock :lock *xmp-package-to-xnd*))
   (let* ((pk (etypecase package
		(null nil)
		(package package)
		((or string symbol) (or (find-package package)
					(make-package package :use nil)))))
	  (pf (when prefix (string prefix)))
	  (nur (when uri (uri-to-string uri)))
	  (pdefs (when pk (gethash pk *xmp-package-to-xnd*)))
	  def1
	  (udefs (when nur (gethash nur *xmp-uri-to-xnd*)))
	  def2
	  newdef one)
     (dolist (p pdefs)
       ;; set def1 to exact match, first nil prefix, or first uri match
       (cond ((null nur) (setf one p))
	     ((xnd-equal-uri nur p) (setf one p))
	     (t (setf one nil)))
       (when one
	 (if pf
	     (if (xnd-equal-prefix pf p)
		 (return (setf def1 p))
	       (when (null (xnd-prefix p))
		 (setf def1 p)))
	   (return (setf def1 p)))))
     (dolist (p udefs)
       ;; set def2 to exact match, first nil prefix, or first pk match
       (cond ((null pk) (setf one p))
	     ((xnd-equal-package pk p) (setf one p))
	     (t (setf one nil)))
       (when one
	 (if pf
	     (if (xnd-equal-prefix pf p)
		 (return (setf def2 p))
	       (when (null (xnd-prefix p))
		 (setf def2 p)))
	   (return (setf def2 p)))))
     (cond ((and def1 (eq def1 def2))
	    (when pf
	      (or (xnd-equal-prefix pf def1)
		  (setf newdef t))))
	   ((and def1 def2) (when pf (setf newdef t)))
	   (def1 (or (null nur) (null pf) (xnd-equal-prefix pf def1) (setf newdef t)))
	   (def2 (or (null pk) (null pf) (xnd-equal-prefix pf def2) (setf newdef t)))
	   ((and pk nur) (setf newdef t)))
     (cond (newdef (setf newdef (make-instance 'xmp-namespace-declaration
					       :xnd-package pk
					       :xnd-prefix pf
					       :xnd-uri nur))
		   (push newdef pdefs)
		   (push newdef udefs))
	   ((null primary-p))
	   (t (when def1 (setf pdefs (cons def1 (remove def1 pdefs))))
	      (when def2 (setf udefs (cons def2 (remove def2 udefs))))))
     (when pk (setf (gethash pk *xmp-package-to-xnd*) pdefs))
     (when nur (setf (gethash nur *xmp-uri-to-xnd*) udefs))
     (or newdef def1 def2))))

(defun delete-namespace (package prefix uri)
  (with-possible-lock
   ((xmp-hash-table-lock :lock *xmp-package-to-xnd*))
   (let* (pdefs def udefs)
     (loop 
      (or (setf def (xmp-find-namespace package prefix uri))
	  (return))
      (when (setf pdefs (gethash (xnd-package def) *xmp-package-to-xnd*))
	(if (and (null (cdr pdefs)) (eq def (first pdefs)))
	    (remhash (xnd-package def) *xmp-package-to-xnd*)
	  (setf (gethash (xnd-package def) *xmp-package-to-xnd*)
		(remove def pdefs))))
      (when (setf udefs (gethash (xnd-uri def) *xmp-uri-to-xnd*))
	(if (and (null (cdr udefs)) (eq def (first udefs)))
	    (remhash (xnd-uri def) *xmp-uri-to-xnd*)
	  (setf (gethash (xnd-uri def) *xmp-uri-to-xnd*)
		(remove def udefs)))))
     )))


(defun define-namespace-map (name &rest parts)
  (xmp-define-namespace-map name nil parts))

(defun xmp-define-namespace-map (name read-only parts)
  ;; When name is nil, create an anonymous map.
  (let* (part 
	 (default
	   (etypecase (setf part (pop parts))
	     (null part)
	     (package part)
	     ((or string symbol) part)
	     (net.uri:uri part)))
	 entries tail
	 (def (when name (gethash name *xmp-namespace-maps*)))
	 dd)
    (and def (xnm-read-only def)
	 (or read-only
	     ;; Ie read-only can only be set at initial creation time.
	     (error "Cannot redefine namespace-map ~S" name)))
    (when (loop
	   (when (null parts) (return nil))
	   (setf part (pop parts))
	   (etypecase part
	     (null nil)
	     ((member :all) (push part entries))
	     ((member :stop) (setf tail part) (return nil))
	     (string (push (list nil nil part) entries))
	     (net.uri:uri (push (list nil nil part) entries))
	     (symbol (cond ((gethash part *xmp-namespace-maps*)
			    (push part entries))
			   (t (return t))))
	     (xmp-namespace-map (push part entries))
	     (xmp-namespace-declaration (push part entries))
	     (cons   (case (third part)
		       ((:any :other :prefix) (setf tail part) (return nil))
		       (t (push part entries))))))
      (error "Undrecognized namespace map part ~S" part))
    (when parts (error "Unreachable namespace map parts ~S" parts))
    (setf entries (mapcar #'(lambda (e)
			      (etypecase e
				(symbol e)
				(xmp-namespace-declaration e)
				(xmp-namespace-map e)
				(cons
				 (or
				  (xmp-find-namespace
				   (first e) (second e) (third e))
				  (xmp-make-namespace
				   (first e) (second e) (third e))))))
			  (nreverse entries)))
    (setf def (make-instance 'xmp-namespace-map
			 :xnm-name name
			 :xnm-read-only read-only
			 :xnm-entries entries
			 :xnm-tail tail))
    (when name (setf (gethash name *xmp-namespace-maps*) def))
    
    ;; Resolve the default after the parts have been defined.
    (when (typecase default
	    (null nil)
	    (package (or (setf dd (xmp-search-map def :package default :all t))
			 (setf dd (xmp-find-namespace default nil nil))
			 (error "Default package ~A is not defined as a namespace."
				(package-name default))))
	    (symbol  (or (setf dd (xmp-search-map def :package default :all t))
			 (setf dd (xmp-find-namespace default nil nil))
			 (error
			  "Default symbol ~A is not defined as a namespace package."
			  default)))
	    (string  (if (equal default "")
			 (progn (setf dd :none) nil)
		       (or (setf dd (xmp-search-map def :package default :all t))
			   (setf dd (xmp-find-namespace default nil nil))
			   (setf dd (xmp-search-map def :uri default :all t))
			   (setf dd (xmp-find-namespace nil nil default))
			   (progn (setf dd default) nil))))
	    (net.uri:uri (or (setf dd (xmp-search-map def :uri default :all t))
			     (setf dd (xmp-find-namespace nil nil default))
			     (progn (setf dd (format nil "~A" default)) nil)))
	    )
      (setf dd (xnd-uri dd)))
    (setf (xnm-default def) dd)
    def))

(defmethod xmp-search-map ((map null) &key default package prefix uri all)
  (declare (ignore default package prefix uri all))
  nil)

(defmethod xmp-search-map ((map symbol) &key default package prefix uri all &aux def)
  (if (setf def (gethash map *xmp-namespace-maps*))
      (xmp-search-map def :all all
		      :default default
		      :package package 
		      :prefix prefix 
		      :uri uri)
    (progn
      (cerror "Continue, using null definition." "Undefined namespace map ~S" map)
      nil)))

(defmethod xmp-search-map ((map cons) &key default package prefix uri all)
  (xmp-search-map (xmp-define-namespace-map nil nil map)
		  :all all
		  :default default
		  :package package 
		  :prefix prefix 
		  :uri uri))



(defmethod xmp-search-map ((map xmp-namespace-map)
			   &key default package prefix uri all)
  ;; Returns nil or xmp-namespace-declaration
  (if default
      (nse-default map)
    (let* ((pk (typecase package
		 (null nil)
		 (package package)
		 ((or string symbol) (find-package package))))
	   (pf (when prefix (string prefix)))
	   (nur (when uri (uri-to-string uri)))
	   nsd tl stop new)
      (flet ((test (e)
		   (cond (pk (and (xnd-equal-package pk e)
				  (or (null pf) (xnd-equal-prefix pf e))
				  (or (null nur) (xnd-equal-uri pf e))
				  (return-from xmp-search-map e)))
			 (nur (and (xnd-equal-uri nur e)
				   (or (null pf) (xnd-equal-prefix pf e))
				   (return-from xmp-search-map e)))
			 (pf (and (xnd-equal-prefix pf e)
				  (return-from xmp-search-map e))))))
	(dolist (e 
		 (xnm-entries map)
		 (cond (all
			;; When searching all known namespace maps, do not
			;;  look at tail.
			nil)
		       ((and nur (null pk))
			(typecase (setf tl (xnm-tail map))
			  (null nil)
			  ((member :stop) (values nil :stop))
			  (cons
			   (case (third tl)
			     ((:any :other)
			      (or (xmp-search-map map :package (first tl)
						  :prefix pf :uri nur :all t)
				  (when (setf new
					      (xmp-find-namespace
					       (first tl) pf nur))
				    (push new (xnm-entries map))
				    new)
				  (progn
				    (setf new (xmp-make-namespace
						(first tl)
						(or pf (second tl) "tns")
						nur))
				    (push new (xnm-entries map))
				    new)))
			     (:prefix
			      (or (xmp-search-map map :prefix pf :uri nur :all t)
				  (let* ((i 0) name)
				    (loop (incf i)
					  (setf name
						(symbol-name
						 (read-from-string
						  (string-downcase
						   (format nil ":~A~A"
							   (first tl) i)))))
					  (when (ignore-errors
						  (make-package name :use nil)
						  t)
					    (return)))
				    (setf new (xmp-make-namespace
						name
						(or pf
						    (format
						     nil
						     "~A~A" (or (second tl) "tns") i))
						nur))
				    (push new (xnm-entries map))
				    new))))))))
		 )
	  (typecase e
	    ((member :all) (cond (all)
				 ((setf nsd (xmp-search-all pk pf nur))
				  (return nsd))))
	    (xmp-namespace-map  (multiple-value-setq (nsd stop)
				  (xmp-search-map e
						  :package pk
						  :prefix pf
						  :uri nur))
				(cond (nsd (return nsd))
				      (stop (return (values nil stop)))
				      ))
	    (symbol  (multiple-value-setq (nsd stop)
		       (xmp-search-map e
				       :package pk
				       :prefix pf
				       :uri nur))
		     (cond (nsd (return nsd))
			   (stop (return (values nil stop)))
			   ))
	    (xmp-namespace-declaration (test e)))
	  )))))

(defun xmp-search-all (pk pf nur)
  ;; Returns nil or xmp-namespace-declaration
  (flet ((test (e)
	       (cond (pk (and (xnd-equal-package pk e)
			      (or (null pf) (xnd-equal-prefix pf e))
			      (or (null nur) (xnd-equal-uri pf e))
			      (return-from xmp-search-all e)))
		     (nur (and (xnd-equal-uri nur e)
			       (or (null pf) (xnd-equal-prefix pf e))
			       (return-from xmp-search-all e)))
		     (pf (and (xnd-equal-prefix pf e)
			      (return-from xmp-search-all e))))))
    (maphash #'(lambda (k v)
		 (typecase v
		   ;; check type to avoid the lock entry
		   (xmp-namespace-map
		    (when (setf k (xmp-search-map v
						  :all t
						  :package pk
						  :prefix pf
						  :uri nur))
		      (return-from xmp-search-all k)))))
	     *xmp-namespace-maps*)
    (maphash #'(lambda (k vl) 
		 (declare (ignore k))
		 (when (consp vl)
		   ;; check type to avoid the lock entry
		   (dolist (v vl) (test v))))
	     *xmp-package-to-xnd*)
    (maphash #'(lambda (k vl) 
		 (declare (ignore k))
		 (when (consp vl)
		   ;; check type to avoid the lock entry
		   (dolist (v vl) (test v))))
	     *xmp-uri-to-xnd*)
    ))




(defun nsd-package (nsd)
  (etypecase nsd
    (null nil)
    (cons (etypecase (first nsd)
	    (symbol (find-package (first nsd)))
	    (package (first nsd))))
    (xmp-namespace-declaration (xnd-package nsd))))

(defun nsd-prefix (nsd) 
  (etypecase nsd
    (null nil)
    (cons (or (second nsd) (nsd-prefix (xmp-find-namespace (first nsd) nil (third nsd)))))
    (xmp-namespace-declaration (xnd-prefix nsd))))

(defun nsd-uri (nsd)
  (etypecase nsd
    (null nil)
    (cons (or (uri-to-string (third nsd)) (nsd-uri (xmp-find-namespace (first nsd) (second nsd) nil))))
    (xmp-namespace-declaration (xnd-uri nsd))))

(defun make-nsd (package prefix uri)
  (xmp-make-namespace package prefix uri))

(defun nse-default (xnm)
  (etypecase xnm
    (null nil)
    (xmp-namespace-map (or (xnm-default xnm)
			   (dolist (d (xnm-entries xnm))
			     (typecase d
			       (null nil)
			       ((member :all) nil)
			       ((member :stop) (return nil))
			       ((or xmp-namespace-map symbol)
				(when (setf d (nse-default d))
				  (return d)))))))
    (cons (nse-default (xmp-normal-nse xnm)))
    (symbol (nse-default (gethash xnm *xmp-namespace-maps*)))))

(defun nse-seen-p (d one seen)
  (case one
    (:package
     (member d seen :test #'xnd-equal-package))
    (:prefix  
     (or (null (xnd-prefix d))
	 (member d seen :test #'xnd-equal-prefix)))
    (:uri
     (member d seen :test #'xnd-equal-uri))))

(defun nse-defs (nse &key 
		     more ;;; xmp-namespace-map | (ext... [map])
		     seen ;;; list of xnd
		     one  ;;; :package | :prefix | :uri | :level
		     &aux defs new ent)
  ;; gather all the actual nsd entries
  (when nse 
    (setf defs
	  (mapcan #'(lambda (d &aux d2 d3)
		      (etypecase d
			(null nil)
			(xmp-namespace-declaration
			 (when (not (nse-seen-p d one seen))
			   (push d seen)
			   (list d)))
			((member :all :stop)
			 (case one (:level (list d)) (otherwise nil)))
			(xmp-namespace-map
			 (case one
			   (:level (list d))
			   (otherwise
			    (multiple-value-setq (d2 seen)
			      (nse-defs d :more nil :seen seen :one one))
			    d2)))
			(cons (setf d2 (xmp-normal-nsd d))
			      (when (not (nse-seen-p d2 one seen))
				(push d2 seen)
				(list d2)))
			(symbol (cond
				  ((eq one :level) (list d))
				  (t
				   (or (setq d2 (gethash d *xmp-namespace-maps*))
				       (error "Cannot find namespace map ~S" d))
				   (multiple-value-setq (d3 seen)
				     (nse-defs d2 :more nil :seen seen :one one))
				   d3)))))
		  (etypecase nse
		    (symbol (if (setf ent (gethash nse *xmp-namespace-maps*))
				(xnm-entries ent)
			      (error "Cannot find namespace map ~S" nse)))
		    (xmp-namespace-map (xnm-entries nse))
		    (cons (setf ent (first (last nse)))
			  (if (and (consp ent)
				   (case (third ent) ((:any :other :prefix) t)))
			      (setf ent (butlast (cdr nse)))
			    (setf ent (cdr nse)))
			  ent)))))
  (etypecase more
    (null nil)
    (symbol
     (multiple-value-setq (new seen) (nse-defs more :nore nil :seen seen :one one)))
    (xmp-namespace-map
     (multiple-value-setq (new seen) (nse-defs more :nore nil :seen seen :one one)))
    (cons (multiple-value-setq (new seen) (nse-defs (first more) :more (cdr more)
						    :seen seen :one one))))
  (values (if new (append defs new) defs)
	  seen))

(defun nse-tail (nse)
  (etypecase nse
    (xmp-namespace-map (xnm-tail nse))
    (symbol (nse-tail (gethash nse *xmp-namespace-maps*)))
    (cons (do ((tl (cdr nse) (cdr tl)))
	      ((atom (cdr tl)) (when (consp (first tl))
				 (case (third (first tl))
				   ((:any :other :prefix) (first tl)))))))))
	  

(defun make-nse (default &rest defs)
  ;;(cons default (append defs nil))
  (xmp-define-namespace-map nil nil (cons default defs)))
(defun make-nse* (default &rest defs*)
  (xmp-define-namespace-map
   nil nil 
   (cond ((null defs*) (list default))
	 ((null (cdr defs*)) (cons default (append (first defs*) nil)))
	 (t (append (list default)
		    (butlast defs*)
		    (first (last defs*))
		    nil)))))

(defun nss-top (nss) (first nss))
(defun nss-tail (nss) (cdr nss))
(defun make-nss (top &rest more) (append (list top) more nil))
(defun make-nss* (top &rest more*)
  (cond ((null more*) (list top))
	((null (cdr more*)) (cons top (append (first more*) nil)))
	(t (append (list top)
		   (butlast more*)
		   (first (last more*))
		   nil))))



(defmethod xmp-translate-nss ((conn t) nss)
  (typecase nss
    ((member :in) (when conn (xmp-in-nss conn)))
    ((member :out) (when conn (xmp-out-nss conn)))
    ((member :dns) (when conn (xmp-initial-nss conn nil)))
    ((member nil) nil)
    (cons (make-nss* (xmp-normal-nse (nss-top nss))
		     (xmp-translate-nss conn (nss-tail nss))))
    (otherwise (error "Cannot translate ~S to a namespace environment." nss))))


(defmethod xmp-uri-to-package ((conn t) uri nss &aux e)
  (setf nss (xmp-translate-nss conn nss))
  (dolist (nse nss
	       ;; if we do not find the package in the current environment
	       ;;    look in the defined namespaces too
	       (when (and conn
			  (or (setf e (xmp-search-map (xmp-message-dns conn) :uri uri))
			      (setf e (xmp-search-map (xmp-base-dns conn) :uri uri))))
		 (nsd-package e))
	       )
    (when (setf e (xmp-search-map nse :uri uri))
      (return (nsd-package e)))))

(defmethod xmp-uri-to-prefix ((conn t) uri nss &aux e)
  (setf nss (xmp-translate-nss conn nss))
  (dolist (nse nss)
    (when (setf e (xmp-search-map nse :uri uri))
      (return (nsd-prefix e)))))

(defmethod xmp-prefix-to-package ((conn t) prefix nss &key suppress-default
				  &aux e d)
  (setf nss (xmp-translate-nss conn nss))
  (when (equal prefix "") (setf prefix nil))
  (dolist (nse nss nil)
    ;; Never search for a prefix outside the context of XML document.
    (when (xmp-initial-nss-marker nse) (return nil))
    (if (null prefix)
	(if suppress-default 
	    (return nil)
	  (case (setf d (nse-default nse))
	    (:none (return nil))
	    ((nil) nil)
	    (otherwise (return (xmp-uri-to-package conn d nss)))))
      (when (setf e (xmp-search-map nse :prefix prefix))
	(return (nsd-package e))))))

(defmethod xmp-package-to-prefix ((conn t) pk nss &key suppress-default
				  &aux prefixes e d)
  (setf nss (xmp-translate-nss conn nss))
  (or (packagep pk)
      (when (null pk) (setf pk *package*))
      (when (setf e (find-package pk)) (setf pk e)))
  (dolist (nse nss)
    (or suppress-default d 
	(case (setf d (nse-default nse))
	  (:none (setf d nil))))
    (when (setf e (xmp-search-map nse :package pk))
      (if (eq pk (nsd-package e))
	  (if (and d (same-uri d (nsd-uri e)))
	      (return-from xmp-package-to-prefix (values "" (nsd-prefix e)))
	    (if (member (nsd-prefix e) prefixes :test #'equal)
		(xmp-error 
		 :namespace
		 :string
		 (list
		  "Namespace prefix ~A of package ~A is masked."
		  (nsd-prefix e) (package-name pk)))
	      (return-from xmp-package-to-prefix (nsd-prefix e))))
	(push (nsd-prefix e) prefixes)))))

(defmethod xmp-default-uri ((conn t) nss)
  (setf nss (xmp-translate-nss conn nss))
  (dolist (nse nss)
    (case (nse-default nse)
      (:none (return nil))
      ((nil) nil)
      (otherwise (return (nse-default nse))))))

(defmethod xmp-default-package ((conn xmp-connector) nss &aux e)
  (when (setf e (xmp-default-uri conn nss)) (xmp-uri-to-package conn e nss)))

(defmethod xmp-default-prefix ((conn xmp-connector) nss &aux e)
  (when (setf e (xmp-default-uri conn nss)) (xmp-uri-to-prefix conn e nss)))



(defmethod xmp-decode-qualified-name ((conn t) (data symbol) (nss t)
				      &key suppress-default)
  (declare (ignore suppress-default))
  (values data data))

(defmethod xmp-decode-qualified-name ((conn t) (data string) nss
				      &key suppress-default)
  ;; First value is a symbol if the string can be mapped to a Lisp
  ;;  symbol;  otherwise it is the input string.
  ;; Second value is name that should appear as a complex-part.
  (let* ((end (length data))
	 (qp (position #\: data))
	 name  pk)
    (cond ((or (eql qp 0) (eql qp (1- end)))
	   ;; :xxx or xxx: -- This is not a well-formed name or qname in 
	   ;; XML;  just pass it through.
	   (values data data))
	  ((and (eql qp 3) (string-equal data "xml" :start1 0 :end1 3))
	   ;; xml:xxx -- This is a name in the reserved XML namespace.
	   (setf name (subseq data 4))
	   (values (intern name :net.xmp.xml) name))
	  ((and (< 4 end)
		(string-equal data "xmlns" :start1 0 :end1 5)
		(or (null qp) (eql qp 5)))
	   ;; The is valid syntax for an attribute name declaring a
	   ;; namespace;  anywhere else it is random data.
	   (values data data))
	  ((setf pk (xmp-prefix-to-package
		     conn (when qp (subseq data 0 qp)) nss
		     :suppress-default suppress-default))
	   ;; If the name can be mapped to a Lisp symbol, both values
	   ;;  are that symbol.
	   (setf name (intern (if qp (subseq data (1+ qp)) data) pk))
	   (values name name))
	  ((null qp)
	   (values
	    (intern data (or (and conn (xmp-lisp-package conn)) *package*))
	    data))
	  (t (xmp-error
	      conn :decode 
	      :string (list "Unknown namespace in qualified name ~A" data)))
	  )))


(defmethod xmp-encode-qualified-name ((conn xmp-string-out-connector)
				      (data string) nss &key suppress-default sanitize)
  (declare (ignore suppress-default nss))
  (xmp-encode-content conn data :sanitize sanitize))

(defmethod xmp-encode-qualified-name ((conn xmp-string-out-connector)
				      (data symbol) nss &key suppress-default sanitize)
  (let* ((pk (symbol-package data))
	 prefix)
    (when (and (setf prefix (xmp-package-to-prefix
			     conn pk nss :suppress-default suppress-default))
	       (not (equal prefix "")))
	(xmp-encode-content conn prefix)
	(xmp-encode-content conn ":" :sanitize nil))
    (xmp-encode-content conn (symbol-name data) :sanitize sanitize)))

(defmethod xmp-encode-qualified-name ((conn xmp-connector)
				      (data t) nss &key suppress-default sanitize)
  (xmp-encode-content
   conn (xmp-encoded-qualified-name
	 conn data nss :suppress-default suppress-default :sanitize sanitize)))

(defmethod xmp-encoded-qualified-name ((conn xmp-connector) (data string) nss
				       &key suppress-default sanitize)
  (declare (ignore suppress-default nss))
  (if sanitize
      (xmp-content-string conn data :sanitize sanitize)
    data))

(defmethod xmp-encoded-qualified-name ((conn xmp-connector) (data symbol) nss
				       &key suppress-default sanitize)
  (xmp-content-string
   conn 
   (let* ((pk (symbol-package data))
	  prefix)
     (if (and (setf prefix (xmp-package-to-prefix
			    conn pk nss :suppress-default suppress-default))
	      (not (equal prefix "")))
	 (concatenate 'string prefix ":" (symbol-name data))
       (symbol-name data)))
   :sanitize sanitize
   ))

	  
(defun xmp-normal-nse (nse &key (new t))
  (etypecase nse
    (null (when (eq new :always) (xmp-define-namespace-map nil nil nil)))
    (xmp-namespace-map nse)
    (symbol (or (gethash nse *xmp-namespace-maps*)
		(error "Cannot find namespace-map ~S" nse)))
    (cons (if new
	      (xmp-define-namespace-map nil nil nse)
	    (error "Expected a namespace map instance only.")
	    ))))



(defun xmp-normal-nsd (nsd &optional test-only &aux pk pf ur)
  (setf pk (nsd-package nsd))
  (setf pf (nsd-prefix nsd))
  (setf ur (nsd-uri nsd))
  (or (and (typecase pk (null t) (package t))
	   (typecase pf ((or null string symbol) t))
	   (typecase ur ((or null string) t))
	   (if test-only
	       nsd
	     (typecase nsd
	       (xmp-namespace-declaration nsd)
	       (otherwise nil))))
      (if test-only
	  nil
	(make-nsd
	 (etypecase pk
	   (package pk)
	   (null (typecase nsd
		   (symbol (setf pk nsd))
		   (cons (setf pk (first pk))))
		 (when pk
		   (error "Cannot find-package in NameSpace spec ~S" nsd))
		 nil))
	 pf
	 ur))))
	
(defmethod xmp-encode-attribute ((conn xmp-string-out-connector)
				 prefix suffix name value qname)
  (xmp-encode-content conn " " :sanitize nil)
  (when prefix (xmp-encode-content conn prefix :sanitize nil))
  (when suffix (xmp-encode-content conn suffix :sanitize t))
  (when name (xmp-encode-qualified-name
	      conn name :out
	      :suppress-default t   ;;; default doas not apply to attr name [bug15443]
	      :sanitize t))
  (xmp-encode-content conn "=" :sanitize nil)
  (xmp-encode-string conn 
		     (if (and qname value (symbolp value))
			 (xmp-encoded-qualified-name
			  conn value :out
			  :suppress-default t  ;;; default doas not apply to attr val
			  )
		       value)
		     :sanitize t))


(defmethod xmp-encode-begin ((conn xmp-string-out-connector) data &rest options 
			      &key namespaces attributes empty dns &allow-other-keys
			      &aux default)
  (declare (ignore options))
  (push (setf namespaces (xmp-normal-nse namespaces)) (xmp-out-nss conn))
  (xmp-encode-content conn "<" :sanitize nil)
  (xmp-encode-qualified-name conn data :out)
  (when (or (setf default (nse-default namespaces))
	    (when dns (setf default (nse-default (xmp-message-dns conn)))))
    (xmp-encode-attribute conn "xmlns" nil nil
			  (case default
			    (:none "")
			    (otherwise default))
			  nil))
  (dolist (nsd (nse-defs namespaces :more (when dns (xmp-out-nss conn)) :one :prefix))
    (when (nsd-prefix nsd)
      (xmp-encode-attribute conn "xmlns:" (nsd-prefix nsd) nil (nsd-uri nsd) nil)))
  (do ((tl attributes (cddr tl)))
      ((atom tl))
    (xmp-encode-attribute conn nil nil (first tl) (second tl) t))
  (cond (empty
	 (xmp-encode-content conn " />" :sanitize nil)
	 (pop (xmp-out-nss conn)))
	(t (xmp-encode-content conn ">" :sanitize nil))))


(defmethod xmp-encode-string ((conn xmp-string-out-connector) data 
			      &key sanitize)
  (let (del
	(string (typecase data
		  (string data)
		  (symbol (symbol-name data))
		  (otherwise (format nil "~A" data))))
	)
    (cond ((null (position #\" string)) (setf del "\""))
	  ((null (position #\' string)) (setf del "\'"))
	  (t (xmp-error 
	      conn :encode :string "String contains both single and double quotes.")))
    (xmp-encode-content conn del)
    (xmp-encode-content conn string :sanitize sanitize)
    (xmp-encode-content conn del)))


(defmethod xmp-encode-end ((conn xmp-string-out-connector) data &rest options 
			      &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content conn "</" :sanitize nil)
  (xmp-encode-qualified-name conn data :out)
  (xmp-encode-content conn ">" :sanitize nil)
  (pop (xmp-out-nss conn)))





(defun remove-keys (keys list &aux keep)
  (do ((tl list (cddr tl)))
      ((atom tl) (nreverse keep))
    (when (not (member (first tl) keys))
      (push (first tl) keep)
      (push (second tl) keep))))

(defvar *xmp-warning-leader* "XMP")
(defmethod xmp-warning-leader ((conn t)) *xmp-warning-leader*)
(defmethod xmp-warning-leader ((conn string)) conn)

(define-condition xmp-warning (simple-warning) ())   ;;; [rfe6173]
(define-condition xmp-syntax (xmp-warning) ())
(define-condition xmp-redefinition (xmp-warning) ())
(defvar *xmp-warning-class* 'xmp-warning)
(defmethod xmp-warning-class ((conn t)) *xmp-warning-class*)
(defmethod xmp-warning-class ((conn null)) *xmp-warning-class*)
(defmethod xmp-warning-class ((conn (eql nil))) *xmp-warning-class*)
(defmethod xmp-warning-class ((conn symbol)) conn)

(defmethod xmp-class-warning ((conn t) *xmp-warning-class* &rest format-args)
  (apply #'xmp-warning conn format-args))

(defmethod xmp-warning ((conn t) &rest format-args)
  (warn (xmp-warning-class conn)
	:format-control "; ~A warning: ~A~%"
	:format-arguments (list (xmp-warning-leader conn)
				(apply 'format nil format-args))))

(defmethod xmp-error ((conn t) code-or-class &rest options
		      &key code sub-code string factor detail &allow-other-keys
		      &aux code-to-use class)
  (typecase string
    ((member nil) nil)
    (string nil)
    (cons (setf string (apply 'format nil string)))
    (otherwise (setf string (format nil "~A" string))))
  (typecase factor
    ((member nil) nil)
    (string nil)
    (cons (setf factor (apply 'format nil factor)))
    (otherwise (setf factor (format nil "~A" factor))))
  (case code-or-class
    (:client (setf class 'xmp-client-condition code-to-use :client))
    (:server (setf class 'xmp-server-condition code-to-use :server))
    (otherwise 
     (if (find-class code-or-class nil)
	 (setf class code-or-class code-to-use code)
       (setf class 'xmp-condition code-to-use code-or-class))))
  (apply 'error
	 class
	 :sub-code sub-code :string string :factor factor
	 :detail detail
	 (append
	  (when code-to-use (list :code code-to-use))
	  (remove-keys '(:sub-code :string :factor :detail) options))))
	 

(defun resolve-package (pk)
  (cond ((packagep pk) pk)
	((find-package pk))
	(t *package*)))

(defun xmp-resolve-uri (uri &optional uri-instance)
  (etypecase uri
    (net.uri:uri (case uri-instance
		   (:path (net.uri:uri-path uri))
		   ((nil) (format nil "~A" uri))
		   (otherwise uri)))
    (string     (case uri-instance
		  ((nil) uri)
		  (otherwise (or (ignore-errors
				   (let ((x (net.uri:parse-uri uri)))
				     (case uri-instance
				       (:path (net.uri:uri-path x))
				       (otherwise x))))
				 uri))))))

(defvar *xmp-initial-nss-marker* (xmp-define-namespace-map nil nil nil))
(defmethod xmp-initial-nss-marker (nse) (eq nse  *xmp-initial-nss-marker*))
(defmethod xmp-initial-nss ((conn xmp-connector) namespaces)
  (let* ((dns (xmp-normal-nse (xmp-message-dns conn)))
	 (base (xmp-normal-nse (xmp-base-dns conn))))
    (list *xmp-initial-nss-marker* (xmp-normal-nse namespaces) dns base)))

(defmethod xmp-parse-message ((conn xmp-connector) source &key namespaces) 
  (let* ((nss (xmp-initial-nss conn namespaces))
	 (pk (or (xmp-default-package conn nss) (xmp-lisp-package conn)))
	 (*package* (resolve-package pk)))
    (multiple-value-bind (xml ns)
	(net.xml.sax:parse-to-lxml	     
	 source
	 :content-only t
	 :uri-to-package
	 (mapcar #'(lambda (d)
		     (cons (xmp-resolve-uri
			    (let ((uri (nsd-uri d)))
			      (or (cond ((null uri) nil)
					((equal uri "") nil)
					(t uri))
				  (error "Cannot use namespace declaration ~A"
					 d))))
			   (resolve-package 
			    (let ((pk (nsd-package d)))
			      (or pk
				  (error "Cannot use namespace declaration ~A"
					 d))))))
		 (nse-defs nil :more nss :one :uri))
	 )
      (setf (xmp-message-pns conn) ns)
      xml)))

(defmethod xmp-parse-message :around ((conn xmp-connector) (string t) &key namespaces) 
  (declare (ignore namespaces))
  (let ((xml (call-next-method)))
    (when (xmp-debug conn) (setf (xmp-message-xml conn) xml))
    xml))
 

(defun encode-base64-string (arg &aux 
				 (string (typecase arg
					   (string arg)
					   (array arg)
					   (otherwise (format nil "~A" arg))))
				 (len (length string)) 
				 out)
  (setf out (make-string (* 4 (ceiling len 3))))
  (do ((i 0 (+ i 3))
       (d 0 (+ d 4))
       (int 
	;; init to guard bits in high order pos, 
	;; converts to leading BAAA characters
	#x040000 #x040000) 
       res k
       )
      ((<= len i)
       out)
    (dotimes (j 3)
      (setf k (+ i j))
      (if (< k len)
	  (setf int (+ (ash int 8)
		       (typecase string
			 (string (char-int (elt string k)))
			 (array (logand #xff (truncate (aref string k)))))))))
    (setf res (excl::integer-to-base64-string int 48))
    (dotimes (n 4)
      (setf (elt out (+ d n)) (elt res (+ 4 n))))))

(defun decode-base64-string (string)
  (do ((i 0)
       (len (length string))
       int res j k p s
       )
      ((<= len i)
       (concatenate '(array (unsigned-byte 8)) (nreverse res)))
    (setf j i k 0 p 0)
    (loop 
     (when (or (eql k 4) (eql j len) (eql p 2)) (return))
     (case (excl::base64-digit-char (elt string j))
       (-1 (incf j))
       (-2 (incf j) (incf k) (incf p))
       (otherwise (incf j) (incf k))))
    (setf s (subseq string i j))
    (setf i j)
    ;;(format t "~&k=~S~%" k)
    (case k
      (4 nil)
      (3 (case p
	   (0 (setf p 1 s (concatenate 'string s "=")))
	   (1 (setf p 2 s (concatenate 'string s "=")))
	   (2 nil)))
      (2 (case p
	   (0 (setf p 2 s (concatenate 'string s "==")))
	   (1 (setf p 2 s (concatenate 'string s "=")))
	   (2 nil)))
      (1 (case p
	   (0 (setf p 2 s (concatenate 'string s "==")))
	   (1 (setf k 0)))))
    (case k
      (0 nil)
      (otherwise (setf int (excl::base64-string-to-integer s))
		 (case p
		   (2 (push (logand #xff int) res))
		   (1 (push (ash (logand #xff00 int) -8) res)
		      (push (logand #xff int) res))
		   (0 (push (ash (logand #xff0000 int) -16) res)
		      (push (ash (logand #xff00 int) -8) res)
		      (push (logand #xff int) res)))))
    ))







(defmethod xmp-enable-server ((server xmp-server-connector)
			      &key enable-exports &allow-other-keys)
  (mp:with-process-lock 
   ((xmp-server-lock server))
   (setf (xmp-server-enabled server) t)
   (when enable-exports
     (let ((tables (xmp-server-exports server)))
       (dotimes (i 3)
	 (maphash #'(lambda (k v)
		      (declare (ignore v))
		      (xmp-enable-method server k :all))
		  (aref tables i)))))
   ;; return server object
   server))

(defgeneric xmp-server-response ((server xmp-server-connector) &key &allow-other-keys)
  ;; called when a message arrives at a server
  )
	


(defmethod xmp-disable-server ((server xmp-server-connector)
			      &key disable-exports &allow-other-keys)
  (mp:with-process-lock 
   ((xmp-server-lock server))
   (setf (xmp-server-enabled server) nil)
   (when disable-exports
     (let ((tables (xmp-server-exports server)))
       (dotimes (i 3)
	 (maphash #'(lambda (k v)
		      (declare (ignore v))
		      (xmp-disable-method server k :all))
		  (aref tables i)))))
   ;; return server object
   server))

(defmethod xmp-signature-equal :around ((conn t) x y)
  (or (equal x y) (call-next-method)))

(defmethod xmp-signature-equal ((conn t) s1 s2)
  (and (consp s1) (consp s2) (eql (length s1) (length s2))
       (mapc #'(lambda (x y)
		 (or (xmp-signature-equal conn x y)
		     (return-from xmp-signature-equal nil)))
	     s1 s2)))

(defmethod xmp-export-method ((conn xmp-server-connector) name sig
			      &key lisp-name (enable t) return help properties
			      &allow-other-keys)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (xmp-lookup table name name nil))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  (entry (assoc sig item :test sig=))
	  )
     (or entry
	 (xmp-lookup
	  table name name nil
	  :value (setf item
		       ;; add the new item at the end so lookup is
		       ;;  in the same order as exports
		       (nconc item (list (setf entry (list sig)))))))
     
     ;; export-table-entry
     ;;   -> (sig return-type lisp-name enabled help-string . properties)

     (setf (cdr entry) (list* (xmp-normal-element-spec conn return :dns)
			      (or lisp-name (intern name))
			      enable
			      help
			      properties))
     name)))

(defmethod xmp-enable-method ((conn xmp-server-connector) name sig)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (xmp-lookup table name name nil))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  entry)
     (when item
       (if (eq sig :all)
	   (dolist (e item name) (setf (fourth e) t))
	 (when (setf entry (assoc sig entry :test sig=))
	   (setf (fourth entry) t)
	   name))))))

(defmethod xmp-disable-method ((conn xmp-server-connector) name sig)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (xmp-lookup table name name nil))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  entry)
     (when item
       (if (eq sig :all)
	   (dolist (e item name) (setf (fourth e) nil))
	 (when (setf entry (assoc sig entry :test sig=))
	   (setf (fourth entry) nil)
	   name))))))

(defmethod xmp-accept-method ((conn xmp-server-connector) name sig (args t))
  ;; return zero values if method is not accepted
  ;; return three values if accepted
  ;;        - lisp-name, return-type, and (possibly modified) arglist
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  (item (xmp-lookup table name name nil))
	  (entry (assoc sig item :test sig=)))
     (if (and entry (fourth entry))
	 (let* ((ldef (third entry))
		(lname (if (consp ldef)
			   (first ldef)
			 ldef))
		(keys (when (consp ldef) (cdr ldef)))
		(ret (second entry))
		(elts (cddr (first entry)))
		)
	   (when keys   ;;; [bug16269]
	     (do ((tl args (cddr tl)) arg r)
		 ((atom tl) (setf args (reverse r)))
	       (setf arg (string (first tl)))
	       (block match
		 (mapc #'(lambda (key elt)
			   (when (xmp-match-name conn arg elt)
			     (push key r) (push (second tl) r) (return-from match)))
		       keys elts))
	       ))
	   (values lname ret args))
       (values)))))

(defmethod xmp-list-methods ((conn xmp-server-connector))
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  all)
     (maphash #'(lambda (k v) (declare (ignore v)) (push k all)) (aref table 0))
     (maphash #'(lambda (k v) (declare (ignore v)) (push k all)) (aref table 1))
     (maphash #'(lambda (k v) (declare (ignore v)) (push k all)) (aref table 2))
     all)))

(defmethod xmp-method-signature ((conn xmp-server-connector) name)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (xmp-lookup table name name nil))
	  all)
     (dolist (entry item)
       (push (cons (second entry) (first entry)) all))
     all)))

(defmethod xmp-method-help ((conn xmp-server-connector) name)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (xmp-lookup table name name nil)))
     (dolist (entry item "")
       (when (fifth entry) (return (fifth entry)))))))

(defmethod xmp-method-prop ((conn xmp-server-connector) name sig prop)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  (item (xmp-lookup table name name nil))
	  (entry (assoc sig item :test sig=)))
     (getf (nthcdr 5 entry) prop))))


(defmethod xmp-invoke-method :around ((server xmp-server-connector) (name t) (args t))
  (let ((*xmp-server* server))
    (call-next-method)))

(defmethod xmp-invoke-method ((server xmp-server-connector) name args)
  (apply name args))

(defmethod xmp-struct-type ((conn t)) (xmp-symbol "struct" :keyword))

(defmethod xmp-slot-value ((conn t) (struct cons) member)
  (if (eq (xmp-struct-type conn) (car struct))
      (second (assoc member (cdr struct) :test #'equal))
    (call-next-method)))

(defgeneric xmp-server-implementation (conn body &rest options &key &allow-other-keys))

(defmethod xmp-run-one-file ((conn xmp-server-connector) from)
  ;; Run one server cycle with XML input from a file
  (with-open-file 
   (s from)
   (xmp-server-implementation conn s)))

(defmethod xmp-parse-file ((conn xmp-connector) file)
  (with-open-file
   (s file)
   (xmp-parse-message conn s)))

(defmethod xmp-decode-file ((conn xmp-connector) file)
  (xmp-decode-message conn (xmp-parse-file conn file)))

(defmethod xmp-decode-string ((conn xmp-connector) string)
  (xmp-decode-message conn (xmp-parse-message conn string)))

(defmethod xmp-decode-stream ((conn xmp-connector) stream)
  (xmp-decode-message conn (xmp-parse-message conn stream)))




;;; COMPLEX-DEF-INSTANCE

(defclass xmp-complex-def-instance ()
  ((complex-def :accessor xmp-cdi-def :initarg :def)
   (state       :accessor xmp-cdi-state :initarg :state :initform nil)
   (found       :accessor xmp-cdi-found :initform nil)
   (stack       :accessor xmp-cdi-stack :initform nil)
   (inner-def
    ;; definition of most recent element match in this instance
    ;; used in recursive descent
    :accessor xmp-cdi-inner :initform nil)
   ))

(defmethod xmp-new-complex-def ((conn t) cd)
  (cond ((atom cd) cd)
	((xmp-any-cpart conn cd) cd)
	(t (make-instance 'xmp-complex-def-instance
			  :def cd
			  :state (case (first cd)
				   ((:seq1 :seq+ :set1 :set+)
				    (when (cdr cd) :incomplete)))
			  ))))

(defmethod xmp-test-complex-def ((conn t) (cdi xmp-complex-def-instance) elt)
  (xmp-match-complex-part conn cdi
			  (xmp-cdi-def cdi)
			  (append (xmp-cdi-found cdi) (when elt (list elt)))))

(defmethod xmp-step-complex-def ((conn t) (cdi xmp-complex-def-instance) elt)
  (setf (xmp-cdi-found cdi)
	(append (xmp-cdi-found cdi) (list elt))))

(defmethod xmp-match-complex-part ((conn t) cdi part found)
  (case (xmp-match-element-def conn cdi part (first found))
    (:match t)
    (:complex 
     (xmp-match-complex-tail
      conn cdi (xmp-top-collector conn part) (xmp-top-parts conn  part) found))
    (otherwise nil)))

(defmethod xmp-top-collector ((conn t) part) (first part))
(defmethod xmp-top-collector ((conn xmp-connector) part)
  (if (consp part)
      (case (first part)
	(:seq?
	 (case (xmp-xml-syntax conn)
	   (:strict :seq)
	   (otherwise :set*)))
	(otherwise (first part))
	)
    part))

(defmethod xmp-top-parts     ((conn t) part) (cdr part))
(defmethod xmp-top-parts ((conn xmp-connector) part)
  (if (consp part)
      (case (first part)
	(:seq?
	 (let ((maybe (dolist (p (cdr part) nil)
			(and (consp p) (eq :maybe (first p)) (return p))))
	       (strict (eq :strict (xmp-xml-syntax conn)))
	       )
	   (if maybe
	       (mapcan #'(lambda (p)
			   (if (and (consp p) (eq :maybe (first p)))
			       (if strict nil (append (cdr p) nil))
			     (list p)))
		       (cdr part))
	     (cdr part))))
	(otherwise (cdr part))
	)
    part))

;;(eval-when (compile load eval) (pushnew :xmp-debug *features*))
;;;

(defmethod xmp-match-complex-tail ((conn t) cdi coll tail found)
  (let* ((stack (xmp-cdi-stack cdi))
	 (top (pop stack))
	 (top-coll (if top
		       (first top)
		     coll))
	 (top-tail (if top
		       (second top)
		     tail))
	 (flength (length found))
	 (fplace (or (when top (third top)) 0))
	 (matched (if top (fourth top) nil))
	 (index (fifth top))
	 (res :new)
	 new-coll new-tail set-state save-place)

    (flet ((match-part (part)
		       (case (xmp-match-element-def conn cdi part (nth fplace found))
			 (:match (values :match nil nil))
			 (:complex 
			  (values :push (first part) (cdr part)))
			 (otherwise nil)))
	   (push-state ()
		       (push (list top-coll top-tail save-place matched index set-state)
			     stack))
	   (pop-state ()
		      (multiple-value-setq
			  (top-coll top-tail save-place matched index set-state)
			(values-list (pop stack)))
		      (when save-place (setf fplace save-place))
		      (setf save-place nil))
	   )

      #+xmp-debug
      (when (xmp-debug conn)
	(format t
		"~&;BEGIN coll=~S tail=~S ~%;      fplace=~S found=~S~%"
		coll tail fplace found))
	
      (setf
       res
       (loop
	
	#+xmp-debug
	(when (xmp-debug conn)
	  (format t "~&;TOP ~S top-coll=~S top-tail=~S ~%" res top-coll top-tail)
	  (format t ";      fplace=~S matched=~S index=~S~%"
		  fplace matched index))
	(setf save-place nil)
	(ecase top-coll

	  ((:seq :seq*)
	   (cond ((eql fplace flength)
		  ;; If all the elements are used up, 
		  ;;    this pattern remains on the stack.
		  (setf res :done))
		 (t 
		  (do ((tl top-tail (cdr tl)))
		      ((atom tl)
		       ;; If we run out of sequence parts,
		       ;;    this pattern is finished.
		       (setf res :pop))
		    (multiple-value-setq (res new-coll new-tail)
		      (match-part (first tl)))
		    (when res
		      (case top-coll
			(:seq* (setf top-tail tl))
			(:seq  (setf top-tail (cdr tl))))
		      (return))))))

	  (:set	     
	   (if (eql fplace flength)
	       (setf res :done)
	     (do ((tl top-tail (cdr tl)) part)
		 ((atom tl) (setf res :pop))
	       (setf part (car tl))
	       (multiple-value-setq (res new-coll new-tail)
		 (match-part part))
	       (when res
		 (setf top-tail (remove part top-tail :count 1))
		 (return)))))
	  (:set*
	   (if (eql fplace flength)
	       (setf res :done)
	     (do ((tl (case res
			((:pop nil) (if (eql index fplace)
					;; complex part failed
					;; try the next match
					matched
				      ;; otherwise look at whole set again
				      top-tail))
			(otherwise top-tail))
		      (cdr tl))
		  part)
		 ((atom tl) (setf res :pop))
	       (setf part (car tl))
	       (multiple-value-setq (res new-coll new-tail)
		 (match-part part))
	       (when res
		 (case res (:push
			    ;; remember where we stopped at this level
			    (setf index fplace matched (cdr tl))))
		 (return)))))

	  (:seq1
	   ;; index is count of items already matched
	   (or index (setf index 0))
	   (case res (:pop (incf index)))
	   (cond ((null res) 
		  ;; If last complex-part failed, propagate the failure.
		  nil)
		 ((eql fplace flength)
		  (if (or (null index) (< index (length (cdr (xmp-cdi-def cdi)))))
		      (setf set-state :incomplete)
		    (setf set-state nil))
		  (setf res :done))
		 ((null top-tail) (setf res :pop))
		 (t (multiple-value-setq (res new-coll new-tail)
		      (match-part (first top-tail)))
		    (when res
		      (case res (:match (incf index)))
		      (setf top-tail (cdr top-tail))))))
	  (:seq+
	   ;; index is count of items already matched
	   (when res (if index (incf index) (setf index 0)))
	   (cond ((null res) (when matched
			       (setf top-tail (cdr top-tail)
				     matched nil
				     res :step)))
		 ((eq res :pop)
		  (setf matched t res :step))
		 ((eql fplace flength)
		  (if (or (null index) (< index (length (cdr (xmp-cdi-def cdi)))))
		    (setf set-state :incomplete)
		    (setf set-state nil))
		  (setf res :done))
		 ((null top-tail) (setf res :pop))
		 (t (multiple-value-setq (res new-coll new-tail)
		      (match-part (first top-tail)))
		    (case res
		      (:match (setf matched t))
		      ((nil) (when matched
			       (setf top-tail (cdr top-tail)
				     matched nil
				     res :step)))))))

	  ((:set1 :set+)
	   (when (null matched)
	     (setf matched (mapcar #'(lambda (x) (declare (ignore x)) nil) top-tail)))
	   (cond ((null res))
		 ((eq res :pop)
		  (setf (nth index matched) index)
		  (setf res :step))
		 ((eql fplace flength)
		  (if (member nil matched)
		      (setf set-state :incomplete)
		    (setf set-state nil))
		  (setf res :done))
		 ((null top-tail) (setf res :pop))
		 (t (let ((i 0))
		      (dolist (part top-tail
				    (if (member nil matched)
					(setf res nil)
				      (setf res :pop)))
			(when (case top-coll
				(:set1 (not (member i matched)))
				(:set+ t))
			  (multiple-value-setq (res new-coll new-tail)
			    (match-part part))
			  (case res
			    (:push  (setf index i))
			    (:match (setf (nth i matched) i)))
			  (when res (return)))
			(incf i))))))

	  (:or
	   (cond ((null res) (pop top-tail) (setf res :step))
		 ((eq res :pop)
		  ;; Once a clause is satisfied, resume the next
		  ;;  pattern off the stack.
		  (setf top-tail nil))
		 ((eql fplace flength)
		  (setf set-state :incomplete)
		  (setf res :done))
		 ((null top-tail) (setf res nil))
		 (t (multiple-value-setq (res new-coll new-tail)
		      (match-part (first top-tail)))
		    (ecase res
		      ((nil) (pop top-tail) (setf res :step))
		      (:match (setf top-tail (list (first top-tail)))
			      (incf fplace)
			      (setf res :pop)
			      )
		      (:push (setf save-place fplace))
		      ))
		 ))
		  
	  )

	#+xmp-debug
	(when (xmp-debug conn)
	  (format
	   t 
	   "~&;BOT ~S top-coll=~S top-tail=~S ~%;     fplace=~S matched=~S~%"
	   res top-coll top-tail fplace matched)
	  (format t ";     new-coll=~S new-tail=~S~%" new-coll new-tail))

	(ecase res
	  (:push   (push-state)
		   (setf top-coll new-coll
			 top-tail new-tail
			 matched nil
			 ))
	  (:match (incf fplace))
	  (:step  nil)
	  (:done
	   ;; We have run out of elements to match, so save the state
	   ;;  of the matching up to now, and return success.
	   (setf save-place fplace)
	   (push-state)
	   (return t))
	  (:pop
	   (cond (stack (pop-state))
		 ((< fplace flength)
		  ;; We have run out of pattern, but there are elements
		  ;;  left to match;  this is failure.
		  (return nil))
		 (t (setf save-place fplace)
		    (push-state)
		    (return t))))
	  ((nil)
	   (cond (stack (pop-state))
		 (t (return nil))))
	  )
	))
      (setf (xmp-cdi-stack cdi) stack
	    (xmp-cdi-state cdi) set-state)

      #+xmp-debug
      (when (xmp-debug conn)
	(format t "~&;EXIT ~S state=~S stack-top=~S ~%" res set-state (first stack)))
 
      res)))

(defmethod xmp-getf ((conn t) plist pattern &optional default)
  (do ((tl plist (cddr tl)))
      ((atom tl) default)
    (when (xmp-match-name conn (first tl) pattern)
      (return (second tl)))))

(defmethod xmp-mem2 ((conn t) plist pattern)
  (do ((tl plist (cddr tl)))
      ((atom tl) nil)
    (when (xmp-match-name conn (first tl) pattern)
      (return tl))))

(defmethod xmp-assoc ((conn t) pattern alist)
  (do ((tl alist (cdr tl)))
      ((atom tl) nil)
    (when (and (consp (first tl)) (xmp-match-name conn (first (first tl)) pattern))
      (return (first tl)))))


(defmethod xmp-match-name ((conn t) target pattern &optional ignore-case)
  (cond ((null target) nil)
	((null pattern) t)
	((eq target pattern) t)
	(t (flet ((match (target pat ignore-case)
			 (or (null pat)
			     (let ((test (if ignore-case #'string-equal #'equal)))
			       (typecase pat
				 (symbol
				  ;; if pattern is a symbol
				  ;;    then only an exact match will do
				  (setf test #'eq))
				 (cons
				  ;; must be (:any-case string)
				  (setf pat (second pat)
					test #'string-equal)))
			       (typecase target
				 (symbol (typecase pat
					   (string (setf target (symbol-name target))))))
			       (funcall test target pat)))))
	     (typecase pattern
	       (string (match target pattern ignore-case))
	       (symbol (eq target pattern))
	       (cons (cond ((equal pattern '(:any)) t)
			   ((and (eq :any-case (first pattern))
				 (stringp (second pattern))
				 (null (cddr pattern)))
			    (match target (second pattern) t))
			   (t (dolist (pat pattern nil)
				(when (match target pat ignore-case)
				  (return t)))))))))))
	       
(defmethod xmp-match-element-def ((conn t) cdi eldef elt &aux type)
  (setf (xmp-cdi-inner cdi) nil)
  (cond ((null eldef) nil)
	((or (symbolp eldef) (stringp eldef))
	 (cond ((xmp-match-name conn elt eldef) :match)
	       ((null (setf type (xmp-find-element conn eldef :in))) nil)
	       ((and (consp type) (eq :complex (first type))
		     (consp (setf type (second type)))
		     (eq :or (pop type))
		     (or (atom (first type))
			 (eq :element (first (first type)))))
		(dolist (sub type)
		  (if (atom sub)
		      (when (xmp-match-name conn elt sub)
			(return (values :match sub)))
		    (and (eq :element (first sub))
			 (xmp-match-name conn elt (second sub))
			 (return (values :match sub))))))))
	(t (case (first eldef)
	     (:any (when elt :match))
	     (:element (when (xmp-match-name conn elt (second eldef))
			 (setf (xmp-cdi-inner cdi) eldef)
			 :match))
	     (otherwise :complex)))))

			     
(defmethod xmp-simple-exel ((conn t) sdef)
  (typecase sdef
    (cons (case (first sdef)
	    (:simple (or (second sdef)
			 (getf (cddr sdef) :simple-content-key)))))))

(defmethod xmp-element-exdef ((conn t) eldef nss &aux ex tp td ar)
  ;; return three values:   
  ;;        - expected-element
  ;;        - the input element name 
  ;;              or the last type name in the chain
  ;;                     leading up to expected-element
  ;;        - an array spec if inside an array
  (typecase eldef
    ((member nil) nil)
    ((or string symbol)
     (cond ((setf td (xmp-find-type conn eldef nss))
	    (multiple-value-setq (ex tp ar)
	      (xmp-element-exdef conn td nss))
	    (values ex (or tp eldef) ar))
	   (t (values nil eldef))))
    (cons (case (first eldef)
	    (:element (xmp-element-exdef conn (third eldef) nss))
	    (:simple 
	     (multiple-value-setq (ex tp) 
	       (xmp-element-exdef conn (second eldef) nss))
	     (or ex
		 (setq ex (xmp-simple-exel conn eldef) tp nil))
	     (values ex tp))
	    (:complex (second eldef))
	    (:array (values `(:seq* (:element nil ,(second eldef))) nil eldef))
	    ))))



(defun xmp-lookup (table dname dstring name4 
			 &key attribute (attributes nil a-p) (value nil v-p)
			 delete default
			 &aux found (not-found t) key edef idef item plist
			 (not '#:not) any ht)
  ;; attributes -> :query            ; return list  (name value ... )
  ;;            -> (name value ... ) ; update existing attribute list
  ;;            -> nil               ; drop all attributes
  ;; attribute  -> symbol            ; get this attribute value
  ;;            -> (name value)      ; update one attribute value
  ;; default  ; default value for get attribute

  ;; NOTE: attribute should only be stored in element definition tables.
  ;;       NOT in export tables.

  (if name4
      (cond (delete (remhash name4 (elt table 3)))
	    (t (let* ((old (gethash name4 (elt table 3)))
		      (tlist (and (consp old) (eq :type (car old)))))
		 (when v-p
		   (if tlist
		       (setf (second old) value)
		     (setf (gethash name4 (elt table 3)) (setf old value))))
		 (cond ((consp attributes)
			(cond (tlist (setf (cddr old) attributes))
			      ((consp old) (error "Adding attributes to type"))
			      (t (setf (gethash name4 (elt table 3))
				       (list* :type old attributes)))))
		       ((eq attributes :query))
		       ((and a-p (null attributes) tlist)
			(setf (gethash name4 (elt table 3)) (second old))))
		 (cond (v-p value)
		       ((eq attributes :query) (when tlist (cddr old)))
		       (a-p attributes)
		       (attribute (if tlist
				      (getf (cddr old) attribute default)
				    default))
		       (tlist (second old))
		       (t old)))))
    (let ()
      (or (when (and dname (symbolp dname))
	    (setf item (gethash (setf key dname) (setf ht (elt table 0)) not))
	    (when (not (eq item not))
	      (setf not-found nil found dname)))
	  (when (typecase dstring
		  ((member nil) nil)
		  (symbol (setf dstring (symbol-name dstring)))
		  (string t))
	    (setf item (gethash (setf key dstring) (setf ht (elt table 1)) not))
	    (when (not (eq item not))
	      (setf not-found nil found dstring)))
	  (when (typecase dstring
		  ((member nil) nil)
		  (symbol (setf dstring (symbol-name dstring)))
		  (string t)
		  (cons (ecase (first dstring)
			  (:any-case (setf dstring (second dstring))))))
	    (setf item (gethash (setf key dstring) (setf ht (elt table 2)) not))
	    (when (not (eq item not))
	      (setf not-found nil found dstring any :any))))
      (setf idef item)
      (and found (consp item) (eq :element (first item))
	   (setf idef (third item) edef item plist (cdddr item)))
      (cond (delete)		      
	    ((and (null v-p) (null a-p))
	     (cond ((null attribute) nil)
		   ((or (atom attribute)
			(and (consp attribute) (eq :any-case (first attribute))))
		    (cond (not-found)
			  (edef
			   (setf idef (xmp-getf nil plist attribute default)))
			  (t (setf idef default))))
		   ((consp attribute)
		    (cond ((eq idef not)
			   (setf v-p t
				 value (list :element
					     (list key)
					     nil
					     (first attribute) (second attribute))))
			  (edef (if (setf edef
					  (xmp-mem2 nil plist (first attribute)))
				    (setf (second edef) (second attribute)
					  v-p t value item)
				  (setf (cdddr item)
					(setf plist
					      (list* (first attribute) (second attribute)
						     (cdddr item)))
					v-p t value item)))
			  (t (setf v-p t
				   value (list :element (list key) item
					       (first attribute) (second attribute))))))
		   (t (error "Ill-formed attribute ~S" attribute))))
	    ((and (null v-p) (eq attributes :query))
	     (setf idef (when edef plist)))
	    ((and (null v-p) a-p (null attributes))
	     (if edef
		 (setf (cdddr edef) nil plist nil idef nil)
	       (setf idef nil)))
	    (v-p (when not-found
		   (cond ((and dname (symbolp dname))
			  (setf ht (elt table 0) key dname))
			 ((null dstring) (setf ht nil))
			 (any   (setf ht (elt table 2) key dstring))
			 (t     (setf ht (elt table 1) key dstring))))
		 (when attributes
		   (when (and (consp value) (eq :element (first value)))
		     (error "Ambiguous call to xmp-lookup (a)."))
		   (setf value (list* :element (list key) value attributes))))
	    (t (error "Ambiguous call to xmp-lookup (b).")))
      (cond (delete (and key ht (values (when (remhash key ht) t))))
	    (v-p (and key ht (values (setf (gethash key ht) value) ht)))
	    (t 
	     ;; values:   item symbol nil   -> found in EQ table
	     ;;           item string nil   -> found in EQUAL table
	     ;;           item string :any  -> found in STRING-EQUAL table
	     (values (if (eq idef not)
			 nil
		       idef)
		     found any plist))))))




(defmethod xmp-find-element ((conn t) name nss)
  (let* ((dname (xmp-decode-qualified-name conn name nss))
	 (dstring (symbol-name dname)))
    (xmp-lookup *defined-xmp-elements* dname dstring nil)))

(defmethod xmp-find-type ((conn t) name nss)
  (let ((dname (xmp-decode-qualified-name conn name nss)))
    (xmp-lookup *defined-xmp-elements* nil nil dname)))

(defun xmp-list* (old &rest new &aux res)
  (setf res
	(do ((t1 old) (t2 new (cdr t2)))
	    ((or (atom t1) (null (cdr t2)))
	     (cond ((consp t1) nil)
		   ((cdr t2) nil)
		   ((null (first t2)) old)
		   (t (dolist (np (first t2) (if t1 nil old))
			(or (consp t1) (return nil))
			(or (eq (pop t1) np) (return nil))))))
	  (or (eq (pop t1) (first t2)) (return nil))))
  (if (eq res old)
      old
    (apply 'list* new)))
    
    
(defmethod xmp-normal-type-spec ((conn t) type-spec nss &rest options
				 &key &allow-other-keys)
  (etypecase type-spec
    ((member nil) nil)
    ((or string symbol)
     (xmp-decode-qualified-name conn type-spec nss))
    (cons (ecase (first type-spec)
	    (:simple (let ((tdef (second type-spec)))
		       (if (typecase tdef
			     ((member nil)
			      (getf (cddr type-spec) :simple-content-key))
			     ((or string symbol) t))
			   (xmp-list*
			    type-spec
			    (first type-spec)
			    (when tdef (xmp-decode-qualified-name
					conn tdef nss))
			    (apply 'xmp-normal-options
				   conn :type-spec (cddr type-spec) nss options))
			 (error "Ill-formed type-spec ~S" type-spec))))
	    (:array (xmp-list* 
		     type-spec
		     (first type-spec)
		     (apply 'xmp-normal-type-spec
			    conn (second type-spec) nss options)
		     (apply 'xmp-normal-options
			    conn :type-spec (cddr type-spec) nss options)))
	    (:complex (xmp-list*
		       type-spec
		       (first type-spec)
		       (apply 'xmp-normal-complex-def
			      conn (second type-spec) nss options)
		       (apply 'xmp-normal-options
			      conn :type-spec (cddr type-spec) nss options)))))))

(defmethod xmp-normal-complex-def ((conn t) complex-def nss &rest options
				   &key inner-p &allow-other-keys)
  (etypecase complex-def
    (cons
     (cond
      ((setf inner-p (xmp-collection-p conn complex-def :inner-p inner-p))
       (xmp-list*
	complex-def
	(first complex-def)
	(mapcar
	 #'(lambda (part)
	     (etypecase part
	       ((or string symbol)
		(nth-value 1 (xmp-decode-qualified-name conn part nss)))
	       (cons (case (first part)
		       (:any part)
		       (:element
			(apply 'xmp-normal-element-spec conn part nss options))
		       (otherwise
			(apply 'xmp-normal-complex-def
			       conn part nss
			       :inner-p inner-p
			       options))))))
	 (cdr complex-def))))
      (t (error "Ill-formed complex-def ~S" complex-def))
      ))))


(defmethod xmp-normal-element-spec ((conn t) part nss &rest options)
  (etypecase part
    (cons (ecase (first part)
	    (:any (when (cdr part)
		    (xmp-class-warning conn 'xmp-syntax "Ignore data in (:any).")
		    (setf part (list :any)))
		  part)
	    (:element
	     (xmp-list*
	      part
	      :element
	      (etypecase (second part)
		((member nil) nil)

		;; CURRENT BEHAVIOR:
		;; Elements with in-line definitions may have 
		;;  different definitions in different occurrences.
		;; BUT XML Schema requires that named defs must be
		;; identical   ??? 

		((or string symbol)
		 (list (second part)))
		(cons
		 (mapcar
		  #'(lambda (name)
		      (etypecase name
			((or string symbol)  name)
			(cons (ecase (first name)
				(:any-case
				 (etypecase (second name)
				   (string  name)))))))
		  (second part))))
	      (apply 'xmp-normal-type-spec
		     conn (third part) nss options)
	      (apply 'xmp-normal-options
		     conn :element (cdddr part) nss
		     options)))))
    ((or string symbol) (xmp-decode-qualified-name conn part nss))
    (xmp-element (xmp-element-name part))
    ))
		    
(defmethod xmp-normal-options ((conn t) case opts nss &rest options
			       &key &allow-other-keys)
  (declare (ignore case options))
  (do ((tl opts (cddr tl)) res)
      ((atom tl) (xmp-list* opts (reverse res)))
    (push (etypecase (first tl)
	    (string (xmp-decode-qualified-name conn (first tl) nss))
	    (symbol (first tl)))
	  res)
    (push (second tl) res)))
	     

(defmethod xmp-pick-name ((conn t) edef)
  (typecase edef
    (atom edef)
    (otherwise
     (case (car edef)
       (:element (typecase (second edef)
		   (atom (second edef))
		   (otherwise (typecase (first (second edef))
				(cons (second (first (second edef))))
				(otherwise (first (second edef)))))))))))

(defmethod xmp-elt-getf-name ((conn t) plist edef &optional ignore-case &aux pattern)
  ;; Look in property list plist for a name that matches some
  ;;  name in edef.
  (typecase edef
    (atom (setf pattern edef))
    (otherwise
     (case (car edef)
       (:element (setf pattern (second edef))))))
  (when pattern 
    (do ((tl plist (cddr tl)))
	((atom tl))
      (when (xmp-match-name conn (car tl) pattern ignore-case)
	(return (car tl))))))


(defmethod define-xmp-type ((conn t) name type-spec &rest options
			    &key (redef *xmp-redef-default*) (nss :dns)
			    &allow-other-keys
			    &aux
			    (dname (xmp-decode-qualified-name conn name nss))
			    (odef
			     (xmp-lookup *defined-xmp-elements* nil nil dname))
			    (tdef
			     (apply 'xmp-normal-type-spec conn type-spec nss options))
			    r)
  (when (and odef (not (equal odef tdef)))
    (case redef
      (:warn (xmp-class-warning
	      conn 'xmp-redefinition 
	      "redefining ~A type ~S" (xmp-warning-leader conn) dname))
      ((nil) nil)
      (otherwise
       (xmp-error conn :def
		  (list "redefining ~A type ~S" (xmp-warning-leader conn) dname)))))
  (setf r
	(xmp-lookup *defined-xmp-elements* nil nil dname
		    :value
		    (when type-spec 
		      (apply 'xmp-normal-type-spec conn type-spec nss options))
		    :attributes 
		    (do ((tl options (cddr tl)) res)
			((atom tl) (reverse res))
		      (case (first tl)
			((:nss :redef) nil)
			;; save any other kw args as attribute specs
			(otherwise (push (first tl) res)
				   (push (second tl) res))))
		    :delete (null type-spec)))
  (if (null type-spec) r dname))

(defmethod define-xmp-element ((conn t) names type-spec &rest options
			       &key (redef *xmp-redef-default*) (nss :dns)
			       &allow-other-keys &aux r d)
  (if (and (consp names) (not (eq :any-case (car names))))
      (progn (mapcar #'(lambda (name)
			 (setf r (apply 'define-xmp-element
					conn name type-spec options)))
		     names)
	     r)
    (let* ((dname names)
	   (table *defined-xmp-elements*)
	   odef oname oany
	   (tdef (when type-spec 
		   (apply 'xmp-normal-type-spec conn type-spec nss options))))
      (multiple-value-setq (odef oname oany) (xmp-lookup table dname dname nil))

      (when (and odef tdef (not (equal odef tdef)))
	(case redef
	  (:warn (xmp-class-warning
		  conn 'xmp-redefinition
		  "redefining ~A element ~S" (xmp-warning-leader conn) dname))
	  ((nil) nil)
	  (otherwise
	   (xmp-error conn :def
		      (list "redefining ~A element ~S"
			    (xmp-warning-leader conn) dname)))))
      (setf r (xmp-lookup table
			  (or oname dname) 
			  (setf d (if oname
				      (if oany
					  (list :any-case oname)
					oname)
				    dname))
			  nil
			  :attributes 
			  (do ((tl options (cddr tl)) res)
			      ((atom tl) (reverse res))
			    (case (first tl)
			      ((:nss :redef) nil)
			      ;; save any other kw args as attribute specs
			      (otherwise (push (first tl) res)
					 (push (second tl) res))))
			  :delete (null type-spec)
			  :value tdef))
      (if (null type-spec) r d))))

(defmethod xmp-getf-in-part ((conn t) part name &optional default)
  (getf (typecase part
	  (cons (case (first part)
		  (:element (cdddr part))
		  (otherwise (cddr part))))
	  (otherwise nil))
	name default))

(defmethod xmp-getf-in-def ((conn t) part name &optional default (nss :dns)
			    &aux idef v def (not-found (list nil)))
  (typecase part
    (cons (case (first part)
	    (:element
	     ;; This is an element definition
	     ;;    save the type name or type def
	     (setf def (third part))
	     ;;    and look in the immediate element options first
	     (setf idef (cdddr part)))
	    (:simple
	     (setf def (second part))
	     (setf idef (cddr part)))
	    (otherwise 
	     ;; this is the end of a chain of definitions
	     (setf idef (cddr part)))
	    ))
    (null nil)
    (otherwise
     ;; Element or type is specified by name, look for the definition
     (when (cond ((setf idef (xmp-find-element conn part nss))
		  (or (eq not-found 
			  (setf v (xmp-lookup
				   *defined-xmp-elements* part part nil
				   :attribute name :default not-found)))
		      (return-from xmp-getf-in-def v))
		  t)
		 ((setf idef (xmp-find-type conn part nss))
		  (or (eq not-found 
			  (setf v (xmp-lookup
				   *defined-xmp-elements* nil nil part
				   :attribute name :default not-found)))
		      (return-from xmp-getf-in-def v))
		  t))
       (typecase idef
	 (cons 
	  ;; The element is defined by an explicit type def,
	  (case (first idef) (:simple (setf def (second idef))))
	  (setf idef (cddr idef)))
	 (null nil)
	 (otherwise 
	  ;; The element is defined by a named type
	  ;;     follow the type def chain
	  (setf def idef idef nil)))
       t)
     ))
  (cond
   ((not (eq not-found (setf v (getf idef name not-found)))) v)
   (def (xmp-getf-in-def conn def name default))
   (t default)
   ))
		      

(defun xmp-new-environment ()
  (setf *defined-xmp-elements* (xmp-make-tables t)
	*xmp-server* nil
	*xmp-package-to-xnd* (make-hash-table :test #'eq)
	*xmp-uri-to-xnd* (make-hash-table :test #'equal)
	*xmp-namespace-maps* (make-hash-table :test #'eq)
	))


(defun xmp-match-uri (pattern uri &aux (n pattern))
  (let* ((item (first n)) (string uri)
	 (k (length item)) 
	 (l (length string))
	 (i (search item string)))
    (when (cond ((null i) nil)
		((eql l (+ i k)) t)
		((and (eql l (+ i k 1))
		      (eql #\/ (elt string (1- l))))
		 t))
      n)))

(defun xmp-match-namespace (uri proposed found &aux m2 match ambi)
  ;; uri is a string
  ;; proposed -> ( (uri package-name prefix) ...)
  ;; found    -> ( (uri package-name  prefix uri) ...)
  ;;
  ;; result:   nil  --> not found
  ;;         item from found --> ambiguous uri
  ;;      item from proposed --> found a good one
  (dolist (m proposed (or match ambi))
    (when (xmp-match-uri m uri)
      (cond ((setf m2 (assoc (first m) found :test #'equal))
	     ;; This is the second URI to match the same known
	     ;;  namespace.
	     (if (same-uri uri (fourth m2))
		 ;; if URIs are identical, it is ok
		 (return m)
	       ;; otherwise, save the ambiguity but keep looking
	       ;; for a better match
	       (setf ambi m2)))
	    ((same-uri uri (first m))
	     ;; We have found an exact match to a known namespace URI in the parse.
	     (return m))
	    (t
	     ;; otherwise, save this match but keep looking
	       ;; for a better match
	     (setf match m))
	    ))))

(defun xmp-lookup-namespaces (other nsmap &aux found nse-defs)
  (etypecase nsmap
    (null (values nil other))
    (symbol (xmp-lookup-namespaces other (gethash nsmap *xmp-namespace-maps*)))
    (cons 
     (xmp-lookup-namespaces
      other
      (xmp-define-namespace-map nil nil nsmap)))
    (xmp-namespace-map 
     ;; Scan the unrecognized namespaces to see if they match
     ;;  some known namespace.
     (dolist (o other) 
       (when (null other) (return))
       (when (setf found (xmp-search-map nsmap :uri o))
	 (setf other (remove o other))
	 (push found nse-defs)))
     (values nse-defs other))))
 



(defun xmp-decode-namespaces (&key
			      pns      ;;; (uri-string ...)
			      needed   ;;; ((uri-substring package-name prefix) ...)
			      optional ;;; ((uri-substring package-name prefix) ...)
			      known    ;;; namespace-map name or instance
			      )
  (let* (r ambi found nse nse-defs other missing new)

    (dolist (uri pns)
      ;; Scan all the namespaces that were detected by the parser
      (if (or (setf r (xmp-match-namespace uri needed found))
	      (setf r (xmp-match-namespace uri optional found)))
	  (cond ((member r found)
		 (push (first (last r)) ambi)
		 (push uri ambi))
		(t (push (append r (list uri)) found)))
	(push uri other)))

    (dolist (n needed)
      ;; Remove any synonyms from the remaining list of needed namespaces.
      (if (member (second n) found :key #'second)
	  ;; this is a synonyms for a found namespace
	  ;; drop it
	  nil
	;; this was needed but not found
	(push n missing)))

    (multiple-value-setq (new other)
      (xmp-lookup-namespaces other known))
    (when new (setf nse-defs (append nse-defs new)))
    
    (setf nse-defs
	  (append
	   (mapcar #'(lambda (f) (make-nsd (second f) (third f) (fourth f))) found)
	   nse-defs
	   ))

    (setf nse (make-nse* nil nse-defs))

    (values nse other ambi missing)))


(defmethod xmp-collector-p ((conn t) item &key inner-p)
  (case item
    ((:seq :seq* :seq1 :seq+ :set :set* :set1 :set+ :or) t)
    (:seq? :seq?)
    (:maybe (case inner-p (:seq? t)))
    ))

(defmethod xmp-collection-p ((conn t) item &key inner-p)
  (and (consp item) (xmp-collector-p conn (first item) :inner-p inner-p)))

(defmethod xmp-collection-type-p ((conn t) item)
  (and (consp item)
       (case (first item)
	 (:complex (xmp-collection-p conn (second item))))))


(xmp-define-namespace-map :all t (list nil :all))


(defun xmp-extract-namespaces (&key string file uri)
  (when uri 
    (multiple-value-bind (body rc h ruri)
	(net.aserve.client:do-http-request uri)
      (if (and body (eql rc 200))
	  (setf string body)
	(error "URI ~A returned error ~S ~S ~S"
	       uri rc h ruri))))
  (when (and string file) (error "Ambiguous call."))
  (cond (string (xmp-namespaces-from-string string))
	(file   (xmp-namespaces-from-file file)))
  )





(defmacro with-tree-match ((data pattern) &rest body)
  (labels ((match-tree-vars (pattern)
			    (typecase pattern
			      (cons (case (first pattern)
				      ((:? :?each :?some :?one)
				       (when (second pattern)
					 (list* (second pattern)
						(match-tree-vars (third pattern))
						)))
				      (otherwise
				       (append (match-tree-vars (first pattern))
					       (match-tree-vars (cdr pattern))))))))
	   (sift (list &aux out)
		 (dolist (l list out) (pushnew l out)))
	   )
    (let* ((vars (sift (match-tree-vars pattern)))
	   (r (gensym)) (a (gensym)))
      (if vars
	  `(multiple-value-bind (,r ,a)
	       (match-tree ,data ',pattern)
	     (when ,r
	       (let ,(mapcar #'(lambda (var)
				 `(,var (cdr (assoc ',var ,a))))
			     vars)
		 ,@body)))
	`(when (match-tree ,data ',pattern)
	   ,@body)))))

(defun match-tree (data pattern &optional alist
			&aux
			key var sub-pat body
			)
  ;; returns 2 values: t-or-nil new-alist
  (typecase pattern
    (cons (typecase (setf key (first pattern))
	    ((member :?quote) (values (equal data (second pattern)) alist))
	    ((member :?) 
	     (setf var (second pattern) sub-pat (third pattern))
	     (setf alist (match-tree-bind var data alist))
	     (if sub-pat
		 (match-tree data sub-pat alist)
	       (values t alist)))
	    ((member :?apply)
	     (setf var (second pattern) body (third pattern) sub-pat (fourth pattern))
	     (cond ((null (eval `((lambda ,var ,body) ',data)))
		    (values nil alist))
		   (sub-pat (match-tree data sub-pat alist))
		   (t (values t alist))))
	    ((member :?or)
	     (dolist (p (cdr pattern) (values nil alist))
	       (multiple-value-bind (r a)
		   (match-tree data p alist)
		 (when r (return (values t a)))
		 (setf alist a))))
	    ((member :?and)
	     (dolist (p (cdr pattern) (values t alist))
	       (multiple-value-bind (r a)
		   (match-tree data p alist)
		 (when (null r) (return (values nil a)))
		 (setf alist a))))
	     
	    (cons (if (listp data)
		      (case (first key)
			(:?each
			 ;; data must be a list, each element must match
			 (setf var (second key) sub-pat (third key))
			 (do ((tl data (cdr tl)) r (a alist))
			     ((atom tl) (values (null tl) a))
			   (multiple-value-setq (r a)
			     (match-tree (first tl) sub-pat a))
			   (or r (return (values nil a)))
			   (when var (setf a (match-tree-collect var (first tl) a)))
			   ))
			(:?some
			 ;; data must be a list, match (and collect)
			 ;;   zero or more matching elements
			 (setf var (second key) sub-pat (third key))
			 (if (null data)
			     (values t alist)
			   (multiple-value-bind (r a)
			       (match-tree (first data) sub-pat alist)
			     (and var r
				  (setf a (match-tree-collect var (first data) a)))
			     (if r
				 (match-tree (cdr data) pattern a)
			       (match-tree data (cdr pattern) a)))))
			(:?one
			 ;; data must be a list, match (and collect)
			 ;;   the first matching elements
			 (setf var (second key) sub-pat (third key))
			 (if (null data)
			     (values nil alist)
			   (multiple-value-bind (r a)
			       (match-tree (first data) sub-pat alist)
			     (and var r (setf a (match-tree-bind var (first data) a)))
			     (if r
				 (match-tree (cdr data) (cdr pattern) a)
			       (values nil a)))))
			(otherwise
			 (if (null data)
			     (values nil alist)
			   (multiple-value-bind (r a)
			       (match-tree (first data) key alist)
			     (if r
				 (match-tree (cdr data) (cdr pattern) a)
			       (values nil a))))))
		    (values nil alist)))
	    (otherwise (cond ((atom data) (values nil alist))
			     (t (multiple-value-bind (r a)
				    (match-tree (first data) key alist)
				  (if r
				      (match-tree (cdr data) (cdr pattern) a)
				    (values nil a))))))
	    ))
    ((member :?any) (values t alist))
    (otherwise (values (equal data pattern) alist))))

(defun match-tree-bind (var data alist)
  (let* ((place (assoc var alist)))
    (or place (push (setf place (cons var nil)) alist))
    (setf (cdr place) data)
    alist))

(defun match-tree-collect (var data alist)
  (let* ((place (assoc var alist)))
    (or place (push (setf place (cons var nil)) alist))
    (nconc place (list data))
    alist))



;;; A simple SAX parser to collect only the namespace attributes

#+(or soap-lxml soap-sax)
(defclass namespace-parser (net.xml.sax:sax-parser)
  ((namespaces :accessor namespace-parser-namespaces))
  )
#+(or soap-lxml soap-sax)
(defun xmp-namespaces-from-string (s)
  (multiple-value-bind (r p)
      (net.xml.sax:sax-parse-string s
			:namespace t :show-xmlns nil :comments nil 
			:validate nil :class 'namespace-parser :external t)
    (when r (namespace-parser-namespaces p))))
#+(or soap-lxml soap-sax)
(defun xmp-namespaces-from-file (file)
  (with-open-file 
   (s file)
   (multiple-value-bind (r p)
       (net.xml.sax:sax-parse-stream s
			 :namespace t :show-xmlns nil :comments nil 
			 :validate nil :class 'namespace-parser :external t)
     (when r (namespace-parser-namespaces p)))))


#+(or soap-lxml soap-sax)
(defmethod net.xml.sax:start-document ((parser namespace-parser))
  (setf (namespace-parser-namespaces parser) nil)
  (call-next-method))
#+(or soap-lxml soap-sax)
(defmethod net.xml.sax:start-prefix-mapping ((parser namespace-parser) (prefix t) iri)
  (or 
   ;; Ignore xmlns=""
   (equal iri "") (null iri)
   (pushnew iri (namespace-parser-namespaces parser) :test #'equal))
  (call-next-method))


#+ignore
(defmethod end-document ((parser lxml-parser))
  (call-next-method))
#+ignore
(defmethod start-element ((parser lxml-parser) iri localname (qname t) attrs)
  (call-next-method))
#+ignore
(defmethod end-element ((parser lxml-parser) (iri t) (localname t) (qname t))
  (call-next-method))
#+ignore
(defmethod end-prefix-mapping ((parser lxml-parser) (prefix t))
  (call-next-method))
#+ignore
(defmethod processing-instruction ((parser lxml-parser) (target t) (data t))
  (call-next-method))
#+ignore
(defmethod content ((parser lxml-parser) content start end ignorable)
  (call-next-method))
#+ignore
(defmethod content-character ((parser lxml-parser) character ignorable)
  (call-next-method))
#+ignore
(defmethod compute-external-address ((parser lxml-parser)
				     (system t) (public t) (current-filename t))
  ;; the default method seems to be sufficient
  ;;(warn "net.xml.sax:compute-external-address not implemented for lxml-parser")
  (call-next-method))
#+ignore
(defmethod compute-external-format ((parser lxml-parser) (encoding t) (ef t))
  ;; the default method seems to be sufficient
  ;;(warn "net.xml.sax:compute-external-format not implemented for lxml-parser")
  (call-next-method))






