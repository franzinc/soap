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

;; $Id: xmp-base.cl,v 1.1.1.1 2003/07/24 00:49:45 layer Exp $

;; Common XML Message Protocol support for SOAP, XMLRPC, and others...

(defpackage :net.xmp)
(in-package :net.xmp)

(defpackage :net.xmp (:use :common-lisp :excl :net.xml.parser))

(eval-when (compile load eval) (require :pxml))

(eval-when (compile load eval) (pxml-version 7 nil nil t))

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

   ;; Accessors
   xmp-message-string
   xmp-expected-elements
   xmp-element-name
   xmp-element-type
   xmp-element-content
   xmp-element-tag1
   xmp-element-tag2
   xmp-lisp-value 
   xmp-destination-leader
   xmp-xml-encoding
   xmp-defined-namespaces
   xmp-server-lock
   xmp-server-exports
   xmp-server-enabled
   xmp-server-start
   xmp-message-dns
   xmp-lisp-package
   xmp-trim-whitespace
   xmp-inside-elements
   xmp-normal-element-spec

   ;; Generic functions
   define-xmp-type
   define-xmp-element

   xmp-string-type
   xmp-struct-type
   xmp-make-connector
   xmp-call-method
   xmp-message-begin
   xmp-encode
   xmp-copy
   xmp-encode-object
   xmp-object-class 
   xmp-encode-begin
   xmp-encode-content
   xmp-encode-end
   xmp-message-send
   xmp-decode-message
   xmp-parse-message
   xmp-decode
   xmp-begin-message
   xmp-begin-element
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
   xmp-list-methods
   xmp-method-signature
   xmp-method-help
   xmp-slot-value
   xmp-run-one-file
   xmp-decode-file
   xmp-signature-equal
   xmp-in-depth
   xmp-out-depth
   xmp-decode-qualified-name
   xmp-content-string
   xmp-find-type
   xmp-find-element
   xmp-elt-getf-name
   xmp-pick-name
   xmp-getf-in-part
   xmp-simple-exel
   xmp-defined-element-exel
   xmp-warning-leader

   ;; Ordinary functions
   encode-base64-string
   decode-base64-string
   same-uri

   ;; Macros
   
   ;; Variables
   *xmp-server*
   *xmp-debug*

   ))


(defvar *xmp-debug* t)

(defclass xmp-connector () 
  (
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

   (string-type :reader xmp-string-type :initform :|string| :allocation :class)

   (xml-leader   :accessor xmp-destination-leader :initarg :xml-leader   :initform "")
   (xml-encoding :accessor xmp-xml-encoding       :initarg :xml-encoding :initform nil)

   (message-xml :accessor xmp-message-xml)
   (message-dtd :accessor xmp-message-dtd)
   (message-pns :accessor xmp-message-pns)  ;;; parser namespace alist ((uri . pkg) ...)
   (message-dns  
    ;; Pre-defined namespaces and prefixes
    ;; value -> (default-namespace-uri
    ;;           (package-or-name prefix uri) ... )
    :accessor xmp-message-dns :initform nil :initarg :message-dns)
   (message-ns
    ;; Namespace mapping returned by XML parser
    ;; value -> (default-namespace-uri
    ;;           (package prefix uri) ... )
    :accessor xmp-message-ns :initform nil)
   (out-nss
    ;; Namespace context stack
    ;; value -> (ns-entry ... )
    :accessor xmp-out-nss :initform nil)
   (in-nss
    ;; Namespace context stack
    ;; value -> (ns-entry ... )
    :accessor xmp-in-nss :initform nil)
   (defined-ns    
    ;; value -> ((url prefix) ... )
    :accessor xmp-defined-namespaces :initform nil)
   (expected-elements :accessor xmp-expected-elements)
   (inside-elements   :accessor xmp-inside-elements :initform nil)

   (lisp-package    :accessor xmp-lisp-package :initarg :lisp-package :initform nil)
   (trim-whitespace
    ;; nil    -> no action
    ;;  t     -> drop leading and trailing #\space #\newline and #\tab
    ;; string -> drop leading and trailing elements of string
    :accessor xmp-trim-whitespace :initarg :trim-whitespace :initform nil)

   (debug            :accessor xmp-debug :initarg :debug :initform *xmp-debug*)
   ))

;;; composite class names:  xml-protocol-transport-role-sendstyle-receivestyle-connector


(defclass xmp-client-connector (xmp-connector) 
  ((role :initform :client)))

(defclass xmp-server-connector (xmp-connector) 
  ((role :initform :server)
   (server-lock :reader xmp-server-lock :initform (mp:make-process-lock))
   (server-enabled :accessor xmp-server-enabled :initform nil)
   (server-start   :accessor xmp-server-start   :initarg :start :initform nil)
   (exports     
    ;; key is string that names an exported method
    ;; value -> ((signature return-type lisp-function enabled) ... )
    :reader xmp-server-exports :initform (make-hash-table :test #'equal))
   ))


(defclass xmp-string-out-connector (xmp-connector) 
  ((message-string :accessor xmp-message-string :initform nil :initarg :message-string)
   ))
(defclass xmp-stream-out-connector (xmp-connector) ())
(defclass xmp-string-in-connector (xmp-connector)  ())
(defclass xmp-event-in-connector (xmp-connector) ())


(defclass xmp-string-in-out-connector 
  (xmp-string-out-connector xmp-string-in-connector)  ())
(defclass xmp-event-string-connector 
  (xmp-string-out-connector xmp-event-in-connector)   ())
(defclass xmp-string-stream-out-connector  
  (xmp-stream-out-connector  xmp-string-in-connector) ())
(defclass xmp-event-stream-connector  
  (xmp-stream-out-connector  xmp-event-in-connector)  ())
   

(defclass xmp-client-string-out-connector 
  (xmp-client-connector xmp-string-out-connector) ())
(defclass xmp-client-stream-out-connector  
  (xmp-client-connector xmp-stream-out-connector) ())
(defclass xmp-client-string-in-connector   
  (xmp-client-connector xmp-string-in-connector)  ())
(defclass xmp-client-event-in-connector   
  (xmp-client-connector xmp-event-in-connector)   ())

(defclass xmp-client-string-in-out-connector
  (xmp-client-string-out-connector 
   xmp-client-string-in-connector
   xmp-string-in-out-connector
   ) ())
(defclass xmp-client-event-string-connector
  (xmp-client-string-out-connector 
   xmp-client-event-in-connector
   xmp-event-string-connector
   ) ())
(defclass xmp-client-string-stream-out-connector
  (xmp-client-stream-out-connector 
   xmp-client-string-in-connector
   xmp-string-stream-out-connector
   ) ())
(defclass xmp-client-event-stream-connector
  (xmp-client-stream-out-connector
   xmp-client-event-in-connector
   xmp-event-stream-connector
   ) ())

(defclass xmp-server-string-out-connector (xmp-server-connector xmp-string-out-connector) ())
(defclass xmp-server-stream-out-connector  (xmp-server-connector xmp-stream-out-connector)  ())
(defclass xmp-server-string-in-connector   (xmp-server-connector xmp-string-in-connector)   ())
(defclass xmp-server-event-in-connector   (xmp-server-connector xmp-event-in-connector)   ())

(defclass xmp-server-string-in-out-connector 
  (xmp-server-string-out-connector 
   xmp-server-string-in-connector
   xmp-string-in-out-connector 
   ) ())
(defclass xmp-server-event-string-connector
  (xmp-server-string-out-connector 
   xmp-server-event-in-connector
   xmp-event-string-connector
   ) ())
(defclass xmp-server-string-stream-out-connector
  (xmp-server-stream-out-connector 
   xmp-server-string-in-connector
   xmp-string-stream-out-connector
   ) ())
(defclass xmp-server-event-stream-connector
  (xmp-server-stream-out-connector 
   xmp-server-event-in-connector
   xmp-event-stream-connector
   ) ())



(defclass xmp-element () 
  ((name :accessor xmp-element-name :initarg :name :initform nil)
   (tag1 :accessor xmp-element-tag1 :initarg :tag1 :initform nil)
   (tag2 :accessor xmp-element-tag2 :initarg :tag2 :initform nil)
   (lisp-value    :accessor xmp-lisp-value      :initarg :lisp-value :initform nil)
   (content       :accessor xmp-element-content :initarg :content :initform nil)
   (type          :accessor xmp-element-type    :initarg :type :initform nil)
   ))



;;; composite class names:  xml-protocol-transport-role-instyle-outstyle-suffix
(defgeneric xmp-make-connector (protocol transport role decodestyle encodestyle
					 &rest options &key &allow-other-keys))

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
     (format s "xmp ~S" (xmp-fault-code e))
     (when sub (format s " sub-code=~S" sub))
     (when fac (format s " factor=~S" fac))
     (when det (format s " detail=~S" det))
     (when str (format s " ~A" str)))))



(defgeneric xmp-encode (connector data type &rest options &key &allow-other-keys))
(defgeneric xmp-decode (connector element &rest options &key &allow-other-keys))
(defgeneric xmp-begin-message (connector))
(defgeneric xmp-end-message (conn data &key &allow-other-keys))
(defgeneric xmp-message-send (conn &key &allow-other-keys))


(defmethod xmp-copy ((conn xmp-connector) (object xmp-connector)
		     &key &allow-other-keys
		     &aux (new (make-instance (class-of object)))
		     )
  (setf (xmp-destination-leader new) (xmp-destination-leader object)
	(xmp-xml-encoding new)       (xmp-xml-encoding object)
	(xmp-message-dns new) (xmp-message-dns object)
	(xmp-out-nss new) (xmp-out-nss object)
	(xmp-in-nss new) (xmp-in-nss object)
	(xmp-expected-elements new) (xmp-expected-elements object)
	(xmp-inside-elements new) (xmp-inside-elements object)
	(xmp-lisp-package new) (xmp-lisp-package object)
	(xmp-trim-whitespace new) (xmp-trim-whitespace object)
	)
  new)
  
  



(defmethod xmp-decode-message ((conn xmp-string-in-connector) data)
  (setf (xmp-expected-elements conn)
	(list (xmp-new-complex-def conn (xmp-begin-message conn))))
  (multiple-value-bind (dt tp)
      (xmp-decode-body conn data)
    (xmp-end-message conn dt :types tp)))
    

(defmethod xmp-decode-body ((conn xmp-string-in-connector) data
			    &rest options &key attributes &allow-other-keys)
  (declare (ignore options))
  ;; data is a list of elements
  ;; 
  ;; If we expect an empty element, then top of expected-elements is (:seq)
  (let (types)
    (values
     (mapcan #'(lambda (e &aux v type)
		 (multiple-value-setq (v type)
		   (xmp-decode conn e :attributes attributes))
		 (when type (push type types))
		 v)
	     data)
     (reverse types))))


(defmethod xmp-element-parts ((conn xmp-connector) elt &aux attr)
  (when (consp elt)
    (if (consp (setf elt (car elt)))
	(setf attr (cdr elt) elt (first elt))
      (setf attr nil))
    (values elt attr)))

(defmethod xmp-decode ((conn xmp-string-in-connector) data
		       &rest options &key &allow-other-keys
		       &aux elt cdi exel attr type types strt 
		       (trim (xmp-trim-whitespace conn))
		       )
  (declare (ignore options))
  ;; data is a single element
  (typecase (setf exel (first (xmp-expected-elements conn)))
    (xmp-complex-def-instance (setf cdi exel) (setf exel nil)))
  (setf strt (xmp-string-type conn))
  (values
   (cond 
    ((and (consp data)
	  (progn
	    (multiple-value-setq (elt attr) (xmp-element-parts conn data))
	    (cond (cdi (xmp-test-complex-def conn cdi elt))
		  ((equal exel '(:seq* (:any)))))))
     (let (vals)

       ;; Update the context first, then get the expected elements
       (push (cons elt attr) (xmp-inside-elements conn))
       (push (xmp-new-complex-def
	      conn (xmp-begin-element conn elt :attributes attr))
	     (xmp-expected-elements conn))

       (multiple-value-setq (vals types)
	 (xmp-decode-body conn (cdr data) :attributes attr))
       (multiple-value-setq (vals type)
	 (xmp-complex-content conn elt vals :types types :attributes attr))
       (xmp-end-element conn elt)
       (pop (xmp-inside-elements conn))
       (pop (xmp-expected-elements conn))

       (when cdi (xmp-step-complex-def conn cdi elt))

       vals))
    ((and (stringp data)
	  (typecase trim
	    ((member nil) nil)
	    (string t)
	    (otherwise (setf trim (concatenate 'string (list #\space #\tab #\newline)))
		       t))
	  (equal "" (setf data (string-trim trim data))))
     (values))
    ((and (stringp data)
	  (cond (exel (or (atom exel)
			  (and (equal exel '(:seq* (:any))) (setf exel strt))))
		(cdi (when (and strt (xmp-test-complex-def conn cdi strt))
		       (setf exel strt)))))
     (let ((res (multiple-value-list 
		 (xmp-simple-content conn exel data))))
       (cond ((null res) (values))
	     ((null (cdr res)) res)
	     ((cddr res) 
	      (xmp-error 
	       conn :client
	       :string (list "Too many values from xmp-simple-content ~S" res)))
	     (t (setf type (second res)) (list (first res)))
	     )))
    (t (xmp-error conn :client :string (list "Expected: ~S  Found: ~S" 
					     (if cdi (xmp-cdi-def cdi) exel) data))))
   type))


(defmethod xmp-defined-element-exel ((conn t) elt nss &aux edef prev dn tn ex)
  ;; return 3 values: expected-element type-name defined-p
  (when (setf 
	 edef
	 (or (typecase conn
	       (xmp-connector
		(typecase (setf prev (first (xmp-expected-elements conn)))
		  (xmp-complex-def-instance 
		   (xmp-cdi-inner prev))))
	       (otherwise nil))
	     (xmp-find-element conn elt nss)))
    (setf dn elt))
  (typecase edef
    ((member nil) nil)
    (cons (case (first edef)
	    (:element (multiple-value-setq (ex tn) (xmp-element-exdef conn edef nss)))
	    (:complex (setf ex (second edef)))
	    (:simple (or (setf ex (second edef))
			 (setf ex (xmp-simple-exel conn edef)))))))
  (values ex tn dn))

(defmethod xmp-begin-element ((conn xmp-string-in-connector) (elt t)
			      &rest options &key &allow-other-keys)
  (declare (ignore options))
  (or (xmp-defined-element-exel conn elt :in)
      ;; The default is to accept anything
      '(:seq* (:any))))


(defmethod xmp-begin-element :before ((conn xmp-string-in-connector) 
				      (elt t)
				      &rest options &key attributes &allow-other-keys
				      &aux nse)
  (declare (ignore options))

  ;; build prefix -> package and uri mapping that may be needed to decode
  ;; some attribute values correctly.
  (do ((tl attributes (cddr tl)))
      ((atom tl) (push nse (xmp-in-nss conn)))
    (let* ((name (string (first tl)))
	   (uri (second tl))
	   )
      (cond ((not (eql 0 (search "xmlns" name))))
	    ((eql 5 (length name)) 
	     (if (null nse) 
		 (setf nse (list (xmp-package-of-uri conn uri)))
	       (setf (first nse) uri)))
	    ((eql 6 (length name)))
	    (t (or nse (setf nse (list nil)))
	       (push (list (xmp-package-of-uri conn uri) (subseq name 6) uri)
		     (cdr nse)))))))

(defun same-uri (x y)
  (typecase x 
    (net.uri:uri 
     (typecase y 
       (net.uri:uri (return-from same-uri (net.uri:uri= x y)))
       (otherwise (rotatef x y)))))
  (typecase y
    (net.uri:uri (setf y (or (net.uri::uri-string y)
			     (format nil "~A" y)))))
  ;;(format t "~&same-uri ~S ~S~%" x y)
  (string-equal x y))

(defmethod xmp-package-of-uri ((conn xmp-connector) uri &aux e)
  (when (setf e (assoc uri (xmp-message-pns conn) :test 'same-uri))
    (cdr e)))


(defmethod xmp-end-element :after ((conn xmp-string-in-connector) 
				      (elt t)
				      &rest options &key &allow-other-keys)
  (declare (ignore options))
  (pop (xmp-in-nss conn)))

(defmethod xmp-in-depth ((conn xmp-connector)) (length (xmp-in-nss conn)))
(defmethod xmp-out-depth ((conn xmp-connector)) (length (xmp-out-nss conn)))


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
       (cons
	(ecase (first def)
	  (:simple
	   (cond ((second def)
		  (return-from xmp-simple-content
		    (apply 'xmp-simple-content conn (second def) data options)))
		 ((setf key (getf (cddr def) :simple-content-key))
		  (when (not (eq key elt))
		    (return-from xmp-simple-content
		      (apply 'xmp-simple-content conn key data options))))
		 ))))))
			 
   ;; default content is the parsed XML string content
   ;;(format t "~&;;xmp-simple-content ~S~%" elt)
   (list elt data)))

(defmethod xmp-complex-content :around ((conn xmp-string-in-connector) 
					(elt t) (data t) &rest options 
					&key &allow-other-keys)
  (declare (ignore options))
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
      (let ((tc (xmp-copy conn conn)))
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
			      &key (class (apply 'xmp-object-class
						 conn data type options))
			      &allow-other-keys)
  (apply 'xmp-encode-object
	 conn
	 (make-instance class :lisp-value data :type type)
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


(defmethod xmp-message-begin ((conn xmp-string-out-connector))
  (let ((s (xmp-message-string conn)))
    (if s
	(setf (fill-pointer s) 0)
      (setf (xmp-message-string conn)
	    (make-array 500 :element-type 'character
			:adjustable t :fill-pointer 0)))))

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
	 (extend 100)
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

(defmethod xmp-translate-nss ((conn xmp-connector) nss)
  (case nss
    (:in (xmp-in-nss conn))
    (:out (xmp-out-nss conn))
    (:dns (list (xmp-normal-nse (xmp-message-dns conn))))
    (otherwise nss)))

(defmethod xmp-uri-to-package ((conn xmp-connector) uri nss &aux e)
  (setf nss (xmp-translate-nss conn nss))
  (dolist (nse nss)
    (when (setf e (member uri (cdr nse) :test 'same-uri :key 'third))
      (setf e (first (first e)))
      (return e))))

(defmethod xmp-uri-to-prefix ((conn xmp-connector) uri nss &aux e)
  (setf nss (xmp-translate-nss conn nss))
  (dolist (nse nss)
    (when (setf e (member uri (cdr nse) :test 'same-uri :key 'third))
      (setf e (second (first e)))
      (return e))))

(defmethod xmp-prefix-to-package ((conn xmp-connector) prefix nss &aux e)
  (setf nss (xmp-translate-nss conn nss))
  (when (equal prefix "") (setf prefix nil))
  (dolist (nse nss)
    (if (null prefix)
	(when (first nse)
	  (return (xmp-uri-to-package conn (first nse) nss)))
      (when (setf e (member prefix (cdr nse) :test 'equal :key 'second))
	(setf e (first (first e)))
	(return e)))))

(defmethod xmp-package-to-prefix ((conn xmp-connector)  pk nss &aux prefixes e d)
  (setf nss (xmp-translate-nss conn nss))
  (or (packagep pk)
      (when (null pk) (setf pk *package*))
      (when (setf e (find-package pk)) (setf pk e)))
  (dolist (nse nss)
    (or d (setf d (first nse)))
    (dolist (e (cdr nse))
      (if (eq pk (first e))
	  (if (same-uri d (third e))
	      (return-from xmp-package-to-prefix (values "" (second e)))
	    (if (member (second e) prefixes :test #'equal)
		(xmp-error 
		 :namespace
		 :string
		 (list
		  "Namespace prefix ~A of package ~A is masked."
		  (second e) (package-name pk)))
	      (return-from xmp-package-to-prefix (second e))))
	(push (second e) prefixes)))))

(defmethod xmp-default-uri ((conn xmp-connector) nss)
  (setf nss (xmp-translate-nss conn nss))
  (dolist (nse nss) (when (first nse) (return (first nse)))))

(defmethod xmp-default-package ((conn xmp-connector) nss &aux e)
  (when (setf e (xmp-default-uri conn nss)) (xmp-uri-to-package conn e nss)))

(defmethod xmp-default-prefix ((conn xmp-connector) nss &aux e)
  (when (setf e (xmp-default-uri conn nss)) (xmp-uri-to-prefix conn e nss)))



(defmethod xmp-decode-qualified-name ((conn t) (data symbol) (nss t))
  data)

(defmethod xmp-decode-qualified-name ((conn t) (data string) (nss t))
  (if (position #\: data)
      (xmp-error conn :decode 
		 :string (list "Unknown namespace in qualified name ~A" data))
    (intern data)))

(defmethod xmp-decode-qualified-name ((conn xmp-connector) (data string) nss)
  (let* ((qp (position #\: data))
	 (prefix (when qp (subseq data 0 qp)))
	 (name (if qp
		   (subseq data (1+ qp))
		 data))
	 pk)
    (if (setf pk (xmp-prefix-to-package conn prefix nss))
	(intern name pk)
      (if qp
	  (xmp-error conn :decode 
		     :string (list "Unknown namespace in qualified name ~A" data))
	(intern name (or (xmp-lisp-package conn) *package*))))))


(defmethod xmp-encode-qualified-name ((conn xmp-string-out-connector)
				      (data string) nss &key sanitize)
  (declare (ignore nss))
  (xmp-encode-content conn data :sanitize sanitize))

(defmethod xmp-encode-qualified-name ((conn xmp-string-out-connector)
				      (data symbol) nss &key sanitize)
  (let* ((pk (symbol-package data))
	 prefix)
    (when (and (setf prefix (xmp-package-to-prefix conn pk nss))
	       (not (equal prefix "")))
	(xmp-encode-content conn prefix)
	(xmp-encode-content conn ":" :sanitize nil))
    (xmp-encode-content conn (symbol-name data) :sanitize sanitize)))
	  
(defun xmp-normal-nse (nse)
  (or (and (typecase (first nse) (string t) ((member nil) t))
	   (dolist (nse (cdr nse) nse)
	     (or (xmp-normal-nsd nse t) (return nil))))
      (list* (etypecase (first nse)
	       (string (first nse))
	       ((member nil) nil))
	     (mapcar 'xmp-normal-nsd (cdr nse)))))

(defun xmp-normal-nsd (nsd &optional test-only)
  (or (and (typecase (first nsd) (package t))
	   (typecase (second nsd) ((or string symbol) t))
	   nsd)
      (if test-only
	  nil
	(list (etypecase (first nsd)
		(package (first nsd))
		((or string symbol) 
		 (or (find-package (first nsd))
		     (xmp-error
		      nil :def
		      :string
		      (list
		       "Cannot find-package in NameSpace spec ~S"
		       nsd)))))
	      (etypecase (second nsd)
		((or string symbol) (second nsd)))
	      (third nsd)))))
	

(defmethod xmp-encode-begin ((conn xmp-string-out-connector) data &rest options 
			      &key namespaces attributes empty &allow-other-keys)
  (declare (ignore options))
  (push (xmp-normal-nse namespaces) (xmp-out-nss conn))
  (xmp-encode-content conn "<" :sanitize nil)
  (xmp-encode-qualified-name conn data :out)
  (when (first namespaces)
    (xmp-encode-content conn " xmlns=" :sanitize nil)
    (xmp-encode-string conn (first namespaces) :sanitize t))
  (dolist (nse (cdr namespaces))
    (when (second nse)
      (xmp-encode-content conn " xmlns:" :sanitize nil)
      (xmp-encode-content conn (second nse) :sanitize t)
      (xmp-encode-content conn "=" :sanitize nil)
      (xmp-encode-string conn (third nse) :sanitize t)))
  (do ((tl attributes (cddr tl)))
      ((atom tl))
    (xmp-encode-content conn " " :sanitize nil)
    (xmp-encode-qualified-name conn (first tl) :out :sanitize t)
    (xmp-encode-content conn "=" :sanitize nil)
    (xmp-encode-string conn (second tl) :sanitize t :qname t))
  (cond (empty
	 (xmp-encode-content conn " />" :sanitize nil)
	 (pop (xmp-out-nss conn)))
	(t (xmp-encode-content conn ">" :sanitize nil))))


(defmethod xmp-encode-string ((conn xmp-string-out-connector) data 
			      &key qname (nss :out) sanitize)
  (let ((del "'"))
    (when (position #\' (string data))
      (when (position #\" (string data))
	(xmp-error 
	 conn :encode :string "String contains both single and double quotes."))
      (setf del "\""))
    (xmp-encode-content conn del)
    (if qname
	(xmp-encode-qualified-name conn data nss :sanitize sanitize)
      (xmp-encode-content conn data :sanitize sanitize))
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

(defmethod xmp-warning-leader ((conn t)) "XMP Warning")
(defmethod xmp-warning-leader ((conn string)) conn)
(defmethod xmp-warning ((conn t) &rest format-args)
  (format t "~&;~A: ~A~%"
	  (xmp-warning-leader conn)
	  (apply 'format nil format-args)))

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

(defun resolve-uri (uri)
  (or (ignore-errors (net.uri:parse-uri uri)) uri))

(defmethod xmp-parse-message ((conn xmp-connector) source &key namespaces) 
  (let* ((dns (xmp-normal-nse (xmp-message-dns conn)))
	 (nss (list namespaces dns))
	 (pk (or (xmp-default-package conn nss) (xmp-lisp-package conn)))
	 (*package* (resolve-package pk)))
    (multiple-value-bind (xml ns)
	(parse-xml source
		   :content-only t
		   :uri-to-package
		   (mapcar #'(lambda (x)
			       (cons (resolve-uri (third x))
				     (resolve-package (first x))))
			   (apply 'append (mapcar 'cdr nss)))
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
	  (setf int (+ (ash int 8) (char-int (elt string k))))))
    (setf res (excl::integer-to-base64-string int 48))
    (dotimes (n 4)
      (setf (elt out (+ d n)) (elt res (+ 4 n))))))

(defun decode-base64-string (string)
  (do ((i 0)
       (len (length string))
       int res j k p s
       )
      ((<= len i)
       (concatenate 'string (nreverse res)))
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
		   (2 (push (code-char (logand #xff int)) res))
		   (1 (push (code-char (ash (logand #xff00 int) -8)) res)
		      (push (code-char (logand #xff int)) res))
		   (0 (push (code-char (ash (logand #xff0000 int) -16)) res)
		      (push (code-char (ash (logand #xff00 int) -8)) res)
		      (push (code-char (logand #xff int)) res)))))
    ))







(defmethod xmp-enable-server ((server xmp-server-connector)
			      &key enable-exports &allow-other-keys)
  (mp:with-process-lock 
   ((xmp-server-lock server))
   (setf (xmp-server-enabled server) t)
   (when enable-exports
     (maphash #'(lambda (k v)
		  (declare (ignore v))
		  (xmp-enable-method server k :all))
	      (xmp-server-exports server)))
   ;; return server object
   server))


	


(defmethod xmp-disable-server ((server xmp-server-connector)
			      &key disable-exports &allow-other-keys)
  (mp:with-process-lock 
   ((xmp-server-lock server))
   (setf (xmp-server-enabled server) nil)
   (when disable-exports
     (maphash #'(lambda (k v)
		  (declare (ignore v))
		  (xmp-disable-method server k :all))
	      (xmp-server-exports server)))
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
			      &key lisp-name (enable t) return help &allow-other-keys)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (gethash name table))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  (entry (assoc sig item :test sig=))
	  )
     (or entry
	 (setf (gethash name table)
	       (setf item
		     (cons (setf entry (list sig)) item))))
     (setf (cdr entry) (list return
			     (or lisp-name (intern name))
			     enable
			     help
			     ))
     name)))

(defmethod xmp-enable-method ((conn xmp-server-connector) name sig)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (gethash name table))
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
	  (item (gethash name table))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  entry)
     (when item
       (if (eq sig :all)
	   (dolist (e item name) (setf (fourth e) nil))
	 (when (setf entry (assoc sig entry :test sig=))
	   (setf (fourth entry) nil)
	   name))))))

(defmethod xmp-accept-method ((conn xmp-server-connector) name sig (args t))
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (sig= #'(lambda (x y) (xmp-signature-equal conn x y)))
	  (item (gethash name table))
	  (entry (assoc sig item :test sig=)))
     (if (and entry (fourth entry))
	 (values (third entry) (second entry))
       (values)))))

(defmethod xmp-list-methods ((conn xmp-server-connector))
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  all)
     (maphash #'(lambda (k v) (declare (ignore v)) (push k all)) table)
     all)))

(defmethod xmp-method-signature ((conn xmp-server-connector) name)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (gethash name table))
	  all)
     (dolist (entry item)
       (push (cons (second entry) (first entry)) all))
     all)))

(defmethod xmp-method-help ((conn xmp-server-connector) name)
  (mp:with-process-lock 
   ((xmp-server-lock conn))
   (let* ((table (xmp-server-exports conn))
	  (item (gethash name table)))
     (dolist (entry item "")
       (when (fifth entry) (return (fifth entry)))))))


(defmethod xmp-invoke-method ((server xmp-server-connector) name args)
  (let ((*xmp-server* server))
    (apply name args)))


(defmethod xmp-struct-type ((conn t)) :|struct|)

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

(defmethod xmp-decode-file ((conn xmp-client-connector) file)
  (with-open-file
   (s file)
   (xmp-decode-message conn (xmp-parse-message conn s))))






;;; COMPLEX-DEF-INSTANCE

(defclass xmp-complex-def-instance ()
  ((complex-def :accessor xmp-cdi-def :initarg :def)
   (state       :accessor xmp-cdi-state :initform nil)
   (found       :accessor xmp-cdi-found :initform nil)
   (stack       :accessor xmp-cdi-stack :initform nil)
   (inner-def   :accessor xmp-cdi-inner :initform nil)
   ))

(defmethod xmp-new-complex-def ((conn t) cd)
  (cond ((atom cd) cd)
	((equal cd '(:seq* (:any))) cd)
	(t (make-instance 'xmp-complex-def-instance :def cd))))

(defmethod xmp-test-complex-def ((conn t) (cdi xmp-complex-def-instance) elt)
  ;; return -> nil | :maybe | t
  (xmp-match-complex-part conn cdi
			  (xmp-cdi-def cdi)
			  (if elt
			      (append (xmp-cdi-found cdi) (list elt))
			    (xmp-cdi-found cdi))))

(defmethod xmp-step-complex-def ((conn t) (cdi xmp-complex-def-instance) elt)
  (when elt
    (setf (xmp-cdi-found cdi)
	  (append (xmp-cdi-found cdi) (list elt)))))

(defmethod xmp-match-complex-part ((conn t) cdi part found)
  (cond ((atom found) (values :partial nil))
	((or (atom part) (case (first part) ((:element :any) t)))
	 (when (xmp-match-element-def conn cdi part (first found))
	   (values t (cdr found))))
	(t (xmp-match-complex-tail conn cdi (first part) (cdr part) found))))
  

(defmethod xmp-match-complex-tail1 ((conn t) cdi coll tail found
				   &key matched)
  (cond ((null tail)  (values t found))
	((null found) (values t nil))
	(t (case coll
	     (:seq
	      (do ((tl tail (cdr tl)))
		  ((atom tl) (values t found))
		(multiple-value-bind (r ftail)
		    (xmp-match-complex-part conn cdi (first tl) found)
		  (when r
		    (return
		     (xmp-match-complex-tail conn cdi coll (cdr tl) ftail))))))
	     (:set
	      (dolist (part tail (values t found))
		(multiple-value-bind (r ftail)
		    (xmp-match-complex-part conn cdi part found)
		  (when r
		    (return
		     (xmp-match-complex-tail
		      conn cdi coll (remove part tail :count 1) ftail))))))
	     (:seq*
	      (do ((tl tail (cdr tl)))
		  ((atom tl) (values t found))
		(multiple-value-bind (r ftail)
		    (xmp-match-complex-part conn cdi (first tl) found)
		  (when r
		    (return
		     (xmp-match-complex-tail conn cdi coll tl ftail))))))
	     (:set*
	      (dolist (part tail (values t found))
		(multiple-value-bind (r ftail)
		    (xmp-match-complex-part conn cdi part found)
		  (when r
		    (return
		     (xmp-match-complex-tail conn cdi coll tail ftail))))))
	     (:seq1
	      (multiple-value-bind (r ftail)
		  (xmp-match-complex-part conn cdi (first tail) found)
		(when r
		  (xmp-match-complex-tail conn cdi coll (cdr tail) ftail))))
	     (:set1
	      (dolist (part tail nil)
		(multiple-value-bind (r ftail)
		    (xmp-match-complex-part conn cdi part found)
		  (when r
		    (return
		     (xmp-match-complex-tail
		      conn cdi coll (remove part tail :count 1) ftail))))))
	     (:seq+
	      (multiple-value-bind (r ftail)
		  (xmp-match-complex-part conn cdi (first tail) found)
		(when r
		  (xmp-match-complex-tail conn cdi coll tail ftail))))
	     (:set+
	      (let ((i 0))
		(dolist (part tail
			      (when (eql matched (length tail))
				(values t found)))
		  (multiple-value-bind (r ftail)
		      (xmp-match-complex-part conn cdi part found)
		    (when r
		      (pushnew i matched)
		      (return
		       (xmp-match-complex-tail
			conn cdi coll tail ftail :matched matched)))))))
	     (:or   (do ((tl tail (cdr tl)) part)
			((atom tl) nil)
		      (setf part (first tl))
		      (multiple-value-bind (r ftail)
			  (xmp-match-complex-part conn cdi part found)
			(when r
			  ;;not right! ???
			  ;;(setf (xmp-cdi-def cdi) (cons :or tl))
			  (return (values t ftail))))))
	     ))))


(defmethod xmp-match-complex-tail ((conn t) cdi coll tail found
				   &key matched)
  (declare (ignore matched))
  (let* ((stack (xmp-cdi-stack cdi))
	 (top (pop stack))
	 (top-coll (if top
		       (first top)
		     coll))
	 (top-tail (if top
		       (second top)
		     tail))
	 (fhead (if top (third top) 0))
	 (matched (if top (fourth top) nil))
	 (ftail found))
    (dotimes (i fhead) (setf ftail (cdr ftail)))

    (flet ((match-part (part)
		       (cond ((null ftail) (values :done nil nil))
			     ((null part) nil)
			     ((case (when (consp part) (first part))
				((:seq :seq* :seq+ :seq1
				       :set :set* :set+ :set1
				       :or)
				 t))
			      (values :push (first part) (cdr part)))
			     ((xmp-match-element-def conn cdi part (first ftail))
			      (values :match nil nil))))

	   )
      (let (res new-coll new-tail)
	(loop

	 (cond ((null ftail)     (setf res :done))
	       ((null top-tail)  (setf res :pop))
	       (t (ecase top-coll

		    ((:seq :seq*)
		     (do ((tl top-tail (cdr tl)))
			 ((atom tl) (setf res :pop))
		       (multiple-value-setq (res new-coll new-tail)
			 (match-part (first tl)))
		       (when res
			 (case top-coll
			   (:seq  (setf top-tail (cdr tl))))
			 (return))))

		    ((:set :set*)
		     (dolist (part top-tail (setf res :pop))
		       (multiple-value-setq (res new-coll new-tail)
			 (match-part part))
		       (when res
			 (case top-coll
			   (:set (setf top-tail (remove part top-tail :count 1))))
			 (return))))

		    (:seq1
		     (multiple-value-setq (res new-coll new-tail)
		       (match-part (first top-tail)))
		     (when res (setf top-tail (cdr top-tail))))
		    (:seq+
		     (multiple-value-setq (res new-coll new-tail)
		       (match-part (first top-tail)))
		     (case res
		       (:push (setf matched t))
		       (:match (setf matched t))
		       ((nil) (when matched
				(setf top-tail (cdr top-tail)
				      matched nil
				      res :step)))))

		    ((:set1 :set+)
		     (let ((i 0))
		       (dolist (part tail
				     (if (eql fhead (length tail))
					 (setf res :pop)
				       (setf res nil)))
			 (multiple-value-setq (res new-coll new-tail)
			   (match-part part))
			 (when res 
			   (case top-coll
			     (:set1 (when (member i matched)
				      (setf res nil)
				      (return))
				    (setf top-tail (remove part top-tail :count 1))
				    ))
			   (pushnew i matched)
			   (return)))))

		    (:or (do ((tl tail (cdr tl)) part)
			     ((atom tl) nil)
			   (setf part (first tl))
			   (multiple-value-setq (res new-coll new-tail)
			     (match-part part))
			   (when res
			     (setf top-tail (cdr tl))
			     (return))))

		    )))

	 (ecase res
	   (:push   (push (list top-coll top-tail fhead matched) stack)
		    (setf top-coll new-coll
			  top-tail new-tail
			  matched nil
			  ))
	   (:match (incf fhead) (pop ftail))
	   (:step  nil)
	   (:done  (setf (xmp-cdi-stack cdi) stack)
		   (return (values t ftail)))
	   (:pop
	    (cond (stack
		   (multiple-value-setq (top-coll top-tail fhead)
		     (values-list (pop stack))))
		  (t (return (values t ftail)))))
	   ((nil) (return (values nil ftail))))
	 
	 )))))
	       
(defmethod xmp-match-element-def ((conn t) cdi eldef elt &aux names)
  (setf (xmp-cdi-inner cdi) nil)
  (cond ((eq eldef elt) t)
	(t (and elt
		(atom elt)
		(consp eldef)
		(cond ((eq (first eldef) :any) t)
		      ((eq (first eldef) :element)
		       (cond ((or (null  (setf names (second eldef)))
				  (eq elt names)
				  (if (listp names)
				      (dolist (name names)
					(when
					    (typecase name
					      (string (equal name (symbol-name elt)))
					      (symbol (eq name elt))
					      (otherwise
					       ;; must be (:any-case string)
					       (string-equal (second name) elt)))
					  (return t)))
				    (and (stringp names)
					 (equal names (symbol-name elt)))))
			      (setf (xmp-cdi-inner cdi) eldef)
			      t))))))))

			     
(defmethod xmp-simple-exel ((conn t) sdef)
  (typecase sdef
    (cons (case (first sdef)
	    (:simple (or (second sdef)
			 (getf (cddr sdef) :simple-content-key)))))))

(defmethod xmp-element-exdef ((conn t) eldef nss &aux ex tp td)
  ;; return two values:   
  ;;        - expected-element
  ;;        - the last element name or type name in the chain
  ;;           leading up to expected-element
  (typecase eldef
    ((member nil) nil)
    (symbol (cond ((null (setf td (xmp-find-type conn eldef nss)))
		   nil)
		  (t (multiple-value-setq (ex tp)
		       (xmp-element-exdef conn td nss))
		     (values ex (or tp eldef)))))
    (cons (case (first eldef)
	    (:element (xmp-element-exdef conn (third eldef) nss))
	    (:simple 
	     (or (multiple-value-setq (ex tp) 
		   (xmp-element-exdef conn (second eldef) nss))
		 (multiple-value-setq (ex tp) 
		   (values (xmp-simple-exel conn eldef) nil))
		 )
	     (values ex tp))
	    (:complex (second eldef))
	    (:array (list :seq* (second eldef)))
	    ))))

(defvar *defined-xmp-elements*
  (vector (make-hash-table :test #'eq)
	  (make-hash-table :test #'equal)
	  (make-hash-table :test #'equalp)
	  
	  ;; Defined Types
	  (make-hash-table :test #'eq)

	  ))

(defmethod xmp-find-element ((conn t) name nss)
  (let* ((dname (xmp-decode-qualified-name conn name nss))
	 (dstring (symbol-name dname)))
    (or (gethash dname (elt *defined-xmp-elements* 0))
	(gethash dstring (elt *defined-xmp-elements* 1))
	(gethash dstring (elt *defined-xmp-elements* 2)))))

(defmethod xmp-find-type ((conn t) name nss)
  (let ((dname (xmp-decode-qualified-name conn name nss)))
    (gethash dname (elt *defined-xmp-elements* 3))))

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
    ((or string symbol) (xmp-decode-qualified-name conn type-spec nss))
    (cons (ecase (first type-spec)
	    (:simple (etypecase (second type-spec)
		       ((or string symbol)
			(xmp-list*
			 type-spec
			 (first type-spec)
			 (xmp-decode-qualified-name
			  conn (second type-spec) nss)
			 (apply 'xmp-normal-options
				conn :type-spec (cddr type-spec) nss options)))))
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
				   &key &allow-other-keys)
  (etypecase complex-def
    (cons
     (ecase (first complex-def)
       ((:seq :seq* :seq1 :seq+ :set :set* :set1 :set+ :or)
	(xmp-list*
	 complex-def
	 (first complex-def)
	 (mapcar
	  #'(lambda (part)
	      (etypecase part
		((or string symbol)
		 (xmp-decode-qualified-name conn part nss))
		(cons (case (first part)
			(:any part)
			(:element
			 (apply 'xmp-normal-element-spec conn part nss options))
			(otherwise
			 (apply 'xmp-normal-complex-def
				conn part nss options))))))
	  (cdr complex-def))))))))


(defmethod xmp-normal-element-spec ((conn t) part nss &rest options)
  (etypecase part
    (cons (ecase (first part)
	    (:any (when (cdr part)
		    (xmp-warning conn "Ignore data in (:any).")
		    (setf part (list :any)))
		  part)
	    (:element
	     (xmp-list*
	      part
	      :element
	      (etypecase (second part)
		((member nil) nil)

		;;??? verify that named elements have consistent
		;;     (ie equal) definitions?

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
    ((or string symbol) (xmp-decode-qualified-name conn part nss))))
		    
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

(defmethod xmp-elt-getf-name ((conn t) plist edef &optional ignore-case &aux v)
  (typecase edef
    (atom (do ((tl plist (cddr tl)))
	      ((atom tl))
	    (when (typecase edef
		    (symbol (eq edef (car tl)))
		    (string (if ignore-case
				(string-equal edef (car tl))
			      (equal edef (symbol-name (car tl))))))
	      (return (car tl)))))
    (otherwise
     (case (car edef)
       (:element (typecase (second edef)
		   (atom (xmp-elt-getf-name conn plist (second edef) ignore-case))
		   (otherwise 
		    (dolist (name (second edef))
		      (when (setf v (xmp-elt-getf-name conn plist 
						       (if (consp name)
							   (second name)
							 name)
						       (consp name)))
			(return v))))))))))



(defmethod define-xmp-type ((conn t) name type-spec &rest options
			       &key (redef :warn) (nss :dns)
			       &allow-other-keys
			       &aux
			       (dname (xmp-decode-qualified-name conn name nss))
			       (odef
				(gethash dname (elt *defined-xmp-elements* 3)))
			       (tdef
				(apply 'xmp-normal-type-spec conn type-spec nss options))
			       )
  (when (and odef (not (equal odef tdef)))
    (case redef
      (:warn (xmp-warning conn "redefining xmp type ~S" dname))
      ((nil) nil)
      (otherwise
       (xmp-error conn :def (list "redefining xmp type ~S" dname)))))
  (setf (gethash dname (elt *defined-xmp-elements* 3))
	(apply 'xmp-normal-type-spec conn type-spec nss options)))

(defmethod define-xmp-element ((conn t) names type-spec &rest options
			       &key (redef :warn) (nss :dns)
			       &allow-other-keys)
  (if (and (consp names) (not (eq :any-case (car names))))
      (mapcar #'(lambda (name)
		  (apply 'define-xmp-element conn name type-spec options))
	      names)
    (let* ((dname names)
	   (table (elt *defined-xmp-elements*
		       (etypecase names
			 (symbol 0)
			 (string 1)
			 (cons (ecase (first names)
				 (:any-case
				  (etypecase (second names)
				    (string
				     (setf dname (second names))
				     2))))))))
	   (odef (gethash dname table))
	   (tdef (apply 'xmp-normal-type-spec conn type-spec nss options))
	   )
      
      (when (and odef (not (equal odef tdef)))
	(case redef
	  (:warn (xmp-warning conn "redefining xmp type ~S" dname))
	  ((nil) nil)
	  (otherwise
	   (xmp-error conn :def (list "redefining xmp type ~S" dname)))))
      (setf (gethash dname table) tdef))))

(defmethod xmp-getf-in-part ((conn t) part name &optional default)
  (getf (typecase part
	  (cons (case (first part)
		  (:element (cdddr part))
		  (otherwise (cddr part))))
	  (otherwise nil))
	name default))

