;; -*- mode: common-lisp; package: net.xmp.xmlrpc -*-
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

;; $Id: xmp-xmlrpc.cl,v 2.0 2004/01/14 18:31:55 layer Exp $

;; Support for XMLRPC protocol over HTTP.

(defpackage :net.xmp.xmlrpc)
(in-package :net.xmp.xmlrpc)



(defpackage :net.xmp.xmlrpc
  (:use :common-lisp :excl :net.xmp)
  (:export

   xmlrpc-error

   xmlrpc-message-client
   xmlrpc-message-server

   xmlrpc-connector

   xmlrpc-client-connector
   xmlrpc-server-connector

   xmlrpc-string-out-connector
   xmlrpc-stream-out-connector

   xmlrpc-string-in-connector
   xmlrpc-event-in-connector

   xmlrpc-string-in-out-connector
   xmlrpc-event-string-connector
   xmlrpc-string-stream-connector
   xmlrpc-event-stream-connector
   
   xmlrpc-client-string-out-connector
   xmlrpc-client-stream-out-connector
   xmlrpc-client-string-in-connector
   xmlrpc-client-event-in-connector
   xmlrpc-client-string-in-out-connector
   xmlrpc-client-event-string-connector
   xmlrpc-client-string-stream-connector
   xmlrpc-client-event-stream-connector

   xmlrpc-server-string-out-connector
   xmlrpc-server-stream-out-connector
   xmlrpc-server-string-in-connector
   xmlrpc-server-event-in-connector
   xmlrpc-server-string-in-out-connector
   xmlrpc-server-event-string-connector
   xmlrpc-server-string-stream-connector
   xmlrpc-server-event-stream-connector

   xmlrpc-aserve-connector

   xmlrpc-aserve-client-connector
   xmlrpc-aserve-server-connector

   xmlrpc-aserve-string-out-connector
   xmlrpc-aserve-stream-out-connector

   xmlrpc-aserve-string-in-connector
   xmlrpc-aserve-event-in-connector

   xmlrpc-aserve-string-in-out-connector
   xmlrpc-aserve-event-string-connector
   xmlrpc-aserve-string-stream-connector
   xmlrpc-aserve-event-stream-connector
   
   xmlrpc-aserve-client-string-out-connector
   xmlrpc-aserve-client-stream-out-connector
   xmlrpc-aserve-client-string-in-connector
   xmlrpc-aserve-client-event-in-connector
   xmlrpc-aserve-client-string-in-out-connector
   xmlrpc-aserve-client-event-string-connector
   xmlrpc-aserve-client-string-stream-connector
   xmlrpc-aserve-client-event-stream-connector

   xmlrpc-aserve-server-string-out-connector
   xmlrpc-aserve-server-stream-out-connector
   xmlrpc-aserve-server-string-in-connector
   xmlrpc-aserve-server-event-in-connector
   xmlrpc-aserve-server-string-in-out-connector
   xmlrpc-aserve-server-event-string-connector
   xmlrpc-aserve-server-string-stream-connector
   xmlrpc-aserve-server-event-stream-connector


   ))
 
(defclass xmlrpc-connector (xmp-connector)
  ((trim-whitespace :initform t)
   (lisp-package :initform :keyword)
   ))

(defclass xmlrpc-client-connector  (xmlrpc-connector xmp-client-connector)  ())
(defclass xmlrpc-server-connector  (xmlrpc-connector xmp-server-connector)  ())

(defclass xmlrpc-string-out-connector 
  (xmlrpc-connector xmp-string-out-connector) ())
(defclass xmlrpc-stream-out-connector  
  (xmlrpc-connector xmp-stream-out-connector) ())
(defclass xmlrpc-string-in-connector   
  (xmlrpc-connector xmp-string-in-connector)  ())
(defclass xmlrpc-event-in-connector
  (xmlrpc-connector xmp-event-in-connector)   ())

(defclass xmlrpc-string-in-out-connector 
  (xmlrpc-string-out-connector 
   xmlrpc-string-in-connector 
   xmp-string-in-out-connector)  ())
(defclass xmlrpc-event-string-connector 
  (xmlrpc-string-out-connector 
   xmlrpc-event-in-connector 
   xmp-event-string-connector)   ())
(defclass xmlrpc-string-stream-connector
  (xmlrpc-stream-out-connector  
   xmlrpc-string-in-connector 
   xmp-string-stream-connector)  ())
(defclass xmlrpc-event-stream-connector
  (xmlrpc-stream-out-connector 
   xmlrpc-event-in-connector 
   xmp-event-stream-connector)   ())
   
(defclass xmlrpc-client-string-out-connector
  (xmlrpc-client-connector 
   xmlrpc-string-out-connector 
   xmp-client-string-out-connector) ())
(defclass xmlrpc-client-stream-out-connector
  (xmlrpc-client-connector 
   xmlrpc-stream-out-connector 
   xmp-client-stream-out-connector) ())
(defclass xmlrpc-client-string-in-connector
  (xmlrpc-client-connector 
   xmlrpc-string-in-connector
   xmp-client-string-in-connector)  ())
(defclass xmlrpc-client-event-in-connector
  (xmlrpc-client-connector 
   xmlrpc-event-in-connector 
   xmp-client-event-in-connector)   ())

(defclass xmlrpc-client-string-in-out-connector
  (xmlrpc-client-string-out-connector 
   xmlrpc-client-string-in-connector
   xmlrpc-string-in-out-connector
   xmp-client-string-in-out-connector) ())
(defclass xmlrpc-client-event-string-connector
  (xmlrpc-client-string-out-connector 
   xmlrpc-client-event-in-connector
   xmlrpc-event-string-connector
   xmp-client-event-string-connector)  ())
(defclass xmlrpc-client-string-stream-connector
  (xmlrpc-client-stream-out-connector 
   xmlrpc-client-string-in-connector
   xmlrpc-string-stream-connector
   xmp-client-string-stream-connector) ())
(defclass xmlrpc-client-event-stream-connector
  (xmlrpc-client-stream-out-connector
   xmlrpc-client-event-in-connector
   xmlrpc-event-stream-connector
   xmp-client-event-stream-connector) ())

(defclass xmlrpc-server-string-out-connector
  (xmlrpc-server-connector 
   xmlrpc-string-out-connector
   xmp-server-string-out-connector) ())
(defclass xmlrpc-server-stream-out-connector
  (xmlrpc-server-connector
   xmlrpc-stream-out-connector
   xmp-server-stream-out-connector) ())
(defclass xmlrpc-server-string-in-connector
  (xmlrpc-server-connector
   xmlrpc-string-in-connector
   xmp-server-string-in-connector)   ())
(defclass xmlrpc-server-event-in-connector
  (xmlrpc-server-connector
   xmlrpc-event-in-connector
   xmp-server-event-in-connector)   ())

(defclass xmlrpc-server-string-in-out-connector 
  (xmlrpc-server-string-out-connector 
   xmlrpc-server-string-in-connector
   xmlrpc-string-in-out-connector 
   xmp-server-string-in-out-connector) ())
(defclass xmlrpc-server-event-string-connector
  (xmlrpc-server-string-out-connector 
   xmlrpc-server-event-in-connector
   xmlrpc-event-string-connector
   xmp-server-event-string-connector) ())
(defclass xmlrpc-server-string-stream-connector
  (xmlrpc-server-stream-out-connector 
   xmlrpc-server-string-in-connector
   xmlrpc-string-stream-connector
   xmp-server-string-stream-connector) ())
(defclass xmlrpc-server-event-stream-connector
  (xmlrpc-server-stream-out-connector 
   xmlrpc-server-event-in-connector
   xmlrpc-event-stream-connector
   xmp-server-event-stream-connector) ())




(defclass xmlrpc-aserve-connector (xmlrpc-connector xmp-aserve-connector)
  ((method        :initform :post)
   (http-protocol :initform :http/1.0)
   (content-type  :initform "text/xml")
   (leader        :initform "<?xml version=\"1.0\"?>")
   ))

(defclass xmlrpc-aserve-client-connector
  (xmlrpc-aserve-connector 
   xmp-aserve-client-connector
   xmlrpc-client-connector)  ())
(defclass xmlrpc-aserve-server-connector
  (xmlrpc-aserve-connector
   xmp-aserve-server-connector
   xmlrpc-server-connector)  ())

(defclass xmlrpc-aserve-string-out-connector
  (xmlrpc-aserve-connector
   xmp-aserve-string-out-connector
   xmlrpc-string-out-connector) ())
(defclass xmlrpc-aserve-stream-out-connector
  (xmlrpc-aserve-connector
   xmp-aserve-stream-out-connector
   xmlrpc-stream-out-connector)  ())
(defclass xmlrpc-aserve-string-in-connector
  (xmlrpc-aserve-connector
   xmp-aserve-string-in-connector
   xmlrpc-string-in-connector)   ())
(defclass xmlrpc-aserve-event-in-connector 
  (xmlrpc-aserve-connector
   xmp-aserve-event-in-connector
   xmlrpc-event-in-connector)   ())

(defclass xmlrpc-aserve-string-in-out-connector 
  (xmlrpc-aserve-string-out-connector
   xmlrpc-aserve-string-in-connector
   xmp-aserve-string-in-out-connector
   xmlrpc-string-in-out-connector) ())
(defclass xmlrpc-aserve-event-string-connector 
  (xmlrpc-aserve-string-out-connector
   xmlrpc-aserve-event-in-connector
   xmp-aserve-event-string-connector
   xmlrpc-event-string-connector)  ())
(defclass xmlrpc-aserve-string-stream-connector
  (xmlrpc-aserve-stream-out-connector
   xmlrpc-aserve-string-in-connector
   xmp-aserve-string-stream-connector
   xmlrpc-string-stream-connector) ())
(defclass xmlrpc-aserve-event-stream-connector
  (xmlrpc-aserve-stream-out-connector
   xmlrpc-aserve-event-in-connector
   xmp-aserve-event-stream-connector
   xmlrpc-event-stream-connector)  ())
   
(defclass xmlrpc-aserve-client-string-out-connector
  (xmlrpc-aserve-client-connector
   xmlrpc-aserve-string-out-connector
   xmp-aserve-client-string-out-connector
   xmlrpc-client-string-out-connector) ())
(defclass xmlrpc-aserve-client-stream-out-connector
  (xmlrpc-aserve-client-connector
   xmlrpc-aserve-stream-out-connector
   xmp-aserve-client-stream-out-connector
   xmlrpc-client-stream-out-connector) ())
(defclass xmlrpc-aserve-client-string-in-connector
  (xmlrpc-aserve-client-connector
   xmlrpc-aserve-string-in-connector
   xmp-aserve-client-string-in-connector
   xmlrpc-client-string-in-connector)  ())
(defclass xmlrpc-aserve-client-event-in-connector
  (xmlrpc-aserve-client-connector
   xmlrpc-aserve-event-in-connector
   xmp-aserve-client-event-in-connector
   xmlrpc-client-event-in-connector)   ())

(defclass xmlrpc-aserve-client-string-in-out-connector
  (xmlrpc-aserve-client-string-out-connector 
   xmlrpc-aserve-client-string-in-connector
   xmlrpc-aserve-string-in-out-connector
   xmp-aserve-client-string-in-out-connector
   xmlrpc-client-string-in-out-connector) ())
(defclass xmlrpc-aserve-client-event-string-connector
  (xmlrpc-aserve-client-string-out-connector 
   xmlrpc-aserve-client-event-in-connector
   xmlrpc-aserve-event-string-connector
   xmp-aserve-client-event-string-connector
   xmlrpc-client-event-string-connector) ())
(defclass xmlrpc-aserve-client-string-stream-connector
  (xmlrpc-aserve-client-stream-out-connector 
   xmlrpc-aserve-client-string-in-connector
   xmlrpc-aserve-string-stream-connector
   xmp-aserve-client-string-stream-connector
   xmlrpc-client-string-stream-connector) ())
(defclass xmlrpc-aserve-client-event-stream-connector
  (xmlrpc-aserve-client-stream-out-connector
   xmlrpc-aserve-client-event-in-connector
   xmlrpc-aserve-event-stream-connector
   xmp-aserve-client-event-stream-connector
   xmlrpc-client-event-stream-connector) ())

(defclass xmlrpc-aserve-server-string-out-connector
  (xmlrpc-aserve-server-connector 
   xmlrpc-aserve-string-out-connector
   xmp-aserve-server-string-out-connector
   xmlrpc-server-string-out-connector) ())
(defclass xmlrpc-aserve-server-stream-out-connector
  (xmlrpc-aserve-server-connector
   xmlrpc-aserve-stream-out-connector
   xmp-aserve-server-stream-out-connector
   xmlrpc-server-stream-out-connector) ())
(defclass xmlrpc-aserve-server-string-in-connector
  (xmlrpc-aserve-server-connector
   xmlrpc-aserve-string-in-connector
   xmp-aserve-server-string-in-connector
   xmlrpc-server-string-in-connector)   ())
(defclass xmlrpc-aserve-server-event-in-connector
  (xmlrpc-aserve-server-connector
   xmlrpc-aserve-event-in-connector
   xmp-aserve-server-event-in-connector
   xmlrpc-server-event-in-connector)   ())

(defclass xmlrpc-aserve-server-string-in-out-connector 
  (xmlrpc-aserve-server-string-out-connector 
   xmlrpc-aserve-server-string-in-connector
   xmlrpc-aserve-string-in-out-connector 
   xmp-aserve-server-string-in-out-connector 
   xmlrpc-server-string-in-out-connector) ())
(defclass xmlrpc-aserve-server-event-string-connector
  (xmlrpc-aserve-server-string-out-connector 
   xmlrpc-aserve-server-event-in-connector
   xmlrpc-aserve-event-string-connector
   xmp-aserve-server-event-string-connector
   xmlrpc-server-event-string-connector) ())
(defclass xmlrpc-aserve-server-string-stream-connector
  (xmlrpc-aserve-server-stream-out-connector 
   xmlrpc-aserve-server-string-in-connector
   xmlrpc-aserve-string-stream-connector
   xmp-aserve-server-string-stream-connector
   xmlrpc-server-string-stream-connector) ())
(defclass xmlrpc-aserve-server-event-stream-connector
  (xmlrpc-aserve-server-stream-out-connector 
   xmlrpc-aserve-server-event-in-connector
   xmlrpc-aserve-event-stream-connector
   xmp-aserve-server-event-stream-connector
   xmlrpc-server-event-stream-connector) ())

(define-condition xmlrpc-condition (xmp-condition) 
  ((xmp-error-code :initform :xml-rpc)))
(defmethod xmlrpc-error ((conn xmlrpc-connector) &rest keys)
  (apply 'xmp-error conn 'xmlrpc-condition keys))

(defun xmlrpc-message-client (&rest options)
  (apply 'xmp-make-connector :xmlrpc :aserve :client :string :string options))

(defmethod xmp-make-connector ((protocol  (eql :xmlrpc)) 
			       (transport (eql :aserve))
			       (role      (eql :client))
			       (receiver  (eql :string)) 
			       (sender    (eql :string)) 
			       &rest options  &key &allow-other-keys)
  (apply 'make-instance 'xmlrpc-aserve-client-string-in-out-connector 
	 options))


#+ignore
(defmethod xmp-make-connector ((protocol  (eql :xmlrpc)) 
			       (transport (eql :aserve))
			       (role      (eql :server))
			       (receiver  (eql :string)) 
			       (sender    (eql :string)) 
			       &rest options &key &allow-other-keys)
  )
#+ignore
(defmethod xmp-make-connector ((protocol  (eql :xmlrpc)) 
			       (transport (eql :aserve))
			       (role      (eql :server))
			       (receiver  (eql :event)) 
			       (sender    (eql :string)) 
			       &rest options &key &allow-other-keys)
  )

#+ignore
(defmethod xmp-make-connector ((protocol  (eql :xmlrpc)) 
			       (transport (eql :aserve))
			       (role      (eql :server))
			       (receiver  (eql :string)) 
			       (sender    (eql :stream)) 
			       &rest options &key &allow-other-keys)
  )

#+ignore
(defmethod xmp-make-connector ((protocol  (eql :xmlrpc)) 
			       (transport (eql :aserve))
			       (role      (eql :server))
			       (receiver  (eql :event)) 
			       (sender    (eql :stream)) 
			       &rest options &key &allow-other-keys)
  )

(defmethod xmp-call-method ((conn xmlrpc-client-connector)
			    method &rest args)
  (xmp-message-begin conn)
  (xmp-encode-content conn (xmp-destination-leader conn) :sanitize nil)
  (xmp-encode-begin conn :|methodCall|)
  (xmp-encode-content conn method :tag1 :|methodName|)
  (xmp-encode-begin conn :|params|)
  (dolist (a args) 
    (xmp-encode-begin conn :|param|)
    (xmp-encode conn a nil)
    (xmp-encode-end conn :|param|))
  (xmp-encode-end conn :|params|)
  (xmp-encode-end conn :|methodCall|)
  (xmp-decode-message conn (xmp-parse-message conn (xmp-message-send conn))))


(defmethod xmp-begin-message ((conn xmlrpc-client-string-in-connector))
  (list :seq1 :|methodResponse|))

(defmethod xmp-end-message ((conn xmlrpc-string-in-connector) data
			    &key types &allow-other-keys)
  (cond ((cdr data)
	 (xmlrpc-error conn :string "Too many elements in message"))
	(data (values (car data) (car types)))
	(t (values))))


(eval-when (compile eval)
  (setf xmp-simple-begin-class 'xmlrpc-aserve-string-in-out-connector)
  )

(define-xmp-element nil ':|methodResponse| '(:complex (:or :|fault| :|params|)))
(define-xmp-element nil ':|fault|          '(:complex (:seq1 :|value|)))
(define-xmp-element nil ':|params|         '(:complex (:seq* :|param|)))
(define-xmp-element nil ':|param|          '(:complex (:seq1 :|value|)))
(define-xmp-element nil ':|value|          '(:complex
					     (:or :|int| :|i4| :|string| :|boolean|
						  :|double| 
						  :|dateTime.iso8601| :|base64|
						  :|struct| :|array|)))
(define-xmp-element nil ':|struct|         '(:complex (:seq* :|member|)))
(define-xmp-element nil ':|member|         '(:complex (:seq1 :|name| :|value|)))
(define-xmp-element nil ':|array|          '(:complex (:seq1 :|data|)))
(define-xmp-element nil ':|data|           '(:complex (:seq* :|value|)))

(define-xmp-element nil ':|int|              '(:simple nil
						       :simple-content-key :|int|))
(define-xmp-element nil ':|i4|               '(:simple nil
						       :simple-content-key :|i4|))
(define-xmp-element nil ':|string|           '(:simple nil
						       :simple-content-key :|string|))
(define-xmp-element nil ':|boolean|          '(:simple nil
						       :simple-content-key :|boolean|))
(define-xmp-element nil ':|double|           '(:simple nil
						       :simple-content-key :|double|))
(define-xmp-element nil ':|dateTime.iso8601| '(:simple nil
						       :simple-content-key
						       :|dateTime.iso8601|))
(define-xmp-element nil ':|base64|           '(:simple nil
						       :simple-content-key :|base64|))
(define-xmp-element nil ':|name|             '(:simple nil
						       :simple-content-key :|name|))



(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|int|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  (values (or (ignore-errors (parse-integer data)) (call-next-method))))

(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|i4|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  (values (or (ignore-errors (parse-integer data)) (call-next-method))))

(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|string|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  data)

(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|boolean|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  (cond ((equal data "0") nil)
	((equal data "1") t)
	(t (call-next-method))))

(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|double|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  ;; Make sure that the data will be parsed as a double-float
  (values (read-from-string (concatenate 'string data "d0") nil nil) :|double|))

(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|dateTime.iso8601|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  ;;??? signal better error if ill-formed...
  (values
   (let* (y m d (h 12) (mn 0) (s 0) 
	    (ds data) 
	    (l (length ds)))
     (setf y (read-from-string ds nil nil :start 0 :end 4)
	   m (read-from-string ds nil nil :start 4 :end 6)
	   d (read-from-string ds nil nil :start 6 :end 8))
     (when (and (< 8 l) (equalp #\t (elt ds 8)))
       (setf h (read-from-string ds nil nil :start 9 :end 11))
       (when (and (< 11 l) (equalp #\: (elt ds 11)))
	 (setf mn (read-from-string ds nil nil :start 12 :end 14))
	 (when (and (< 14 l) (equalp #\: (elt ds 14)))
	   (setf s (read-from-string ds nil nil :start 15 :end 17))
	   )))
     (if (< 1899 y)
	 (encode-universal-time s mn h d m y)
       (list s mn h d m y)))
   :|date|))

(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|base64|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  (decode-base64-string data)
  )

(defmethod xmp-simple-content ((conn xmlrpc-string-in-connector) 
			       (elt (eql :|name|))
			       data &rest options &key &allow-other-keys)
  (declare (ignore options))
  (list elt data)
  )

(defmethod xmp-complex-content ((conn xmlrpc-string-in-connector) 
				(elt (eql :|member|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (if (and
       (cond ((atom data) nil)
	     ((atom (cdr data)) nil)
	     ((consp (cddr data)) nil)
	     ((consp (car data)) data)
	     (t (setf data (list (second data) (first data)))))
       (eq :|name| (first (first data))))
      (list (list (second (first data)) (second data)))
    (xmlrpc-error conn :string `("Ill-formed member: ~S" data))))

(defmethod xmp-complex-content ((conn xmlrpc-string-in-connector) 
				(elt (eql :|struct|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (list (list* elt data)))

(defmethod xmp-complex-content ((conn xmlrpc-string-in-connector) 
				(elt (eql :|data|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (list data))

(defmethod xmp-complex-content ((conn xmlrpc-string-in-connector) 
				(elt (eql :|array|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (if (cdr data)
      (xmlrpc-error conn :string `("More than one element in <array> element: ~S" data))
    data))

(defmethod xmp-complex-content ((conn xmlrpc-string-in-connector) 
				(elt (eql :|fault|)) data
				&rest options &key &allow-other-keys)
  (declare (ignore options))
  (list (list* elt data)))


;; in XMLRPC we allow the calling program to use sloppy type keywords 
;;    when encoding
(defmethod xmp-encode ((dest xmlrpc-connector) data (type t)
		       &rest options &key &allow-other-keys &aux name low)
  (if (and type (symbolp type)
	   (not (equal (setf name (symbol-name type)) 
		       (setf low (string-downcase name)))))
      (apply 'xmp-encode dest data (intern low :keyword) options)
    (call-next-method)))


(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|int|))
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content dest (format nil "~A" (truncate data)) :tag1 :|value| :tag2 :|i4|)
  :|int|)

(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|truncate|))
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content dest (format nil "~A" (truncate data)) :tag1 :|value| :tag2 :|i4|)
  :|truncate|)

(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|round|))
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content dest (format nil "~A" (round data)) :tag1 :|value| :tag2 :|i4|)
  :|round|)

(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|string|))
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content dest (format nil "~A" data) :tag1 :|value| :tag2 :|string|)
  :|string|)

(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|boolean|))
		        &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content dest (if data "1" "0") :tag1 :|value| :tag2 :|boolean|)
  :|boolean|)

(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|double|))
		       &rest options &key &allow-other-keys)
  (apply 'xmp-encode dest (coerce data 'double-float) nil options)
  :|double|)

(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|date|))
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content 
   dest 
   (multiple-value-bind (s m h dy mo yr)
       (typecase data
	 (cons (values-list data))
	 (integer (decode-universal-time data))
	 ((member nil :now) (get-decoded-time))
	 (otherwise (xmlrpc-error dest :string `("Cannot encode date ~S" data))))
     (format nil 
	     "~4,'0D~2,'0D~2,'0DT~2,'0D:~2,'0D:~2,'0D"
	     yr mo dy h m s))
   :tag1 :|value| :tag2 :|dateTime.iso8601|)
  :|date|)

(defmethod xmp-encode ((dest xmlrpc-connector) data (type (eql :|base64|))
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content 
   dest (encode-base64-string data) :tag1 :|value| :tag2 :|base64|)
  :|base64|)


(defmethod xmp-encode ((conn xmlrpc-connector) data (type (eql :|struct|))
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  ;; data is list of (name value [type]) pairs
  (when (eq :|struct| (car data)) (pop data))
  (xmp-encode-begin conn :|value|)
  (xmp-encode-begin conn :|struct|)
  (mapc #'(lambda (m)
	    (if (consp m)
		(xmp-encode conn (second m) :|member| 
			    :name (first m) :type (third m))
	      (xmp-encode conn m nil)))
	data)
  (xmp-encode-end conn :|struct|)
  (xmp-encode-end conn :|value|)
  :|struct|)

(defmethod xmp-encode ((conn xmlrpc-connector) data (dtype (eql :|member|))
		       &rest options &key name type &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-begin conn :|member|)
  (xmp-encode-content conn name :tag1 :|name|)
  (xmp-encode conn data type)
  (xmp-encode-end conn :|member|)
  :|member|)


(defmethod xmp-encode ((conn xmlrpc-connector) data (dtype (eql :|array|))
		       &rest options &key type &allow-other-keys)
  (declare (ignore options))
  ;; data is list of elements or if type is :pairs then (value type)
  (xmp-encode-begin conn :|value|)
  (xmp-encode-begin conn :|array|)
  (xmp-encode-begin conn :|data|)
  (dotimes (i (length data))
    (let ((m (elt data i)))
      (case type
	(:pairs (xmp-encode conn (first m) (second m)))
	(otherwise (xmp-encode conn m type)))))
  (xmp-encode-end conn :|data|)
  (xmp-encode-end conn :|array|)
  (xmp-encode-end conn :|value|)
  :|array|)


(defmethod xmp-encode ((dest xmlrpc-connector) (data integer) (type null)
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content dest (format nil "~A" data) :tag1 :|value| :tag2 :|i4|)
  :|int|)

(defmethod xmp-encode ((dest xmlrpc-connector) (data float) (type null)
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (let ((f (coerce data 'double-float)))
    (xmp-encode-content
     dest 
     (string-trim " "
		  (format nil "~VF" 
			  (+ 
			   ;; This is conservatively the most significant digits
			   ;; in a double-float number, plus room for a leading or
			   ;; trailing zero.
			   22
			   ;; This is conservatively the most leading or trailing
			   ;; zeroes that will be printed.
			   (abs (truncate (log (abs f) 10))))
			  f))
     :tag1 :|value| :tag2 :|double|)
    :|double|))

(defmethod xmp-encode ((dest xmlrpc-connector) (data string) (type null)
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode-content dest data :tag1 :|value| :tag2 :|string|)
  :|string|)

(defmethod xmp-encode ((dest xmlrpc-connector) (data sequence) (type null)
		       &rest options &key &allow-other-keys)
  (if (and (consp data) (eq :|struct| (car data)))
      (apply 'xmp-encode dest data :|struct| options)
    (apply 'xmp-encode dest data :|array| options)))

#+ignore
(defmethod xmp-encode ((dest xmlrpc-connector) (data xmp-struct) (type null)
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode dest (xmp-members data) :|struct|)
  :|struct|)

#+ignore
(defmethod xmp-encode ((dest xmlrpc-connector) (data xmp-array) (type null)
		       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (xmp-encode dest (xmp-members data) :|array|)
  :|array|)

#+ignore
(defmethod xmp-encode ((dest xmlrpc-connector) (data xmp-member) (type null)
		       &rest options &key &allow-other-keys &aux name)
  (declare (ignore options))
  (etypecase (setf name (xmp-element-name data))
    (string  (xmp-encode dest (xmp-lisp-value data) 
		 	 :|member| :name name :type (xmp-element-type data)))
    (integer (xmp-encode dest (xmp-lisp-value data) (xmp-element-type data))))
  :|member|)



;;; XMLRPC Server

(defun xmlrpc-message-server (&key start 
				   (enable :start) (introspect t)
				   publish 
				   (class 
				    'xmlrpc-aserve-server-string-in-out-connector)
				   )
  (or (member :path publish) (setq publish (list* :path "/RPC2" publish)))
  (when (or (member :function publish) (member :content-type publish))
    (xmlrpc-error nil :sub-code :argument-error
		  :string "Server does not allow :function or :content-type arguments"))
  (let ((server (make-instance class :start start :parameters publish)))
    (when introspect (xmp-export-standard-methods server :enable t))
    (case enable 
      ((nil) nil)
      (:start (xmp-start-server server :enable t))
      (otherwise (xmp-enable-server server)))
    server))


(define-xmp-element nil ':|methodCall|   '(:complex (:set1 :|methodName| :|params|)))

(defmethod xmp-begin-message ((conn xmlrpc-server-string-in-connector))
  (list :seq1 :|methodCall|))

(defmethod xmp-complex-content ((server xmlrpc-server-connector) 
				(elt (eql :|methodName|))
				data &rest options &key &allow-other-keys)
  (declare (ignore options))
  (list (cons elt data)))

(defmethod xmp-complex-content ((server xmlrpc-server-connector) 
				(elt (eql :|value|))
				data &rest options &key types &allow-other-keys)
  (declare (ignore options))
  (values data (if (or (null types) (null (car types)) (eq elt (car types)))
		   :|string|
		   (car types))))

(defmethod xmp-complex-content ((server xmlrpc-server-connector) 
				(elt (eql :|params|))
				data &rest options &key types &allow-other-keys)
  (declare (ignore options))
  (list (list elt data types)))

(defmethod xmp-complex-content ((server xmlrpc-server-connector) 
				(elt (eql :|methodCall|))
				data &rest options &key &allow-other-keys)
  (declare (ignore options))
  (let* ((name (assoc :|methodName| data))
	 (params (assoc :|params| data)))
    (if (and name params)
	(list (list :method (second name) (third params) (second params)))
      (xmlrpc-error server :string "Ill-formed call"))))


(defmethod xmp-export-standard-methods ((server xmlrpc-server-connector)
					&key enable &allow-other-keys)
  (xmp-export-method server "system.listMethods" nil
		     :lisp-name 'xmp-list-methods
		     :return :|array|
		     :enable enable
		     :help "Returns an array of the names of all the exported methods on the server."
		     )
  (xmp-export-method server "system.methodSignature" '(:|string|)
		     :lisp-name 'xmp-method-signature
		     :return :|array|
		     :enable enable
		     :help "Returns an array of signature arrays: ((return-type arg-type ...) ...)."
		     )
  (xmp-export-method server "system.methodHelp" '(:|string|)
		     :lisp-name 'xmp-method-help
		     :return :|string|
		     :enable enable
		     :help "Returns a string describing a method.")
  t)

(defmethod xmp-invoke-method ((server xmlrpc-server-connector) fn args)
  (case fn
    ((xmp-list-methods xmp-method-signature xmp-method-help)
     (apply fn server args))
    (otherwise (call-next-method))))


(defmethod xmp-server-implementation ((server xmlrpc-aserve-server-connector) body
				      &rest options &key &allow-other-keys)
  (declare (ignore options))
  ;; parse an XML rpc call and pass it to the exported function
  (when (xmp-server-enabled server)
    (let* (code string rval)
      (multiple-value-bind (v e)
	  (ignore-errors
	    (let* ((r (xmp-decode-message server (xmp-parse-message server body)))
		   (method (second r))
		   (signature (third r))
		   (params (fourth r))
		   )
	      (multiple-value-bind (fn rt)
		  (xmp-accept-method server method signature params)
		(let ((vals (multiple-value-list
			     (xmp-invoke-method server fn params))))
		  (if (null vals)
		      (xmlrpc-error server :string "Call refused")
		    (setf rval (xmp-encode-object server (first vals) rt))))
		)
	      nil))
	(declare (ignore v))
	(when e
	  (setf code 2
		string (format nil "Error in RPC call: ~A" e)))
	(xmp-message-begin server)
	(xmp-encode-content server (xmp-destination-leader server) :sanitize nil)
	(xmp-encode-begin server :|methodResponse|)
	(if* code
	     then
	     ;; emit fault
	     (xmp-encode-begin server :|fault|)
	     (xmp-encode server 
			 (list (list "faultCode" code)
			       (list "faultString" string))
			 :|struct|)
	     (xmp-encode-end server :|fault|)
	     else
	     ;; emit value
	     (xmp-encode-begin server :|params|)
	     (xmp-encode-begin server :|param|)
	     (xmp-encode server rval nil)
	     (xmp-encode-end server :|param|)
	     (xmp-encode-end server :|params|)
	     )
	(xmp-encode-end server :|methodResponse|)
	))))


(defmethod xmp-signature-equal ((conn xmlrpc-connector) s1 s2)
  (cond	((eq s1 :|int|) (eq s2 :|i4|))
	((eq s2 :|int|) (eq s1 :|i4|))
	((eq s1 :|date|) (eq s2 :|dateTime.iso8601|))
	((eq s2 :|date|) (eq s1 :|dateTime.iso8601|))
	(t (call-next-method))))



