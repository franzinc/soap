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

;; $Id: xmp-aserve.cl,v 1.1.1.1 2003/07/24 00:49:45 layer Exp $

;; Using AllegroServe as the transport layer.

(in-package :net.xmp)

(eval-when (compile load eval)
  (require :aserve)
  )

(defpackage :net.xmp
  (:use :net.aserve :net.aserve.client :net.html.generator)
  (:export

   ;; Classes
   xmp-aserve-connector

   xmp-aserve-client-connector
   xmp-aserve-server-connector

   xmp-aserve-string-out-connector
   xmp-aserve-stream-out-connector

   xmp-aserve-string-in-connector
   xmp-aserve-event-in-connector

   xmp-aserve-string-in-out-connector
   xmp-aserve-event-string-connector
   xmp-aserve-string-stream-connector
   xmp-aserve-event-stream-connector
   
   xmp-aserve-client-string-out-connector
   xmp-aserve-client-stream-out-connector
   xmp-aserve-client-string-in-connector
   xmp-aserve-client-event-in-connector
   xmp-aserve-client-string-in-out-connector
   xmp-aserve-client-event-string-connector
   xmp-aserve-client-string-stream-connector
   xmp-aserve-client-event-stream-connector

   xmp-aserve-server-string-out-connector
   xmp-aserve-server-stream-out-connector
   xmp-aserve-server-string-in-connector
   xmp-aserve-server-event-in-connector
   xmp-aserve-server-string-in-out-connector
   xmp-aserve-server-event-string-connector
   xmp-aserve-server-string-stream-connector
   xmp-aserve-server-event-stream-connector

   ;; Slot names 
   host agent content-type http-protocol method url

   ;; Accessors

   ;; Generic functions
   
   ;; Methods

   ;; Ordinary functions
   
   ))

(defclass xmp-aserve-connector (xmp-connector) 
  ((transport :initform :aserve)
   (host          :accessor xmp-destination-host :initform nil)
   (agent         :accessor xmp-destination-agent :initform nil)
   (content-type  :accessor xmp-destination-content-type)
   (http-protocol :accessor xmp-destination-http-protocol)
   (method        :accessor xmp-destination-method)
   (url           :accessor xmp-destination-url           :initarg :url)
   (name          :accessor xmp-server-name
		  :initform (format 
			     nil 
			     "AllegroServe/~{~A.~A.~A~}(Allegro Common Lisp)"
			     *aserve-version*))
   (parameters    :accessor xmp-server-parameters :initarg :parameters :initform nil)
   ))


(defclass xmp-aserve-client-connector  (xmp-aserve-connector xmp-client-connector)  ())

(defclass xmp-aserve-server-connector  (xmp-aserve-connector xmp-server-connector)
  ((server    :accessor xmp-aserve-server :initform nil)
   ))

(defclass xmp-aserve-string-out-connector 
  (xmp-aserve-connector xmp-string-out-connector) ())
(defclass xmp-aserve-stream-out-connector  
  (xmp-aserve-connector xmp-stream-out-connector)  ())
(defclass xmp-aserve-string-in-connector   
  (xmp-aserve-connector xmp-string-in-connector)   ())
(defclass xmp-aserve-event-in-connector
  (xmp-aserve-connector xmp-event-in-connector)   ())

(defclass xmp-aserve-string-in-out-connector 
  (xmp-aserve-string-out-connector
   xmp-aserve-string-in-connector
   xmp-string-in-out-connector)   ())
(defclass xmp-aserve-event-string-connector 
  (xmp-aserve-string-out-connector
   xmp-aserve-event-in-connector
   xmp-event-string-connector)   ())
(defclass xmp-aserve-string-stream-connector
  (xmp-aserve-stream-out-connector
   xmp-aserve-string-in-connector
   xmp-string-stream-connector)  ())
(defclass xmp-aserve-event-stream-connector
  (xmp-aserve-stream-out-connector
   xmp-aserve-event-in-connector
   xmp-event-stream-connector)  ())
   
(defclass xmp-aserve-client-string-out-connector
  (xmp-aserve-client-connector xmp-aserve-string-out-connector xmp-client-string-out-connector)
  ())
(defclass xmp-aserve-client-stream-out-connector
  (xmp-aserve-client-connector xmp-aserve-stream-out-connector xmp-client-stream-out-connector)
  ())
(defclass xmp-aserve-client-string-in-connector
  (xmp-aserve-client-connector xmp-aserve-string-in-connector xmp-client-string-in-connector)
  ())
(defclass xmp-aserve-client-event-in-connector
  (xmp-aserve-client-connector xmp-aserve-event-in-connector xmp-client-event-in-connector)
  ())

(defclass xmp-aserve-client-string-in-out-connector
  (xmp-aserve-client-string-out-connector 
   xmp-aserve-client-string-in-connector
   xmp-aserve-string-in-out-connector
   xmp-client-string-in-out-connector
   ) ())
(defclass xmp-aserve-client-event-string-connector
  (xmp-aserve-client-string-out-connector 
   xmp-aserve-client-event-in-connector
   xmp-aserve-event-string-connector
   xmp-client-event-string-connector
   ) ())
(defclass xmp-aserve-client-string-stream-connector
  (xmp-aserve-client-stream-out-connector 
   xmp-aserve-client-string-in-connector
   xmp-aserve-string-stream-connector
   xmp-client-string-stream-connector
   ) ())
(defclass xmp-aserve-client-event-stream-connector
  (xmp-aserve-client-stream-out-connector
   xmp-aserve-client-event-in-connector
   xmp-aserve-event-stream-connector
   xmp-client-event-stream-connector
   ) ())

(defclass xmp-aserve-server-string-out-connector
  (xmp-aserve-server-connector 
   xmp-aserve-string-out-connector
   xmp-server-string-out-connector) ())
(defclass xmp-aserve-server-stream-out-connector
  (xmp-aserve-server-connector
   xmp-aserve-stream-out-connector
   xmp-server-stream-out-connector) ())
(defclass xmp-aserve-server-string-in-connector
  (xmp-aserve-server-connector
   xmp-aserve-string-in-connector
   xmp-server-string-in-connector)   ())
(defclass xmp-aserve-server-event-in-connector
  (xmp-aserve-server-connector
   xmp-aserve-event-in-connector
   xmp-server-event-in-connector)   ())

(defclass xmp-aserve-server-string-in-out-connector 
  (xmp-aserve-server-string-out-connector 
   xmp-aserve-server-string-in-connector
   xmp-aserve-string-in-out-connector 
   xmp-server-string-in-out-connector 
   ) ())
(defclass xmp-aserve-server-event-string-connector
  (xmp-aserve-server-string-out-connector 
   xmp-aserve-server-event-in-connector
   xmp-aserve-event-string-connector
   xmp-server-event-string-connector
   ) ())
(defclass xmp-aserve-server-string-stream-connector
  (xmp-aserve-server-stream-out-connector 
   xmp-aserve-server-string-in-connector
   xmp-aserve-string-stream-connector
   xmp-server-string-stream-connector
   ) ())
(defclass xmp-aserve-server-event-stream-connector
  (xmp-aserve-server-stream-out-connector 
   xmp-aserve-server-event-in-connector
   xmp-aserve-event-stream-connector
   xmp-server-event-stream-connector
   ) ())



(defmethod xmp-external-format ((conn xmp-aserve-connector)) 
  (or (typecase (xmp-xml-encoding conn)
	((member nil) nil)
	(cons (first (xmp-xml-encoding conn)))
	(otherwise (xmp-xml-encoding conn)))
      *default-aserve-external-format*))

(defmethod xmp-message-send ((conn xmp-aserve-client-string-in-out-connector)
				   &key headers &allow-other-keys)
  (multiple-value-bind (s rc)
      (do-http-request
       (xmp-destination-url conn)
       :method (xmp-destination-method conn)
       :protocol (xmp-destination-http-protocol conn) 
       :content-type (xmp-destination-content-type conn)
       :content (xmp-message-string conn)
       :user-agent (xmp-destination-agent conn)
       :headers (append (let ((host (xmp-destination-host conn))) 
			  (when host `(("Host" . ,host))))
			headers)
       :external-format (xmp-external-format conn)
       )
    (case rc
      (200 s)
      (500 s)
      (otherwise
       (xmp-error conn :server :sub-code :http-response-code :detail (list rc s)))
      )))

(defmethod xmp-message-send ((conn xmp-aserve-server-string-in-out-connector)
			     &key request entity &allow-other-keys)
  (with-http-response 
   (request entity)
   (with-http-body 
    (request entity 
	     :headers `((:server . ,(xmp-server-name conn))))
    (html (:princ (xmp-message-string conn))))))


(defmethod xmp-start-server ((conn xmp-aserve-server-connector) 
			     &key (start nil s-p) enable &allow-other-keys)
  (setf (xmp-aserve-server conn)
	(apply 'start
	       (append
		(when (xmp-aserve-server conn)
		  (list :start (xmp-aserve-server conn)))
		(if s-p start (xmp-server-start conn)))))
  (when enable (xmp-enable-server conn))
  conn)

(defmethod xmp-stop-server ((conn xmp-aserve-server-connector)
			    &key disable &allow-other-keys)
  (when disable (xmp-disable-server conn))
  (when (xmp-aserve-server conn)
    (shutdown :server (xmp-aserve-server conn)))
  conn)
  

(defmethod xmp-enable-server :around ((server xmp-aserve-server-connector)
				     &key &allow-other-keys)
  (call-next-method)
  (apply #'publish     
	 :server (xmp-aserve-server server)
	 :function #'(lambda (request entity) 
		       (xmp-server-implementation server (get-request-body request))
		       (xmp-message-send server :request request :entity entity))
	 :content-type "text/xml"
	 (xmp-server-parameters server))
  server)
