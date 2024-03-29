;; -*- mode: common-lisp; package: net.xmp -*-
;;
;; See the file LICENSE for the full license governing this code.


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
   xmp-destination-url
   xmp-more-headers

   ;; Generic functions
   xmp-server-response 
   
   ;; Ordinary functions
   xmp-header-slot
   
   ))


(def-xmp-sub-classes ("xmp-aserve" "connector") (("xmp" "connector"))
  (nil
   ((transport :initform :aserve)
    (host          :accessor xmp-destination-host
		   :initarg :http-host :initform nil)
    (agent         :accessor xmp-destination-agent
		   :initarg :http-agent :initform nil)
    (content-type  :accessor xmp-destination-content-type 
		   :initarg :content-type :initform nil)
    (http-protocol :accessor xmp-destination-http-protocol
		   :initarg :http-protocol :initform nil)
    (method        :accessor xmp-destination-method
		   :initarg :http-method :initform nil)
    (more-headers  :accessor xmp-more-headers :initarg :http-headers :initform nil)
    (url           :accessor xmp-destination-url    :initform nil :initarg :url)
    (name          :accessor xmp-server-name
		   :initform (format 
			      nil 
			      "AllegroServe/~{~A.~A.~A~}(Allegro Common Lisp)"
			      *aserve-version*)
		   :documentation "no-xmp-copy")
    (parameters    :accessor xmp-server-parameters :initarg :parameters :initform nil)
    (request       :accessor aserve-request :initform nil
		   :documentation "no-xmp-copy")
    ))

  ("server"
   ((server    :accessor xmp-aserve-server :initform nil)
    ))

  )



(defmethod xmp-external-format ((conn xmp-aserve-connector)) 
  (or (typecase (xmp-xml-encoding conn)
	((member nil) nil)
	(cons (first (xmp-xml-encoding conn)))
	(otherwise (xmp-xml-encoding conn)))
      *default-aserve-external-format*))

(defmethod xmp-message-send ((conn xmp-aserve-client-string-in-out-connector)
				   &key headers &allow-other-keys)
  (setf (xmp-received-string conn)
	;; Set to nil to avoid confusing old reply for reply
	;;  of failed http request.
	nil)
  (multiple-value-bind (s rc)
      (apply 
       'do-http-request
       (xmp-destination-url conn)
       :method (xmp-destination-method conn)
       :protocol (xmp-destination-http-protocol conn) 
       :content-type (xmp-destination-content-type conn)
       :content (xmp-message-string conn)
       :user-agent (xmp-destination-agent conn)
       :headers (append (let ((host (xmp-destination-host conn))) 
			  (when host `(("Host" . ,host))))
			headers
			(xmp-more-headers conn)  ;;; [rfe6233]
			)
       :external-format (xmp-external-format conn)
       (xmp-client-start conn))
    ;; [rfe12678] Save the received reply string.
    (setf (xmp-received-string conn) s)
    (case rc
      (200 s)
      (500 s)
      (otherwise
       (xmp-error conn :server :sub-code :http-response-code
		  :string rc
		  :detail s))
      )))

(defmethod xmp-message-send ((conn xmp-aserve-server-string-in-out-connector)
			     &key request entity &allow-other-keys)
  (with-http-response 
   (request entity
	    :content-type (or (xmp-destination-content-type conn) "text/xml")
	    )
   (with-http-body 
    (request entity 
	     :external-format (xmp-external-format conn)
	     :headers `((:server . ,(xmp-server-name conn))
			,@(xmp-more-headers conn)   ;;; [rfe6233]
			))
    (html (:princ (xmp-message-string conn))))))


(defmethod xmp-start-server ((conn xmp-aserve-server-connector) 
			     &key new (start nil s-p) enable &allow-other-keys)
  (when new (setf (xmp-aserve-server conn) (make-instance 'net.aserve:wserver)))
  (setf (xmp-aserve-server conn)
	(apply 'start
	       (append
		(when (xmp-aserve-server conn)
		  (list :server (xmp-aserve-server conn)))
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
  (or (xmp-aserve-server server)
      (setf (xmp-aserve-server server) net.aserve:*wserver*))
  (call-next-method)
  (apply #'publish     
	 :server (xmp-aserve-server server)
	 :function #'(lambda (request entity) 
		       (xmp-server-response server :request request :entity entity))
	 :content-type "text/xml"
	 (xmp-server-parameters server))
  server)

(defmethod xmp-server-response ((server xmp-aserve-server-connector)
				&key request entity options &allow-other-keys)
  (let ((server
	 (mp:with-process-lock
	  ((xmp-server-lock server))
	  ;; Make a copy of the server to allow multiple
	  ;;     aserve worker threads
	  (xmp-copy server)))
	(req-string (get-request-body request))
	)
    (setf (aserve-request server) request)
    ;; [rfe12678] Save the received request string.
    (setf (xmp-received-string server) req-string)
    (apply 'xmp-server-implementation server req-string options)
    (xmp-message-send server :request request :entity entity)))

(defun xmp-header-slot (request slot)

  ;; aserve normalizes header names this way
  (setf slot (read-from-string
	      (concatenate 
	       'string ":" (string-downcase (symbol-name slot)))))

  ;; remove string quotes [spr29245]

  (let* ((val (header-slot-value request slot))
	 (len (length val)))
    (cond ((< len 2) val)
	  ((and (eql #\" (elt val 0)) (eql #\" (elt val (1- len))))
	   (subseq val 1 (1- len)))
	  ((and (eql #\' (elt val 0)) (eql #\' (elt val (1- len))))
	   (subseq val 1 (1- len)))
	  (t val))))

