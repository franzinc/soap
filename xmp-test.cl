;; -*- mode: common-lisp; package: user -*-
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

;; $Id: xmp-test.cl,v 2.3 2004/04/23 18:42:16 mm Exp $

;; Internal-use test cases

(in-package :user)

(eval-when (compile load eval)
  (require :soap)
  (load (compile-file-if-needed "soapval1.cl"))
  (load (compile-file-if-needed "soapex.cl"))
  )

(defpackage :user (:use :net.xmp.soap)) 

(defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
(defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
(defpackage :net.xmp.soap.none (:use) (:nicknames :none))
(defpackage :net.xmp.soap.envelope (:use) (:nicknames :env))
(defpackage :net.xmp.soap.encoding (:use) (:nicknames :enc))



(defun s1 () (net.xmp::decode-schema "envelope.xml"))
(defun s2 () (net.xmp::decode-schema "encoding.xml"))
(defun s3 () (net.xmp::decode-schema "XMLSchema.xml"))

(defun sp (&optional (n 1) (d :warn) (u nil))
  (let ((s (soap-message-client :lisp-package :keyword
				:decode-flag d :must-understand u)))
    (values
     (net.xmp::xmp-decode-file s (format nil "ex~A.xml" n))
     s)))


(defclass ct1 (net.xmp:xmp-string-in-connector) ())
(defmethod net.xmp:xmp-begin-message ((conn ct1)) (list :seq1 (list :or :ct1 :ct2)))
(defmethod net.xmp:xmp-end-message ((conn ct1) data &key types &allow-other-keys)
  (values data types))			       
(net.xmp:define-xmp-element nil :ct1 '(:complex 
				       (:or 
					(:seq (:element :foo xsd:|int|) :bar)
					(:seq (:element :foo xsd:|string|) :baz))))
(defun ct1 ()
  (let* ((conn (make-instance 'ct1)))
    (list
     (net.xmp::xmp-decode-message
      conn
      '((:ct1 (:foo "123") (:bar "xyz"))))
     (net.xmp::xmp-decode-message
      conn
      '((:ct1 (:foo "123") (:baz "xyz"))))
     conn)))

;;; This kind of definition should be avoided (or prohibited)
;;;      element foo has two conflicting definitions.
;;; This is not legal in XML Schema definition.
(net.xmp:define-xmp-element nil :ct2 '(:complex (:or (:seq (:element 
						    :foo 
						    (:complex (:seq :bar :baz)))
						   :bob)
					     (:seq (:element 
						    :foo 
						    (:complex (:seq :baz :bar)))
						   :box))))
(defun ct2 () 
  (let* ((conn (make-instance 'ct1)))
    (net.xmp::xmp-decode-message
     conn
     '((:ct2 (:foo (:bar "bar") (:baz "baz")) (:bob "bob"))))))
(defun ct3 () 
  (let* ((conn (make-instance 'ct1)))
    (net.xmp::xmp-decode-message
     conn
     '((:ct2 (:foo (:baz "baz") (:bar "bar")) (:box "box"))))))
(defun ct4 () 
  ;; should fail
  (let* ((conn (make-instance 'ct1)))
    (net.xmp::xmp-decode-message
     conn
     '((:ct2 (:foo (:baz "baz") (:bar "bar")) (:bob "bob"))))))





;;; SOAP Server Tests

(define-soap-element nil :soap-method-1
  '(:complex (:seq (:element :|elt1| enc:|int|)
		   (:element :|elt2| enc:|int|)
		   (:element :|elt3| enc:|int|)
		   )
	     :action "uri:method1"
	     ))
(define-soap-element nil :soap-result-1 '(:simple enc:|int|))
(defun soap-method-1 (&key (elt1 0) (elt2 0) (elt3 0)) (+ elt1 elt2 elt3))
(defun ss1 (&key (index 1 )(port 4567) (path "/SOAP"))
  (let ((server (soap-message-server :start `(:port ,port)
				     :publish `(:path ,path)
				     :lisp-package :keyword
				     )))

    (soap-export-method server :soap-method-1 '(:elt1 :elt2 :elt3)
			:return :soap-result-1
			:action "uri:method1"
			:lisp-name 'soap-method-1)
    (let* ((client (soap-message-client :url (format nil "http://localhost:~A~A"
						     port path)
					:lisp-package :keyword
					))
	   (result
	    (list
	     (case index
	       ((:all 1)
		(call-soap-method client :soap-method-1 :elt1 111 :elt2 222 :elt3 333)))
	     (case index
	       ((:all 2)
		(call-soap-method client :soap-method-1 :elt1 123)))
	     (case index
	       ((:all 3)
		(call-soap-method client :soap-method-1 :elt2 456)))
	     (case index
	       ((:all 4)
		(call-soap-method client :soap-method-1 :elt3 789)))
	     (case index
	       ((:all 5)
		(call-soap-method client :soap-method-1 :elt3 444 :elt1 333)))
	     ))
	   )
      (values result server client net.xmp.soap::*soap-last-server*)
      )))


(defpackage :amazon (:use))
(defparameter *base-dns-tail*
  (list 
   (list :net.xmp.schema
	 "xsd"
	 "http://www.w3.org/2001/XMLSchema")
   (list :net.xmp.schema-instance
	 "xsi"
	 "http://www.w3.org/1999/XMLSchema-instance")))
(defparameter *amazon-dns*
  (list*
   nil
   (list :amazon
	 "typens"
	 "http://soap.amazon.com")
   *base-dns-tail*))
(defparameter *google-dns*
  (list*
   nil
   (list :gg
	 "typens"
	 "urn:GoogleSearch")
   *base-dns-tail*))


(defun wsdl-all (&key (root "./WSDL-files") index &aux (i 0) res)
  (flet ((do-one (i file)
		 (multiple-value-bind (vals e)
		     (ignore-errors
		      (multiple-value-list
		       (decode-wsdl-file file 
					 :namespaces '(nil (:prefix "tn") . :guess)
					 )))
		   (format t "~&~A. ~A ~S~%" i file (or e (first vals)))
		   (push (list i file (or e :ok)) res)
		   (or e
		       (format t "~&~{   ~S~%~}" (cdr vals))))))
    (dolist (file (directory (concatenate 'string root "/*.xml")))
      (when (or (null index) (eql index i))
	(do-one i file))
      (incf i))
    (dolist (file (directory (concatenate 'string root "/*.wsdl")))
      (when (or (null index) (eql index i))
	(do-one i file))
      (incf i))
   res ))

(defun wsdl01 (&optional (file "Atest.xml")
			verbose
			(ns *amazon-dns*)
			&aux 
			(net.xmp.soap:*wsdl-default-namespaces*
			 net.xmp.soap:*wsdl-1.1-namespaces*)
			) 
  (let* ((res (multiple-value-list
	      (net.xmp.soap:decode-wsdl-file
	       file :namespaces ns :lisp-package :keyword)))
	 (conn (first res)))
    (when verbose
      (format t "~&~%;; TYPES~%~S~%" (net.xmp:schema-types conn))
      (format t "~&~%;; MESSAGES~%~S~%" (net.xmp.soap::wsdl-messages conn))
      (format t "~&~%;; INTERFACES~%~S~%" (net.xmp.soap::wsdl-interfaces conn))
      (format t "~&~%;; PORT-TYPES~%~S~%" (net.xmp.soap::wsdl-port-types conn))
      (format t "~&~%;; BINDINGS~%~S~%" (net.xmp.soap::wsdl-bindings conn))
      (format t "~&~%;; SERVICES~%~S~%" (net.xmp.soap::wsdl-services conn))
      )
    (values-list res)))
	 


(defun wsdl21 (&optional verbose) (wsdl01 "AmazonWebServices.xml" verbose))

(defun wsdl22 (&optional verbose eval (dest t))
  (make-client-interface (wsdl21 verbose) 0 dest :eval eval))

(defun wsdl25 (&optional verbose (eval t) (dest "amazon.cl") &aux sv)
  (make-server-interface (wsdl21 verbose) 0 dest :eval eval)
  (values
   dest
   (setf sv (funcall 'make-server))
   (encode-wsdl-file "amazon.wsdl" :servers sv
		     :namespaces *amazon-dns*
		     :name "AmazonSearch"
		     :target (third (second *amazon-dns*))
		     )))

;; This version was needed to workaround xmlns="" problem
;;(defun wsdl31 (&optional verbose) (wsdl01 "amazon.xml" verbose)) 


(defun wsdl41 (&optional verbose) (wsdl01 "google.xml" verbose *google-dns*))

(defun wsdl42 (&optional verbose eval (dest t))
  (make-client-interface (wsdl41 verbose) 0 dest :eval eval))
(defun wsdl43 (&optional verbose (eval t) (dest t))
  (make-server-interface (wsdl41 verbose) 0 dest :eval eval))

(defun wsdl45 (&optional verbose (eval t) (dest "google.cl") &aux sv)
  (make-server-interface (wsdl41 verbose) 0 dest :eval eval)
  (values 
   dest
   (setf sv (funcall 'make-server))
   (encode-wsdl-file "google.wsdl" :servers sv
		     :namespaces *google-dns*
		     :name "GoogleSearch"
		     :target (third (second *google-dns*))
		     )))

;; this call generates a warning because a package is not defined
(defun wsdl49 (&optional verbose eval)
  (make-client-interface
   (wsdl01 "google.xml" verbose (list* nil *base-dns-tail*))
   0 t :eval eval))







(defun test-clients (&key index (error-p nil) val-p &aux (fail 0) answer)

  (macrolet ((run-one (this body &optional expected)
		      `(when (or (null index) (eql index ,this))
			 (let ((res (if error-p
					,body
				      (multiple-value-bind (v e)
					  (ignore-errors (multiple-value-list ,body))
					(if e
					    (list nil e)
					  v)))))
			   (if val-p 
			       (setf answer res)
			     (setf res (test-res res ',expected)))
			   (format t "~&;;~2D. ~A~%" ,this res)))))
    (flet ((test-res (res ex &aux elt)
		     (cond ((null (first res))
			    (incf fail)
			    (format nil "~A" (second res)))
			   (t (setf elt (if (consp (first res))
					    (first (first res))
					  (first res)))
			      (cond ((null ex)
				     (format nil "~S" elt))
				    ((eq elt ex) "OK")
				    (t (incf fail)
				       (format nil "Expected ~S found ~S" ex elt)))))))


      (run-one 01 (sp01))
      (run-one 10 (sp10) temp::|getTempResponse|)
      (run-one 21 (sp21) baseball::|GetTeamsResponse|)
      (run-one 22 (sp22) baseball::|GetPlayersResponse|)
      (run-one 30 (sp30) temp:|getRateResponse|)
      (run-one 40 (sp40) temp::|getVersionResponse|)
      (run-one 51 (sp51) temp:|SearchRecipesResponse|)
      ;; (run-one 52 (sp52 id))
      (run-one 61 (gs)  gg:|doGoogleSearchResponse|)
      (run-one 62 (gsp) gg:|doSpellingSuggestionResponse|)
      (run-one 63 (gcp) gg:|doGetCachedPageResponse|)

      (format t "~%;;; ~S tests failed.~%" fail)
      (if val-p (values-list answer) (eql 0 fail))
      )))
 
(defun random-string (min limit &optional inc1 inc2)
  (let* ((ln (+ min (random (- limit min))))
	 (s (make-string ln))
	 (data0  "abcdefghijklmnopqrstuvwxyz0123456789")
	 (data1  "abcdefghijklmnopqrstuvwxyz0123456789+-_&%@!:;'/.,")
	 (data2  "abcdefghijklmnopqrstuvwxyz0123456789*$#[]{}|?><")
	 (data12 "abcdefghijklmnopqrstuvwxyz0123456789+-_&%@!:;'/.,*$#[]{}|?><")
	 (data (cond ((and inc1 inc2) data12)
		     (inc1 data1)
		     (inc2 data2)
		     (t data0)))
	 )
    (dotimes (i ln s)
      (setf (elt s i)
	    (elt data (random (length data)))))))


(defvar *server* nil)
(defun test-validator1 (&key index (port 8080)
			     (host "localhost")
			     (path *validator1-path*)
			     (reps 10)
			     (stop t)
			     (verbose nil)
			     (error-p t)
			     &aux fail)
  (setf *server* (make-validator1-server port))
  (macrolet ((run-one (body)
		      `(when (or (null index) (eql index this))
			 (dotimes (n reps)
			   (if error-p
			       ,body
			     (multiple-value-bind (v e)
				 (ignore-errors (multiple-value-list ,body))
			       (if e
				   (let ()
				     (push this fail)
				     (values nil e))
				 v)))))))

    (let ((client (soap-message-client :url (format nil "http://~A:~A~A"
						    host port path)
				       :lisp-package :keyword
				       :message-dns *validator1-dns*
				       ))
	  (this 0))

      (flet ((fmt (&rest args) (when verbose (apply 'format t args))))
	(flet ((run1
		(n)
		(declare (ignore n))
		;; countTheEntities
		(let* ((lb (random 10))
		       (rb (random 10))
		       (am (random 10))
		       (ap (random 10))
		       (qt (random 10))
		       (sp (+ lb rb am ap qt))
		       (ch (+ 10 (* sp (+ 2 (random 5)))))
		       (all (+ sp ch))
		       (sch `( 
			      ,@(if (zerop lb) nil (list (list #\< lb)))
			      ,@(if (zerop rb) nil (list (list #\> rb)))
			      ,@(if (zerop am) nil (list (list #\& am)))
			      ,@(if (zerop ap) nil (list (list #\' ap)))
			      ,@(if (zerop qt) nil (list (list #\" qt)))
			      ))
		       (data (let ((data (make-string all)))
			       (dotimes (i all data)
				 (setf
				  (elt data i)
				  (if sch
				      (if (< (random (+ sp ch)) sp)
					  (let* ((j (random (length sch)))
						 (item (elt sch j))
						 (char (first item))
						 (count (second item)))
					    (decf (second item)) (decf sp)
					    (when (eql count 1)
					      (setf sch (remove item sch)))
					    char)
					(let ()
					  (decf ch)
					  (elt "abcdefghijklmnopqrstuvw" (random 23))))
				    (elt "abcdefghijklmnopqrstuvw" (random 23)))))))
		       (rep (call-soap-method client "countTheEntities" :|s| data))
		       (res (soap-sub-element-content (cdr rep) :|struct|))
		       )
		  (if (and (eql lb (soap-sub-element-content
				    res :|ctLeftAngleBrackets|))
			   (eql rb (soap-sub-element-content
				    res :|ctRightAngleBrackets|))
			   (eql am (soap-sub-element-content res :|ctAmpersands|))
			   (eql ap (soap-sub-element-content res :|ctApostrophes|))
			   (eql qt (soap-sub-element-content res :|ctQuotes|)))
		      (fmt "~&;;~2D. ok all=~S~%" this all)
		    (error "bad result ~S ~S" data rep))
		  ))
	       (run2
		(n)
		(declare (ignore n))
		;; easyStructTest
		(let* ((m (random 100))
		       (l (random 100))
		       (c (random 100))
		       (rep (call-soap-method client "easyStructTest"
					      :|stooges|
					      (list :|moe| m :|larry| l :|curly| c)))
		       (num (soap-sub-element-content (cdr rep) :|number|)))
		  (if (eql num (+ m l c))
		      (fmt "~&;;~2D. ok num=~S~%" this num)
		    (error "bad result ~S ~S"
			   (list :|moe| m :|larry| l :|curly| c) rep))))
	       (run3
		(n)
		;; echoStructTest
		(let* ((out (let (new)
			      (dotimes (j (+ 2 (random 5)))
				(setf new
				      (append
				       new
				       (list
					(intern (format nil "substruct~A" j) :keyword)
					(list :|moe|   (format nil "~A" (random 10))
					      :|larry| (format nil "~A" (random 10))
					      :|curly| (format nil "~A" (random 10))
					      )))))
			      new))
		       (rep (call-soap-method client "echoStructTest" :|myStruct| out))
		       (res (soap-sub-element-content (cdr rep) :|myStruct|))
		       )
		  (if (equal out (soap-alist-to-plist res t))
		      (fmt "~&;;~2D. ok n=~S~%" this n)
		    (error "bad result ~S ~S" out rep))))
	       (run4
		(n)
		(let* ((num (random 100))
		       (bool (eql 1 (random 2)))
		       (st (random-string 5 15))
		       (db (random 10.1))
		       (dt (format nil "~A" (random 100000)))
		       (bin (random-string 50 100))
		       (out (list num bool st db dt bin))
		       (rep (call-soap-method client "manyTypesTest"
					      :|num| num
					      :|bool| bool
					      :|state| st
					      :|doub| db
					      :|dat| dt
					      :|bin| bin))
		       (res (soap-sub-element-content (cdr rep) :|Result|))
		       )
		  (if (and (arrayp res) 
			   (equal '(6) (array-dimensions res))
			   (equal out (concatenate 'list res)))
		      (fmt "~&;;~2D. ok n=~S~%" this n)
		    (error "bad result ~S ~S" out res)))
		)
	       (run5
		(n)
		;; moderateSizeArrayCheck
		(let* (out rep res)
		  (dotimes (i (+ 5 (random 100)))
		    (push (random-string 1 10) out))
		  (setf rep (call-soap-method client "moderateSizeArrayCheck"
					      :|myArray| out))
		  (setf res (soap-sub-element-content (cdr rep) :|result|))
		  (if (equal res (concatenate 'string (first out) (first (last out))))
		      (fmt "~&;;~2D. ok n=~S alen=~S~%" this n (length out))
		    (error "bad result ~S ~S" out res))))
	       (run6
		(n)
		;; nestedStructTest
		(flet ((random-days 
			(from to)
			(let* (new (days (random (1+ (- to from)))))
			  (dotimes (i days (reverse new))
			    (push (read-from-string (format nil ":day~2,'0D"
							    (+ from i)))
				  new)
			    (push (list :|moe| 0 :|larry| 0 :|curly| 0) new)))))
		  (flet ((random-months
			  (from to)
			  (let* (new (mos (random (1+ (- to from)))))
			    (dotimes (i mos (reverse new))
			      (push (read-from-string (format nil ":month~2,'0D"
							      (+ from i)))
				    new)
			      (push (random-days 1 30)  new)))))
		    (let* ((m (random 100))
			   (l (random 100))
			   (c (random 100))
			   (out `(:|year2000|
				   ( ,@(random-months 1 3)
				       |month04|
				       (:|day01|
					 (:|moe| ,m :|larry| ,l :|curly| ,c))
				       ,@(random-days 2 30)
				       ,@(random-months 5 12)
				       )))
			   (rep (call-soap-method client "nestedStructTest"
						  :|myStruct| out))
			   (res (soap-sub-element-content (cdr rep) :|result|))
			   )
		      (if (eql res (+ m l c))
			  (fmt "~&;;~2D. ok n=~S mlc=~S~%" this n (list m l c))
			(error "bad result ~S ~S" out rep)))
		    )))
	       (run7
		(n)
		(let* ((num (random 1000))
		       (rep (call-soap-method client "simpleStructReturnTest"
					      :|myNumber| num)))
		  (if (equal rep
			     (list  :|simpleStructReturnTestResult|
				    (list :|struct|
					  (list :|times10| (* num 10))
					  (list :|times100| (* num 100))
					  (list :|times1000| (* num 1000))
					  )))
		      (fmt "~&;;~2D. ok n=~S mnum=~S~%" this n num)
		    (error "bad result ~S ~S" num rep))))
	       (run8
		(n &aux (rep (call-soap-method client "whichToolkit")))
		(if (equal rep
			   '(:|whichToolkitResult|
			      (:|struct|
				(:|toolkitDocsUrl| "http://franz.com")
				(:|toolkitName| "Allegro Common Lisp SOAP")
				(:|toolkitVersion| "6.2")
				(:|toolkitOperatingSystem| "Windows and Unix"))))
		    (fmt "~&;;~2D. ok n=~S~%" this n)
		  (error "bad result ~S" rep)))
	       )
 
	  (incf this)
	  (run-one (run1 n))
	  (incf this)
	  (run-one (run2 n))
	  (incf this)
	  (run-one (run3 n))
	  (incf this)
	  (run-one (run4 n))
	  (incf this)
	  (run-one (run5 n))
	  (incf this)
	  (run-one (run6 n))
	  (incf this)
	  (run-one (run7 n))
	  (incf this)
	  (run-one (run8 n))
		
	  (when stop (net.xmp:xmp-stop-server *server*))

	  (if fail
	      (values nil fail)
	    :all-ok))))))







;;; Test code to verify class precedence lists

#+ignore
(in-package :net.xmp.xmlrpc)

#+ignore
(eval-when (compile)
  (dolist (n '(
	       xmp-connector

	       xmp-client-connector
	       xmp-server-connector

	       xmp-message-connector
	       xmp-stream-connector

	       xmp-reply-connector
	       xmp-event-connector

	       xmp-message-reply-connector
	       xmp-message-event-connector
	       xmp-stream-reply-connector
	       xmp-stream-event-connector
   
	       xmp-client-message-connector
	       xmp-client-stream-connector
	       xmp-client-reply-connector
	       xmp-client-event-connector
	       xmp-client-message-reply-connector
	       xmp-client-message-event-connector
	       xmp-client-stream-reply-connector
	       xmp-client-stream-event-connector

	       xmp-server-message-connector
	       xmp-server-stream-connector
	       xmp-server-reply-connector
	       xmp-server-event-connector
	       xmp-server-message-reply-connector
	       xmp-server-message-event-connector
	       xmp-server-stream-reply-connector
	       xmp-server-stream-event-connector
	       ))
    (make-instance n))

  ;; to verify precedence lists
  (dolist (n '(
	       xmp-aserve-connector

	       xmp-aserve-client-connector
	       xmp-aserve-server-connector

	       xmp-aserve-message-connector
	       xmp-aserve-stream-connector

	       xmp-aserve-reply-connector
	       xmp-aserve-event-connector

	       xmp-aserve-message-reply-connector
	       xmp-aserve-message-event-connector
	       xmp-aserve-stream-reply-connector
	       xmp-aserve-stream-event-connector
   
	       xmp-aserve-client-message-connector
	       xmp-aserve-client-stream-connector
	       xmp-aserve-client-reply-connector
	       xmp-aserve-client-event-connector
	       xmp-aserve-client-message-reply-connector
	       xmp-aserve-client-message-event-connector
	       xmp-aserve-client-stream-reply-connector
	       xmp-aserve-client-stream-event-connector

	       xmp-aserve-server-message-connector
	       xmp-aserve-server-stream-connector
	       xmp-aserve-server-reply-connector
	       xmp-aserve-server-event-connector
	       xmp-aserve-server-message-reply-connector
	       xmp-aserve-server-message-event-connector
	       xmp-aserve-server-stream-reply-connector
	       xmp-aserve-server-stream-event-connector
	       ))
    (make-instance n))

  (dolist (n '(
	       xmlrpc-connector

	       xmlrpc-client-connector
	       xmlrpc-server-connector

	       xmlrpc-message-connector
	       xmlrpc-stream-connector

	       xmlrpc-reply-connector
	       xmlrpc-event-connector

	       xmlrpc-message-reply-connector
	       xmlrpc-message-event-connector
	       xmlrpc-stream-reply-connector
	       xmlrpc-stream-event-connector
   
	       xmlrpc-client-message-connector
	       xmlrpc-client-stream-connector
	       xmlrpc-client-reply-connector
	       xmlrpc-client-event-connector
	       xmlrpc-client-message-reply-connector
	       xmlrpc-client-message-event-connector
	       xmlrpc-client-stream-reply-connector
	       xmlrpc-client-stream-event-connector

	       xmlrpc-server-message-connector
	       xmlrpc-server-stream-connector
	       xmlrpc-server-reply-connector
	       xmlrpc-server-event-connector
	       xmlrpc-server-message-reply-connector
	       xmlrpc-server-message-event-connector
	       xmlrpc-server-stream-reply-connector
	       xmlrpc-server-stream-event-connector

	       xmlrpc-aserve-connector

	       xmlrpc-aserve-client-connector
	       xmlrpc-aserve-server-connector

	       xmlrpc-aserve-message-connector
	       xmlrpc-aserve-stream-connector

	       xmlrpc-aserve-reply-connector
	       xmlrpc-aserve-event-connector

	       xmlrpc-aserve-message-reply-connector
	       xmlrpc-aserve-message-event-connector
	       xmlrpc-aserve-stream-reply-connector
	       xmlrpc-aserve-stream-event-connector
   
	       xmlrpc-aserve-client-message-connector
	       xmlrpc-aserve-client-stream-connector
	       xmlrpc-aserve-client-reply-connector
	       xmlrpc-aserve-client-event-connector
	       xmlrpc-aserve-client-message-reply-connector
	       xmlrpc-aserve-client-message-event-connector
	       xmlrpc-aserve-client-stream-reply-connector
	       xmlrpc-aserve-client-stream-event-connector

	       xmlrpc-aserve-server-message-connector
	       xmlrpc-aserve-server-stream-connector
	       xmlrpc-aserve-server-reply-connector
	       xmlrpc-aserve-server-event-connector
	       xmlrpc-aserve-server-message-reply-connector
	       xmlrpc-aserve-server-message-event-connector
	       xmlrpc-aserve-server-stream-reply-connector
	       xmlrpc-aserve-server-stream-event-connector
	       ))
    (make-instance n)))

