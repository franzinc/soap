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

;; $Id: xmp-test.cl,v 2.4 2005/08/03 05:09:48 layer Exp $

;; Internal-use test cases

(in-package :user)

(eval-when (compile load eval)

  (let* ((module (ecase *current-case-mode*
		  (:case-sensitive-lower :soapm)
		  (:case-insensitive-upper :soapa)))
	(file (string-downcase (format nil "~A.fasl"  module))))
    (when (probe-file file)
      (load file)
      (provide module)
      (provide :soap))
    (require :soap)
    (require :tester)
    )
  
  ;; always compile these to avoid casemode problems
  (load (compile-file "soapval1.cl"))
  (load (compile-file "soapex.cl"))
  (load (compile-file "bignum-server.cl"))
  )

(defpackage :user (:use :net.xmp.soap  :util.test)) 

(defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
(defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
(defpackage :net.xmp.soap.none (:use) (:nicknames :none))
(defpackage :net.xmp.soap.envelope (:use) (:nicknames :env))
(defpackage :net.xmp.soap.encoding (:use) (:nicknames :enc))

#|

----- TESTING PROCEDURE (of sorts) -- run on alisp AND mlisp -----

(test-soap-local) --> test report from with-tests wrapper
                      (soap-local-tests)  - just the tests

(test-soap-remote) --> test report from with-tests wrapper
                       (soap-remote-tests)  - just the tests

(test-soap-x [remote]) -> test report  (parse all WSDL at XMethods.com)


Individual tests:

- From soapex.cl and soapval1.cl -
(test-clients) --> t   ;; some clients may not respond
(ss1 :index :all) --> t
(ss2) --> t
(test-validator1 :port pppp :stop nil) --> :all-ok
   pppp should be a port that can be seen through the firewall
   then go to http://validator.soapware.org/
         enter the host name or address and the port pppp
          verify that all the tests return OK.

- from bignum-server.cl -
(start-server)
(try-server)
(bn-wsdl) --> bn.wsdl
  (decode-wsdl-file "bn.wsdl")
  (make-client-interface * 0 "bn-client.cl")  --> bn-client.cl ==> try with SOAPScope 


- interacting with xmethods.com -
(xmeth-def)          ;; decode xmeth.wsdl
(xmeth-get)          ;; get list of methods from server
(xmeth-read)          ;; read local list of methods
(xmeth-save)          ;; save local list of methods
(xmeth-one-remote [i [errorp [folder]]])   ;; fetch and parse one WSDL def from server
(xmeth-one-local [i [errorp [folder]]])   ;; fetch and parse one local WSDL def
(xmeth-all-remote)         ;; run through all remote xmethods
(xmeth-all-local)         ;; run through all local xmethods

- WSDL decoding tests - 
(wsdl11) --> ss1.wsdl ==> try with SOAPScope
(wsdl12) --> ss2.wsdl ==> try with SOAPScope
(wsdl13) --> ss3.wsdl ==> try with SOAPScope
(wsdl22) --> amazon-client.cl
(wsdl25) --> amazon-gen.wsdl
(wsdl42) --> google-client.cl
(wsdl43) --> google-server.cl
(wsdl45) --> google-gen.wsdl

|#


(defun test-soap-local ()
  (with-tests
   (:name "SOAP module (local)")
   (setf *error-protect-tests* t)
   (soap-local-tests)))

(defun soap-local-tests ()
  (test nil (null (xmp-match-tests)))
  (test nil (null (xmp-break-tests)))

  (test nil (null (ss1 :index :all)))
  (test nil (null (ss2)))
  (test :all-ok (test-validator1))
  (test nil (null (funcall 'start-server)))
  (test t (test-bn))
  (test-no-error (stop-soap-server (symbol-value '*bn-server*)))
  )

(defun test-soap-remote ()
  (with-tests
   (:name "SOAP module (external servers)")
   (setf *error-protect-tests* t)
   (soap-remote-tests)))

(defun soap-remote-tests ()

   (test nil (null (test-clients)))

   )

(defun test-soap-x (&optional remote)
  (if remote 
      (with-tests 
       (:name "Xmethods (fetch external WSDL)")
       (setf *error-protect-tests* t)
       (test-no-error (xmeth-def :uri))
       (test-no-error (xmeth-get))
       (test-no-error (xmeth-save))
       (test-no-error (xmeth-all-remote))
       (test-no-error (xmeth-save))
       )
    (with-tests
     (:name "Xmethods (local copies of WSDL)")
     (setf *error-protect-tests* t)
     (test-no-error (xmeth-def))
     (test-no-error (xmeth-read))
     (test-no-error (xmeth-all-local))
     (test-no-error (xmeth-save))
     )
    ))
  
	

(defun test-bn ()
  (let* ((server (start-server))
	(r (try-server))
	(v (and (equal (format nil "~A" (apply 'factorial 17 0 nil))
		(soap-result-only nil (elt r 0) nil "calculateResponse" "calcResult"))
	 (equal "18"
		(soap-result-only nil (elt r 1) nil "encodeNumResponse" "encResult"))
	 (equal `((:|item| 1) (:|item| 2) (:|item| 3) (:|item| 4) (:|item| 5))
		(soap-result-only nil (elt r 2) nil "decodeNumResponse" "decResult"))
	 t)))
    (stop-soap-server server)
    v))




(defun s1 () (net.xmp::decode-schema "envelope.xml"))
(defun s2 () (net.xmp::decode-schema "encoding.xml"))
;;(defun s3 () (net.xmp::decode-schema "XMLSchema.xml"))

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

(defpackage :ss1 (:use))
(define-namespace :ss1 "ss1" "urn:SS1NS")
(define-soap-element nil 'ss1::soap-method-1
  '(:complex (:seq (:element :|elt1| xsd:|int|)
		   (:element :|elt2| xsd:|int|)
		   (:element :|elt3| xsd:|int|)
		   )
	     :action "uri:method1"
	     ))
(define-soap-element nil 'ss1::soap-result-1 
  '(:complex (:seq (:element ss1::result (:simple xsd:|int|)))))
(defun soap-method-1 (&key (|elt1| 0) (|elt2| 0) (|elt3| 0))
  (list 'ss1::result (+ |elt1| |elt2| |elt3|)))
(defun ss1 (&key (index 1 )(port 4567) (path "/SOAP") debug)
  (let* ((url (format nil "http://localhost:~A~A" port path))
	 (server (soap-message-server :start `(:port ,port)
				     :publish `(:path ,path)
				     :lisp-package :keyword
				     :message-dns '(nil (:ss1))
				     :soap-debug
				     (case debug ((:client nil) nil) (otherwise t))
				     :url url
				     )))

    (soap-export-method server 'ss1::soap-method-1 '(:|elt1| :|elt2| :|elt3|)
			:return 'ss1::soap-result-1
			:action "uri:method1"
			:lisp-name 'soap-method-1)
    (flet ((one (client index case e1 e2 e3)
		(if (or (eq index :all)
			(eql index case))
		    (let (r)
		      (cond ((and e1 e2 e3)
			     (setq r (call-soap-method
				      client 'ss1::soap-method-1
				      :|elt1| e1 :|elt2| e2 :|elt3| e3)))
			    ((and e1 e2)
			     (setq r (call-soap-method
				      client 'ss1::soap-method-1 :|elt1| e1 :|elt2| e2)))
			    ((and e1 e3)
			     (setq r (call-soap-method
				      client 'ss1::soap-method-1 :|elt1| e1 :|elt3| e3)))
			    ((and e2 e3)
			     (setq r (call-soap-method
				      client 'ss1::soap-method-1 :|elt2| e2 :|elt3| e3)))
			    (e1
			     (setq r (call-soap-method
				      client 'ss1::soap-method-1 :|elt1| e1)))
			    (e2
			     (setq r (call-soap-method
				      client 'ss1::soap-method-1 :|elt2| e2)))
			    (e3
			     (setq r (call-soap-method
				      client 'ss1::soap-method-1 :|elt3| e3))))
		      (equal r (list 'ss1::soap-result-1
				     (list 'ss1::result
					   (+ (or e1 0) (or e2 0) (or e3 0))))))
		  t)))
      (let* ((client (soap-message-client :url url
					  :lisp-package :keyword
					  :message-dns '(nil (:ss1))
					  :soap-debug
					  (case debug ((:server nil) nil) (otherwise t))
					  ))
	     (result
	      (and  (one client index 0 111 222 333)
		    (one client index 1 123 nil nil)
		    (one client index 2 nil 456 nil)
		    (one client index 3 nil nil 789))))		  
	(values result server client net.xmp.soap::*soap-last-server*)
	))))

(defun ss2 () 
  (and (simple-server :ns 0)
       (simple-server :ns 1)
       (null (simple-server :ns 2))))




(defpackage :amazon (:use))
(define-namespace :amazon "typens" "http://soap.amazon.com")
(define-namespace :gg "typens" "urn:GoogleSearch")


(defun wsdl-all (&key (root "./WSDL-files") index &aux (i 0) res)
  (flet ((do-one (i file)
		 (multiple-value-bind (vals e)
		     (ignore-errors
		      (multiple-value-list
		       (decode-wsdl-file file)))
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

(defun wsdl01 (&optional (file "Atest.xml") verbose (ns :decode))
  (let* ((res (multiple-value-list
	      (net.xmp.soap:decode-wsdl-file
	       file :namespaces ns :lisp-package :keyword)))
	 (conn (first res)))
    (when (and conn verbose)
      (format t "~&~%;; TYPES~%~S~%" (net.xmp:schema-types conn))
      (format t "~&~%;; MESSAGES~%~S~%" (net.xmp.soap::wsdl-messages conn))
      (format t "~&~%;; INTERFACES~%~S~%" (net.xmp.soap::wsdl-interfaces conn))
      (format t "~&~%;; PORT-TYPES~%~S~%" (net.xmp.soap::wsdl-port-types conn))
      (format t "~&~%;; BINDINGS~%~S~%" (net.xmp.soap::wsdl-bindings conn))
      (format t "~&~%;; SERVICES~%~S~%" (net.xmp.soap::wsdl-services conn))
      )
    (values-list res)))
	 

(defun wsdl11 ()
  
  (encode-wsdl-file "ss1.wsdl"
		    :servers (list "SS1" (nth-value 1 (ss1 :index 17)))
		    :target "urn:SS1NS"
		    ))
(defun wsdl12 ()
  (encode-wsdl-file "ss2.wsdl"
		    :servers (nth-value 1 (simple-server :ns 1))
		    :target :ts
		    ))
(defun wsdl13 ()
  (encode-wsdl-file "ss3.wsdl"
		    :servers (make-validator1-server)
		    :target :keyword
		    ))


(defun wsdl21 (&optional verbose) (wsdl01 "amazon.xml" verbose))

(defun wsdl22 (&optional verbose (dest "amazon-client.cl"))
  (make-client-interface (wsdl21 verbose) 0 dest))

(defun wsdl25 (&optional verbose (dest "amazon-server.cl") &aux conn sv)
  (make-server-interface (setf conn (wsdl21 verbose)) 0 dest)
  (when (stringp dest)
    (load (compile-file dest))
    (setf sv (funcall 'server-make-server)))
  (values
   dest
   conn
   sv
   (when sv
     (encode-wsdl-file "amazon-gen.wsdl" :servers sv
		       :name "AmazonSearch"
		       :target :amazon
		       ))))


(defun wsdl41 (&optional verbose) (wsdl01 "google.xml" verbose))

(defun wsdl42 (&optional verbose (dest "google-client.cl"))
  (make-client-interface (wsdl41 verbose) 0 dest))
(defun wsdl43 (&optional verbose (dest "google-server.cl"))
  (make-server-interface (wsdl41 verbose) 0 dest))

(defun wsdl45 (&optional verbose (dest "google-server.cl") &aux sv)
  (make-server-interface (wsdl41 verbose) 0 dest)
  (when (stringp dest)
    (load (compile-file dest))
    (setf sv (funcall 'server-make-server)))
  (values 
   dest
   sv
   (when sv (encode-wsdl-file "google-gen.wsdl" :servers sv
			      :name "GoogleSearch"
			      :target (define-namespace :gg nil nil)
			      ))))









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


      ;;(run-one 01 (sp01))
      (run-one 10 (sp10) temp::|getTempResponse|)
      ;;(run-one 21 (sp21) baseball::|GetTeamsResponse|)
      ;;(run-one 22 (sp22) baseball::|GetPlayersResponse|)
      (run-one 30 (sp30) temp:|getRateResponse|)
      (run-one 40 (sp40) temp::|getVersionResponse|)
      ;;(run-one 51 (sp51) temp:|SearchRecipesResponse|)
      ;; (run-one 52 (sp52 id))
      (run-one 61 (gs)  gg:|doGoogleSearchResponse|)
      (run-one 62 (gsp) gg:|doSpellingSuggestionResponse|)
      (run-one 63 (gcp) gg:|doGetCachedPageResponse|)

      (format t "~%;;; ~S client tests failed.~%" fail)
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
			     debug log
			     (verbose nil)
			     (error-p t)
			     &aux fail)
  (let ((oldlog net.aserve::*enable-logging*))
    (unwind-protect
	(let ()
	  (setf net.aserve::*enable-logging* log)
	  (setf *server* (make-validator1-server :port port
						 :debug (case debug
							  ((nil :client) nil)
							  (otherwise debug))))
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
					       :decode-flag
					       nil ;;; ignore undefined elts and types
					       :soap-debug (case debug
							     ((nil :server) nil)
							     (otherwise debug))
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
			       (res (soap-sub-element-content (cdr rep) :|struct1|))
			       )
			  (if (and (eql lb (soap-sub-element-content
					    res :|ctLeftAngleBrackets|))
				   (eql rb (soap-sub-element-content
					    res :|ctRightAngleBrackets|))
				   (eql am (soap-sub-element-content res :|ctAmpersands|))
				   (eql ap (soap-sub-element-content res :|ctApostrophes|))
				   (eql qt (soap-sub-element-content res :|ctQuotes|)))
			      (fmt "~&;;~2D. ok all=~S~%" this all)
			    (error "run1: bad result ~S ~S" data rep))
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
			    (error "run2: bad result ~S ~S"
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
			       (rep (call-soap-method client "echoStructTest" :|myStruct1| out))
			       (res (soap-sub-element-content (cdr rep) :|myStruct1|))
			       )
			  (if (equal out (soap-alist-to-plist res t))
			      (fmt "~&;;~2D. ok n=~S~%" this n)
			    (error "run3: bad result ~S ~S" out rep))))
		       (run4
			(n)
			(let* ((num (random 100))
			       (bool (eql 1 (random 2)))
			       (st (random-string 5 15))
			       (db (random 10.1d0))
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
			       (res (soap-sub-element-content (cdr rep) :|Result1|))
			       )
			  (if (and (arrayp res) 
				   (equal '(6) (array-dimensions res))
				   (equal out (concatenate 'list res)))
			      (fmt "~&;;~2D. ok n=~S~%" this n)
			    (error "run4: bad result ~S ~S" out res)))
			)
		       (run5
			(n)
			;; moderateSizeArrayCheck
			(let* (out rep res)
			  (dotimes (i (+ 5 (random 100)))
			    (push (random-string 1 10) out))
			  (setf rep (call-soap-method client "moderateSizeArrayCheck"
						      :|myArray| out))
			  (setf res (soap-sub-element-content (cdr rep) :|result2|))
			  (if (equal res (concatenate 'string (first out) (first (last out))))
			      (fmt "~&;;~2D. ok n=~S alen=~S~%" this n (length out))
			    (error "run5: bad result ~S ~S" out res))))
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
							  :|myStruct2| out))
				   (res (soap-sub-element-content (cdr rep) :|result3|))
				   )
			      (if (eql res (+ m l c))
				  (fmt "~&;;~2D. ok n=~S mlc=~S~%" this n (list m l c))
				(error "run6: bad result ~S ~S" out rep)))
			    )))
		       (run7
			(n)
			(let* ((num (random 1000))
			       (rep (call-soap-method client "simpleStructReturnTest"
						      :|myNumber| num)))
			  (if (equal rep
				     (list  :|simpleStructReturnTestResult|
					    (list :|struct2|
						  (list :|times10| (* num 10))
						  (list :|times100| (* num 100))
						  (list :|times1000| (* num 1000))
						  )))
			      (fmt "~&;;~2D. ok n=~S mnum=~S~%" this n num)
			    (error "run7: bad result ~S ~S" num rep))))
		       (run8
			(n &aux (rep (call-soap-method client "whichToolkit")))
			(if (equal rep
				   '(:|whichToolkitResult|
				      (:|struct3|
					(:|toolkitDocsUrl| "http://franz.com")
					(:|toolkitName| "Allegro Common Lisp SOAP")
					(:|toolkitVersion| "6.2")
					(:|toolkitOperatingSystem| "Windows and Unix"))))
			    (fmt "~&;;~2D. ok n=~S~%" this n)
			  (error "run8: bad result ~S" rep)))
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
      (setf net.aserve::*enable-logging* oldlog))))
      




;;; xmethods.com

(eval-when (compile load eval)

  (defpackage :user
    #+(version>= 7) (:import-from :sys #:with-timeout)
    #-(version>= 7) (:import-from :mp #:with-timeout)
    )
  )

#-(version>= 7) (mp:start-scheduler)   ;; to enable with-timeout
 
(defpackage :tns (:use))

(define-namespace-map :xmeth-base
  nil
  '(:keyword
    "tns3"
    "http://www.xmethods.net/interfaces/query")
  :all
  '(:keyword nil :any))

(define-namespace-map :xmeth-entry nil :all)

(defun xmeth-def (&optional (source "xmeth.wsdl"))
  (let ((w (typecase source
	     ((member :uri)
	      (decode-wsdl-at-uri "http://www.xmethods.net/wsdl/query.wsdl"
				  :namespaces :decode
				  :base       :xmeth-base))
	     ((or string pathname)
	      (decode-wsdl-file source
				:namespaces :decode
				:base       :xmeth-base)))))
    (make-client-interface w 0 "xmeth-client.cl"
			   :suffix :message :verbose t :map :xmeth-ns)
    (load (compile-file-if-needed "xmeth-client.cl"))
    "xmeth-client.cl"))

(defun xmeth-client-call ()
  (funcall '|client-getAllServiceNames|))
    

(defvar *xmethods* nil )
(defun xmeth-get ()
  (setf *xmethods* 
	(concatenate 'list (soap-result-part nil (xmeth-client-call) nil nil)))
  )
(defun xmeth-save ()
  (with-open-file (s "xmeth-list.data"
		     :direction :output :if-exists :supersede)
		  (format s "~&(~%~{  ~S~%~}~%)~%" *xmethods*))
  (probe-file "xmeth-list.data"))

(defun xmeth-read ()
  (with-open-file (s "xmeth-list.data") (setf *xmethods* (read s))))

(defun xmeth-load (&optional init-table init-clients)
  (when init-clients (xmeth-def))
  (cond ((probe-file "xmeth-client.cl")
	 (load (compile-file-if-needed "xmeth-client.cl"))
	 )
	(t (xmeth-def)))
  (when init-table (xmeth-get) (xmeth-save))
  (cond (*xmethods*)
	((probe-file "xmeth-list.data") (xmeth-read))
	(t (xmeth-get) (xmeth-save))))



(defvar *errors* nil)
(defvar *ok* 0)
(defvar *bad* 0)
(defun xmeth-errorp (errorp i name body)
  (let* (
	 (text nil)
	 ;;(text (open name :direction :output :if-exists :supersede))
	 
	 (place (elt *xmethods* i))
	 (fmark (assoc :acl-name place))
	 (fres  (assoc :acl-result place))
	 (ferr  (assoc :acl-error place))
	 (fskip (assoc :acl-skip place))
	 )
    (if fmark
	(setf (second fmark) name)
      (setf (cdr place)
	    (cons (setf fmark (list :acl-name name)) (cdr place))))
    (or fres (setf (cdr place)
		   (cons (setf fres (list :acl-result nil)) (cdr place))))
    (or ferr (setf (cdr place)
		   (cons (setf ferr (list :acl-error nil)) (cdr place))))
    (if fskip
	(format t "~& Skip ~A~%" i)
      (unwind-protect
	  (let (vals err)
	    (format t "~&Begin ~A at ~A~%" i (timestamp nil))
	    (if errorp
		(setf vals (multiple-value-list (funcall body i)))
	      (multiple-value-bind (v e)
		  (ignore-errors
		    (with-timeout
		     (300 (error "timeout"))
		     (multiple-value-list (funcall body i))))
		(cond
		 ((null e) (setf (second ferr) nil))
		 (t
		  (setf err "with error")
		  (setf e (format nil "~A" e))
		  (when (search "[in Sax parser]" e :test 'equalp)
		    (setf e "SAX parse error"))
		  (setf (second ferr) e)
		  (when (< 80 (length e)) (setf e (subseq e 0 80)))
		  (format t    "~&~%ERROR ~A~%~A~%" i e)
		  (when (< 50 (length e)) (setf e (subseq e 0 50)))
		  (let* ((place (assoc e *errors* :test #'equal)))
		    (if place
			(incf (second place))
		      (push (setf place (list e 1)) *errors*))
		    (cond ((< 12 (length place)))
			  ((= 12 (length place)) (push :more (cddr place)))
			  (t (push i (cddr place))))
		    )
		  ))
		(setf vals v)))
	    (cond (err (setf (cdr fres) :error)
		       (incf *bad*))
		  (t   (setf (cdr fres) :ok)
		       (incf *ok*)))
	    (format t "~&  End ~A ~A~%" i (or err "ok"))
	    vals)
	(when text (close text))
	))))

(defun xmeth-one-remote (&optional (i 0) errorp (folder "xmethods"))
  ;; fetch WSDL from URL
  (let* ((name (read-from-string (format nil ":xmeth~3,'0D" i))))
    (xmeth-errorp
     errorp i (format nil "~A/~A.txt" folder name)
     #'(lambda (i)
	 (let* ((id (soap-result-part nil (elt *xmethods* i) :|id|))
		(res (funcall '|client-getServiceDetail| :id id))
		(def (soap-result-only nil res :error "getServiceDetailResponse" nil))
		(wsdl-url (soap-result-part nil def "wsdlURL"))
		(string (net.aserve.client:do-http-request wsdl-url))
		)
	   (if string
	       (with-open-file (s (format nil "~A/~A.xml" folder name)
				  :direction :output
				  :if-exists :supersede)
			       (write-line string s))
	     (error "URL ~A returns nil."  wsdl-url))
	   (or (and (< 20 (length string))
		    (let ((nb (position #\space string :test-not #'eql)))
		      (string-equal "<?xml " string :start2 nb :end2 (+ nb 6))))
	       (error "Reply is not XML"))
	   (xmeth-body i folder name)
	   )))))

(defun xmeth-one-local (&optional (i 0) errorp (folder "xmethods"))
  ;; assume WSDL is in local file
  (let* ((name (read-from-string (format nil ":xmeth~3,'0D" i))))
    (xmeth-errorp
     errorp i (format nil "~A/~A-2.txt" folder name)
     #'(lambda (i) (xmeth-body i folder name)))))

(defun xmeth-body (i folder name)
  (let* (vals (path (format nil "~A/~A.xml" folder name)))
    (or (probe-file path) (error "XML file not found."))
    (setf vals (multiple-value-list
		(decode-wsdl-file path
				  :namespaces :decode
				  :base
				  (list nil 
					:xmeth-entry
					(list (format nil "tns~A-" i) nil :prefix))
				  )))
	   
    (xmeth-services vals i folder name)
    ))

(defun xmeth-one-decode (&optional (i 0) (folder "xmethods"))
  (let* ((name (read-from-string (format nil ":xmeth~3,'0D" i))))
    (decode-wsdl-file (format nil "~A/~A.xml" folder name)
		      :namespaces :decode
		      :base
		      (list nil 
			    :xmeth-entry
			    (list (format nil "tns~A-" i) nil :prefix))
		      )))

(defun xmeth-one-ns (&optional (i 0) (map :xmeth-base) (folder "xmethods"))
  (let* ((name (read-from-string (format nil ":xmeth~3,'0D" i))))
    (decode-wsdl-namespaces :file (format nil "~A/~A.xml" folder name)
			    :map map
			    )))



(defun xmeth-services (vals i folder name
			    &aux
			    (sns (format nil "client~A-" i))
			    wconn services done)
  (when (null (car vals))
    (error "NAMESPACE PROBLEM~%~{~S~%~}" (cdr vals)))
  (setf wconn (car vals))
  (setf services (wsdl-service-names wconn :verbose))
  (if (and (eql 1 (length services))
	   (eql 2 (length (first services))))
      (if (third (second (first services)))
	  (xmeth-client wconn (first services) folder name nil sns)
	(error "Not a SOAP service."))
    (dolist (s services (or done (error "Did not find a SOAP service.")))
      (dotimes (j (length (cdr s)))
	(when (third (elt (cdr s) j))
	  (setf done t)
	  (xmeth-client wconn s folder name j sns)
	  )))))

(defun xmeth-client (wconn s folder name j sns &aux r)
  (setf r (make-client-interface wconn (first s)
				 (if j
				     (format nil "~A/~A-~A.cl"
					     folder name j)
				   (format nil "~A/~A.cl" folder name))
				 :port j
				 :prefix (if j (format nil "~A-~A" sns j) sns)
				 :map (read-from-string
				       (format nil ":ns-~A-~A" sns (or j "ns")))
				 :text-file nil
				 ))
  (dolist (d r (error "Service with no client functions."))
    (and (consp (first d))
	 (eq 'defun (getf (cdr (first d)) :key))
	 (cdr d)
	 (return))))


(defun xmeth-summary (stream)
  (setf *errors* (sort *errors* #'< :key #'second))
  (format stream "~%Errors:~%~{ ~S~%~}"  *errors*)
  (format stream "~%OK: ~A   All errors: ~A   Total: ~A~%"
	  *ok*
	  *bad*
	  (+ *ok* *bad*))
  )

(defun xmeth-all-remote (&optional (start 0) (end (length *xmethods*) e-p))
  ;; fetch WSDL from URLs
  (xmeth-load)
  (or e-p (setf end (length *xmethods*)))
  (dribble "xmeth.log")
  (setf *errors* nil *ok* 0 *bad* 0)
  (dotimes (i end) (or (< i start) (xmeth-one-remote i)))
  (xmeth-summary t)
  (xmeth-save)
  (dribble))

(defun xmeth-all-local (&optional (start 0) (end (length *xmethods*) e-p))
  ;; scan local WSDL files
  (xmeth-load)
  (or e-p (setf end (length *xmethods*)))
  (setf *errors* nil *ok* 0 *bad* 0)
  (dribble "xmeth.log")
  (dotimes (i end) (or (< i start) (xmeth-one-local i)))
  (xmeth-summary t)
  (xmeth-save)
  (dribble))


(defun timestamp (stream &key seconds date (prefix "") (suffix ""))
  (multiple-value-bind (sec min hr day mo yr)
      (get-decoded-time)
    (cond
     ((and seconds date)
      (format stream "~A~A-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~A"
	      prefix yr mo day hr min sec suffix))
     (date (format stream "~A~A-~2,'0D-~2,'0D ~2,'0D:~2,'0D~A"
		   prefix yr mo day hr min suffix))
     (seconds (format stream "~A~2,'0D:~2,'0D:~2,'0D~A" prefix hr min sec suffix))
     (t (format stream "~A~2,'0D:~2,'0D~A" prefix hr min suffix)))))


;;; xmethods.com






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



(defun xmp-match-tests (&optional index &aux (i 0) (sum t) (good 0) (bad 0))
  (let ((all '(
	       ;; * pattern [data result alist]... 
               *
	       (foo (bar baz))    (foo (bar baz))      t  nil
	                          (foo (bar baz) 1)   nil nil
               *
	       (foo (bar :?any))  (foo (bar baz))      t  nil
                                  (foo (bar bax))      t  nil
               *
	       ((:?each nil foo)) (foo foo foo)        t  nil
                                  (foo foo bar)       nil nil
               *
               ((:?each x foo))   (foo foo foo)        t  ((x foo foo foo))
	                          ()                   t  nil
	       *
	       ((:? x) ((:?each nil foo)))
	                          (a (foo foo))        t  ((x . a))
               *
	       (:?or foo bar)     foo                  t  nil
                                  bar                  t  nil
                                  bax                 nil nil
               *
	       ((:?or (:? x foo) (:? y bar)) . (:? tail))
                                  (foo 1 2)             t ((tail 1 2) (x . foo))
				  (bar 3 4)             t ((tail 3 4) (y . bar)
							   (x . bar))
				  (baz 5 6)            nil ((y . baz) (x . baz))
               *
	       ((:?or (:? x foo) (:? x bar)) . (:? tail))
	                          (bar 3 4)             t ((tail 3 4) (x . bar))
	       *
	       ((:?or (:?and foo (:? x)) (:?and bar (:? y))) . :?any)
                                  (foo 1 2)             t  ((x . foo))
				  (bar 1 2)             t  ((y . bar))
				  (baz 1 2)            nil nil
               *
	       (foo (bar (:?each nil :?any)))
                                  (foo (bar a b))       t nil
	     
	       ))
	x pattern data res alst)
    (loop
     (when (atom all) (return))
     (setf x (pop all))
     (when (eql x '*) (setf pattern (pop all)) (setf x (pop all)))
     (setf data x res (pop all) alst (pop all))
     (when (or (null index) (eql index i))
       (multiple-value-bind (r a)
	   (net.xmp:match-tree data pattern nil)
	 (or (and (eql res r) (equal a alst))
	     (format t "~&; test ~A   pattern: ~S   data: ~S~%"
		     i pattern data))
	 (or (eql res r) 
	     (format t "~&;  results not eql expected ~S returned ~S~%"
		     res r))
	 (or (equal a alst)
	     (format t "~&;  expected alist ~S returned ~S~%"
		     alst a))
	 (cond ((and (eql res r) (equal a alst))
		(incf good))
	       (t (incf bad) (setf sum nil)))
	 ))
     (incf i))
    (values sum bad good)))

(defun xmp-break-tests (&aux (all t) (good 0) (bad 0))
  (dolist (c '(("getVal"       "get-val")
	       ("accessVO"     "access-vo")
	       ("objectId_12"  "object-id_12")
	       ("FooClass"     "foo-class")
	       ("URLLink"      "url-link")
	       ("longURL"      "long-url")
	       ("longIDString" "long-id-string")
	       ))
    (let ((in (first c)) r
	  (out (second c)))
      (cond ((equal out (setf r (net.xmp.soap::break-at-case-shifts in)))
	     (incf good))
	    (t (incf bad) (setf all nil)
	       (format t "~&; in:~S want:~S res:~S~%" in out r)))))
  (values all good bad))



(defun http-test1 ()
  (unwind-protect
      (let ((c (soap-message-client
		:lisp-package :keyword
		:decode-flag nil :soap-debug t
		:url "http://services.xmethods.net:80/soap/servlet/rpcrouter"
		:xml-leader "xml version='1.0'"
		:xml-encoding '(:utf8-base "UTF8")
		:http-method :get
		:http-protocol :http/1.1
		:content-type  "text/mime"
		:http-agent    "foo"
		:http-host     "bar"
		:http-headers  '(("MORE" . "headers"))
		)))
	(trace net.aserve.client:do-http-request)
	(ignore-errors
	  (call-soap-method c '(:element "msg" (:complex (:seq))))))
    (trace net.aserve.client:do-http-request)))


	    
