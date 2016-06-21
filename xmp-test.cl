;; -*- mode: common-lisp; package: user -*-
;;
;; See the file LICENSE for the full license governing this code.


;; Internal-use test cases

(in-package :user)

(eval-when (compile load eval)
  (defvar *xmp-test-case-mode* (macrolet ((cm () *current-case-mode*)) (cm)))
  )

(eval-when (load eval)
  (or (eq *xmp-test-case-mode* *current-case-mode*)
      (cerror "load anyway!" "This file xmptest.cl must be compiled in the run-time case mode."))
  )

#|

----- TESTING PROCEDURE (of sorts) -- run on alisp AND mlisp -----

(test-soap-local) --> test report from with-tests wrapper
                      (soap-local-tests)  - just the tests
(test-soap-files) --> run wsdl tests in xmp-file-tests

(test-soap-remote) --> test report from with-tests wrapper
                       (soap-remote-tests)  - just the tests

;; :cd xmethods-test-folder
(test-soap-x [remote]) -> test report  (parse all WSDL at XMethods.com)



Individual tests:

;; If ./xmp-file-tests folder is visible
(soap-files-tests)

- From soapex.cl and soapval1.cl -
(test-clients) --> t   ;; some clients may not respond
(run3 count) --> ((init extend calls-to-adjust-array) ...)
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

(run4)       ;;; anyType encoding
(run5)       ;;; nillable elements

- interacting with xmethods.com -
(xmeth-def)          ;; decode xmeth.wsdl
(xmeth-get)          ;; get list of methods from server
(xmeth-read)          ;; read local list of methods
(xmeth-save)          ;; save local list of methods

;; fetch and parse one WSDL def from server
(xmeth-one-remote [:i i [:errorp errorp [:folder folder]]])
;; fetch and parse one local WSDL def
(xmeth-one-local [:i i [:errorp errorp [:folder folder]]])

(xmeth-all-remote)         ;; run through all remote xmethods
(xmeth-all-local)         ;; run through all local xmethods

- WSDL decoding tests - 
(wsdl11) --> ss1.wsdl ==> try with SOAPScope
(wsdl12) --> ss2.wsdl ==> try with SOAPScope
(wsdl13) --> ssv.wsdl ==> try with SOAPScope
(wsdl22) --> amazon-client.cl
(wsdl25) --> amazon-gen.wsdl
(wsdl42) --> google-client.cl
(wsdl43) --> google-server.cl
(wsdl45) --> google-gen.wsdl

|#


(eval-when (compile load eval) (require :tester))
(defpackage :user (:use :util.test)) 

(eval-when (compile load eval)

  #-soap-two-fasls
  (or (member :soap *modules* :test #'string-equal)
      (when (probe-file "soap.fasl") (load "soap.fasl")))
	     
  #+soap-two-fasls
  (let* ((module (ecase *current-case-mode*
		   (:case-sensitive-lower :soapm)
		   (:case-insensitive-upper :soapa)))
	 (file (string-downcase (format nil "~A.fasl"  module))))
    (or (member :soap *modules* :test #'string-equal)
	(when (probe-file file)
	  (load file)
	  (provide module)
	  (provide :soap))))

  (require :soap)
  )

(eval-when (compile load eval)
  ;; always compile these to avoid casemode problems
  (load (compile-file "xmp-mtest.cl"))
  (load (compile-file "soapex.cl"))
  (load (compile-file "soapval1.cl"))
  )

(defpackage :user (:use :net.xmp.soap)) 

(defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
(defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
(defpackage :net.xmp.soap.none (:use) (:nicknames :none))
(defpackage :net.xmp.soap.envelope (:use) (:nicknames :env))
(defpackage :net.xmp.soap.encoding (:use) (:nicknames :enc))

(defpackage :temp (:use))
(defpackage :gg (:use))



(defun test-soap-local (&key verbose (files "xmp-file-tests") &aux r)
  (with-tests
   (:name "SOAP module (local)")
   (setf *error-protect-tests* t)
   (setf r (soap-local-tests :verbose verbose :files files)))
  r)


(defun test-soap-files (&key verbose (folder "xmp-file-tests") &aux r v)
  (with-tests
   (:name "SOAP file tests")
   (setf *error-protect-tests* t)
   (multiple-value-setq (r v) (soap-files-tests :folder folder :verbose verbose)))
  (values r v))

(defun soap-files-tests (&key verbose (folder "xmp-file-tests") &aux up r v)
  (unwind-protect
      (let ()
	(tpl:do-command :cd folder)
	(setf up t)
	(load (compile-file "xmp-driver"))
	(multiple-value-setq (r v) (funcall 'xmp-run-test-files :verbose verbose)))
    (ignore-errors (delete-file "xmp-driver.fasl"))
    (when up 	(tpl:do-command :cd "..")))
  (values r v))

(defun soap-local-tests (&key verbose (files "xmp-file-tests"))
  (macrolet ((with-timer
	      (&rest forms &aux (tag (cond ((null forms) "empty")
				((consp (first forms)) (caar forms))
				(t (first forms)))))
	      `(let ((time (get-universal-time))
		     (tag ',tag))
		 (when verbose
		     (format t "~&;begin ~A" tag))
		 (unwind-protect (let () ,@forms)
		   (when verbose
		     (format t "~&;  end ~A - ~A seconds~%" tag
			     (ignore-errors (- (get-universal-time) time))))))))
    (and 
     (with-timer (test-decoder-all))
     (with-timer (xmp-match-tests))
     (with-timer (xmp-break-tests))

     (with-timer "ss1" (test nil (null (ss1 :index :all))))
     (with-timer "ss2" (test nil (null (ss2))))
     (with-timer (load (compile-file "soapval1.cl")))
     (with-timer "validator1" (test :all-ok (test-validator1)))
     (with-timer (run4))
     (with-timer (run5))
     (with-timer (ts1001))
     (with-timer (ts1002))
     (with-timer (ts1003))
     (with-timer (ts2001))
     (with-timer (ts2010))
     (with-timer (ts2020))
     (with-timer (ts2030))
     (with-timer "soap-files-tests" 
		 (if (and files (probe-file files))
		     (soap-files-tests :verbose verbose :folder files)
		   t))
     ;; do this one after soap-files-tests to avoid package pollution
     (with-timer "test-bn" (test t (test-bn)))
     (with-timer "stop-soap-server" 
		 (test-no-error (stop-soap-server (symbol-value '*bn-server*))))
     )))

(defun test-soap-remote (&key (timeout 60) &aux r)
  (with-tests
   (:name "SOAP module (external servers)")
   (setf *error-protect-tests* t)
   (setf r (soap-remote-tests :timeout timeout)))
  r)

(defun soap-remote-tests (&key (timeout 60))
  (load (compile-file "soapex.cl")) ;; to make sure namespaces are defined
  (test nil (null (test-clients :timeout timeout)))
  )

(defun test-soap-x (&optional remote &aux r)
  (xmeth-folder)
  (if remote 
      (with-tests 
       (:name "Xmethods (fetch external WSDL)")
       (setf *error-protect-tests* t)
       (setf r (and 
		(test-no-error (xmeth-def :source :uri))
		(test-no-error (xmeth-get))
		(test-no-error (xmeth-save))
		(progn (xmeth-all-remote) t)
		(test-no-error (xmeth-save))
		)))
    (with-tests
     (:name "Xmethods (local copies of WSDL)")
     (setf *error-protect-tests* t)
     (setf r (and 
	      (test-no-error (xmeth-def))
	      (test-no-error (xmeth-read))
	      (progn (xmeth-all-local) t)
	      (test-no-error (xmeth-save))
	      )))
    )
  r)
  
	

(defun test-bn ()
  (load (compile-file "bignum-server.cl"))
  (let* ((server (funcall 'start-server))
	(r (funcall 'try-server))
	(v (and (equal (format nil "~A" (apply 'factorial 17 0 nil))
		(soap-result-only nil (elt r 0) nil "calculateResponse" "calcResult"))
	 (equal "17"
		(soap-result-only nil (elt r 1) nil "encodeNumResponse" "encResult"))
	 (equal `((:|item| 1) (:|item| 2) (:|item| 3) (:|item| 4) (:|item| 5))
		(soap-result-only nil (elt r 2) nil "decodeNumResponse" "decResult"))
	 t)))
    (stop-soap-server server)
    v))




(defun s1 () (net.xmp::decode-schema :file "envelope.xml"))
(defun s2 () (net.xmp::decode-schema :file "encoding.xml"))
;;(defun s3 () (net.xmp::decode-schema :file "XMLSchema.xml"))

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

(defun ct1 ()

  (net.xmp:define-xmp-element
   nil :ct1 '(:complex 
	      (:or 
	       (:seq (:element :foo xsd:|int|) :bar)
	       (:seq (:element :foo xsd:|string|) :baz))))
  
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
(defun ct2 () 

  (net.xmp:define-xmp-element
   nil :ct2 '(:complex (:or (:seq (:element 
				   :foo 
				   (:complex (:seq :bar :baz)))
				  :bob)
			    (:seq (:element 
				   :foo 
				   (:complex (:seq :baz :bar)))
				  :box))))

  (let* ((conn (make-instance 'ct1)))
    (net.xmp::xmp-decode-message
     conn
     '((:ct2 (:foo (:bar "bar") (:baz "baz")) (:bob "bob"))))))
(defun ct3 () 

  (net.xmp:define-xmp-element
   nil :ct2 '(:complex (:or (:seq (:element 
				   :foo 
				   (:complex (:seq :bar :baz)))
				  :bob)
			    (:seq (:element 
				   :foo 
				   (:complex (:seq :baz :bar)))
				  :box))))

  (let* ((conn (make-instance 'ct1)))
    (net.xmp::xmp-decode-message
     conn
     '((:ct2 (:foo (:baz "baz") (:bar "bar")) (:box "box"))))))
(defun ct4 () 

  (net.xmp:define-xmp-element
   nil :ct2 '(:complex (:or (:seq (:element 
				   :foo 
				   (:complex (:seq :bar :baz)))
				  :bob)
			    (:seq (:element 
				   :foo 
				   (:complex (:seq :baz :bar)))
				  :box))))

  ;; should fail
  (let* ((conn (make-instance 'ct1)))
    (net.xmp::xmp-decode-message
     conn
     '((:ct2 (:foo (:baz "baz") (:bar "bar")) (:bob "bob"))))))





;;; SOAP Server Tests

(defpackage :ss1 (:use))

(defun soap-method-1 (&key (|elt1| 0) (|elt2| 0) (|elt3| 0))
  (list 'ss1::result (+ |elt1| |elt2| |elt3|)))

(defun ss1 (&key (index 1 )(port 4567) (path "/SOAP") debug (stop t))

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

  (let* ((url (format nil "http://localhost:~A~A" port path))
	 (server (soap-message-server :start `(:port ,port)
				      :publish `(:path ,path)
				      :lisp-package :keyword
				      :message-dns '(nil (:ss1))
				      :soap-debug (ts-debug :server debug)
				      :url url
				      :wsdl "<def/>"
				      )))
    ;; Define a non-SOAP response from the server to test 
    ;;  the fix for bug17614.
    (net.aserve:publish :path (format nil "~Ax" path)
			:function #'(lambda (req ent)
				      (net.aserve:with-http-response
				       (req ent)
				       (net.aserve:with-http-body
					(req ent)
					(net.html.generator:html (:body "body"))))))

    (soap-export-method server 'ss1::soap-method-1 '(:|elt1| :|elt2| :|elt3|)
			:return 'ss1::soap-result-1
			:action "uri:method1"
			:lisp-name 'soap-method-1)
    (unwind-protect
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
					  client 'ss1::soap-method-1 
					  :|elt1| e1 :|elt2| e2)))
				((and e1 e3)
				 (setq r (call-soap-method
					  client 'ss1::soap-method-1
					  :|elt1| e1 :|elt3| e3)))
				((and e2 e3)
				 (setq r (call-soap-method
					  client 'ss1::soap-method-1 
					  :|elt2| e2 :|elt3| e3)))
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
	  (let ((r1
		 (let* ((client (soap-message-client :url url
						     :lisp-package :keyword
						     :message-dns '(nil (:ss1))
						     :soap-debug (ts-debug :client debug)
						     ))
			(result
			 (and  (one client index 0 111 222 333)
			       (one client index 1 123 nil nil)
			       (one client index 2 nil 456 nil)
			       (one client index 3 nil nil 789))))		  
		   (list result server client net.xmp.soap::*soap-last-server*)
		   ))
		(r2
		 (let* ((client (soap-message-client :url url
						     :lisp-package :keyword
						     :message-dns '(nil (:ss1))
						     :soap-debug (ts-debug :client debug)
						     ))
			(result
			 (and  (one client index 0 111 222 333)
			       (one client index 1 123 nil nil)
			       (one client index 2 nil 456 nil)
			       (one client index 3 nil nil 789))))		  
		   (list result server client net.xmp.soap::*soap-last-server*)
		   ))
		(r3
		 (let* ((client (soap-message-client :url (format nil "~Ax" url) ;;; create a bad URL
						     :lisp-package :keyword
						     :message-dns '(nil (:ss1))
						     :soap-debug (ts-debug :client debug)
						     )))
		   (multiple-value-bind (v e)
		       (ignore-errors 
			 (progn
			   (call-soap-method client 'ss1::soap-method-1 :|elt2| 17)
			   17))
		     (list
		      (and (null v) e (search "not a SOAP XML document" (format nil "~A" e)))
		      v e))  ))
		(r4 (multiple-value-bind 
			(body rc)
			(net.aserve.client:do-http-request    ;; test for wsdl [rfe8375]
			 (format nil "~A?wsdl" url))
		      (list (and (eql 200 rc)
				 (equal "<def/>" body))
			    rc body)))
		)
	    (values (and (first r1) (first r2) (first r3) (first r4)) r1 r2 r3 r4)
	    ))
      (and stop server (stop-soap-server server)))
    ))

(defun ss2 () 
  (load (compile-file "soapex.cl"))
  (and (simple-server :ns 0)
       (simple-server :ns 1)
       (null (simple-server :ns 2))))






;; Manual test - no simple way to predict all results 
(defun run3 (count &rest keys)

  (define-soap-element nil 'ss1::soap-method-3
    '(:complex (:seq (:element :|elt1| xsd:|string|))
	       :action "uri:method3"
	       ))
  (define-soap-element nil 'ss1::soap-result-3 
    '(:complex (:seq (:element ss1::result (:simple xsd:|int|)))))
  (defun soap-method-3 (&key |elt1|)
    (list 'ss1::result (length  |elt1|)))

  (mapcar #'(lambda (args)
	      (apply #'(lambda (init extend)
			 (prof:with-profiling
			  (:type :count-only)
			  (apply 'ss3 :count count :init init :extend extend keys))
			 (list init extend 
			       (dolist (c (prof:list-call-counts))
				 (when (eq 'adjust-array
					   (ignore-errors
					     (excl::external-fn_symdef (cdr c))))
				   (return (car c))))))
		     args))
	  '(
	    (nil nil)
	    (500 nil)
	    (500 100)
	    (12000 nil)
	    (25000 nil)
	    (12000 0.5)
	    (1000 2.0)
	    )))
  

(defun ss3 (&key (count 100) log
		 (length 20000)
		 (init nil) (extend nil)
		 (port 4567) (path "/SOAP") debug (stop t))

  (define-soap-element nil 'ss1::soap-method-3
    '(:complex (:seq (:element :|elt1| xsd:|string|))
	       :action "uri:method3"
	       ))
  (define-soap-element nil 'ss1::soap-result-3 
    '(:complex (:seq (:element ss1::result (:simple xsd:|int|)))))
  (defun soap-method-3 (&key |elt1|)
    (list 'ss1::result (length  |elt1|)))

  (let* ((oldlog net.aserve::*enable-logging*)
	 (url (format nil "http://localhost:~A~A" port path))
	 (server (soap-message-server :start `(:port ,port)
				      :publish `(:path ,path)
				      :lisp-package :keyword
				      :message-dns '(nil (:ss1))
				      :soap-debug (ts-debug :server debug)
				      :url url
				      )))
    (setf net.aserve::*enable-logging* log)
    (soap-export-method server 'ss1::soap-method-3 '(:|elt1|)
			:return 'ss1::soap-result-3
			:action "uri:method3"
			:lisp-name 'soap-method-3)
    (unwind-protect
	(let (client result)		  
	    
	  (dotimes (i count)
	    ;; make a new client each time around to start with fresh
	    ;; message buffer each time
	    (setf client (soap-message-client :url url
					      :lisp-package :keyword
					      :message-dns '(nil (:ss1))
					      :message-init
					      (if extend (list init extend) init)
					      :soap-debug (ts-debug :client debug)
					      ))
	    (or (eql
		 length
		 (soap-result-only
		  client 
		  (setf 
		   result
		   (call-soap-method 
		    client 'ss1::soap-method-3
		    :|elt1| (make-string length :initial-element #\a)))
		  t 'ss1::soap-result-3 'ss1::result))
		(error "Not eql ~S ~S" length result)))
	  count)
      (and stop server (stop-soap-server server))
      (setf net.aserve::*enable-logging* oldlog)
      )
    ))




;;??? test encoders for anyType

(defun soap-method-4 (&key |elt1|)
  (list 'ss1::result (typecase |elt1|
		       (string (length  |elt1|))
		       (cons (length (format nil "~A" |elt1|)))
		       (otherwise -1))))

(defun run4 (&key debug new)
  (soap-test-run
   :new new
   :debug debug
   :dns '(nil (:ss1))
   :server '(:decode-flag nil)
   :client '(:decode-flag nil)
   :define
   #'(lambda ()
       (define-namespace :ss1 "ss1" "urn:SS1NS")
       (define-soap-element nil 'ss1::soap-method-4
	 '(:complex (:seq (:element :|elt1| xsd:|anyType|))
		    :action "uri:method4"
		    ))
       (define-soap-element nil 'ss1::soap-result-4 
	 '(:complex (:seq (:element ss1::result (:simple xsd:|int|))))))
   :export
   #'(lambda (server)
       (soap-export-method server 'ss1::soap-method-4 '(:|elt1|)
			   :return 'ss1::soap-result-4
			   :action "uri:method4"
			   :lisp-name 'soap-method-4))
   :call
   #'(lambda (client)
       (let* ((length 17)
	      (string (make-string length :initial-element #\a)))
	 (and 
	  (test
	   length
	   (soap-result-only
	    client 
	    (call-soap-method 
	     client 'ss1::soap-method-4 :|elt1| string)
	    t 'ss1::soap-result-4 'ss1::result))
	  (test
	   2
	   (soap-result-only
	    client 
	    (call-soap-method 
	     client 'ss1::soap-method-4 :|elt1| 17)
	    t 'ss1::soap-result-4 'ss1::result))
	  (test
	   12 ;; "((foo 1223))"
	   (soap-result-only
	    client 
	    (call-soap-method 
	    client 'ss1::soap-method-4 :|elt1|
	    (soap-encode-object client :foo '(:simple xsd:|string|) (list 12 23)))
	    t 'ss1::soap-result-4 'ss1::result))
	  :ok)
	 
	 ))
   ))
     


      


(defun soap-test-run (&key new define export call 
			   dns debug (port 0) (path "/SOAP") (stop t)
			   log (lisp :keyword) server client)
  (when new (soap-new-environment))
  (when define (funcall define))
  (let* ((oldlog net.aserve::*enable-logging*)
	 (server-instance (apply #'soap-message-server :start `(:port ,port)
				 :publish `(:path ,path)
				 :lisp-package lisp
				 :message-dns dns
				 :soap-debug (ts-debug :server debug)
				 server))
	 (sock (slot-value net.aserve:*wserver* 'net.aserve::socket))
	 (lport (socket:local-port sock))
	 (url (format nil "http://localhost:~A~A" lport path))
	 client-instance result)
    (setf net.aserve::*enable-logging* log)
    (when export (funcall export server-instance))
    (unwind-protect
	(let ()		  
	  (setf client-instance (apply #'soap-message-client :url url
				       :lisp-package lisp
				       :message-dns dns
				       :soap-debug (ts-debug :client debug)
				       client))
	  (when call (setf result (funcall call client-instance))))
      (and server-instance stop (stop-soap-server server-instance))
      (setf net.aserve::*enable-logging* oldlog)
      )
    (values result server-instance client-instance)))


(defmacro show-error ((verbose tag) &rest body &aux (v (gensym)) (e (gensym)))
  `(multiple-value-bind (,v ,e)
       (ignore-errors 
	 (multiple-value-list (progn ,@body)))
     (cond (,e (when ,verbose
		 (format t "~&;show-error ~A: ~S~%; ~A~%" ,tag ,e ,e))
	      (error ,e))
	   (t (values-list ,v)))))


;; Test nillable elements and options [bug15971]
(defun soap-method-5 (&rest args)
	 (list 'ss1::result
	       (format nil "~S" args)))
(defvar *run5verbose* nil)
(defvar *run5-attrs* nil)
(defun run5-attrs (conn elt)
  (declare (ignore conn elt))
  *run5-attrs*)

(defun run5-defs (&key nillable)
  (define-namespace :ss1 "ss1" "urn:SS1NS")
  (case nillable 
    (:strict (define-soap-element nil :|elt2| '(:complex (:seq* :any))
	       :nillable t)))
  (case nillable
    (:ignore   (define-soap-element nil
		 'ss1::soap-method-5
		 '(:complex (:seq (:element :|elt1| xsd:|string|)
				  (:element :|elt2| xsd:|string|)
				  )
			    :action "uri:method5")))
    (otherwise (define-soap-element nil
		 'ss1::soap-method-5
		 '(:complex (:seq (:element :|elt1| xsd:|string|))
			    :action "uri:method5"))))	       
  (define-soap-element nil 'ss1::soap-result-5 
    '(:complex (:seq (:element ss1::result (:simple xsd:|string|))))))

(defun run5-export (server)
       (soap-export-method server 'ss1::soap-method-5 '(:|elt1|)
			   :return 'ss1::soap-result-5
			   :action "uri:method5"
			   :lisp-name 'soap-method-5))

(defun run5-export-ignore (server)
       (soap-export-method server 'ss1::soap-method-5 '(:|elt1| :|elt2|)
			   :return 'ss1::soap-result-5
			   :action "uri:method5"
			   :lisp-name 'soap-method-5))

(defun run5-accept (client
		    &aux
		    (edef '(:element
			    ss1::soap-method-5
			    ;; We need a definition of this element local to the
			    ;; client call because the client must send an element
			    ;; that is not even listed in the server definition.
			    (:complex
			     (:seq (:element :|elt1| xsd:|string|)
				   (:element
				    :|elt2|
				    (:simple xsd:|string|
					     :computed-attributes run5-attrs)))
			     :action "uri:method5"
			     )))
		    )  
  (flet ((result (attr str &rest args)
		 (let ((*run5-attrs* `(xsi:|nil| ,attr)))
		   (test
		    (format nil "(~S ~S)" ':|elt1| str)
		    (soap-result-string client
					(apply 'call-soap-method 
					       client edef args)
					'ss1::soap-result-5 'ss1::result)
		    :test #'equal)))
	 (erret (attr &rest args)
		(let ((*run5-attrs* `(xsi:|nil| ,attr)))
		  (test-error
		   (show-error (*run5verbose* (list "run5-accept" attr))
		    (apply 'call-soap-method client edef args))
		   :condition-type 'net.xmp.soap::soap-client-fault
		   )))
	 )

    (and (result "true" "str1" :|elt1| "str1" :|elt2| nil)
	 (result "true" "str2" :|elt2| nil :|elt1| "str2")
	 (result "1" "str3" :|elt1| "str3" :|elt2| nil)
	 (result "1" "str4" :|elt2| nil :|elt1| "str4")
	 (erret "false" :|elt1| "str5" :|elt2| nil)
	 (erret "0" :|elt1| "str6" :|elt2| nil)
	  )))


(defun run5-strict (client
		    &aux
		    (edef '(:element
			    ss1::soap-method-5
			    (:complex (:seq (:element :|elt1| xsd:|string|)
					    (:element
					     :|elt2|
					     (:simple xsd:|string|
						      :computed-attributes run5-attrs))
					    )
				      :action "uri:method5"
				      )))
		    )  
  (flet ((result (attr str &rest args)
		 (let ((*run5-attrs* `(xsi:|nil| ,attr)))
		   (test
		    (format nil "(~S ~S)" ':|elt1| str)
		    (soap-result-string client
					(apply 'call-soap-method 
					       client edef args)
					'ss1::soap-result-5 'ss1::result)
		    :test #'equal)))
	 (erret (attr &rest args)
		(let ((*run5-attrs* `(xsi:|nil| ,attr)))
		  (test-error
		   (show-error (*run5verbose* (list "run5-strict" attr))
		    (apply 'call-soap-method client edef args))
		   :condition-type 'net.xmp.soap::soap-client-fault
		   )))
	 )

    (and (result "true" "str1" :|elt1| "str1" :|elt2| nil)
	 (result "true" "str2" :|elt2| nil :|elt1| "str2")
	 (result "1" "str3" :|elt1| "str3" :|elt2| nil)
	 (result "1" "str4" :|elt2| nil :|elt1| "str4")
	 (erret "false" :|elt1| "str5" :|elt2| nil)
	 (erret "0" :|elt1| "str6" :|elt2| nil)
	 (erret "true" :|elt1| "str6" :|elt2| "s7")  ;;; not empty
	  )))

(defun run5-strict-b (client
		      &aux
		      (edef '(:element
			      ss1::soap-method-5
			      (:complex (:seq (:element :|elt1| xsd:|string|)
					      (:element
					       :|elt2|
					       (:simple xsd:|string|
							:computed-attributes run5-attrs))
					      )
				      :action "uri:method5"
				      )))
		      )  
  (flet ((erret (attr &rest args)
		(let ((*run5-attrs* `(xsi:|nil| ,attr)))
		  (test-error
		   (show-error (*run5verbose* (list "run5-strict-b" attr))
		    (apply 'call-soap-method client edef args))
		   :condition-type 'net.xmp.soap::soap-client-fault
		   ))))

    (and (erret "true" :|elt1| "str5" :|elt2| nil)
	 (erret "false" :|elt1| "str5" :|elt2| nil)
	 (erret "true" :|elt1| "str6" :|elt2| "s7")  ;;; not empty
	  )))

(defun run5-ignore (client
		    &aux
		    (edef '(:element
			    ss1::soap-method-5
			    (:complex (:seq (:element :|elt1| xsd:|string|)
					    (:element
					     :|elt2|
					     (:simple xsd:|string|
						      :computed-attributes run5-attrs))
					    )
				      :action "uri:method5"
				      )))
		    )  
  (flet ((result (attr &rest args)
		 (let ((*run5-attrs* `(xsi:|nil| ,attr)))
		   (test
		    (format nil "~S" (mapcar #'(lambda (arg) (or arg "")) args))
		    (soap-result-string client
					(apply 'call-soap-method 
					       client edef args)
					'ss1::soap-result-5 'ss1::result)
		    :test #'equal :fail-info (list 'run5-ignore attr))))
	 )

    (and (result "true" :|elt1| "str1" :|elt2| nil)
	 (result "false" :|elt1| "str1" :|elt2| nil)
	 (result "1" :|elt1| "str3" :|elt2| nil)
	 (result "0" :|elt1| "str3" :|elt2| nil)
	  )))


(defun run5 (&key debug verbose &aux (*run5verbose* verbose))
  (and (run5b :debug debug :new t)
       (run5b-b :debug debug :new t)
       (run5c :debug debug :new t)
       (run5d :debug debug :new t)
       (run5e :debug debug :new t)
       ))



(defun run5b (&key debug new)
  ;; :strict setting
  (soap-test-run
   :new new :debug debug
   :define #'(lambda () (run5-defs :nillable :strict))
   :export #'run5-export
   :client '(:nillable :strict)
   :server '(:nillable :strict)
   :call #'run5-strict 
   :dns '(nil (:ss1))
   ))

(defun run5b-b (&key debug new)
  ;; :strict setting but not declared
  (soap-test-run
   :new new :debug debug
   :define #'(lambda () (run5-defs))
   :export #'run5-export
   :client '(:nillable :strict)
   :server '(:nillable :strict)
   :call  #'run5-strict-b
   :dns '(nil (:ss1))
   ))

(defun run5c (&key debug new)
  ;; explicit :accept setting
  (soap-test-run
   :new new :debug debug
   :define #'(lambda () (run5-defs))
   :export #'run5-export
   :client '(:nillable :accept)
   :server '(:nillable :accept)
   :call  #'run5-accept 
   :dns '(nil (:ss1))
   ))

(defun run5d (&key debug new)
  ;; explicit :ignore setting
  (soap-test-run
   :new new :debug debug
   :define #'(lambda () (run5-defs :nillable :ignore))
   :export #'run5-export-ignore
   :client '(:nillable :ignore)
   :server '(:nillable :ignore)
   :call  #'run5-ignore
   :dns '(nil (:ss1))
   ))

(defun run5e (&key debug new)
  ;; default :ignore setting
  (soap-test-run
   :new new :debug debug
   :define #'(lambda () (run5-defs :nillable :ignore))
   :export #'run5-export-ignore
   :call  #'run5-ignore
   :dns '(nil (:ss1))
   ))



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
	 

(defun wsdl11 (&optional (file "ss1.wsdl"))
  
  (encode-wsdl-file file
		    :servers (list "SS1" (nth-value 1 (ss1 :index 17)))
		    :target "urn:SS1NS"
		    ))


(defclass ss1-class (wsdl-file-connector) ())
(defmethod wsdl-generate-code ((conn ss1-class) (mode (eql :client)) (info t)
			       (op (eql 'soap-message-client)) &rest args)
  (list* op :more "more" args))
(defun wsdl11a ()
  (wsdl11 "ss1-test.wsdl")
  (soap-new-environment)
  (make-client-interface 
   (decode-wsdl-file "ss1-test.wsdl")
   0
   "ss1-test1.cl"
   )
  (soap-new-environment)
  (make-client-interface 
   (decode-wsdl-file "ss1-test.wsdl" :class 'ss1-class)
   0
   "ss1-test2.cl"
   )
  )


(defun wsdl12 ()
  (encode-wsdl-file "ss2.wsdl"
		    :servers (nth-value 1 (simple-server :ns 1))
		    :target :ts
		    ))
(defun wsdl13 ()
  (load (compile-file "soapval1.cl"))
  (encode-wsdl-file "ssv.wsdl"
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









(defun test-clients (&key index (error-p nil) val-p (timeout 60)
			  &aux (fail 0) answer)

  (macrolet ((run-one (this body &optional (expected nil e-p))
		      `(when (or (null index) (eql index ,this))
			 (let ((res (mp:with-timeout
				     (timeout :timeout)
				     (if error-p
					,body
				      (multiple-value-bind (v e)
					  (ignore-errors (multiple-value-list ,body))
					(if e
					    (list nil e)
					  v))))))
			   (cond (val-p (setf answer res))
				 ((eq res :timeout))
				 ,@(when e-p `((t (setf res (test-res res ',expected)))))
				 )
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
      ;;(run-one 10 (sp10) temp::|getTempResponse|)
      ;;(run-one 21 (sp21) baseball::|GetTeamsResponse|)
      ;;(run-one 22 (sp22) baseball::|GetPlayersResponse|)
      ;;(run-one 30 (sp30) temp::|getRateResponse|)
      (let ((*xmp-redef-default* nil))
	(run-one 40 (sp40) temp::|getVersionResponse|))
      (run-one '40g (sp40gen))
      ;;(run-one 51 (sp51) temp:|SearchRecipesResponse|)
      ;; (run-one 52 (sp52 id))
      ;;(run-one 61 (gs)  gg::|doGoogleSearchResponse|)
      ;;(run-one 62 (gsp) gg::|doSpellingSuggestionResponse|)
      ;;(run-one 63 (gcp) gg::|doGetCachedPageResponse|)

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
(defvar *soap-test-verbose* nil)

(defun st-fmt (&rest args) (when *soap-test-verbose* (apply 'format t args)))

(defun test-validator1 (&key index (port 8080)
			     (host "localhost")
			     (path *validator1-path*)
			     (reps 10)
			     (stop t)
			     debug log
			     (verbose nil)
			     &aux fail
			     (*soap-test-verbose* verbose)
			     (oldlog net.aserve::*enable-logging*))
  (unwind-protect
      (let ()
	(setf net.aserve::*enable-logging* log)
	(and *server* stop 
	     (progn (net.xmp:xmp-stop-server *server*)
		    (setf *server* nil)))
	(or *server*
	    (setf *server* (make-validator1-server :port port
						   :debug (ts-debug :server debug))))
	(macrolet ((run-one (body)
			    `(when (or (null index) (eql index this))
			       (dotimes (n reps)
				 (multiple-value-bind (v e)
				     (ignore-errors (values ,body))
				   (cond (e (push (cons this e) fail))
					 (v)
					 (t (push this fail))))))))

	  (let ((client (soap-message-client :url (format nil "http://~A:~A~A"
							  host port path)
					     :lisp-package :keyword
					     :decode-flag
					     nil ;;; ignore undefined elts and types
					     :soap-debug (ts-debug :client debug)
					     ))
		(this 0))
	    (incf this)
	    (run-one (stv1-run1 client this n))
	    (incf this)
	    (run-one (stv1-run2 client this n))
	    (incf this)
	    (run-one (stv1-run3 client this n))
	    (incf this)
	    (run-one (stv1-run4 client this n))
	    (incf this)
	    (run-one (stv1-run5 client this n))
	    (incf this)
	    (run-one (stv1-run6 client this n))
	    (incf this)
	    (run-one (stv1-run7 client this n))
	    (incf this)
	    (run-one (stv1-run8 client this n))
	    (incf this)
	    (run-one (stv1-run9 client this n))
		
	    (when stop (net.xmp:xmp-stop-server *server*))

	    (if fail
		(values nil fail)
	      :all-ok))))
    (setf net.aserve::*enable-logging* oldlog)))
      

(defun stv1-run1 (client this n)
  (declare (ignore this n))
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
			    (elt "abcdefghijklmnopqrstuvw"
				 (random 23))))
		      (elt "abcdefghijklmnopqrstuvw" 
			   (random 23)))))))
	 (rep (call-soap-method
	       client "countTheEntities" :|s| data))
	 (res (soap-sub-element-content (cdr rep) :|struct1|))
	 (ok t))
    (or (test lb (soap-sub-element-content
		  res :|ctLeftAngleBrackets|))
	(setq ok nil))
    (or (test rb (soap-sub-element-content
		  res :|ctRightAngleBrackets|))
	(setq ok nil))
    (or (test am (soap-sub-element-content
		  res :|ctAmpersands|))
	(setq ok nil))
    (or (test ap (soap-sub-element-content
		  res :|ctApostrophes|))
	(setq ok nil))
    (or (test qt (soap-sub-element-content res :|ctQuotes|))
	(setq ok nil))
    ok))

(defun stv1-run2 (client this n)
  (declare (ignore this n))
  ;; easyStructTest
  (let* ((m (random 100))
	 (l (random 100))
	 (c (random 100))
	 (rep (call-soap-method
	       client "easyStructTest"
	       :|stooges|
	       (list :|moe| m :|larry| l :|curly| c))))
    (test (+ m l c) (soap-sub-element-content (cdr rep) :|number|))))

(defun stv1-run3 (client this n)
  (declare (ignore this n))
  ;; echoStructTest
  (let* ((out
	  (let (new)
	    (dotimes (j (+ 2 (random 5)))
	      (setf new
		    (append
		     new
		     (list
		      (intern
		       (format nil "substruct~A" j) :keyword)
		      (list :|moe|   (format nil "~A" (random 10))
			    :|larry| (format nil "~A" (random 10))
			    :|curly| (format nil "~A" (random 10))
			    )))))
	    new))
	 (rep (call-soap-method
	       client "echoStructTest" :|myStruct1| out))
	 (res (soap-sub-element-content (cdr rep) :|myStruct1|))
	 )
    (test out (soap-alist-to-plist res t) :test 'equal)))

(defun stv1-run4 (client this n)
  (declare (ignore this n))
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
	 (ok t))
    (or (test t (when (arrayp res) t))
	(setq ok nil))
    (or (test '(6) (array-dimensions res) :test 'equal)
	(setq ok nil))
    (or (test out (concatenate 'list res) :test 'equal)
	(setq ok nil))
    ok))

(defun stv1-run5 (client this n)
  (declare (ignore this n))
  ;; moderateSizeArrayCheck
  (let* (out rep res)
    (dotimes (i (+ 5 (random 100)))
      (push (random-string 1 10) out))
    (setf rep (call-soap-method client "moderateSizeArrayCheck"
				:|myArray| out))
    (setf res (soap-sub-element-content (cdr rep) :|result2|))
    (test (concatenate 'string (first out) (first (last out))) res :test 'equal)))

(defun stv1-run6 (client this n)
  (declare (ignore this n))
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
	(test (+ m l c) res))
      )))

(defun stv1-run7 (client this n)
  (declare (ignore this n))
  (let* ((num (random 1000))
	 (rep (call-soap-method client "simpleStructReturnTest"
				:|myNumber| num)))
    (test (list  :|simpleStructReturnTestResult|
		 (list :|struct2|
		       (list :|times10| (* num 10))
		       (list :|times100| (* num 100))
		       (list :|times1000| (* num 1000))
		       ))
	  rep 
	  :test 'equal)))

(defun stv1-run8 (client this n &aux (rep (call-soap-method client "whichToolkit")))
  (declare (ignore this n))
  (test '(:|whichToolkitResult|
	   (:|struct3|
	     (:|toolkitDocsUrl| "http://franz.com")
	     (:|toolkitName| "Allegro Common Lisp SOAP")
	     (:|toolkitVersion| "x.y")
	     (:|toolkitOperatingSystem| "Windows and Unix")))
	rep :test 'equal))

(defun stv1-run9 (client this n)
  (declare (ignore this n))
  (let* ((num (random 100))
	 (bool (eql 1 (random 2)))
	 (st (random-string 5 15))
	 (db (random 10.1d0))
	 (dt (format nil "~A" (random 100000)))
	 (bin (random-string 50 100))
	 (out (list num bool st db dt bin
		    'xsd:|token|))
	 (rep (call-soap-method client "manyTypesTest2"
				:|num1| num
				:|bool| bool
				:|state| st
				:|doub| db
				:|dat| dt
				:|bin| bin
				:|qname| 'xsd:|token|
				))
	 (res (soap-sub-element-content (cdr rep) :|Result1|))
	 (ok t))
    (or (test t (when (arrayp res) t))
	(setq ok nil))
    (or (test '(9) (array-dimensions res) :test 'equal)
	(setq ok nil))
    (or (test out (subseq (concatenate 'list res) 0 7) :test 'equal)
	(setq ok nil))
    (setf rep (call-soap-method client "manyTypesTest2"
				:|num2| num
				:|bool| bool
				:|state| st
				:|doub| db
				:|dat| dt
				:|bin| bin
				:|qname| 'xsd:|token|
				))
    (setq res (soap-sub-element-content (cdr rep) :|Result1|))
    (or (test t (when (arrayp res) t))
	(setq ok nil))
    (or (test '(9) (array-dimensions res) :test 'equal)
	(setq ok nil))
    (or (test out (subseq (concatenate 'list res) 0 7) :test 'equal)
	(setq ok nil))
    ok))



;;; xmethods.com

(eval-when (compile load eval)

  (defpackage :user
    #+(version>= 7) (:import-from :sys #:with-timeout)
    #-(version>= 7) (:import-from :mp #:with-timeout)
    )
  )

#-(version>= 7) (mp:start-scheduler)   ;; to enable with-timeout
 
(defpackage :tnsw (:use))
(defpackage :tnst (:use))

(defvar *xmeth-verbose* nil)


(defun xmeth-folder (&aux all)
  (or (and (probe-file "xmethods") (file-directory-p "xmethods"))
      (null (setf all (directory ".")))
      (let (xm (i 0))
	(dolist (a all) (when (eql 0 (search "xmeth-" (pathname-name a)))
			  (push i xm) (incf i)
			  (push (namestring a) xm)))
	(setf xm (reverse xm))
	(cond ((null xm)
	       (error "Cannot find any xmethods folders"))
	      (t (format t "~&~{; ~A: ~A~%~}" xm)
		 (cerror "pick a folder by index" "several xmethods folders")
		 (format t "~&~{; ~A: ~A~%~}" xm)
		 (format t "~&;Enter index to above list: ")
		 (tpl:do-command :cd (elt xm (1+ (* 2 (read))))))))))

(defun xmeth-def (&key syntax (source "xmeth.wsdl"))
  
  (define-namespace-map :xmeth-base
    nil
    '(:tnsw
      "tnsw"
      "http://www.xmethods.net/interfaces/query.wsdl")
    '(:tnst
      "tnst"
      "http://www.xmethods.net/interfaces/query.xsd")
    `(:tnsx
      "tnsx"
      "http://www.xmethods.net/interfaces/query")
    :all
    '(:keyword nil :any))

  (define-namespace-map :xmeth-entry nil :all)

  (let (w)
    (etypecase source
      ((member :uri)
       (when (directory ".") (error "Must start in empty folder."))
       (multiple-value-bind (def rc)
	   (net.aserve.client:do-http-request
	    "http://www.xmethods.net/wsdl/query.wsdl")
	 (cond ((not (eql 200 rc)) (error "Return ~S from http request." rc))
	       ((not (stringp def)) (error "Odd return ~S from http request."
					   def)))
	 (with-open-file (s "xmeth.wsdl" :direction :output)
			 (write-line def s)))
       (make-directory "xmethods")
       (setf source "xmeth.wsdl"))
      ((or string pathname) (or (probe-file source) 
				(error "Not found: ~S" source))))
    (or (probe-file "xmeth.wsdl") (error "File xmeth.wsdl does not exist."))
    (cond ((null (probe-file "xmethods"))
	   (error "sub-directory xmethods not found."))
	  ((file-directory-p "xmethods"))
	  (t (error "xmethods is not a sub-directory.")))
    (setf w (decode-wsdl-file source
			      :namespaces :decode
			      :xml-syntax syntax 
			      :base       :xmeth-base))
    (make-client-interface w 0 "xmeth-client.cl"
			   :prefix :xmeth-
			   :suffix :message :verbose t :map :xmeth-ns)
    (load (compile-file-if-needed "xmeth-client.cl"))
    "xmeth-client.cl"))

(defun xmeth-client-call ()
  (funcall '|xmeth-getAllServiceNames|))
    

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



(defvar *soap-errors* nil)
(defvar *xml-errors* nil)
(defvar *ok* 0)
(defvar *bad* 0)
(defvar *bad-xml* 0)
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
	  (let (vals err xe)
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
		  (when (or (search "[in Sax parser]" e :test 'equalp)
			    (search "illegal namestring:" e :test 'equalp)
			    (search "XML file not found" e :test 'equalp)
			    (search "Reply is not XML" e :test 'equalp)
			    (search "Reply is neither WSDL nor XML" e :test 'equalp)
			    (search "URL returns nil:" e :test 'equalp)
			    )
		    (setf xe t))
		  (setf (second ferr) e)
		  (or *xmeth-verbose*
		      (when (< 80 (length e)) (setf e (subseq e 0 80))))
		  (format t    "~&~%ERROR ~A~%   ~A~%" i e)
		  (when (< 50 (length e)) (setf e (subseq e 0 50)))
		  (let* ((place (if xe
				    (assoc e *xml-errors* :test #'equal) 
				  (assoc e *soap-errors* :test #'equal))))
		    (if place
			(incf (second place))
		      (if xe
			  (push (setf place (list e 1)) *xml-errors*)
		      (push (setf place (list e 1)) *soap-errors*)))
		    (push i (cddr place))
		    )
		  ))
		(setf vals v)))
	    (cond (err (setf (cdr fres) :error)
		       (if xe (incf *bad-xml*) (incf *bad*)))
		  (t   (setf (cdr fres) :ok)
		       (incf *ok*)))
	    (format t "~&  End ~A ~A~%" i (or err "ok"))
	    vals)
	(when text (close text))
	))))


(defvar *xmeth-remote-external* nil)
(defmethod net.xml.sax:compute-external-address
  :around ((parser t) (system t) (public t) (current-filename t))
  (multiple-value-bind (v e w)
      (ignore-errors (let ((v (call-next-method))) v))
    (cond ((and (null e) v (ignore-errors (setf w (probe-file v)))) w)
	  ((atom *xmeth-remote-external*) (when e (error e)))
	  ((eq :remote (first *xmeth-remote-external*))
	   (xmeth-remote-external nil (or w system) (second *xmeth-remote-external*)
				  (third *xmeth-remote-external*)))
	  (t (xmeth-local-external nil (or w system) (second *xmeth-remote-external*)
				  (third *xmeth-remote-external*))))))
		  

(defun xmeth-remote-external (conn url root index)
  (declare (ignore conn))
  (let* ((string (xmeth-get-remote-file url))
	 (out (format nil "~A~A.dtd" root (prog1 (first index) (incf (first index)))))
	 )
    (when string
      (with-open-file (s out :direction :output :if-exists :supersede)
		      (write-line string s))
      (probe-file out))))

(defun xmeth-local-external (conn url root index)
  (declare (ignore conn url))
  (probe-file (format nil "~A~A.dtd" root (prog1 (first index) (incf (first index))))))

(defvar *wsdl-url* nil)

(defun xmeth-get-remote-file (url)
  (let* ((string (ignore-errors (net.aserve.client:do-http-request url)))
	 p)
    (when (and (null string)
	       *wsdl-url*
	       (setf p (position #\/ *wsdl-url* :from-end t))
	       (not (eql (1+ p) (length *wsdl-url*))))
      (setf string (ignore-errors 
		     (net.aserve.client:do-http-request 
		      (concatenate 'string
				   (subseq *wsdl-url* 0 (1+ p))
				   url)))))
    (ignore-errors (xmeth-verify :string string :top "schema" :write nil))))

(defun xmeth-remote-insert (conn url root index)
  (declare (ignore conn))
  (let* ((string (xmeth-get-remote-file url))
	 (out (format nil "~A~A.xml" root (prog1 (first index) (incf (first index))))))
    (when string
      (with-open-file (s out :direction :output :if-exists :supersede)
		      (write-line string s))
      string)))

(defun xmeth-local-insert (conn url root index)
  (declare (ignore conn url))
  (let* ((in (format nil "~A~A.xml" root (prog1 (first index) (incf (first index)))))
	 )
    (ignore-errors (xmeth-verify :string (file-contents in) :top "schema" :write nil))))

(defun xmeth-file-name (i) (read-from-string (format nil ":xmeth~3,'0D" i)))
(defun xmeth-file-path (folder name) (format nil "~A/~A.xml" folder name))

(defun xmeth-one-remote (&key (i 0) errorp syntax (folder "xmethods") (out "xmout"))
  ;; fetch WSDL from URL
  (let* ((name (xmeth-file-name i)))
    (xmeth-errorp
     errorp i (format nil "~A/~A.txt" folder name)
     #'(lambda (i)
	 (let* ((id (soap-result-part nil (elt *xmethods* i) :|id|))
		(res (funcall '|xmeth-getServiceDetail| :id id))
		(def (soap-result-only nil res :error "getServiceDetailResponse" nil))
		(wsdl-url (soap-result-part nil def "wsdlURL"))
		(*wsdl-url* wsdl-url)
		(string (ignore-errors (net.aserve.client:do-http-request wsdl-url)))
		(*xmeth-remote-external* (list :remote)))
	   (if string
	       (with-open-file (s (xmeth-file-path folder name)
				  :direction :output
				  :if-exists :supersede)
			       (write-line string s))
	     (error "URL returns nil: ~A"  wsdl-url))
	   (xmeth-verify :string string :folder folder :name name :syntax syntax)
	   (xmeth-body i folder out name syntax 'xmeth-remote-insert)
	   )))))


(defun xmeth-verify (&key syntax string folder name
			  (top "definitions") (write t)
			  &aux (file (xmeth-file-path folder name)) nb)
  (when (null string)
    (or (probe-file file)
	(error "XML file not found: ~A" file))
    (setf string (file-contents (xmeth-file-path folder name))))
  (or (and (< 20 (length string))
	   (string-equal
	    "<?xml " string
	    :start2 (setf nb (position #\space string :test-not #'eql))
	    :end2 (+ nb 6))
	   string)
      (and (eq syntax :strict) (error "Reply is not XML"))
      (when (and nb
		 (and (eql nb (search "<" string))
		      (search top string
			      :start2 nb :end2 (+ nb 20))))
	(when write
	  (with-open-file (s (xmeth-file-path folder name)
			     :direction :output
			     :if-exists :supersede)
			  (write-line "<?xml version=\"1.0\" ?>" s)
			  (write-line string s)))
	string)
      (error "Reply is neither WSDL nor XML")))

(defun xmeth-one-local (&key (i 0) errorp (folder "xmethods") (out "xmout") syntax)
  ;; assume WSDL is in local file
  (let* ((name (xmeth-file-name i))
	 (*xmeth-remote-external* (list :local)))
    (xmeth-errorp
     errorp i (format nil "~A/~A-2.txt" folder name)
     #'(lambda (i)
	 (xmeth-verify :syntax syntax :folder folder :name name)
	 (xmeth-body i folder out name syntax 'xmeth-local-insert)))))

(defun xmeth-body (i folder out name syntax insert)
  (let* (vals
	 (iplace (list 0))
	 (root (format nil "~A/~A" folder name))
	 (path (format nil "~A.xml" root)))
    (or (probe-file path) (error "XML file not found."))
    (setf (cdr *xmeth-remote-external*) (list root (list 0)))
    (setf vals (multiple-value-list
		(decode-wsdl-file path
				  :namespaces :decode
				  :xml-syntax syntax
				  :base
				  (list nil 
					:xmeth-entry
					(list (format nil "tns~A-" i) nil :prefix))
				  :include (list insert root iplace)
				  :import  (list insert root iplace)
				  )))
	   
    (xmeth-services vals i out name)
    ))

(defun xmeth-one-decode (&optional (i 0) (folder "xmethods"))
  (let* ((name (read-from-string (format nil ":xmeth~3,'0D" i))))
    (decode-wsdl-file (xmeth-file-path folder name)
		      :namespaces :decode
		      :base
		      (list nil 
			    :xmeth-entry
			    (list (format nil "tns~A-" i) nil :prefix))
		      )))

(defun xmeth-one-ns (&optional (i 0) (map :xmeth-base) (folder "xmethods"))
  (let* ((name (read-from-string (format nil ":xmeth~3,'0D" i))))
    (decode-wsdl-namespaces :file (xmeth-file-path folder name)
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
  (cond ((null (probe-file folder)) (make-directory folder))
	((not (file-directory-p folder))
	 (error "Output must go to folder: ~S?" folder)))
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
  (setf *soap-errors* (sort *soap-errors* #'< :key #'second))
  (setf *xml-errors* (sort *xml-errors* #'< :key #'second))
  (format stream "~%XML Errors:~%~{ ~S~%~}"  *xml-errors*)
  (format stream "~%SOAP Errors:~%~{ ~S~%~}"  *soap-errors*)
  (format stream "~%OK: ~A   SOAP errors: ~A   XML errors: ~A   Total: ~A~%"
	  *ok*
	  *bad*
	  *bad-xml*
	  (+ *ok* *bad* *bad-xml*))
  )

(defun xmeth-all-remote (&key (start 0) (out "xmout") syntax
			      (end (length *xmethods*) e-p))
  ;; fetch WSDL from URLs
  (xmeth-load)
  (or e-p (setf end (length *xmethods*)))
  (dribble "xmeth.log")
  (setf *soap-errors* nil *xml-errors* nil *ok* 0 *bad* 0 *bad-xml* 0) 
  (dotimes (i end) (or (< i start) (xmeth-one-remote :i i :out out :syntax syntax)))
  (xmeth-summary t)
  (xmeth-save)
  (dribble))

(defun xmeth-all-local (&key (start 0) (out "xmout") syntax
			     (end (length *xmethods*) e-p))
  ;; scan local WSDL files
  (xmeth-load)
  (or e-p (setf end (length *xmethods*)))
  (setf *soap-errors* nil *xml-errors* nil *ok* 0 *bad* 0 *bad-xml* 0)
  (dribble "xmeth.log")
  (dotimes (i end) (or (< i start) (xmeth-one-local :i i :out out :syntax syntax)))
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
	 (test res r :fail-info (list 'xmp-match-tests i :pattern pattern :data data))
	 (test alst a :test #'equal
	       :fail-info (list 'xmp-match-tests i :pattern pattern :data data))
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
	       ("CAPS"         "caps")
	       ))
    (let* ((in (first c))
	   (out (second c))
	   (r (net.xmp.soap::break-at-case-shifts in))
	   )
      (cond ((test out r :test #'equal :fail-info (list 'xmp-break-tests in))
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


	    
(defun sc01 (&optional syntax)
  (net.xmp::decode-schema
   :verbose t :syntax  syntax
   :string "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><xsd:schema targetNamespace=\"http://www.myweb-services.com\" xmlns:xsd='http://www.w3.org/2001/XMLSchema'
>
 <xsd:import namespace=\"http://schemas.xmlsoap.org/soap/encoding/\" />
 <xsd:import namespace=\"http://schemas.xmlsoap.org/wsdl/\" />
 <xsd:complexType name=\"SearchStruct\">
  <xsd:all>
   <xsd:element name=\"SearchEngineName\" type=\"xsd:string\"/>
   <xsd:element name=\"SearchTitle\" type=\"xsd:string\"/>
   <xsd:element name=\"SearchLink\" type=\"xsd:string\"/>
   <xsd:element name=\"SearchDescription\" type=\"xsd:string\"/>
  </xsd:all>
 </xsd:complexType>
 <xsd:complexType name=\"ArrayOfSearchStruct\">
  <xsd:complexContent>
   <xsd:restriction base=\"SOAP-ENC:Array\">
    <xsd:attribute ref=\"SOAP-ENC:arrayType\" wsdl:arrayType=\"tns:SearchStruct[]\"/>
   </xsd:restriction>
  </xsd:complexContent>
 </xsd:complexType>
</xsd:schema>

"))




;;;
;;; SOAP encoding tests

(defun ts-debug (mode debug)
  (case debug
    ((:client :server) (eq mode debug))
    (:stop (case mode (:client debug)))
    (otherwise debug)))

(defun ts-alog (log &key keep (set t) &aux oldlog)
  (setf oldlog net.aserve::*enable-logging*)
  (or keep (net.aserve:shutdown))
  (when set (setf net.aserve::*enable-logging* log))
  oldlog)

(defmacro with-alog ((&key log keep (new t)) &body body)
  `(let ((oldlog (ts-alog nil :keep t :set nil)))
     (unwind-protect
	 (let ()
	   (ts-alog ,log :keep (if ,new nil t) :set t)
	   ,@body)
       (ts-alog oldlog :keep ,keep :set t))))
  

(defun soap-test-server (&key (new t) (port 0) debug (path "/SOAP"))
  (when new (soap-new-environment))
  (let* ((server (soap-message-server :start `(:port ,port)
				      :publish `(:path ,path)
				      :lisp-package :keyword
				      :soap-debug debug
				      ))
	 (wserver (net.xmp::xmp-aserve-server server))
	 (socket  (net.aserve:wserver-socket wserver))
	 (url (format nil "http://localhost:~A~A" (socket:local-port socket) path)))
    (values url server)))

(defvar *soap000* nil)
(defun soap000 (&key (new t) debug nillable)
  (when new (soap-new-environment))
  (let* (server url)

    (multiple-value-setq (url server)
      (soap-test-server :new new :debug (ts-debug :server debug)))
    (when nillable (setf (net.xmp::xmp-nillable server) nillable))
    

    (setf *soap000* server)

    (define-soap-type nil :soap001
      '(:complex
	(:seq (:element
	       "parts"
	       (:complex
		(:seq
		 (:element "str" xsd:|string|)
		 (:element "qnm" xsd:|QName|) ;;; needs different test???
		 (:element "b64" xsd:|base64Binary|) ;;;needs different test???
		 (:element "dec" xsd:|decimal|)
		 (:element "long" xsd:|long|)
		 (:element "ulong" xsd:|unsignedLong|)
		 (:element "int"  xsd:|int|)
		 (:element "uint"  xsd:|unsignedInt|)
		 (:element "integer" xsd:|integer|)
		 (:element "nonpos" xsd:|nonPositiveInteger|)
		 (:element "nonneg"  xsd:|nonNegativeInteger|)
		 (:element "neg"   xsd:|negativeInteger|)
		 (:element "pos" xsd:|positiveInteger|)
		 (:element "short" xsd:|short|)
		 (:element "byte"  xsd:|byte|)
		 (:element "ushort"  xsd:|unsignedShort|)
		 (:element "ubyte" xsd:|unsignedByte|)
		 (:element "bool"  xsd:|boolean|)
		 (:element "float"  xsd:|float|)
		 (:element "dbl"  xsd:|double|)

		 (:element "float-string"  xsd:|float|)
		 (:element "dbl-string"  xsd:|double|)
		 (:element "bool-string"  xsd:|boolean|)
			  
		 ))))
	))
    (define-soap-type nil :soap001c
      '(:complex
	(:seq (:element
	       "parts"
	       (:complex
		(:seq
		 (:element "str" xsd:|string|)
		 (:element "qnm" xsd:|QName|) ;;; needs different test???
		 (:element "b64" xsd:|base64Binary|) ;;;needs different test???
		 (:element "dec" xsd:|decimal|)
		 (:element "long" xsd:|long|)
		 (:element "ulong" xsd:|unsignedLong|)
		 (:element "int"  xsd:|int|)
		 (:element "uint"  xsd:|unsignedInt|)
		 (:element "integer" xsd:|integer|)
		 (:element "nonpos" xsd:|nonPositiveInteger|)
		 (:element "nonneg"  xsd:|nonNegativeInteger|)
		 (:element "neg"   xsd:|negativeInteger|)
		 (:element "pos" xsd:|positiveInteger|)
		 (:element "short" xsd:|short|)
		 (:element "byte"  xsd:|byte|)
		 (:element "ushort"  xsd:|unsignedShort|)
		 (:element "ubyte" xsd:|unsignedByte|)
		 (:element "bool"  xsd:|boolean|)
		 (:element "float"  xsd:|float|)
		 (:element "dbl"  xsd:|double|)

		 (:element "float-string"  xsd:|string|)
		 (:element "dbl-string"  xsd:|string|)
		 (:element "bool-string" xsd:|string|)
			  
		 ))))
	))

    (define-soap-element nil "msg000" 
      '(:complex (:seq (:element "parts" (:complex (:seq* (:any)))))))
    (define-soap-element nil "res000" 
      '(:complex (:seq (:element "return" xsd:|string|))))
    (define-soap-element nil "msg001" :soap001)

    (soap-export-method server "msg000" '("parts")
			:return "res000"  :action nil
			:lisp-name 'soap000-msg000)
    (soap-export-method server "msg001" '("parts")
			:return "res000"  :action nil
			:lisp-name 'soap000-msg000)
    (soap-export-method server "msg002" '("parts")
			:return "res000"  :action nil
			:lisp-name 'soap002-msg002)
    (soap-export-method server "soap003" '("str1" "str2" "str3" "str4")
			:return "res000"  :action nil
			:lisp-name '(soap003-msg003 :s1 :s2 :s3 :s4))
    (soap-export-method server "soap003n" '("str1" "str2" "str3" "str4")
			:return "res000"  :action nil
			:lisp-name '(soap003-msg003 :s1 :s2 :s3 :s4))

    (values url server)))

(defun soap000-msg000 (&rest args)
  (let* ((parts (member "parts" args :test #'string-equal)))
    (list "return"
	  (cond ((null parts)
		 (if args
		     (format nil ":other - ~A" args)
		   ":none "))
		((not (eq parts args))
		 (format nil ":prefix - ~A" args))
		((cddr parts)
		 (format nil ":suffix - ~A" args))
		((and (consp (second parts)) (null (cdr (second parts)))
		      (consp (first (second parts))))
		 (let* ((arg (first (second parts)))
			(elt (first arg)) (val (second arg)))
		   (format nil ":parts ~A ~A ~A " elt val (type-of val))))
		(t (format nil ":odd - ~S" (second parts)))))))

(defun ts1001 (&key debug keep i log)

  ;; Test simple data type encoding

  (let (conn url (ok t) (key 'ts1001))
    (with-alog
     (:log log :keep keep)
     (setf url (soap000 :debug debug))
     (setf conn (soap-message-client :url url
				     :send-type nil
				     :soap-debug (ts-debug :client debug)
				     ))
     (let* ((msg 
	     ;; Encode the element with the type specs in :soap001c
	     ;;  where xxx-string components are sent from hand-coded
	     ;;  string data but decoded as xsd types.
	     (list :element "msg001" :soap001c))
	    expected res (j 0))
       (dolist (parts
		`(
		  ;; (sub-elt content)
		  ;; ((sub-elt content) e1
		  ;;           e1 is expected result
		  ;;           :call-error -- expect an error

		  ;;0
		  ("str" "only a string")

		  ;;1
		  (("str" "")  ":parts str  (simple-array character (0)) ") ;;; empty string

		  ;;2
		  (("str" nil) ":parts str  (simple-array character (0)) ")

		  ;;3
		  (("qnm" xsd:|string|)
		   ":parts qnm string symbol ")

		  ;;4
		  ("dec" 12345)

		  ;;5
		  (("dec" nil) ":parts dec 0 fixnum ")

		  ;;6
		  (("dec" :foo) :call-error)
		  
		  ;;7
		  ("long" 12345678)

		  ;;8
		  (("long" nil) ":parts long 0 fixnum ")

		  ;;9
		  ("ulong" 12345678)

		  ;;10
		  (("ulong" nil) ":parts ulong 0 fixnum ")

		  ;;11
		  (("ulong" -5) :call-error :none fixnum)

		  ;;12
		  ("int" 234)

		  ;;13
		  (("int" nil) ":parts int 0 fixnum ")

		  ;;14
		  ("uint" 234)

		  ;;15
		  (("uint" nil) ":parts uint 0 fixnum ")

		  ;;16
		  ("integer" 234)

		  ;;17
		  (("integer" nil) ":parts integer 0 fixnum ")

		  ;;18
		  ("nonpos" -456)

		  ;;19
		  ("nonpos" 0)

		  ;;20
		  (("nonpos" nil) ":parts nonpos 0 fixnum ")

		  ;;21
		  ("nonneg" 776)

		  ;;22
		  ("nonneg" 0)

		  ;;23
		  (("nonneg" nil) ":parts nonneg 0 fixnum ")

		  ;;24
		  ("neg" -888)

		  ;;25
		  ("pos" 345)

		  ;;26
		  (("pos" nil) ":parts pos 0 fixnum ")

		  ;;27
		  ("short" 1234)

		  ;;28
		  ("short" -1234)

		  ;;29
		  ("byte" 127)

		  ;;30
		  ("byte" -127)

		  ;;31
		  (("byte" 1278) :call-error)

		  ;;32
		  (("byte" 128) :call-error)

		  ;;33
		  (("byte" -129) :call-error)

		  ;;34
		  ("ushort" 1234)

		  ;;35
		  (("ushort" -8) :call-error)

		  ;;36
		  ("ubyte" 127)

		  ;;37
		  (("ubyte" -9) :call-error)

		  ;;38
		  (("ubyte" 256) :call-error)

		  ;;39
		  (("bool" 1)   ":parts bool t symbol ")

		  ;;40
		  (("bool" t)   ":parts bool t symbol ")

		  ;;41
		  (("bool" 0)   ":parts bool t symbol ")

		  ;;42
		  (("bool" nil) ":parts bool nil null ")

		  ;;43
		  (("float-string" "76.5152e3")
		   ":parts float-string 76515.2d0 double-float ")

		  ;;44
		  (("float" 123.456) ":parts float 123.456d0 double-float ")

		  ;;45
		  (("float" 23.456e17) ":parts float 2.3456d+18 double-float ")

		  ;;46
		  (("float" 321.456e27) ":parts float 3.21456d+29 double-float ")

		  ;;47
		  (("float" 234.456e-3) ":parts float 0.234456d0 double-float ")

		  ;;48
		  (("float" -123.456E-3) ":parts float -0.123456d0 double-float ")

		  ;;49
		  (("dbl" 123.456) ":parts dbl 123.45600128173828d0 double-float ")

		  ;;50
		  (("dbl" 23.456d11)   ":parts dbl 2.3456d+12 double-float ")

		  ;;51
		  (("dbl" 1235.456e5)  ":parts dbl 1.235456d+8 double-float ")

		  ;;52
		  (("dbl" 223.456D-4)  ":parts dbl 0.0223456d0 double-float ")
		 
		  ;;53
		  (("float-string" "12345678.123456e0")
		   ":parts float-string 1.2345678123456d+7 double-float ")

		  ;;54
		  (("dbl-string" "77.123e0") ":parts dbl-string 77.123d0 double-float ")

		  ;;55   bug13943
		  (("float" 0.0) ":parts float 0.0d0 double-float ")

		  ;;56   bug21405
		  (("dbl" 0.0) ":parts dbl 0.0d0 double-float ")

		  ;;57   bug17461
		  (("bool-string" "   0   ") ":parts bool-string nil null ")

		  ;;58  bug17461
		  (("bool-string" "   true   ") ":parts bool-string t symbol ")

		  ;;("dec" 123.45)
		  ;;("dec" 123/45)
		
		  ))
	 (when (or (null i)
		   (eql i j)
		   (and (consp i) (member j i)))
	   (when debug
	     (format t "~&~%;BEGIN ~S ~%" parts))
	   (cond ((consp (first parts))
		  (setf expected (second parts))
		  (setf parts (first parts)))
		 (t (setf expected nil)))
	   (or expected 
	       (setf expected (format nil ":parts ~A ~A ~A " 
				      (first parts) (second parts) (type-of (second parts))
				      )))
	   (setq res :unset)
	   (or
	    (case expected
	      (:call-error
	       (test-error
		(call-soap-method conn msg "parts" parts)
		:fail-info (list* key j :default parts)))
	      (otherwise
	       (test-no-error
		(setf res (soap-result-part 
			   conn (call-soap-method conn msg "parts" parts)
			   "res000" "return"))
		:fail-info (list* key j :default parts))))
	    (setf ok nil))
	   (or
	    (eq expected :call-error)
	    (eq res :unset)
	    ;; Test with equalp to allow cross-case-compiled test file.
	    (test expected res :test #'equalp  
		  :fail-info (list* key j :default parts))
	    (setf ok nil))
	   
	   )
	
	 (incf j)
	 )
       ))
    ok))


(defun soap002-msg002 (&rest args)
  (let* ((conn *soap-server*)
	 (body (soap-message-body conn))
	 (msg (soap-result-pair conn body "msg002"))
	 (mattr (soap-get-attributes conn msg))
	 (parts (soap-result-pair conn msg nil "parts"))
	 (pattr (soap-get-attributes conn parts))
	 (sub1  (soap-result-pair conn parts nil "sub1"))
	 (attr1 (soap-get-attributes conn sub1))
	 (sub2  (soap-result-pair conn parts nil "sub2"))
	 (attr2 (soap-get-attributes conn sub2))
	 )
    (list "return" 
	  (let ((*print-pretty* nil))
	    (format nil "msg:~A  parts:~A  sub1:~A  sub2:~A"
		    mattr pattr attr1 attr2)))
  ))



(defun ts1002 (&key debug keep i log)

  ;; Test attribute encoding

  (let* (url conn (ok t) (j 0) (key 'ts1002)
	     (attr1 (list "attr1" "on-element-def"))
	     (attr2 (list "attr2" "on-type-2"))
	     (attr3 (list "attr3" "on-type-3"))
	     (attr4 (list "attr4" "on-parts-type"))
	     (attr5 (list "attr5" "on-sub1"))
	     (attr6 (list "attr6" "on-sub2"))
	     (attr7 (list "attr7" "on-parts-elt"))
	     )
    (with-alog
     (:log log :keep keep)
     (setf url (soap000 :debug debug))
     (define-soap-type nil :soap002a
       `(:complex (:seq (:element
			 "parts"
			 (:complex
			  (:seq
			   (:element "sub1" xsd:|string| :attributes ,attr5)
			   (:element "sub2" xsd:|string| :attributes ,attr6)
			   )
			  :attributes ,attr4)
			 :attributes ,attr7))
		  :attributes ,attr3))
     (define-soap-type nil :soap002 :soap002a :attributes attr2)
			  
     (define-soap-element nil "msg002" :soap002 :attributes attr1)
     (setf conn (soap-message-client
		 :url url
		 :send-type nil
		 :soap-debug (ts-debug :client debug)
		 ))
	  
     (flet ((pp (str &rest args)
		(let ((*print-pretty* nil))
		  (apply #'format nil str args)))
	    (skipij () (incf j) (and i (not (eql i j))))
	    )

       (or (skipij)
	   (test
	    (pp "msg:~A  parts:~A  sub1:~A  sub2:~A" attr1 attr7 attr5 attr6)
	    (soap-result-string
	     conn
	     (call-soap-method
	      conn "msg002" "parts" (list "sub1" "aaa" "sub2" "bbb"))
	     nil nil)
	    :test #'equal :fail-info (list key j))
	   (setf ok nil))

       (or (skipij)
	   (test
	    (pp "msg:~A  parts:~A  sub1:~A  sub2:~A" attr2 attr7 attr5 attr6)
	    (soap-result-string
	     conn
	     (call-soap-method conn '(:element "msg002" :soap002)
			       "parts" (list "sub1" "aaa" "sub2" "bbb"))
	     nil nil)
	    :test #'equal :fail-info (list key j))
	   (setf ok nil))

       (or (skipij)
	   (test
	    (pp "msg:~A  parts:~A  sub1:~A  sub2:~A" attr3 attr7 attr5 attr6)
	    (soap-result-string
	     conn
	     (call-soap-method conn '(:element "msg002" :soap002a)
			       "parts" (list "sub1" "aaa" "sub2" "bbb"))
	     nil nil)
	    :test #'equal :fail-info (list key j))
	   (setf ok nil))
       ))

    ok))



(defun soap003-msg003 (&rest keys &key s1 s2 s3 s4)
  (list "return"
	(format nil "~{~A ~A ~}"
		(append
		 (when (member :s1 keys) (list :|s1| s1))
		 (when (member :s2 keys) (list :|s2| s2))
		 (when (member :s3 keys) (list :|s3| s3))
		 (when (member :s4 keys)
		   (typecase s4
		     (integer (list :|i4| s4))
		     (otherwise (list :|s4| s4))))))))


(defun ts1003 (&key debug keep i log)

  ;; Test :null-element handling [spr31688] and
  ;;      [bug16269] keyword mapping in server

  (let (url (ok t) (key :ts1003) (j 0))
    (with-alog
     (:log log :keep keep)
     (dolist (nillable '(nil :accept :strict :ignore))
       (setf url (soap000 :nillable nillable :debug debug))
       (define-soap-type nil :|soap003t|
	 '(:complex
	   (:seq
	    (:element "str1" xsd:|string|)
	    (:element "str2" xsd:|string|)
	    (:element "str3" xsd:|string|)
	    (:element "str4" xsd:|int|)			  
	    )
	   ))
       (define-soap-type nil :|soap003tn|
	 '(:complex
	   (:seq
	    (:element "str1" xsd:|string| )
	    (:element "str2" xsd:|string| :nillable t)
	    (:element "str3" xsd:|string| :nillable t)
	    (:element "str4" xsd:|int|    :nillable t)			  
	    )
	   ))
       (define-soap-element nil :|soap003| :|soap003t|)
       (define-soap-element nil :|soap003n| :|soap003tn|)
       (dolist
	   (parts
	    `(
	      ;; (parts (nillable (null-element element expected)
	      ;;                  ...
	      ;;                  (null-element (element expected) ...)
	      ;;                  ...)
	      ;;         ...)	
	      (("a" "b" "c" 17)
	       (:all     (:all    (:|soap003|  "s1 a s2 b s3 c i4 17 ")
				  (:|soap003n| "s1 a s2 b s3 c i4 17 "))
			 )
	       )
	      (("a" "b" "c" nil)
	       (nil      (:default       :|soap003|  "s1 a s2 b s3 c i4 0 ")
			 (:default-value :|soap003|  "s1 a s2 b s3 c i4 0 ")
			 (:empty         :|soap003|  "s1 a s2 b s3 c i4 0 ")
			 (:nilled        :|soap003|  "s1 a s2 b s3 c i4 0 ")
			 (:none          :|soap003|  "s1 a s2 b s3 c ")
			 )
	       (:accept  (:nilled        :|soap003n| "s1 a s2 b s3 c ")
			 )
	       )
	      (("a" "b" nil 17)
	       (nil      (:default       :|soap003|  "s1 a s2 b s3  i4 17 ")
			 (:default-value :|soap003|  "s1 a s2 b s3  i4 17 ")
			 (:empty         :|soap003|  "s1 a s2 b s3  i4 17 ")
			 (:nilled        :|soap003|  "s1 a s2 b s3  i4 17 ")
			 (:none          :|soap003|  "s1 a s2 b i4 17 ")
			 )
	       (:accept  (:nilled        :|soap003n| "s1 a s2 b i4 17 ")
			 )
	       )
	      (("a" nil "c" 17)
	       (nil      (:default       :|soap003|  "s1 a s2  s3 c i4 17 ")
			 (:default-value :|soap003|  "s1 a s2  s3 c i4 17 ")
			 (:empty         :|soap003|  "s1 a s2  s3 c i4 17 ")
			 (:none          :|soap003|  "s1 a s3 c i4 17 ")
			 )
	       )
	      ((nil "b" "c" 17)
	       (nil      (:default       :|soap003|  "s1  s2 b s3 c i4 17 ")
			 (:default-value :|soap003|  "s1  s2 b s3 c i4 17 ")
			 (:empty         :|soap003|  "s1  s2 b s3 c i4 17 ")
			 (:nilled        :|soap003|  "s1  s2 b s3 c i4 17 ")
			 (:none          :|soap003|  "s2 b s3 c i4 17 ")
			 )
	       (:accept  (:nilled        :|soap003n| "s2 b s3 c i4 17 ")
			 )
	       (:strict  (:nilled        :|soap003n| "s1  s2 b s3 c i4 17 ")
			 )
	       )
	      (("a" "b" "c" :none)
	       (:all     (:all        :|soap003| "s1 a s2 b s3 c ")
			 (:all        :|soap003n| "s1 a s2 b s3 c ")
			 )
	       )
	      (("a" "b" :none 17)
	       (:all      (:all       :|soap003| "s1 a s2 b i4 17 ")
			  (:all       :|soap003n| "s1 a s2 b i4 17 ")
			  )
	       )
	      (("a" :none "c" 17)
	       (:all      (:all       :|soap003| "s1 a s3 c i4 17 ")
			  (:all       :|soap003n| "s1 a s3 c i4 17 ")
			  )
	       )
	      ((:none "b" "c" 17)
	       (:all      (:all      :|soap003| "s2 b s3 c i4 17 ")
			  (:all      :|soap003n| "s2 b s3 c i4 17 ")
			  )
	       )

	      ))

	 (dolist (null-elt '(:default :default-value :empty :none :nilled
				      :nilled-or-default :nilled-or-empty
				      :nilled-or-none))
	      
	   (let* (
		  (strs (first parts))
		  (s1 (first strs))
		  (s2 (second strs))
		  (s3 (third strs))
		  (s4 (fourth strs))
		  (nl-entry (or (assoc :all (cdr parts))
				(assoc nillable (cdr parts))))
		  (ne-entry (or (assoc :all (cdr nl-entry))
				(assoc null-elt (cdr nl-entry))))
		  (e-list (if (consp (second ne-entry))
			      (cdr ne-entry)
			    (when ne-entry (list (cdr ne-entry))))))
	     (dolist (entry e-list)
	       (when (or (null i)
			 (eql i j)
			 (and (consp i) (member j i)))
		 (let* ((element (first entry))
			(expected (second entry))
			conn)
		   (setf conn (apply 'soap-message-client :url url
				     :send-type nil
				     :soap-debug (ts-debug :client debug)
				     (case null-elt
				       (:default nil)
				       (otherwise (list :null-element null-elt)))
				     ))
		   (flet ((send (key s) (case s
					  (:none nil)
					  (otherwise (list key s)))))
		     (flet ((docall
			     ()
			     (apply 'call-soap-method conn
				    element
				    (append
				     (send "str1" s1)
				     (send "str2" s2)
				     (send "str3" s3)
				     (send "str4" s4))))
			    )
		       (etypecase expected
			 (string
			  (or
			   (test expected
				 (soap-result-string conn (docall) nil "return")
				 :test #'equal
				 :fail-info (list key j nillable null-elt element strs))
			   (setf ok nil)))
			 ((member :error)
			  (or (test-error
			       (docall)
			       :announce t
			       :fail-info (list key j nillable null-elt element strs))
			      (setf ok nil)))
			 )))
		    
		   ))
	       (incf j)
	       )))
	 )))
    ok))

(defun ts2001-server (&key debug)
  (multiple-value-bind (url server)
      (soap-test-server :debug debug)
    (define-soap-element nil "res000" 
      '(:complex (:seq (:element "return" xsd:|string|))))
    (soap-export-method server "soap2001body" '("body") :return "res000" 
			:action nil :lisp-name '(ts2001-body :body))
    (soap-export-method server "soap2001msg" () :return "res000" 
			:action nil :lisp-name 'ts2001-msg)
    url))

(defun ts2001 (&key debug keep log)
  
  ;; Test SOAP headers   bug16470

  (with-alog
   (:keep keep :log log)
   (let* ((url (ts2001-server :debug debug))
	  (client (soap-message-client :url url :soap-debug debug))
	  (message "message")
	  (h1elt "head1")
	  (h1cont "content1")
	  (h2elt "head2")
	  (h2cont "content2")
	  (h1 (make-soap-header client `(:element ,h1elt (:simple xsd:|string|))
				h1cont))
	  (h2 (make-soap-header client `(:element ,h2elt (:simple xsd:|string|))
				h2cont))
	  )

     ;; Test rfe12678 soap-sent-string  soap-received-string
     (call-soap-method client '(:element "soap2001body"
						(:complex
						 (:seq (:element "body" xsd:|string|))))
			      "body" 12345)
     (test t (< 25 (search ">12345</body></soap2001body>" (soap-sent-string client))))
     (test t (< 25 (search ">12345</return></res000>" (soap-received-string client))))

     (soap-add-header client h1)
     (soap-add-header client h2)
     (test nil
	   (null
	    (member
	     (soap-result-part
	      client
	      (call-soap-method client
				'(:element "soap2001msg" (:simple xsd:|string|))
				message)
	      nil nil)
	     (list (format nil "~A ~A ~A" 
			   message (list h1elt h1cont) (list h2elt h2cont))
		   (format nil "~A ~A ~A" 
			   message (list h2elt h2cont) (list h1elt h1cont)))
	     :test #'equal))
	   )
		     
     (test message
	   (soap-result-part
	    client
	    (call-soap-method client '(:element "soap2001body"
						(:complex
						 (:seq (:element "body" xsd:|string|))))
			      "body" message)
	    nil nil)
	   :test #'equal)
     )))

(defun ts2001-msg ()
  (list "return" (format nil "~A ~A ~A"
			 (soap-result-part
			  *soap-server*
			  (soap-message-body *soap-server*)
			  nil)
			 (soap-result-pair
			  *soap-server*
			  (soap-message-headers *soap-server*)
			  :header 0)
			 (soap-result-pair
			  *soap-server*
			  (soap-message-headers *soap-server*)
			  :header 1)
			 )))

(defun ts2001-body (&key body)
  (list "return" body))


(defun ts2010 (&aux *xmp-report-error-action*)
  ;; Test include/import options
		    
  (setq *xmp-report-error-action* :ignore)
  (test-no-error
   (decode-wsdl-string 
    "<wsdl:definitions    
        xmlns:wsdl='http://schemas.xmlsoap.org/wsdl/'
        xmlns:xsd='http://www.w3.org/2001/XMLSchema/'
        >
      <xsd:include schemaLocation='foo.wsdl' />
    </wsdl:definitions>"
    :namespaces :wsdl1.1
    :include 'no-such-function
    ))

  (setq *xmp-report-error-action* :warn)
  (test-warning
   (decode-wsdl-string 
    "<wsdl:definitions    
        xmlns:wsdl='http://schemas.xmlsoap.org/wsdl/'
        xmlns:xsd='http://www.w3.org/2001/XMLSchema/'
        >
      <xsd:include schemaLocation='foo.wsdl' />
    </wsdl:definitions>"
    :namespaces :wsdl1.1
    :include 'no-such-function
    ))

  (setq *xmp-report-error-action* :error)
  (test-error
   (decode-wsdl-string 
    "<wsdl:definitions    
        xmlns:wsdl='http://schemas.xmlsoap.org/wsdl/'
        xmlns:xsd='http://www.w3.org/2001/XMLSchema/'
        >
      <xsd:include schemaLocation='foo.wsdl' />
    </wsdl:definitions>"
    :namespaces :wsdl1.1
    :include 'no-such-function
    )
   :include-subtypes t
   :announce t)
  (test-error
   (decode-wsdl-string 
    "<wsdl:definitions    
        xmlns:wsdl='http://schemas.xmlsoap.org/wsdl/'
        xmlns:xsd='http://www.w3.org/2001/XMLSchema/'
        >
      <xsd:include schemaLocation='foo.wsdl' />
    </wsdl:definitions>"
    :namespaces :wsdl1.1
    :include 'wsdl-include-file
    )
   :include-subtypes t
   :announce t)
  (test-error
   (decode-wsdl-string 
    "<wsdl:definitions    
        xmlns:wsdl='http://schemas.xmlsoap.org/wsdl/'
        xmlns:xsd='http://www.w3.org/2001/XMLSchema/'
        >
      <xsd:include schemaLocation='http://localhost:1234' />
    </wsdl:definitions>"
    :namespaces :wsdl1.1
    :include 'wsdl-include-url
    )
   :include-subtypes t
   :announce t)
  (test-error
   (decode-wsdl-string 
    "<wsdl:definitions    
        xmlns:wsdl='http://schemas.xmlsoap.org/wsdl/'
        xmlns:xsd='http://www.w3.org/2001/XMLSchema/'
        >
      <xsd:include schemaLocation='http://localhost:1234' />
    </wsdl:definitions>"
    :namespaces :wsdl1.1
    :include '(wsdl-include-url :timeout 0.001)
    )
   :include-subtypes t
   :announce t)
  (test-error
   (decode-wsdl-string 
    "<wsdl:definitions    
        xmlns:wsdl='http://schemas.xmlsoap.org/wsdl/'
        xmlns:xsd='http://www.w3.org/2001/XMLSchema/'
        >
      <xsd:include schemaLocation='http://franz.com/nosuchpage.htm' />
    </wsdl:definitions>"
    :namespaces :wsdl1.1
    :include 'wsdl-include-url
    )
   :include-subtypes t
   :announce t)

  )

(defparameter 
  *ts2020-wsdl*
  "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
<definitions xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"
             xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
             xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
             xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\"
             xmlns:psaDataTypes=\"psaDataTypes\"
             xmlns=\"http://schemas.xmlsoap.org/wsdl/\"
             xmlns:tns=\"urn:repository\"
             targetNamespace=\"urn:repository\">

    <types>
        <xsd:schema targetNamespace=\"urn:repository\"
                    elementFormDefault=\"qualified\"
                    attributeFormDefault=\"unqualified\"
                >

      <xsd:complexType name=\"PSAModelType\">
        <xsd:annotation>
            <xsd:documentation>This data type is created based on PSA data schema</xsd:documentation>
        </xsd:annotation>
        <xsd:sequence>
            <xsd:element name=\"id\" type=\"xsd:integer\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>
            <xsd:element name=\"name\" type=\"xsd:string\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>
        </xsd:sequence>
    </xsd:complexType>

            <xsd:complexType name=\"resultStatusType\">
              <xsd:sequence>
                <xsd:element name=\"code\" type=\"xsd:int\" minOccurs=\"1\" default=\"0\"/>
                <xsd:element name=\"message\" type=\"xsd:string\" minOccurs=\"1\" default=\"OK\"/>
              </xsd:sequence>
            </xsd:complexType>

            <xsd:element name=\"InsertPSAModelData\" type=\"tns:PSAModelType\"/>
            <xsd:element name=\"InsertPSAModelResult\" type=\"tns:resultStatusType\"/>


            <xsd:element name=\"SelectPSAModelData\" type=\"xsd:string\"/>
            <xsd:element name=\"SelectPSAModelResult\" type=\"tns:SelectPSAModelResultType\"/>
            <xsd:complexType name=\"SelectPSAModelResultType\">
                <xsd:sequence>
                    <xsd:element name=\"resultStatus\" type=\"tns:resultStatusType\"/>
                    <xsd:element name=\"data\" type=\"tns:PSAModelType\"/>
                </xsd:sequence>
            </xsd:complexType>

        </xsd:schema>
    </types>

    <!-- Definition of input and output parameters for the operation -->
    <message name=\"InsertPSAModelRequest\">
        <part name=\"PSAModels\" element=\"tns:InsertPSAModelData\"/>
    </message>

    <message name=\"InsertPSAModelResponse\">
        <part name=\"InsertPSAModelOut\" element=\"tns:InsertPSAModelResult\"/>
    </message>

    <message name=\"SelectPSAModelRequest\">
        <part name=\"PSAModelId\" element=\"tns:SelectPSAModelData\" />
    </message>

    <message name=\"SelectPSAModelResponse\">
        <part name=\"SelectPSAModelOut\" element=\"tns:SelectPSAModelResult\" />
    </message>

    <portType name=\"ContentManager\">

        <operation name=\"InsertPSAModel\">
            <input message=\"tns:InsertPSAModelRequest\"/>
            <output message=\"tns:InsertPSAModelResponse\"/>
        </operation>

        <operation name=\"SelectPSAModel\">
            <input message=\"tns:SelectPSAModelRequest\"/>
            <output message=\"tns:SelectPSAModelResponse\"/>
        </operation>

    </portType>

    <binding name=\"ContentManagerBinding\" type=\"tns:ContentManager\">
        <soap:binding style=\"document\" transport=\"http://schemas.xmlsoap.org/soap/http\"/>

        <operation name=\"InsertPSAModel\">
            <input>
                <soap:body use=\"literal\"/>
            </input>
            <output>
                <soap:body use=\"literal\"/>
            </output>
        </operation>

        <operation name=\"SelectPSAModel\">
            <input>
                <soap:body use=\"literal\"/>
            </input>
            <output>
                <soap:body use=\"literal\"/>
            </output>
        </operation>
    </binding>

    <service name=\"Repository\">
        <port name=\"ContentManagerPort\" binding=\"tns:ContentManagerBinding\">
            <soap:address location=\"http://127.0.0.1:8080/services/Repository\"/>
        </port>
    </service>


</definitions>
"
  )

(defun ts2020 (&key (clean t) &aux def)
  ;; test for bug21565 fix
  (soap-new-environment)
  (define-namespace :repo "repo" "urn:repository")
  (define-namespace-map :repomap nil '(:repo))
  (setf def (decode-wsdl-string
	     *ts2020-wsdl*
	     ;; "spr39898.wsdl" ;;; "massifrepository2.wsdl" 
	     :namespaces :repomap :verbose t :import t))
  (prog1 (test-no-error (make-client-interface def 0 "ts2020.cl"))
    (when clean (delete-file "ts2020.cl") (delete-file "ts2020.txt"))))


(defun ts2030 (&aux 
	       (cl (soap-message-client :url "dummy"))
	       (type '(:complex
		       (:seq
			(:element "with-choice"
				  (:complex 
				   (:or (:element "foo" xsd:|string|)
					(:element "bar" xsd:|string|)))))))
	       (type2 '(:complex
			(:seq
			 (:or (:element "foo" xsd:|string|)
			      (:element "bar" xsd:|string|)))))
	       (type3 '(:complex
			(:or (:element "foo" xsd:|string|)
			     (:element "bar" xsd:|string|))))
	       )
  ;; Test case for 
  ;; bug20672/spr38758 SOAP module mishandles xsd:choice constructs
  (test
   "<top><with-choice><bar xsi:type=\"xsd:string\">FOOBODY</bar></with-choice></top>"
   (xmp-element-content
    (soap-encode-object 
     cl "top" type (list "with-choice" (list "bar" "FOOBODY"))))
   :test #'equal)
  (test
   "<top><bar xsi:type=\"xsd:string\">FOOBODY</bar></top>"
   (xmp-element-content
    (soap-encode-object 
     cl "top" type2 (list "bar" "FOOBODY")))
   :test #'equal)
  (test
   "<top><bar xsi:type=\"xsd:string\">FOOBODY</bar></top>"
   (xmp-element-content
    (soap-encode-object 
     cl "top" type3 (list "bar" "FOOBODY")))
   :test #'equal)
  )
   
  
