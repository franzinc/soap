
(in-package :user)
(require :xmpt-driver)

(push "bnwsdl.out" *xmpt-cleanup*)
(push "bn-client.cgen" *xmpt-cleanup*)
(push "bn-client.txt" *xmpt-cleanup*)
(push "bn-client.fasl" *xmpt-cleanup*)
(push "bn-server.cgen" *xmpt-cleanup*)
(push "bn-server.txt" *xmpt-cleanup*)


;;; COPIED FROM examples/bignum-server.cl

;; Test a SOAP server/client pair generated from Lisp code.
;; Generate WSDL from Lisp definitions of server.
;; Generate Lisp client from WSDL.
;; Test generated client code.


(defpackage :net.xmp.schema (:use) (:nicknames :xsd))



;;;
;;; The Lisp part of this application
;;;

;; A Lisp function to translate
;;   a string to a function name symbol
;;   a second string to an arbitrarily large integer
;;   a third string to a second integer
;; The result is a string of digits representing the answer.
(defun bignum-calculate (op arg1 arg2)
  (let* ((opsym (read-from-string (format nil "~A" op)))
	 (num1 (parse-integer arg1))
	 (num2 (parse-integer arg2)))
    (case opsym
      ((+ - * ash truncate ceiling expt bnfactorial gcd rem)
       (format nil "~A" (funcall opsym num1 num2)))
      (otherwise (error "Unknown operation"))
      )))

;; A Lisp function to translate a string of digits into a list
;;   of digits in some arbitrary base.
;; This function allows a foreign language caller to view the
;;   large integer as a vector of integers within the integer
;;   range of the foreign language.
(defun bignum-decode-num (string base)
  (let ((num (parse-integer string)) rem res)
    (loop
     (multiple-value-setq (num rem) (truncate num base))
     (push rem res)
     (when (zerop num) (return)))
    res))
	
;; A Lisp function to translate a sequence of digits into
;;   a string representing the actual number.
;; Each digit appears as the list (:item digit)
(defun bignum-encode-num (seq base &aux (res 0))
  (dotimes (i (length seq) (format nil "~A" res))
    (setf res (+ (* res base) (second (elt seq i))))))

;; The most popular function to generate really big integers.
(defun bnfactorial (n dummy &aux (r 1))
  (declare (ignore dummy))
  (loop
   (when (< n 2) (return r))
   (setf r (* r n))
   (decf n)))



;;;
;;; The SOAP interface to the application
;;;


(defun bignum-defs (pk map)
  ;; Define one namespace-to-package mapping
  ;; The package :bn will be created to hold the namespace-qualified 
  ;;     symbols in this application
  (define-namespace pk (string pk) "urn:bignumserver")

  ;; Define a named namespace-to-package mapping
  (define-namespace-map map nil (list pk))


  (define-soap-element nil (intern "calculate" pk)
    `(:complex
      (:seq 
       (:element "opname" xsd:|string|)
       (:element "num1"   xsd:|string|)
       (:element "num2"   xsd:|string|))
      :action "calculate"
      :namespaces ,map
      ))

  (define-soap-element nil "calculateResponse"
    `(:complex
      (:seq
       (:element "calcResult" xsd:|string|))))


  (define-soap-element nil  (intern "decodeNum" pk)
    `(:complex
      (:seq
       (:element "num" xsd:|string|)
       (:element "base" xsd:|int|))
      :action "decodeNum"
      :namespaces ,map
      ))

  (define-soap-type nil (intern "arrayOfBigits" pk)
    '(:complex (:seq* (:element "item" xsd:|int|))))

  (define-soap-element nil "decodeNumResponse"
    `(:complex
      (:seq
       (:element "decResult" ,(intern "arrayOfBigits" pk)))))


  (define-soap-element nil (intern "encodeNum" pk)
    `(:complex
      (:seq
       (:element "bigits" ,(intern "arrayOfBigits" pk))
       (:element "base"  xsd:|int|))
      :action "encodeNum"
      :namespaces ,map
      ))

  (define-soap-element nil "encodeNumResponse"
    `(:complex
      (:seq
       (:element "encResult" xsd:|string|))))
  )


(defvar *bignum-server* nil)
(defun bignum-start-server (&key debug pk map (port 1776) (host "localhost"))
  (let ((server (soap-message-server :start (list :port port)
				     :lisp-package :keyword
				     :message-dns map
				     :url (format nil "http://~A:~A/SOAP" host port)
				     :service-name "BigNumService"
				     :soap-debug debug
				     )))
    (soap-export-method
     server (intern "calculate" pk) '("opname" "num1" "num2")
     :lisp-name 'bignum-calculate-method
     :return "calculateResponse"
     :action "calculate"
     )
    (soap-export-method
     server (intern "decodeNum" pk) '("num" "base")
     :lisp-name 'bignum-decode-num-method
     :return "decodeNumResponse"
     :action "decodeNum"
     )
    (soap-export-method
     server (intern "encodeNum" pk) '("bigits" "base")
     :lisp-name 'bignum-encode-num-method
     :return "encodeNumResponse"
     :action "encodeNum"
     )
    (setf *bignum-server* server)))

(defun bignum-calculate-method (&key ((:|opname| op)) ((:|num1| n1)) ((:|num2| n2))) 
  (list "calcResult" (bignum-calculate op n1 n2)))

(defun bignum-decode-num-method (&key ((:|num| num)) ((:|base| base)))
  (list "decResult" 
	(mapcan #'(lambda (n) (list "item" n)) (bignum-decode-num num base))))

(defun bignum-encode-num-method (&key ((:|bigits| bigits)) ((:|base| base)))
  (list "encResult" (bignum-encode-num bigits base)))





(defun bignum-try-server (&key pk debug (port 1776) (host "localhost"))
  (let ((conn (soap-message-client 
	       :url (format nil "http://~A:~A/SOAP" host port)
	       :soap-debug debug
	       :lisp-package :keyword
	       )))
    (xmptr
     (test (format nil "~A" (bnfactorial 17 nil))
	   (soap-result-only 
	    conn
	    (call-soap-method
	     conn (intern "calculate" pk) "opname" "bnfactorial" "num1" "17" "num2" "0")
	    nil nil "calcResult")
	   :test #'equal)
     (test "18"
	   (soap-result-only 
	    conn
	    (call-soap-method
	     conn (intern "encodeNum" pk)
	     "bigits" (list "item" 1 "item" 2 "item" 3) "base" 3)
	    nil nil "encResult")
	   :test #'equal)
     (test '((:|item| 1) (:|item| 2) (:|item| 3) (:|item| 4) (:|item| 5))
	   (soap-result-only 
	    conn
	    (call-soap-method
	     conn (intern "decodeNum" pk) "num" "12345" "base" 10)
	    nil nil "decResult")
	   :test #'equal)
     )))

(defun bignum-try-calc (op n1 n2 &key pk debug (port 1776) (host "localhost"))
  (let ((client (soap-message-client 
		 :url (format nil "http://~A:~A/SOAP" host port)
		 :soap-debug debug
		 :lisp-package :keyword
		 )))
    (call-soap-method
     client (intern "calculate" pk) "opname" op "num1" n1 "num2" n2)))

;; Test a SOAP server/client pair generated from Lisp code.
(xmptr
 (test-no-error (bignum-defs :pk :pkm))
 (test-no-error (bignum-start-server :pk :pk :map :pkm))
 )
(bignum-try-server :pk :pk)


(defun bignum-gen-wsdl ()
  (encode-wsdl-file 
   "bnwsdl.out"
   :servers *bignum-server*
   :target "urn:bignumserver"
   :namespaces '(nil
		 :pkm
		 :schema
		 )))

;; Generate WSDL from Lisp definitions.
(xmptr
 (test-no-error (bignum-gen-wsdl))
 (test-no-error (stop-soap-server *bignum-server*))
 (test-no-error (soap-new-environment))
 )




(defun bignum-gen-client ()
  (make-client-interface
   (decode-wsdl-file "bnwsdl.out") 0 "bn-client.cgen" :prefix "bnc-" :suffix :message)
  (load (compile-file "bn-client.cgen")))

(defun bignum-gen-server ()
  (make-server-interface
   (decode-wsdl-file "bnwsdl.out") 0 "bn-server.cgen" :prefix "bns-" :suffix :message))

(define-namespace :pkg "pkg" "urn:bignumserver")
(define-namespace-map :pkgm nil (list :pkg))
;; Generate Lisp client from WSDL.
(xmptr
 (test-no-error (bignum-gen-client))
 (test-no-error (bignum-gen-server))
 (test-no-error (bignum-start-server :pk :pkg :map :pkgm))
 )


(defun bignum-try-gen (&key debug (port 1776) (host "localhost"))
  (let ((*bnm-service-url* (format nil "http://~A:~A/SOAP" host port)))
    (xmptr
     (test (format nil "~A" (bnfactorial 17 nil))
	   (soap-result-only 
	    nil
	    (bnc-calculate :opname "bnfactorial" :num1 "17" :num2 "0")
	    nil nil "calcResult")
	   :test #'equal)
     (test "18"
	   (soap-result-only 
	    nil
	    (|bnc-encodeNum| :bigits (list "item" 1 "item" 2 "item" 3) :base 3)
	    nil nil "encResult")
	   :test #'equal)
     (test '((:|item| 1) (:|item| 2) (:|item| 3) (:|item| 4) (:|item| 5))
	   (soap-result-only 
	    nil
	    (|bnc-decodeNum| :num "12345" :base 10)
	    nil nil "decResult")
	   :test #'equal))))
  
;; Test generated client code.
(bignum-try-gen)

