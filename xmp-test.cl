;; -*- mode: common-lisp; package: user -*-
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

;; $Id: xmp-test.cl,v 1.1.1.1 2003/07/24 00:49:45 layer Exp $

;; Internal-use test cases

(in-package :user)

(eval-when (compile load eval)
  (require :pxml)
  (require :soap)
  )

(defpackage :user (:use :net.xmp :net,xmp.soap)) 



(defun s1 () (net.xmp::xmp-schema "envelope.xml"))
(defun s2 () (net.xmp::xmp-schema "encoding.xml"))
(defun s3 () (net.xmp::xmp-schema "XMLSchema.xml"))

(defun sp (&optional (n 1) (d :warn) (u nil))
  (let ((s (soap-message-client :lisp-package :keyword
				:decode-flag d :must-understand u)))
    (values
     (net.xmp::xmp-decode-file s (format nil "ex~A.xml" n))
     s)))


(defclass ct1 (xmp-string-in-connector) ())
(defmethod xmp-begin-message ((conn ct1)) (list :seq1 (list :or :ct1 :ct2)))
(defmethod xmp-end-message ((conn ct1) data &key types &allow-other-keys)
  (values data types))
(defmethod xmp-simple-content ((conn ct1) (elt (eql :int)) data &rest options
			       &key attributes &allow-other-keys)
  (values (parse-integer data)))
(defmethod xmp-simple-content ((conn ct1) (elt (eql :string)) data &rest options
			       &key attributes &allow-other-keys)
  data)			       
(define-xmp-element nil :ct1 '(:complex (:or (:seq (:element :foo :int) :bar)
					     (:seq (:element :foo :string) :baz))))
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
;;;      element foo has two conflicting definitions
(define-xmp-element nil :ct2 '(:complex (:or (:seq (:element 
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



;;; Test code to verify class precedence lists
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

