;; -*- mode: common-lisp; package: net.xmp -*-
;;
;; See the file LICENSE for the full license governing this code.

;; $Id: xmp-mtest.cl,v 2.3 2007/04/17 21:50:41 layer Exp $

;; Test cases for XML decoder

(in-package :user)

(eval-when (compile load eval)

  (let* ((module (ecase *current-case-mode*
		   (:case-sensitive-lower :soapm)
		   (:case-insensitive-upper :soapa)))
	 (file (string-downcase (format nil "~A.fasl"  module))))
    (or (member :soap *modules* :test #'string-equal)
	(when (probe-file file)
	  (load file)
	  (provide module)
	  (provide :soap)))
    (require :soap)
    (require :tester)
    )
  )
(defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
(defpackage :user (:use :net.xmp))


(defclass test-decoder-conn (xmp-string-in-out-connector)
  ((message-dns   :initform :tt)
   (first :initarg :first)
   ))
	


(defmethod xmp-begin-message ((conn test-decoder-conn))
  (list :seq1 (slot-value conn 'first)))

(defmethod xmp-end-message ((conn test-decoder-conn) data
			    &key types &allow-other-keys)
  (values data types))

(defmethod xmp-complex-content ((conn test-decoder-conn) (elt t) data
				&rest options &key &allow-other-keys
				)
  (values (list (cons elt data)) elt))

(defmethod xmp-simple-content :around ((conn test-decoder-conn) 
				       (elt t) (data t)
				       &rest options &key &allow-other-keys)
  (declare (ignore options))
  (let ((v (multiple-value-list (call-next-method))))
    (cond ((and (consp v) (consp (first v)))
	   (if (cdr (first v))
	       (values (second (first v)) (second v))
	     (values)))
	  (t (values-list v)))))




(defun make-decoder-test-string (top &rest parts)
  (concatenate 'string
	       "<?xml version='1.0' ?>"
	       (format nil "<~A xmlns='urn:tester'>" top)
	       (apply 'concatenate
		      'string
		      (mapcar 'make-decoder-test-part parts))
	       (format nil "</~A>" top)))

(defun make-decoder-test-part (p)
  (etypecase p
    (string (format nil "~A" p))
    (symbol (format nil "<~A />" p))
    (cons (format nil
		  "~A~{~A~}~A"
		  (etypecase (first p)
		    (string (format nil "<~A>" (first p)))
		    (symbol (format nil "<~A>" (first p)))
		    (cons   (format nil "<~A ~{~A='~A' ~}>" 
				    (first (first p)) (cdr (first p)))))
		  (mapcar 'make-decoder-test-part (cdr p))
		  (etypecase (first p)
		    (string (format nil "</~A>" (first p)))
		    (symbol (format nil "</~A>" (first p)))
		    (cons   (format nil "</~A>" (first (first p)))))
		  ))))
	   
(defun one-decoder-test (data &key syntax verbose &aux in)
  (setf in (apply 'make-decoder-test-string data))
  (when verbose (format t "~&;one-decoder-test ~S ~S~%" data in))
  (multiple-value-bind (v e)
      (xmp-decode-string (make-instance 'test-decoder-conn :first (first data)
					:xml-syntax syntax
					)
			 (apply 'make-decoder-test-string data))
    (test data (first v) :test #'equal :fail-info data)
    (values (equal (first v) data)
	    v e data)))

(defun err-decoder-test (data &key syntax verbose)
  (multiple-value-bind (v e)
       (ignore-errors
	 (multiple-value-list
	  (one-decoder-test data :syntax syntax :verbose verbose)))
     (if e
	 (values (format nil "~A" e) e v)
       (values-list (list* nil v)))))

(defvar *all-decoder-tests*)

(defun test-decoder-all (&key i verbose debug errorp &aux fail (*xmp-debug* debug))
  
  (delete-namespace :keyword nil nil)
  (define-namespace :keyword "ts" "urn:tester")
  (define-namespace-map :tt nil '(:keyword))

  (dolist (test-case *all-decoder-tests* (if fail nil t))
    (etypecase (first test-case)
      ((member :def)
       (when verbose
	 (format
	  t
	  "~&~%;test-decoder-all def: ~S ~S~%~%"
	  (second test-case)
	  (third test-case)))
       (define-xmp-element nil (second test-case) (third test-case)))
      (number
       (when (and verbose (or (null i) (eql i (first test-case))))
	 (format t "~&;test-decoder-all ~A: ~S ~S~%"
		 (first test-case) (second test-case) (third test-case))) 
       (dolist (syntax '(nil :strict))
	 ;;  syntax =nil  :strict
	 ;;  :eval  eval  skip 
	 ;;  :err   err   skip 
	 ;;  :eva2  eval  eval
	 ;;  :err2  err   err
	 ;;  :evas  skip  eval
	 ;;  :errs  skip  err
	 (let ((res (multiple-value-list
		     (apply 'test-decoder-one errorp syntax i verbose test-case))))
	   (cond ((null res))
		 ((null (first res))
		  (when i
		    (return-from test-decoder-all
		      (values-list (list* nil (first test-case) :error (cdr res)))))
		  (format t "~&;test-decoder-all ~A syntax=~S error: ~A~%"
			  (first test-case) syntax (second res))
		  (setf fail t))
		 ((eql :skip (first res)))
		 ((eql i (first test-case))
		  (return (values-list (list* (first res) (first test-case) (cdr res)))))
		 (verbose
		  (format t "~&;test-decoder-all ~A:     ~S~%"
			  (first test-case) (second res))))))))))

(defun test-decoder-one (errorp syntax i verbose key kind data &aux errtest)
  (cond ((and i (not (eql i key))) (values))
	((ecase kind
	   (:eval (case syntax (:strict nil) (otherwise t)))
	   (:err  (case syntax (:strict nil) (otherwise (setf errtest t) t)))
	   (:eva2 t)
	   (:err2 (setf errtest t) t)
	   (:evas (case syntax (:strict t)))
	   (:errs (case syntax (:strict (setf errtest t) t))))
	 (cond (errorp   (with-simple-restart
			  (:continue-tests "Continue testing")
			  (one-decoder-test data :verbose verbose :syntax syntax)))
	       (errtest  (err-decoder-test data :verbose verbose :syntax syntax))
	       (t        (one-decoder-test data :verbose verbose :syntax syntax)))
	 )
	(t (values))))


(setf *all-decoder-tests*
  '(

    (:def :t001 (:complex (:seq :e1 :e2 :e3)))
    ;; (key kind data)
    (1 :eval (:t001 (:e1) (:e2) (:e3)))
    (2 :eval (:t001 (:e1) (:e2)))
    (3 :eval (:t001 (:e1) (:e3)))
    (4 :eval (:t001 (:e2) (:e3)))
    (5 :eval (:t001 (:e1)))
    (6 :eval (:t001 (:e2)))
    (7 :eval (:t001 (:e3)))
    (8 :err  (:t001 (:e2) (:e3) (:e1)))
    (9 :eval (:t001))


    (:def :t010 (:complex (:set :e1 :e2 :e3)))

    (10 :eval (:t010 (:e1) (:e2) (:e3)))
    (11 :eval (:t010 (:e1) (:e3) (:e2)))
    (12 :eval (:t010 (:e2) (:e1) (:e3)))
    (13 :eval (:t010 (:e2) (:e3)))
    (14 :eval (:t010 (:e3) (:e2)))
    (15 :eval (:t010 (:e3)))
    (16 :eval (:t010))
    (17 :err  (:t010 (:e1) (:e2) (:e3) (:e2)))
    (18 :err  (:t010 (:e1) (:e2) (:e3) (:e4)))


    (:def :t020 (:complex (:set* :e1 :e2 :e3)))

    (20 :eval (:t020 (:e1) (:e2) (:e3)))
    (21 :eval (:t020 (:e1) (:e2)))
    (22 :eval (:t020 (:e1) (:e3)))
    (23 :eval (:t020 (:e2) (:e3)))
    (24 :eval (:t020 (:e1) (:e2) (:e1) (:e3)))
    (25 :eval (:t020 (:e1) (:e2) (:e1) (:e1) (:e3)))
    (26 :eval (:t020 (:e2) (:e2) (:e2) (:e3) (:e3)))
    (27 :err  (:t020 (:e2) (:e2) (:e2) (:e3) (:e3) (:e4)))
    (28 :eval (:t020))


    (:def :t030 (:complex (:seq* :e1 :e2 :e3)))

    (30 :eval (:t030 (:e1) (:e2) (:e3)))
    (31 :eval (:t030 (:e1) (:e2)))
    (32 :eval (:t030 (:e2) (:e3)))
    (33 :eval (:t030 (:e1) (:e1) (:e2) (:e3)))
    (34 :eval (:t030 (:e1) (:e2) (:e2) (:e2) (:e3)))
    (35 :eval (:t030))
    (36 :err  (:t030 (:e1) (:e3) (:e2)))
    

    (:def :t040 (:complex (:seq1 :e1 :e2 :e3)))

    (40 :eval (:t040 (:e1) (:e2) (:e3)))
    (41 :err  (:t040 (:e1) (:e2)))
    (42 :err  (:t040 (:e1) (:e3)))
    (43 :err  (:t040 (:e2) (:e3)))
    (44 :err  (:t040 (:e1)))
    (45 :err  (:t040 (:e2)))
    (46 :err  (:t040 (:e3)))
    (47 :err  (:t040))
    (48 :err  (:t040 (:e1) (:e2) (:e2) (:e3)))
    (49 :err  (:t040 (:e1) (:e2) (:e4) (:e3)))

    
    (:def :t050 (:complex (:seq+ :e1 :e2 :e3)))

    (50 :eval (:t050 (:e1) (:e2) (:e3)))
    (51 :eval (:t050 (:e1) (:e2) (:e2) (:e3)))
    (52 :eval (:t050 (:e1) (:e2) (:e2) (:e2) (:e3)))
    (53 :eval (:t050 (:e1) (:e1) (:e2) (:e3)))
    (54 :eval (:t050 (:e1) (:e1) (:e1) (:e2) (:e3)))
    (55 :eval (:t050 (:e1) (:e1) (:e2) (:e2) (:e3)))
    (56 :err  (:t050 (:e1) (:e3)))
    (57 :err  (:t050 (:e1) (:e4) (:e3)))


    (:def :t060 (:complex (:set1 :e1 :e2 :e3)))

    (60 :eval (:t060 (:e1) (:e2) (:e3)))
    (61 :eval (:t060 (:e2) (:e1) (:e3)))
    (62 :eval (:t060 (:e1) (:e3) (:e2)))
    (63 :eval (:t060 (:e3) (:e1) (:e2)))
    (64 :err  (:t060 (:e1) (:e2)))
    (65 :err  (:t060 (:e1) (:e2) (:e3) (:e2)))
    (66 :err  (:t060))


    (:def :t070 (:complex (:set+ :e1 :e2 :e3)))

    (70 :eval (:t070 (:e1) (:e2) (:e3)))
    (71 :eval (:t070 (:e1) (:e2) (:e1) (:e3)))
    (72 :eval (:t070 (:e1) (:e2) (:e3) (:e2) (:e2)))
    (73 :eval (:t070 (:e3) (:e2) (:e3) (:e1) (:e1)))
    (74 :err  (:t070 (:e1) (:e2)))
    (75 :err  (:t070 (:e1) (:e2) (:e3) (:e4)))


    (:def :t080 (:complex (:seq (:set* :a :b) (:or :c :d)
				(:set1 :e :f :g) (:set+ :h :i))))

    (80 :eval (:t080 (:a) (:c) (:e) (:f) (:g) (:h) (:i)))
    (81 :eval (:t080 (:b) (:d) (:f) (:g) (:e) (:i) (:h) (:i)))


    (:def :t1010 (:complex (:seq? :a :b :c (:maybe :d))))

    (1010 :eva2 (:t1010 (:a) (:b) (:c)))
    (1011 :err2 (:t1010 (:a) (:b) (:c) (:e)))
    (1020 :eval (:t1010 (:a) (:b) (:c) (:d)))
    (1021 :eval (:t1010 (:b) (:c) (:a) (:d)))
    (1022 :eval (:t1010 (:b) (:c) (:a) (:b) (:d)))
    (1023 :errs (:t1010 (:a) (:b) (:c) (:d)))
    (1024 :errs (:t1010 (:b) (:c) (:a) (:d)))
    (1025 :errs (:t1010 (:b) (:c) (:a) (:b) (:d)))

    (:def :t9001 (:complex (:seq 
			    (:set* :inc "imp")
			    :doc
			    (:element :types (:complex (:seq* (:any))))
			    (:set* :ms (:or :int :port) :bind :serv)
			    (:seq* (:any)))))
    (9001 :eval (:t9001 (:types) (:ms) (:ms) (:ms) (:port) (:bind) (:serv)))


    ;; this is probably ill-formed XML or Schema sincd element :foo appears
    ;;  with two different definitions
    (:def :ct2 (:complex (:or (:seq (:element :foo (:complex (:seq :bar :baz)))
				    :bob)
			      (:seq (:element :foo (:complex (:seq :baz :bar)))
				    :box))))
    (9010 :eval (:ct2 (:foo (:bar) (:baz)) (:bob)))
    ;; the following is an error because we do not backtrack after picking
    ;;  up element :baz
    (9011 :err (:ct2 (:foo (:baz) (:bar)) (:box)))
    (9012 :err  (:ct2 (:foo (:baz) (:bar)) (:bob)))

    
    (:def :t9021 (:complex (:or (:element :f1 xsd:|int|) (:element :f2 xsd:|int|))))
    (:def :t9020 (:complex
		  (:seq :t9021 (:element :e1 xsd:|int|) (:element :e2 xsd:|int|))))
    (9020 :eval (:t9020 (:f1 "123") (:e1 "45") (:e2 "67")))
    (9021 :eval (:t9020 (:f2 "123") (:e1 "45") (:e2 "67")))
    (9022 :eval (:t9020 (:e1 "45") (:e2 "67")))
    
    ))


	     

	      
