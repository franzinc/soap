;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2003-2006 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: $

;; Comprehensive SOAP/WSDL tests

(in-package :user)

#|
----- TESTING PROCEDURE -- run on alisp AND mlisp -----

Called with soap-files-tests from xmp-test.cl.

xmp-run-test-files  scans all file names and types.
  For each name:
      If there is a .lisp file, just load it (it does all the rest).
      If there is a .cl file, compile and load (it does all the rest).
      If there is a .wsdl file, decode, make client interface and
                            compile interface.
|#


(eval-when (compile load eval)

  #+ignore
  (let* ((module (ecase *current-case-mode*
		   (:case-sensitive-lower :soapm)
		   (:case-insensitive-upper :soapa)))
	 (file (string-downcase (format nil "~A.fasl"  module)))
	 pop)
    ;; OLD soapm/soapa style
    (or (member :soap *modules* :test #'string-equal)
	(when (or (probe-file file)
		  (progn (tpl:do-command :pushd "..")
			 (setf pop t)
			 (probe-file file)))
	  (load file)
	  (when pop (tpl:do-command :popd))
	  (provide module)
	  (provide :soap)
	  t)
	(cerror "Continue with distributed module"
		"Cannot find local SOAP module")
	)
    )
  (let (pop)
    ;; NEW single soap.fasl
    (or (member :soap *modules* :test #'string-equal)
	(when (or (probe-file "soap.fasl")
		  (progn (tpl:do-command :pushd "..")
			 (setf pop t)
			 (probe-file "soap.fasl")))
	  (load "soap.fasl")
	  (when pop (tpl:do-command :popd))
	  (provide :soap)
	  t)
	(cerror "Continue with distributed module"
		"Cannot find local SOAP module")
	))

  (require :soap)
  (require :tester)
  (use-package :util.test)
  (provide :xmpt-driver)
  )

(defpackage :user (:use :net.xmp.soap :util.test)) 

(defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
(defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
(defpackage :net.xmp.soap.none (:use) (:nicknames :none))
(defpackage :net.xmp.soap.envelope (:use) (:nicknames :env))
(defpackage :net.xmp.soap.encoding (:use) (:nicknames :enc))

(defvar *xmpt-name*)
(defvar *xmpt-wsdl*)
(defvar *xmpt-out*)
(defvar *xmpt-cleanup*)
(defvar *xmpt-syntax*)
(defvar *xmpt-result*)
(defvar *xmpt-errorp* nil)

(defmacro xmptr (&rest forms &aux (r (gensym)))
  ;; 
  ;; (xmptr form1 form2 ...) 
  ;;   If formi returns nil, update (logior) *xmpt-result* with #x02
  ;;              otherwise, update (logior) *xmpt-result* with #x01
  ;;
  `(let ((,r 0))
     ,@(mapcar #'(lambda (form)
		   `(if ,form
			(setf ,r (logior ,r 1))
		      (setf ,r (logior ,r 2))))
	       forms)
     (setf *xmpt-result* (if *xmpt-result* (logior *xmpt-result* ,r) ,r))))

(defun xmpt-report (fail-info e)
  (format
   t "~&;~A;  reporting error ~S~%;  ~A~%;  ~S~%;  ~S~%"
   fail-info e e
   (simple-condition-format-control e)
   (simple-condition-format-arguments e)
   ))

(defmacro xmpt-error (form &rest more &key matching fail-info &allow-other-keys)
  (cond
   (matching
    (let ((x (member :matching more)))
      (setf more (mapcon #'(lambda (y)
			     (cond ((eq y x) nil)
				   ((eq y (cdr x)) nil)
				   (t (list (car y)))))
			 more)))
    `(multiple-value-bind (v e) (ignore-errors (multiple-value-list ,form))
       (when (and e *xmpt-errorp*) (xmpt-report ,fail-info e))
       (if (match-re ,matching (format nil "~A" e))
	   (xmptr (test-error (error e) :announce t ,@more))
	 (xmptr (test-error nil ,@more)))))
   (t `(xmptr
	(cond
	 (*xmpt-errorp*
	  (multiple-value-bind (v e) (ignore-errors (multiple-value-list ,form))
	    (cond (e (xmpt-report ,fail-info e)
		     (test-error (error e) ,@more))
		  (t (test-error nil ,@more)))))
	 (t  (test-error ,form ,@more)))))))


(defmacro xmpt-warning (form &rest more &key matching fail-info &allow-other-keys)
  (progn
    (let ((x (member :matching more)))
      (setf more (mapcon #'(lambda (y)
			     (cond ((eq y x) nil)
				   ((eq y (cdr x)) nil)
				   (t (list (car y)))))
			 more)))
    `(multiple-value-bind (v e w)
	 (let (ww)
	   (handler-bind 
	    ((warning (lambda (w) (setq ww w))))
	    (multiple-value-bind (vv ee) (ignore-errors (multiple-value-list ,form))
	      (values vv ee ww))))
       (when (and e *xmpt-errorp*) (xmpt-report ,fail-info e))
       (if (and w (or (null ,matching) (match-re ,matching (format nil "~A" w))))
	   (xmptr (test-error (error (format nil "found the warning")) ,@more))
	 (xmptr (test-error nil ,@more))))))



(defvar *wsdl-def*)
(defun xmp-run-test-files (&key one keep verbose syntax break)
  ;; break -> nil    --- do not stop for anything
  ;;       -> :error --- break for unexpected error signals
  ;;       -> :fail  --- break for test failures
  ;;       -> other  --- break for errors and failures
  (let* ((all (directory "."))
	 (names (do ((tl all (cdr tl)) e p n r)
		    ((atom tl) (sort r #'string-lessp :key #'first))
		  (when (setf p (file-namestring (first tl)))
		    (setf n (pathname-name p))
		    (when (not (equal n "xmp-driver"))
		      (or (setf e (assoc n r :test #'equal))
			  (push (setf e (list n)) r))
		      (push (pathname-type p) (cdr e))))))
	 (*xmpt-errorp* break)
	 (*error-protect-tests* (case break
				  ((nil) t)
				  (otherwise nil)))
	 (*break-on-test-failures* (case break
				     ((nil) nil)
				     (otherwise t)))
	 failed
	 )

    (dolist (name names)
      (when (or (null one)
		(etypecase one
		  ((or string symbol) (string-equal one (first name)))
		  (cons (member (first name) one :test #'string-equal)))
		)
	(let* (
	       (*xmpt-name* (first name))
	       (*xmpt-wsdl* (namestring (make-pathname :name (first name) :type "wsdl")))
	       (*xmpt-out*  (namestring
			     (make-pathname :type "out" :defaults *xmpt-wsdl*)))
	       (*xmpt-cleanup* nil)
	       (*xmpt-syntax* syntax)
	       (*xmpt-result* nil)
	      )
	  (soap-new-environment)
	  (when verbose (format t "~&;Begin ~A~%" name))
	  (xmpt-out "fasl")   ;;; Just in case...
	  (unwind-protect
	      (cond
	       ((member "lisp" (cdr name) :test #'equal)
		(load (make-pathname :name (first name) :type "lisp")))
	       ((member "cl" (cdr name) :test #'equal)
		(push (make-pathname :name (first name) :type "fasl") *xmpt-cleanup*)
		(load (compile-file (make-pathname :name (first name) :type "cl"))))
	       ((member "wsdl" (cdr name) :test #'equal)
		(let* ((wsdl *xmpt-wsdl*) *wsdl-def* out tform)
		  (and
		   (setf *xmpt-result* 2)
		   (setf tform `(decode-wsdl-file ',wsdl :xml-syntax ',*xmpt-syntax*))
		   (test-no-error  (setf *wsdl-def* (eval tform))  :fail-info tform)
		   (setf tform `(make-client-interface
				 *wsdl-def* 0 ',(setf out *xmpt-out*)
				 :text-file nil
				 :prefix ',(first name)
				 :xml-syntax ',*xmpt-syntax*))
		   (test-no-error (eval tform)   :fail-info tform)
		   (test-no-error (compile-file out)
				  :fail-info (list :compile-client-interface wsdl out))
		   (push out *xmpt-cleanup*)
		   (push (make-pathname :name (first name) :type "fasl") *xmpt-cleanup*)
		   (setf *xmpt-result* 1)		   
		   )))
	       )
	    (or keep
		(dolist (fl *xmpt-cleanup*)
		  (ignore-errors (delete-file fl))))
	    )
	  (when (not (eql *xmpt-result* 1)) (push name failed))
	  (when verbose (format t "~&;  End ~A ~A~%" name
				(case *xmpt-result*
				  (1 "all tests succeeded")
				  (2 "all tests failed")
				  (3 "some tests succeeded")
				  (otherwise "tests did not run"))))
	  )))
    (values (null failed) failed)
    ))

(defun xmpt-decode-file (&rest args)
  (let (r)
    (test-no-error
     (setf r (apply #'decode-wsdl-file *xmpt-wsdl* :xml-syntax *xmpt-syntax* args))
     :fail-info (list* :decode *xmpt-wsdl* :xml-syntax *xmpt-syntax* args))
    r))

(defun xmpt-out (&optional (type) &aux file)
  (cond (type (push (setf file 
			  (namestring (make-pathname :type type :defaults *xmpt-out*)))
		    *xmpt-cleanup*)
	      file)
	(t (push *xmpt-out* *xmpt-cleanup*) *xmpt-out*)))

(defun xmpt-fail-info (key &rest more)
  (list* key *xmpt-wsdl* more))

(defun xmpt-decode-client ()
  (let* ((wconn (xmpt-decode-file)) r)
    (push *xmpt-out* *xmpt-cleanup*)
    (xmptr
     (test-no-error
      (setf r (make-client-interface
	       wconn 0 *xmpt-out* :text-file nil :xml-syntax *xmpt-syntax*))
      :fail-info (list* :make-client *xmpt-wsdl* :xml-syntax *xmpt-syntax*)))
    r))



(format t "~&~%;;; xmp-run-test-files &key one keep verbose syntax errorp~%~%")
