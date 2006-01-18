
;; $Id: build.cl,v 2.7 2006/01/18 21:07:23 mm Exp $ 

(in-package :user)

(let* ((filenames (list "xmp-base"
			"xmp-aserve"
			"xmp-schema"
			"xmp-soap"
			"xmp-wsdl"
			))
       (soap (ecase *current-case-mode*
	       (:case-sensitive-lower "soapm")
	       (:case-insensitive-upper "soapa")))
       (soap-cl (format nil "~a.cl" soap))
       (soap-fasl (format nil "~a.fasl" soap)))

  (dolist (f filenames) (load (compile-file (concatenate 'string f ".cl"))))
  (when (eq :case-sensitive-lower *current-case-mode*)
    (compile-file "soap.cl"))
  
  (with-open-file
   (out soap-cl :direction :output :if-exists :supersede
	:if-does-not-exist :create)
   (print `(in-package :user) out)

   (flet ((patch-level
	   (n)
	   (print `(sys:defpatch ,(ecase *current-case-mode*
				    (:case-sensitive-lower "soapm")
				    (:case-insensitive-upper "soapa"))
				 ,n
				 ,(ecase *current-case-mode*
				    (:case-sensitive-lower "SOAPM code")
				    (:case-insensitive-upper  "SOAPA code"))
				 :type :system
				 :post-loadable t)
		  out)))	       

     #+(version= 8 0) (patch-level 1)
     #+(version= 7 0) (patch-level 4)
     #+(version= 6 2) (patch-level 5)
     
     )

   (terpri out))
  (compile-file soap-cl)
  
  (with-open-file
   (out soap-fasl
	;;:element-type '(unsigned-byte 8)
	:direction :output 
	:if-exists :append  ;;; soapa.fasl or soapm.fasl was just written
	:if-does-not-exist :create)
   (dolist (file filenames)
     (with-open-file 
      (in (concatenate 'string file ".fasl")
	  ;;:element-type '(unsigned-byte 8)
	  )
      (format t "~%; ~s" file)
      (let ((buf (make-array 2048 :element-type '(unsigned-byte 8))))
	(loop as x = (read-sequence buf in)
	      until (= x 0)
	      do (write-sequence buf out :end x)))))))

