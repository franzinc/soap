
;; $Id: build.cl,v 1.1.1.1 2003/07/24 00:49:45 layer Exp $ 

(in-package :user)

(let ((filenames 
       (list
	"xmp-base"
	"xmp-aserve"
	"xmp-schema"
	"xmp-soap")))

  (dolist (f filenames) (load (compile-file (concatenate 'string f ".cl"))))
  (compile-file "soap.cl")
  
  (with-open-file (out 
		   (ecase *current-case-mode*
		     (:case-sensitive-lower "soapm.fasl")
		     (:case-insensitive-upper "soapa.fasl"))
		   :element-type '(unsigned-byte 8)
		   :direction :output
		   :if-exists :supersede 
		   :if-does-not-exist :create)
    (dolist (file filenames)
      (with-open-file (in (concatenate 'string file ".fasl")
		       :element-type '(unsigned-byte 8))
        (format t "~%; ~s" file)
	(let ((buf (make-array 2048 :element-type '(unsigned-byte 8))))
	  (loop as x = (read-sequence buf in)
	      until (= x 0)
	      do (write-sequence buf out :end x)))))))
  
