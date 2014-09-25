
(in-package :user)
(require :xmpt-driver)

(let ((wconn (xmpt-decode-file))
      (out (xmpt-out)))
  (xmptr
   (test-no-error (make-client-interface wconn 0 out :text-file nil
					 :prefix *xmpt-name*)
		  :fail-info (list :make-client *xmpt-wsdl*))
   (test-no-error (load (compile-file out))
		  :fail-info (list :load-gen *xmpt-wsdl*))
   ))

