(in-package :user)
(require :xmpt-driver)

;; bug17080 wsdl document style reply may be a simple type

(let ((wconn (xmpt-decode-file))
      (out2 (xmpt-out "out2"))
      )
  (xmptr
   (test-no-error (make-server-interface wconn 0 out2 :text-file nil
					 :prefix *xmpt-name*)
		  :fail-info (list :make-client *xmpt-wsdl*))
   (test-no-error (load (compile-file out2))
		  :fail-info (list :load-gen *xmpt-wsdl*))
   
   ))

