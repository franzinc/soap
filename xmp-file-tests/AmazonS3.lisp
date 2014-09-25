
(in-package :user)
(require :xmpt-driver)

(let ((wconn (xmpt-decode-file))
      (out (xmpt-out))
      (out2 (xmpt-out "out2"))
      )
  (xmpt-error (make-client-interface wconn 0 out :text-file nil)
	      :matching ".*type may be defined in included Schema.*"
	      :fail-info (xmpt-fail-info :make-client-no-import))
  )
(let ((wconn (xmpt-decode-file :include 'wsdl-include-file))
      (out3 (xmpt-out "out3"))
      (out4 (xmpt-out "out4"))
      )
  (xmptr
   (test-no-error (make-client-interface wconn 0 out3 
					 :prefix "as3a" :text-file nil)
		  :fail-info (xmpt-fail-info :make-client-3-import)))
  (xmptr
   (test-no-error (make-client-interface wconn 0 out4 
					 :prefix "as3b" :text-file nil
					 :object-class 'soap-object-class)
		  :fail-info (xmpt-fail-info :make-client-4-import)))
  (xmptr
   (test-no-error (load (compile-file out4))))
  (xmptr (test-no-error (new-status :code 17)))
  )




