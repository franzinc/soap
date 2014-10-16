
(in-package :user)
(require :xmpt-driver)

(let ((wconn (xmpt-decode-file))
      (out (xmpt-out)))
  (xmptr
   (xmpt-error (make-client-interface wconn 0 out :text-file nil)
	       :matching ".*Cannot find service.*"
	       :fail-info (list :make-client *xmpt-wsdl* 0))
   (test-no-error (make-client-interface wconn :none out :text-file nil)
		  :fail-info (list :make-client *xmpt-wsdl* :none))
   ))

