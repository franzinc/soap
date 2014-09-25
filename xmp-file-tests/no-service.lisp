
(in-package :user)
(require :xmpt-driver)

(let ((wconn (xmpt-decode-file))
      (out (xmpt-out)))
  (xmpt-error (make-client-interface wconn 0 out :text-file nil)
	      :matching ".*Cannot find service.*"
	      :fail-info (list :make-client *xmpt-wsdl* 0))
  (xmptr
   (test-no-error (make-client-interface wconn :none out :text-file nil)
		  :fail-info (list :make-client *xmpt-wsdl* :none))
   ))

