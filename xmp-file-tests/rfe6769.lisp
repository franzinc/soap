(in-package :user)
(require :xmpt-driver)

;; Document style WSDL with multi-part reply  [rfe6769]
;;  Generate client call with :body-form :many.

(let ((wconn (xmpt-decode-file))
      (out (xmpt-out)))
  (xmptr
   (test-no-error (make-client-interface wconn 0 out :text-file nil
					 :prefix *xmpt-name*)
		  :fail-info (list :make-client *xmpt-wsdl*))
   (test-no-error (load (compile-file out))
		  :fail-info (list :load-gen *xmpt-wsdl*))
   (test
    nil
    (not
     (stringp
      (xmpt-find-lines
       out nil "Send client message GetMultipartHTML" -10 ":body-form :many"))))
   ))

