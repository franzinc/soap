(in-package :user)
(require :xmpt-driver)

;; bug21752  WSDL decoder is confused when SOAP message is a simple type

(let ((wconn (xmpt-decode-file))
      (out (xmpt-out "out"))
      )
  (xmptr
   (test-no-error (make-client-interface wconn 0 out :text-file nil
					 :prefix *xmpt-name*)
		  :fail-info (list :make-client *xmpt-wsdl*))
   (multiple-value-bind (v e)
       (ignore-errors (values (load (compile-file out))))
     (test nil e :fail-info (list :load-gen *xmpt-wsdl*)))
   )
  (let ((*soap-client-debug* :stop))
    (multiple-value-bind (a b conn)
	(bug217521 :|psaModelId| 123)
      (setq a (net.xml.sax:parse-to-lxml (net.xmp:xmp-message-string conn)))
      (setq a (first a))   ;;; get Envelope
      (setq a (second a))  ;;; get Body
      (setq a (second a))  ;;; get psaModelId element
      (setq b (first a))
      (xmptr
       (test "psaModelId" (string (first b)) :test 'equal)
       (test "123" (second a) :test 'equal)
       )))
  )

