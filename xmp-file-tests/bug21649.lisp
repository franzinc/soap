
(in-package :user)
(require :xmpt-driver)

;; bug21649  30    4 encode-wsdl-file cannot handle nested collectors

(define-soap-type nil :nested
  '(:complex
      (:seq1
         (:element "id" xs:string)
         (:or
            (:element "children" xs:string)
            (:element "verifications" xs:string)
            ))))

(let ((out (xmpt-out)))
  (xmptr
   (test-no-error (encode-wsdl-file out :types '(:nested)))
   ))


