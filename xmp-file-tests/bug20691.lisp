
(in-package :user)
(require :xmpt-driver)

;; From spr38758

(define-soap-type 
    nil 'multi_sale_params
  '(:complex
    (:set
     (:element ("payment_method")
      (:complex
       (:seq
        (:or
         (:element ("card_data")
                   (:complex
                    (:set
                     (:element ("card_number")
                               (:complex (:seq* (:any))))
                     (:element ("card_code")
                               (:complex (:seq* (:any))))
                     (:element ("expiration_month")
                               (:complex (:seq* (:any))))
                     (:element ("expiration_year")
                               (:complex (:seq* (:any))))
                     (:element ("issue_number")
                               (:complex (:seq* (:any))))
                     (:element ("name_on_card")
                               (:complex (:seq* (:any))))
                     (:element 
                      ("secure3d")
                      (:complex
                       (:set
                        (:element ("id_secure3d_auth")
                                  net.xmp.schema:|unsignedLong|)))))))
         (:element ("account_data")
                   (:complex
                    (:set
                     (:element ("account_country")
                               (:complex (:seq* (:any))))
                     (:element ("bank_code")
                               (:complex (:seq* (:any))))
                     (:element ("account_number")
                               (:complex (:seq* (:any))))
                     (:element ("account_holder")
                               (:complex (:seq* (:any)))))))))))
     (:element ("capture_later")
      net.xmp.schema:|boolean|)
     (:element ("fraud_check_on")
      net.xmp.schema:|boolean|)
     (:element ("avs_check_level")
      net.xmp.schema:|nonNegativeInteger|)
     (:element ("amount") (:complex (:seq* (:any))))
     (:element ("currency_code")
      (:complex (:seq* (:any))))
     (:element ("processing_date")
      (:complex (:seq* (:any))))
     (:element ("product")
      (:complex
       (:seq
        (:or
         (:element ("description")
                   (:complex (:seq* (:any)))))))))))

(define-soap-element nil 'multiSale
  '(:complex
    (:seq
     (:element ("params")
      multi_sale_params))
    :namespaces
    (nil
     (:paylane "tns"
      "http://www.paylane.com/Direct.wsdl"))
    :action
    "http://www.paylane.com/Direct.wsdl/multiSale"))

(defun paylane-multi-sale
    (&key payment_method capture_later fraud_check_on avs_check_level
     amount currency_code processing_date product)
  (let ((conn
          (soap-message-client :url ""
                               :start '(:basic-authorization
                                        ("test_billin" . "BiLLin4123"))
                               :lisp-package :keyword)))
    (multiple-value-call #'values
      (call-soap-method conn 'multiSale "params"
                        (list "payment_method" payment_method
                              "capture_later" capture_later
                              ;; "fraud_check_on" fraud_check_on
                              ;; "avs_check_level" avs_check_level
                              "amount" amount
                              "currency_code" currency_code
                              "processing_date" processing_date
                              "product" product))
      conn)))

(defparameter *test-payment*
	   '("card_data"
	     ("card_number" "4111111111111111" "card_code" "000" "expiration_year"
	      "2014" "expiration_month" "11" "name_on_card" "Slawomir Zak")))

(defun bug20691 ()
  (let* ((tpcopy (copy-tree *test-payment*))
	 (*soap-client-debug* :stop))
    (paylane-multi-sale
     :payment_method tpcopy
     :amount "96"
     :currency_code "PLN"
     :capture_later nil)
    tpcopy))

(xmptr (test *test-payment* (bug20691) :test #'equal))

;; This was result before bug fix.
#+ignore
(PAYLANE::|multiSaleResponse|
 (:|response| (:OK (:|id_sale| 2375315))
  (:DATA (:|fraud_score| 139/10))))
