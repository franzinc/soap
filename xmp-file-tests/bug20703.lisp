(in-package :user)
(require :xmpt-driver)

;; bug20703   restriction on xsd:decimal translates to :any


(let ((conn (xmpt-decode-file))
      (out (xmpt-out)))
  (xmptr
   (make-client-interface conn :none out :text-file nil)
   (null (xmpt-find-lines out nil "(:seq* (:any))"))
   ))
