
(in-package :user)
(require :xmpt-driver)

(defun xmeth325 ()
  (dolist (d (xmpt-decode-client)
	     (case *xmpt-syntax*
	       (:strict nil)
	       (otherwise (error "Service with no client functions."))))
    (and (consp (first d))
	 (eq 'defun (getf (cdr (first d)) :key))
	 (cdr d)
	 (case *xmpt-syntax*
	   (:strict (error "Detected incorrectly named messages"))
	   ))))

(xmpt-error (xmeth325) :matching ".*Service with no client functions.*")
