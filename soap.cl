;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2003-2005 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

;; $Id: soap.cl,v 2.7 2006/01/18 21:07:23 mm Exp $

;; Loader that picks the correct fasl for the current case-mode.

#+(version= 8 0)
(sys:defpatch "soap" 1 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v1: improved handling of nillable and anyType."
  :type :system
  :post-loadable t)

#+(version= 7 0)
(sys:defpatch "soap" 4 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v1: bug fixes and improved handling of some SOAP messages;
v3: bring up to ACL 8.0 level;
v4: improved handling of nillable and anyType."
  :type :system
  :post-loadable t)

;; asof 2006-01-10 this patch level (5) has not been built or tested
;;  if there is a request for a 6.2 patch, this work will need to be done
#+(version= 6 2)(eval-when (compile load eval) (cerror "not ready for release"))
#+(version= 6 2)
(sys:defpatch "soap" 5 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v0: The Allegro SOAP client;
v1: SOAP server;
v2: WSDL input;
v3: parse WSDL definition more correctly;
v4: WSDL generation and lots of bug fixes."
  :type :system
  :post-loadable t)

(in-package :user)

(let ((module (ecase *current-case-mode*
	      (:case-sensitive-lower :soapm)
	      (:case-insensitive-upper :soapa))))
  (require module)
  (provide module)
  (provide :soap))

(provide :soap)
