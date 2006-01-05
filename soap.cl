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

;; $Id: soap.cl,v 2.5 2006/01/05 22:46:06 layer Exp $

;; Loader that picks the correct fasl for the current case-mode.

#+(version= 7 0)
(sys:defpatch "soap" 2 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v1: bug fixes and improved handling of some SOAP messages."
  :type :system
  :post-loadable t)

#+(version= 6 2)
(sys:defpatch "soap" 4 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
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
