;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2003-2004 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: soap.cl,v 2.2 2004/02/13 05:35:28 layer Exp $

;; Loader that picks the correct fasl for the current case-mode.

#+ignore
(sys:defpatch "soap" 3 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v0: The Allegro SOAP client;
v1: SOAP server;
v2: WSDL input;
v3: parse wsdl definition more correctly."
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
