;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA
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

;; $Id: soap.cl,v 1.1.1.1 2003/07/24 00:49:45 layer Exp $

;; Loader that picks the correct fasl for the current case-mode.

(in-package :user)

(load (ecase *current-case-mode*
	(:case-sensitive-lower "soapm.fasl")
	(:case-insensitive-upper "soapa.fasl")))

(provide :soap)
