;; -*- mode: common-lisp; package: user -*-
;;
;; See the file LICENSE for the full license governing this code.


;; Loader that picks the correct fasl for the current case-mode.
;; THE BUILD SCRIPT IS IN build.cl.

#+(version= 10 0)
(sys:defpatch "soap" 1
  "v1: Avoid error when WSDL specifies an empty body."
  :type :system
  :post-loadable t)

#+(version= 9 0)
(sys:defpatch "soap" 4
  "v4: Avoid error when WSDL specifies an empty body;
v3: generate zero arg calls from WSDL;
v2: avoid no-applicable-method error for schema-raw-attribute;
v1: decode double-float zero, suppress whitespace, include/import option fixes."
  :type :system
  :post-loadable t)

#+(version= 8 2)
(sys:defpatch "soap" 4 
  "v4: Avoid error when WSDL specifies an empty body;
v3: generate zero arg calls from WSDL;
v2: avoid no-applicable-method error for schema-raw-attribute;
v1: decode double-float zero, suppress whitespace, include/import option fixes."
  :type :system
  :post-loadable t)


#+(version= 8 1)
(sys:defpatch "soap" 3 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v3: xmldecl optional, quote soapaction, no accessors for trivial types;
v2: encode and decode float values correctly;
v1: soap server returns wsdl if requested."
  :type :system
  :post-loadable t)

#+(version= 8 0)
(sys:defpatch "soap" 5 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v1: improved handling of nillable and anyType
v2: <include> handling and many other extensions;
v3: generate check-type with quoted typespec, message has one header element
    with one or more parts, WSDL simpleContent with extension generates
    garbled def
v4: encode and decode float values correctly;
v5:xmldecl optional, quote soapaction, no accessors for trivial types."
  :type :system
  :post-loadable t)

#+(version= 7 0)
(sys:defpatch "soap" 6 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v1: bug fixes and improved handling of some SOAP messages;
v3: bring up to ACL 8.0 level;
v4: improved handling of nillable and anyType
v5: <include> handling and many other extensions
v6: encode and decode float values correctly."
  :type :system
  :post-loadable t)

;; asof 2006-01-10 this patch level (5) has not been built or tested
;;  if there is a request for a 6.2 patch, this work will need to be done
#+(and (version>= 6 2) (not (version>= 7)))
(eval-when (compile load eval) (cerror "not ready for release"))
#+(and (version>= 6 2) (not (version>= 7)))
(sys:defpatch "soap" 5 ;;; ALSO CHANGE: incf the # in build.cl & Makefile
  "v0: The Allegro SOAP client;
v1: SOAP server;
v2: WSDL input;
v3: parse WSDL definition more correctly;
v4: WSDL generation and lots of bug fixes."
  :type :system
  :post-loadable t)

(in-package :user)

#+soap-two-fasls
(let ((module (ecase *current-case-mode*
	      (:case-sensitive-lower :soapm)
	      (:case-insensitive-upper :soapa))))
  (require module)
  (provide module)
  (provide :soap))

(provide :soap)
