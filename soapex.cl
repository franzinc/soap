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

;; $Id: soapex.cl,v 2.0 2004/01/14 18:31:55 layer Exp $

;; SOAP client examples

(in-package :user)

(eval-when (compile load eval) (require :soap "soap.fasl"))

(defpackage :user (:use :net.xmp.soap)) 

(defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
(defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
(defpackage :net.xmp.soap.none (:use) (:nicknames :none))
(defpackage :net.xmp.soap.envelope (:use) (:nicknames :env))
(defpackage :net.xmp.soap.encoding (:use) (:nicknames :enc))



(defun sp01 ()

  ;; Sometimes fails: "getCurrentTime" not defined (may be server problem).

  (let ((conn (soap-message-client 
	       :url "http://time.soapware.org/currentTime"
	       :message-dns
	       (list nil
		     (list :net.xmp.soap.envelope
			   "SOAP-ENV"
			   "http://schemas.xmlsoap.org/soap/envelope/")
		     (list :net.xmp.soap.encoding
			   "SOAP-ENC"
			   "http://schemas.xmlsoap.org/soap/encoding/")
		     (list :net.xmp.schema
			   "xsd"
			   "http://www.w3.org/1999/XMLSchema")
		     (list :net.xmp.schema-instance
			   "xsi"
			   "http://www.w3.org/1999/XMLSchema-instance")
		     )
	       )))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element "getCurrentTime"
			(:complex (:seq)
				  :action "/currentTime"
				  ))))
      (list conn)))))

(defpackage :temp (:use) (:export "getTemp"))
(defun sp10 (&optional (zip "98325"))

  ;; http://www.xmethods.net/sd/2001/TemperatureService.wsdl

  (let ((conn (soap-message-client 
	       :lisp-package :keyword
	       :decode-flag nil
	       :url "http://services.xmethods.net:80/soap/servlet/rpcrouter")))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element temp:|getTemp|
			(:complex (:seq (:element "zipcode" xsd:|string|))
				  :action ""
				  :namespaces
				  (nil (:temp "tns" "urn:xmethods-Temperature"))
				  ))
	:|zipcode| zip
	))
      (list conn)))))





;; http://webservices.empowered.com/statsws/stats.asmx
;; http://www.xignite.com/xstatistics.asmx?WSDL

(defpackage :baseball (:use) (:export
			      "GetTeams"
			      "GetPlayers"
			      ))
(defun sp21 (&optional (encoding (list :utf8-base :utf-8)))

  (let ((conn (soap-message-client 
	       :url "http://webservices.empowered.com/statsws/stats.asmx"
	       :encoding-style nil
	       :xml-encoding encoding 
	       )))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element
	       baseball:|GetTeams|
	       (:complex
		(:seq)
		:action
		"http://webservices.empowered.com/StatsWS/DataService/GetTeams"
		:namespaces
		("http://webservices.empowered.com/StatsWS/DataService"
		  (:baseball
		   nil
		   "http://webservices.empowered.com/StatsWS/DataService" ))
		))))
      (list conn)))))

(defun sp22 (&optional (encoding (list :utf8-base :utf-8)))

  ;;  sending message nearly identical to sample on web

  (let ((conn (soap-message-client 
	       :url "http://webservices.empowered.com/statsws/stats.asmx"
	       :encoding-style nil
	       :xml-encoding encoding
	       )))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element
	       baseball:|GetPlayers|
	       (:complex
		(:seq)
		:action
		"http://webservices.empowered.com/StatsWS/DataService/GetPlayers"
		:namespaces
		("http://webservices.empowered.com/StatsWS/DataService"
		  (:baseball
		   nil
		   "http://webservices.empowered.com/StatsWS/DataService" ))
		))))
      (list conn)))))



;; http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl     

(defpackage :temp (:use) (:export "getRate" "Result" "getRateResponse"))
(define-soap-element nil
  'temp:|getRateResponse|
  '(:complex (:seq (:element "Result" xsd:|float|))))
(defun sp30 (&optional (country1 "Canada") (country2 "USA"))
  (let ((conn (soap-message-client 
	       :lisp-package :keyword
	       :url "http://services.xmethods.net:80/soap")))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element temp:|getRate|
			(:complex (:seq (:element "country1" xsd:|string|)
					(:element "country2" xsd:|string|)
					)
				  :action ""
				  :namespaces
				  (nil (:temp "tns" "urn:xmethods-CurrencyExchange"))
				  ))
	:|country1| country1 :|country2| country2
	))
      (list conn)))))




;; http://arcweb.esri.com
;; http://arcweb.esri.com/services/v2/RouteFinder.wsdl     

(defpackage :temp (:use) (:export "getVersion" "Result"))
(define-soap-element nil "getVersionResponse"
  '(:complex (:seq (:element "Result" xsd:|string|))))
(defun sp40 ()
  (let ((conn (soap-message-client 
	       :lisp-package :keyword
	       :url "http://arcweb.esri.com/services/v2/RouteFinder")))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element temp:|getVersion|
			(:complex (:seq )
				  :action "getVersion"
				  :namespaces
				  (nil (:temp "tns" "http://arcweb.esri.com/v2"))
				  ))
	))
      (list conn)))))





;; http://icuisine.net/webservices/RecipeService.asmx

(defpackage :temp (:use) (:export "SearchRecipes" "SearchRecipesResponse"
				  "SearchRecipesResult"
				  "TotalCount" "PageSice" "Recipes" "item"
				  "Name" "ID" "Servings" "Ingredients"
				  "GetRecipe"
				  ))
(define-soap-element nil
  'temp:|SearchRecipesResponse|
  '(:complex
    (:seq (:element "SearchRecipesResult"
		    (:complex
		     (:seq 
		      (:element "TotalCount" xsd:|int|)
		      (:element "PageSize"   xsd:|int|)
		      (:element
		       "Recipes"
		       (:complex
			(:seq* (:element "item"
					 (:complex
					  (:seq
					   (:element "Name" xsd:|string|)
					   (:element "ID"   xsd:|string|)
					   (:element "Servings" xsd:|int|)
					   "Ingredients"
					   ))))))))))))
(define-soap-element nil "Ingredients" '(:complex (:seq* (:any))))
(defun sp51 (&optional (criteria "fried eggplant") (pg 0))
  (let ((conn (soap-message-client 
	       :lisp-package :keyword
	       :xml-encoding (list :utf8-base :utf-8)
	       :url "http://icuisine.net/webservices/RecipeService.asmx")))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element temp:|SearchRecipes|
			(:complex (:seq (:element "criteria" xsd:|string|)
					(:element "pageNumber" xsd:|int|)
					(:element "serviceID" xsd:|int|)
					(:element "email" xsd:|string|)
					)
				  :action
				  "http://www.icuisine.net/webservices/SearchRecipes"
				  :namespaces
				  ("http://www.icuisine.net/webservices"
				   (:temp 
				    nil
				    "http://www.icuisine.net/webservices"))
				  ))
	:|criteria| criteria :|pageNumber| pg :|serviceID| 0 :|email| "foo"
	))
      (list conn)))))

(defun sp52 (id)
  (let ((conn (soap-message-client 
	       :lisp-package :keyword
	       :xml-encoding (list :utf8-base :utf-8)
	       :url "http://icuisine.net/webservices/RecipeService.asmx")))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method
	conn '(:element temp:|GetRecipe|
			(:complex (:seq (:element "guid" xsd:|string|)
					(:element "serviceID" xsd:|int|)
					(:element "email" xsd:|string|)
					)
				  :action
				  "http://www.icuisine.net/webservices/GetRecipe"
				  :namespaces
				  ("http://www.icuisine.net/webservices"
				   (:temp 
				    nil
				    "http://www.icuisine.net/webservices"))
				  ))
	:guid id :serviceID 0 :email "foo"
	))
      (list conn)))))




;;; Google API

(defvar *google-key*)
;; The file googlekey.cl is assumed to contain the forms:
;;     (in-package :user)
;;     (setf *google-key* "google-user-key")
;; The Google user key is obtained from http://www.google.com/apis/index.html
;;     after a simple registration process.

(when (probe-file "googlekey.cl") (load "googlekey.cl"))

(defpackage :gg
  (:use)
  (:export "doGoogleSearch"
	   "doGoogleSearchResponse"
	   "doSpellingSuggestion"
	   "doSpellingSuggestionResponse"
	   "doGetCachedPage"
	   "doGetCachedPageResponse" 
	   "ResultElement"
	   "DirectoryCategory"
	   "GoogleSearchResult"
	   ))

(define-soap-element nil 'gg:|doGoogleSearch|
  '(:complex
    (:seq1
     (:element :|key|        xsd:|string|  :send-type t)
     (:element :|q|          xsd:|string|  :send-type t)
     (:element :|start|      xsd:|int|     :send-type t)
     (:element :|maxResults| xsd:|int|     :send-type t)
     (:element :|filter|     xsd:|boolean| :send-type t)
     (:element :|restrict|   xsd:|string|  :send-type t)
     (:element :|safeSearch| xsd:|boolean| :send-type t)
     (:element :|lr|         xsd:|string|  :send-type t)
     (:element :|ie|         xsd:|string|  :send-type t)
     (:element :|oe|         xsd:|string|  :send-type t)
     )
    :action "urn:GoogleSearchAction"
    :namespaces (nil (:gg "gg" "urn:GoogleSearch"))
    ))
(define-soap-element nil 'gg:|doSpellingSuggestion|
  '(:complex
    (:seq1
     (:element :|key|        xsd:|string|  :send-type t)
     (:element :|phrase|     xsd:|string|  :send-type t)
     )
    :action "urn:GoogleSearchAction"
    :namespaces (nil (:gg "gg" "urn:GoogleSearch"))
    ))
(define-soap-element nil 'gg:|doGetCachedPage|
  '(:complex
    (:seq1
     (:element :|key|        xsd:|string|  :send-type t)
     (:element :|url|        xsd:|string|  :send-type t)
     )
    :action "urn:GoogleSearchAction"
    :namespaces (nil (:gg "gg" "urn:GoogleSearch"))
    ))
(define-soap-element nil 'gg:|doGoogleSearchResponse|
  '(:complex (:seq (:element "return" gg:|GoogleSearchResult|))))
(define-soap-type nil 'gg:|GoogleSearchResult| '(:complex (:seq* (:any))))
(define-soap-type nil 'gg:|ResultElement| '(:complex (:seq* (:any))))
(define-soap-type nil 'gg:|DirectoryCategory| '(:complex (:seq* (:any))))
(defun gs (&optional (q "AllegroCL"))
  (let ((conn (soap-message-client 
	       :url "http://api.google.com/search/beta2"
	       :lisp-package :keyword
	       )))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method conn
			'gg:|doGoogleSearch|
			:|key| *google-key*
			:|q|   q
			:|start| 0
			:|maxResults| 10
			:|filter| "true"
			:|safeSearch| "true"
			:|ie| "latin1"
			:|oe| "latin1"
			))
      (list conn)))))


(defun gsp (&optional (phrase "common lisp s-expresion"))
  (let ((conn (soap-message-client 
	       :url "http://api.google.com/search/beta2"
	       :lisp-package :keyword
	       )))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method conn
			'gg:|doSpellingSuggestion|
			:|key| *google-key*
			:|phrase| phrase
			))
      (list conn)))))


(defun gcp (&key (url "http://www.franz.com"))
  (let ((conn (soap-message-client 
	       :url "http://api.google.com/search/beta2"
	       :lisp-package :keyword
	       )))
    (values-list
     (append
      (multiple-value-list
       (call-soap-method conn
			'gg:|doGetCachedPage|
			:|key| *google-key*
			:|url| url
			))
      (list conn)))))



(define-soap-element nil "testServer"
  '(:complex (:seq* (:any))
	     :action "ACLSOAP"
	     ))

(defun simple-server (&key (port 4567)
			   (server-action "ACLSOAP")
			   (method-action :default)
			   (client-action server-action) 
			   )
  (let ((host "localhost")
	(path "/ACL-SOAP")
	)
    (let ((s (soap-message-server
	      :start (list :port port) :enable :start
	      :publish `(:path ,path)
	      :action server-action
	      :lisp-package :keyword
	      )))

      (soap-export-method s "testServer" (list :|a| :|b| :|c|)
			  :lisp-name 'simple-server-1
			  :action method-action
			  :return '(:element "sResponse" xsd:|string|))
      (sleep 1))
    
    (let ((c (soap-message-client :url (format nil "http://~A:~A~A"
					       host port path)
				  :lisp-package :keyword
				  )))
      (equal
       '(:|sResponse| "123")
       (call-soap-method c 
			 `(:element "testServer"
				    (:complex (:seq* (:any))
					      :action ,client-action
					      ))  
			 "a" 1 "b" 2 "c" 3)))))

(defun simple-server-1 (&key |a| |b| |c|) (concatenate 'string |a| |b| |c|))

