
;; $Id: README.txt,v 2.0 2004/01/14 18:31:55 layer Exp $

 The SOAP module requires xmlutils version 7.0.1 which has been
 committed on the trunk.  It will signal error if the wrong xmlutils
 is loaded.

 STEP 1 - build and/or load the right xmlutils

 STEP 2 - build soap.fasl for desired host

 in main folder:

	start ACL
	:ld build

 This step should be done twice, first in ANSI, then in Modern image.

 STEP 3 - try the examples (with an internet connection)

 to run examples:

 in main folder:

	start ACL
	:cl soapex


	(sp01)    --- this call fails even though we send the exact message
                      string in the sample message

	(sp10 [zipcode-string])   --- works sometimes

	(sp21)    --- returns an empty result
        (sp22)    --- returns an empty result
	
	(sp30 [country1 [country2]])   --- returns exchange rate

	(sp40)    --- return version string

	(sp51 [search-words-string [page-number]]) --- return array of recipes
	(sp52 recipe-id)   --- return recipe text and data

	(gs [search-string])   --- returns array of results

	(gsp [phrase-string])   --- returns spelling correction

	(gcp [url-string])    --- returns cached page text


 STEP 4 - try your own examples

 read soap-client.txt
 look at examples in soapex.cl


-------------------------------------------

RELEASED FILES:

    soap.fasl    -- common loader stub must be compiled in Modern ACL
    soapa.fasl   -- ANSI version
    soapm.fasl   -- Modern version

    soapex.cl    -- client examples
    soapval1.cl  -- server example



