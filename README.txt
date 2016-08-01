

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

 to run examples and tests:

 in main folder:

	start ACL
	:cl xmp-test
	;; comments in xmp-test.cl describe testing procedure...


-------------------------------------------

RELEASED FILES:

  in ACL/code/
    soap.fasl    -- common loader stub must be compiled in Modern ACL
    soapa.fasl   -- ANSI version
    soapm.fasl   -- Modern version

  in ACL/examples/soap/
    soapex.cl          -- client examples
    soapval1.cl        -- server example
    bignum-server.cl   -- server example and WSDL generation


