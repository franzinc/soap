# Allegro SOAP

Please refer to the
[documentation](https://franz.com/support/documentation/current/doc/soap.htm)
for details on the implementation.  This module uses two other modules
from Allegro Common Lisp: `:sax` and `:aserve`.  Without them this
package will not work.

See the file `LICENSE` for details on the license for this code.

We provide a loadable copy of the `:soap` module, but make no
guaranties other than that.

To build SOAP, evaluate `(load "build.cl")` twice, first in ANSI
(alisp), then in Modern (mlisp) Allegro CL images.  It will build
these files:

* `soap.fasl`: common loader stub must be compiled in Modern ACL
* `soapa.fasl`: ANSI version
* `soapm.fasl`: Modern version

The examples are:

* `soapex.cl`: client examples
* `soapval1.cl`: server example
* `bignum-server.cl`: server example and WSDL generation

You can run the tests by doing this:

    :cl xmp-test
    ;; comments in xmp-test.cl describe testing procedure...
