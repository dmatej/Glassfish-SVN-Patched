
This document provides instructions on (1) how to use this module to build the
descriptors, (2) adding new descriptors and (3) adding new unit tests.

----

(1) Building and testing

In order to build the schemas, you need Apache Ant 1.7.0 or above.

Just running
  ant
will generate the schemas in the "build" directory and run the tests.

You can also perform the steps separately with
  ant build
and
  ant test
respectively.

(2) Adding a new deployment descriptor

Descriptor sources go in the "src" directory.
The file name for a descriptor source must end in ".xsds".
All files in "src" with a "xsds" extension will be considered descriptor
sources and will be processed by the build script.

Descriptor sources must be well-formed XML files.
Any XML comments (i.e. <!-- ... --> sections) will be stripped away by the
build script.

Descriptors can include text for common sections using XInclude.
Here's an example:

  <xsd:annotation>
    <xsd:documentation>
      <xi:include xmlns:xi="http://www.w3.org/2001/XInclude"
                  href="license.inc"
                  parse="text"/>
    </xsd:documentation>
  </xsd:annotation>

Note that the included files are included as text, not as XML. This is to
avoid extra namespace declarations in the generated descriptor.
By convention, the name of included files ends in ".inc". This further
highlights that they are not XML files
To allow licenses to appear in included files, the build scripts strip C-like
comment blocks (i.e. /* ... */ sections) from included files before passing
them on to XInclude.

There are four text files meant for inclusion:
  common.inc  -- contains a description of the conventions that apply to all
                 descriptors
  glossary.inc -- contains a description of some common terms used by several
                  descriptors
  license.inc -- contains the Sun license notice
  license-ibm.inc -- contains the IBM license notice

The best way to see how these rules/conventions work in practice is to look at
the existing descriptor source files.

The build script expands XInclude directives, performs some basic validation
of descriptor sources, checks for some stylistic conventions (like ensuring
any element declarations appear before type declarations) and finally
prettyprints the output.

Although the existing descriptor source files come formatted in a way that is
very close to the final output, the script performs quite extensive
reformatting. For example, this test.xsds file:

	<xsd:schema
	   targetNamespace="http://java.sun.com/xml/ns/javaee"
	   xmlns:javaee="http://java.sun.com/xml/ns/javaee"
	   xmlns:xsd="http://www.w3.org/2001/XMLSchema"
	   elementFormDefault="qualified" attributeFormDefault="unqualified"
	      version="5">
	  <xsd:annotation>
	    <xsd:documentation>$Id$</xsd:documentation>
	  </xsd:annotation>
	  <xsd:import namespace="http://www.w3.org/XML/1998/namespace"
	    schemaLocation="http://www.w3.org/2001/xml.xsd"/>
	  <xsd:include schemaLocation="javaee_web_services_client_1_2.xsd"/>
	  <xsd:group name="descriptionGroup">
	    <xsd:annotation>
	      <xsd:documentation>
	  All elements may occur multiple times with different languages,
	  to support localization of the content.
	      </xsd:documentation>
	    </xsd:annotation>
	    <xsd:sequence>
	      <xsd:element name="description" type="javaee:descriptionType"
	       minOccurs="0" maxOccurs="unbounded"/>
	      <xsd:element name="icon" type="javaee:iconType"
	       minOccurs="0" maxOccurs="unbounded"/>
	    </xsd:sequence>
	  </xsd:group>
	</xsd:schema>

results in this test.xsd output:

	<?xml version="1.0" encoding="UTF-8"?>
	<xsd:schema targetNamespace="http://java.sun.com/xml/ns/javaee"
	            xmlns:javaee="http://java.sun.com/xml/ns/javaee"
	            xmlns:xsd="http://www.w3.org/2001/XMLSchema"
	            elementFormDefault="qualified"
	            attributeFormDefault="unqualified"
	            version="5">
	  <xsd:annotation>
	    <xsd:documentation>

	      $Id$
	    </xsd:documentation>
	  </xsd:annotation>

	  <xsd:import namespace="http://www.w3.org/XML/1998/namespace"
	              schemaLocation="http://www.w3.org/2001/xml.xsd"/>

	  <xsd:include schemaLocation="javaee_web_services_client_1_2.xsd"/>

	  <xsd:group name="descriptionGroup">
	    <xsd:annotation>
	      <xsd:documentation>

	        All elements may occur multiple times with different languages,
	        to support localization of the content.
        
	      </xsd:documentation>
	    </xsd:annotation>
	    <xsd:sequence>
	      <xsd:element name="description"
	                   type="javaee:descriptionType"
	                   minOccurs="0"
	                   maxOccurs="unbounded"/>
	      <xsd:element name="icon"
	                   type="javaee:iconType"
	                   minOccurs="0"
	                   maxOccurs="unbounded"/>
	    </xsd:sequence>
	  </xsd:group>

	</xsd:schema>

(3) Adding new tests

Once your new descriptor is in place, you should add some tests.

Any file in the "test" directory whose name ends with ".xml" is considered a
test.

A test document is a descriptor to be validated against on the descriptor
schemas generated at build time.

To tell the build script which schema a test document you should add a
"validateAgainst" processing instruction at the end of the document. E.g.

  <?validateAgainst ejb-jar_3_0.xsd?>

If the test is a positive one, i.e. you expect validation to succeed, then you
don't need to do anything else.

If the test is a negative one, i.e. you expect some validation errors to be
thrown by the parser, then you must add at the end of the document one
"expectError" processing instruction for each error produced by the parser. E.g.

  <?expectError 62:cvc-identity-constraint.4.1?>

This PI means that we expect a "cvc-identity-constraint.4.1" error to be
raised on line 62 of the document. (Incidentally, the reason we recommend
adding PIs at the end of the document is that doing this won't change the line
numbers.)

Errors must be listed in the order they will be raised by the parser; in
particular errors must appear ordered by line number. E.g.

  <?expectError 62:cvc-identity-constraint.4.1?>
  <?expectError 118:cvc-identity-constraint.4.2.2?>
  <?expectError 137:cvc-identity-constraint.4.1?>
  <?expectError 149:cvc-identity-constraint.4.1?>
  <?expectError 180:cvc-enumeration-valid?>

Naturally, the best (or only) way to figure out what errors to expect is to
run "ant test" with the descriptor, take note of the error messages and then
add one expectError PI for each error.

For example, given the five PIs given above were added in response to these
error messages reported at test time:

[java] sax error: 62:cvc-identity-constraint.4.1: Duplicate unique value [ejb/OrderRef] declared for identity constraint "session-ejb-local-ref-name-uniqueness" of element "session".
[java] sax error: 118:cvc-identity-constraint.4.2.2: Duplicate key value [handler1] declared for identity constraint "service-ref_handler-name-key" of element "service-ref".
[java] sax error: 137:cvc-identity-constraint.4.1: Duplicate unique value [whatever/POPD] declared for identity constraint "session-resource-env-ref-uniqueness" of element "session".
[java] sax error: 149:cvc-identity-constraint.4.1: Duplicate unique value [jms/Stocks] declared for identity constraint "session-message-destination-ref-uniqueness" of element "session".
[java] sax error: 180:cvc-enumeration-valid: Value 'Undefined' is not facet-valid with respect to enumeration '[Transaction, Extended]'. It must be a value from the enumeration.

As you can see, it's a matter of copying and pasting the line number and error
id information from the console to the document.

(4) Any other questions?

Please let us know so we can update this document!
