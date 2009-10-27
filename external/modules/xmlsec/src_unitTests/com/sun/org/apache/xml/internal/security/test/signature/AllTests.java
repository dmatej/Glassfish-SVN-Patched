package com.sun.org.apache.xml.internal.security.test.signature;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

    public static Test suite() {
	TestSuite suite = new TestSuite(
			"Test for com.sun.org.apache.xml.internal.security.test.signature");
	//$JUnit-BEGIN$
	suite.addTest(CreateSignatureTest.suite());
	suite.addTestSuite(X509DataTest.class);
	suite.addTest(XMLSignatureInputTest.suite());
	suite.addTest(UnknownAlgoSignatureTest.suite());
	suite.addTest(KeyValueTest.suite());
	//$JUnit-END$
	return suite;
    }
}
