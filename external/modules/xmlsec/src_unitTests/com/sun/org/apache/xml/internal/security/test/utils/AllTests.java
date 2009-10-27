package com.sun.org.apache.xml.internal.security.test.utils;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.sun.org.apache.xml.internal.security.test.utils");
		//$JUnit-BEGIN$
		suite.addTest(IdResolverTest.suite());
		suite.addTest(Base64Test.suite());
		suite.addTestSuite(OldApiTest.class);
		//$JUnit-END$
		suite.addTest(com.sun.org.apache.xml.internal.security.test.utils.resolver.AllTests.suite());
		return suite;
	}

}
