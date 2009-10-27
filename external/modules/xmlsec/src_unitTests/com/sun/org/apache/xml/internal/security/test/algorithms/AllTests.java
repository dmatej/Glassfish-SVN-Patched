package com.sun.org.apache.xml.internal.security.test.algorithms;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.sun.org.apache.xml.internal.security.test.algorithms");
		//$JUnit-BEGIN$
		suite.addTest(SignatureAlgorithmTest.suite());
		//$JUnit-END$
		return suite;
	}

}
