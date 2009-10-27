package com.sun.org.apache.xml.internal.security.c14n.implementations;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.sun.org.apache.xml.internal.security.c14n.implementations");
		//$JUnit-BEGIN$
		suite.addTest(NameSpaceSymbTableTest.suite());
		suite.addTest(UtfHelperTest.suite());
		//$JUnit-END$
		return suite;
	}

}
