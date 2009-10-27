package com.sun.org.apache.xml.internal.security.test.utils.resolver;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests  {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.sun.org.apache.xml.internal.security.test.utils.resolver");
		//$JUnit-BEGIN$
		suite.addTest(ResourceResolverTest.suite());
		suite.addTestSuite(ResolverDirectHTTP.class);
		//$JUnit-END$
		return suite;
	}

}
