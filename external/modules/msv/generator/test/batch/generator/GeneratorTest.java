/*
 * @(#)$Id: GeneratorTest.java 1478 2002-12-23 23:17:33Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package batch.generator;

import junit.framework.TestSuite;

/**
 * batch test class that is called from ant as a part of the whole test.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public class GeneratorTest {
	
	public static void main( String[] args ) throws Exception {
		junit.textui.TestRunner.run(GeneratorTest.suite());
	}
	
	public static TestSuite suite() throws Exception {
		TestSuite s = new TestSuite();

		append( s, "RELAXBatchTestDir", "relax" );
		append( s, "TREXBatchTestDir", "trex" );
		append( s, "XSDBatchTestDir", "xsd" );
		append( s, "DTDBatchTestDir", "dtd" );
		return s;
	}
	
	private static void append( TestSuite s, String propName, String target ) throws Exception {
		s.addTest( new GeneratorTester().createFromProperty(target,propName) );
	}
}
