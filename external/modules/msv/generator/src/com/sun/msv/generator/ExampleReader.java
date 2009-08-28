/*
 * @(#)$Id: ExampleReader.java 1478 2002-12-23 23:17:33Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.generator;

import java.util.Set;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

/**
 * collects all string literals appeared in the text.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public class ExampleReader extends DefaultHandler {
	
	private Set tokens;
	
	public ExampleReader( Set s ) {
		tokens = s;
	}
	
	public void characters( char[] buf, int start, int len ) {
		tokens.add( new String(buf,start,len) );
	}
	public void startElement( String uri, String local, String qname, Attributes at ) {
		int len = at.getLength();
		for( int i=0; i<len; i++ )
			tokens.add( at.getValue(i) );
	}
}
