/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import com.sun.appserv.management.DomainRoot;


/**
    Write your own code, then run it in the command line eg:
    <p>
    <code>Enter Command > mystuff.MyCmd [args]</code>
    
    Implement the runCmd() method any way you like.
 */
public class CmdTemplate
{
		public 
	CmdTemplate()
	{
	}
	
	    protected String
	argsToString(final String[] args)
	{
	    final StringBuilder builder = new StringBuilder();
	    
	    final String NEWLINE    = System.getProperty( "line.separator" );
	    
	    builder.append( this.getClass().getName() + ".runCmd()" );
	    builder.append( NEWLINE );
	    for( int i = 0; i < args.length; ++i )
	    {
	        builder.append( "arg[" + i + "] = "  + args[i] );
	        builder.append( NEWLINE );
	    }
	    
	    return builder.toString();
	}
	
	/**
	    args[0] is the class name
	 */
	    public Object
	runCmd(
	    final DomainRoot    domainRoot,
	    final String[]      args )
	{
	    return argsToString( args );
	}
}




