/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import java.util.Map;
import java.util.HashMap;

import com.sun.appserv.management.DomainRoot;
import com.sun.appserv.management.base.Util;


import com.sun.appserv.management.config.DomainConfig;
import com.sun.appserv.management.config.StandaloneServerConfig;
import com.sun.appserv.management.config.NodeAgentConfig;
import com.sun.appserv.management.config.ConfigConfig;


/**
 */
public final class Demo extends CmdTemplate
{
    DomainRoot  mDomainRoot;
    
		public 
	Demo()
	{
	}
	
		private void
	require( final boolean	test )
	    throws UsageException
	{
		if ( ! test )
		{
		    printUsage();
			throw new UsageException( USAGE );
		}
	}
	
	private static final String INDENT = "  ";
	private static final String CREATE_INSTANCE = "create-instance";
	private static final String REMOVE_INSTANCE = "remove-instance";
	
	private static final String USAGE   =
	    "USAGE:\n" +
	    "Demo <sub-cmd> [args]\n" +
	    INDENT + CREATE_INSTANCE + " <name> <node-agent-name> <baseport>" + "\n" +
	    INDENT + REMOVE_INSTANCE + " <name>" + "\n" +
	    "";
	
	    private void
	printUsage()
	{
	    println( USAGE );
	}
	
	    private void
	println( final String msg)
	{
	    System.out.println( "" + msg );
	}
	
	static private final class UsageException extends Exception
	{
	    public static final long serialVersionUID    = 13456789;
	    public UsageException( final String msg )   { super( msg ); }
	}
	
	    private void
	error( final String msg)
	{
	    println( "ERROR: " + msg );
	}
	
	    private void
	nyi( final String subCmd )
	{
	    println( "Not yet implemented: " + subCmd );
	}
	
	/**
	    args[0] is the class name
	 */
	    public Object
	runCmd(
	    final DomainRoot    domainRoot,
	    final String[]      args )
	{
	    mDomainRoot = domainRoot;
	    
	    final int numArgs   = args.length;
	    if ( numArgs == 1 )
	    {
	        printUsage();
	        return null;
	    }
	    
	    Object  result  = null;
	    
	    final String cmd    = args[ 0 ];
	    final String subCmd = args[ 1 ];
	    
	    final int argStart    = 2;
	    try
    	{
    	    if ( subCmd.equals( CREATE_INSTANCE ) )
    	    {
    	        require( numArgs == 2+3 );
    	        final StandaloneServerConfig serverConfig   =
        	        createInstance(
        	            args[argStart],
        	            args[argStart+1],
        	            Integer.parseInt( args[argStart+2] ) );
        	    if ( serverConfig != null )
        	    {
        	        result  = "Instance created successfully: " +
        	                        Util.getObjectName( serverConfig );
        	    }
    	    }
    	    else if ( subCmd.equals( REMOVE_INSTANCE ) )
    	    {
    	        result = removeInstance( args[argStart] );
    	    }
    	    else
    	    {
    	        printUsage();
    	    }
    	 }
    	 catch( final UsageException e )
    	 {
    	    result  = null;
    	 }
	    
	    return result;
	}
	
	    private DomainConfig
	getDomainConfig()
	{
	    return mDomainRoot.getDomainConfig();
	}
	
	    private StandaloneServerConfig
	createInstance(
	    final String instanceName,
	    final String nodeAgentName,
	    final int    basePort )
	{
	    if ( getDomainConfig().getStandaloneServerConfigMap().get( instanceName ) != null )
	    {
	        error( "Server " + instanceName + " already exists." );
	        return null;
	    }
	    
        if ( getDomainConfig().getNodeAgentConfigMap().get( nodeAgentName )== null )
        {
            error( "Node agent does not exist: " + nodeAgentName);
	        return null;
        }

        final String configName    = instanceName + "-config";
	    println( "Creating instance " + instanceName +
	        " using node agent " + nodeAgentName +
	        " with base port " + basePort +
	        " and config " + configName );
        
        final ConfigSetup setup = new ConfigSetup( mDomainRoot );
        ConfigConfig    config  = getDomainConfig().getConfigConfigMap().get( configName );
        if ( config == null )
        {
            config  = setup.createConfig( configName );
        }
        
        final StandaloneServerConfig  serverConfig    =
                setup.createServer( instanceName, basePort, nodeAgentName, config.getName() );
        
        return serverConfig;
	}
	
	    private String
	removeInstance( final String instanceName )
	{
	    String result   = null;
	    
	    if ( getDomainConfig().getStandaloneServerConfigMap().get( instanceName ) != null )
	    {
	        getDomainConfig().removeStandaloneServerConfig( instanceName );
	        result  = "Instance " + instanceName + " removed.";
	    }
	    return result;
	}
}

















