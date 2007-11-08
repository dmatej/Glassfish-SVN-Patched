/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import java.io.File;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Properties;
import java.net.ConnectException;

import java.lang.reflect.Method;

import java.security.cert.CertificateException;

import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnector;
import javax.management.InstanceNotFoundException;

import com.sun.appserv.management.DomainRoot;

import com.sun.appserv.management.base.AMX;
import com.sun.appserv.management.base.XTypes;

import com.sun.appserv.management.client.TLSParams;
import com.sun.appserv.management.client.TrustStoreTrustManager;
import com.sun.appserv.management.client.AppserverConnectionSource;
import com.sun.appserv.management.client.HandshakeCompletedListenerImpl;

import com.sun.appserv.management.util.misc.StringUtil;
import com.sun.appserv.management.util.misc.ExceptionUtil;
import com.sun.appserv.management.util.misc.ArrayUtil;

import com.sun.appserv.management.util.stringifier.StringifierRegistryImpl;
import com.sun.appserv.management.util.stringifier.SmartStringifier;
import com.sun.appserv.management.util.stringifier.SmartStringifier;

import com.sun.appserv.management.util.jmx.stringifier.StringifierRegistryIniter;

/**
	Main class demonstrating a variety of MBean API (AMX) usages.
	Enters an interactive loop in which the user can run various commands.
 */
public final class SampleMain
{
	private final DomainRoot				mDomainRoot;
	private HandshakeCompletedListenerImpl	mHandshakeCompletedListener;
	
		public static void
	main( final String[] args )
	{
		if ( args.length > 1 )
		{
			SampleUtil.println( "Specify a properties file or nothing." );
			System.exit( 255 );
		}
		new StringifierRegistryIniter( StringifierRegistryImpl.DEFAULT );
		
		checkForMissingClasses();
		
		try
		{
			new SampleMain( args.length == 1 ? args[ 0 ] : "SampleMain.properties" );
		}
		catch( final Throwable t )
		{
		    final Throwable rootCause   = ExceptionUtil.getRootCause( t );
		    
		    if ( rootCause instanceof ConnectException )
		    {
		        SampleUtil.println( "Unable to connect to specified host and port." );
		    }
		    else
		    {
			    rootCause.printStackTrace();
			}
		}
	}
	
		private static void
	checkForMissingClasses()
	{
		try
		{
		    // verify presence of j2ee classes
		    Class.forName( "javax.management.j2ee.statistics.Statistic" );
		}
		catch( Throwable t )
		{
			System.err.println(
			    "WARNING: Can't find j2ee Statistic classes (some functionality won't work)" );
			    
			System.err.println("Please verify that 'javaee.jar' is in the classpath." );
			System.err.println();
		}
	}
	
	
	private static final String	QUIT			= "quit";
	private static final String	HELP			= "help";
	private static String	LIST			= "list";
	private static String	DEPLOY			= "deploy";
	private static String	UNDEPLOY		= "undeploy";
	private static String	QUERY			= "query";
	private static String	SHOW_HIERARCHY	= "show-hierarchy";
	private static String	START_SERVER	= "start-server";
	private static String	STOP_SERVER		= "stop-server";
	private static String	LIST_ATTRIBUTES	= "list-attributes";
	private static String	LIST_DOTTED_NAMES	= "list-dotted-names";
	private static String	LIST_PROPERTIES	= "list-properties";
	private static String	SET_MONITORING	= "set-monitoring";
	private static String	DEMO_JMX_MONITOR	= "demo-jmx-monitor";
	private static String	DISPLAY_LOGGING	= "display-logging";
	private static String	LISTEN_TO_LOGGING	= "listen-to-logging";
	private static String	RUN_ALL			= "run-all";
	
	private static final String[]	MENU_CHOICES	= new String[]
	{
		DEMO_JMX_MONITOR, DEPLOY, HELP, 
		LIST_ATTRIBUTES, LIST, LIST_DOTTED_NAMES, LIST_PROPERTIES,
		LISTEN_TO_LOGGING, DISPLAY_LOGGING,
		QUERY, QUIT, RUN_ALL,
		START_SERVER, STOP_SERVER,
		SHOW_HIERARCHY, SET_MONITORING, UNDEPLOY
	};
	
	
	final String[]	RUN_ALL_CMDS	= new String[]
	{
		DEMO_JMX_MONITOR, LIST_ATTRIBUTES, LIST, LIST_DOTTED_NAMES, LIST_PROPERTIES, QUERY,
		SHOW_HIERARCHY, LISTEN_TO_LOGGING, DISPLAY_LOGGING
	};
	
	final String MENU	= SampleUtil.arrayToString( MENU_CHOICES, "  ", "\n");
	final String COMMANDS	= "COMMANDS:\n" + MENU;
	final String PROMPT	=  "Enter command> ";

		private void
	require(
		final boolean	test,
		final String	msg )
	{
		if ( ! test )
		{
			SampleUtil.println( msg );
			throw new IllegalArgumentException();
		}
	}
	
		private void
	handleChoice(
		final Samples	samples,
		final String	line )
		throws IOException, InstanceNotFoundException
	{
		final String[]	parts	= line.split( "[ \t]+" );
		final int		numArgs	= parts.length - 1;
		final String	cmd	= parts[ 0 ];
		
		final String[] args    = ArrayUtil.newArray( parts, 1, numArgs );
		
		if ( cmd.length() != 0 )
		{
			SampleUtil.println( "cmd: " + SampleUtil.arrayToString( parts, " ", "" ) );
		}
		
		if ( cmd.equals( QUIT ) || cmd.equals( "q" ) )
		{
			require( numArgs == 0, "Usage: " + QUIT );
			System.exit( 0 );
		}
		if ( cmd.equalsIgnoreCase( HELP ) || cmd.equals( "?" ) )
		{
			SampleUtil.println( COMMANDS );
		}
		else if ( cmd.length() == 0 )
		{
			// do nothing
		}
		else if ( cmd.equals( DEPLOY ) )
		{
			require( numArgs >= 1, "Usage: " + DEPLOY + " <archive-name>" );
			for( int i = 1; i < parts.length; ++i )
			{
				samples.deploy( new File( parts[ i ] ) );
			}
		}
		else if ( cmd.equals( UNDEPLOY ) )
		{
			require( numArgs >= 1, "Usage: " + UNDEPLOY + " [<name>[ <name>]*]" );
			for( int i = 1; i < parts.length; ++i )
			{
				samples.undeploy( parts[ i ] );
			}
		}
		else if ( cmd.equals( START_SERVER ) )
		{
			require( numArgs == 1, "Usage: " + START_SERVER + " <server-name>" );
			samples.startServer( parts[ 1 ] );
		}
		else if ( cmd.equals( STOP_SERVER ) )
		{
			require( numArgs == 1, "Usage: " + STOP_SERVER + " <server-name>" );
			samples.stopServer( parts[ 1 ] );
		}
		else if ( cmd.equals( LIST ) )
		{
			require( numArgs == 0, "Usage: " + LIST );
			samples.handleList();
		}
		else if ( cmd.equals( SHOW_HIERARCHY ) )
		{
			if ( numArgs == 0 )
			{
				samples.displayHierarchy( false );
			}
			else
			{
			    int count   = parts.length;
			    final String last   = parts[ count - 1];
			    
			    boolean showAttrs   = false;
			    if ( last.equalsIgnoreCase( "true" ) ||
			            last.equalsIgnoreCase( "false" ) )
			    {
			        SampleUtil.println( "XXX" );
			        showAttrs   = Boolean.valueOf( last );
			        --count;
			    }
			    
			    if ( count >= 2 )
			    {
    				for( int i = 1; i < count; ++i )
    				{
    					samples.displayHierarchy( parts[ i ], showAttrs );
    				}
				}
				else
				{
				    samples.displayHierarchy( showAttrs );
				}
			}
		}
		else if ( cmd.equals( QUERY) )
		{
			require( numArgs == 0, "Usage: " + QUERY );
			
			samples.demoQuery();
		}
		else if ( cmd.equals( LIST_ATTRIBUTES ) )
		{
			if ( numArgs == 0 )
			{
				samples.displayAllAttributes( getDomainRoot() );
			}
			else
			{
				for( int i = 1; i < parts.length; ++i )
				{
					samples.displayAllAttributes( parts[ i ] );
				}
			}
		}
		else if ( cmd.equals( LIST_DOTTED_NAMES ) )
		{
			require( numArgs == 0, "Usage: " + LIST_DOTTED_NAMES );
			samples.displayDottedNames( );
		}
		else if ( cmd.equals( LIST_PROPERTIES ) )
		{
			require( numArgs == 0, "Usage: " + LIST_PROPERTIES );
			samples.displayAllProperties( );
		}
		else if ( cmd.equals( RUN_ALL ) )
		{
			require( numArgs == 0, "Usage: " + RUN_ALL );
			
			for( int i = 0; i < RUN_ALL_CMDS.length; ++i )
			{
				final String	choice	= RUN_ALL_CMDS[ i ];
				
				handleChoice( samples, choice );
			}
		}
		else if ( cmd.equals( DEMO_JMX_MONITOR ) )
		{
			samples.demoJMXMonitor();
		}
		else if ( cmd.equals( SET_MONITORING ) )
		{
			require( numArgs == 2, "Usage: " + SET_MONITORING + " <config-name> HIGH|LOW|OFF" );
			
			samples.setMonitoring( args[ 0 ], args[ 1 ]);
		}
		else if ( cmd.equals( DISPLAY_LOGGING ) )
		{
			samples.displayLogging( args );
		}
		else if ( cmd.equals( LISTEN_TO_LOGGING ) )
		{
			require( numArgs == 1, "Usage: " + LISTEN_TO_LOGGING + " true|false [server]*" );
			
			final String[] serverNames  = ArrayUtil.newArray( args, 1, args.length - 1 );
			samples.listenToLogging( new Boolean( args[ 0 ] ), serverNames );
		}
		else
		{
		    handleUnknownCmd( parts );
		}
	}
	
	
	    private void
    handleUnknownCmd( final String[] parts )
    {
		final String	cmd	= parts[ 0 ];
	    final Class[]    sig    = new Class[]
	    {
	        DomainRoot.class,
	        String[].class
	    };
	    
	    Class  cmdClass = null;
	    try
	    {
		    try
		    {
		        cmdClass = Class.forName( cmd );
		    }
		    catch( Exception e )
		    {
		        cmdClass = Class.forName( this.getClass().getPackage().getName() + "." + cmd );
		    }
	    }
	    catch( Throwable t )
	    {
		    SampleUtil.println( "Unknown command or class not found: " + cmd );
		    SampleUtil.println( COMMANDS );
	    }
	    
	    
	    if ( cmdClass != null )
	    {
    	    try
    	    {
    	        final Object cmdImpl      = cmdClass.newInstance();
    	        final Method runMethod  = cmdClass.getMethod( "runCmd", sig );
    	        
    	        final Object[] args = new Object[] { mDomainRoot, parts };
    	        final Object   result =runMethod.invoke( cmdImpl, args );
    	        SampleUtil.println( SmartStringifier.DEFAULT.stringify( result ) );
    	    }
    	    catch( Throwable t )
    	    {
    	        final String msg   = ExceptionUtil.toString( t );
    		    SampleUtil.println( "ERROR:" );
    		    SampleUtil.println( msg );
    	    }
	    }
    }
		
		private void
	demo()
		throws IOException
	{
		final LineReaderImpl	in	= new LineReaderImpl( System.in );
		
		final Samples	samples	= new Samples( getDomainRoot() );
		while ( true )
		{
			final String	line	= in.readLine( "\n" + PROMPT );
			try
			{
				handleChoice( samples, line.trim() );
			}
			catch( IllegalArgumentException e )
			{
			}
			catch( Exception e )
			{
				e.printStackTrace();
			}
		}
	}
	
		private final DomainRoot
	getDomainRoot()
	{
		return( mDomainRoot );
	}
	
	
	
	
		private TLSParams
	createTLSParams(
		final String	trustStore,
		final String	password )
	{
		final File trustStoreFile	= new File( trustStore );
		final char[] trustStorePassword	= password.toCharArray();
					
		mHandshakeCompletedListener	= new HandshakeCompletedListenerImpl();
		final TrustStoreTrustManager trustMgr =
			new TrustStoreTrustManager( trustStoreFile, trustStorePassword);
		trustMgr.setPrompt( true );

		final TLSParams	tlsParams = new TLSParams( trustMgr, mHandshakeCompletedListener );

		return( tlsParams );
	}
	
	/**
		Read connect properties from a file.
	 */
		private final Properties
	getConnectProperties( final String file )
		throws IOException
	{
		final Properties	props	= new Properties();
		
		if ( file != null )
		{
			SampleUtil.println( "Reading properties from: " + StringUtil.quote( file ) );
			final File	f	= new File( file );
			
			if ( f.exists() )
			{
				final FileInputStream	is	= new FileInputStream( f );
				try
				{
					props.load( is );
				}
				finally
				{
					is.close();
				}
			}
			else
			{
				SampleUtil.println("File \"" + file + " does not exist, using defaults." );
			}
		}
		
		return( props );
	}
	
	private final static String	DEFAULT_TRUST_STORE_FILE		= "~/.keystore";
	private final static String	DEFAULT_TRUST_STORE_PASSWORD	= "changeme";
	
	/**
		@param host	hostname or IP address of Domain Admin Server
		@param port	RMI administrative port
		@param user	admin user
		@param password admin user password
		@param tlsParams TLS parameters, may be null
		@return AppserverConnectionSource
	 */
		public static AppserverConnectionSource
	connect(
		final String	host,
		final int		port,
		final String	user,
		final String	password,
		final TLSParams	tlsParams )
		throws IOException
	{
		final String info = "host=" + host + ", port=" + port +
			", user=" + user + ", password=" + password +
			", tls=" + (tlsParams != null);
			
		SampleUtil.println( "Connecting...:" + info );
		
		final AppserverConnectionSource conn	=
			new AppserverConnectionSource( AppserverConnectionSource.PROTOCOL_RMI,
				host, port, user, password, tlsParams, null);
		
		// force the connection now
		conn.getJMXConnector( false );

		SampleUtil.println( "Connected: " + info );
		
		return( conn );
	}
	
	
	    private AppserverConnectionSource
	 attemptConnect(
		final String	host,
		final int		port,
		final String	user,
		final String	password,
	    final boolean  useTLS,
	    final String   trustStore,
	    final String   trustStorePassword )
		throws IOException
	 {
	    final TLSParams	tlsParams	= useTLS ?
			createTLSParams( trustStore, trustStorePassword) : null;
		
		final AppserverConnectionSource	conn 	= connect( host, port, user, password, tlsParams );
		
		if ( useTLS && mHandshakeCompletedListener != null )
		{
			SampleUtil.println( "HandshakeCompletedEvent: " +
				SmartStringifier.toString( mHandshakeCompletedListener.getLastEvent() ) );
		}
		
		return conn;
	 }
	
	/**
	 */
		public
	SampleMain( final String optionalPropertiesFile )
		throws IOException
	{
		final Properties	props	= getConnectProperties( optionalPropertiesFile );
		
		final String	host	= props.getProperty( "connect.host", "localhost" );
		final int		port	= Integer.parseInt( props.getProperty( "connect.port", "8686" ) );
		final String	user	= props.getProperty( "connect.user", "admin" );
		final String	password	= props.getProperty( "connect.password", "admin123" );
		final String	trustStore	= props.getProperty( "connect.truststore", DEFAULT_TRUST_STORE_FILE);
		final String	trustStorePassword	=
				props.getProperty( "connect.truststorePassword", DEFAULT_TRUST_STORE_PASSWORD);
		final boolean	useTLS	= 
			Boolean.valueOf( props.getProperty( "connect.useTLS", "false" ) ).booleanValue();
		
		AppserverConnectionSource	conn    = null;
		try
		{
		    conn    = attemptConnect( host, port, user, password,
		                useTLS, trustStore, trustStorePassword );
		}
		catch( Throwable t )
		{
		    final Throwable rootCause   = ExceptionUtil.getRootCause( t );
		    SampleUtil.println( "Caught " + rootCause.getClass().getName() );
		    
		    if ( rootCause instanceof CertificateException  )
		    {
		        SampleUtil.println( "Attempting connection with connect.useTLS = " + ! useTLS );
    		    conn    = attemptConnect( host, port, user, password,
    		                ! useTLS, trustStore, trustStorePassword );
		    }
		}
		
		assert conn != null : "Can't connect to server";
		mDomainRoot	= conn.getDomainRoot();
		
		try
		{
			demo( );
		}
		finally
		{
			// close the connection (not necessary, but here for as an example)
			conn.getJMXConnector( false ).close();
		}
	}
}









