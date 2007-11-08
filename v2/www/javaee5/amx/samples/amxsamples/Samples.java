/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Serializable;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Collection;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Level;


import javax.management.Attribute;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;
import javax.management.NotificationListener;
import javax.management.NotificationEmitter;
import javax.management.ListenerNotFoundException;
import javax.management.ObjectName;
import javax.management.NotificationFilter;
import javax.management.MBeanServerConnection;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanServerNotification;

import com.sun.appserv.management.DomainRoot;

import com.sun.appserv.management.base.AMX;
import com.sun.appserv.management.base.XTypes;
import com.sun.appserv.management.base.Container;
import com.sun.appserv.management.base.Sample;
import com.sun.appserv.management.base.Util;
import com.sun.appserv.management.base.Extra;
import com.sun.appserv.management.base.StdAttributesAccess;
import com.sun.appserv.management.base.QueryMgr;
import com.sun.appserv.management.base.Util;

import com.sun.appserv.management.util.misc.CollectionUtil;
import com.sun.appserv.management.util.misc.GSetUtil;
import com.sun.appserv.management.util.misc.MapUtil;
import com.sun.appserv.management.util.misc.StringUtil;
import com.sun.appserv.management.util.misc.ListUtil;

import com.sun.appserv.management.util.stringifier.SmartStringifier;
import com.sun.appserv.management.util.stringifier.ArrayStringifier;

import com.sun.appserv.management.util.jmx.JMXUtil;

import com.sun.appserv.management.client.AppserverConnectionSource;
import com.sun.appserv.management.client.TLSParams;
import com.sun.appserv.management.client.HandshakeCompletedListenerImpl;

import com.sun.appserv.management.j2ee.J2EEDomain;
import com.sun.appserv.management.j2ee.J2EEServer;

import com.sun.appserv.management.deploy.DeploymentMgr;
import com.sun.appserv.management.deploy.DeploymentStatus;
import com.sun.appserv.management.deploy.DeploymentProgress;
import com.sun.appserv.management.deploy.DeploymentSupport;

import com.sun.appserv.management.monitor.MonitoringDottedNames;
import com.sun.appserv.management.monitor.JMXMonitorMgr;
import com.sun.appserv.management.monitor.AMXStringMonitor;
import com.sun.appserv.management.monitor.ServerRootMonitor;

import com.sun.appserv.management.config.DomainConfig;
import com.sun.appserv.management.config.HTTPServiceConfig;
import com.sun.appserv.management.config.ConfigConfig;
import com.sun.appserv.management.config.ConfigDottedNames;
import com.sun.appserv.management.config.StandaloneServerConfig;
import com.sun.appserv.management.config.ModuleMonitoringLevelsConfig;
import com.sun.appserv.management.config.ModuleMonitoringLevelValues;
import com.sun.appserv.management.config.PropertiesAccess;

import com.sun.appserv.management.ext.logging.Logging;
import com.sun.appserv.management.ext.logging.LogModuleNames;


/**
	Main class demonstrating a variety of MBean API (AMX) usages.
 */
public final class Samples
{
	private final DomainRoot			        mDomainRoot;
    private final Map<String,LoggingListener>   mLoggingListeners;
    private final String NL;
    
		public
	Samples( final DomainRoot	domainRoot )
	{
	    NL  = StringUtil.NEWLINE();
	    
		mDomainRoot	= domainRoot;
		mLoggingListeners   = new HashMap<String,LoggingListener>();
	}
	
		public final DomainRoot
	getDomainRoot()
	{
		return( mDomainRoot );
	}
	
		public DomainConfig
	getDomainConfig()
	{
		return( getDomainRoot().getDomainConfig() );
	}
	
		public QueryMgr
	getQueryMgr()
	{
		return( getDomainRoot().getQueryMgr() );
	}
	
	/**
		Print a message.
	 */
		public void
	println( Object o )
	{
		SampleUtil.println( toString( o ) );
	}
	
	/**
		Turn an object into a useful String
	 */
		public String
	toString( Object o )
	{
		return( SmartStringifier.toString( o ) );
	}
	
	/**
		Display a Map to System.out.
	 */
		private void
	displayMap(
		final String	msg,
		final Map<?,?> 		m)
	{
		println( msg + ": " + toString( m.keySet() ) );
	}

	/**
		Demonstrates how to access various types of  {@link AMX} by obtaining a Map and then displaying it.
	 */
		public void
	handleList()
	{
		final DomainConfig	dcp	= getDomainConfig();
		
		// Top-level items
		println( "\n--- Top-level --- \n" );
		
		displayMap( "ConfigConfig", dcp.getConfigConfigMap() );
		
		displayMap( "ServerConfig", dcp.getServerConfigMap() );
		
		displayMap( "StandaloneServerConfig", dcp.getStandaloneServerConfigMap() );
		
		displayMap( "ClusteredServerConfig", dcp.getClusteredServerConfigMap() );
		
		displayMap( "ClusterConfig", dcp.getClusterConfigMap() );
		
		
		// deployed items
		println( "\n--- DeployedItems --- \n" );
		displayMap( "J2EEApplicationConfig", dcp.getJ2EEApplicationConfigMap() );
		displayMap( "EJBModuleConfig", dcp.getEJBModuleConfigMap() );
		displayMap( "WebModuleConfig", dcp.getWebModuleConfigMap() );
		displayMap( "RARModuleConfig", dcp.getRARModuleConfigMap() );
		displayMap( "AppClientModuleConfig", dcp.getAppClientModuleConfigMap() );
		displayMap( "LifecycleModuleConfig", dcp.getLifecycleModuleConfigMap() );
		
		
		// resources
		println( "\n--- Resources --- \n" );
		
		displayMap( "CustomResourceConfig", dcp.getCustomResourceConfigMap() );
		displayMap( "PersistenceManagerFactoryResourceConfig",
			dcp.getPersistenceManagerFactoryResourceConfigMap() );
		displayMap( "JNDIResourceConfig", dcp.getJNDIResourceConfigMap() );
		displayMap( "JDBCResourceConfig", dcp.getJDBCResourceConfigMap() );
		displayMap( "ConnectorResourceConfig", dcp.getConnectorResourceConfigMap() );
		displayMap( "JDBCConnectionPoolConfig", dcp.getJDBCConnectionPoolConfigMap() );
		displayMap( "PersistenceManagerFactoryResourceConfig",
			dcp.getPersistenceManagerFactoryResourceConfigMap() );
		displayMap( "ConnectorConnectionPoolConfig",
			dcp.getConnectorConnectionPoolConfigMap() );
		displayMap( "AdminObjectResourceConfig", dcp.getAdminObjectResourceConfigMap() );
		displayMap( "ResourceAdapterConfig", dcp.getResourceAdapterConfigMap() );
		displayMap( "MailResourceConfig", dcp.getMailResourceConfigMap() );
		
		
		// get a ConfigConfig
		final ConfigConfig	config	=
			(ConfigConfig)dcp.getConfigConfigMap().get( "server-config" );
			

		// HTTPService
		println( "\n--- HTTPService --- \n" );
		
		final HTTPServiceConfig httpService = config.getHTTPServiceConfig();
		displayMap( "HTTPListeners", httpService.getHTTPListenerConfigMap() );
		displayMap( "VirtualServers", httpService.getVirtualServerConfigMap() );
	}
	
	/**
		Return a Set of {@link AMX} whose ObjectName has the property
		 <i>property-name</i>=<i>property-value</i>.
		
		@param propertyName
		@param propertyValue
		@return Set of {@link AMX}
	*/
		public Set<? extends AMX>
	queryWild(
		final String propertyName,
		final String propertyValue)
	{
		final String[]	propNames	= new String[] { propertyName };
		final String[]	propValues	= new String[]{ propertyValue };
		
		final Set<AMX>	amxs	= getQueryMgr().queryWildSet( propNames, propValues );
		
		return( amxs );
	}
	
	/**
		Call queryWild( propertyName, propertyValue ) and display the result.
		
		@param propertyName
		@param propertyValue
	*/
		public void
	displayWild(
		final String propertyName,
		final String propertyValue)
	{
		final Set<? extends AMX>	items	= queryWild( propertyName, propertyValue );
		
		println( "\n--- Queried for " + propertyName + "=" + propertyValue + " ---" );
		for( final AMX item : items )
		{
			println( "j2eeType=" + item.getJ2EEType() + "," + "name=" + item.getName() );
		}
	}
	
		public Set<? extends AMX>
	queryForJ2EEType( final String j2eeType )
	{
		final String	prop	= Util.makeJ2EETypeProp( j2eeType );
		final Set<? extends AMX> items	= getQueryMgr().queryPropsSet( prop );
		
		return( items );
	}
	
		public void
	displayAvailableChildTypes( final String j2eeType )
	{
		final DomainRoot		domainRoot	= getDomainRoot();
		
	}
	
		private String
	getIndent( final int num )
	{
		final char[]	indent	= new char[ num ];
		for( int i = 0; i < num; ++i )
		{
			indent[ i ]	= ' ';
		}
		return( new String( indent ) );
	}
	
	/**
		Display the j2eeType and name (if not {@link AMX#NO_NAME})
	 */
		private void
	displayAMX(
		final AMX       amx,
		final boolean   showAttributes,
		final int	    indentCount )
	{
		final String indent	= getIndent( indentCount );
		
		final String	j2eeType	= amx.getJ2EEType();
		final String	name		= amx.getName();
		if ( name.equals( AMX.NO_NAME ) )
		{
			println( indent + j2eeType );
		}
		else
		{
			println( indent + j2eeType + "=" + name );
		}
		
		if ( showAttributes )
		{
	        final int   MAX_LEN = 60;
	        final String ATTR_PREFIX    = "@";
	        
		    final Map<String,Object>    attrs   = Util.getExtra( amx ).getAllAttributes();
		    
		    final String[]  attrNames   = GSetUtil.toStringArray( attrs.keySet() );
		    Arrays.sort( attrNames );
		    final String    attrIndent  = getIndent( indentCount + 2 );
		    for( final String attrName : attrNames )
		    {
		        final Object value  = attrs.get( attrName );
		        String attrString  = "" + SmartStringifier.DEFAULT.stringify( value );
		        if ( attrString.length() > MAX_LEN )
		        {
		            attrString  = attrString.substring( 0, MAX_LEN ) + "...";
		        }
		        
		        println( attrIndent + ATTR_PREFIX + attrName + " = " + attrString );
		    }
		}
	}
	
		private void
	displayHierarchy(
		final Collection<? extends AMX>	amxSet,
		final boolean       showAttributes,
		final int			indentCount )
	{
		for( final AMX amx : amxSet )
		{
			displayHierarchy( amx, showAttributes, indentCount );
		}
	}
	
	/**
		Display the hierarchy of {@link AMX} beginning with the specified one
	 */
		public void
	displayHierarchy(
		final AMX   amx,
		final boolean showAttributes,
		final int	indentCount )
	{
		displayAMX( amx, showAttributes, indentCount );
		
		if ( amx instanceof Container )
		{
		    final Container container   = (Container)amx;
		    
			// get Maps of all contained items
			final Map<String,Map<String,AMX>>	m = container.getMultiContaineeMap( null );
			
			final String[]  j2eeTypes   = GSetUtil.toStringArray( m.keySet() );
			Arrays.sort( j2eeTypes );
			for( final String j2eeType : j2eeTypes )
			{
			    final Map<String,AMX> instancesMap = m.get( j2eeType );
			    
				displayHierarchy( instancesMap.values(), showAttributes, indentCount + 2);
			}
		}
		
	}
	
	/**
		Display the entire MBean hierarchy.
	 */
		public void
	displayHierarchy( boolean showAttributes )
	{
		displayHierarchy( getDomainRoot(), showAttributes, 0 );
	}
	
	/**
		Display the MBean hierarchy beginning with j2eeType.
	 */
		public void
	displayHierarchy(
	    final String j2eeType,
	    final boolean showAttributes )
	{
		final Set<? extends AMX>	items	= getQueryMgr().queryJ2EETypeSet( j2eeType );
		
		if ( items.size() == 0 )
		{
			println( "No AMX MBean of j2eeType " + StringUtil.quote( j2eeType ) + " found" );
		}
		else
		{
			displayHierarchy( items, showAttributes, 0);
		}
	}
	
	
	/**
		Display all MBeans that have j2eeType=<j2eeType>
	 */
		public void
	displayJ2EEType( final String j2eeType )
	{
		final Set<? extends AMX>		items	= queryForJ2EEType( j2eeType );
		
		println( "\n--- Queried for j2eeType=" + j2eeType + " ---" );
		
		for( final AMX item : items )
		{
			// they may or may not have unique names, so show ObjectNames
			println( Util.getObjectName( item ) );
		}
		println( "" );
	}
	
	
	/**
		Display all Attributes in the {@link AMX}.
	 */
		public void
	displayAllAttributes( final AMX item )
	{
		println( "\n--- Attributes for " + item.getJ2EEType() + "=" + item.getName() + " ---" );
		
		final Extra	extra	= Util.getExtra( item );
		
		final Map<String,Object>	attrs	= extra.getAllAttributes();
		
		for( final String name : attrs.keySet() )
		{
			final Object	value	= attrs.get( name );
			
			println( name + "=" + toString( value ) );
		}
	}
	
	/**
		Display all Attributes in the {@link AMX}.
	 */
		public void
	displayAllAttributes( final String j2eeType )
	{
		final Set<? extends AMX>		items	= queryForJ2EEType( j2eeType );
		
		if ( items.size() == 0 )
		{
			println( "No {@link AMX} of j2eeType " + StringUtil.quote( j2eeType ) + " found" );
		}
		else
		{
		    for( final AMX amx : items )
			{
				displayAllAttributes( amx );
				println( "" );
			}
		}
	}
	
	
	/**
		Display all dotted names.
	 */
		public void
	displayDottedNames()
	{
		final ConfigDottedNames	configDottedNames	= getDomainRoot().getConfigDottedNames();
		Attribute[] result	= (Attribute[])configDottedNames.dottedNameGet( "*" );
		println( "--- ConfigDottedNames ---" );
		println( SampleUtil.arrayToString( result, "", "\n" ) );
		
		println( "\n--- MonitoringDottedNames ---" );
		
		final MonitoringDottedNames	monDottedNames	= getDomainRoot().getMonitoringDottedNames();
		result	= (Attribute[])monDottedNames.dottedNameGet( "*" );
		println( SampleUtil.arrayToString( result, "", "\n" ) );
	}
	
	
	/**
		Demonstrate how to use the {@link com.sun.appserv.management.base.QueryMgr} facilities.
	 */
		public void
	demoQuery()
	{
		displayWild( AMX.J2EE_TYPE_KEY, "X-*ResourceConfig" );
		displayWild( AMX.J2EE_TYPE_KEY, "X-*ServerConfig" );
		
		displayJ2EEType( XTypes.SSL_CONFIG );
		displayJ2EEType( XTypes.CLUSTER_CONFIG );
	}

	
		private Object
	uploadArchive( final File archive  )
		throws IOException
	{
		final FileInputStream	input	= new FileInputStream( archive );
		final long	length	= input.available();
		final DeploymentMgr	mgr	= getDomainRoot().getDeploymentMgr();
		final Object	uploadID	= mgr.initiateFileUpload( length );
			
		try
		{
			final int	chunkSize	= 256 * 1024;
			long remaining	= length;
			while ( remaining != 0 )
			{
				final int	actual	= remaining < chunkSize ? (int)remaining : chunkSize;
				
				final byte[]	bytes	= new byte[ actual ];
				final int	num	= input.read( bytes );
				if ( num != actual )
				{
					throw new IOException();
				}

				mgr.uploadBytes( uploadID, bytes );
				remaining	-= actual;
			}
		}
		finally
		{
			input.close();
		}
		
		return( uploadID );
	}
	
	
		private final String
	getAppName( final String archiveName )
	{
		String	result	= archiveName;
		
		final int	idx	= archiveName.lastIndexOf( "." );
		if ( idx > 1 )
		{
			result	= archiveName.substring( 0, idx );
		}
		
		return( result );
	}
	
	    private void
	sleep( final long millis )
	{
		try
		{
			Thread.sleep( millis );
		}
		catch( InterruptedException e )
		{
		}
	}

	/**
		Deploy an archive.
		<p>
		To deploy, you will need an archive to deploy.  A recommended sample may be found at:
		<i>INSTALL_ROOT</i>/samples/ejb/stateless/apps/simple.ear
		<p>
		This sample deploys the archive to the domain, but does not create any references
		to it, so it will not actually be associated with any server.
		<p>
		To associate an application with a server, use
		{@link StandaloneServerConfig#createDeployedItemRefConfig}
		@see com.sun.appserv.management.config.StandaloneServerConfig
		@see com.sun.appserv.management.config.DeployedItemRefConfigCR
	 */
		public void
	deploy( final File archive )
		throws IOException
	{
		final Object	uploadID	= uploadArchive( archive );
		final DeploymentMgr	mgr	= getDomainRoot().getDeploymentMgr();
		
		final Object	deployID	= mgr.initDeploy( );
		final DeployNotificationListener myListener	= new DeployNotificationListener( deployID);
		mgr.addNotificationListener( myListener, null, null);
		
		try
		{
			final Map<String,String>	options	= new HashMap<String,String>();
			
			final String	archiveName	= archive.getName();
			final String	deployName	= getAppName( archiveName );
			SampleUtil.println( "Deploying " + archiveName + " as " + deployName );
			
			options.put( DeploymentMgr.DEPLOY_OPTION_NAME_KEY, deployName );
			options.put( DeploymentMgr.DEPLOY_OPTION_VERIFY_KEY, Boolean.TRUE.toString() );
			options.put( DeploymentMgr.DEPLOY_OPTION_DESCRIPTION_KEY, "deployed from sample cmd line" );
			
			println( NL + "--- Deploy options ---" + NL + MapUtil.toString( options, NL) + NL );
			
			mgr.startDeploy( deployID, uploadID, null, options);
            
            Map<String,String> foo  = new HashMap<String,String>();
            mgr.startDeploy( deployID, foo, foo, options );
			
			while ( ! myListener.isCompleted() )
			{
				println( "deploy: waiting for deploy of " + archive);
			    sleep( 1000 );
			}
			
			final DeploymentStatus	status	= myListener.getDeploymentStatus();
			final Map<String,Serializable>	additionalStatus	= status.getAdditionalStatus();
			final String	moduleID	=
				(String)additionalStatus.get( DeploymentStatus.MODULE_ID_KEY );
			
			final int   statusCode = status.getStageStatus();
			if ( statusCode == DeploymentStatus.STATUS_CODE_SUCCESS )
			{
    			SampleUtil.println( "Deployed " + quote(archiveName) + " as " + quote(deployName) +
    				 ": status=" + status.getStageStatus() + ", moduleID = " + quote(moduleID) +
    				 ", AdditionalStatus=" + MapUtil.toString( additionalStatus, " ") );
    		
    		    assert status.getStageThrowable() == null;
		    }
			else if ( statusCode == DeploymentStatus.STATUS_CODE_FAILURE )
			{
    			SampleUtil.println(
    			    "Deployment of " + quote(archiveName) + " as " + quote(deployName) +
    				 " ***FAILED***, AdditionalStatus = " +
    				    MapUtil.toString( additionalStatus, " ") );
    		
    		    assert status.getStageThrowable() == null;
			}
			else
			{
    			SampleUtil.println(
    			    "Deployment of " + quote(archiveName) + " as " + quote(deployName) +
    				 "returned with status code " + statusCode + ", AdditionalStatus = " +
    				    MapUtil.toString( additionalStatus, " ") );
    		
    		    assert status.getStageThrowable() == null;
			}
			
			if ( status.getStageThrowable() != null )
			{
				status.getStageThrowable().printStackTrace();
			}
		}
		finally
		{
			try
			{
				mgr.removeNotificationListener( myListener );
			}
			catch( Exception e )
			{
			}
		}
	}
	
		private String
	quote( final String s )
	{
		return StringUtil.quote( s );
	}
	
	/**
		Undeploys a deployed module.
	 */
		public void
	undeploy( final String moduleName )
		throws IOException
	{
		final DeploymentMgr	mgr	= getDomainRoot().getDeploymentMgr();
		
		final Map<String,Serializable>	statusData	= mgr.undeploy( moduleName, null );
		final DeploymentStatus	status	= 
			DeploymentSupport.mapToDeploymentStatus( statusData );
			
		println( "Undeployment result: " + status.getStageStatus() );
		if ( status.getStageThrowable() != null )
		{
			status.getStageThrowable().printStackTrace();
		}
	}
	
	/**
		Get a J2EEServer by name.
	 */
		public J2EEServer
	getJ2EEServer( final String serverName )
	{
		final J2EEDomain	j2eeDomain	= getDomainRoot().getJ2EEDomain();
		final Map<String,J2EEServer>			servers	= j2eeDomain.getJ2EEServerMap();
		final J2EEServer	server	= (J2EEServer)servers.get( serverName );
		
		if ( server == null )
		{
			throw new IllegalArgumentException( serverName );
		}
		
		return( server );
	}
	
	/**
		Create a standalone server.
		
		@param configName
	 */
		public ConfigConfig
	createConfig( final String configName )
	{
		final ConfigConfig	config	= getDomainConfig().createConfigConfig( configName, null );
		return( config );
	}
	
	/**
		Create a standalone server.
		
		@param serverName
	 */
		public StandaloneServerConfig
	createServer(
		final String	serverName,
		final String	configName )
	{
		final String	nodeAgentName	= null;
		
		final StandaloneServerConfig	server	= (StandaloneServerConfig)
			getDomainConfig().createStandaloneServerConfig( serverName, nodeAgentName, configName, null );
		
		return( server );
	}
	
		public StandaloneServerConfig
	createServer( final String	serverName )
	{
		final ConfigConfig	config	= createConfig( serverName + "-config" );
		
		final StandaloneServerConfig	server	= createServer( serverName, config.getName() );
		return( server );
	}
	
	
	/**
		Start a server.
	 */
		public void
	startServer( final String serverName )
	{
		final J2EEServer	server	= getJ2EEServer( serverName );
		
		server.start();
	}
	
	/**
		Stop a server.
	 */
		public void
	stopServer( final String serverName )
	{
		final J2EEServer	server	= getJ2EEServer( serverName );
		
		server.stop();
	}
	
	
	private static final Set<String>	LEGAL_MON	=
		GSetUtil.newUnmodifiableStringSet( 
    		ModuleMonitoringLevelValues.HIGH,
    		ModuleMonitoringLevelValues.LOW,
    		ModuleMonitoringLevelValues.OFF
	    );
		
	/**
		Sets the monitoring state for all available modules.
		
		@param configName	configuration element on which to operate
		@param state		one of HIGH, LOW, OFF
	 */
		public void
	setMonitoring(
		final String	configName,
		final String	 state )
	{
		if ( ! LEGAL_MON.contains( state ) )
		{
			throw new IllegalArgumentException( state );
		}
		
		final Map<String,ConfigConfig>  configs = getDomainConfig().getConfigConfigMap();
		final ConfigConfig	config	= configs.get( configName );
	    if ( config == null )
	    {
	        println( "No such config: " + configName );
	        println( "Available configs: " + CollectionUtil.toString( configs.keySet() ) );
	    }
	    else
	    {
    		final ModuleMonitoringLevelsConfig	mon	=
    			config.getMonitoringServiceConfig().getModuleMonitoringLevelsConfig();
    		
    		mon.changeAll( state );
		}
	}
	
	/**
		Get a Map of <i>property-name</i>=<i>property-value</i>.
		
		@param pa	a PropertiesAccess
	 */
		public Map<String,String>
	getProperties( final PropertiesAccess pa )
	{
		final Map<String,String>	m	= new HashMap<String,String>();
		
		final String[]	names	= pa.getPropertyNames();
		for( int i = 0; i < names.length; ++i )
		{
			m.put( names[ i ], pa.getPropertyValue( names[ i ] ) );
		}
		
		return( m );
	}
	
	/**
		Display all properties found on all {@link AMX}.
		
		@see #getProperties(PropertiesAccess)
		@see PropertiesAccess#getPropertyNames
	 */
		public void
	displayAllProperties( )
	{
		for( final AMX amx : getQueryMgr().queryAllSet() )
		{
			if ( amx instanceof PropertiesAccess )
			{
				final PropertiesAccess	pa	= (PropertiesAccess)amx;
				
				final Map<String,String>	props	= getProperties( pa );
				if ( props.keySet().size() != 0 )
				{
					println( "\nProperties for: " + Util.getObjectName( (AMX)pa ) );
					println( MapUtil.toString( getProperties( pa ), "\n") );
				}
			}
		}
	}
	
		private void
	mySleep( final long millis )
	{
		try
		{
			Thread.sleep( millis );
		}
		catch( InterruptedException e )
		{
		}
	}
	
	
	public final static String	MBEAN_SERVER_DELEGATE	=
							"JMImplementation:type=MBeanServerDelegate";
		public static ObjectName
	getMBeanServerDelegateObjectName()
	{
		return( Util.newObjectName( MBEAN_SERVER_DELEGATE ) );
	}
	
	
		private void
	waitNumNotifs(
	    final Map<String,List<Notification>> notifs,
	    final String    type,
	    final int       numRequired )
	{
		while ( true )
		{
			final List<Notification>	list	= notifs.get( type );
			if ( list != null && list.size() >= numRequired )
			{
				break;
			}

			mySleep( 50 );
		}
	}
	
		
	
		private void
	waitMBeanServerNotification(
		final SampleListener	listener,
		final String			type,
		final ObjectName		objectName )
	{
		List<Notification>		list	= null;
		while ( (list = listener.getNotifsReceived( type )) == null )
		{
			mySleep( 50 );
		}
		
		boolean	waiting	= true;
		while ( waiting )
		{
		    for( final Notification notif : list )
			{
			    final MBeanServerNotification   mbsNotif    = (MBeanServerNotification)notif;
			    
				if ( mbsNotif.getMBeanName().equals( objectName ) )
				{
					waiting	= false;
					break;
				}
				else
				{
					println( "Unexpected ObjectName: " + objectName + " != " + mbsNotif.getMBeanName() );
				}
			}
			mySleep( 100 );
		}
	}
	
	
	/**
		Demonstrates the use of a javax.management.monitor MBean
		to be notified of changes in the value of an Attribute.
	 */
		public void
	demoJMXMonitor()
		throws InstanceNotFoundException, IOException
	{
		final JMXMonitorMgr	mgr	= getDomainRoot().getJMXMonitorMgr();
		
		final String	attrName	= "SampleString";
		final String	attrValue	= "hello";
		
		// listen to the MBeanServerDelegate, too, so we can see our sample monitor
		// get registered.
		final SampleListener	sampleListener	= new SampleListener();
		final MBeanServerConnection	conn	=
			Util.getExtra( mgr ).getConnectionSource().getExistingMBeanServerConnection();
		conn.addNotificationListener(
			getMBeanServerDelegateObjectName(), sampleListener, null, null );
		
			
		final Sample	sample	= (Sample)getDomainRoot().getContainee( XTypes.SAMPLE );
		
		final String	monitorName	= "SampleStringMonitor";
		AMXStringMonitor	mon	= null;
		try
		{
			// cleanup in case it was left around by mistake...
			try { mgr.remove( monitorName ); } catch( Exception e )	{}
			
			// create a new one
			mon	= mgr.createStringMonitor( monitorName );
			// observer that we've been notified (not required)
			waitMBeanServerNotification( sampleListener,
				MBeanServerNotification.REGISTRATION_NOTIFICATION, Util.getObjectName( mon ) );
		
			// we'll modify this Attribute's value, to force a change
			sample.addAttribute( attrName, attrValue );
			
			// listen to the monitor
			mon.addNotificationListener( sampleListener, null, null );
			mon.setObservedAttribute( attrName );
			mon.setStringToCompare( attrValue );
			mon.setNotifyDiffer( true );
			mon.setNotifyMatch( true );
			
			// tell the monitor to observe sample
			mon.addObservedObject( Util.getObjectName( sample ) );
			
			// since the Attribute was added dynamically, there is no
			// getter method, so we must access the Attribute via JMX
			final StdAttributesAccess	attrs	= Util.getExtra( sample );
			attrs.setAttribute( new Attribute( attrName, "goodbye" ) );
			// set it to original value
			attrs.setAttribute( new Attribute( attrName, attrValue ) );
			
			// we added it,so we should remove it
			sample.removeAttribute( attrName );
			
			// let the Notifications arrive...
			final Map<String,List<Notification>>	notifs	= sampleListener.getNotifsReceived();
			waitNumNotifs( notifs, AttributeChangeNotification.ATTRIBUTE_CHANGE, 4 );
		}
		catch( Throwable t )
		{
			t.printStackTrace();
		}
		finally
		{
			try
			{
				mon.removeNotificationListener( sampleListener );
				
				// don't leave monitors around
				if ( mon != null )
				{
					mgr.remove( mon.getName() );
					// observer that we've been notified (not required)
					waitMBeanServerNotification( sampleListener,
						MBeanServerNotification.UNREGISTRATION_NOTIFICATION,
						Util.getObjectName( mon ) );
				}
				
				conn.removeNotificationListener(
					getMBeanServerDelegateObjectName(), sampleListener );
			}
			catch( ListenerNotFoundException e )
			{
			}
		}
	}
	
	
	    public void
	dumpLogging( final String server )
	{
	    final ServerRootMonitor monitor =
	        getDomainRoot().getMonitoringRoot().getServerRootMonitorMap().get( server );
	    
	    if ( monitor == null )
	    {
	        println( "Server " + quote( server ) + " does not exist or is not running." );
	    }
	    else
	    {
	        println( "--- server " + quote( server )  + " ---" );
	        
    	    final Logging logging = monitor.getLogging();
    	    
    	    for( final String moduleName : LogModuleNames.ALL_NAMES )
    	    {
    	        println( moduleName + ": " + logging.getModuleLogLevel( moduleName ) );
    	    }
    	    
    	    println( "" );
    	    
    	    final String[]  logFileKeys = logging.getLogFileKeys();
    	    println( "Log file keys: " + ArrayStringifier.stringify( logFileKeys, ", ") );
    	    
    	    for( final String key : logFileKeys )
    	    {
    	        if ( Logging.ACCESS_KEY.equals( key ) )
    	        {
    	            continue;   // not supported
    	        }
    	        
    	        final String[]  logFileNames    = logging.getLogFileNames( key );
    	        if ( logFileNames.length != 0 )
    	        {
    	            println( key + " log: " + NL +
    	                ArrayStringifier.stringify( logFileNames, NL) );
    	        }
    	    }
    	    
    	    println( NL + "Error statistics are being retained for " +
    	                logging.getKeepErrorStatisticsForIntervals() +
    	                " intervals of " + 
    	                logging.getErrorStatisticsIntervalMinutes() + " minutes." + NL
    	                );
    	                
    	    println( "ERROR INFOS: " + NL );
    	    
    	    final Map<String,Number>[]  errorInfos  = logging.getErrorInfo();
    	    for( final Map<String,Number> errorInfo : errorInfos )
    	    {
    	        final Number timestamp      = errorInfo.get( Logging.TIMESTAMP_KEY );
    	        final Number severeCount    = errorInfo.get( Logging.SEVERE_COUNT_KEY );
    	        final Number warningCount   = errorInfo.get( Logging.WARNING_COUNT_KEY );
    	        
    	        println( new java.util.Date( timestamp.longValue() ) );
    	        println( getIndent(1) + Logging.SEVERE_COUNT_KEY + ": " + severeCount );
    	        
    	        final Map<String,Integer>  severeDistribution    =
    	            logging.getErrorDistribution( timestamp.longValue(), Level.SEVERE.toString() );
    	        println( SampleUtil.mapToString( severeDistribution, getIndent(2), NL ) );
    	        
    	        println( getIndent(1) + Logging.WARNING_COUNT_KEY + ": " + warningCount );
    	        final Map<String,Integer>  warningDistribution    =
    	            logging.getErrorDistribution( timestamp.longValue(), Level.WARNING.toString() );
    	        println( SampleUtil.mapToString( warningDistribution, getIndent(2), NL ) );
    	    }
    	    
    	    
    	    println( NL + "LOGGER NAMES: " + NL );
    	    final String[]  loggerNames = logging.getLoggerNames();
    	    Arrays.sort( loggerNames );
    	    println( ArrayStringifier.stringify( loggerNames, NL) );
	    }
	}
	
	    final Set<String>
	getPrefixes( final String[] names )
	{
	    final String DELIM  = ".";
	    final Set<String>   namesSet    = GSetUtil.newUnmodifiableStringSet( names );
	    
	    final Set<String>   results = new HashSet<String>();
	    
	    for( final String name : names )
	    {
	        final int idx   = name.lastIndexOf( DELIM );
	        if ( idx > 0 )
	        {
	            final String prefix = name.substring( 0, idx - 1 );
	        
	            results.add( prefix );
	        }
	        else
	        {
	            // top-level
	            results.add( name );
	        }
	    }
	    
	    return results;
	}
	
	    private List<String>
    getServers( final String[] servers )
    {
	    List<String> serverNames = new ArrayList<String>();
	    
	    if ( servers.length == 0 )
	    {
	        serverNames.addAll( getDomainRoot().getDomainConfig().getServerConfigMap().keySet() );
	    }
	    else
	    {
	        serverNames = ListUtil.newListFromArray( servers );
	    }
	    
	    return serverNames;
    }
	
	    public void
	displayLogging( final String[] servers )
	{
	    for( final String server : getServers( servers ) )
	    {
	        dumpLogging( server );
	        println( "" );
	    }
	}
	
	
	    private void
	removeNotificationListener(
	    final NotificationEmitter  emitter,
	    final NotificationListener listener )
	{
        try
        {
            emitter.removeNotificationListener( listener );
        }
        catch( ListenerNotFoundException e )
        {
            // ignore
        }
	}
	
	    public void
	listenToLogging(
	    final boolean listen,
	    final String  server )
	{
	    final ServerRootMonitor monitor =
	        getDomainRoot().getMonitoringRoot().getServerRootMonitorMap().get( server );
	    
	    if ( monitor == null )
	    {
	        println( "Server " + quote( server ) + " does not exist or is not running." );
	    }
	    else
	    {
    	    final Logging logging = monitor.getLogging();
    	    
    	    final boolean alreadyListening  = mLoggingListeners.containsKey( server );
    	    
	        if ( alreadyListening )
	        {
	            removeNotificationListener( logging, mLoggingListeners.get( server ) );
                mLoggingListeners.remove( server );
	        }
	        
    	    if ( listen )
    	    {
    	        final LoggingListener listener  = new LoggingListener();
    	        logging.addNotificationListener( listener, null, server);
    	        mLoggingListeners.put( server, listener );
    	        
    	        println( "Listening to " + JMXUtil.toString( Util.getObjectName( logging ) ) );
    	        
    	        // emit a test log message, and wait for it.
    	        logging.testEmitLogMessage( "INFO", "TEST LOG MESSAGE FROM listenToLogging()" );
    	        while ( listener.getHistory().size() == 0 )
    	        {
    	            sleep( 100 );
    	        }
    	    }
    	    else
    	    {
    	        final String msg    = alreadyListening ?
    	            "Stopped listening to Logging on server " : "Not listening to Logging on server ";
    	            
    	        // already did it
    	        println( msg + server );
    	        
    	    }
	    }
	}
	
	    public void
	listenToLogging(
	    final boolean listen,
	    final String[] servers )
	{
	    for( final String server : getServers( servers ) )
	    {
	        listenToLogging( listen, server );
	        println( "" );
	    }
	}
}















