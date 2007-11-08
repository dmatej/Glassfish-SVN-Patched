/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import java.util.Map;
import java.util.Set;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

import javax.management.NotificationListener;
import javax.management.Notification;

import com.sun.appserv.management.base.Util;

import com.sun.appserv.management.ext.logging.Logging;

import com.sun.appserv.management.util.misc.GSetUtil;
import com.sun.appserv.management.util.misc.StringUtil;
import com.sun.appserv.management.util.misc.MapUtil;
import com.sun.appserv.management.util.misc.ListUtil;
import com.sun.appserv.management.util.misc.CircularList;

import com.sun.appserv.management.util.jmx.stringifier.NotificationStringifier;


/**
	A JMX NotificationListener for deployment.
 */
public final class LoggingListener
		implements NotificationListener
{
    private final Set<String>         mTypes;
    private final List<Notification>  mHistory;
    
		public
	LoggingListener()
	{
	    mTypes  = GSetUtil.newStringSet( Logging.LOG_RECORD_SEVERE_NOTIFICATION_TYPE,
	                    Logging.LOG_RECORD_WARNING_NOTIFICATION_TYPE,
	                    Logging.LOG_RECORD_INFO_NOTIFICATION_TYPE );
	    
	    mHistory    = new CircularList<Notification>( Notification.class, 100 );
	}
	
		public synchronized void
	handleNotification(
		final Notification	notif, 
		final Object		handback) 
	{
		try
		{
			realHandleNotification( notif, handback );
		}
		catch( final Exception e )
		{
			e.printStackTrace();
		}
	}
	
	    public void
	setNotificationTypes( final Set<String> types )
	{
	    mTypes.clear();
	    mTypes.addAll( types );
	}
	
	    public Set<String>
	getNotificationTypes( )
	{
	    return Collections.unmodifiableSet( mTypes );
	}
	
	
	/**
		Note that Notifications are not guaranteed to be delivered in order.
		Thus, it is theoretically possible for a DEPLOYMENT_COMPLETED_NOTIFICATION_TYPE
		to be received before a DEPLOYMENT_STARTED_NOTIFICATION_TYPE.
	 */
		public void
	realHandleNotification(
		final Notification	notif, 
		final Object		handback) 
	{
		final String	type	= notif.getType();
		final Map<String,Serializable> m	= Util.getAMXNotificationData( notif );
		
		if ( type.startsWith( Logging.LOG_RECORD_NOTIFICATION_PREFIX ) &&
		    mTypes.contains( type ) )
		{
		    SampleUtil.println( NotificationStringifier.toString( notif ) + 
		        StringUtil.NEWLINE() + "USER DATA: " + MapUtil.toString( m ) );
		        
		    mHistory.add( notif );
		}
	}
	
	    public synchronized List<Notification>
	getHistory()
	{
	    final List<Notification>    list    = new ArrayList<Notification>();
	    
	    list.addAll( mHistory );
	    return list;
	}
}













	
	