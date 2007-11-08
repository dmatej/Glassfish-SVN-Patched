/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;

import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.MBeanServerNotification;

import com.sun.appserv.management.util.jmx.stringifier.NotificationStringifier;

/**
	Displays any {@link javax.management.Notification} received.
	<p>
	Note that most {@link com.sun.appserv.management.base.AMX} Notifications include a Map in the
	userData field of the Notification.
	
	@see com.sun.appserv.management.base.AMX
 */
public final class SampleListener implements NotificationListener
{
	private Map<String,List<Notification>>	mNotifs;
	
		public
	SampleListener()
	{
		mNotifs	= null;
		clearNotifs();
	}
	
	/**
		Keep a Map, keyed by Notification type, of all Notifications received.
	 */
		private synchronized void
	addNotif( final Notification	notif )
	{
		final String	type	= notif.getType();
		
		List<Notification>	list	= null;
		
		if ( ! mNotifs.keySet().contains( type ) )
		{
			clearNotifs( type );
		}
		
		list	= mNotifs.get( type );
		
		list.add( notif );
	}
	
	/**
		Return a Map, keyed by Notification type, of all Notifications received so far.
	 */
		public Map<String,List<Notification>>
	getNotifsReceived()
	{
		return( mNotifs );
	}
	
		public List<Notification>
	getNotifsReceived( final String type )
	{
		return( mNotifs.get( type ) );
	}
	
	/**
		Clear the history of Notifications received.
	 */
		public synchronized Map<String,List<Notification>>
	clearNotifs()
	{
		final Map<String,List<Notification>>	existing	= getNotifsReceived();
		
		mNotifs	= Collections.synchronizedMap(
		        new HashMap<String,List<Notification>>() );
		
		return( existing );
	}
	
		public synchronized List
	clearNotifs( final String type )
	{
		final Map<String,List<Notification>>	existing	= getNotifsReceived();
		
		final List<Notification> newList	=
		    Collections.synchronizedList( new ArrayList<Notification>() );
		
		final List<Notification> existingList	= existing.get( type );
		
		mNotifs.put( type, newList );
		
		return( existingList );
	}
	
	/**
		The Notification is delivered here.
	 */
		public void
	handleNotification(
		final Notification	notif, 
		final Object		handback) 
	{
		final String	type		= notif.getType();
		final Object	userData	= notif.getUserData();
		
		addNotif( notif );
		
		SampleUtil.println( "SampleListener: received: " +
		    NotificationStringifier.toString( notif ) );
		SampleUtil.println( "" );
	}
	
	
	
}
