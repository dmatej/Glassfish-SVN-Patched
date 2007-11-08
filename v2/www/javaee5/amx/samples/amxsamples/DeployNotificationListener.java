/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package amxsamples;

import java.util.Map;

import java.io.Serializable;

import javax.management.NotificationListener;
import javax.management.Notification;

import com.sun.appserv.management.deploy.DeploymentStatus;
import com.sun.appserv.management.deploy.DeploymentMgr;
import com.sun.appserv.management.deploy.DeploymentSupport;
import com.sun.appserv.management.deploy.DeploymentProgress;

import com.sun.appserv.management.base.Util;
import com.sun.appserv.management.util.misc.TypeCast;
import static com.sun.appserv.management.deploy.DeploymentMgr.*;


/**
	A JMX NotificationListener for deployment.
 */
public final class DeployNotificationListener
		implements NotificationListener
{
	private final Object		mDeployID;
	private boolean				mIsCompleted;
	private DeploymentStatus	mDeploymentStatus;
	
		public
	DeployNotificationListener( final Object	deployID )
	{
		mDeployID		= deployID;
		mIsCompleted	= false;
	}
	
		public boolean
	isCompleted()
	{
		return( mIsCompleted );
	}
	
		public DeploymentStatus
	getDeploymentStatus()
	{
		return( mDeploymentStatus );
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
		catch( Exception e )
		{
			e.printStackTrace();
		}
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
		final String type	= notif.getType();
		
		final Object deployID	=
		    Util.getAMXNotificationValue( notif, NOTIF_DEPLOYMENT_ID_KEY );
		
		if ( deployID.equals( mDeployID ) )
		{
			if ( type.equals( DEPLOYMENT_STARTED_NOTIFICATION_TYPE ) )
			{
				SampleUtil.println( "Deployment started for " + deployID);
			}
			else if ( type.equals( DEPLOYMENT_COMPLETED_NOTIFICATION_TYPE ) )
			{
			    final Map value = (Map)
			        Util.getAMXNotificationValue( notif,
			            NOTIF_DEPLOYMENT_COMPLETED_STATUS_KEY );
			        
				final Map<String,Serializable>	statusData =
				    TypeCast.checkMap( value, String.class, Serializable.class);
				
				final DeploymentStatus	status	= 
					DeploymentSupport.mapToDeploymentStatus( statusData );
				
				SampleUtil.println( "Deployment completed for " + deployID + " with status: " + 
					status.getStageStatus() );
				
				mIsCompleted	= true;
				mDeploymentStatus	= status;
			}
			else if ( type.equals( DEPLOYMENT_PROGRESS_NOTIFICATION_TYPE ) )
			{
			   final Map m = (Map)
			        Util.getAMXNotificationValue( notif, NOTIF_DEPLOYMENT_PROGRESS_KEY );
				final Map<String,Serializable>	progressData	=
				    TypeCast.checkMap( m, String.class, Serializable.class );
				
				final DeploymentProgress	progress	= 
					DeploymentSupport.mapToDeploymentProgress( progressData );
					
				SampleUtil.println( "Deployment progress for " + deployID + " = " + 
					progress.getProgressPercent() + "%" );
			}
		}
	}
}
	
	