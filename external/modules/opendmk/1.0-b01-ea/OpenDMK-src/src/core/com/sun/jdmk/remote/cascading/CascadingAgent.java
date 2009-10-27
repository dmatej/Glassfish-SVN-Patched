/*
 * @(#)file      CascadingAgent.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.16
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
 *
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 */
package com.sun.jdmk.remote.cascading;

// java import
import java.util.Set;
import java.io.IOException;

// jdmk import
import javax.management.QueryExp;
import javax.management.ObjectName;
import javax.management.ObjectInstance;
import javax.management.MBeanServer;
import javax.management.MBeanRegistration;
import javax.management.MBeanRegistrationException;
import javax.management.RuntimeMBeanException;
import javax.management.Notification;
import javax.management.MBeanNotificationInfo;
import javax.management.NotificationEmitter;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationListener;
import javax.management.NotificationFilter;
import javax.management.NotificationBroadcasterSupport;
import javax.management.InstanceAlreadyExistsException;
import javax.management.ListenerNotFoundException;
import javax.management.MalformedObjectNameException;

import javax.management.remote.JMXConnectionNotification;
import com.sun.jdmk.defaults.Utils;


/**
 * This class is an abstract MBean class that provides a basic default
 * implementation for some methods of the {@link CascadingAgentMBean}
 * interface.
 * <p>
 * Using this class directly is discouraged. You should envisage using
 * the {@link CascadingService} instead.
 * <p>
 * A <tt>CascadingAgent</tt> is an MBean that is able to <i>mount</i> a partial
 * view of a <i>source MBeanServer</i> into a <i>target MBeanServer</i>.
 * The source <tt>MBeanServer</tt> is also sometimes called the 
 * <i>cascaded MBeanServer</i>, while the target <tt>MBeanServer</tt> is 
 * called the <i>cascading MBeanServer</i>.
 * <p>
 * The Java DMK cascading API introduces the notion of <i>domain path</i>.
 * An ObjectName is thus decomposed into three parts:
 * <pre>
 * &lt;domain-path&gt;&lt;domain-base-name&gt;:&lt;key-property-list&gt;
 * </pre>
 * The <i>domain path</i> is a hierarchical name similar to a UNIX path name,
 * and using the character `/' as separator.
 * <br>  
 * The Java DMK cascading API provides the ability to mount MBeans from
 * a source <tt>MBeanServer</tt> under a <i>target domain path</i> in a target 
 * <tt>MBeanServer</tt>.
 * <br>
 * For instance, this makes it possible to:
 * <ul>
 * <li>
 * mount <tt>"java.lang:*"</tt> MBeans from a subagent 1 under 
 *       <tt>"server1/instance1"</tt> 
 * </li>
 * <li>
 * mount <tt>"java.lang:*"</tt> MBeans from a subagent 2 under 
 *       <tt>"server1/instance2"</tt>
 * </li>
 * </ul>
 * The content of the target MBeanServer as returned by 
 * <tt>queryNames(null,null)</tt> would then appear as:
 * <pre>
 * [...]
 * java.lang:type=Compilation
 * java.lang:type=Threading
 * [...]
 * server1/instance1/java.lang:type=Threading
 * server1/instance1/java.lang:type=Compilation
 * [...]
 * server1/instance2/java.lang:type=Threading
 * server1/instance2/java.lang:type=Compilation
 * [...]
 * </pre>
 * See {@link com.sun.jdmk.remote.cascading} for more details.
 *
 * @since Java DMK 5.1
 **/
public abstract class CascadingAgent 
    implements MBeanRegistration, NotificationEmitter, 
	       CascadingAgentMBean {
  
    /**
     * Construct a new <tt>CascadingAgent</tt> MBean.
     * 
     * @param sourceConnection An <tt>MBeanServerConnectionFactory</tt>
     *        providing connections to the source (cascaded) 
     *        <tt>MBeanServer</tt>.
     *        <p>The <tt>CascadingAgent</tt> will call {@link 
     *        MBeanServerConnectionFactory#getMBeanServerConnection() 
     *        sourceConnection.getMBeanServerConnection()} every time it 
     *        needs to access the subagent.
     *        <p>
     * @param sourcePattern An <tt>ObjectName</tt> pattern that must be 
     *        satisfied by the <tt>ObjectName</tt>s of the source MBeans. 
     *        <p>The sourcePattern is evaluated in the context of the source 
     *        <tt>MBeanServer</tt>. The source pattern is used to perform
     *        a partial mount of the source <tt>MBeanServer</tt> in the target
     *        <tt>MBeanServer</tt>. Only those MBeans that satisfy the pattern
     *        will be mounted. The source pattern is thus a filter
     *        element. A <tt>null</tt> sourcePattern is equivalent to
     *        the wildcard <tt>"*:*"</tt>.
     *        <p>
     * @param sourceQuery A <tt>QueryExp</tt> that must be satisfied by the 
     *        source MBeans. 
     *        <p>The sourceQuery is evaluated in the context of 
     *        the source <tt>MBeanServer</tt>. <b>Using this functionality
     *        is discouraged</b>. It is recommended to always pass a 
     *        <tt>null</tt> parameter. If however, you chose to pass a 
     *        non null <var>sourceQuery</var>, the given <tt>QueryExp</tt> 
     *        should not involve any properties or attributes that vary 
     *        over time.
     *        The evaluation of the <var>sourceQuery</var> against a given 
     *        MBean should be guaranteed to always consistently yield the 
     *        same result. Our implementation does not enforce this 
     *        constraint, but the behavior of the <tt>CascadingAgent</tt> 
     *        in the case where this constraint were not respected is 
     *        unspecified and  could be unpredictable.
     *        <p>
     * @param targetPath The <i>domain path</i> under which the source
     *        MBeans will be mounted in the target <tt>MBeanServer</tt>.
     *        <p>If this parameter is not <tt>null</tt>, all source MBean names
     *        will be transformed in the target <tt>MBeanServer</tt> by 
     *        prefixing their domain name with the string 
     *        <tt><i>targetPath</i>+"/"</tt>. An MBean whose name is
     *        <tt>"D:k1=v1,k2=v2"</tt> will thus be mounted as 
     *        <tt>"<i>targetPath</i>/D:k1=v1,k2=v2"</tt>.
     *        <p>
     *        A <tt>null</tt> <var>targetPath</var> means that MBeans are
     *        mounted directly at the root of the hierarchy, that is, as if
     *        they were local MBeans. <b>Using a null <i>targetPath</i> is
     *        thus highly discouraged, as it could cause name conflicts
     *        in the target <tt>MBeanServer</tt></b>. 
     *        <p>
     *        Similarly, MBeans from different sources should not be
     *        mounted under the same <var>targetPath</var>. Moreover,
     *        an application should not attempt to mount source MBeans under
     *        a <var>targetPath</var> that already contain MBeans in the
     *        target <tt>MBeanServer</tt>.
     *        <p>
     *        However, our implementation does not enforce these rules: 
     *        It is the responsibility of the application creating the
     *        <tt>CascadingAgent</tt> to ensure the consistency of
     *        the mounting strategy.
     *        <p>
     *        <b>Note:</b> A zero-length <var>targetPath</var> is treated 
     *        as a null <var>targetPath</var>.
     *        <p>
     * @param targetMBS The <i>target MBeanServer</i> in which the source 
     *        MBeans will be mounted under <var>targetPath</var>.
     *        <p>If this parameter is null, the target <tt>MBeanServer</tt> 
     *        is the <tt>MBeanServer</tt> passed through the 
     *        {@link MBeanRegistration} interface at registration time.
     *        <p>
     * @exception IllegalArgumentException if <var>targetPath</var> is
     *            not syntactically valid (e.g. it contains wildcard 
     *            characters).
     **/
    protected CascadingAgent(MBeanServerConnectionFactory sourceConnection,
			     ObjectName sourcePattern, QueryExp sourceQuery, 
			     String targetPath, MBeanServer targetMBS) {
	if (targetPath != null && targetPath.length() != 0) {
	    try {
		if (ObjectName.getInstance(targetPath+"/D:k=v").isPattern())
		    throw new IllegalArgumentException("targetPath `" +
						       targetPath +
						       "' contains reserved "+
						       "`*' or `?' " +
						       "characters");
	    } catch (MalformedObjectNameException x) {
		final IllegalArgumentException iae =
		    new IllegalArgumentException("targetPath `" +
						 targetPath +
						 "' is not valid");
		throw (IllegalArgumentException) Utils.initCause(iae,x);
	    }
	} else targetPath = null;

	connectionFactory = sourceConnection;
	emitter = new NotificationBroadcasterSupport() {
		protected void handleNotification(
				      NotificationListener listener,
				      Notification notif, Object handback) {
		    CascadingAgent.this.handleNotification(listener,
							   notif, handback);
		}
	    };
	this.sourcePattern      = sourcePattern;
	this.sourceQuery        = sourceQuery;
	this.targetPath         = targetPath;
	this.targetMBS          = targetMBS;
    }

    private class ConnectionListener 
	implements NotificationListener {
	public void handleNotification(Notification notification, 
				       Object handback) {
	    if (enabled())
	    handleJMXConnectionNotification(notification,handback);
	}
	public synchronized void enable() {
	    enabled=true;
	}
	public synchronized void disable() {
	    enabled=false;
	}
	public synchronized boolean enabled() {
	    return enabled;
	}
	private boolean enabled=false;
    }

    /**
     * This method should only be called by subclasses.
     * Register for {@link JMXConnectionNotification} with the underlying
     * {@link MBeanServerConnectionFactory}. 
     * Calling this method causes the {@link 
     * #handleJMXConnectionNotification(Notification,Object)} method to be
     * called when a <code>JMXConnectionNotification</code> is received
     * through the underlying <code>MBeanServerConnectionFactory</code>.
     * Subclasses are expected to call this method as part of the 
     * implementation of the {@link #start()} method.
     **/
    protected synchronized void enableConnectionNotifications() 
        throws IOException {
	final ConnectionListener l = new ConnectionListener();
	connectionFactory.addConnectionNotificationListener(l,null,null);
	connectionListener = l;
	connectionListener.enable();
    }


    /**
     * This method should only be called by subclasses.
     * Deregister for {@link JMXConnectionNotification} with the underlying
     * {@link MBeanServerConnectionFactory}. 
     * Subclasses are expected to call this method as part of the 
     * implementation of the {@link #stop()} method.
     * @see #enableConnectionNotifications
     **/
    protected synchronized void disableConnectionNotifications() 
        throws IOException {
	try {
	    final ConnectionListener l = connectionListener;
	    if (l == null) return;
	    connectionListener = null;
	    l.disable();
	    connectionFactory.
		removeConnectionNotificationListener(l,null,null);
	} catch (ListenerNotFoundException x) {
	    // OK: Strange, but it is what we wanted anyway.
	}
    }

    // from CascadingAgentMBean
    //
    public final String getTargetPath() {
	return targetPath;
    }

    // from CascadingAgentMBean
    //
    public int getCascadedMBeanCount() {
	return getCascadedMBeans().size();
    }

    // from CascadingAgentMBean
    //
    public abstract Set getCascadedMBeans();


    // from CascadingAgentMBean
    //
    public abstract void start() 
	throws IOException;	
  
    // from CascadingAgentMBean
    //
    public abstract void start(boolean conflictAllowed) 
	throws IOException, InstanceAlreadyExistsException;	
  
    // from CascadingAgentMBean
    //
    public abstract void stop() throws IOException;

    // from CascadingAgentMBean
    //
    public abstract boolean isActive();
  
    // from CascadingAgentMBean
    //
    public abstract String getDescription();

    /**
     * This method is called internally when a
     * {@link JMXConnectionNotification} is received through the underlying
     * {@link MBeanServerConnectionFactory}. 
     * This method is called only if reception of
     * <code>JMXConnectionNotifications</code> has been enabled with
     * {@link #enableConnectionNotifications}.
     * This method is a callback that should never be called directly
     * by subclasses.
     **/
    protected abstract void handleJMXConnectionNotification(Notification n,
							    Object handback);

    // from CascadingAgentMBean
    //
    public final ObjectName getPattern() {
	return sourcePattern;
    }

    // from CascadingAgentMBean
    //
    public final QueryExp getQuery() {
	return sourceQuery;
    }


    // from NotificationEmitter
    //
    public void addNotificationListener(NotificationListener listener,
					NotificationFilter filter,
					Object handback)
	throws java.lang.IllegalArgumentException {
	emitter.addNotificationListener(listener,filter,handback);
    }
    
    // from NotificationEmitter
    //
    public void removeNotificationListener(NotificationListener listener) 
	throws ListenerNotFoundException {
	emitter.removeNotificationListener(listener);
    }
    
    // from NotificationEmitter
    //
    public void removeNotificationListener(NotificationListener listener,
					   NotificationFilter filter,
					   Object handback)
	throws ListenerNotFoundException {
	emitter.removeNotificationListener(listener,filter,handback);
    }


    // from NotificationEmitter
    //
    public MBeanNotificationInfo[] getNotificationInfo() {
	final MBeanNotificationInfo[] info = {
	    jmxConnectionNotificationInfo
	};
	return info;
    }

    /**
     * Sends a notification.
     *   
     * @param notification The notification to send.
     * @see NotificationBroadcasterSupport#sendNotification
     */
    protected void sendNotification(Notification notification) {
	emitter.sendNotification(notification);
    }

    /**
     * <p>This method is called by {@link #sendNotification
     * sendNotification} for each listener in order to send the
     * notification to that listener.  It can be overridden in
     * subclasses to change the behavior of notification delivery,
     * for instance to deliver the notification in a separate
     * thread.</p>
     *
     * <p>It is not guaranteed that this method is called by the same
     * thread as the one that called {@link #sendNotification
     * sendNotification}.</p>
     *
     * <p>The default implementation of this method is equivalent to
     * <pre>
     * listener.handleNotification(notif, handback);
     * </pre>
     *
     * @param listener the listener to which the notification is being
     * delivered.
     * @param notif the notification being delivered to the listener.
     * @param handback the handback object that was supplied when the
     * listener was added.
     * @see NotificationBroadcasterSupport#handleNotification
     */
    protected void handleNotification(NotificationListener listener,
				      Notification notif, Object handback) {
	listener.handleNotification(notif, handback);
    }

    /**
     * The <i>target MBeanServer</i> in which the source 
     * MBeans will be mounted under the <var>target path</var>.
     **/
    public final MBeanServer getTargetMBeanServer() {
	return (targetMBS != null)?targetMBS:myMBS;
    }

    /**
     * Allows the MBean to perform any operations it needs before
     * being registered in the MBean server. 
     * Check that the given <var>name</var> is not null.
     * If the target MBeanServer supplied at construction time was null,
     * then  <var>server</var> becomes the target <tt>MBeanServer</tt>.
     *
     * @param server The MBean server in which the MBean will be registered.
     *
     * @param name The object name of the MBean. If <var>name</var>
     *        this method throws an <tt>IllegalArgumentException</tt>.
     *
     * @return The given <var>name</var>.
     *
     * @exception IllegalArgumentException if the parameter <var>name</var>
     *         is <tt>null</tt>, or if no target <tt>MBeanServer</tt> was 
     *         specified in the constructor and this object is already 
     *         registered in an <tt>MBeanServer</tt>.
     * @see MBeanRegistration#preRegister
     */
    public ObjectName preRegister(MBeanServer server,
				  ObjectName name) 
	throws java.lang.Exception {
	if (name == null) 
	    throw new IllegalArgumentException("Illegal ObjectName: null");
	synchronized (this) {
	    if ((targetMBS == null) && (myMBS != null && myMBS != server))
		throw new IllegalArgumentException("Already registered");
	    myMBS = server;
	}
	return name;
    }

    // from MBeanRegistration 
    //
    public void postRegister(Boolean registrationDone) {
    }


    // from MBeanRegistration 
    //
    public void preDeregister() throws java.lang.Exception {
    }


    // from MBeanRegistration 
    //
    public void postDeregister() { 
	myMBS = null;
    }

    /**
     * Returns the <tt>MBeanServerConnectionFactory</tt>, as passed to this
     * object's constructor.
     **/
    public final MBeanServerConnectionFactory getConnectionFactory() {
	return connectionFactory;
    }

    /**
     * A constant holding the standard ObjectName of the 
     * <tt>MBeanServerDelegate</tt> MBean.
     **/
    public final static ObjectName MBSDelegateObjectName;
    static {
	try {
            MBSDelegateObjectName = 
		new ObjectName("JMImplementation:type=MBeanServerDelegate");
	} catch (MalformedObjectNameException e) {
	    throw new UnsupportedOperationException(e.getMessage());
	}
    }

    private static final String[] jmxConnectionNotificationTypes = {
	JMXConnectionNotification.OPENED,
	JMXConnectionNotification.CLOSED,
	JMXConnectionNotification.FAILED,
	JMXConnectionNotification.NOTIFS_LOST
    };

    private static final MBeanNotificationInfo jmxConnectionNotificationInfo =
	new MBeanNotificationInfo(jmxConnectionNotificationTypes,
				  JMXConnectionNotification.class.getName(),
				  "Notifications relating to the underlying "+
				  "JMX Remote Connection.");

    
    /**
     * The <tt>MBeanServerConnectionFactory</tt>, to the source MBeanServer,
     * as passed to this object's constructor.
     **/
    private final MBeanServerConnectionFactory connectionFactory;

    /**
     * The source <tt>ObjectName</tt> pattern filter, as passed to this
     * object's constructor.
     **/
    private final ObjectName sourcePattern;

    /**
     * The source <tt>QueryExp</tt> query filter, as passed to this
     * object's constructor.
     **/
    private   final QueryExp   sourceQuery;

    private   final String targetPath;
    private   final MBeanServer targetMBS;

    private   final NotificationBroadcasterSupport emitter;
    private   ConnectionListener connectionListener;

    private   MBeanServer myMBS     = null;  
}
