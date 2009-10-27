/*
 * @(#)file      BasicMBeanServerConnectionFactory.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.9
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

import java.io.IOException;
import java.util.Map;

import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectionNotification;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanServerConnection;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.MBeanServer;

import javax.security.auth.Subject;

import javax.management.remote.JMXServiceURL;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectionNotification;
import javax.management.remote.JMXConnectorFactory;

/**
 * A basic {@link MBeanServerConnectionFactory} that wraps a 
 * {@link JMXConnector}.
 * This implementation does not support transparent reconnection.
 *
 * @since Java DMK 5.1
 **/
public class BasicMBeanServerConnectionFactory 
    implements MBeanServerConnectionFactory {

    /**
     * Creates a new <tt>BasicMBeanServerConnectionFactory</tt> for the given
     * {@link JMXConnector connector} with the given 
     * {@link Subject delegationSubject}.
     * The given connector must be connected before the factory can be used.
     * @param connector A <tt>JMConnector</tt> from which to obtain the 
     *        <tt>MBeanServerConnection</tt>.
     * @param delegationSubject A delegation subject used to obtain
     *        the underlying {@link MBeanServerConnection}.
     * @see JMXConnector#getMBeanServerConnection(Subject)
     */
    public BasicMBeanServerConnectionFactory(JMXConnector connector,
					     Subject delegationSubject) {
	this.connector=connector;
	this.subject=delegationSubject;
	this.connection=null;
	this.emitter = new NotificationBroadcasterSupport();
	this.listener = new NotificationListener() {
		public void handleNotification(Notification n, 
					       Object handback) {
			handleConnectionNotification(n,handback);
		}
	    };
	this.failed = false;
	if (connector != null) 
	    this.connector.addConnectionNotificationListener(listener,null,
							     connector);
    }

    /**
     * Returns the underlying <code>JMXConnector</code> used by this
     * object. This is the connector that was passed to this object's
     * constructor.
     * @return the underlying <tt>JMXConnector</tt>.
     **/
    public synchronized JMXConnector getJMXConnector() {
	return connector;
    }

    /**
     * The delegation subject used by the underlying 
     * <tt>MBeanServerConnection</tt>.
     * This is the {@link Subject} that was passed to this object's 
     * constructor.
     * @see JMXConnector#getMBeanServerConnection(Subject)
     **/
    public final Subject getDelegationSubject() {
	return subject;
    }


    /**
     * <p>Returns an <code>MBeanServerConnection</code> object
     * representing a remote MBean server.</p>  
     * <p>This implementation always return the same 
     * <tt>MBeanServerConnection</tt> object.
     * The first time <code>getMBeanServerConnection()</code> is called,
     * this method will obtain a new 
     * <code>MBeanServerConnection</code> by calling <code>
     * getJMXConnector().getMBeanServerConnection(getDelegationSubject());
     * </code></p>
     * This same <code>MBeanServerConnection</code> will then be returned by
     * subsequent calls. This behaviour can be changed by subclasses.
     *
     * @return an object that implements the
     * <code>MBeanServerConnection</code> interface by forwarding its
     * methods to the remote MBean server.
     *
     * @exception IOException if a valid
     * <code>MBeanServerConnection</code> cannot be created.
     *
     * @see JMXConnector#getMBeanServerConnection()
     * @see JMXConnector#getMBeanServerConnection(Subject)
     */
    public synchronized MBeanServerConnection getMBeanServerConnection()
	throws IOException {
	if (failed) throw new IOException("connection already failed");
	if (this.connection == null) 
	    this.connection = connector.getMBeanServerConnection(subject);
	return this.connection;
    }

    // MBeanServerConnectionFactory
    // 
    public void
	addConnectionNotificationListener(NotificationListener listener,
					  NotificationFilter filter,
					  Object handback) {
	emitter.addNotificationListener(listener,
					filter,
					handback);
    }

    // MBeanServerConnectionFactory
    // 
    public void
	removeConnectionNotificationListener(NotificationListener listener)
	throws ListenerNotFoundException {
	emitter.removeNotificationListener(listener);
    }

    // MBeanServerConnectionFactory
    // 
    public void removeConnectionNotificationListener(NotificationListener l,
						     NotificationFilter f,
						     Object handback)
	throws ListenerNotFoundException {
	emitter.removeNotificationListener(l,f,handback);
    }

    // MBeanServerConnectionFactory
    // 
    public String getConnectionId() throws IOException {
	if (failed) throw new IOException("connection already failed");
	return getJMXConnector().getConnectionId();
    }

    /**
     * Creates a new instance of the 
     * <tt>BasicMBeanServerConnectionFactory</tt>.
     * This is equivalent to {@link #newInstance(JMXServiceURL,Map,Subject)
     * newInstance(url,null,null)}.
     * @param url A JMX Service URL from which to create a 
     *  <tt>JMXConnector</tt>.
     * @throws IOException if a connected connector cannot be obtained
     *         from the {@link JMXConnectorFactory}.
     **/
    public static MBeanServerConnectionFactory newInstance(JMXServiceURL url) 
	throws IOException {
	return newInstance(url,null,null);
    }

    /**
     * Creates a new instance of the 
     * <tt>BasicMBeanServerConnectionFactory</tt>.
     * This is equivalent to {@link #newInstance(JMXServiceURL,Map,Subject)
     * newInstance(url,map,null)}.
     * @param url A JMX Service URL from which to create a 
     * <tt>JMXConnector</tt>.
     * @param map An attributes map passed to the {@link 
     *            JMXConnectorFactory#connect(JMXServiceURL,Map)} method.
     * @throws IOException if a connected connector cannot be obtained
     *         from the {@link JMXConnectorFactory}.
     **/
    public static MBeanServerConnectionFactory newInstance(JMXServiceURL url,
							   Map map) 
	throws IOException {
	return newInstance(url,map,null);

    }

    /**
     * Creates a new instance of the  
     * <tt>BasicMBeanServerConnectionFactory</tt>.
     * This is equivalent to {@link #newInstance(JMXConnector,Subject)
     * newInstance(JMXConnectorFactory.connect(url,map),subject)}.
     * @param url A JMX Service URL from which to create a 
     * <tt>JMXConnector</tt>.
     * @param map An attributes map passed to the {@link 
     *            JMXConnectorFactory#connect(JMXServiceURL,Map)} method.
     * @param subject A subject for the underlying 
     * <tt>MBeanServerConnection</tt>.
     * @throws IOException if a connected connector cannot be obtained
     *         from the {@link JMXConnectorFactory}.
     * @see JMXConnector#getMBeanServerConnection(Subject)
     **/
    public static MBeanServerConnectionFactory newInstance(JMXServiceURL url,
							   Map map,
							   Subject subject) 
	throws IOException {
	return newInstance(JMXConnectorFactory.connect(url,map),subject);
    }

    /**
     * Creates a new instance of the   
     * <tt>BasicMBeanServerConnectionFactory</tt>.
     * This is equivalent to {@link 
     * #BasicMBeanServerConnectionFactory(JMXConnector,Subject)
     * new BasicMBeanServerConnectionFactory(c,null)}.
     * @param c A <tt>JMXConnector</tt>. The connector must be connected 
     *        before the returned factory can be used.
     **/
    public static MBeanServerConnectionFactory newInstance(JMXConnector c) {
	return newInstance(c,null);
    }

    /**
     * Creates a new instance of the    
     * <tt>BasicMBeanServerConnectionFactory</tt>.
     * This is equivalent to {@link 
     * #BasicMBeanServerConnectionFactory(JMXConnector,Subject)
     * new BasicMBeanServerConnectionFactory(c,subject)}.
     * @param c A <tt>JMXConnector</tt>. The connector must be connected 
     *        before the returned factory can be used.
     * @param subject A subject for the underlying 
     *        <tt>MBeanServerConnection</tt>.
     * @see JMXConnector#getMBeanServerConnection(Subject)
     **/
    public static MBeanServerConnectionFactory newInstance(JMXConnector c,
							   Subject subject) {
	return new BasicMBeanServerConnectionFactory(c,subject);
    }

    private void handleConnectionNotification(Notification n, 
					      Object handback) {

	try {
	    if (JMXConnectionNotification.FAILED.equals(n.getType())) {
		synchronized(this) {
		    if (!failed && handback == getJMXConnector()) {
			failed = true;
		    }
		}
	    }
	} catch (Exception x) {
	    // OK. Don't want to know...
	} finally {
	    // System.err.println("MBSCF: Forwarding " + n.getType() );
	    emitter.sendNotification(n);
	}
    }

    // Subject
    //
    private final Subject subject;

    // JMXConnector. 
    // Can be accessed - getJMXConnector()  by subclasses.
    //
    private final JMXConnector  connector;

    // The underlying MBeanServerConnection. Lazy evaluation done by
    // getMBeanServerConnection(); Access should protected by 
    // synchronized() block. 
    // Can be accessed by subclasses using getMBeanServerConnection();
    //
    private MBeanServerConnection connection;


    private boolean failed = false;

    private final NotificationBroadcasterSupport  emitter; 
    private final NotificationListener            listener; 

    
}
