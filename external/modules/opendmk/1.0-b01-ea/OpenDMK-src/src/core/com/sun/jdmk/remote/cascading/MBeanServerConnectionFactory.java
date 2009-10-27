/*
 * @(#)file      MBeanServerConnectionFactory.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.8
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
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.security.auth.Subject;

/**
 * An object that is able to return connections to a given MBeanServer.
 * The MBeanServer could be local ({@link LocalMBeanServerConnectionFactory})
 * or remote ({@link BasicMBeanServerConnectionFactory}). 
 * <p>Implementations of the {@link MBeanServerConnectionFactory} may or may
 * not support transparent reconnection when the underlying connection is
 * abruptly closed (server going down, network failure, etc...). 
 * Whether transparent reconnection is supported or not should be 
 * transparent to the client code.</p>
 * <p>If transparent reconnection is supported, then the
 * {@link MBeanServerConnectionFactory} implementation is responsible for
 * transparently initiating a new connection with the server side, by, e.g,
 * obtaining a new <code>JMXConnector</code> from a lookup service.</p>
 * <p>The {@link MBeanServerConnectionFactory} emits 
 * {@link JMXConnectionNotification} relating to the underlying connection:
 * <ul><li>{@link JMXConnectionNotification#OPENED}: The connection with
 *     the underlying MBeanServer was (re)opened.</li>
 * <li>{@link JMXConnectionNotification#CLOSED}: The connection with
 *     the underlying MBeanServer was closed.</li>
 * <li>{@link JMXConnectionNotification#FAILED}: The connection with
 *     the underlying MBeanServer failed - and can no longer be used.</li>
 * </ul>
 * Implementations that support transparent reconnection could only
 * emit {@link JMXConnectionNotification#OPENED} and {@link 
 * JMXConnectionNotification#CLOSED} notifications.
 * Implementations that do not support transparent reconnection could 
 * emit all three notifications. An implementation providing connections to
 * a local MBeanServer could emit none of them.
 * </p>
 *
 * @since Java DMK 5.1
 **/
public interface MBeanServerConnectionFactory {

    /**
     * <p>Returns an <code>MBeanServerConnection</code> object
     * representing a remote MBean server.  For a given
     * <code>MBeanServerConnectionFactory</code>, two successful 
     * calls to this method will usually return the same 
     * <code>MBeanServerConnection</code>
     * object, though this is not required. In particular, if
     * the <tt>MBeanServerConnectionFactory</tt> handles transparent 
     * reconnection after a server/network failure, it could return a new 
     * <tt>MBeanServerConnection</tt> obtained from a new 
     * {@link JMXConnector}</p>
     * 
     * @return an object that implements the
     * <code>MBeanServerConnection</code> interface by forwarding its
     * methods to the remote MBean server.
     *
     * @exception IOException if a valid
     * <code>MBeanServerConnection</code> cannot be created.
     *
     * @see JMXConnector#getMBeanServerConnection()
     */
    public MBeanServerConnection getMBeanServerConnection()
	    throws IOException;

    /**
     * <p>Adds a listener to be informed of changes in connection
     * status.  The listener will receive notifications of type {@link
     * JMXConnectionNotification}.  An implementation can send other
     * types of notifications too.</p>
     *
     * <p>Any number of listeners can be added with this method.  The
     * same listener can be added more than once with the same or
     * different values for the filter and handback.  There is no
     * special treatment of a duplicate entry.  For example, if a
     * listener is registered twice with no filter, then its
     * <code>handleNotification</code> method will be called twice for
     * each notification.</p>
     *
     * @param listener a listener to receive connection status
     * notifications.
     * @param filter a filter to select which notifications are to be
     * delivered to the listener, or null if all notifications are to
     * be delivered.
     * @param handback an object to be given to the listener along
     * with each notification.  Can be null.
     *
     * @exception NullPointerException if <code>listener</code> is
     * null.
     *
     * @see #removeConnectionNotificationListener
     * @see NotificationBroadcaster#addNotificationListener
     */
    public void
	addConnectionNotificationListener(NotificationListener listener,
					  NotificationFilter filter,
					  Object handback);

    /**
     * <p>Removes a listener from the list to be informed of changes
     * in status.  The listener must previously have been added.  If
     * there is more than one matching listener, all are removed.</p>
     *
     * @param listener a listener to receive connection status
     * notifications.
     *
     * @exception NullPointerException if <code>listener</code> is
     * null.
     *
     * @exception ListenerNotFoundException if the listener is not
     * registered with this <code>JMXConnector</code>.
     *
     * @see #removeConnectionNotificationListener(NotificationListener,
     * NotificationFilter, Object)
     * @see #addConnectionNotificationListener
     * @see NotificationEmitter#removeNotificationListener
     */
    public void
	removeConnectionNotificationListener(NotificationListener listener)
	    throws ListenerNotFoundException;

    /**
     * <p>Removes a listener from the list to be informed of changes
     * in status.  The listener must previously have been added with
     * the same three parameters.  If there is more than one matching
     * listener, only one is removed.</p>
     *
     * @param l a listener to receive connection status notifications.
     * @param f a filter to select which notifications are to be
     * delivered to the listener.  Can be null.
     * @param handback an object to be given to the listener along
     * with each notification.  Can be null.
     *
     * @exception ListenerNotFoundException if the listener is not
     * registered with this <code>JMXConnector</code>, or is not
     * registered with the given filter and handback.
     *
     * @see #removeConnectionNotificationListener(NotificationListener)
     * @see #addConnectionNotificationListener
     * @see NotificationEmitter#removeNotificationListener
     */
    public void removeConnectionNotificationListener(NotificationListener l,
						     NotificationFilter f,
						     Object handback)
	    throws ListenerNotFoundException;

    /**
     * <p>Gets the current connection's ID from the connector server.  For a
     * given connector server, every connection will have a unique id
     * which does not change during the lifetime of the
     * connection.</p>
     * <p>If this <tt>MBeanServerConnectionFactory</tt> provides access to
     * a remote MBeanServer, then the connection ID will be the ID of the
     * underlying <tt>JMXConnector</tt>'s connection currently in use. The
     * {@link javax.management.remote javax.management.remote 
     * package description} describes the conventions for such connection 
     * IDs. Otherwise, the format of the connection ID is undefined.
     *
     * @return the unique ID of the current connection.  This is the same as
     * the ID that the connector server includes in its {@link
     * JMXConnectionNotification}s.  
     *
     * @exception IOException if the connection ID cannot be obtained,
     * for instance because the connection is closed or broken.
     */
    public String getConnectionId() throws IOException;

}

