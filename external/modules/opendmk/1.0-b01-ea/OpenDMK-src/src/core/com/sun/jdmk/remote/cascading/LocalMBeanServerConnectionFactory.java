/*
 * @(#)file      LocalMBeanServerConnectionFactory.java
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
import javax.management.ListenerNotFoundException;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServer;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.security.auth.Subject;

/**
 * An {@link MBeanServerConnectionFactory} that wraps a local 
 * <tt>MBeanServerConnection</tt>, e.g, an <tt>MBeanServer</tt>. 
 * The default implementation of this class is to always return the 
 * <tt>MBeanServerConnection</tt> passed to its constructor. 
 * The add/remove connection listener methods are
 * not implemented - they simply do nothing, because the underlying
 * <tt>MBeanServerConnection</tt> is expected to be a local 
 * <tt>MBeanServer</tt>, or an object that wraps a local <tt>MBeanServer</tt>.
 * 
 * @since Java DMK 5.1
 **/
public class LocalMBeanServerConnectionFactory 
    implements MBeanServerConnectionFactory {

    /**
     * Creates a <tt>LocalMBeanServerConnectionFactory</tt> from a local
     * <tt>MBeanServerConnection</tt>. 
     * @param local A local <tt>MBeanServerConnection</tt>. This 
     *        <tt>MBeanServerConnection</tt> is expected to be an an 
     *        {@link MBeanServer}, or an object that wraps an 
     *        <tt>MBeanServer</tt>. The wrapped object should not be a 
     *        remote <tt>MBeanServerConnection</tt> returned by a 
     *        <tt>JMXConnector</tt>. If you wish to create a 
     *        <code>MBeanServerConnectionFactory</code> for a remote
     *        agent, use the {@link BasicMBeanServerConnectionFactory} 
     *        instead.
     * @param localID An ID identifying this local connection. This is the
     *        string that will be returned by {link #getConnectionId()}.
     **/
    public LocalMBeanServerConnectionFactory(MBeanServerConnection local,
					     String localID) {
	this.localConnection=local;
	this.connectionId=localID;
    }

    /**
     * Return the local <code>MBeanServerConnection</code> as passed
     * to the constructor of this object. Usually this 
     * <code>MBeanServerConnection</code> is an {@link MBeanServer}, or an 
     * object that wraps an <tt>MBeanServer</tt>.
     * @return the local <tt>MBeanServerConnection</tt>.
     */
    public final MBeanServerConnection getMBeanServerConnection()
	throws IOException {
	return localConnection;
    }

    /**
     * <p>This implementation does nothing. Since local connections never
     *    change, they never emit notifications.</p>
     * 
     */
    public void
	addConnectionNotificationListener(NotificationListener listener,
					  NotificationFilter filter,
					  Object handback) {
	// localConnection are never broken etc..
    }

    /**
     * <p>This implementation does nothing. Since local connections never
     *    change, they never emit notifications.</p>
     */
    public void
	removeConnectionNotificationListener(NotificationListener listener)
	throws ListenerNotFoundException {
	// localConnection are never broken etc..
    }

    /**
     * <p>This implementation does nothing. Since local connections never
     *    change, they never emit notifications.</p>
     */
    public void removeConnectionNotificationListener(NotificationListener l,
						     NotificationFilter f,
						     Object handback)
	throws ListenerNotFoundException {
	// localConnection are never broken etc..
    }

    /**
     * <p>Returns the connection ID identifying this local connection,
     * as passed to this object's constructor.</p>
     *
     * @return the unique ID of this local connection.
     */
    public final String getConnectionId() throws IOException {
	return connectionId;
    }

    /**
     * Creates a new instance of a {@link LocalMBeanServerConnectionFactory}.
     * This is equivalent to {@link 
     * LocalMBeanServerConnectionFactory#LocalMBeanServerConnectionFactory(
     * MBeanServerConnection,String)
     * new LocalMBeanServerConnectionFactory(s,cid)}.
     * @param s A local <tt>MBeanServer</tt> for which to obtain a 
     *          <tt>MBeanServerConnectionFactory</tt>.
     * @see #getConnectionId()
     **/
    public static MBeanServerConnectionFactory newInstance(MBeanServer s) {
	String mbsid = "unknown_MBeanServerId";
	try {
	    mbsid = (String)
		s.getAttribute(CascadingAgent.MBSDelegateObjectName,
			       "MBeanServerId");
	} catch(Exception x) {
	    // OK: should never happen...
	}
	final String cid="local://"+mbsid;
	return new LocalMBeanServerConnectionFactory(s,cid);
    }

    // Private because it is final, and can be accessed through
    // getMBeanServerConnection().
    //
    final private MBeanServerConnection localConnection;

    // Private because it is final, and can be accessed through
    // getConnectionId().
    //
    final private String connectionId;
}
