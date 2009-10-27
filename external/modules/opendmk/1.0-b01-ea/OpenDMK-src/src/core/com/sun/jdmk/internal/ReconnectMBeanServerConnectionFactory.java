/*
 * @(#)file      ReconnectMBeanServerConnectionFactory.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.13
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

package com.sun.jdmk.internal;

import java.io.IOException;
import java.io.Serializable;

import java.util.Map;

import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectionNotification;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanServerConnection;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.MBeanServer;

import javax.security.auth.Subject;

import javax.management.remote.JMXServiceURL;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;

import com.sun.jdmk.remote.cascading.BasicMBeanServerConnectionFactory;
import com.sun.jdmk.remote.cascading.MBeanServerConnectionFactory;

/**
 * A basic {@link MBeanServerConnectionFactory} that wraps a 
 * {@link JMXConnector}.
 * This implementation does support transparent reconnection for JMXConnector
 * created with a deterministic JMXService URL.
 * The reconnection is bound by an envelop time. Reconnection occurs after 
 * a period of time. Both values are expressed in milliseconds and are provided 
 * at construction time. If envelopTime == 0, no reconnection is done.
 **/
public class ReconnectMBeanServerConnectionFactory 
    extends BasicMBeanServerConnectionFactory implements Runnable, 
							 Serializable {
    private static final long serialVersionUID = -5764359043690071419L;
    
    private long envelopTime;
    private long period;
    private boolean stop = false;
    private transient boolean failed = false;

    private final NotificationBroadcasterSupport broadcaster = 
	new NotificationBroadcasterSupport();

    private JMXServiceURL url;
    private Map map;
    
    private String currentConnectionId;
    private transient JMXConnector connector = null;
    private transient MBeanServerConnection connection = null;
    private final NotificationListener listener;
 

    /**
     * Creates a new <tt>ReconnectMBeanServerConnectionFactory</tt> for 
     * the given {@link JMXServiceURL url.}
     * @param url Used to create JMXConnector.
     * @param map Used to create JMXConnector.
     * @param envelopTime Reconnect time limit. If envelopTime == 0, 
     * no reconnection is done.
     * 
     * @throws IOException If connection fails.
     */
    public ReconnectMBeanServerConnectionFactory(JMXServiceURL url, 
						 Map map,
						 long envelopTime,
						 long period) 
	throws IOException {
	super(null, null);
	this.url = url;
	this.map = map;
	this.envelopTime = envelopTime;
	this.period = period;
	this.listener = 
	    new NotificationListener() {
		    public void handleNotification(Notification n, 
						   Object handback) {
			handleConnectionNotification(n,handback);
		    }
		};
	if(getJMXConnector() == null)
	    if(envelopTime == 0)
		throw new IOException("Unable to connect to " + url);
	    else
		reconnect();
	else {
	    currentConnectionId = getConnectionId();
	}
    }
    
    /*
     * Return the url used to create and connector the JMXConnector.
     * @return the url.
     */
    public JMXServiceURL getURL() {
	return url;
    }

    private static JMXConnector makeConnector(JMXServiceURL url, 
					      Map map)  
	throws IOException {
	JMXConnector c = null;
	try {
	    c = JMXConnectorFactory.connect(url, map);
	}catch(IOException e) {
	    //
	}
	return c;
    }
    
    /**
     * Call this method to stop the ongoing reconnection (if any).
     * In case reconnection is ongoing, "jmx.remote.connection.failed" 
     * notification is sent.
     */
    public synchronized void stopReconnection() {
	stop = true;
    }

    public void run() {
	long startTime = System.currentTimeMillis();
	long currentTime;
	JMXConnector old = getJMXConnector();
	String connectionId = null;
	do{
	    try {
		JMXConnector c = makeConnector(url, map);

		if(c == null) throw new Exception("Not connected");

		c.addConnectionNotificationListener(listener, 
						    null, 
						    null);
		synchronized (this) {
		    connector=c;
		    connection = null;
		}

		connectionId = c.getConnectionId();
		JMXConnectionNotification notif = 
		    new JMXConnectionNotification("jmx.remote.connection.open",
						  "",
						  connectionId,
						  0,
						  "",
						  "");
		broadcaster.sendNotification(notif);
		return;
	    }catch(Exception e) {
		//e.printStackTrace();
		try {
		    Thread.sleep(period);
		}catch(Exception ex) {}
	    }
	    currentTime =  System.currentTimeMillis();
	}while(((currentTime - startTime) < envelopTime) && !stop);
	
	if(!stop) {
	    if(currentConnectionId == null)
		currentConnectionId = "NeverConnectedConnection";
	    
	    JMXConnectionNotification notif = 
		new JMXConnectionNotification("jmx.remote.connection.failed",
					      "",
					      currentConnectionId,
					      0,
					      "",
					      "");
	    broadcaster.sendNotification(notif);
	    try {
		currentConnectionId = getConnectionId();
	    }catch(Exception e) {
		currentConnectionId = null;
	    }
	}
    }
    
    private void reconnect() {
	//Launch reconnection
	if(envelopTime > 0) {
	    Thread thread = new Thread(this);
	    thread.start();
	}	
    }

    public void handleConnectionNotification(Notification notification, 
					     java.lang.Object handback) {
	if(notification.getType().equals("jmx.remote.connection.failed")) {
	    //if(notification.getMessage().startsWith("Failed to communicate with the server: java.rmi.UnmarshalException: error unmarshalling return; nested exception is:")) {
	    //System.out.println("WORKAROUND, DO NOTHING");
	    //return;
	    //}

	    //Remove from failed connector
	    try {
		getJMXConnector().
		    removeConnectionNotificationListener(listener);
	    }catch(Exception e) {
		//We should have it all the time.
		//Don't now if we are automaticaly removed on failed?
	    }
	    reconnect();
	} else
	    broadcaster.sendNotification(notification);
    }
    
    // MBeanServerConnectionFactory
    // 
    public void
	addConnectionNotificationListener(NotificationListener l,
					  NotificationFilter filter,
					  Object handback) {
	broadcaster.addNotificationListener(l,
					    filter,
					    handback);
    }

    // MBeanServerConnectionFactory
    // 
    public void
	removeConnectionNotificationListener(NotificationListener l)
	throws ListenerNotFoundException {
	broadcaster.removeNotificationListener(l);
    }

    // MBeanServerConnectionFactory
    // 
    public void removeConnectionNotificationListener(NotificationListener l,
						     NotificationFilter f,
						     Object handback)
	throws ListenerNotFoundException {
	broadcaster.removeNotificationListener(l,f,handback);
    }
    
    /**
     * Creates a new <tt>ReconnectMBeanServerConnectionFactory</tt> for 
     * the given {@link JMXServiceURL url.}
     * @param url Used to create JMXConnector.
     * @param map Used to create JMXConnector.
     * @param envelopTime Reconnect time limit. If envelopTime == 0 no 
     * reconnection is done.
     *
     * @throws IOException If connection fails.
     */
    public static MBeanServerConnectionFactory newInstance(JMXServiceURL url,
							   Map map,
							   long envelopTime,
							   long period) 
	throws IOException {
	return new ReconnectMBeanServerConnectionFactory(url,
							 map,
							 envelopTime,
							 period);
    }

    public synchronized JMXConnector getJMXConnector() {
	try {
	    if (connector == null) {
		connector = makeConnector(url, map); 
		connector.addConnectionNotificationListener(listener,null,
							    connector);
		connection = null;
	    }
	} catch (IOException io) {
	    // OK. return old one if any. 
	}
	return connector;
    }

    public synchronized MBeanServerConnection getMBeanServerConnection()
	throws IOException {
	if (failed) throw new IOException("connection already failed");
	if (getJMXConnector()==null) 
	    throw new IOException("Unable to connect");
	if (connection == null) 
	    connection = getJMXConnector().
		getMBeanServerConnection(getDelegationSubject());
	return this.connection;
    }

}
