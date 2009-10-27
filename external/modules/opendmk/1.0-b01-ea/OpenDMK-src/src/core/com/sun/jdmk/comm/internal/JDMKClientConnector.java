/*
 * Z%file      JDMKClientConnector.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.11
 * @(#)date      07/04/04
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
 *
 */
package com.sun.jdmk.comm.internal;

// java
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.HashSet;
import java.util.Collections;
import java.io.IOException;
import java.net.InetAddress;

// javax
import javax.security.auth.Subject;

// jmx
import javax.management.*;

// rjmx
import javax.management.remote.*;

// jdmk
import com.sun.jdmk.*;
import com.sun.jdmk.comm.*;
import com.sun.jdmk.internal.ClassLogger;

/**
 * This class is used to wrap a JDMK legacy client connector into <code>JMXConnector</code>
 */
public class JDMKClientConnector implements JMXConnector, JdmkLegacyConnector {
    /**
     * Constructs a JMXConnector object which wraps a JDMK legacy connector client.
     * A protocol specified in the serverURL should only be "jdmk-rmi", "jdmk-http" or "jdmk-https".
     */
    public JDMKClientConnector(JMXServiceURL serviceURL, Map env) throws IOException {
	if (serviceURL == null) {
	    throw new IllegalArgumentException("Null url.");
	}

        if (env == null) {
            this.env = Collections.EMPTY_MAP;
        } else {
            this.env = Collections.unmodifiableMap(env);
        } 

	this.address = serviceURL;
	final String protocol = address.getProtocol();

	logger = new ClassLogger(ClassLogger.LOGGER_LEGACY_CLIENT_WRAPPER,
				 "JDMKClientConnector-"+protocol);

	if (logger.fineOn()) {
	    logger.fine("Constructor", "Construct a JDMK legacy connector wrapper: "+address);
	}

	// lookup local host name used to receive notifs
	String local = null;
	try {
	    local = (String)this.env.get(JdmkLegacyConnector.CLIENT_LOCALHOST);
	} catch (ClassCastException e) {
	    IOException ioe = new IOException(e.toString());
	    EnvHelp.initCause(ioe, e);

	    throw ioe;
	}

	// rmi only
	if (JdmkLegacyConnector.RMI_CONNECTOR.equals(protocol)) {
	    RmiConnectorClient client = new RmiConnectorClient(local);

	    mserver = client;
	    nregistry = client;
	    return;
	}

	// now http or https only
	if (JdmkLegacyConnector.HTTP_CONNECTOR.equals(protocol)) {
	    HttpConnectorClient client = new HttpConnectorClient(local);

	    mserver = client;
	    nregistry = client;
	} else if (JdmkLegacyConnector.HTTPS_CONNECTOR.equals(protocol)) {
	    boolean needClientAuth = true;
	    final String needClientAuthStr = (String)
		env.get(JdmkLegacyConnector.HTTPS_SERVER_NEED_CLIENT_AUTH);
	    if (needClientAuthStr != null &&
		"false".equals(needClientAuthStr.toLowerCase()))
		needClientAuth = false;
	    HttpsConnectorClient client =
		new HttpsConnectorClient(local, needClientAuth);

	    mserver = client;
	    nregistry = client;
	} else {
	    throw new IllegalArgumentException("Unknown protocol: "+protocol);
	}
    }

//-------------------------------------
// implement JMXConnector interface
//-------------------------------------
    public void connect() throws IOException {
	connect(null);
    }

    public void connect(Map env) throws IOException {
	if (logger.fineOn()) {
	    logger.fine("connect", "Connecting the client to "+address);
	}

	synchronized(lock) {
	    if (terminated) {
		throw new IOException("The client has been closed.");
	    }

	    if (connected) {
		return;
	    }

	    HashMap tmpEnv = new HashMap(this.env);
	    if (env != null) {
		EnvHelp.checkAttributes(env);
		tmpEnv.putAll(env);
	    }

	    ConnectorAddress addr = null;

	    if (JdmkLegacyConnector.RMI_CONNECTOR.equals(address.getProtocol())) {
		// look for service name
	        String sn = address.getURLPath();

		int i = sn.indexOf("/");
		if (i<0 || sn.length() == i+1) {
		    sn = ServiceName.RMI_CONNECTOR_SERVER;
		} else {
		    sn = sn.substring(i+1);

		    if (sn.trim().equals("")) {
			sn = ServiceName.RMI_CONNECTOR_SERVER;
		    }
		}

		addr = new RmiConnectorAddress(address.getHost(),
					       address.getPort(),
					       sn);
	    } else {// http or https
	        AuthInfo ai = null;

		try {
		    ai = (AuthInfo)tmpEnv.get(JdmkLegacyConnector.HTTP_CLIENT_AUTHINFO);
		} catch (ClassCastException e) {
		    IOException ioe = new IOException(e.toString());
		    EnvHelp.initCause(ioe, e);

		    throw ioe;
		}

		if (ai == null) {
		    addr = new HttpConnectorAddress(address.getHost(),
						    address.getPort());
		} else {
		    addr = new HttpConnectorAddress(address.getHost(),
						    address.getPort(),
						    ai);
		}
	    }

	    // set ID
	    connectionId = address.getProtocol()+"://"+
		InetAddress.getLocalHost().getHostName()+":"+
		address.getPort()+"  "+idCounter++;

	    // add a internal listener to receive HeartBeatNotification
	    ((HeartBeatClientHandler)mserver).addHeartBeatNotificationListener(myListener, null, null);

	    try {
		mserver.connect(addr);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }

	    if (logger.fineOn()) {
		logger.fine("connect", "The client has been connected with the ID "+connectionId);
	    }

	    serverConnection = new MBeanServerConnectionImpl();

	    this.env = tmpEnv;
	    connected = true;
	}
    }

    public void close() throws IOException {
	if (logger.fineOn()) {
	    logger.fine("close", "The client "+connectionId+" is asked to be closed.");
	}

	synchronized(lock) {
	    if (terminated) {
		return;
	    }

	    terminated = true;
	    connected = false;

	    mserver.disconnect();
	}
    }

    public String getConnectionId() throws IOException {
	return connectionId;
    }

    public MBeanServerConnection getMBeanServerConnection()
	throws IOException {

	synchronized(lock) {
	    if (!connected) {
		throw new IOException("Not connected.");
	    }

	    return serverConnection;
	}
    }

    /**
     * Throws UnsupportedOperationException if Subject is not null.
     */
    public MBeanServerConnection getMBeanServerConnection(Subject delegationSubject)
	throws IOException {

	if (delegationSubject == null) {
	    return getMBeanServerConnection();
	}

	throw new UnsupportedOperationException("Sorry.");
    }

    public void addConnectionNotificationListener(NotificationListener listener,
						  NotificationFilter filter,
						  Object handback) {
	connectionBroadcaster.addNotificationListener(listener, filter, handback);
    }

    public void removeConnectionNotificationListener(NotificationListener listener)
	throws ListenerNotFoundException {

	connectionBroadcaster.removeNotificationListener(listener);
    }


    public void removeConnectionNotificationListener(NotificationListener listener,
						     NotificationFilter filter,
						     Object handback)
	throws ListenerNotFoundException {

	connectionBroadcaster.removeNotificationListener(listener, filter, handback);
    }

// --------------------------------------------
// JDMKConnectorWrapper interface
// --------------------------------------------
    public Object getWrapped() {
	return mserver;
    }

//-------------------------------------
// private classes
//-------------------------------------
    private class MBeanServerConnectionImpl implements MBeanServerConnection {

	public ObjectInstance createMBean(String className, ObjectName name)
	    throws ReflectionException,
	    InstanceAlreadyExistsException,
	    MBeanRegistrationException,
	    MBeanException,
	    NotCompliantMBeanException,
	    IOException {

	    try {
		return mserver.createMBean(className, name);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public ObjectInstance createMBean(String className,
					  ObjectName name,
					  ObjectName loaderName)
	    throws ReflectionException,
	    InstanceAlreadyExistsException,
	    MBeanRegistrationException,
	    MBeanException,
	    NotCompliantMBeanException,
	    InstanceNotFoundException,
	    IOException {

	    try {
		return mserver.createMBean(className, name, loaderName);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public ObjectInstance createMBean(String className,
					  ObjectName name,
					  Object[] params,
					  String[] signature)
	    throws ReflectionException,
	    InstanceAlreadyExistsException,
	    MBeanRegistrationException,
	    MBeanException,
	    NotCompliantMBeanException,
	    java.io.IOException {

	    try {
		return mserver.createMBean(className, name, params, signature);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public ObjectInstance createMBean(String className,
					  ObjectName name,
					  ObjectName loaderName,
					  Object[] params,
					  String[] signature)
	    throws ReflectionException,
	    InstanceAlreadyExistsException,
	    MBeanRegistrationException,
	    MBeanException,
	    NotCompliantMBeanException,
	    InstanceNotFoundException,
	    java.io.IOException {

	    try {
		return mserver.createMBean(className, name, loaderName, params, signature);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public void unregisterMBean(ObjectName name)
	    throws InstanceNotFoundException,
	    MBeanRegistrationException,
	    java.io.IOException {

	    try {
		mserver.unregisterMBean(name);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public ObjectInstance getObjectInstance(ObjectName name)
	    throws InstanceNotFoundException,
	    java.io.IOException {

	    try {
		return mserver.getObjectInstance(name);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public Set queryMBeans(ObjectName name, QueryExp query)
	    throws IOException {

	    try {
		return mserver.queryMBeans(name, query);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public Set queryNames(ObjectName name, QueryExp query)
	    throws IOException {

	    try {
		return mserver.queryNames(name, query);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public boolean isRegistered(ObjectName name)
	    throws IOException {

	    try {
		return mserver.isRegistered(name);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public Integer getMBeanCount()
	    throws IOException {
    
	    try {
		return mserver.getMBeanCount();
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public Object getAttribute(ObjectName name, String attribute)
	    throws MBeanException,
	    AttributeNotFoundException,
	    InstanceNotFoundException,
	    ReflectionException,
	    java.io.IOException {

	    try {
		return mserver.getAttribute(name, attribute);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public AttributeList getAttributes(ObjectName name, String[] attributes)
	    throws InstanceNotFoundException,
	    ReflectionException,
	    java.io.IOException {

	    try {
		return mserver.getAttributes(name, attributes);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public void setAttribute(ObjectName name, Attribute attribute)
	    throws InstanceNotFoundException,
	    AttributeNotFoundException,
	    InvalidAttributeValueException,
	    MBeanException,
	    ReflectionException,
	    java.io.IOException {

	    try {
		mserver.setAttribute(name, attribute);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public AttributeList setAttributes(ObjectName name, AttributeList attributes)
	    throws InstanceNotFoundException,
	    ReflectionException,
	    java.io.IOException {

	    try {
		return mserver.setAttributes(name, attributes);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public Object invoke(ObjectName name,
			     String operationName,
			     Object[] params,
			     String[] signature)
	    throws InstanceNotFoundException,
	    MBeanException,
	    ReflectionException,
	    IOException {

	    try {
		return mserver.invoke(name, operationName, params, signature);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public String getDefaultDomain() throws IOException {

	    try {
		return mserver.getDefaultDomain();
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}
	
	public String[] getDomains() throws IOException {
	    Set ns = mserver.queryNames(null, null);

	    HashSet list = new HashSet();
	    for (Iterator iter=ns.iterator(); iter.hasNext();) {
		String d = ((ObjectName)iter.next()).getDomain();

		list.add(d);
	    }

	    try {
		return (String[])list.toArray(new String[list.size()]);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public void addNotificationListener(ObjectName name,
                                    NotificationListener listener,
                                    NotificationFilter filter,
                                    Object handback)
	    throws InstanceNotFoundException,
	    java.io.IOException {

	    try {
		nregistry.addNotificationListener(name, listener, filter, handback);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public void addNotificationListener(ObjectName name,
					    ObjectName listener,
					    NotificationFilter filter,
					    Object handback)
	    throws InstanceNotFoundException,
	    IOException {

	    try {
		throw new UnsupportedOperationException("Sorry.");
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public void removeNotificationListener(ObjectName name, ObjectName listener)
	    throws InstanceNotFoundException,
	    ListenerNotFoundException,
	    IOException {

	    throw new UnsupportedOperationException("Sorry.");
	}

	public void removeNotificationListener(ObjectName name,
					       ObjectName listener,
					       NotificationFilter filter,
					       Object handback)
	    throws InstanceNotFoundException,
	    ListenerNotFoundException,
	    IOException {

	    throw new UnsupportedOperationException("Sorry.");
	}

	public void removeNotificationListener(ObjectName name, NotificationListener listener)
	    throws InstanceNotFoundException,
	    ListenerNotFoundException,
	    IOException {

	    try {
		nregistry.removeNotificationListener(name, listener);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public void removeNotificationListener(ObjectName name,
					       NotificationListener listener,
					       NotificationFilter filter,
					       Object handback)
	    throws InstanceNotFoundException, ListenerNotFoundException, IOException {

	    throw new UnsupportedOperationException("Sorry.");
	}

	public MBeanInfo getMBeanInfo(ObjectName name)
	    throws InstanceNotFoundException,
	    IntrospectionException,
	    ReflectionException,
	    IOException {

	    try {
		return mserver.getMBeanInfo(name);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}

	public boolean isInstanceOf(ObjectName name, String className)
	    throws InstanceNotFoundException, IOException {

	    try {
		return mserver.isInstanceOf(name, className);
	    } catch (CommunicationException ce) {
		throw wrapToIO(ce);
	    }
	}
    }

    // used to receive HeartBeatNotification
    private class HeartBeatNotifListener implements NotificationListener {
	public void handleNotification(Notification notif, Object hd) {
	    HeartBeatNotification hbn = (HeartBeatNotification)notif;

	    if (HeartBeatNotification.CONNECTION_ESTABLISHED.equals(hbn.getType())) {
		final JMXConnectionNotification jn = new JMXConnectionNotification(
							  JMXConnectionNotification.OPENED,
							  this,
							  connectionId,
							  nextSequenceNumber(),
							  "The client has been connected.",
							  null);
		connectionBroadcaster.sendNotification(jn);
	    } else if (HeartBeatNotification.CONNECTION_LOST.equals(hbn.getType())) {
		final JMXConnectionNotification jn = new JMXConnectionNotification(
							  JMXConnectionNotification.FAILED,
							  this,
							  connectionId,
							  nextSequenceNumber(),
							  "The connection to the server is lost.",
							  null);
		connectionBroadcaster.sendNotification(jn);
	    } else if (HeartBeatNotification.CONNECTION_REESTABLISHED.equals(hbn.getType())) {
		final JMXConnectionNotification jn = new JMXConnectionNotification(
							  JMXConnectionNotification.OPENED,
							  this,
							  connectionId,
							  nextSequenceNumber(),
							  "The client has been connected.",
							  null);
		connectionBroadcaster.sendNotification(jn);
	    } else if (HeartBeatNotification.CONNECTION_RETRYING.equals(hbn.getType())) {
		// do nothing
	    } else if (HeartBeatNotification.CONNECTION_TERMINATED.equals(hbn.getType())) {
		final JMXConnectionNotification jn = new JMXConnectionNotification(
							  JMXConnectionNotification.CLOSED,
							  this,
							  connectionId,
							  nextSequenceNumber(),
							  "The client has been connected.",
							  null);
		connectionBroadcaster.sendNotification(jn);

		if (terminated) {// make sure that it is terminated from the wrapped.
		    try {
			connectionBroadcaster.removeNotificationListener(myListener);
		    } catch (Exception e) {
			// OK.
			// It is terminated
		    }
		}
	    }
	}

	private synchronized long nextSequenceNumber() {
	    return sequenceCounter++;
	}

	private long sequenceCounter = 0;
    }

//-------------------------------------
// private methods
//-------------------------------------
    private IOException wrapToIO(CommunicationException ce) {		
	IOException ioe = new IOException(ce.toString());
	if (ce.getTargetException() != null) {
	    EnvHelp.initCause(ioe, ce.getTargetException());
	} else {
	    EnvHelp.initCause(ioe, ce);
	}

	return ioe;
    }

//-------------------------------------
// private variables
//-------------------------------------
    private MBeanServerConnectionImpl serverConnection;

    private final RemoteMBeanServer mserver;
    private final NotificationRegistration nregistry;

    private Map env;
    private JMXServiceURL address;

    private String connectionId = null;
    private static int idCounter = 0;

    private boolean connected = false;
    private boolean terminated = false;
    private final int[] lock = new int[0];

    private final NotificationBroadcasterSupport connectionBroadcaster
	= new NotificationBroadcasterSupport();
    private final HeartBeatNotifListener myListener = new HeartBeatNotifListener();

    private ClassLogger logger;
}
