/*
 * @(#)file      JDMKServerConnector.java
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

// javax
import java.io.IOException;
import java.net.InetAddress;
import java.util.Map;
import java.util.HashMap;
import java.util.Collection;
import java.util.Collections;
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
 * This class is used to wrap a JDMK legacy server connector into
 * <code>JMXConnectorServer</code>
 */
public class JDMKServerConnector
    extends JMXConnectorServer
    implements JdmkLegacyConnector {

    /**
     * Constructs a JMXConnectorServer which wraps a JDMK legacy connector
     * server.
     *
     * The protocol specified in the serverURL should only be jdmk-rmi,
     * jdmk-http or jdmk-htts.
     */
    public JDMKServerConnector(JMXServiceURL serviceURL,
			       Map env, MBeanServer server)
	throws IOException {
	super(server);

	if (serviceURL == null) {
	    throw new IllegalArgumentException("Null url.");
	}

	address = serviceURL;

	logger = new ClassLogger(ClassLogger.LOGGER_LEGACY_SERVER_WRAPPER,
				 "JDMKClientConnector-"+address.getProtocol());

	if (logger.finerOn()) {
	    logger.fine("Constructor",
			"Construct a JDMK legacy connector server wrapper: " +
			address);
	}

        if (env == null) {
            this.env = Collections.EMPTY_MAP;
        } else {
            this.env = Collections.unmodifiableMap(env);
        }

	final String protocol = address.getProtocol();

	if (JdmkLegacyConnector.HTTP_CONNECTOR.equals(protocol) ||
	    JdmkLegacyConnector.HTTPS_CONNECTOR.equals(protocol)) {
	    wrapped = getHttpConnectorServer(address);
	} else if (JdmkLegacyConnector.RMI_CONNECTOR.equals(protocol)) {
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

	    wrapped = new RmiConnectorServer(serviceURL.getPort(), sn);
	} else {
	    throw new IllegalArgumentException("Unknown protocol: "+protocol);
	}
    }

    private CommunicatorServer getHttpConnectorServer(JMXServiceURL address)
	throws IOException {
	if (JdmkLegacyConnector.HTTP_CONNECTOR.equals(address.getProtocol()))
	    return new HttpConnectorServer(address.getPort(),
					   getAuthInfo(),
					   getLocalAddress());
	else
	    return new HttpsConnectorServer(address.getPort(),
					    getAuthInfo(),
					    getLocalAddress(),
					    getNeedClientAuth());
    }

    private AuthInfo[] getAuthInfo() throws IOException {
	try {
	    return (AuthInfo[])
	        env.get(JdmkLegacyConnector.HTTP_SERVER_AUTHINFO_LIST);
	} catch (ClassCastException e) {
	    IOException ioe = new IOException(e.toString());
	    EnvHelp.initCause(ioe, e);
	    throw ioe;
	}
    }

    private InetAddress getLocalAddress() throws IOException {
	try {
	    return (InetAddress)
		env.get(JdmkLegacyConnector.HTTP_SERVER_LOCAL_ADDRESS);
	} catch (ClassCastException e) {
	    IOException ioe = new IOException(e.toString());
	    EnvHelp.initCause(ioe, e);
	    throw ioe;
	}
    }

    private boolean getNeedClientAuth() throws IOException {
	try {
	    final String needClientAuth = (String)
		env.get(JdmkLegacyConnector.HTTPS_SERVER_NEED_CLIENT_AUTH);
	    if (needClientAuth != null &&
		"false".equals(needClientAuth.toLowerCase()))
		return false;
	    return true;
	} catch (ClassCastException e) {
	    IOException ioe = new IOException(e.toString());
	    EnvHelp.initCause(ioe, e);
	    throw ioe;
	}
    }

// --------------------------------------------
// JMXConnectorServerMBean interface
// --------------------------------------------
    public JMXServiceURL getAddress() {
	return address;
    }

    public Map getAttributes() {
        Map map = EnvHelp.filterAttributes(env);
        return Collections.unmodifiableMap(map);
    }

    public boolean isActive() {
	return wrapped.isActive();
    }

    public void start() throws IOException {
	if (logger.finerOn()) {
	    logger.fine("start", "start the server: "+address);
	}

	Object hb = new int[0];
	MyListener ml = new MyListener(hb);
	wrapped.addNotificationListener(ml, null, hb);

	try {
	    wrapped.start();
	} catch (CommunicationException ce) {
	    IOException ioe = new IOException(ce.toString());
	    if (ce.getTargetException() != null) {
		EnvHelp.initCause(ioe, ce.getTargetException());
	    } else {
		EnvHelp.initCause(ioe, ce);
	    }
	    throw ioe;
	}

	// waiting server state change
	synchronized(hb) {
	    while(!ml.stateChanged) {
		try {
		    hb.wait();
		} catch (InterruptedException ie) {
		    break;
		}
	    }
	}

	try {
	    wrapped.removeNotificationListener(ml);
	} catch (Exception e) {
	    // OK.
	    // never?
	}

	if (!wrapped.isActive()) {
	    throw new IOException("Failed to start the server, " +
				  "the server is not active.");
	}
    }

    public void stop() throws IOException {
	if (logger.finerOn()) {
	    logger.fine("stop", "stop the server: "+address);
	}

	wrapped.stop();
    }

    public String[] getConnectionIds() {
	throw new UnsupportedOperationException("Sorry.");
    }

// --------------------------------------------
// JDMKConnectorWrapper interface
// --------------------------------------------
    public Object getWrapped() {
	return wrapped;
    }

// --------------------------------------------
// Interface MBeanRegistration: overwrite
// --------------------------------------------
    public ObjectName preRegister(MBeanServer server, ObjectName name) {
	super.preRegister(server, name);

	try {
	    return ((MBeanRegistration)wrapped).preRegister(server, name);
	} catch (Exception e) {
	    // OK.
	    // JMXConnectorServer.preRegister() does not throw an Exception
	}

	return name;
    }

    public void postRegister(Boolean registrationDone) {
	((MBeanRegistration)wrapped).postRegister(registrationDone);
    }

    public void preDeregister() throws Exception {
	((MBeanRegistration)wrapped).preDeregister();
    }

// --------------------------------------------
// Interface NotificationBroadcaster: overwrite
// --------------------------------------------
    /**
     * Returns an empty <code>MBeanNotificationInfo</code> list.
     * This server wrapper implementation does not send notification.
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
	return new MBeanNotificationInfo[0];
    }

// --------------------------------------------
// private stuff
// --------------------------------------------
    private class MyListener implements NotificationListener {
	public MyListener(Object handback) {
	    this.handback = handback;
	}

	public void handleNotification(Notification n, Object hb) {
	    if (handback != hb ||
		!(n instanceof AttributeChangeNotification) ||
		wrapped.getState() == CommunicatorServer.STARTING) {
		return;
	    }

	    synchronized(handback) {
		stateChanged = true;

		handback.notifyAll();
	    }
	}

	public boolean stateChanged = false;

	private Object handback;
    }

    private CommunicatorServer wrapped;

    private JMXServiceURL address;
    private Map env;

    private ClassLogger logger;
}
