/*
 * @(#)file      HttpConnectorServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.30
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


package com.sun.jdmk.comm;

// java import
//
import java.net.InetAddress;

// jmx import
//
import javax.management.ObjectName;
import javax.management.MBeanServer;

/**
 * This class implements the server part of the HTTP/TCP connector.
 * This class inherits most of its behavior. It performs only
 * TCP-specific tasks: 
 * <UL>
 * <LI>it creates, reads, writes and closes the TCP sockets,</LI>
 * <LI>it defines the default port of the connector: 8081.</LI>
 * </UL>
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This class may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 * @see com.sun.jdmk.comm.HttpConnectorClient
 *
 */
public class HttpConnectorServer extends GenericHttpConnectorServer implements HttpConnectorServerMBean {

    /**
     * Constructs an <CODE>HttpConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the default port (8081).
     */
    public HttpConnectorServer() {
        super(CommunicatorServer.HTTP_TYPE, com.sun.jdmk.ServiceName.HTTP_CONNECTOR_PORT);
    }

    /**
     * Constructs an <CODE>HttpConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port.
     *
     * @param port The port number.
     */
    public HttpConnectorServer(int port) {
        super(CommunicatorServer.HTTP_TYPE, port);
    }

    /**
     * Constructs an <CODE>HttpConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port
     * and user authentication information list.
     *
     * @param port The port number.
     * @param authInfoList The user authentication information list.
     */
    public HttpConnectorServer(int port, AuthInfo[] authInfoList) {
        super(CommunicatorServer.HTTP_TYPE, port, authInfoList);
    }

    /**
     * Constructs an <CODE>HttpConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port.
     *
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to.
     */
    // NPCTE fix for bug 4873785
    public HttpConnectorServer(int port, InetAddress bindAddr) {
        super(CommunicatorServer.HTTP_TYPE, port, bindAddr);
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Constructs an <CODE>HttpConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port
     * and user authentication information list.
     *
     * @param port The port number.
     * @param authInfoList The user authentication information list.
     * @param bindAddr The local InetAddress the server will bind to.
     */
    // NPCTE fix for bug 4873785
    public HttpConnectorServer(int port, AuthInfo[] authInfoList,
			       InetAddress bindAddr) {
        super(CommunicatorServer.HTTP_TYPE, port, authInfoList, bindAddr);
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Allows the MBean to perform any operations it needs before being
     * registered in the MBean server. 
     * If the name of the HTTP connector server MBean is not specified, 
     * it is initialized with the default value:
     * {@link com.sun.jdmk.ServiceName#DOMAIN com.sun.jdmk.ServiceName.DOMAIN}:
     * {@link com.sun.jdmk.ServiceName#HTTP_CONNECTOR_SERVER com.sun.jdmk.ServiceName.HTTP_CONNECTOR_SERVER}.
     * If any exception is raised, the HTTP connector server MBean will not be registered in the MBean server.
     *
     * @param server The MBeanServer in which the MBean will be registered.
     * @param name The object name of the MBean.
     *
     * @return The name of the registered MBean.
     *
     * @exception Exception This exception should be caught by the MBeanServer
     * and re-thrown as an MBeanRegistrationException.
     */
    public ObjectName preRegister(MBeanServer server, ObjectName name) throws Exception {
        if (name == null) {
            name = new ObjectName(server.getDefaultDomain() + ":" + com.sun.jdmk.ServiceName.HTTP_CONNECTOR_SERVER);
        }
        return super.preRegister(server, name);
    }

    /**
     * Gets an instance of the socket factory used by this connector.
     */
    GenericHttpSocket createSocket() {
	HttpSocket http_socket = new HttpSocket();
	http_socket.setTimeout(getTimeout());
        return http_socket;
    }

    /**
     * Gets the notification forwarder used by this connector.
     */
    GenericHttpNotificationForwarder getNotificationForwarder(GenericHttpConnectorAddress address) {
        return new HttpNotificationForwarder(address);
    }

    /**
     * Returns the name of the protocol used.
     *
     * @return The string "http".
     */
    public String getProtocol() {
        return "http";
    }

    /**
     * Returns setting for Timeout.
     * <P>
     * 0 returns implies that the option is disabled
     * (i.e. timeout of infinity).
     * <P>
     * The default value for timeout is 60000 milliseconds.
     *
     * @return The current value of the "Timeout" property.
     */
    public int getTimeout() {
        if ( sockListen != null ) {
            return sockListen.getTimeout();
        } else {
            return timeout;
        }
    }

    /**  
     * Enables/disables Timeout with the specified timeout, in milliseconds.
     * <P>
     * With this option set to a non-zero timeout, a read() call on the 
     * InputStream associated with this Socket will block for only this 
     * amount of time. If the timeout expires, a {@link 
     * java.io.InterruptedIOException} is raised, though the Socket
     * is still valid. The option must be enabled prior to entering the 
     * blocking operation to have effect. The timeout must be > 0. 
     * A timeout of zero is interpreted as an infinite timeout.
     * </P>
     * The default value for timeout is 60000 milliseconds.
     *
     * @param value The new value of the property.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the connector was ONLINE or STARTING.
     */
    public void setTimeout(int value) throws java.lang.IllegalStateException {
        if ((state == ONLINE) || (state == STARTING)) {
            throw new IllegalStateException("Stop server before carrying out this operation");
        }
	timeout = value;
        if ( sockListen != null ) {
            sockListen.setTimeout(value);
	}
    }

    // TRACE STUFF
    //------------

    /**
     * Returns the string used in debug traces.
     */
    String makeDebugTag() {
        return "HttpConnectorServer[" + getProtocol() + ":" + getPort() + "]";
    }

    // PRIVATE VARIABLES
    //------------------

    /**
     * The timeout
     */
    private int timeout = 60000;
}
