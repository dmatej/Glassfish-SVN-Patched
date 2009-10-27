/*
 * @(#)file      HttpsConnectorServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.17
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
 * This class implements the server part of the HTTP/SSL connector.
 * This class inherits most of its behavior. It performs only
 * TCP-specific tasks: 
 * <UL>
 * <LI>it creates, reads, writes and closes the SSL sockets,</LI>
 * <LI>it defines the default port of the connector: 8084.</LI>
 * </UL>
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This class may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 * @see com.sun.jdmk.comm.HttpsConnectorClient
 *
 */
public class HttpsConnectorServer
    extends GenericHttpConnectorServer
    implements HttpsConnectorServerMBean {

    /**
     * Constructs an <CODE>HttpsConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the default port (8084)
     * and SSL client authentication required.
     */
    public HttpsConnectorServer() {
        super(CommunicatorServer.HTTPS_TYPE,
	      com.sun.jdmk.ServiceName.HTTPS_CONNECTOR_PORT);
    }

    /**
     * Constructs an <CODE>HttpsConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port
     * and SSL client authentication required.
     *  
     * @param port The port number.
     */
    public HttpsConnectorServer(int port) {
        super(CommunicatorServer.HTTPS_TYPE, port);
    }

    /**
     * Constructs an <CODE>HttpsConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port,
     * user authentication information list and SSL client
     * authentication required.
     *
     * @param port The port number.
     * @param authInfoList The user authentication information list.
     * If <code>authInfoList</code> is null no user authentication
     * is performed.
     */
    public HttpsConnectorServer(int port, AuthInfo[] authInfoList) {
        super(CommunicatorServer.HTTPS_TYPE, port, authInfoList);
    }

    /**
     * Constructs an <CODE>HttpsConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port,
     * local IP address to bind to and SSL client authentication
     * required.
     *
     * @param port The port number.
     * @param bindAddr The local IP address the server will bind to. If
     * <code>bindAddr</code> is null, it will default accepting connections
     * on any/all local addresses.
     */
    // NPCTE fix for bug 4873785
    public HttpsConnectorServer(int port, InetAddress bindAddr) {
        super(CommunicatorServer.HTTPS_TYPE, port, bindAddr);
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Constructs an <CODE>HttpsConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port,
     * user authentication information list, local IP address to
     * bind to and SSL client authentication required.
     *
     * @param port The port number.
     * @param authInfoList The user authentication information list.
     * If <code>authInfoList</code> is null no user authentication
     * is performed.
     * @param bindAddr The local IP address the server will bind to. If
     * <code>bindAddr</code> is null, it will default accepting connections
     * on any/all local addresses.
     */
    // NPCTE fix for bug 4873785
    public HttpsConnectorServer(int port, AuthInfo[] authInfoList,
				InetAddress bindAddr) {
        super(CommunicatorServer.HTTPS_TYPE, port, authInfoList, bindAddr);
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Constructs an <CODE>HttpsConnectorServer</CODE>.
     * <P>
     * Initializes this connector server with the specified port,
     * user authentication information list, local IP address to
     * bind to and SSL client authentication flag.
     *
     * @param port The port number.
     * @param authInfoList The user authentication information list.
     * If <code>authInfoList</code> is null no user authentication
     * is performed.
     * @param bindAddr The local IP address the server will bind to. If
     * <code>bindAddr</code> is null, it will default accepting connections
     * on any/all local addresses.
     * @param needClientAuth <code>true</code> to require client
     * authentication on SSL connections accepted by the server
     * socket created by this connector server; <code>false</code>
     * to not require client authentication.
     *
     * @since Java DMK 5.1
     */
    public HttpsConnectorServer(int port, AuthInfo[] authInfoList,
				InetAddress bindAddr, boolean needClientAuth) {
        super(CommunicatorServer.HTTPS_TYPE, port, authInfoList, bindAddr);
        this.needClientAuth = needClientAuth;
    }

    /**
     * Allows the MBean to perform any operations it needs before being
     * registered in the MBean server. 
     * If the name of the HTTPS connector server MBean is not specified, 
     * it is initialized with the default value:
     * {@link com.sun.jdmk.ServiceName#DOMAIN com.sun.jdmk.ServiceName.DOMAIN}:
     * {@link com.sun.jdmk.ServiceName#HTTPS_CONNECTOR_SERVER 
     *        com.sun.jdmk.ServiceName.HTTPS_CONNECTOR_SERVER}.
     * If any exception is raised, the HTTPS connector server MBean will not 
     * be registered in the MBean server.
     *
     * @param server The MBeanServer in which the MBean will be registered.
     * @param name The object name of the MBean.
     *
     * @return The name of the registered MBean.
     *
     * @exception Exception This exception should be caught by the MBeanServer
     * and re-thrown as an MBeanRegistrationException.
     */
    public ObjectName preRegister(MBeanServer server, ObjectName name) 
	throws Exception {
        if (name == null) {
            name = new ObjectName(com.sun.jdmk.ServiceName.DOMAIN + ":" + 
			   com.sun.jdmk.ServiceName.HTTPS_CONNECTOR_SERVER);
        }
        return super.preRegister(server, name);
    }

    /**
     * Gets an instance of the socket factory used by this connector.
     */
    GenericHttpSocket createSocket() {
	HttpsSocket https_socket =
	    new HttpsSocket(0, null, getNeedClientAuth());
	https_socket.setTimeout(getTimeout());
        return https_socket;
    }

    /**
     * Gets the notification forwarder used by this connector.
     */
    GenericHttpNotificationForwarder
	getNotificationForwarder(GenericHttpConnectorAddress address) {
        return new HttpsNotificationForwarder(address);
    }

    /**
     * Returns the name of the protocol used.
     *
     * @return The string "https".
     */
    public String getProtocol() {
        return "https";
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
     * <P>
     * The default value for timeout is 60000 milliseconds.
     *
     * @param value The new value of the property.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the connector was ONLINE or STARTING.
     */
    public void setTimeout(int value) throws java.lang.IllegalStateException {
        if ((state == ONLINE) || (state == STARTING)) {
            throw new IllegalStateException(
                  "Stop server before carrying out this operation");
        }
	timeout = value;
        if ( sockListen != null ) {
            sockListen.setTimeout(value);
	}
    }

    /**
     * <p>Returns <code>true</code> if client authentication is
     * required on SSL connections accepted by the server socket
     * created by this connector server.</p>
     *
     * @return <code>true</code> if client authentication is required
     *
     * @since Java DMK 5.1
     */
    public final boolean getNeedClientAuth() {
        return needClientAuth;
    }

    // TRACE STUFF
    //------------

    /**
     * Returns the string used in debug traces.
     */
    String makeDebugTag() {
        return "HttpsConnectorServer[" + getProtocol() + ":" + getPort() + "]";
    }

    // PRIVATE VARIABLES
    //------------------

    /**
     * The timeout
     */
    private int timeout = 60000;

    /**
     * The SSL need client authentication flag
     */
    private boolean needClientAuth = true;
}
