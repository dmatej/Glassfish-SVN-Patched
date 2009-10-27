/*
 * @(#)file      HttpConnectorClient.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.22
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

import java.net.InetAddress;

/**
 * This class provides an implementation of the 
 * {@link com.sun.jdmk.comm.RemoteMBeanServer RemoteMBeanServer}
 * interface based on the HTTP/TCP protocol.
 * <P>
 * Querying a Java Dynamic Management agent with this connector implies that an instance of
 * {@link com.sun.jdmk.comm.HttpConnectorServer HttpConnectorServer}
 * is running on the remote Java Dynamic Management agent.
 * <P>
 * <strong>Note - </strong> Use the
 * {@link #connect connect}
 * method to identify the Java Dynamic Management agent with which the connector must communicate.
 * <P>
 * It is possible to request the use of a specific proxy through the java properties
 * <CODE>http.proxyHost=</CODE><VAR>host</VAR> and <CODE>http.proxyPort=</CODE><VAR>port</VAR>.
 * <P>
 * The authentication information required to configure the connector is provided by the ConnectorAddress
 * parameter in the method {@link #connect connect}.
 * <p>
 * By default, port 8081 is used to communicate with HTTP/TCP connector.
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This class may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 * @see com.sun.jdmk.comm.RemoteMBeanServer
 * @see com.sun.jdmk.comm.HttpConnectorServer
 *
 */
public class HttpConnectorClient extends GenericHttpConnectorClient {
    private static final long serialVersionUID = -7034170604861491738L;

    /**
     * Constructs an <CODE>HttpConnectorClient</CODE>.
     * <P>
     * Initializes this connector client with the default port (8081).
     */
    public HttpConnectorClient() {
        super();
    }

    /**
     * Constructs an <CODE>HttpConnectorClient</CODE>.
     * <P>
     * Initializes this connector client with the default port (8081).
     *
     * @deprecated replaced by the method HttpConnectorClient(String localhost)
     * @param localhost a user specified local host address to receive notifications from the server.
     */
    public HttpConnectorClient(InetAddress localhost) {
        super(localhost);
    }

    /**
     * Constructs an <CODE>HttpConnectorClient</CODE>.
     * <P>
     * Initializes this connector client with the default port (8081).
     *
     * @param localhost a local host address to receive notifications
     *                  from the server.
     *
     * @since Java DMK 5.0
     */
    public HttpConnectorClient(String localhost) {
        super(localhost);
    }

    /**
     * Gets the socket factory used by this HTTP/TCP connector client.
     *
     * @return A HTTP/TCP connector client socket.
     */
    GenericHttpSocketFactory getSocketFactory() {
        return new HttpSocketFactory();
    }

    /**
     * Gets the notification receiver used by this HTTP/TCP connector client.
     *
     * @return The notification receiver used by this client connector.
     */
    GenericHttpNotificationReceiver getNotificationReceiver(GenericHttpConnectorClient connector,
                                                            ClientNotificationDispatcher dispatcher) {
        return new HttpNotificationReceiver(connector, dispatcher);
    }
}
