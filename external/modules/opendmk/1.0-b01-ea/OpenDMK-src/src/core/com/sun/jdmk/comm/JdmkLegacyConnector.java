/*
 * @(#)file      JdmkLegacyConnector.java
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
package com.sun.jdmk.comm;

/**
 * <p>Java DMK legacy connectors (HTTP, HTTPS and RMI) can be created
 * through the standard <code>JMXConnectorFactory</code> and
 * <code>JMXConnectorServerFactory</code> since Java DMK 5.1.</p>
 *
 * <p>The factories will wrap a JDMK legacy connector into a
 * <code>JMXConnector</code> or a <code>JMXConnectorServer</code>.</p>
 *
 * <p>This interface specifies a protocol name for each JDMK legacy connector
 * which will be recognized by the JMX factories, and a list of configuration
 * properties which can be passed to the factories at creation time.</p>
 *
 * <p>The {link #getWrapped()} method is specified by this interface in order
 * to allow users to get the original wrapped JDMK legacy connector.</p>
 *
 * @since Java DMK 5.1
 */
public interface JdmkLegacyConnector {

    /**
     * Specifies a protocol name for the JDMK HTTP connector.
     * <p>
     * The value is the string "jdmk-http".
     */
    public final static String HTTP_CONNECTOR = "jdmk-http";

    /**
     * Specifies a protocol name for the JDMK HTTPS connector.
     * <p>
     * The value is the string "jdmk-https".
     */
    public final static String HTTPS_CONNECTOR = "jdmk-https";

    /**
     * Specifies a protocol name for the JDMK RMI connector.
     * <p>
     * The value is the string "jdmk-rmi".
     */
    public final static String RMI_CONNECTOR = "jdmk-rmi";

    /**
     * Specifies a list of AuthInfo (com.sun.jdmk.comm.AuthInfo[]) for an
     * HTTP or HTTPS connector server.
     * <p>
     * The key value is the string "com.sun.jdmk.http.server.authinfo.list".
     */
    public final static String HTTP_SERVER_AUTHINFO_LIST =
        "com.sun.jdmk.http.server.authinfo.list";

    /**
     * Specifies the local InetAddress the HTTP/HTTPS connector server will
     * bind to.
     * <p>
     * The key value is the string "com.sun.jdmk.http.server.local.address".
     */
    public final static String HTTP_SERVER_LOCAL_ADDRESS =
        "com.sun.jdmk.http.server.local.address";

    /**
     * Specifies the SSL needClientAuth flag used by the HTTPS connector server
     * to require or to not require mutual authentication. By default the HTTPS
     * connector server requires client authentication.
     * <p>
     * The key value is the string "com.sun.jdmk.https.server.need.client.auth",
     * the value type is a <code>String</code> that must be "true" or "false".
     * <p>
     * This property can be also used in the client side map to specify the
     * needClientAuth flag for the HTTPS server the client will create when
     * listening for notifications in push mode.
     */
    public final static String HTTPS_SERVER_NEED_CLIENT_AUTH =
        "com.sun.jdmk.https.server.need.client.auth";

    /**
     * Specifies a String object as a local host for a client (HTTP/HTTPS/RMI)
     * to receive notifications from its server.
     * <p>
     * The key value is "com.sun.jdmk.client.localhost", the value type is
     * <code>String</code>, the string represents the hostname or IP address.
     */
    public final static String CLIENT_LOCALHOST =
        "com.sun.jdmk.client.localhost";

    /**
     * Specifies an AuthInfo object used by an HTTP/HTTPS client to connect to
     * its server.
     * <p>
     * The key value is "com.sun.jdmk.http.client.authinfo", the value type is
     * <code>com.sun.jdmk.comm.AuthInfo</code>.
     */
    public final static String HTTP_CLIENT_AUTHINFO =
        "com.sun.jdmk.http.client.authinfo";

    /**
     * Returns a wrapped JDMK legacy connector client or server. A user should
     * know to which JDMK legacy connector the returned object should be cast:
     * HTTP client/server, HTTPS client/server or RMI client/server.
     * <p>
     * The returned object is used to call those methods specified only in the
     * original JDMK legacy connector, for example the methods to configure
     * the Heartbeat service. A user should call directly a method from the
     * JMXConnector/Server wrapper if that method is mapped by the wrapper.
     */
    public Object getWrapped();
}
