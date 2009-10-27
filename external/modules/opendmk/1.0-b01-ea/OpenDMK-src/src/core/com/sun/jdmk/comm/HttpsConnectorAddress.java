/*
 * @(#)file      HttpsConnectorAddress.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.12
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
import java.io.*;
import java.net.*;

import com.sun.jdmk.internal.ClassLogger;

/**
 * This class defines the Authentication Information required by the
 * HTTPS connector in order to carry out login/password authentication.
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This class may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 */

public class HttpsConnectorAddress implements GenericHttpConnectorAddress {
    private static final long serialVersionUID = -4844146863078871936L;

    // CONSTRUCTORS
    //-------------

    /**
     * HttpsConnectorAddress constructor.
     * <P>
     * Using this constructor no authentication information is provided, i.e. the connector
     * client using this address will not send login/password information to its corresponding
     * connector server.
     * <P>
     * The default host is the local hostname.
     * <P>
     * The default port is defined by com.sun.jdmk.ServiceName.HTTPS_CONNECTOR_PORT.
     */
    public HttpsConnectorAddress() {
        //
        // Set default host and use default port.
        //
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (System.getProperty("jdmk.hostname") != null)
            host = System.getProperty("jdmk.hostname") ;
        else {
            try {
                host = InetAddress.getLocalHost().getHostName();
            } catch (Exception e) {
                host = "localhost";
            }
        }
        // end of NPCTE fix for bugId 4770217
    }

    /**
     * HttpsConnectorAddress constructor.
     * <P>
     * Using this constructor no authentication information is provided, i.e. the connector
     * client using this address will not send login/password information to its corresponding
     * connector server.
     *
     * @param host The host specifies the hostname where the connector server runs.
     * @param port The port specifies the port number where the connector server is listening for incoming connections.
     */
    public HttpsConnectorAddress(String host, int port) {

        this.port = port;

        if (host != null) {
            this.host = host;
        } else {
            try {
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null)
                    this.host = System.getProperty("jdmk.hostname");
                else
                    this.host = java.net.InetAddress.getLocalHost().getHostName();
                // end of NPCTE fix for bugId 4770217
            } catch (Exception e) {
                this.host = "localhost";
            }
        }
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (logger.finerOn())
            logger.finer("HttpsConnectorAddress", "host="+this.host);
        // end of NPCTE fix for bugId 4770217
    }

    /**
     * HttpsConnectorAddress constructor.
     *
     * @param host The host specifies the hostname where the connector server runs.
     * @param port The port specifies the port number where the connector server is listening for incoming connections.
     * @param authInfo The authentication info specifies the login/password required by the connector server.
     */
    public HttpsConnectorAddress(String host, int port, AuthInfo authInfo) {

        this.port = port;
        this.authInfo = authInfo;

        if (host != null) {
            this.host = host;
        } else {
            try {
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null)
                    this.host = System.getProperty("jdmk.hostname");
                else
                    this.host = java.net.InetAddress.getLocalHost().getHostName();
                // end of NPCTE fix for bugId 4770217
            } catch (Exception e) {
                this.host = "localhost";
            }
        }
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (logger.finerOn())
            logger.finer("HttpsConnectorAddress", "host="+this.host);
        // end of NPCTE fix for bugId 4770217
    }

    // GETTERS/SETTERS
    //----------------

    /**
     * Returns the type of connector.
     *
     * @return the value of the connector type property.
     */
    public String getConnectorType() {
        return connType;
    }

    /**
     * Returns the value of the server host property.
     *
     * @return the value of the server host property.
     */
    public String getHost() {
        return host;
    }

    /**
     * Sets the value of the server host property.
     *
     * @param host the new value of the server host property.
     */
    public void setHost(String host) {
        this.host = host;
    }

    /**
     * Returns the value of the server port property.
     *
     * @return the value of the server port property.
     */
    public int getPort() {
        return port;
    }

    /**
     * Sets the value of the server port property.
     *
     * @param port the new value of the server port property.
     */
    public void setPort(int port) {
        this.port = port;
    }

    /**
     * Returns the value of the authentication info property.
     *
     * @return the value of the authentication info property.
     */
    public AuthInfo getAuthInfo() {
        return authInfo;
    }

    /**
     * Sets the value of the authentication info property.
     *
     * @param authInfo the new value of the authentication info property.
     */
    public void setAuthInfo(AuthInfo authInfo) {
        this.authInfo = authInfo;
    }

    // CLONEABLE INTERFACE
    //--------------------

    /**
     * Clone this <CODE>HttpsConnectorAddress</CODE>.
     * The cloning is done using Object.clone().
     *
     * @see Object#clone
     */
    public Object clone() {
        Object newHttpsConnectorAddress = null;
        try {
            newHttpsConnectorAddress = super.clone();
        } catch(CloneNotSupportedException e) {
        }
        return newHttpsConnectorAddress;
    }

    // PRIVATE VARIABLES
    //------------------

    /**
     * @serial The connector type value
     */
    private String connType = "SUN HTTPS";

    /**
     * @serial The server host value
     */
    private String host = "localhost";
  
    /**
     * @serial The server port value
     */
    private int port = 8084;

    /**
     * @serial The authentication info value
     */
    private AuthInfo authInfo = null;

    private final static ClassLogger logger =
        new ClassLogger(ClassLogger.LOGGER_LEGACY_HTTPS,
                        "HttpsConnectorAddress");
}
