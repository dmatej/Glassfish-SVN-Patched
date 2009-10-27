/*
 * @(#)file      RmiConnectorAddress.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.23
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
 */

package com.sun.jdmk.comm;

import com.sun.jdmk.internal.ClassLogger;

/**
 * Defines the RMI connector address. This object is used by the connector client
 * to check that the address used as the argument of its connect method is of the
 * appropriate type.
 * <P>
 * The default rmi registry hostname is the local hostname.
 * <P>
 * The default rmi registry port is specified by the constant
 * {@link com.sun.jdmk.ServiceName#RMI_CONNECTOR_PORT  ServiceName.RMI_CONNECTOR_PORT}.
 * <P>
 * The default rmi service name (the <CODE>RmiConnectorServer</CODE>'s service name) is specified by
 * the constant {@link com.sun.jdmk.ServiceName#RMI_CONNECTOR_SERVER ServiceName.RMI_CONNECTOR_SERVER}.
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  The legacy RMI connector,
 * including this class, may be removed in a future version of Java
 * DMK.  See {@link javax.management.remote.rmi} and {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 */
public class RmiConnectorAddress implements ConnectorAddress {

/* NPCTE fix for bugId 4878413, esc 0, MR, June 2003 */
private static final long serialVersionUID = -3866575485397551102L;
/* end of NPCTE fix for bugId 4878413 */

    /**
     * RmiConnectorAddress constructor.
     * <P>
     * The default host is the local hostname.
     * <P>
     * The default port is defined by com.sun.jdmk.ServiceName.RMI_CONNECTOR_PORT.
     * <P>
     * The default service name is defined by com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER.
     */
    public RmiConnectorAddress() {
   
        // ------------------------
        // Default port and service name
        // ------------------------
        // Initialization ...

        // ------------------------
        // Set the host
        // ------------------------
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (System.getProperty("jdmk.hostname")!=null) {
            host = System.getProperty("jdmk.hostname"); 
        }
        else {
            try {
                host = java.net.InetAddress.getLocalHost().getHostName();
            } catch (Exception e) {
                host = "localhost";
            } 
        }
        if (logger.finerOn())
            logger.finer("RmiConnectorAddress", "host="+host);
        // end of NPCTE fix for bugId 4770217
    }

    /**
     * RmiConnectorAddress constructor.
     *
     * @param host The host specifies the hostname where the rmi registry is running.
     * @param port The port specifies the port number where the rmi registry is running.
     * @param serviceName The service name specifies the name the RMI connector server used to
     * register in the rmi registry.
     */
    public RmiConnectorAddress (String host, int port, String serviceName) {

        // ------------------------
        // Set port and service name
        // ------------------------
        this.port = port;
        this.serviceName = serviceName;

        // ------------------------
        // Set the host
        // ------------------------
        if (host != null) {
            this.host = host;
        } else {
            try {
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname")!=null)
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
            logger.finer("RmiConnectorAddress", "host="+this.host);
        // end of NPCTE fix for bugId 4770217
    }

    /**
     * Returns the type of connector.
     *
     * @return the value of the connector type property.
     */
    public String getConnectorType() {
        return "SUN RMI";
    }

    /**
     * Returns the RMI Connector Server hostname.
     *
     * @return the value of the server host property.
     */
    public String getHost() {
        return host;
    }

    /**
     * Sets the value of the RMI Connector Server host property.
     *
     * @param host the new value of the server host property.
     */
    public void setHost(String host) {
        this.host = host;
    }

    /**
     * Returns the RMI Connector Server port number.
     *
     * @return the value of the server port property.
     */
    public int getPort() {
        return port;
    }

    /**
     * Sets the value of the RMI Connector Server port property.
     *
     * @param port the new value of the server port property.
     */
    public void setPort(int port) {
        this.port = port;
    }

    /**
     * Returns the RMI Connector Server service name.
     *
     * @return the value of the server service name property.
     */
    public String getName() {
        return serviceName;
    }

    /**
     * Sets the value of the RMI Connector Server service name property.
     *
     * @param serviceName the new value of the server service name property.
     */
    public void setName(String serviceName) {
        this.serviceName = serviceName;
    }

    // ----------------------------------
    // Private variables
    // ----------------------------------

    /**
     * The RMI host
     */
    private String host = null ;

    /**
     * The RMI port number
     */
    private int defaultPort = com.sun.jdmk.ServiceName.RMI_CONNECTOR_PORT ;
    private int port = defaultPort ;

    /**
     * The RMI connector server service name
     */
    private String defaultServiceName = com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER ;
    private String serviceName = defaultServiceName ;

    private final static ClassLogger logger =
        new ClassLogger(ClassLogger.LOGGER_LEGACY_RMI,
                        "RmiConnectorAddress");
}
