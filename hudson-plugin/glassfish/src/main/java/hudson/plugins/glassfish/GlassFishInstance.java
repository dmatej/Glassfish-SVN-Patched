/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
package hudson.plugins.glassfish;

/**
 * GlassFish Application Server Instance Configuration.
 *
 * @author Harshad Vilekar
 *
 */
public class GlassFishInstance {

    String instanceName;
    // host number on which this instance supposed to run.
    // currently, only 1 host is supported
    int hostNum = 1;
    String hostName = "";
    // allocate the ports starting from this port
    int basePort;
    // preferred port number start
    int http_listener_port, http_ssl_listener_port,
            iiop_listener_port, iiop_ssl_listener_port, iiop_ssl_mutualauth_port,
            jmx_system_connector_port, jms_provider_port, asadmin_listener_port;

    // initialize with preferred port numbers
    public GlassFishInstance(GlassFishCluster gfc, String instanceName, int basePort) {
        this.instanceName = instanceName;
        this.basePort = basePort;
        http_listener_port = basePort++;
        http_ssl_listener_port = basePort++;
        iiop_listener_port = basePort++;
        iiop_ssl_listener_port = basePort++;
        iiop_ssl_mutualauth_port = basePort++;
        jmx_system_connector_port = basePort++;
        jms_provider_port = basePort++;
        asadmin_listener_port = basePort++;
    }

    // Try to allocate the port. If the port is not available, update the port
    // value to the available port.
    public void updatePerPortAvailability() {
        String portName = instanceName + " " + hostName + ":" + "http_listener_port";
        http_listener_port = GlassFishCluster.getAvailablePort(http_listener_port, portName);

        portName = instanceName + " " + hostName + ":" + "http_ssl_listener_port";
        http_ssl_listener_port = GlassFishCluster.getAvailablePort(http_ssl_listener_port, portName);

        portName = instanceName + " " + hostName + ":" + "iiop_listener_port";
        iiop_listener_port = GlassFishCluster.getAvailablePort(iiop_listener_port, portName);

        portName = instanceName + " " + hostName + ":" + "iiop_ssl_listener_port";
        iiop_ssl_listener_port = GlassFishCluster.getAvailablePort(iiop_ssl_listener_port, portName);

        portName = instanceName + " " + hostName + ":" + "iiop_ssl_mutualauth_port";
        iiop_ssl_mutualauth_port = GlassFishCluster.getAvailablePort(iiop_ssl_mutualauth_port, portName);

        portName = instanceName + " " + hostName + ":" + "jmx_system_connector_port";
        jmx_system_connector_port = GlassFishCluster.getAvailablePort(jmx_system_connector_port, portName);

        portName = instanceName + " " + hostName + ":" + "jms_provider_port";
        jms_provider_port = GlassFishCluster.getAvailablePort(jms_provider_port, portName);

        portName = instanceName + " " + hostName + ":" + "asadmin_listener_port";
        asadmin_listener_port = GlassFishCluster.getAvailablePort(asadmin_listener_port, portName);
    }

    public String getPortList() {

        return "HTTP_LISTENER_PORT=" + http_listener_port
                + ":HTTP_SSL_LISTENER_PORT=" + http_ssl_listener_port
                + ":IIOP_LISTENER_PORT=" + iiop_listener_port
                + ":IIOP_SSL_LISTENER_PORT=" + iiop_ssl_listener_port
                + ":IIOP_SSL_MUTUALAUTH_PORT=" + iiop_ssl_mutualauth_port
                + ":JMX_SYSTEM_CONNECTOR_PORT=" + jmx_system_connector_port
                + ":JMS_PROVIDER_PORT=" + jms_provider_port
                + ":ASADMIN_LISTENER_PORT=" + asadmin_listener_port
                + " ";

    }

    // non standard local contract for some ant scripts
    // this is a old format - and may be removed in future
    public String getPropsForAntS() {
        return hostName + ":"
                + http_listener_port + ":"
                + http_ssl_listener_port + ":"
                + iiop_ssl_listener_port + ":"
                + iiop_listener_port + ":"
                + jmx_system_connector_port + ":"
                + iiop_ssl_mutualauth_port + ":"
                + jms_provider_port + ":"
                + asadmin_listener_port + ":"
                + instanceName;
    }


    public String getProps() {
        return 
                "instanceName=" + instanceName
                + "\nhostName=" + hostName
                + "\nhttp_listener_port=" + http_listener_port
                + "\nhttp_ssl_listener_port=" + http_ssl_listener_port
                + "\niiop_ssl_listener_port="+ iiop_ssl_listener_port
                + "\niiop_listener_port=" + iiop_listener_port
                + "\njmx_system_connector_port="+ jmx_system_connector_port
                + "\niiop_ssl_mutualauth_port="+ iiop_ssl_mutualauth_port
                + "\njms_provider_port="+ jms_provider_port
                + "\nasadmin_listener_port="+ asadmin_listener_port
                + "\n" ;

    }

    public String toStr() {
        return instanceName + " on " + hostName + ": " + getPortList();
    }
}
