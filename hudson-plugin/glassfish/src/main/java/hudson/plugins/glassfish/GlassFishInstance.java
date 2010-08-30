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

import hudson.FilePath;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

/**
 * GlassFish Application Server Instance Configuration.
 *
 * @author Harshad Vilekar
 *
 */
@SuppressWarnings("deprecation")
public class GlassFishInstance {

    String instanceName;
    String nodeName = "";
    String s1as_home = "";
    // preferred port number start
    int http_listener_port, http_ssl_listener_port,
            iiop_listener_port, iiop_ssl_listener_port, iiop_ssl_mutualauth_port,
            jmx_system_connector_port, jms_provider_port, asadmin_listener_port,
            gms_listener_port;
    GlassFishClusterNode clusterNode;
    GlassFishCluster gfc;
    PrintStream logger;

    // initialize with preferred port numbers
    // allocate the ports starting from basePort
    public GlassFishInstance(GlassFishCluster gfc,
            PrintStream logger,
            String instanceName,
            int basePort) {
        this(gfc,
                logger,
                instanceName,
                "",
                "",
                basePort++,
                basePort++,
                basePort++,
                basePort++,
                basePort++,
                basePort++,
                basePort++,
                basePort++,
                basePort++);

    }

    public GlassFishInstance(
            GlassFishCluster gfc,
            PrintStream logger,
            String instanceName,
            String nodeName,
            String s1as_home,
            int http_listener_port,
            int http_ssl_listener_port,
            int iiop_listener_port,
            int iiop_ssl_listener_port,
            int iiop_ssl_mutualauth_port,
            int jmx_system_connector_port,
            int jms_provider_port,
            int asadmin_listener_port,
            int gms_listener_port) {
        this.gfc = gfc;
        this.logger = logger;
        this.instanceName = instanceName;
        this.nodeName = nodeName;
        this.s1as_home = s1as_home;
        this.http_listener_port = http_listener_port;
        this.http_ssl_listener_port = http_ssl_listener_port;
        this.iiop_listener_port = iiop_listener_port;
        this.iiop_ssl_listener_port = iiop_ssl_listener_port;
        this.iiop_ssl_mutualauth_port = iiop_ssl_mutualauth_port;
        this.jmx_system_connector_port = jmx_system_connector_port;
        this.jms_provider_port = jms_provider_port;
        this.asadmin_listener_port = asadmin_listener_port;
        this.gms_listener_port = gms_listener_port;

    }

    // Try to allocate the port. If the port is not available, update the port
    // value to the available port.
    public void updatePerPortAvailability() {
        String str = instanceName + " " + nodeName + ":";
        String portName = str + "http_listener_port";
        http_listener_port = clusterNode.getAvailablePort(http_listener_port, portName);

        portName = str + "http_ssl_listener_port";
        http_ssl_listener_port = clusterNode.getAvailablePort(http_ssl_listener_port, portName);

        portName = str + "iiop_listener_port";
        iiop_listener_port = clusterNode.getAvailablePort(iiop_listener_port, portName);

        portName = str + "iiop_ssl_listener_port";
        iiop_ssl_listener_port = clusterNode.getAvailablePort(iiop_ssl_listener_port, portName);

        portName = str + "iiop_ssl_mutualauth_port";
        iiop_ssl_mutualauth_port = clusterNode.getAvailablePort(iiop_ssl_mutualauth_port, portName);

        portName = str + "jmx_system_connector_port";
        jmx_system_connector_port = clusterNode.getAvailablePort(jmx_system_connector_port, portName);

        portName = str + "jms_provider_port";
        jms_provider_port = clusterNode.getAvailablePort(jms_provider_port, portName);

        portName = str + "asadmin_listener_port";
        asadmin_listener_port = clusterNode.getAvailablePort(asadmin_listener_port, portName);

        portName = str + "gms_listener_port";
        gms_listener_port = clusterNode.getAvailablePort(gms_listener_port, portName);
    }

    GlassFishClusterNode getClusterNode() {
        return clusterNode;
    }

    /** Returns FilePath for the logs directory for current instance. The logs directory is present at:
     * $GFHOME/nodes/<nodeName>/<instanceName/logs/
     */
    FilePath getInstanceLogs() {
        try {
            FilePath fp = getClusterNode().getNode().createPath(getClusterNode().getInstaller().GFHomeDir.toString() + "/glassfish/nodes");
            List<FilePath> fpList = fp.listDirectories();
            if (fpList == null || fpList.isEmpty()) {
                logger.println("No subdirectories found for " + getClusterNode().getNodeName() + ":" + fp.toString());
                return null;
            }
            // look for the subdirectory of nodes, which contains the nodename (only hostname, no domain name)
            // on which this instance is deployed
            boolean matchFound = false;
            for (FilePath thisfp : fpList) {
                // the nodename subdirectory inside "nodes" may have
                // domain name removed (as in qm2.sfbay changed to "qm2" or may have
                // fully qualified domain name added (as in qm2.sfbay changed to "qm2.sfbay.sun.com"
                // Hence, check if one string is substring of another, and vice versa.

                if (getClusterNode().getNodeName().startsWith(thisfp.getName())
                        || thisfp.getName().startsWith(getClusterNode().getNodeName())
                        || thisfp.getName().equals("localhost")) {
                    matchFound = true;
                    fp = thisfp;
                    break;
                }
            }
            if (!matchFound) {
                return null;
            } else {
                // at this point, we found $GFHOME/nodeagents/<nodeName>, append "<instanceName>/logs" to it, and return that value.
                return getClusterNode().getNode().createPath(fp.toString() + "/" + instanceName + "/logs");
            }

        } catch (IOException e) {
            e.printStackTrace(logger);
            return null;
        } catch (InterruptedException e) {
            e.printStackTrace(logger);
            return null;
        }
    }

    // the returned string is used by asadmin command
    String getPortList() {

        return "HTTP_LISTENER_PORT=" + http_listener_port
                + ":HTTP_SSL_LISTENER_PORT=" + http_ssl_listener_port
                + ":IIOP_LISTENER_PORT=" + iiop_listener_port
                + ":IIOP_SSL_LISTENER_PORT=" + iiop_ssl_listener_port
                + ":IIOP_SSL_MUTUALAUTH_PORT=" + iiop_ssl_mutualauth_port
                + ":JMX_SYSTEM_CONNECTOR_PORT=" + jmx_system_connector_port
                + ":JMS_PROVIDER_PORT=" + jms_provider_port
                + ":ASADMIN_LISTENER_PORT=" + asadmin_listener_port
                + ":GMS_LISTENER_PORT-" + gfc.clusterName + "=" + gms_listener_port;
    }

    // non standard local contract for some ant scripts
    // this is a old format - and may be removed in future
    public String getPropsForAntS() {
        return nodeName + ":"
                + http_listener_port + ":"
                + http_ssl_listener_port + ":"
                + iiop_ssl_listener_port + ":"
                + iiop_listener_port + ":"
                + jmx_system_connector_port + ":"
                + iiop_ssl_mutualauth_port + ":"
                + jms_provider_port + ":"
                + asadmin_listener_port + ":"
                + gms_listener_port + ":"
                + instanceName;
    }

    // the returned string is added to the cluster.props file.
    public String getProps(int instanceId) {
        String idStr = "instance" + instanceId + "_";
        String str = idStr + "name=" + instanceName;
        idStr = "\n" + idStr;

        // Note: cygwin plugin takes care of converting windows path to Unix path.
        
        str = str
                + idStr + "node=" + nodeName
                + idStr + "s1as_home=" + s1as_home
                + idStr + "HTTP_LISTENER_PORT=" + http_listener_port
                + idStr + "HTTP_SSL_LISTENER_PORT=" + http_ssl_listener_port
                + idStr + "IIOP_LISTENER_PORT=" + iiop_listener_port
                + idStr + "IIOP_SSL_LISTENER_PORT=" + iiop_ssl_listener_port
                + idStr + "IIOP_SSL_MUTUALAUTH_PORT=" + iiop_ssl_mutualauth_port
                + idStr + "JMX_SYSTEM_CONNECTOR_PORT=" + jmx_system_connector_port
                + idStr + "JMS_PROVIDER_PORT=" + jms_provider_port
                + idStr + "ASADMIN_LISTENER_PORT=" + asadmin_listener_port
                + idStr + "GMS_LISTENER_PORT=" + gms_listener_port
                + "\n";
        return str;

    }

    public String toStr(boolean verbose) {
        if (verbose) {
            return instanceName + " on " + nodeName + ": " + getPortList() + " ";
        } else {
            return instanceName + " on " + nodeName;
        }
    }
}
