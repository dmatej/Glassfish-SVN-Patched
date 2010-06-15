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

import hudson.Proc;
import hudson.Launcher;
import hudson.FilePath;
import hudson.model.AbstractBuild;

import java.io.IOException;
import java.io.PrintStream;

/**
 *
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public class GlassFishCluster {

    private AbstractBuild build;
    private Launcher launcher;
    private PrintStream logger;
    private GlassFishBuilder gfbuilder;
    private int baseport = 8100;

    public GlassFishCluster(AbstractBuild build,
            Launcher launcher,
            PrintStream logger,
            GlassFishBuilder gfbuilder) {
        this.build = build;
        this.launcher = launcher;
        this.logger = logger;
        this.gfbuilder = gfbuilder;
    }

    public boolean createGFCluster() {
      
        String adminCmd = GlassFishInstaller.GFV3ASADMIN_CMD;
        String CMD = adminCmd + " start-domain";
        if (!execCommand(CMD)) {
            return false;
        }

        CMD = adminCmd + " create-cluster " + gfbuilder.getClusterName();
        if (!execCommand(CMD)) {
            return false;
        }

        for (int i = 1; i <= gfbuilder.numInstances(); i++) {
            CMD = adminCmd + " create-local-instance --cluster " + gfbuilder.getClusterName() + getPortNumberString()
                    + gfbuilder.getInstanceNamePrefix() + i;

            if (!execCommand(CMD)) {
                return false;
            }

        }

        if (gfbuilder.getStartCluster()) {
            for (int i = 1; i <= gfbuilder.numInstances(); i++) {

                CMD = adminCmd + " start-local-instance " + gfbuilder.getInstanceNamePrefix() + i;
                if (!execCommand(CMD)) {
                    return false;
                }

            }
        }

        CMD = adminCmd + " list-instances ";
        if (!execCommand(CMD)) {
            return false;
        }

        for (int i = 1; i <= gfbuilder.numInstances(); i++) {
            CMD = adminCmd + " stop-local-instance " + gfbuilder.getInstanceNamePrefix() + i;
            if (!execCommand(CMD)) {
                return false;
            }
        }

        for (int i = 1; i <= gfbuilder.numInstances(); i++) {
            CMD = adminCmd + " delete-local-instance " + gfbuilder.getInstanceNamePrefix() + i;
            if (!execCommand(CMD)) {
                return false;
            }
        }

        CMD = adminCmd + " delete-cluster " + gfbuilder.getClusterName();
        if (!execCommand(CMD)) {
            return false;
        }


        CMD = adminCmd + " stop-domain";
        if (!execCommand(CMD)) {
            return false;
        }

        return true;
    }

    public boolean execCommand(String cmd) {
        try {
            Proc proc = launcher.launch(cmd, build.getEnvVars(), logger, build.getProject().getWorkspace());
            int exitCode = proc.join();
            if (exitCode == 0) {
                logger.println("OK: " + cmd);
                return true;
            } else {
                logger.println("ERROR: " + cmd);
                return false;
            }
        } catch (IOException e) {
            e.printStackTrace();
            logger.println("ERROR (IOException): " + cmd);
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("ERROR (InterruptedException): " + cmd);
            return false;
        }
    }

    // Note: Properties file format is changed. TODO: Implement the new format.
    public boolean createGFClusterPropertiesFile() {

        FilePath projectWorkspace = build.getProject().getWorkspace();
        FilePath propsFile = new FilePath(projectWorkspace, "cluster.properties");
        String clusterConfig = "";

        String clusterConfigFormat =
                "#FORMAT=hostname:HTTP_LISTENER_PORT:HTTP_SSL_LISTENER_PORT:"
                + "IIOP_LISTENER_PORT:IIOP_SSL_LISTENER_PORT:JMX_SYSTEM_CONNECTOR_PORT:"
                + "IIOP_SSL_MUTUALAUTH_PORT:JMS_PROVIDER_PORT:ASADMIN_LISTENER_PORT:"
                + "instancename:clustername\n";

        String properties =
                "as.admin=" + GlassFishInstaller.GFV3BIN_DIR + "asadmin" + "\n"
                + "cluster.name=" + gfbuilder.getClusterName() + "\n";

        clusterConfig = clusterConfig + properties;
        // base values for various ports
        int http_listener_port = 8080, http_ssl_listener_port = 8181,
                iiop_ssl_listener_port = 3800, iiop_listener_port = 3700,
                jmx_system_connector_port = 7676, iiop_ssl_mutualauth_port = 3801,
                jms_provider_port = 8686, asadmin_listener_port = 4848;

        for (int i = 1; i <= gfbuilder.numInstances(); i++) {
            if (i == 1) {
                clusterConfig = clusterConfig + "instancelist=";
            }
            String iStr = "localhost:"
                    + ++http_listener_port + ":"
                    + ++http_ssl_listener_port + ":"
                    + ++iiop_ssl_listener_port + ":"
                    + ++iiop_listener_port + ":"
                    + ++jmx_system_connector_port + ":"
                    + ++iiop_ssl_mutualauth_port + ":"
                    + ++jms_provider_port + ":"
                    + ++asadmin_listener_port + ":"
                    + gfbuilder.getInstanceNamePrefix() + i;

            if (i < gfbuilder.numInstances()) {
                iStr = iStr + ",";
            } else {
                iStr = iStr + "\n";
            }
            clusterConfig = clusterConfig + iStr;
        }

        logger.println(clusterConfig);

        try {
            propsFile.write(clusterConfig, null);

        } catch (IOException e) {
            e.printStackTrace();
            logger.println("IOException !");
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("InterruptedException!");
            return false;
        }

        return true;
    }

    private String getPortNumberString() {
        return " --systemproperties "
                + "HTTP_LISTENER_PORT=" + getNextPort() + ":HTTP_SSL_LISTENER_PORT=" + getNextPort()
                + ":IIOP_SSL_LISTENER_PORT=" + getNextPort() + ":IIOP_LISTENER_PORT=" + getNextPort()
                + ":JMX_SYSTEM_CONNECTOR_PORT=" + getNextPort() + ":IIOP_SSL_MUTUALAUTH_PORT=" + getNextPort()
                + ":JMS_PROVIDER_PORT=" + getNextPort() + ":ASADMIN_LISTENER_PORT=" + getNextPort() + " ";

    }

    // Simply return next port number
    // TODO: Check if the port if avalable.
    private int getNextPort() {
        return ++baseport;
    }
}
