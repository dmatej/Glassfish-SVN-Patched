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

import hudson.Launcher;
import hudson.model.AbstractBuild;
import java.io.PrintStream;
import hudson.EnvVars;

/**
 *
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public class GlassFishAdminCmd {

    private AbstractBuild build;
    private Launcher launcher;
    private PrintStream logger;
    private GlassFishBuilder gfbuilder;
    GlassFishCluster gfc;
    final boolean daemonProcess = true;

    public GlassFishAdminCmd(AbstractBuild build,
            Launcher launcher,
            PrintStream logger,
            GlassFishBuilder gfbuilder,
            GlassFishCluster gfc) {
        this.build = build;
        this.launcher = launcher;
        this.logger = logger;
        this.gfbuilder = gfbuilder;
        this.gfc = gfc;
    }

    // make sure the default domain on DAS node can be started
    // print out the version information
    // then stop the default domain
    // return false in case of any asadmin command failures.
    public boolean verifyGFInstall() {

        // stop any earlier running domain
        String CMD = " stop-domain";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD)) {
            return false;
        }

        CMD = " start-domain";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD, daemonProcess)) {
            return false;
        }

        CMD = " version";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD)) {
            return false;
        }

        CMD = " stop-domain";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD)) {
            return false;
        }

        return true;
    }

    // make sure the default domain on DAS node can be started
    // print out the version information
    // then stop the default domain
    // return false in case of any asadmin command failures.
    public boolean getGFVersion() {

        String CMD = " version";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD)) {
            return false;
        }

        return true;
    }

    public boolean createGFCluster() {

        // stop any earlier running domain
        String CMD = " stop-domain";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD)) {
            return false;
        }

        CMD = " start-domain";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD, daemonProcess)) {
            return false;
        }

        logger.println("Creating cluster " + gfbuilder.getClusterName()
                + " with instances: ");
        gfc.listInstances();

        CMD = " create-cluster " + gfbuilder.getClusterName();
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD)) {
            return false;
        }

        for (String key : gfc.clusterMap.keySet()) {
            GlassFishInstance gfi = gfc.clusterMap.get(key);
            CMD = " --host " + gfc.getDasNodeName() + " --port " + gfc.getDasAdminPort()
                    + " create-local-instance --cluster "
                    + gfbuilder.getClusterName()
                    + " --systemproperties " + gfi.getPortList()
                    + ":instance_name=" + key + " "
                    + key;

            if (!execAdminCommand(gfi.getClusterNode(), CMD)) {
                return false;
            }
        }
        return true;
    }

    public boolean startGFCluster() {
        String CMD;

        // if DAS port is available (indicating DAS is not already running),
        // then start DAS first
        if (gfc.verifyDasPortAvailability()) {
            CMD = " start-domain";
            if (!execAdminCommand(gfc.getDasClusterNode(), CMD, daemonProcess)) {
                return false;
            }
        }
        
        for (String key : gfc.clusterMap.keySet()) {
            GlassFishInstance gfi = gfc.clusterMap.get(key);
            CMD = " start-local-instance " + key;
            if (!execAdminCommand(gfi.getClusterNode(), CMD, daemonProcess)) {
                return false;
            }
        }

        return true;
    }

    // stop the cluster, delete all instances, delete the cluster, stop the domain
    public boolean stopGFCluster() {

        String CMD;
        boolean returnValue = true;

        for (String key : gfc.clusterMap.keySet()) {
            GlassFishInstance gfi = gfc.clusterMap.get(key);
            CMD = " stop-local-instance " + key;
            if (!execAdminCommand(gfi.getClusterNode(), CMD)) {
                //continue, and try to stop other instances, even if this attempt failed
                returnValue = false;
            }
        }

        // todo: optionally, delete-local-instance and delete-cluster

        if (!stopDomain()) {
            returnValue = false;
        }

        return returnValue;
    }

    public boolean stopDomain() {
        String CMD = " stop-domain";
        if (!execAdminCommand(gfc.getDasClusterNode(), CMD)) {
            return false;
        }
        return true;
    }

    public boolean execAdminCommand(GlassFishClusterNode clusterNode, String cmd) {
        return (execAdminCommand(clusterNode, cmd, false));
    }

    public boolean execAdminCommand(GlassFishClusterNode clusterNode, String cmd, boolean daemon) {

        EnvVars additionalEnvVars = null;

        if (daemon) {
            //Tells Hudson to not kill daemon processes (like start instance, start domain).
            //see http://issues.hudson-ci.org/browse/HUDSON-2729
            String key = "BUILD_ID", value = "doNotKill";

            additionalEnvVars = new EnvVars(key, value);
        }
        return (clusterNode.execCommand(additionalEnvVars, clusterNode.getInstaller().getAdminCmd() + " " + cmd));
    }
}
