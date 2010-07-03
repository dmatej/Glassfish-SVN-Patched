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
import hudson.model.AbstractBuild;
import java.util.Map;
import java.io.IOException;
import java.io.PrintStream;


/**
 *
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public class GlassFishAdminCmd {

    private static AbstractBuild build;
    private static Launcher launcher;
    private static PrintStream logger;
    private GlassFishBuilder gfbuilder;
    private String adminCmd;
    GlassFishCluster gfc;
    GlassFishInstaller gfi;

    public GlassFishAdminCmd(AbstractBuild build,
            Launcher launcher,
            PrintStream logger,
            GlassFishBuilder gfbuilder,
            GlassFishCluster gfc,
            GlassFishInstaller gfi) {
        this.build = build;
        this.launcher = launcher;
        this.logger = logger;
        this.gfbuilder = gfbuilder;
        this.gfc = gfc;
        this.gfi = gfi;
        adminCmd = gfi.GFADMIN_CMD ;
    }

    public boolean createGFCluster() {

        String CMD = adminCmd + " start-domain";
        if (!execCommand(CMD)) {
            return false;
        }

        logger.println("Creating cluster " + gfbuilder.getClusterName()
                + " with instances: ");
        gfc.listInstances();

        CMD = adminCmd + " create-cluster " + gfbuilder.getClusterName();
        if (!execCommand(CMD)) {
            return false;
        }

        for (String key : gfc.clusterMap.keySet()) {
            GlassFishInstance gfi = gfc.clusterMap.get(key);
            CMD = adminCmd + " --host " + gfc.getDasHostName() + " --port " + gfc.getDasAdminPort()
                    + " create-local-instance --cluster "
                    + gfbuilder.getClusterName()
                    + " --systemproperties " + gfi.getPortList()
                    + key;

            if (!execCommand(CMD)) {
                return false;
            }
        }
        return true;
    }

    public boolean startGFCluster() {
        String CMD;

        for (String key : gfc.clusterMap.keySet()) {

            CMD = adminCmd + " start-local-instance " + key;
            if (!execCommand(CMD)) {
                return false;
            }
        }


        CMD = adminCmd + " list-instances ";
        if (!execCommand(CMD)) {
            return false;
        }

        return true;
    }

    // stop the cluster, delete all instances, delete the cluster, stop the domain
    public boolean stopGFCluster() {

        String CMD;

        for (String key : gfc.clusterMap.keySet()) {

            CMD = adminCmd + " stop-local-instance " + key;
            if (!execCommand(CMD)) {
                return false;
            }
        }

        CMD = adminCmd + " list-instances ";
        if (!execCommand(CMD)) {
            return false;
        }

        /****************
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
         *****************/
        CMD = adminCmd + " stop-domain";
        if (!execCommand(CMD)) {
            return false;
        }

        return true;
    }

    public boolean execCommand(String cmd) {
        try {
            Map envVars = build.getEnvVars() ;
            //Tells Hudson to not kill daemon processes (like start instance, start domain).
            //see http://issues.hudson-ci.org/browse/HUDSON-2729
            envVars.put("BUILD_ID", "doNotKill");
            Proc proc = launcher.launch(cmd, envVars, logger, build.getProject().getWorkspace());
            
            int exitCode = proc.join();
            if (exitCode == 0) {
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
}
