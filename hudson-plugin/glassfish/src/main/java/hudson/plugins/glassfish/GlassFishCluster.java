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
import hudson.model.Executor;
import hudson.model.Computer;
import  hudson.FilePath;
import java.io.IOException;
import java.io.PrintStream;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import java.util.Properties;
import java.io.StringReader;

/**
 * Represents GlassFish Application Server Cluster and it's Instances.
 * @author Harshad Vilekar
 */
@SuppressWarnings("deprecation")
public class GlassFishCluster {

    private static AbstractBuild build;
    private static Launcher launcher;
    private static PrintStream logger;
    private GlassFishBuilder gfbuilder;
    private int basePort;
    // Number of hosts that run the cluster instances.
    // Multiple instances may be deployed on the same host.
    int numHosts ; 
    static final int dasAdminPort = 4848, dasHttpPort = 8080, dasHostNum = 1;
    String[] clusterHosts;
    String clusterName ;
    Map<String, GlassFishInstance> clusterMap = new HashMap<String, GlassFishInstance>();
    //GlassFishCluster gfc;

    public GlassFishCluster(AbstractBuild build,
            Launcher launcher,
            PrintStream logger,
            GlassFishBuilder gfbuilder,
            int numHosts,
            int basePort,
            String clusterName) {
        this.build = build;
        this.launcher = launcher;
        this.logger = logger;
        this.gfbuilder = gfbuilder;
        this.numHosts = numHosts;
        this.basePort = basePort;
        this.clusterName = clusterName ;
    }

    // Currently, DAS and all instances run on a single host - identified by "localhost"
    // TODO: Dynamically Allocate Hudson Slaves Here (GF3.1 MS3)
    public String[] assignClusterHosts() {
        if (numHosts <= 0) {
            return null;
        }
        String[] hosts = new String[numHosts];
        for (int i = 0; i < numHosts; i++) {
            hosts[i] = "localhost";

        }
        return hosts;
    }

    public int getDasAdminPort() {
        return dasAdminPort;
    }

    public String getDasHostName() {
        return getHostName(dasHostNum);
    }

    public String getHostName(int hostNum) {
        if ((hostNum > numHosts) || (hostNum <= 0)) {
            logger.println("ERROR: Invalid Host Number:" + hostNum);
            return "";
        } else {
            return clusterHosts[hostNum - 1];
        }
    }

    /**
      * Gives a hint about availability of the specified "basePort".
      * Checks if the specified basePort is available on the current computer.
      * If the port is available - return the port number.
      * If the port is not available, return randomly selected "available" port.
      * Note: This simply indicates that the port is available at this moment.
      * The port is is not actually reserverd - and may be unavailable when the
      * GlassFish instance tries to bind to it
      */
    public static int getAvailablePort(int basePort, String portName) {
        int localPort;
        final Computer cur = Executor.currentExecutor().getOwner();
        org.jvnet.hudson.plugins.port_allocator.PortAllocationManager pam =
                org.jvnet.hudson.plugins.port_allocator.PortAllocationManager.getManager(cur);
        try {
            localPort = pam.allocateRandom(build, basePort);
            if (localPort != basePort) {
                logger.println("Updated " + portName + "=" + localPort
                        + " (" + basePort + " is not available!)");
            }
        } catch (IOException e) {
            e.printStackTrace();
            logger.println("IOException !");
            return -1;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.println("InterruptedException!");
            return -1;
        }
        return localPort;
    }

    // auto assign default values
    public void createAutoAssignedClusterMap(String instanceNamePrefix, int numInstances) {
        int base_port = this.basePort;
        for (int i = 1; i <= numInstances; i++) {
            String instanceName = instanceNamePrefix + i;
            GlassFishInstance gfi = new GlassFishInstance(this, instanceName, base_port);
            clusterMap.put(instanceName, gfi);
            logger.println("Added: " + instanceName + ":" + base_port);
            base_port = base_port + 0x100;
        }
    }

    public void listInstances() {
        for (GlassFishInstance in : clusterMap.values()) {
            logger.println(in.toStr());
        }
    }

    // get values from Custom Instance Textbox,
    // form key value pairs of instance name and base port
    // and add those instances to the cluster map.
    public boolean updateClusterMapPerUserPrefs() {

        boolean retVal = true;
        // load the instance name and value pairs from customInstanceText field
        Properties p = new Properties();

        try {
            p.load(new StringReader(gfbuilder.getCustomInstanceText()));
        } catch (IOException e) {
            logger.println("ERROR Loading customInstanceText: "
                    + gfbuilder.getCustomInstanceText());
            e.printStackTrace();
            return false;
        }

        for (Entry<Object, Object> entry : p.entrySet()) {
            String instance_name;
            int base_port;
            try {
                instance_name = entry.getKey().toString();
                base_port = new Integer(entry.getValue().toString()).intValue();
                GlassFishInstance gfi = new GlassFishInstance(this, instance_name, base_port);
                if (clusterMap.containsKey(instance_name)) {
                    logger.println("Updated: " + instance_name + ":" + base_port);
                } else {
                    logger.println("Added: " + instance_name + ":" + base_port);
                }
                clusterMap.put(instance_name, gfi);

            } catch (NumberFormatException nfe) {
                logger.println("ERROR: Invalid Entry: "
                        + entry.getKey().toString() + " " + entry.getKey().toString());
                retVal = false;
            }
        }
        return retVal;
    }

    // host1 is reserved for DAS
    // instance1..instance(numInstances) are deployed on host1..host(numHosts) in
    // round robbin
    public void assignHostsToInstances() {
        int host_num = 0;
        for (GlassFishInstance in : clusterMap.values()) {
            in.hostName = getHostName(++host_num);
            host_num = host_num % numHosts;
        }
    }

    public void updateClusterMapPerPortAvailability() {
        for (GlassFishInstance in : clusterMap.values()) {
            in.updatePerPortAvailability();
        }
    }

    /**
     *
     * Create a map of all the instances in the cluster.
     * Override auto assigned ports with user defined - if any.
     * Update port values based  upon which ports are actually free on the system
     */
    
    public boolean createClusterMap(String instanceNamePrefix, int numInstances) {

        logger.println("INFO: Cluster " + clusterName);

        clusterHosts = assignClusterHosts();

        createAutoAssignedClusterMap(instanceNamePrefix, numInstances);

        if (!updateClusterMapPerUserPrefs()) {
            logger.println("ERROR: Couldn't load customInstanceProperties, Build Aborted!");
            return false;
        }

        assignHostsToInstances();

        updateClusterMapPerPortAvailability();

        return true;
    }

    public boolean createGFClusterPropertiesFile() {

        FilePath projectWorkspace = build.getProject().getWorkspace();
        FilePath propsFile = new FilePath(projectWorkspace, "cluster.properties");        

        String properties =
                "as.admin=" + GlassFishInstaller.GFADMIN_CMD  + "\n"
                + "cluster.name=" + clusterName + "\n";       

        try {
            propsFile.write(properties, null);

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

}
