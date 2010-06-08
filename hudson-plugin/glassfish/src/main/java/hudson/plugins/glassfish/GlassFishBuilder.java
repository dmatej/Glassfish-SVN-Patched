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
import hudson.model.Computer;

import hudson.Launcher;
import hudson.Extension;
import hudson.util.FormValidation;
import hudson.model.AbstractBuild;
import hudson.model.BuildListener;
import hudson.model.AbstractProject;
import hudson.model.Node;
import hudson.tasks.Builder;
import hudson.tasks.BuildStepDescriptor;

import org.kohsuke.stapler.DataBoundConstructor;
import org.kohsuke.stapler.QueryParameter;

import javax.servlet.ServletException;
import java.io.IOException;

/**
 * Adds a Build Step to Create and Configure GlassFish Cluster.
 *
 * When a build is performed, the {@link #perform(Build, Launcher, BuildListener)} 
 * method will be invoked.
 *
 */

@SuppressWarnings("deprecation")

public class GlassFishBuilder extends Builder {

    private final String clusterSize, clusterName, instanceNamePrefix;
    private final boolean startCluster;

    // Fields in config.jelly match the parameter names in the "DataBoundConstructor"
    @DataBoundConstructor
    public GlassFishBuilder(
            String clusterSize,
            String clusterName,
            String instanceNamePrefix,
            boolean startCluster) {
        this.clusterSize = clusterSize.trim();
        this.clusterName = clusterName.trim();
        this.instanceNamePrefix = instanceNamePrefix.trim();
        this.startCluster = startCluster;
    }

    /**
     * We'll use this from the <tt>config.jelly</tt>.
     */
    public String getClusterSize() {
        return clusterSize;
    }

    public String getClusterName() {
        return clusterName;
    }

    public String getInstanceNamePrefix() {
        return instanceNamePrefix;
    }

    public boolean getStartCluster() {
        return startCluster;
    }

    /**
     * This is where we 'build' the project.
     * For now, we simply invoke a dummy command here.
     * TO DO: Do some useful work here.
     */
     
    @Override
    public boolean perform(AbstractBuild build, Launcher launcher, BuildListener listener) {

        String msg = "GlassFish Cluster, Name=" + clusterName + ", Size=" + clusterSize + ", will be created";

        if (getStartCluster()) {
            msg = msg + ", and started !\n";
        } else {
            msg = msg + ", but won't be started !\n";
        }

        listener.getLogger().println(msg);

        // launch a dummy command on the current computer
        Computer c = Computer.currentComputer();
        Node n = c.getNode();
        try {            
            String cmd = "hostname";
            Proc proc = launcher.launch(cmd, build.getEnvVars(), listener.getLogger(), build.getProject().getWorkspace());
            int exitCode = proc.join();
            return exitCode == 0;
        } catch (IOException e) {
            e.printStackTrace();
            listener.getLogger().println("IOException !");
            return false;
        } catch (InterruptedException e) {
            e.printStackTrace();
            listener.getLogger().println("InterruptedException!");
            return false;
        }
    }

    /**
     * Descriptor for {@link GlassFishBuilder}. Used as a singleton.
     */
    @Extension 
    public static final class DescriptorImpl extends BuildStepDescriptor<Builder> {

        /**
         * Performs on-the-fly validation of the form fields.
         *
         * @param value
         *      This parameter receives the value that the user has typed.
         * @return
         *      Indicates the outcome of the validation. This is sent to the browser.
         */
        public FormValidation doCheckClusterName(@QueryParameter String value) throws IOException, ServletException {
            value = value.trim();
            if (value.length() == 0) {
                return FormValidation.error("Please set the Cluster Name");
            }
            if (value.length() > 99) {
                return FormValidation.warning("Warning: Cluster Name is very long.");
            }
            // todo: add additional checks for Cluster Name validation here
            return FormValidation.ok();
        }

        public FormValidation doCheckInstanceNamePrefix(@QueryParameter String value) throws IOException, ServletException {
            value = value.trim();
            if (value.length() == 0) {
                return FormValidation.error("Please set the Instance Name Prefix");
            }
            if (value.length() > 99) {
                return FormValidation.warning("Warning: Instance Name Prefix is very long.");
            }
            // todo: add additional checks for Instance Name Prefix validation here
            return FormValidation.ok();
        }

        public FormValidation doCheckClusterSize(@QueryParameter String value) throws IOException, ServletException {
            if (value.length() == 0) {
                return FormValidation.error("Please set the Cluster Size");
            }

            int size = -1;
            try {
                size = Integer.parseInt(value.trim());
            } catch (NumberFormatException e) {
            }

            if (size > 999) {
                return FormValidation.warning("Are you sure ?");
            }

            if (size < 1) {
                return FormValidation.error("Invalid Cluster Size!");
            }

            // todo: add additional checks for cluster name validation here
            return FormValidation.ok();
        }

        public boolean isApplicable(Class<? extends AbstractProject> aClass) {
            // indicates that this builder can be used with all kinds of project types 
            return true;
        }

        /**
         * This human readable name is used in the configuration screen.
         */
        public String getDisplayName() {
            return "Create GlassFish Cluster";
        }
    }
}
