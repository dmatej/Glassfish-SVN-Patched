/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012-2013 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
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
package org.glassfish.build.nexus.mojos;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.DeploymentRepository;
import org.apache.maven.model.DistributionManagement;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.settings.Proxy;
import org.apache.maven.settings.Server;
import org.apache.maven.settings.Settings;
import org.glassfish.nexus.client.NexusClient;
import org.glassfish.nexus.client.NexusClientException;
import org.glassfish.nexus.client.NexusClientImpl;
import org.glassfish.nexus.client.RestClient;
import org.glassfish.nexus.client.logging.CustomPrinter;

/**
 *
 * @author romano
 */
public abstract class AbstractNexusMojo extends AbstractMojo implements CustomPrinter {

    /**
     * The maven project.
     *
     * @parameter expression="${project}" @required @readonly
     */
    protected MavenProject project;
    
    /**
     * The Maven Session Object
     *
     * @parameter expression="${session}"
     * @required
     * @readonly
     */
    private MavenSession session;    

    /**
     * The system settings for Maven. This is the instance resulting from
     * merging global and user-level settings files.
     *
     * @parameter expression="${settings}"
     * @readonly
     * @required
     */
    protected Settings settings;

    /**
     * @component
     * @readonly
     */
    protected ArtifactResolver artifactResolver;

    /**
     * @parameter expression="${localRepository}"
     * @required
     * @readonly
     */
    protected ArtifactRepository localRepository;
    
    /**
     * @parameter expression="${ignoreFailures}" default-value="false"
     */
    protected boolean ignoreFailures;    

    protected NexusClient nexusClient;


    @Override
    public void info(String message) {
        getLog().debug(message);
    }

    @Override
    public void warning(String message) {
        getLog().warn(message);
    }

    @Override
    public void error(String message) {
        getLog().error(message);
    }

    @Override
    public void debug(String message) {
        getLog().debug(message);
    }

    private static DeploymentRepository getDeploymentRepoFromModel(Model m) throws MojoFailureException{
        
        DistributionManagement dm = m.getDistributionManagement();
        if (dm != null) {
            DeploymentRepository r = dm.getRepository();
            if (r != null) {
                return r;
            }
        }
        throw new MojoFailureException(
                "unable to get deployment repo from distributionManagement");
    }
    
    private static String getRepoUrlFromModel(Model m) throws MojoFailureException {
        
        DeploymentRepository dr = getDeploymentRepoFromModel(m);
        String u = dr.getUrl();
        if (u != null) {
               return u;
        }
        throw new MojoFailureException(
                "unable to get repo URL from distributionManagement");
    }    
    
    private static String getRepoIdFromModel(Model m) 
            throws MojoFailureException {
        
        DeploymentRepository dr = getDeploymentRepoFromModel(m);
        String id = dr.getId();
        if (id != null) {
            return id;
        }
        throw new MojoFailureException(
                "unable to get repo Id from distributionManagement");
    }
    
    private static Server getServerFromSettings(Settings s, String repoId) throws MojoFailureException {
        if (s == null) {
                throw new IllegalArgumentException("settings can't be null");
        }
        Server server = s.getServer(repoId);
        if (server == null) {
            throw new MojoFailureException(
                    String.format(
                    "unable to retrieve the server entry for '%s' in settings.xml",
                    repoId));
        }
        return server;
    }
    
    protected void createNexusClient() throws MojoFailureException, MojoExecutionException {
        // this method resolves requires values with a specific ordering.
        // 1. plugin parameters
        String _repoURL = session.getUserProperties().getProperty("nexusRepoUrl");
        String repoId = session.getUserProperties().getProperty("nexusRepoId");
        String username = session.getUserProperties().getProperty("nexusRepoUsername");
        String password = session.getUserProperties().getProperty("nexusRepoPassword");
        
        // 2. repoId and repoURL provided as part of altDeploy, if not supplied via plugin param.   
        String altDeployRepo = session.getUserProperties().getProperty("altDeploymentRepository");
        if (altDeployRepo != null) {
            String[] tokens = altDeployRepo.split("::");
            if (tokens.length == 3) {
                repoId = tokens[0];
                _repoURL = tokens[2];
            }
        }
        
        // 3. effective-model, using distributionManagement from model    
        if (_repoURL == null) {
            _repoURL = getRepoUrlFromModel(project.getModel());
        }
        if (repoId == null) {
            repoId = getRepoIdFromModel(project.getModel());
        }

        // 4. settings.xml to get username and password, if not supplied via plugin param.
        if (username == null) {
            username = getServerFromSettings(settings, repoId).getUsername();
        }
        if (password == null) {
            password = getServerFromSettings(settings, repoId).getPassword();
        }

        createNexusClient(_repoURL, repoId, username, password);
    }
    
    protected void createNexusClient(String nexusURL, String repoId, String username, String password) throws MojoFailureException, MojoExecutionException {
        // convert the string to URL
        URL repoURL = null;
        if (nexusURL != null && !nexusURL.isEmpty()) {
            try {
                repoURL = new URL(nexusURL.replaceAll("/service/local/staging/deploy/maven2", ""));
            } catch (MalformedURLException ex) {
                throw new MojoFailureException("Error in resolving nexusRepoUrl", ex);
            }
        }
                
        // proxy configuration
        String proxyHost = null;
        int proxyPort = 80;
        for (Proxy proxy : settings.getProxies()) {
            if (proxy.getProtocol().equals(repoURL.getProtocol())) {
                proxyHost = proxy.getHost();
                proxyPort = proxy.getPort();
                continue;
            }
        }
        if (proxyHost == null) {
            Properties props = System.getProperties();
            proxyHost = props.getProperty(repoURL.getProtocol() + ".proxyHost");
            String proxyPortString = props.getProperty(repoURL.getProtocol() + ".proxyPort");
            if (proxyPortString != null) {
                proxyPort = Integer.valueOf(proxyPortString);
            }
        }

        try {
            RestClient restClient = new RestClient(
                    proxyHost, proxyPort,
                    username, password,
                    repoURL.getProtocol().equals("https"),
                    this);
            nexusClient = NexusClientImpl.init(
                    restClient,
                    repoURL.toString(),
                    this);
            
        } catch (NexusClientException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }
}