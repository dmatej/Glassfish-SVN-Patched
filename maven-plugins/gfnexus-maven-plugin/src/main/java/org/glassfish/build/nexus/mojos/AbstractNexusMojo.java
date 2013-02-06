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
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.VersionRange;
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
import org.glassfish.nexus.client.beans.MavenArtifactInfo;
import org.glassfish.nexus.client.beans.Repo;
import org.glassfish.nexus.client.logging.CustomHandler;

/**
 *
 * @author romano
 */
public abstract class AbstractNexusMojo extends AbstractMojo{

    /**
     * The maven project.
     *
     * @parameter expression="${project}" @required @readonly
     */
    protected MavenProject project;

    /**
     * The system settings for Maven. This is the instance resulting from
     * merging global and user-level settings files.
     *
     * @parameter expression="${settings}"
     * @readonly
     * @required
     */
    private Settings settings;

    /**
     * @component
     * @readonly
     */
    private ArtifactResolver artifactResolver;

    /**
     * @parameter expression="${localRepository}"
     * @required
     * @readonly
     */
    protected ArtifactRepository localRepository;

    /**
     * Coordinates of the reference artifact used to verify repositories
     * groupId:artifactId:version
     *
     * @required
     * @parameter expression="${referenceCoordinates}"
     */
    private String referenceCoordinates;

    /**
     * Staging profile
     *
     * @required
     * @parameter expression="${stagingProfile}"
     */
    private String stagingProfile;
    
   /**
     * @parameter expression="${ignoreFailures}" default-value="false"
     */
    private boolean ignoreFailures;    

    protected NexusClient nexusClient;

    protected MavenArtifactInfo refArtifact;

    protected Repo stagingRepo;

    protected URL repoURL;

    private class RestLoggerHandler extends CustomHandler {

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
    }

    private class NexusClientLoggerHandler extends CustomHandler {

        @Override
        public void info(String message) {
            getLog().info(message);
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
    }
    
    private static Artifact parseCoordinates(String coordinates,String projectVersion) throws MojoFailureException{
        String[] referenceCoordinatesTokens = coordinates.split(":");

        if(referenceCoordinatesTokens.length < 2){
            throw new MojoFailureException("unable to parse referenceArtifact");
        }

        String version = projectVersion;
        if(referenceCoordinatesTokens.length > 2){
            version = referenceCoordinatesTokens[2];
        }
        
        String packaging = "jar";
        if(referenceCoordinatesTokens.length > 3){
            packaging = referenceCoordinatesTokens[3];
        }

        return new DefaultArtifact(
                referenceCoordinatesTokens[0],
                referenceCoordinatesTokens[1],
                VersionRange.createFromVersion(version),
                "runtime",
                packaging,
                null,
                new DefaultArtifactHandler(packaging));        
    }
    
    private void createNexusClient() throws MojoFailureException, MojoExecutionException{
        try {
            repoURL = new URL(
                    project.getModel().getDistributionManagement().getRepository().getUrl().replaceAll("/service/local/staging/deploy/maven2", ""));
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
                if(proxyPortString != null){
                    proxyPort = Integer.valueOf(proxyPortString);
                }
            }
            String repoId = project.getModel().getDistributionManagement().getRepository().getId();
            Server server = settings.getServer(repoId);

            nexusClient = NexusClientImpl.init(
                    new RestClient(
                        proxyHost, proxyPort,
                        server.getUsername(), server.getPassword(),
                        repoURL.getProtocol().equals("https"),
                        new RestLoggerHandler())
                    ,repoURL.toString()
                    ,new NexusClientLoggerHandler());
        } catch (NexusClientException ex){
            throw new MojoExecutionException(ex.getMessage(),ex);
        } catch (MalformedURLException ex) {
            throw new MojoFailureException("error while initializing NexusClient", ex);
        }        
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {

        Artifact artifact = 
                parseCoordinates (referenceCoordinates,project.getVersion());
        
        try {
            artifactResolver.resolve(artifact,
                    project.getRemoteArtifactRepositories(),
                    localRepository);
        } catch (ArtifactResolutionException ex) {
            throw new MojoFailureException(ex.getMessage(), ex);
        } catch (ArtifactNotFoundException ex) {
            throw new MojoFailureException(ex.getMessage(), ex);
        }

        refArtifact = new MavenArtifactInfo(
                artifact.getGroupId(),
                artifact.getArtifactId(),
                artifact.getVersion(),
                artifact.getClassifier(),
                artifact.getType(),
                artifact.getFile());
        
        createNexusClient();
        
        try {
            stagingRepo = nexusClient.getStagingRepo(stagingProfile, refArtifact);
            nexusMojoExecute();
        } catch (NexusClientException ex) {
            if (!ignoreFailures) {
                throw ex;
            }
        }
    }

    public abstract void nexusMojoExecute() throws NexusClientException, MojoFailureException;
}