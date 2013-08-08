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
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.glassfish.nexus.client.NexusClientException;
import org.glassfish.nexus.client.beans.MavenArtifactInfo;
import org.glassfish.nexus.client.beans.Repo;

/**
 *
 * @author romano
 */
public abstract class AbstractNexusStagingMojo extends AbstractNexusMojo {
    /**
     * Coordinates of the reference artifact used to verify repositories
     * groupId:artifactId:version
     *
     * @required
     * @parameter expression="${stagingRepos}"
     */
    private List<StagingRepo> stagingRepos;

    /**
     * @parameter expression="${nexusRepoUrl}"
     * @readonly
     */
    protected String nexusRepoUrl = null;
    
    /**
     * @parameter expression="${nexusRepoId}"
     * @readonly
     */
    protected String nexusRepoId = null;
    
    /**
     * @parameter expression="${nexusRepoUsername}"
     * @readonly
     */
    protected String nexusRepoUsername = null;
    
    /**
     * @parameter expression="${nexusRepoPassword}"
     * @readonly
     */
    protected String nexusRepoPassword = null;
    
    private MavenArtifactInfo refArtifact;

    protected Repo stagingRepo;    

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        // init
        try {
            createNexusClient(
                    new URL(nexusRepoUrl),
                    nexusRepoId,
                    nexusRepoUsername,
                    nexusRepoPassword);
        } catch (MalformedURLException ex) {
            throw new MojoExecutionException(ex.getMessage(),ex);
        }
        
        for (StagingRepo repo : stagingRepos) {
            Artifact artifact = repo.getRefArtifact(project.getVersion());
            if(artifact == null){
                continue;
            }
            try {
                artifactResolver.resolve(
                        artifact,
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

            try {
                stagingRepo = nexusClient.getStagingRepo(repo.getProfile(), refArtifact);
                nexusMojoExecute();
            } catch (NexusClientException ex) {
                if (!ignoreFailures) {
                    throw ex;
                }
            }
        }
    }

    public abstract void nexusMojoExecute() throws NexusClientException, MojoFailureException;
}