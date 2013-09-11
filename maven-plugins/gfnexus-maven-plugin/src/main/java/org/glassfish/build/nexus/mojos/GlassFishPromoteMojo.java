/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
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

import java.util.List;
import java.util.Set;
import org.apache.maven.plugin.MojoFailureException;
import org.glassfish.nexus.client.NexusClientException;
import org.glassfish.nexus.client.StagingAggregation;
import org.glassfish.nexus.client.beans.MavenArtifactInfo;
import org.glassfish.nexus.client.beans.Repo;
import org.sonatype.aether.RepositorySystem;
import org.sonatype.aether.RepositorySystemSession;
import org.sonatype.aether.collection.CollectRequest;
import org.sonatype.aether.collection.DependencyCollectionException;
import org.sonatype.aether.graph.Dependency;
import org.sonatype.aether.graph.DependencyNode;
import org.sonatype.aether.repository.RemoteRepository;
import org.sonatype.aether.resolution.ArtifactResult;
import org.sonatype.aether.resolution.DependencyRequest;
import org.sonatype.aether.resolution.DependencyResolutionException;
import org.sonatype.aether.util.artifact.DefaultArtifact;

/**
 * Promote a staging repository
 *
 * @goal glassfish-promote
 * @phase package
 *
 * @author Romain Grecourt
 */
public class GlassFishPromoteMojo extends AbstractNexusStagingMojo{

   /**
     * @required
     * @parameter expression="${promotionProfile}"
     */
    private String promotionProfile;

   /**
     * @parameter expression="${autoClose}" default-value="true"
     */
    private boolean autoClose;

   /**
     * The entry point to Aether, i.e. the component doing all the work.
     *
     * @component
     */
    private RepositorySystem repoSystem;

    /**
     * The current repository/network configuration of Maven.
     *
     * @parameter default-value="${repositorySystemSession}"
     * @readonly
     */
    private RepositorySystemSession repoSession;

    /**
     * The project's remote repositories to use for the resolution of project dependencies.
     *
     * @parameter default-value="${project.remoteProjectRepositories}"
     * @readonly
     */
    private List<RemoteRepository> projectRepos;

    /**
     * The project's remote repositories to use for the resolution of plugins and their dependencies.
     *
     * @parameter default-value="${project.remotePluginRepositories}"
     * @readonly
     */
    private List<RemoteRepository> pluginRepos;

    /**
     * @required
     * @parameter expression="${nexusPromotedRepoId}"
     * @readonly
     */
    private String nexusPromotedRepoId;

    /**
     *
     * @parameter expression="${aggregationProfileId}"
     * @readonly
     */
    private String aggregationProfileId;

    private List<ArtifactResult> resolveTransitive(DefaultArtifact artifact) throws MojoFailureException {
        Dependency dependency = new Dependency(artifact, "runtime");

        CollectRequest collectRequest = new CollectRequest();
        collectRequest.setRoot(dependency);

        for (RemoteRepository repo : projectRepos) {
            collectRequest.addRepository(repo);
        }
        for (RemoteRepository pluginRepo : pluginRepos) {
            collectRequest.addRepository(pluginRepo);
        }
        DependencyNode node = null;
        try {
            node = repoSystem.collectDependencies(repoSession, collectRequest).getRoot();
        } catch (DependencyCollectionException ex) {
            throw new MojoFailureException(ex.getMessage(), ex);
        }
        DependencyRequest dependencyRequest = new DependencyRequest(node, null);
        CollectRequest cr = new CollectRequest(dependency, projectRepos);

        try {
            return repoSystem.resolveDependencies(repoSession, dependencyRequest).getArtifactResults();
        } catch (DependencyResolutionException ex) {
            throw new MojoFailureException(ex.getMessage(), ex);
        }
    }

    @Override
    public void nexusMojoExecute() throws NexusClientException, MojoFailureException {

        String repoGroup = null;
        for(RemoteRepository remoteRepo : projectRepos){
            if(remoteRepo.getId().equals(nexusPromotedRepoId)){
                repoGroup = remoteRepo.getUrl().toString().replaceFirst(nexusClient.getNexusURL()+"content/groups/","");
            }
        }
        if(repoGroup == null){
            throw new MojoFailureException("unable to find a configured repository with id \""
                    +nexusPromotedRepoId+"\"");
        }

        if (stagingRepo.isOpen()) {
            if (autoClose) {
                stagingRepo.close("Autoclosing " + message);
            } else {
                throw new MojoFailureException(
                        "staging repository "
                        + stagingRepo.getId()
                        + " is open, use autoClose=true");
            }
        }
        
        // promote the staging repo separetly and initialize the aggregation
        StagingAggregation aggregation = 
                new StagingAggregation(stagingRepo.promote(promotionProfile,message));
        
        // retrieve the full content of the repository
        Set<MavenArtifactInfo> artifactsInfo = stagingRepo.getContent();

        getLog().info("");
        getLog().info(
                "-- searching for transitive dependencies available in "
                + nexusPromotedRepoId
                + " --");

        for (MavenArtifactInfo artifactInfo : artifactsInfo) {
            getLog().info("processing "+artifactInfo);

            for (ArtifactResult result :
                    resolveTransitive(new DefaultArtifact(artifactInfo.toString()))) {

                MavenArtifactInfo artifact = new MavenArtifactInfo(
                        result.getArtifact().getGroupId(),
                        result.getArtifact().getArtifactId(),
                        result.getArtifact().getVersion(),
                        result.getArtifact().getClassifier(),
                        result.getArtifact().getExtension(),
                        result.getArtifact().getFile());
                
                if(nexusClient.existsInRepoGroup(repoGroup, artifact)){

                    getLog().info(artifactInfo
                            + " is available on "+nexusPromotedRepoId);

                    Repo repo = nexusClient.getStagingRepo(artifact.getFile());

                    if(repo == null || repo.equals(stagingRepo)){
                        continue;
                    }

                    Repo parent;
                    // promote repo sepately if not done...
                    if((parent = repo.getParent()) == null){
                        parent = repo.promote(promotionProfile,message);
                    }

                    Repo grandParent;
                    if((grandParent = parent.getParent()) != null){
                            if(grandParent.getProfileName().equals(aggregationProfileId)){
                                
                                getLog().info(artifactInfo
                                    + " already promoted with "+aggregationProfileId);
                                getLog().info("removing +"+artifactInfo+" from "+grandParent.getId());

                                // remove parentfrom previous aggregation
                                Set<Repo> uncles = grandParent.getGroupTree();
                                grandParent.drop("Dropping grandparent " + message);
                                uncles.remove(parent);
                                aggregation.aggregate(
                                        new StagingAggregation(uncles).promote(aggregationProfileId,message));
                                
                            } else {
                                aggregation.aggregate(grandParent);
                            }
                    } else {
                        // not previously aggregated
                        aggregation.aggregate(parent);
                    }
                }
            }
        }
        
        aggregation.promote(aggregationProfileId,message);
    }
}