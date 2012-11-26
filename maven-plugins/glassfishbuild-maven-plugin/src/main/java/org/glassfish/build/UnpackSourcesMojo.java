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
package org.glassfish.build;

import java.io.File;
import java.util.List;
import java.util.Set;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.archiver.manager.ArchiverManager;
import org.glassfish.build.utils.MavenUtils;
import org.sonatype.aether.RepositorySystem;
import org.sonatype.aether.RepositorySystemSession;
import org.sonatype.aether.repository.RemoteRepository;
import org.sonatype.aether.resolution.ArtifactResult;
import org.sonatype.aether.util.artifact.DefaultArtifact;

/**
 * Resolves and unpack corresponding sources of project's dependencies
 *
 * @goal unpack-sources
 * @requiresDependencyResolution compile
 * @phase process-resources
 * @requiresProject
 *
 * @author Romain Grecourt
 */
public class UnpackSourcesMojo extends AbstractMojo {

    /**
     * The maven project.
     *
     * @parameter expression="${project}"
     * @required
     * @readonly
     */
    protected MavenProject project;
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
     * The project's remote repositories to use for the resolution of plugins
     * and their dependencies.
     *
     * @parameter default-value="${project.remoteProjectRepositories}"
     * @readonly
     */
    private List<RemoteRepository> remoteRepos;
    /**
     * To look up Archiver/UnArchiver implementations
     *
     * @component
     */
    protected ArchiverManager archiverManager;
    /**
     * @parameter expression="${gfbuild.unpack.includes}"
     */
    private String includes;
    /**
     * @parameter expression="${gfbuild.unpack.excludes}"
     */
    private String excludes;
    /**
     * If we should exclude transitive dependencies
     *
     * @optional
     * @parameter expression="${excludeTransitive}" default-value="false"
     */
    protected boolean excludeTransitive;
    /**
     * Comma Separated list of Types to include. Empty String indicates include
     * everything (default).
     *
     * @parameter expression="${includeTypes}" default-value=""
     * @optional
     */
    protected String includeTypes;
    /**
     * Comma Separated list of Types to exclude. Empty String indicates don't
     * exclude anything (default).
     *
     * @parameter expression="${excludeTypes}" default-value=""
     * @optional
     */
    protected String excludeTypes;
    /**
     * Scope to include. An Empty string indicates all scopes (default).
     *
     * @parameter expression="${includeScope}" default-value=""
     * @optional
     */
    protected String includeScope;
    /**
     * Scope to exclude. An Empty string indicates no scopes (default).
     *
     * @parameter expression="${excludeScope}" default-value=""
     * @optional
     */
    protected String excludeScope;
    /**
     * Comma Separated list of Classifiers to include. Empty String indicates
     * include everything (default).
     *
     * @parameter expression="${includeClassifiers}" default-value=""
     * @optional
     */
    protected String includeClassifiers;
    /**
     * Comma Separated list of Classifiers to exclude. Empty String indicates
     * don't exclude anything (default).
     *
     * @parameter expression="${excludeClassifiers}" default-value=""
     * @optional
     */
    protected String excludeClassifiers;
    /**
     * Comma separated list of Artifact names too exclude.
     *
     * @optional
     * @parameter expression="${excludeArtifactIds}" default-value=""
     */
    protected String excludeArtifactIds;
    /**
     * Comma separated list of Artifact names to include.
     *
     * @optional
     * @parameter expression="${includeArtifactIds}" default-value=""
     */
    protected String includeArtifactIds;
    /**
     * Comma separated list of GroupId Names to exclude.
     *
     * @optional
     * @parameter expression="${excludeGroupIds}" default-value=""
     */
    protected String excludeGroupIds;
    /**
     * Comma separated list of GroupIds to include.
     *
     * @optional
     * @parameter expression="${includeGroupIds}" default-value=""
     */
    protected String includeGroupIds;
    /**
     * @parameter 
     * expression="${gfbuild.unpack.outputDirectory}"
     * default-value="${project.build.directory}/sources-dependency"
     */
    private File outputDirectory;
    /**
     * @parameter 
     * expression="${gfbuild.unpack.silent}"
     * default-value="false"
     */
    private boolean silent;
    /**
     * @parameter 
     * expression="${gfbuild.unpack.attach-sources}"
     * default-value="false"
     */
    private boolean attachSources;
    
    /**
     * @parameter 
     * expression="${gfbuild.unpack.skip}"
     * default-value="false"
     */    
    private boolean skip;

    @Override
    public void execute() throws MojoExecutionException {
        if (skip) {
            getLog().info("Skipping unpack-sources");
            return;
        }        
        
        // get dependencies
        Set<Artifact> filteredDependencies = MavenUtils.filterArtifacts(
                project.getArtifacts(),
                project.getDependencyArtifacts(),
                excludeTransitive, includeScope,
                excludeScope,
                excludeTypes,
                includeTypes,
                includeClassifiers,
                excludeClassifiers,
                includeGroupIds,
                excludeGroupIds,
                includeArtifactIds,
                excludeArtifactIds);

        for (Artifact artifact : filteredDependencies) {

            // resolve sources.jar
            DefaultArtifact requestArtifact = new DefaultArtifact(
                    artifact.getGroupId(),
                    artifact.getArtifactId(),
                    "sources",
                    "jar",
                    artifact.getVersion());
            ArtifactResult result = MavenUtils.resolveArtifact(
                    requestArtifact,
                    repoSystem,
                    repoSession,
                    remoteRepos);

            // unpack
            MavenUtils.unpack(
                    result.getArtifact().getFile(),
                    outputDirectory,
                    MavenUtils.cleanToBeTokenizedString(this.includes),
                    MavenUtils.cleanToBeTokenizedString(this.excludes),
                    silent,
                    getLog(),
                    archiverManager);
        }

        if (attachSources) {
            project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        }
    }
}
