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
import java.util.Iterator;
import java.util.List;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.deployer.ArtifactDeployer;
import org.apache.maven.artifact.deployer.ArtifactDeploymentException;
import org.apache.maven.artifact.metadata.ArtifactMetadata;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.artifact.ProjectArtifactMetadata;

/**
 *
 * Deploy aggregate
 *
 * @goal pkgdeploy
 * @phase deploy
 * @requiresDependencyResolution runtime
 *
 * @author Romain Grecourt
 */
public class PkgDeploy extends AbstractAggregatorMojo {

    /**
     * @component
     */
    private ArtifactDeployer deployer;

    /**
     * @parameter expression="${localRepository}"
     * @required
     * @readonly
     */
    protected ArtifactRepository localRepository;
    
    /**
     * @parameter default-value="${project.file}"
     * @required
     * @readonly
     */
    private File pomFile;
    
    /**
     * @parameter default-value="${project.artifact}"
     * @required
     * @readonly
     */
    private Artifact artifact;
    
    /**
     * @parameter default-value="${project.attachedArtifacts}
     * @required
     * @readonly
     */
    private List attachedArtifacts;
    
    @Override
    protected void executeAggregate() throws MojoExecutionException {

        ArtifactRepository repo =
                project.getDistributionManagementArtifactRepository();

        Iterator<MavenProject> it = modules.iterator();

        try {

            // deploy all submodules
            while (it.hasNext()) {
                MavenProject p = it.next();
                getLog().info("");
                getLog().info("Deploying sub-module (" + p.getName() + ")");
                deployer.deploy(p.getArtifact().getFile(), p.getArtifact(), repo, localRepository);
            }
            
            // deploy parent module
            getLog().info("");
            getLog().info("Deploying parent-module (" + project.getName() + ")");
            ArtifactMetadata metadata = new ProjectArtifactMetadata(artifact, pomFile);
            artifact.addMetadata(metadata);
            deployer.deploy(pomFile, artifact, repo, localRepository);

            // deploy attached artifacts of parent module
            Iterator<Artifact> it2 = attachedArtifacts.iterator();
            while (it.hasNext()) {
                Artifact attached = it2.next();
                getLog().info("Deploying parent-module attached artifact (" + attached.getClassifier() + ")");
                deployer.deploy(attached.getFile(), attached, repo, localRepository);
            }
        } catch (ArtifactDeploymentException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }
}
