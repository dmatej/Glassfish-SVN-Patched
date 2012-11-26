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
import org.apache.maven.artifact.installer.ArtifactInstallationException;
import org.apache.maven.artifact.installer.ArtifactInstaller;
import org.apache.maven.artifact.metadata.ArtifactMetadata;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.install.AbstractInstallMojo;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.artifact.ProjectArtifactMetadata;
import org.glassfish.build.utils.MavenUtils;

/**
 * Lazy version of install:install.
 * does not require any phase to run.
 * does not require any dependency resolution.
 * does not use the maven API to get the attached files.
 * does the pomFile can be supplied.
 *
 * @goal lazy-install
 *
 * @author Romain Grecourt
 */
public class LazyInstallMojo extends AbstractInstallMojo {
    /**
     * @component
     */
    private ArtifactInstaller installer;       
    
    /**
     * @parameter default-value="${project.packaging}"
     * @required
     */
    protected String packaging;
    /**
     * @parameter default-value="${project}"
     * @required
     * @readonly
     */
    private MavenProject project;
    
    /**
     * @parameter expression="${lazyInstall.pomFile}"
     * default-value="${project.file}"
     * @required
     */
    private File pomFile;
    
    /**
     *       
     * @parameter expression="${maven.install.skip}" default-value="false"
     */
    private boolean skip;

    public void execute() throws MojoExecutionException, MojoFailureException {
        if (skip) {
            getLog().info("Skipping artifact installation");
            return;
        }
        
        // if supplied pomFile is invalid, default to the project's pom
        if (pomFile == null || !pomFile.exists()) {
            pomFile = project.getFile();
        }

        // read the model manually
        Model model = MavenUtils.readModel(pomFile);
        // create the project artifact manually
        Artifact artifact = MavenUtils.createArtifact(project.getBuild().getDirectory(), model);
        // create the project attached artifact manually
        List<Artifact> attachedArtifacts = MavenUtils.createAttachedArtifacts(project.getBuild().getDirectory(), model);

        // TODO: push into transformation
        boolean isPomArtifact = "pom".equals(packaging);

        if (updateReleaseInfo) {
            artifact.setRelease(true);
        }

        ArtifactMetadata metadata;
        try {
            if (isPomArtifact) {
                installer.install(pomFile, artifact, localRepository);
                installChecksums(artifact);
            } else {
                metadata = new ProjectArtifactMetadata(artifact, pomFile);
                artifact.addMetadata(metadata);

                File file = artifact.getFile();

                if (file != null && file.isFile()) {
                    installer.install(file, artifact, localRepository);
                    installChecksums(artifact);
                } else if (!attachedArtifacts.isEmpty()) {
                    getLog().info("No primary artifact to install, installing attached artifacts instead.");

                    Artifact pomArtifact = artifactFactory.createProjectArtifact(
                            artifact.getGroupId(),
                            artifact.getArtifactId(),
                            artifact.getBaseVersion());
                    pomArtifact.setFile(pomFile);
                    if (updateReleaseInfo) {
                        pomArtifact.setRelease(true);
                    }

                    installer.install(pomFile, pomArtifact, localRepository);
                    installChecksums(pomArtifact);
                } else {
                    throw new MojoExecutionException(
                            "The packaging for this project did not assign a file to the build artifact");
                }
            }

            for (Iterator i = attachedArtifacts.iterator(); i.hasNext();) {
                Artifact attached = (Artifact) i.next();
                installer.install(attached.getFile(), attached, localRepository);
                installChecksums(attached);
            }
        } catch (ArtifactInstallationException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }
}
