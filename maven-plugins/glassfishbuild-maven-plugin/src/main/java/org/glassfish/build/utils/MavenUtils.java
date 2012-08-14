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
package org.glassfish.build.utils;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.installer.ArtifactInstallationException;
import org.apache.maven.artifact.installer.ArtifactInstaller;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.DefaultModelWriter;
import org.apache.maven.model.io.ModelWriter;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.artifact.ProjectArtifactMetadata;
import org.apache.maven.shared.artifact.filter.collection.*;

/**
 *
 * @author Romain Grecourt
 */
public class MavenUtils {
    private static ModelWriter modelWriter = new DefaultModelWriter();

    public static Artifact createArtifact(
            String groupId, String artifactId, String version, String type, String classifier) {
        
        return new DefaultArtifact(
                groupId,
                artifactId,
                VersionRange.createFromVersion(version),
                "runtime",
                type,
                classifier,
                new DefaultArtifactHandler(type));
    }
    
    public static Artifact createArtifact(
            String groupId, String artifactId, String version, String type) {
        
        return createArtifact(
                groupId,
                artifactId,
                version,
                type,
                null);
    }
    
    public static Artifact createArtifact(Dependency dep) {
        return createArtifact(
                dep.getGroupId(),
                dep.getArtifactId(),
                dep.getVersion(),
                dep.getType(),
                dep.getClassifier());
    }    

    public static void writePom(Model m, File buildDir) throws IOException {
        File pomFile = new File(buildDir, m.getArtifactId() + ".pom");
        modelWriter.write(pomFile, null, m);
        m.setPomFile(pomFile);
    }

    public static void installPom(
            ArtifactInstaller installer,
            ArtifactRepository repository,
            MavenProject p,
            Model m) throws MojoExecutionException {
        
        Artifact artifact = createArtifact(
                p.getArtifact().getGroupId(),
                p.getArtifact().getArtifactId(),
                p.getArtifact().getVersion(),
                p.getArtifact().getType());
        artifact.addMetadata(new ProjectArtifactMetadata(artifact, m.getPomFile()));

        try {
            installer.install(m.getPomFile(), artifact, repository);
        } catch (ArtifactInstallationException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }

    public static void installAttached(
            ArtifactInstaller installer, 
            ArtifactRepository repository,
            MavenProject project,
            File file,
            String type,
            String classifier) throws MojoExecutionException {
        
        Artifact attached = createArtifact(
                project.getGroupId(),
                project.getArtifactId(),
                project.getVersion(),
                type,
                classifier);

        try {
            installer.install(file, attached, repository);
        } catch (ArtifactInstallationException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }
    
    public static String getManagedVersion(
            MavenProject module, String groupId, String artifactId) {
        
        // 1st look at the dependencyManagement
        for (Dependency d : module.getModel().getDependencyManagement().getDependencies()) {
            if (d.getGroupId().equals(groupId)
                    && d.getArtifactId().equals(artifactId)) {
                return d.getVersion();
            }
        }
        // 2nd, look at the modules dependencies
        for(Artifact a : getDependencySet(module, null)){
            if (a.getGroupId().equals(groupId)
                    && a.getArtifactId().equals(artifactId)) {
                return a.getVersion();
            }
        }
        // 3rd, if groupId match the module return module's version
        if(module.getGroupId().equals(groupId))
            return module.getModel().getVersion();
        else 
            return "";
    }
    
    public static Set<Artifact> getDependencySet(
            MavenProject p, Map<MavenProject, Set<Artifact>> dependencySets) {

        if (dependencySets != null && 
                dependencySets.containsKey(p)) {
            return dependencySets.get(p);
        }
        
        FilterArtifacts filter = new FilterArtifacts();
        filter.addFilter(new ProjectTransitivityFilter(p.getDependencyArtifacts(), false));
        filter.addFilter(new ScopeFilter("runtime", "test"));
        filter.addFilter(new TypeFilter("jar", ""));

        Set<Artifact> artifacts = new HashSet<Artifact>();
        try {
            artifacts = filter.filter(p.getArtifacts());
        } catch (ArtifactFilterException e) { }
        
        if(dependencySets != null){
            dependencySets.put(p, artifacts);
        }
        return artifacts;
    }
}
