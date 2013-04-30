/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
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
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.Resource;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.tools.ant.types.ZipFileSet;
import org.codehaus.plexus.archiver.manager.ArchiverManager;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.glassfish.build.utils.MavenUtils;
import org.sonatype.aether.RepositorySystem;
import org.sonatype.aether.RepositorySystemSession;
import org.sonatype.aether.repository.RemoteRepository;
import org.sonatype.aether.resolution.ArtifactResult;

/**
 * Resolves and unpack corresponding sources of project's dependencies
 *
 * @goal make-source-bundle
 * @requiresDependencyResolution runtime
 * @phase package
 * @requiresProject
 *
 * @author Romain Grecourt
 */
public class SourceBuildMojo extends AbstractMojo {

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
     * @parameter 
     * expression="${gfbuild.sourcebuild.outputDirectory}"
     * default-value="${project.build.directory}/sources-bundle"
     */
    private File outputDirectory;
    /**
     * @parameter 
     * expression="${gfbuild.sourcebuild.globalIncludes}"
     */
    private List<String> globalIncludes;
    
    /**
     * @parameter 
     * expression="${gfbuild.sourcebuild.skip}"
     * default-value="false"
     */    
    private boolean skip;
    
    private static Artifact getArtifactItem(MavenProject p, Xpp3Dom aItem) throws MojoExecutionException {
        if (aItem == null) {
            return null;
        }
        if (aItem.getName().equals("artifactItem")) {
            Xpp3Dom[] gId = aItem.getChildren("groupId");
            Xpp3Dom[] aId = aItem.getChildren("artifactId");
            Xpp3Dom[] type = aItem.getChildren("type");
            Xpp3Dom[] v = aItem.getChildren("version");
            if (gId != null && gId.length == 1
                    && aId != null && aId.length == 1) {
                
                String groupId = gId[0].getValue();
                if (!groupId.startsWith("org.glassfish")) {
                    return null;
                }
                String artifactId = aId[0].getValue();
                String extension = (type != null && type.length == 1)
                        ? type[0].getValue()
                        : "jar";
                String version = (v != null && v.length == 1)
                        ? v[0].getValue()
                        : MavenUtils.getManagedVersion(p, groupId, artifactId);
                return MavenUtils.createArtifact(
                        groupId,
                        artifactId,
                        extension,
                        version);
            }
        }
        return null;
    }
    
    private static Set<Artifact> getArtifactItems(MavenProject p) throws MojoExecutionException{
        Set<Artifact> artifactItems = new HashSet<Artifact>();

        // scan dependency plugin for possible dependencies
        Plugin dependencyPlugin = p.getPlugin("org.apache.maven.plugins:maven-dependency-plugin");
        if(dependencyPlugin != null){
            for(PluginExecution pe : dependencyPlugin.getExecutions()){
                Xpp3Dom dom = (Xpp3Dom) pe.getConfiguration();
                if(dom == null){
                    continue;
                }
                Xpp3Dom[] aItemsNodes = dom.getChildren("artifactItems");
                if (aItemsNodes != null && aItemsNodes.length > 0) {
                    for (Xpp3Dom aItemsNode : aItemsNodes) {
                        Xpp3Dom[] aItems = aItemsNode.getChildren("artifactItem");
                        for (Xpp3Dom aItem : aItems) {
                            artifactItems.add(getArtifactItem(p,aItem));
                        }
                    }
                }
            }
            Xpp3Dom dom = (Xpp3Dom) dependencyPlugin.getConfiguration();
            if (dom != null) {
                Xpp3Dom[] aItems = dom.getChildren("artifactItems");
                if (aItems != null && aItems.length > 0) {
                    for (Xpp3Dom aItem : aItems) {
                        artifactItems.add(getArtifactItem(p,aItem));
                    }
                }
            }
        }
        return artifactItems;
    }
    
    private static void processModel(File pom) throws MojoExecutionException {
        Model m = MavenUtils.readModel(pom);

        // update the model to remove relativePath for parent poms
        if (m.getParent() != null
                && m.getParent().getRelativePath() != null
                && !m.getParent().getRelativePath().isEmpty()) {
            m.getParent().setRelativePath("NOTHING");
            try {
                String ms = new String(MavenUtils.writePom(m));
                ms = ms.replaceFirst("<relativePath>NOTHING</relativePath>", "<relativePath/>");
                FileWriter writer = new FileWriter(pom);
                writer.write(ms);
                writer.flush();
                writer.close();
            } catch (IOException ex) {
                throw new MojoExecutionException(ex.getMessage(), ex);
            }
        }
    }
    
    private List<String> processModules(Set<Artifact> dependencies) 
            throws MojoExecutionException {

        List<String> modules = new ArrayList<String>();
        
        for (Artifact artifact : dependencies) {
            // resolve sources.type
            ArtifactResult sourceResult = MavenUtils.resolveArtifact(
                    artifact.getGroupId(),
                    artifact.getArtifactId(),
                    "sources",
                    artifact.getType(),
                    artifact.getVersion(),
                    repoSystem,
                    repoSession,
                    remoteRepos);

            String moduleDir =
                    sourceResult.getArtifact().getArtifactId()
                    + "-"
                    + sourceResult.getArtifact().getVersion();
            File dir = new File(outputDirectory, moduleDir);

            // unpack
            MavenUtils.unpack(
                    sourceResult.getArtifact().getFile(),
                    dir,
                    "**/*",
                    "",
                    false,
                    getLog(),
                    archiverManager);

            // lay down the pom.xml
            File pom = new File(dir, "pom.xml");
            if (!pom.exists()) {
                ArtifactResult pomResult = 
                        MavenUtils.resolveArtifact(
                        artifact.getGroupId(),
                        artifact.getArtifactId(),
                        null,
                        "pom",
                        artifact.getVersion(),
                        repoSystem,
                        repoSession,
                        remoteRepos);
                try {
                    FileUtils.copyFile(pomResult.getArtifact().getFile(), pom);
                } catch (IOException ex) {
                    throw new MojoExecutionException(ex.getMessage(), ex);
                }
            }
            processModel(pom);
            modules.add(moduleDir);
        }
        return modules;
    }
    
    private void processAggregator(List<String> modules) throws MojoExecutionException{
        // create aggregator
        Model m = new Model();
        m.setGroupId(project.getGroupId());
        m.setArtifactId("source-build-aggregator");
        m.setVersion(project.getVersion());
        m.setModules(modules);
        m.setModelVersion("4.0.0");
        m.setPackaging("pom");
        try {
            MavenUtils.writePom(m, outputDirectory);
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }        
    }
    
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if (skip) {
            getLog().info("Skipping make-source-bundle");
            return;
        }

        // main bundle
        List<ZipFileSet> filesets = getProjectContent();
        List<String> modules = new ArrayList<String>();
        modules.add(project.getArtifactId()+"-" + project.getArtifact().getVersion());        
        
        // get dependencies
        Set<Artifact> dependencies = getArtifactItems(project);
        dependencies.addAll(project.getDependencyArtifacts());
        dependencies = MavenUtils.excludeTransitive(
                project.getArtifacts(),
                dependencies);        
        filesets.add(MavenUtils.createZipFileSet(outputDirectory, "",""));
        modules.addAll(processModules(dependencies));

        // create aggregator pom
        processAggregator(modules);
        
        // compute final name
        String type = project.getArtifact().getFile().getName();
        int idx = type.lastIndexOf(".");
        type = type.substring(idx+1);
        File target = new File(
                project.getBuild().getDirectory(), 
                project.getBuild().getFinalName()+"-sources."+type);
        
        MavenUtils.createZip(
                project.getProperties(),
                getLog(),
                "add",
                filesets,
                target);
        
        // attached produced artifact
        Artifact sourceArtifact = 
                MavenUtils.createArtifact(
                project.getGroupId(),
                project.getArtifactId(),
                project.getVersion(),
                type,
                "sources");
        sourceArtifact.setFile(target);
        project.addAttachedArtifact(sourceArtifact);
    }
    
    private String getRelativePath(File f){
        String path = f.getAbsolutePath().replaceFirst(
                project.getBasedir().getAbsolutePath(), "");
        if (path.startsWith("/")) {
            path = path.substring(1, path.length());
        }
        return path;
    }
    
    protected List<ZipFileSet> getProjectContent() throws MojoExecutionException{
        List<ZipFileSet> filesets = new ArrayList<ZipFileSet>();
        String moduleDir = project.getArtifactId()
                +"-"
                + project.getArtifact().getVersion();
        
        // process the pom
        File pom = new File(outputDirectory,moduleDir+"/pom.xml");
        try {
            FileUtils.copyFile(project.getFile(), pom);
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
        processModel(pom);
        filesets.add(
                MavenUtils.createZipFileSet(
                outputDirectory,
                moduleDir+"/**", ""));
        
        List<String> includes = new ArrayList<String>(globalIncludes);
        List<String> excludes = new ArrayList<String>();

        // sources
        for (String s : project.getCompileSourceRoots()) {
            File sourceDirectory = new File(s);
            if (sourceDirectory.exists()) {
                includes.add(getRelativePath(sourceDirectory)+"/**");
            }
        }
        
        // resources
        List<Resource> resources = project.getResources();
        if (resources != null && !resources.isEmpty()) {
            for (Resource resource : resources) {

                File sourceDirectory = new File(resource.getDirectory());
                if (!sourceDirectory.exists()) {
                    continue;
                }
                
                String path = getRelativePath(sourceDirectory);
                if(resource.getIncludes().isEmpty()){
                    includes.add(path+"/**");
                } else {
                    for(String s : resource.getIncludes()){
                        includes.add(path+"/"+s);
                    }
                }
                for(String s : resource.getExcludes()){
                     excludes.add(path+"/"+s);
                }
            }
        }
        excludes.add("target/**");
        filesets.add(
                MavenUtils.createZipFileSet(
                project.getBasedir(),
                moduleDir,
                includes,
                excludes));
        
        return filesets;
    }
}
