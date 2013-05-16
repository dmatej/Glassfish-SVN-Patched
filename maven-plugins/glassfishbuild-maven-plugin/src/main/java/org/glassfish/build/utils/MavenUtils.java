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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.installer.ArtifactInstallationException;
import org.apache.maven.artifact.installer.ArtifactInstaller;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.building.DefaultModelBuildingRequest;
import org.apache.maven.model.building.ModelBuilder;
import org.apache.maven.model.io.DefaultModelReader;
import org.apache.maven.model.io.DefaultModelWriter;
import org.apache.maven.model.resolution.ModelResolver;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.artifact.ProjectArtifactMetadata;
import org.apache.maven.shared.artifact.filter.collection.*;
import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.ZipFileSet;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.plexus.archiver.ArchiverException;
import org.codehaus.plexus.archiver.UnArchiver;
import org.codehaus.plexus.archiver.manager.ArchiverManager;
import org.codehaus.plexus.archiver.manager.NoSuchArchiverException;
import org.codehaus.plexus.components.io.fileselectors.IncludeExcludeFileSelector;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.IOUtil;
import org.codehaus.plexus.util.StringUtils;
import org.codehaus.plexus.util.WriterFactory;
import org.codehaus.stax2.XMLInputFactory2;
import org.sonatype.aether.RepositorySystem;
import org.sonatype.aether.RepositorySystemSession;
import org.sonatype.aether.repository.RemoteRepository;
import org.sonatype.aether.resolution.ArtifactRequest;
import org.sonatype.aether.resolution.ArtifactResolutionException;
import org.sonatype.aether.resolution.ArtifactResult;

/**
 *
 * @author Romain Grecourt
 */
public class MavenUtils {
    
    public static Model resolveEffectiveModel(
            ModelBuilder modelBuilder,
            ModelResolver modelResolver,
            File pomfile) {
        
        try {
            DefaultModelBuildingRequest mbr = new DefaultModelBuildingRequest();
            mbr.setPomFile(pomfile);
            mbr.setModelResolver(modelResolver);
            return modelBuilder.build(mbr).getEffectiveModel();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }    
    
    /**
     * Reads a given model
     * @param pom the pom File
     * @return an instance of Model
     * @throws MojoExecutionException
     */
    public static Model readModel(File pom) throws MojoExecutionException{
        try {
           return new DefaultModelReader().read(pom, null);
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(),ex);
        }
    }
    
    /**
     * Reads a given model
     * @param pom the pom File
     * @return an instance of Model
     * @throws MojoExecutionException
     */
    public static Model readModel(String input) throws MojoExecutionException {
        try {
           return new DefaultModelReader().read(new ByteArrayInputStream(input.getBytes("UTF-8")), null);
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(),ex);
        }
    }
    
    private static String getFinalName(Model model){
        Build b = model.getBuild();
        String finalName;
        if(b != null && b.getFinalName() !=null){
            finalName = b.getFinalName();
        } else {
            String version = model.getVersion() != null ? model.getVersion() : model.getParent().getVersion();
            finalName = model.getArtifactId()+"-"+version;
        }
        return finalName;
    }
    
    /**
     * Create a list of attached artifacts and their associated files by searching for target/${project.build.finalName}-*.*
     * 
     * @param dir ${project.build.directory}
     * @param model an instance of model
     * @return
     * @throws MojoExecutionException
     */
    public static List<Artifact> createAttachedArtifacts(String dir, Artifact artifact, Model model)
            throws MojoExecutionException {

        String artifactName = "", finalName;
        
        // compute finalName
        if(artifact!=null 
                && artifact.getFile() != null
                && artifact.getFile().exists()){
            artifactName = artifact.getFile().getName();
            finalName = artifactName.substring(0, artifactName.lastIndexOf('.'));
        } else {
            finalName = getFinalName(model);
        }
        
        List<File> attachedFiles = MavenUtils.getFiles(
                    dir,
                    finalName+"*.*",
                    artifactName);
        
        List<Artifact> attachedArtifacts = new ArrayList<Artifact>();
        if(!attachedFiles.isEmpty()){
            for(File attached : attachedFiles){
                String tokens = attached.getName().substring(finalName.length());
                
                // pom is not an attached artifact
                if(tokens.endsWith(".pom")){
                    continue;
                }
                
                String type;
                if(tokens.endsWith(".asc")){
                    // compute type as xxx.asc
                    type = tokens.substring(
                            tokens.substring(0,tokens.length()-4).lastIndexOf('.')+1,
                            tokens.length());
                } else {
                    type = tokens.substring(
                            tokens.lastIndexOf('.')+1,
                            tokens.length());
                }
                
                String classifier;
                if(tokens.endsWith(".pom.asc")){
                    // pom.asc does not have any classifier
                    classifier = "";
                } else {
                    // classifier = tokens - type
                    classifier = tokens.substring(
                            tokens.lastIndexOf('-') + 1,
                            tokens.length() - (type.length() + 1));

                    if (classifier.contains(artifact.getVersion())) {
                        classifier = classifier.substring(
                                classifier.indexOf(artifact.getVersion() + 1,
                                classifier.length() - (artifact.getVersion().length())));
                    }
                }
                
                Artifact attachedArtifact = MavenUtils.createArtifact(model,type,classifier);
                attachedArtifact.setFile(attached);
                attachedArtifacts.add(attachedArtifact);
            }
        }
        return attachedArtifacts;
    }
    
    private static Artifact getArtifactFile(String dir, String finalName, Model model) throws MojoExecutionException{
        List<File> files = MavenUtils.getFiles(
                    dir,
                    finalName+".*",
                    finalName+"-*.");
        
        Map<String, File> extensionMap = new HashMap<String, File>(files.size());
        for (File f : files) {
            extensionMap.put(f.getName().substring(finalName.length() + 1), f);
        }
        
        // 1. guess the extension from the packaging
        File artifactFile = extensionMap.get(model.getPackaging());
        if(artifactFile != null){
            Artifact artifact = MavenUtils.createArtifact(model);
            artifact.setFile(artifactFile);
            return artifact;
        }
        // 2. take what's available
        for(String ext : extensionMap.keySet()){
            if(!ext.equals("pom") 
                    && !ext.endsWith(".asc")){
                // packaging does not match the type
                // hence we provide type = ext
                Artifact artifact = MavenUtils.createArtifact(model,ext,null);
                artifact.setFile(extensionMap.get(ext));
                return artifact;
            }
        }
        return null;
    }
    
    /**
     * Create an artifact and its associated file by searching for target/${project.build.finalName}.${project.packaging}
     * 
     * @param dir ${project.build.directory}
     * @param model an instance of model
     * @return
     * @throws MojoExecutionException
     */
    public static Artifact createArtifact(String dir, Model model) throws MojoExecutionException{
        // resolving using finalName
        Artifact artifact = getArtifactFile(dir,getFinalName(model),model);
        if(artifact == null){
            // resolving using artifactId
            artifact = getArtifactFile(dir,model.getArtifactId(),model);
        }
        return artifact;
    }
    
    /**
     * Returns the pom installed in target or null if not found
     * 
     * @param dir ${project.build.directory}
     * @return an instance of the pom file or null if not found
     * @throws MojoExecutionException
     */
    public static File getPomInTarget(String dir) throws MojoExecutionException{
        // check for an existing .pom
         List<File> poms = MavenUtils.getFiles(
                    dir,
                    "*.pom",
                    "");
         if(!poms.isEmpty()){
            return poms.get(0);
         }
         return null;
    }

    /**
     * Return the files contained in the directory, using inclusion and exclusion Ant patterns
     * 
     * @param dir the directory to scan
     * @param includes the includes pattern, comma separated
     * @param excludes the excludes pattern, comma separated
     * @return
     * @throws MojoExecutionException if an IO exception occurred
     */
    public static List<File> getFiles(String dir, String includes, String excludes)
            throws MojoExecutionException{

        File f = new File(dir);
        if (f.exists() && f.isDirectory()) {
            try {
                return FileUtils.getFiles(f, includes, excludes);
            } catch (IOException ex) {
                throw new MojoExecutionException(ex.getMessage(), ex);
            }
        }
        return Collections.EMPTY_LIST;
    }
    
    /**
     * Creates an artifact instance for the supplied model
     * 
     * @param m the model
     * @return the created artifact
     */
    public static Artifact createArtifact(Model m) {
        return createArtifact(m,m.getPackaging(),null);
    }
    
    /**
     * Creates an artifact instance for the supplied model
     * 
     * @param m the model
     * @param type the type of the artifact
     * @param classifier the classifier to use
     * @return the created artifact
     */
    public static Artifact createArtifact(Model m, String type, String classifier) {
        
        String groupId = m.getGroupId();
        groupId = (groupId == null?m.getParent().getGroupId():groupId);
        String version = m.getVersion();
        version = (version == null?m.getParent().getVersion():version);

        return new DefaultArtifact(
                groupId,
                m.getArtifactId(),
                VersionRange.createFromVersion(version),
                "runtime",
                type,
                classifier,
                new DefaultArtifactHandler(type));
    }
    
    /**
     * Creates an artifact instance for the supplied coordinates
     * 
     * @param groupId The groupId
     * @param artifactId The artifactId
     * @param version The version
     * @param type The type of the artifact. e.g "jar", "war" or "zip"
     * @param classifier The classifier
     * @return the created artifact
     */
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

    /**
     * Creates an artifact instance for the supplied coordinates
     * 
     * @param groupId The groupId
     * @param artifactId The artifactId
     * @param version The version
     * @param type The type of the artifact. e.g "jar", "war" or "zip"
     * @return the created artifact
     */
    public static Artifact createArtifact(
            String groupId, String artifactId, String version, String type) {

        return createArtifact(
                groupId,
                artifactId,
                version,
                type,
                null);
    }

    /**
     * Creates an artifact instance out of a dependency object.
     * 
     * @param dep
     * @return the created artifact
     */
    public static Artifact createArtifact(Dependency dep) {
        return createArtifact(
                dep.getGroupId(),
                dep.getArtifactId(),
                dep.getVersion(),
                dep.getType(),
                dep.getClassifier());
    }

    /**
     * Write the model to the buildDir/${project.build.finalName}.pom
     * @param m an instance of model
     * @param buildDir the directory in which to write the pom
     * @throws IOException
     */
    public static void writePom(Model m, File buildDir) throws IOException {
        writePom(m, buildDir, null);
    }
    
    /**
     * Write the model to the buildDir/${project.build.finalName}.pom
     * @param m an instance of model
     * @param buildDir the directory in which to write the pom
     * @param pomFileName the name of the written pom
     * @throws IOException
     */
    public static void writePom(Model m, File buildDir, String pomFileName) 
            throws IOException {
        
        if (pomFileName == null) {
            if (m.getBuild() != null && m.getBuild().getFinalName() != null) {
                pomFileName = m.getBuild().getFinalName() + ".pom";
            } else {
                pomFileName = "pom.xml";
            }
        }
        File pomFile = new File(buildDir, pomFileName);
        new DefaultModelWriter().write(pomFile, null, m);
        
        m.setPomFile(pomFile);
    }
    /**
     * Write the model to the buildDir/${project.build.finalName}.pom
     * @param m an instance of model
     * @param buildDir the directory in which to write the pom
     * @param pomFileName the name of the written pom
     * @throws IOException
     */
    public static String modelAsString(Model m) throws MojoExecutionException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try{
            new DefaultModelWriter().write(baos, null, m);
            return new String(baos.toByteArray());
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }
    
    /**
     * Write the model to the buildDir/${project.build.finalName}.pom
     * @param m an instance of model
     * @param buildDir the directory in which to write the pom
     * @param pomFileName the name of the written pom
     * @throws IOException
     */
    public static ByteArrayOutputStream writePomToOutputStream(Model m) 
            throws IOException {
        
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new DefaultModelWriter().write(baos, null, m);
        return baos;
    }

    /**
     * Installs a given model to the local repository
     * 
     * @param installer an ArtifactInstaller instance
     * @param repository an ArtifactRepository instance
     * @param p the maven project against which to install the pom
     * @param m The model to install
     * @throws MojoExecutionException
     */
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
        artifact.addMetadata(
                new ProjectArtifactMetadata(artifact, m.getPomFile()));

        try {
            installer.install(m.getPomFile(), artifact, repository);
        } catch (ArtifactInstallationException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }

    /**
     * Installs a given file as attached artifact for a given project and given classifier
     * @param installer an ArtifactInstaller instance
     * @param repository an ArtifactRepository instance
     * @param project the maven project against which to install the pom
     * @param file The file to install
     * @param type The type of the attached artifact
     * @param classifier The classifier to use
     * @throws MojoExecutionException
     */
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

    /**
     * Resolves the version of a given groupId/ArtifactId pair
     * 
     * @param module the MavenProject on which to process
     * @param groupId the groupId
     * @param artifactId the artifactId
     * @return the resolved version
     */
    public static String getManagedVersion(
            MavenProject module, String groupId, String artifactId) 
            throws MojoExecutionException {

        // 1st look at the dependencyManagement
        for (Dependency d : module.getModel().getDependencyManagement().getDependencies()) {
            if (d.getGroupId().equals(groupId)
                    && d.getArtifactId().equals(artifactId)) {
                return d.getVersion();
            }
        }
        // 2nd, look at the modules dependencies
        for (Artifact a : getDependencySet(module, null)) {
            if (a.getGroupId().equals(groupId)
                    && a.getArtifactId().equals(artifactId)) {
                return a.getVersion();
            }
        }
        // 3rd, if groupId match the module return module's version
        if (module.getGroupId().equals(groupId)) {
            return module.getModel().getVersion();
        } else {
            return "";
        }
    }

    public static String getCombinedExcludes(
            List additionalExcludes,
            boolean useDefaultExcludes,
            String[] excludes) {

        List<String> combinedExcludes = new ArrayList<String>();

        if (useDefaultExcludes) {
            combinedExcludes.addAll(FileUtils.getDefaultExcludesAsList());
        }

        if (excludes != null && excludes.length > 0) {
            combinedExcludes.addAll(Arrays.asList(excludes));
        }

        if (additionalExcludes != null && additionalExcludes.size() > 0) {
            combinedExcludes.addAll(additionalExcludes);
        }

        if (combinedExcludes.isEmpty()) {
            combinedExcludes.addAll(Arrays.asList(new String[]{}));
        }
        
        StringBuilder sb = new StringBuilder();
        for(int i=0 ; i < combinedExcludes.size(); i++){
            sb.append(combinedExcludes.get(i));
            if(i > combinedExcludes.size() -1){
                sb.append(',');
            }
        }
        return sb.toString();
    }
    public static String getCombinedExcludes(
            List additionalExcludes,
            boolean useDefaultExcludes,
            List excludes) {
        return getCombinedExcludes(
                additionalExcludes,
                useDefaultExcludes,
                (String[]) excludes.toArray(new String[excludes.size()]));
    }
    
    
    
    public static String getCombinedIncludes(
            List additionalIncludes,
            String[] includes) {
        
        List<String> combinedIncludes = new ArrayList<String>();

        if (includes != null && includes.length > 0) {
            combinedIncludes.addAll(Arrays.asList(includes));
        }

        if (additionalIncludes != null && additionalIncludes.size() > 0) {
            combinedIncludes.addAll(additionalIncludes);
        }

        // If there are no other includes, use the default.
        if (combinedIncludes.isEmpty()) {
            combinedIncludes.addAll(Arrays.asList(new String[]{"**/*"}));
        }
        
        StringBuilder sb = new StringBuilder();
        for(int i=0 ; i < combinedIncludes.size(); i++){
            sb.append(combinedIncludes.get(i));
            if(i > combinedIncludes.size() -1){
                sb.append(',');
            }
        }
        return sb.toString();
    }   
    public static String getCombinedIncludes(
            List additionalIncludes,
            List includes) {
        
        return getCombinedIncludes(
                additionalIncludes,
                (String[]) includes.toArray(new String[includes.size()]));
    }   

    /**
     * Get the Dependency set of a given project
     * 
     * @param p the project on which to execute
     * @param dependencySets a map to cache the dependency set, can be null
     * @return The dependency set
     */
    public static Set<Artifact> getDependencySet(
            MavenProject p, Map<MavenProject, Set<Artifact>> dependencySets) 
            throws MojoExecutionException {

        if (dependencySets != null
                && dependencySets.containsKey(p)) {
            return dependencySets.get(p);
        }

        Set<Artifact> artifacts = filterArtifacts(
                p.getArtifacts(),
                p.getDependencyArtifacts(),
                false,
                "runtime",
                "test",
                "jar",
                "");

        if (dependencySets != null) {
            dependencySets.put(p, artifacts);
        }
        return artifacts;
    }
    
    /**
     * Filters a set of artifacts
     * 
     * @param artifacts the set of artifacts to filter
     * @param dependencyArtifacts the set of artifact representing direct dependencies
     * @param excludeTransitive exclude transitive dependencies
     * @param includeScope the scopes to include, comma separated, can be null
     * @param excludeScope the scopes to exclude, comma separated, can be null
     * @param excludeTypes the types to include, comma separated, can be null
     * @param includeTypes the types to exclude, comma separated, can be null
     * @return the set of filtered artifacts
     * @throws MojoExecutionException
     */
    public static Set<Artifact> filterArtifacts(
            Set<Artifact> artifacts,
            Set<Artifact> dependencyArtifacts,
            boolean excludeTransitive,
            String includeScope,
            String excludeScope,
            String excludeTypes,
            String includeTypes) throws MojoExecutionException {
        
        return filterArtifacts(
                artifacts,
                dependencyArtifacts,
                excludeTransitive,
                includeScope,
                excludeScope,
                includeTypes,
                excludeTypes,
                null,
                null,
                null,
                null,
                null,
                null);
    }
    
    /**
     * Filters a set of artifacts
     * 
     * @param artifacts the set of artifacts to filter
     * @param dependencyArtifacts the set of artifact representing direct dependencies
     * @return the set of filtered artifacts* 
     * @throws MojoExecutionException
     */
    public static Set<Artifact> excludeTransitive(
            Set<Artifact> artifacts,
            Set<Artifact> dependencyArtifacts) throws MojoExecutionException{
        return filterArtifacts(
                artifacts,
                dependencyArtifacts,
                true, 
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null);        
    }
    
    /**
     * Filters a set of artifacts
     * 
     * @param artifacts the set of artifacts to filter
     * @param dependencyArtifacts the set of artifact representing direct dependencies
     * @param excludeTransitive exclude transitive dependencies
     * @param includeScope the scopes to include, comma separated, can be null
     * @param excludeScope the scopes to exclude, comma separated, can be null
     * @param excludeTypes the types to exclude, comma separated, can be null
     * @param includeTypes the types to include, comma separated, can be null
     * @param includeClassifiers the classifiers to include, comma separated, can be null
     * @param excludeClassifiers the classifiers to exclude, comma separated, can be null
     * @param includeGroupIds the groupIds to include, comma separated, can be null
     * @param excludeGroupIds the groupIds to exclude, comma separated, can be null
     * @param includeArtifactIds the artifactIds to include, comma separated, can be null
     * @param excludeArtifactIds the artifactIds to exclude, comma separated, can be null
     * @return the set of filtered artifacts* 
     * @throws MojoExecutionException
     */
    public static Set<Artifact> filterArtifacts(
            Set<Artifact> artifacts,
            Set<Artifact> dependencyArtifacts,
            boolean excludeTransitive,
            String includeScope,
            String excludeScope,
            String excludeTypes,
            String includeTypes,
            String includeClassifiers,
            String excludeClassifiers,
            String includeGroupIds,
            String excludeGroupIds,
            String includeArtifactIds,
            String excludeArtifactIds) throws MojoExecutionException {
        
        // init all params with empty string if null
        includeScope = (includeScope == null? "":includeScope);
        excludeScope = (excludeScope == null? "":excludeScope);
        includeTypes = (includeTypes == null? "":includeTypes);
        excludeTypes = (excludeTypes == null? "":excludeTypes);
        includeClassifiers = (includeClassifiers == null? "":includeClassifiers);
        excludeClassifiers = (excludeClassifiers == null? "":excludeClassifiers);
        includeGroupIds = (includeGroupIds == null? "":includeGroupIds);
        excludeGroupIds = (excludeGroupIds == null? "":excludeGroupIds);
        includeArtifactIds = (includeArtifactIds == null? "":includeArtifactIds);
        excludeArtifactIds = (excludeArtifactIds == null? "":excludeArtifactIds);

        FilterArtifacts filter = new FilterArtifacts();

        filter.addFilter(new ProjectTransitivityFilter(
                dependencyArtifacts,
                excludeTransitive));

        filter.addFilter(new ScopeFilter(
                cleanToBeTokenizedString(includeScope),
                cleanToBeTokenizedString(excludeScope)));

        filter.addFilter(new TypeFilter(
                cleanToBeTokenizedString(includeTypes),
                cleanToBeTokenizedString(excludeTypes)));

        filter.addFilter(new ClassifierFilter(
                cleanToBeTokenizedString(includeClassifiers),
                cleanToBeTokenizedString(excludeClassifiers)));

        filter.addFilter(new GroupIdFilter(
                cleanToBeTokenizedString(includeGroupIds),
                cleanToBeTokenizedString(excludeGroupIds)));

        filter.addFilter(new ArtifactIdFilter(
                cleanToBeTokenizedString(includeArtifactIds),
                cleanToBeTokenizedString(excludeArtifactIds)));

        try {
            artifacts = filter.filter(artifacts);
        } catch (ArtifactFilterException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
        return artifacts;
    }
    
    

    /**
     * Unpacks a given file
     * 
     * @param file the file to unpack
     * @param location the directory where to unpack
     * @param includes includes pattern for the files to unpack
     * @param excludes exclude pattern for the files to unpack
     * @param silent log unpack or not
     * @param log the Maven logger instance, can be null
     * @param archiverManager an instance of ArchiveManager
     * @throws MojoExecutionException
     */
    public static void unpack(
            File file,
            File location,
            String includes,
            String excludes,
            boolean silent,
            Log log,
            ArchiverManager archiverManager) throws MojoExecutionException {

        if (log != null && log.isInfoEnabled() && !silent) {
            log.info(logUnpack(file, location, includes, excludes));
        }

        location.mkdirs();

        try {
            UnArchiver unArchiver = archiverManager.getUnArchiver(file);
            unArchiver.setSourceFile(file);
            unArchiver.setDestDirectory(location);

            if (StringUtils.isNotEmpty(excludes)
                    || StringUtils.isNotEmpty(includes)) {

                IncludeExcludeFileSelector[] selectors =
                        new IncludeExcludeFileSelector[]{
                    new IncludeExcludeFileSelector()
                };

                if (StringUtils.isNotEmpty(excludes)) {
                    selectors[0].setExcludes(excludes.split(","));
                }
                if (StringUtils.isNotEmpty(includes)) {
                    selectors[0].setIncludes(includes.split(","));
                }
                unArchiver.setFileSelectors(selectors);
            }

            unArchiver.extract();
        } catch (NoSuchArchiverException e) {
            throw new MojoExecutionException("Unknown archiver type", e);
        } catch (ArchiverException e) {
            throw new MojoExecutionException(
                    "Error unpacking file: " + file + " to: " + location + "\r\n"
                    + e.toString(), e);
        }
    }

    private static String logUnpack(
            File file,
            File location,
            String includes,
            String excludes) {

        StringBuilder msg = new StringBuilder();
        msg.append("Unpacking ");
        msg.append(file);
        msg.append(" to ");
        msg.append(location);

        if (includes != null && excludes != null) {
            msg.append(" with includes \"");
            msg.append(includes);
            msg.append("\" and excludes \"");
            msg.append(excludes);
            msg.append("\"");
        } else if (includes != null) {
            msg.append(" with includes \"");
            msg.append(includes);
            msg.append("\"");
        } else if (excludes != null) {
            msg.append(" with excludes \"");
            msg.append(excludes);
            msg.append("\"");
        }

        return msg.toString();
    }

    /**
     * Resolves an artifact
     * 
     * @param requestArtifact the requested artifact
     * @param repoSystem an instance of RepositorySystem
     * @param repoSession an instance of RepositorySystemSession
     * @param remoteRepos a list of RemoteRepository
     * @return an instance of ArtifactResult
     * @throws MojoExecutionException
     */
    public static ArtifactResult resolveArtifact(
            org.sonatype.aether.util.artifact.DefaultArtifact requestArtifact,
            RepositorySystem repoSystem,
            RepositorySystemSession repoSession,
            List<RemoteRepository> remoteRepos) throws MojoExecutionException {

        ArtifactRequest request = new ArtifactRequest();
        request.setArtifact(requestArtifact);
        request.setRepositories(remoteRepos);

        ArtifactResult result;
        try {
            result = repoSystem.resolveArtifact(repoSession, request);
        } catch (ArtifactResolutionException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
        return result;
    }
    
    public static ArtifactResult resolveArtifact(
            String groupId,
            String artifactId,
            String classifier,
            String type,
            String version,
            RepositorySystem repoSystem,
            RepositorySystemSession repoSession,
            List<RemoteRepository> remoteRepos) throws MojoExecutionException {

        return resolveArtifact(
                new org.sonatype.aether.util.artifact.DefaultArtifact(
                groupId,
                artifactId,
                classifier,
                type,
                version),
                repoSystem,
                repoSession,
                remoteRepos);
    }

    /**
     * Clean the pattern string for future regexp usage
     * @param str the string to cleanup
     * @return the cleaned string
     */
    public static String cleanToBeTokenizedString(String str) {
        String ret = "";
        if (!StringUtils.isEmpty(str)) {
            ret = str.trim().replaceAll("[\\s]*,[\\s]*", ",");
        }
        return ret;
    }
    
    public static ModifiedPomXMLEventReader newModifiedPomXER(StringBuilder input) 
            throws XMLStreamException {
        
        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty(XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE);
        return new ModifiedPomXMLEventReader(input, inputFactory);
    }
    
    public static void writeFile(File outFile, StringBuilder input) 
            throws IOException {
        
        Writer writer = WriterFactory.newXmlWriter(outFile);
        try {
            IOUtil.copy(input.toString(), writer);
        } finally {
            IOUtil.close(writer);
        }
    }
    
    public static List<String> getCommaSeparatedList(String list){
        if (list != null) {
            String[] listArray = list.split(",");
            if (listArray != null) {
                return Arrays.asList(listArray);
            }
        }
        return Collections.EMPTY_LIST;
    }
    
    private static String listToString(List<String> list) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < list.size(); i++) {
            sb.append(list.get(i));
            if (i < list.size() - 1) {
                sb.append(',');
            }
        }
        return sb.toString();
    }
    
    public static ZipFileSet createZipFileSet(
            File dir,
            List<String> includes,
            List<String> excludes) {
        
        return createZipFileSet(
                dir,
                listToString(includes),
                listToString(excludes));
    }
    
    public static ZipFileSet createZipFileSet(
            File dir,
            String prefix,
            List<String> includes,
            List<String> excludes) {
        
        ZipFileSet fset = createZipFileSet(dir, includes, excludes);
        if (prefix != null && !prefix.isEmpty()) {
            fset.setPrefix(prefix);
        }
        return fset;
    }
    
    public static ZipFileSet createZipFileSet(
            File dir,
            String includes,
            String excludes) {
        ZipFileSet fset = new ZipFileSet();
        fset.setDir(dir);
        fset.setIncludes(includes);
        fset.setExcludes(excludes);
        fset.setDescription(
                String.format(
                "file set: %s ( excludes: [ %s ], includes: [ %s ])",
                dir.getAbsolutePath(),
                excludes,
                includes));
        return fset;
    }
    
    public static ZipFileSet createZipFileSet(
            File dir,
            String prefix,
            String includes
            , String excludes) {
        ZipFileSet fset = createZipFileSet(dir, includes, excludes);
        if (prefix != null && !prefix.isEmpty()) {
            fset.setPrefix(prefix);
        }
        return fset;
    }
    
    public static void createZip(
            Properties props,
            Log log,
            String duplicate,
            List<ZipFileSet> fsets,
            File target) {

        ZipUtil zipUtil = new ZipUtil();
        zipUtil.zip(props, log, duplicate, fsets, target);
    }
    
    public static void createZip(
            Properties props,
            Log log,
            String duplicate,
            ZipFileSet fset,
            File target) {

        List<ZipFileSet> fsets = new ArrayList<ZipFileSet>();
        fsets.add(fset);
        createZip(props, log, duplicate, fsets, target);
    }

    private static class ZipUtil implements BuildListener {

        private org.apache.tools.ant.taskdefs.Zip zip;
        private Project antProject;
        private Log log;

        private ZipUtil() {
            antProject = new Project();
            antProject.addBuildListener((BuildListener) this);
            zip = new org.apache.tools.ant.taskdefs.Zip();
        }

        private void zip(
                Properties properties,
                Log log,
                String duplicate,
                List<ZipFileSet> fsets,
                File target) {

            this.log = log;
            Iterator it = properties.keySet().iterator();
            while (it.hasNext()) {
                String key = (String) it.next();
                antProject.setProperty(key, properties.getProperty(key));
            }
            zip.setProject(antProject);
            zip.setDestFile(target);
            org.apache.tools.ant.taskdefs.Zip.Duplicate df =
                    new org.apache.tools.ant.taskdefs.Zip.Duplicate();
            df.setValue(duplicate);
            zip.setDuplicate(df);
            log.info(String.format("[zip] duplicate: %s", duplicate));

            if (fsets == null){
                fsets = new ArrayList<ZipFileSet>();
            }
            if(fsets.isEmpty()) {
                ZipFileSet zfs = MavenUtils.createZipFileSet(new File(""), "","");
                // work around for 
                // http://issues.apache.org/bugzilla/show_bug.cgi?id=42122
                zfs.setDirMode("755");
                zfs.setFileMode("644");
                fsets.add(zfs);
            }
            
            for(ZipFileSet fset:fsets){
                zip.addZipfileset(fset);
                log.info(String.format("[zip] %s", fset.getDescription()));
            }
            zip.executeMain();
        }

        public void buildStarted(BuildEvent event) {
        }

        public void buildFinished(BuildEvent event) {
        }

        public void targetStarted(BuildEvent event) {
        }

        public void targetFinished(BuildEvent event) {
        }

        public void taskStarted(BuildEvent event) {
        }

        public void taskFinished(BuildEvent event) {
        }

        public void messageLogged(BuildEvent event) {
            if (event.getPriority() < 3) {
                log.info(String.format("[zip] %s", event.getMessage()));
            } else {
                log.debug(String.format("[zip] %s", event.getMessage()));
            }
        }
    }
}
