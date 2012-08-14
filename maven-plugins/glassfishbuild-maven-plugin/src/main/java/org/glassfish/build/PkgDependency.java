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
import java.io.IOException;
import java.util.*;
import javax.xml.bind.JAXBException;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.installer.ArtifactInstaller;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.glassfish.build.descriptors.Packages;
import org.glassfish.build.descriptors.Packages.DuplicatedArtifactIdEx;
import org.glassfish.build.descriptors.Packages.Package;
import org.glassfish.build.utils.DependencyWrapper;
import org.glassfish.build.utils.MavenUtils;

/**
 *
 * TODO: override default lifecycle using dist-fragment
 * TODO: check every non generated dependency is optional, or force marking as optional
 *
 * @goal pkgdependency
 * @phase install
 * @requiresDependencyResolution runtime
 *
 * @author Romain Grecourt
 */
public class PkgDependency extends AbstractAggregatorMojo {
    
    /**
     * @component
     * @readonly
     */
    private ArtifactResolver artifactResolver;    

    /**
     * @component
     */
    protected ArtifactInstaller installer;
    
    /**
     * @parameter expression="${localRepository}"
     * @required
     * @readonly
     */
    protected ArtifactRepository localRepository;  
    
    /**
     * The file name of the created XML descriptor.
     *
     * @parameter expression="${descriptorName}" default-value="packages-descriptor"
     */
    protected String descriptorName;
    
    /**
     * Fail for non-resolved dependencies
     *
     * @parameter expression="${strictResolution}" default-value="true"
     */
    protected Boolean strictResolution;
    
    /**
     * Process hidden dependencies ?
     *
     * @parameter expression="${processHidden}" default-value="true"
     */
    protected Boolean processHidden;
    
    /**
     * Process hidden dependencies ?
     *
     * @parameter expression="${skipPkgDependency}" default-value="false"
     */
    protected Boolean skipPkgDependency;
    
    /**
     * Process hidden dependencies ?
     *
     * @parameter expression="${failForDuplicates}" default-value="true"
     */
    protected Boolean failForDuplicates;
    
    /**
     *  hiddenDependencies that are resolved as part of the package
     *
     * @parameter expression="${hiddenDependencies}"
     */
    protected List<Dependency> hiddenDependencies;
    
    /**
     *  GroupIds to exclude from resolution
     *
     * @parameter expression="${excludeGroupIds}"
     */
    protected List<String> excludeGroupIds;
    
    /**
     *  ArtifactsIds to exclude from resolution
     *
     * @parameter expression="${excludeArtifactIds}"
     */
    protected List<String> excludeArtifactIds;   
    
    /**
     * List of directories separated by comma where to search for jars
     *
     * @parameter expression="${staggingDir}" default-value="stage"
     */
    protected String staggingDir;
    
    /**
     * List of packaging types separated by comma
     *
     * @parameter expression="${packagingTypes}" default-value="distribution-fragment"
     */
    protected String packagingTypes;
    
    protected static final String DESCRIPTOR_CLASSIFIER = "packages-descriptor";
    protected static final String DESC_TYPE = "xml";
    protected static final String GF_PLUGIN_KEY = "org.glassfish.build:glassfishbuild-maven-plugin";
    protected File descriptorFile = null;
    
    @Override
    protected void executeAggregate() throws MojoExecutionException {
        if(skipPkgDependency.booleanValue())
            return;
        
        HashMap<MavenProject,Set<Artifact>> allDeps = new HashMap<MavenProject,Set<Artifact>>();
        List<String> packagingTypesList = Arrays.asList(packagingTypes.split(","));
        File buildDirectory = new File(parent.getBuild().getDirectory());
        buildDirectory.mkdir();
        descriptorFile = new File(buildDirectory, descriptorName+"."+DESC_TYPE);
        
        // write the descriptor for all packages in the reactor
        Packages descriptor = null;
        try {
            descriptor = createDescriptor(
                    parent,
                    modules,
                    descriptorFile,
                    allDeps,
                    staggingDir,
                    packagingTypesList,
                    processHidden);
        } catch (DuplicatedArtifactIdEx ex) {
            if(failForDuplicates)
                throw new MojoExecutionException(ex.getMessage());
            else
                getLog().warn(ex.getMessage());
        }
        
        // read provided descriptor if exist
        List<Packages> providedDescriptors = readDescriptors(parent, localRepository);
        providedDescriptors.add(descriptor);

        // process packages
        Iterator<MavenProject> it = this.modules.iterator();
        while (it.hasNext()) {
            MavenProject module = it.next();
            
            if(packagingTypesList.isEmpty() 
                    || packagingTypesList.contains(module.getPackaging())){
                Model originalModel = module.getOriginalModel();
                HashSet<DependencyWrapper> generatedDeps = new HashSet<DependencyWrapper>();

                // process dependencies
                Iterator<Artifact> deps = MavenUtils.getDependencySet(module, allDeps).iterator();
                while (deps.hasNext()) {
                    Artifact artifact = deps.next();

                    // if the current dependency is not excluded
                    if (excludeGroupIds != null && !excludeGroupIds.contains(artifact.getGroupId())) {
                        if (excludeArtifactIds != null && !excludeArtifactIds.contains(artifact.getArtifactId())) {

                            // resolve the dependency: find the package that resolves it.
                            Dependency dep =  resolveDependency(providedDescriptors, artifact);

                            // add the dependency to the pom
                            if (dep != null) {
                                // don't add dependency to ourself
                                if ((!dep.getGroupId().equals(module.getGroupId())
                                        || !dep.getArtifactId().equals(module.getArtifactId())
                                        || !dep.getVersion().equals(module.getVersion()))) {

                                    getLog().debug("Creating new dependency(" + dep + ") for (" + module.getName() + ")");
                                    generatedDeps.add(new DependencyWrapper(dep));
                                }
                            } 
                            // unresolved dependency!
                            else {
                                String msg = "No package found to resolve: "
                                        + artifact
                                        + " in "
                                        + module.getArtifactId();
                                if (strictResolution.booleanValue()) {
                                    throw new MojoExecutionException(msg);
                                } else {
                                    getLog().warn(msg);
                                }
                            }
                        }
                    }
                }

                // write and install new pom if needed
                if (!generatedDeps.isEmpty()) {

                    getLog().info("");
                    getLog().info("Post processing - " + module.getName());

                    // add generated dependencies
                    for(DependencyWrapper d : generatedDeps){
                        originalModel.getDependencies().add(d.getDependency());
                    }
                    // write the new pom
                    try {
                        MavenUtils.writePom(originalModel,
                                new File(module.getBuild().getDirectory()));
                    } catch (IOException ex) {
                        getLog().error(ex);
                    }
                    // install the new pom
                    MavenUtils.installPom(installer, localRepository, module, originalModel);
                }                
            }
        }
        
        // write the packages descriptor to an XML file
        try {
            descriptor.writeXML(descriptorFile);
        } catch (JAXBException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
        
        // attach the descriptor
        MavenUtils.installAttached(
                installer,
                localRepository,
                parent,
                descriptorFile,
                DESC_TYPE,
                DESCRIPTOR_CLASSIFIER);
    }
    
    protected Dependency resolveDependency(List<Packages> packagesList, Artifact art) {
        if (!art.getScope().equals(Artifact.SCOPE_TEST)) {
            Package pkg;
            // some external artifacts still depends on old glassfish groupIds...
            // hence we just search for artifactId
            if((pkg = Packages.getPackageWithDependency(
                    packagesList,
                    null,
                    art.getArtifactId(),
                    null)) != null) {
                Dependency dep = new Dependency();
                dep.setGroupId(pkg.getGroupId());
                dep.setArtifactId(pkg.getArtifactId());
                dep.setType(Packages.DEFAULT_TYPE);
                dep.setVersion(pkg.getVersion());
                return dep;
            }
        }
        return null;
    }

    protected List<Packages> readDescriptors(
            MavenProject parent, ArtifactRepository repository)
            throws MojoExecutionException {

        List<Packages> parentDescriptors = new ArrayList<Packages>();

        Iterator<Dependency> itD = parent.getDependencies().iterator();
        while (itD.hasNext()) {
            Dependency dep = itD.next();
            if (dep.getClassifier() != null
                    && dep.getClassifier().equals(DESCRIPTOR_CLASSIFIER)) {
                try {
                    Artifact artifact = MavenUtils.createArtifact(dep);
                    artifactResolver.resolve(
                            artifact,
                            parent.getRemoteArtifactRepositories(),
                            repository);
                    parentDescriptors.add(Packages.readXML(artifact.getFile()));
                } catch (ArtifactResolutionException ex) {
                    throw new MojoExecutionException(ex.getMessage(), ex);
                } catch (ArtifactNotFoundException ex) {
                    throw new MojoExecutionException(ex.getMessage(), ex);
                } catch (JAXBException ex) {
                    throw new MojoExecutionException(ex.getMessage(), ex);
                }
            }
        }
        return parentDescriptors;
    }

    protected static Packages createDescriptor(
            MavenProject parent, 
            List<MavenProject> modules,
            File desc, Map<MavenProject, Set<Artifact>> allDeps,
            String dirsString,
            List<String> includePackagingTypes,
            boolean processHidden)
            throws MojoExecutionException, DuplicatedArtifactIdEx {

        Packages packages = new Packages(parent);
        Iterator<MavenProject> modulesIt = modules.iterator();
        
        while (modulesIt.hasNext()) {

            MavenProject module = modulesIt.next();
            
            // run only for given packaging type
            if(includePackagingTypes.isEmpty() 
                    || includePackagingTypes.contains(module.getPackaging())){
                
                List<String> packageContent = getPackageContent(module,dirsString);
                Set<Artifact> deps = MavenUtils.getDependencySet(module, allDeps);

                Package curPackage = new Package(
                            module.getGroupId(),
                            module.getArtifactId(),
                            module.getVersion());

                if (processHidden)
                    curPackage = processHiddenDependencies(module,curPackage);

                // describe what was filtered by dependency plugin
                Iterator<Artifact> it = deps.iterator();
                while(it.hasNext()){
                    Artifact dep = it.next();

                    // describe only what is trully packaged
                    if(hasItemEndingWith(packageContent,
                            dep.getArtifactId().concat(".").concat(dep.getType()))){

                        curPackage.addPackageItem(new Package.PackageItem(
                                dep.getGroupId(),
                                dep.getArtifactId(),
                                dep.getBaseVersion()));
                    }
                }
                packages.addPackage(curPackage);                
            }
        }
        
        return packages;
    }    
    
    protected static Package processHiddenDependencies(MavenProject module, Package pkg) 
            throws MojoExecutionException, DuplicatedArtifactIdEx {
        
        if(pkg != null){
            Xpp3Dom dom = (Xpp3Dom) module.getPlugin(GF_PLUGIN_KEY).getConfiguration();
            Xpp3Dom hiddenDeps;
            if (dom != null 
                    && (hiddenDeps = dom.getChild("hiddenDependencies")) != null) {
                
                for (Xpp3Dom hiddenDep : hiddenDeps.getChildren()) {
                    
                    Xpp3Dom groupId, artifactId;
                    if ((groupId = hiddenDep.getChild("groupId")) != null
                            && (artifactId = hiddenDep.getChild("artifactId")) != null) {
                        
                        if (groupId.getValue() == null
                                || artifactId.getValue() == null) {
                            throw new MojoExecutionException("dom value missing!");
                        }
                        
                        String version;
                        if((hiddenDep.getChild("version")) != null){
                            version = hiddenDep.getChild("version").getValue();
                        } else {
                            version = MavenUtils.getManagedVersion(
                                    module,
                                    groupId.getValue(),
                                    artifactId.getValue());
                        }
                        
                        pkg.addPackageItem(new Package.PackageItem(
                                groupId.getValue(),
                                artifactId.getValue(),
                                version));
                    }
                }
            }
        }
        return pkg;
    }

    protected static List<String> getPackageContent(MavenProject module, String staggingDir)
            throws MojoExecutionException {
        File basedir = new File(module.getBuild().getDirectory());
        List<String> fileNames = new ArrayList<String>();

        // list every files
        File dir = new File(basedir, staggingDir);
        if (dir.exists()) {
            try {
                fileNames.addAll(FileUtils.getFileNames(dir, "**/*", "", true));
            } catch (IOException ex) {
                throw new MojoExecutionException(ex.getMessage(), ex);
            }
        }
        return fileNames;
    }

    public static boolean hasItemEndingWith(List<String> list, String str)
            throws MojoExecutionException {
        for (String s : list) {
            if(s.endsWith(str)){
                return true;
            }
        }
        return false;
    }
}
