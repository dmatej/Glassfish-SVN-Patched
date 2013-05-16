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

import edu.emory.mathcs.backport.java.util.Arrays;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.maven.model.Resource;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.FileUtils;

/**
 * TODO provide some exclusions for compileSourceRoot and resources
 *
 * @goal bootstrap-source-build
 * @phase initialize
 * @requiresProject
 *
 * @author Romain Grecourt
 */
public class BootstrapSourceBuildMojo extends AbstractMojo {

    /**
     * The maven project.
     *
     * @parameter expression="${project}"
     * @required
     * @readonly
     */
    protected MavenProject project;
    
    /**
     * @parameter 
     * expression="${gfbuild.bootstrapsourcebuild.skip}"
     * default-value="false"
     */    
    private boolean skip;
    
    /**
     * @parameter 
     * expression="${gfbuild.bootstrapsourcebuild.resourcesExcludes}"
     * default-value="target/**, pom.xml, **\/*.java"
     */
    private String resourcesExcludes;
    
    /**
     * @parameter 
     * expression="${gfbuild.bootstrapsourcebuild.sourcesExcludes}"
     * default-value=""
     */
    private String sourcesExcludes;
    
    
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if (skip) {
            getLog().info("Skipping bootstrap-source-build");
            return;
        }
        
        if(project.getPackaging().equals("pom")){
            getLog().info("Skipping bootstrap-source-build, for packaging POM");
            return;
        }
        
        // sourceExcludes is actually an include pattern for deleting files...
        try {
            List<File> toDelete = 
                    FileUtils.getFiles(project.getBasedir(), sourcesExcludes, "");
            for(File f : toDelete){
                if (f.exists()) {
                    if (f.isDirectory()) {
                        FileUtils.deleteDirectory(f);
                    } else {
                        f.delete();
                    }
                }
            }
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
        
        Resource resource = new Resource();
        resource.setDirectory(project.getBasedir().getAbsolutePath());
        resource.setTargetPath(project.getBuild().getOutputDirectory());
        resource.setExcludes(Arrays.asList(resourcesExcludes.split(",")));
        try {
            List<File> resourceFiles =
                    FileUtils.getFiles(project.getBasedir(), "**", resourcesExcludes);
            for(File f : resourceFiles){
                resource.addInclude(f.getAbsolutePath().replace(
                        project.getBasedir()+"/"
                        , ""));
            }
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
        getLog().info("adding resource: "+resource);
        project.addResource(resource);
        
        getLog().info("addding compile sourceRoot: "+project.getBasedir().getAbsolutePath());
        project.addCompileSourceRoot(project.getBasedir().getAbsolutePath());
        
        getLog().info("removing jars under "+project.getBuild().getDirectory());
        File target = new File(project.getBuild().getDirectory());
        if (target.exists()) {
            try {
                List<File> resourceFiles =
                        FileUtils.getFiles(
                        target,
                        "**/*.jar, **/*.class, **/*.java",
                        "");
                for (File f : resourceFiles) {
                    f.delete();
                }
            } catch (IOException ex) {
                throw new MojoExecutionException(ex.getMessage(), ex);
            }
        }
    }
}
