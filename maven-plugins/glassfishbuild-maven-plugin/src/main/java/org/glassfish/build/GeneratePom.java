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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.List;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Developer;
import org.apache.maven.model.IssueManagement;
import org.apache.maven.model.License;
import org.apache.maven.model.MailingList;
import org.apache.maven.model.Model;
import org.apache.maven.model.Organization;
import org.apache.maven.model.Parent;
import org.apache.maven.model.Scm;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.glassfish.build.utils.MavenUtils;

/**
 * Generates a pom from another pom
 *
 * @goal generate-pom
 *
 * @author Romain Grecourt
 */
public class GeneratePom extends AbstractMojo {
    /**
     * @parameter default-value="${project}"
     * @required
     * @readonly
     */
    protected MavenProject project;
 
    /**
     * @parameter expression="${generate.pom.outputDirectory}" default-value="${project.build.directory}"
     */
    protected File outputDirectory;
    
    /**
     * @parameter expression="${generate.pom.pomFile}" default-value="${project.file}"
     */
    protected File pomFile;
    
    /**
     * @parameter expression="${generate.pom.groupId}" default-value="${project.groupId}"
     * @required
     */
    protected String groupId;    
    
    /**
     * @parameter expression="${generate.pom.artifactId}" default-value="${project.artifactId}"
     */
    protected String artifactId;
    
    /**
     * @parameter expression="${generate.pom.version}" default-value="${project.version}"
     */
    protected String version;
    
    /**
     * @parameter expression="${generate.pom.parent}"
     */
    protected Parent parent;
    
    /**
     * @parameter expression="${generate.pom.description}" default-value="${project.description}"
     */
    protected String description;
    
    /**
     * @parameter expression="${generate.pom.name}" default-value="${project.name}"
     */
    protected String name;
    
    /**
     * @parameter expression="${generate.pom.scm}" default-value="${project.scm}"
     */
    protected Scm scm;
    
    /**
     * @parameter expression="${generate.pom.issueManagement}" default-value="${project.issueManagement}"
     */
    protected IssueManagement issueManagement;
    
    /**
     * @parameter expression="${generate.pom.mailingLists}" default-value="${project.mailingLists}"
     */
    protected List<MailingList> mailingLists;
    
    /**
     *
     * @parameter expression="${generate.pom.developers}" default-value="${project.developers}"
     */
    protected List<Developer> devevelopers;
    
    /**
     *
     * @parameter expression="${generate.pom.licenses}" default-value="${project.licenses}"
     */
    protected List<License> licenses;
    
    /**
     *
     * @parameter expression="${generate.pom.organization}" default-value="${project.organization}"
     */
    protected Organization organization;
    
    /**
     *
     * @parameter expression="${generate.pom.excludeDependencies}"
     */
    protected String excludeDependencies;
    
    /**
     *
     * @parameter expression="${generate.pom.excludeDependencyScope}" default-value="system,test"
     */
    protected String excludeDependencyScopes;
    
    /**
     *
     * @parameter expression="${generate.pom.dependencies}" default-value="${project.dependencies}"
     */
    protected List<Dependency> dependencies;
    
    /**
     *
     * @parameter expression="${generate.pom.skip}" default-value="false"
     */
    protected Boolean skip;
    
    /**
     *
     * @parameter expression="${generate.pom.attach}" default-value="false"
     */
    protected Boolean attach;

    public void execute() throws MojoExecutionException, MojoFailureException {
        if(skip.booleanValue()){
            getLog().info("skipping...");
            return;
        }
        String input;
        try {
             input = PomHelper.readXmlFile(pomFile).toString();
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(),ex);
        }
        Model model = MavenUtils.readModel(input);
        
        model.setGroupId(groupId);
        model.setArtifactId(artifactId);
        model.setVersion(version);
        model.setDevelopers(devevelopers);
        model.setParent(parent);
        model.setName(name);
        model.setDescription(description);
        model.setScm(scm);
        model.setIssueManagement(issueManagement);
        model.setMailingLists(mailingLists);
        model.setLicenses(licenses);
        model.setOrganization(organization);
        model.setBuild(new Build());
        
        if (excludeDependencies != null) {
            
            String[] artifactIdExclusions = excludeDependencies.split(",");
            String[] scopeExclusions = excludeDependencyScopes.split(",");
            if (artifactIdExclusions != null 
                    && scopeExclusions != null) {
                
                List<String> ArtifactIdExclusionsList = Arrays.asList(artifactIdExclusions);
                List<String> scopeExclusionsList = Arrays.asList(scopeExclusions);
                
                for (Object d : dependencies.toArray()) {
                    if (ArtifactIdExclusionsList.contains(((Dependency)d).getArtifactId())
                            || scopeExclusionsList.contains(((Dependency)d).getScope())) {
                        dependencies.remove((Dependency)d);
                    }
                }
            }
            model.setDependencies(dependencies);
        }
        
        File newPomFile = new File(outputDirectory,"pom.xml");
        newPomFile.getParentFile().mkdirs();
        
        FileWriter fw = null;
        try {
            // write comments from base pom
            fw = new FileWriter(newPomFile);
            String line;
            BufferedReader br = new BufferedReader(new StringReader(input));
            while((line = br.readLine()) !=null && !line.startsWith("<project")){
                fw.write(line);
                fw.write('\n');
            }
            
            // write new pom and skip first line (xml header)
            String pom = MavenUtils.writePomToOutputStream(model).toString();
            int ind = pom.indexOf('\n');
            fw.write(pom.substring(ind));
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(),ex);
        } finally {
            try {
                if (fw != null) {
                    fw.close();
                }
            } catch (Exception ex) {
            }
        }
        
        if(attach){
            project.setFile(newPomFile);
        }
    }
}