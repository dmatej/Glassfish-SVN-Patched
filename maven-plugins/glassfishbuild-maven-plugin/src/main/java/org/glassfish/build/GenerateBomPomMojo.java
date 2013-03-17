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
import java.io.IOException;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.glassfish.build.utils.MavenUtils;

/**
 * Generates a bom pom from the reactor
 *
 * @goal generate-bom-pom
 *
 * @author Romain Grecourt
 */
public class GenerateBomPomMojo extends AbstractAggregatorMojo {
 
    /**
     * @parameter expression="${generate.bom.pom.outputFile}" default-value="${project.build.directory}/bom-pom.xml"
     */
    protected File outputFile;
    
    /**
     * @parameter expression="${generate.bom.pom.groupId}" default-value="${project.groupId}"
     * @required
     */
    protected String groupId;    
    
    /**
     * @parameter expression="${generate.bom.pom.artifactId}" default-value="bom-pom"
     */
    protected String artifactId;
    
    /**
     * @parameter expression="${generate.bom.pom.version}" default-value="${project.version}"
     */
    protected String version;
    
    /**
     * @parameter expression="${generate.bom.pom.parentPom}"
     */
    protected Parent parentPom;
    
    /**
     * @parameter expression="${generate.pom.description}"
     */
    protected String description;
    
    /**
     *
     * @parameter expression="${generate.bom.pom.skip}" default-value="false"
     */
    protected Boolean skip;

    @Override
    protected void executeAggregate() throws MojoExecutionException {
        if(skip.booleanValue()){
            getLog().info("skipping...");
            return;
        }
        
        Model model = new Model();
        if(parentPom != null){
            model.setParent(parentPom);
        }
        model.setGroupId(groupId);
        model.setArtifactId(artifactId);
        model.setVersion(version);
        if(description != null && !description.isEmpty()){
            model.setDescription(description);
        }
        
        DependencyManagement dependencyMgmt = new DependencyManagement();
        for(MavenProject p : reactorProjects){
            Dependency d = new Dependency();
            d.setGroupId(p.getGroupId());
            d.setArtifactId(p.getArtifactId());
            d.setVersion(p.getVersion());
            if(p.getArtifact().getFile().exists()){
                String name = p.getArtifact().getFile().getName();
                d.setType(name.substring(name.lastIndexOf(".")));
            }
            dependencyMgmt.addDependency(d);
            for(Artifact a : p.getAttachedArtifacts()){
                if(a.getFile().exists()){
                    Dependency attached = d.clone();
                    attached.setClassifier(a.getClassifier());
                    String name = a.getFile().getName();
                    attached.setType(name.substring(name.lastIndexOf(".")));
                    dependencyMgmt.addDependency(attached);
                }
            }
        }
        
        try {
            MavenUtils.writePom(model, outputFile);
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
        
        // TODO - should have a deployment option as this is a *new* artifact
    }
}