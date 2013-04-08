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
package org.glassfish.spec.maven;

import java.io.File;
import java.io.IOException;
import java.util.jar.JarFile;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.glassfish.spec.Artifact;
import org.glassfish.spec.Metadata;
import org.glassfish.spec.Spec;


/**
 *
 * @goal check-module
 * @phase package
 *
 * @author Romain Grecourt
 */
public class CheckModuleMojo extends AbstractMojo {
    /**
     * @parameter default-value="${project}"
     * @required
     * @readonly
     */
    protected MavenProject project;
    
    /**
     * Module to verify
     * 
     * @parameter expression="${module}"
     */
    protected File module;
    
    /**
     * Spec
     * 
     * @parameter expression="${spec}"
     */    
    protected Spec spec;
    
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if(module ==null || !module.exists()){
            module = project.getArtifact().getFile();
            if(module == null || !module.exists()){
                throw new MojoFailureException("no jar to verify");
            }
        }
        try {
            if (spec == null) {
                spec = new Spec();
            }
            spec.setArtifact(new Artifact(
                    project.getGroupId(),
                    project.getArtifactId(),
                    project.getVersion()));
            spec.setMetadata(Metadata.fromJar(new JarFile(module)));
            spec.verify();

            if (!spec.getErrors().isEmpty()) {
                System.out.println("");
                System.out.println(spec.getArtifact().toString());
                String specDesc = spec.toString();
                if (!specDesc.isEmpty()) {
                    System.out.println(spec.toString());
                }
                for (int i = 0; i < spec.getErrors().size(); i++) {
                    System.out.println(new StringBuilder()
                            .append('-')
                            .append(' ')
                            .append(spec.getErrors().get(i))
                            .toString());
                }
                System.out.println("");
                throw new MojoFailureException("spec verification failed.");
            }
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }
}