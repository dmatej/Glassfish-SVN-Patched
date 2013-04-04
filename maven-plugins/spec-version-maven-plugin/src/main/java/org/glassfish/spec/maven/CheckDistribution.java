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
import java.util.Collections;
import java.util.List;
import java.util.jar.JarFile;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.plexus.util.FileUtils;
import org.glassfish.spec.Artifact;
import org.glassfish.spec.Metadata;
import org.glassfish.spec.Spec;


/**
 *
 * @goal check-distribution
 *
 * @author Romain Grecourt
 */
public class CheckDistribution extends AbstractMojo {
    
    /**
     * @parameter expression="${ignoreFailures}" default-value="false"
     */
    protected boolean ignoreFailures;     
    
    /**
     * include pattern
     * 
     * @parameter expression="${includes}" default-value="javax*.jar"
     */
    protected String includes;
    
    /**
     * exclude pattern
     * 
     * @parameter expression="${excludes}"
     */
    protected String excludes;
    
    /**
     * include pattern for inclusion
     * 
     * @required
     * @parameter expression="${dir}"
     */
    protected File dir;
    
    /**
     * Specs
     * 
     * @parameter expression="${specs}"
     */    
    protected List<Spec> specs;
    
    
    private Spec getSpec(Artifact a) throws IOException{
        for(Spec s : specs){
            if(s.getArtifact().equals(a)){
                return s;
            }
        }
        return null;
    }
    
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        try {
            List<File> jars = Collections.EMPTY_LIST;
            try {
                jars = FileUtils.getFiles(dir, includes, excludes);
            } catch (IOException ex) {
                throw new MojoExecutionException(ex.getMessage(), ex);
            }

            for (File jar : jars) {
                JarFile jarFile = new JarFile(jar);
                Artifact artifact = Artifact.fromJar(jarFile);
                Spec spec = getSpec(artifact);

                List<String> errors;
                if (spec == null) {
                    System.out.println("WARNING: no spec information found for " + jar.getName());
                    Metadata m = Metadata.fromJar(jarFile);
                    errors = m.getErrors();
                } else {
                    spec.read(jarFile);
                    spec.verify();
                    errors = spec.getErrors();
                }

                if (!errors.isEmpty()) {
                    System.out.println("");
                    System.out.println("[ " + jar.getName() + " ] "
                            + artifact.toString());
                    for (int i = 0; i < errors.size(); i++) {
                        System.out.println("-" + String.valueOf(i)
                                + " " + errors.get(i));
                    }
                    System.out.println("");
                }
            }
        } catch (IOException ex) {
            getLog().warn(ex.getMessage(), ex);
        }
    }
}