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

import java.util.Properties;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.glassfish.spec.SpecVersion;
import org.glassfish.spec.SpecVersion.MavenVersion;
import org.glassfish.spec.SpecVersion.SpecException;


/**
 *
 * @author Romain Grecourt
 */
public abstract class AbstractSpecMojo extends AbstractMojo {

    /**
     * @parameter default-value="${project}"
     * @required
     * @readonly
     */
    protected MavenProject project;
    
    /**
     * @parameter expression="${ignoreFailures}" default-value="false"
     */
    protected boolean ignoreFailures; 
    
    /**
     * non-final specification
     * 
     * @parameter expression="${nonfinal}" default-value="true"
     */
    protected boolean nonfinal;
    
    /**
     * non-final specification
     * 
     * @parameter expression="${api}" default-value="true"
     */
    protected boolean api;
    
    /**
     * non-final specification
     * 
     * @parameter expression="${impl}" default-value="false"
     */
    protected boolean impl;    
    
    /**
     * API has a standalone implementation
     * 
     * @parameter expression="${standalone}" default-value="false"
     */
    protected boolean standalone;
    
    /**
     * API package
     * 
     * @parameter expression="${apipackage}"
     */
    protected String apipackage;
    
    /**
     * implementation package
     * 
     * @parameter expression="${apipackage}"
     */
    protected String implpackage;
    
    /**
     * version number of the JCP specification
     * 
     * @required
     * @parameter expression="${specversion}"
     */
    protected String specversion;
    
    /**
     * version number of the API classes
     * 
     * @parameter expression="${specimplversion}"
     */
    protected String specimplversion;
    
    /**
     * version number of the implementation
     * 
     * @parameter expression="${implversion}"
     */
    protected String implversion;
    
    /**
     * version number of the spec under development
     * 
     * @parameter expression="${newspecversion}"
     */
    protected String newspecversion;
    
    /**
     * build number of spec API jar file
     * 
     * @parameter expression="${specbuild}"
     */
    protected String specbuild;
    
    /**
     * version number of the implementation when final
     * 
     * @parameter expression="${newimplversion}"
     */
    protected String newimplversion;
    
    /**
     * build number of implementation jar file
     * 
     * @parameter expression="${implbuild}"
     */
    protected String implbuild;
    
    protected static MavenVersion mavenVersion = null;
    
    protected MavenVersion getMavenVersion() {
        if (mavenVersion == null) {
            ArtifactVersion v = new DefaultArtifactVersion(project.getVersion());
            mavenVersion = new MavenVersion(
                    String.valueOf(v.getMajorVersion()),
                    String.valueOf(v.getMinorVersion()),
                    v.getQualifier());
        }
        return mavenVersion;
    }
    
   protected void checkParams() throws MojoExecutionException {
       try {
            SpecVersion.checkParams(
                    nonfinal,
                    api,
                    impl,
                    apipackage,
                    standalone,
                    implpackage,
                    implversion,
                    newimplversion,
                    specversion,
                    newspecversion,
                    getMavenVersion().getBuildNumber(),
                    getMavenVersion().getBuildNumber(),
                    specimplversion,
                    getMavenVersion(),
                    project.getGroupId(),
                    project.getArtifactId());
        } catch (SpecException ex) {
            throw new MojoExecutionException(ex.getMessage());
        }
    }

   protected Properties computeSpecProperties(){
        return SpecVersion.computeProperties(
                nonfinal,
                api,
                impl,
                apipackage,
                specversion,
                getMavenVersion().getBuildNumber(),
                implpackage,
                implversion,
                getMavenVersion().getBuildNumber());
    }
}