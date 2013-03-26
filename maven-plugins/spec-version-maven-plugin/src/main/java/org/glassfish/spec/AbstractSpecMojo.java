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
package org.glassfish.spec;

import java.util.Properties;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;


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
    
    private static final String SNAPSHOT_QUALIFIER = "SNAPSHOT";
    
    private void fail(String s) throws MojoExecutionException{
        throw new MojoExecutionException(s);
    }    
    
    protected void checkParams() throws MojoExecutionException {
        ArtifactVersion mavenVersion = new DefaultArtifactVersion(project.getVersion());
        String qualifier = mavenVersion.getQualifier();
        if (qualifier.endsWith(SNAPSHOT_QUALIFIER)){
            qualifier = qualifier.substring(0,qualifier.length()-SNAPSHOT_QUALIFIER.length()-1);
        }
        String buildNumber = null;
        if(qualifier.startsWith("b")){
            buildNumber = qualifier.substring(1);
        }
        
        String absoluteVersion = 
                mavenVersion.getMajorVersion()+
                "."+mavenVersion.getMinorVersion()+
                "-"+qualifier;
        
        if(!nonfinal && buildNumber != null){
            fail("maven version can't include a build number for final spec");
        }
        
	if (apipackage == null){
            fail("apipackage is required");
        }
        if(!apipackage.startsWith("javax.")){
	    fail("API packages must start with \"javax.\"");
        }
        
	if (standalone) {
	    if (implpackage == null){
                fail("implpackage is required for standalone");
            }
            if(implversion == null){
                fail("implversion is required for standalone");
            }
	} else {
	    if (implpackage != null)
		fail("implpackage must not be specified " +
				    "if no standalone implementation");
	    if (implversion != null)
		fail("implversion must not be specified " +
				    "if no standalone implementation");
	    if (newimplversion != null)
		fail("newimplversion must not be specified " +
				    "if no standalone implementation");
	}

	if (implpackage != null && implpackage.startsWith("javax."))
	    fail("Implementation packages must NOT start with \"javax.\"");

	if (specversion == null){
	    fail("specversion is required");
        }

	if (!specversion.matches("[0-9]+\\.[0-9]+"))
	    fail("JCP specification version number must be " +
				"of the form <major>.<minor>");
	if (specimplversion == null){
	    fail("specimplversion is required");
        }
        
	String sv = nonfinal ? newspecversion : specversion;
	if (!(specimplversion.equals(sv) ||
		specimplversion.startsWith(sv + ".") ||
		specimplversion.startsWith(sv + "-"))){
	    fail("API jar file version must start with " +
				"JCP specification version number");
        }

	if (nonfinal) {
	    if (newspecversion == null){
                fail("newspecversion is required for nonfinal");
            }
            if (specbuild == null) {
                if (buildNumber != null) {
                    specbuild = buildNumber;
                } else {
                    fail("specbuild is required for nonfinal");
                }
            }
            if (standalone) {
                if (newimplversion == null) {
                    fail("newimplversion is required for nonfinal standalone");
                }
                if (standalone && implbuild == null) {
                    fail("implbuild is required for nonfinal standalone");
                }
            }
            
            // check maven version
            if (api) {
                if (!absoluteVersion.contentEquals(newspecversion + "-b" + specbuild)) {
                    fail("maven version should be " + newspecversion + "-b" + specbuild + "[-SNAPSHOT]");
                }
            } else {
                if (!absoluteVersion.contentEquals(newimplversion + "-b" + implbuild)) {
                    fail("maven version should be " + newimplversion + "-b" + implbuild + "[-SNAPSHOT]");
                }
            }
	} else {
	    if (newspecversion != null){
		fail("newspecversion must not be specified for final specification");
            }
	    if (specbuild != null){
		fail("specbuild must not be specified for final specification");
            }
	    if (newimplversion != null){
		fail("newimplversion must not be specified for final specification");
            }
	    if (implbuild != null){
		fail("implbuild must not be specified for final specification");
            }
            
            // check maven version
            if (api) {
                if(!absoluteVersion.contentEquals(specversion)){
                    fail("maven version should be "+specversion+"[-SNAPSHOT]");
                }
                
            } else {
                if(!absoluteVersion.contentEquals(implversion)){
                    fail("maven version should be "+implversion+"[-SNAPSHOT]");
                }
            }
	}
        
        if(api){
            // check groupId
            if(!project.getGroupId().contentEquals(apipackage)){
                fail("groupId should be "+apipackage+", not "+project.getGroupId());
            }
            // check artifactId
            if(!project.getArtifactId().contentEquals(apipackage+"-api")){
                fail("artifactId should be "+apipackage+"-api"+", not "+project.getArtifactId());
            }
        } else if (impl || standalone){
            // check artifactId
            if(!project.getArtifactId().contentEquals(apipackage)){
                fail("artifactId should be "+apipackage+", not "+project.getArtifactId());
            }
        }
    }
    
    protected Properties computeSpecProperties(){
        Properties specProps = new Properties();

        specProps.put("spec.extension.name", apipackage);
        if (nonfinal) {
            specProps.put("spec.bundle.spec.version", specversion + ".99.b" + specbuild);
        } else {
            specProps.put("spec.bundle.spec.version", specversion);
        }

        if (api) {
            specProps.put("spec.bundle.symbolic-name", apipackage + "-api");
            if (nonfinal) {
                specProps.put("spec.bundle.version", specversion + ".99.b" + specbuild);
                specProps.put("spec.implementation.version", specversion + "-b" + specbuild);
            } else {
                specProps.put("spec.bundle.version", specversion);
                specProps.put("spec.implementation.version", specversion);
            }
        } else {
            specProps.put("spec.bundle.symbolic-name", implpackage + "." + apipackage);
            if (nonfinal) {
                specProps.put("spec.specification.version", implversion + ".99." + specbuild);
                specProps.put("spec.implementation.version", implversion + "-b" + implbuild);
            } else {
                specProps.put("spec.specification.version", implversion);
                specProps.put("spec.implementation.version", implversion);
            }
        }
        return specProps;
    }
}