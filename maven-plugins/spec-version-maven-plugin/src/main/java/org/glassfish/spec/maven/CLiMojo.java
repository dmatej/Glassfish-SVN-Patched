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

import java.io.Console;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import java.util.jar.JarFile;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.glassfish.spec.Artifact;
import org.glassfish.spec.Spec;


/**
 *
 * @goal cli
 * @phase validate
 * @requiresProject
 *
 * @author Romain Grecourt
 */
public class CLiMojo extends AbstractSpecMojo {
    /**
     * 
     * @parameter expression="${properties}"
     */
    protected File properties;
    
    /**
     * 
     * @parameter expression="${help}"
     */
    protected Boolean help;
    

    private static Console cons;    
    
    /**
     * Prompt with the string and return the user's input.
     */
    private static String prompt(String p) {
	if (cons == null)
	    return null;
	String s = cons.readLine("%s: ", p);
	if (s == null || s.length() == 0)
	    return null;
	return s;
    }
    
    private static boolean getBooleanProperty(Properties p, String name, boolean def) {
	String s = p.getProperty(name);
	if (s == null)
	    return def;
	return Boolean.parseBoolean(s);
    }
    
    /**
     * Print error and exit.
     */
    private static void fail(String s) {
	System.err.println("ERROR: " + s);
	System.exit(1);
    }
    
    private static void printParam(String arg, String desc){
        StringBuilder sb = new StringBuilder("\t-D");
        System.out.println(sb.append(arg).append(' ').append(desc).toString());
    }
    
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if (help) {
            printParam("properties", "file\tread settings from property file");
            printParam("nonfinal", "\t\tnon-final specification");
            printParam("standalone", "\t\tAPI has a standalone implementation");
            printParam("apijar", "api.jar\tAPI jar file");
            printParam("impljar", "impl.jar\timplementation jar file");
            printParam("apipackage", "package\tAPI package");
            printParam("implpackage", "package\timplementation package");
            printParam("specversion", "version\tversion number of the JCP specification");
            printParam("specimplversion", "vers\tversion number of the API classes");
            printParam("implversion", "version\tversion number of the implementation");
            printParam("newspecversion", "vers\tversion number of the spec under development");
            printParam("specbuild", "num\tbuild number of spec API jar file");
            printParam("newimplversion", "vers\tversion number of the implementation when final");
            printParam("implbuild", "num\tbuild number of implementation jar file");
            return;
        }
        
        Artifact artifact = null;
        
        if (properties != null) {
            FileInputStream fis = null;
            try {
                Properties p = new Properties();
                fis = new FileInputStream(properties);
                p.load(fis);
                fis.close();
                apiPackage = p.getProperty("API_PACKAGE", apiPackage);
                implNamespace = p.getProperty("IMPL_NAMESPACE", implNamespace);
                isAPI = !getBooleanProperty(p, "STANDALONE_IMPL", isAPI);
                if(isAPI){
                    implVersion = p.getProperty("SPEC_IMPL_VERSION", implVersion);
                    specBuild = p.getProperty("SPEC_BUILD", specBuild);
                    newSpecVersion = p.getProperty("NEW_SPEC_VERSION", newSpecVersion);
                    apiJar = p.getProperty("API_JAR",apiJar);
                    artifact = Artifact.fromJar(new JarFile(apiJar));
                } else {
                    implVersion = p.getProperty("IMPL_VERSION", implVersion);
                    implBuild = p.getProperty("IMPL_BUILD", implBuild);
                    newImplVersion = p.getProperty("NEW_IMPL_VERSION", newImplVersion);
                    implJar = p.getProperty("IMPL_JAR", implJar);
                    artifact = Artifact.fromJar(new JarFile(implJar));
                }
                specVersion = p.getProperty("SPEC_VERSION", specVersion);
                isFinal = newSpecVersion == null;	// really, any of the above 4
            } catch (FileNotFoundException ex) {
                throw new MojoExecutionException(ex.getMessage(), ex);
            } catch (IOException ex) {
                throw new MojoExecutionException(ex.getMessage(), ex);
            } finally {
                try {
                    fis.close();
                } catch (IOException ex) {
                    getLog().warn(ex.getMessage());
                }
            }
        }

        if (isAPI) {
	    if (implJar != null)
		fail("--impljar must not be specified if no standalone implementation");
	    if (implNamespace != null)
		fail("--implpackage must not be specified if no standalone implementation");
	    if (implVersion != null)
		fail("--implversion must not be specified if no standalone implementation");
	    if (newImplVersion != null)
		fail("--newimplversion must not be specified if no standalone implementation");
	}

	if (isFinal) {
	    if (newSpecVersion != null)
		fail("--newspecversion must not be specified for final specification");
	    if (specBuild != null)
		fail("--specbuild must not be specified for final specification");
	    if (newImplVersion != null)
		fail("--newimplversion must not be specified for final specification");
	    if (implBuild != null)
		fail("--implbuild must not be specified for final specification");
	}
        
        
        // if no options, prompt for everything
        if (properties == null
                && apiJar == null
                && implJar == null
                && implNamespace == null
                && apiPackage == null
                && specVersion == null
                && specImplVersion == null
                && implVersion == null
                && newImplVersion == null
                && newSpecVersion == null
                && specBuild == null
                && implBuild == null) {

            cons = System.console();
            String s;
            s = prompt("Is this a non-final specification?");
            isFinal = !(s.charAt(0) == 'y');
            s = prompt("Is there a standalone implementation of this specification?");
            isAPI = !(s.charAt(0) == 'y');

            apiPackage = prompt("Enter the main API package (e.g., javax.wombat)");
            specVersion = prompt("Enter the version number of the JCP specification");

            if (isAPI) {
                specImplVersion = prompt("Enter the version number of the API jar file");
                newSpecVersion = prompt("Enter the version number of the implementation that will be used when the implementation is final");
                if (!isFinal) {
                    specBuild = prompt("Enter the build number of the implementation jar file");
                }
                artifact = new Artifact(apiPackage, apiPackage+Spec.API_SUFFIX, newSpecVersion);
            } else {
                implNamespace = prompt("Enter the main implementation package (e.g., com.sun.wombat)");
                if (!isFinal) {
                    implBuild = prompt("Enter the build number of the implementation jar file");
                }
                newImplVersion = prompt("Enter the version number of the Impl jar file");
                artifact = new Artifact(implNamespace, apiPackage, newImplVersion);
            }
        }
        
        Spec spec = new Spec(
                artifact,
                specVersion,
                newSpecVersion,
                specImplVersion,
                implVersion,
                newImplVersion,
                specBuild,
                implBuild,
                apiPackage,
                implNamespace);
        spec.verify();
        
        for(String error : spec.getErrors()){
            System.out.println(error);
        }
    }
}