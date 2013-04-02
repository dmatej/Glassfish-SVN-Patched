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

import java.io.IOException;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 *
 * @author Romain Grecourt
 */
public final class Spec {
    private Artifact artifact;
    private Metadata metadata;
    private String specVersion;
    private String newVersion;
    private String implVersion;
    
    static final String START_WITH_JAVAX = "start with \"javax.\"";
    
    static void checkSpecVersion(String s, String key, List<String> errors) {
        if (s != null && !s.matches("[0-9]+\\.[0-9]+")) {
            String msg = "invalid "+key+" (" + s + "):"
                    + " JCP specification version number "
                    + "must be of the form <major>.<minor>";
            if (errors != null) {
                errors.add(msg);
            }
        }
    }

    static void checkApiPackage(String s, List<String> errors) {
        if (s!=null && !s.startsWith("javax.")) {
            String msg = "API packages must" + START_WITH_JAVAX + " but is \"" + s + "\"";
            if (errors != null) {
                errors.add(msg);
            }
        }
    }
    
    public void verify(){
        
//        if(_artifact == null 
//                || specVersion == null || specVersion.isEmpty()
//                || implVersion == null || implVersion.isEmpty()){
//            
//             throw new IllegalArgumentException("artifact, specVersion"
//                     + " and implVersion can't be null or empty");
//        }
        
//        if (!_artifact.isFinal()) {
//            if (newVersion == null) {
//                
//                throw new IllegalArgumentException("new version is required for nonfinal");
//                
//            } else if (!_artifact.specVersionEquals(newVersion)) {
//                
//                throw new IllegalArgumentException(
//                        "maven version should be "
//                        + newVersion + "-[b|m]" + _artifact.getBuildNumber()
//                        + "[-SNAPSHOT]");
//            }
//        } else {
//            if (newVersion != null && !newVersion.isEmpty()) {
//                
//                throw new IllegalArgumentException(
//                        "newspecversion must not be specified for final specification");
//            }
//        }
        
        List<String> errors = artifact.getErrors();

        if (artifact.isAPI()) {

            checkSpecVersion(specVersion,"spec version",errors);
            checkSpecVersion(newVersion,"new spec version",errors);            
            String sv = artifact.isFinal() ? specVersion : newVersion;
            
            if (!(implVersion.equals(sv)
                    || implVersion.startsWith(sv + ".")
                    || implVersion.startsWith(sv + "-"))) {
                
                errors.add("spec implementation ("+implVersion+") version must"
                        + " start with JCP specification version number ("+sv+")");
            }
        } else {
            checkSpecVersion(specVersion,"spec version",errors);
            checkSpecVersion(newVersion,"new impl version",errors);              
        }
        
//        if(bundleSymbolicName == null
//                || bundleVersion == null){
//            return;
//        }
//        
//        boolean isAPI = false;
//        if (bundleSymbolicName.contains(Artifact.API_SUFFIX)) {
//            isAPI = true;
//        }
//        boolean isFinal = true;
//        if (bundleVersion.contains(NONFINAL_BUILD_SEPARATOR)) {
//            isFinal = false;
//        }
//
//        String apiPackage = jarExtensionName;
//        Spec.checkApiPackage(apiPackage, errors);
//        
//        if (isAPI) {
//            //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
//            if (bundleSymbolicName!=null
//                    && apiPackage != null
//                    &&!apiPackage.equals(
//                    bundleSymbolicName.substring(0,
//                    bundleSymbolicName.lastIndexOf(Artifact.API_SUFFIX)))) {
//                errors.add(BUNDLE_SYMBOLIC_NAME+" should be"
//                        + " \"" + apiPackage + Artifact.API_SUFFIX + "\"");
//            }
//        } else {
//            //  OSGi Bundle-SymbolicName:	${IMPL_NAMESPACE}.${API_PACKAGE}
//            if (bundleSymbolicName!= null
//                    && apiPackage != null
//                    && !bundleSymbolicName.contains("." + apiPackage)) {
//                 errors.add(BUNDLE_SYMBOLIC_NAME + " (" + bundleSymbolicName + ") "
//                        + "should end with \"."+ apiPackage + "\"");
//            }
//        }
//
//        if (isFinal) {
//            //  OSGi Bundle-Version:    ${SPEC_IMPL_VERSION}
//            //  jar Implementation-Version:	${SPEC_IMPL_VERSION}
//            if (bundleVersion!=null
//                    && jarImplementationVersion !=null
//                    && !bundleVersion.equals(jarImplementationVersion)) {
//                 errors.add(BUNDLE_VERSION
//                        + " (" + bundleVersion + ") should be equal"
//                        + " to "+JAR_IMPLEMENTATION_VERSION
//                        + " (" + jarImplementationVersion + ")");
//            }
//
//            Spec.checkSpecVersion(jarSpecificationVersion, "jarSpecificationVersion",errors);
//        } else {
//            // Specification-Version can't use '.b' for the build number
//            if (jarSpecificationVersion.contains(".b")) {
//                 errors.add(JAR_SPECIFICATION_VERSION
//                        +" cannot contain '.b': "+ jarSpecificationVersion);
//            }
//
//            // Specification-Version and Bundle-Version should be similar
//            // however one uses .b. as a separator, the other does not
//            if (!bundleVersion.replace(
//                    NONFINAL_BUILD_SEPARATOR,
//                    NONFINAL_BUILD_SEPARATOR_SPEC).equals(jarSpecificationVersion)) {
//                errors.add(BUNDLE_VERSION + " and "
//                        + JAR_SPECIFICATION_VERSION + " mismatch");
//            }
//
//            //  .99.XX
//            int idx = jarSpecificationVersion.lastIndexOf(NONFINAL_BUILD_SEPARATOR_SPEC);
//            Spec.checkSpecVersion(jarSpecificationVersion.substring(0, idx), "jarSpecificationVersion", errors);
//            
//            // .99-bXX
//            idx = bundleVersion.lastIndexOf(NONFINAL_BUILD_SEPARATOR);
//            Spec.checkSpecVersion(bundleVersion.substring(0, idx), "bundleVersion",errors);
//        }        
    }

    public Spec(JarFile jar) throws IOException{
        this.artifact = Artifact.fromJar(jar);
        this.metadata = Metadata.fromJar(jar);
    }
    
    public Spec(
            Artifact _artifact,
            String _specVersion,
            String _newVersion,
            String _implVersion) {
        
        this.specVersion = _specVersion;
        this.newVersion = _newVersion;
        this.implVersion = _implVersion;
        this.artifact = _artifact;
        this.metadata = Metadata.generate(artifact,specVersion,newVersion,implVersion);
    }
    
    public Artifact getArtifact() {
        return artifact;
    }

    public Metadata getMetadata() {
        return metadata;
    }
    
    public List<String> getErrors(){
        return metadata.getErrors();
    }
    
    private static void err(String s) {
	System.out.println("ERROR: " + s);
    }

    private static void warn(String s) {
	System.out.println("WARNING: " + s);
    }

    /**
     * Check that the classes in the jar file are in one of the
     * specified packages.
     */
    private static void checkClasses(JarFile jf, String... pkgs) {
	Enumeration<JarEntry> e = jf.entries();
	Set<String> badPackages = new HashSet<String>();
	entries:
	while (e.hasMoreElements()) {
	    JarEntry je = e.nextElement();
	    if (je.isDirectory())
		continue;
	    String name = je.getName();
	    if (!name.endsWith(".class"))
		continue;
	    name = name.substring(0, name.length() - 6); // remove ".class"
	    name = name.replace('/', '.');

	    // name is now the class name,
	    // is it in one of the allowed packages?
	    for (String p : pkgs) {
		if (name.startsWith(p) && name.charAt(p.length()) == '.')
		    continue entries;	// yes, move on to next entry
	    }

	    // not in an allowed package
	    int i = name.lastIndexOf('.');
	    if (i > 0)
		name = name.substring(0, i);	// remove class name

	    // see if we've already complained about it
	    if (!badPackages.contains(name)) {
		badPackages.add(name);
		if (name.startsWith("javax."))
		    err("jar file includes class in wrong package: " + name);
		else
		    warn("jar file includes class in wrong package: " + name);
	    }
	}
    }
}