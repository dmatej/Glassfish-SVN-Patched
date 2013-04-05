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
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

/**
 *
 * @author Romain Grecourt
 */
public class Spec {

    private Artifact artifact;
    private Metadata metadata;
    private JarFile jar;
    private String specVersion;
    private String newSpecVersion;
    private String specImplVersion;
    private String implVersion;
    private String newImplVersion;
    private String specBuild;
    private String implBuild;
    private String apiPackage;
    private String implNamespace;
    private Boolean nonFinal = null;
    private Boolean standaloneImpl = null;
    
    private List<String> errors = new LinkedList<String>();
    private static final String NONFINAL_BUILD_SEPARATOR_SPEC = ".99.";
    private static final String NONFINAL_BUILD_SEPARATOR = NONFINAL_BUILD_SEPARATOR_SPEC + "b";
    public static final String API_SUFFIX = "-api";

    public Spec(){
    }

    public Spec(
            Artifact _artifact,
            String _specVersion,
            String _newSpecVersion,
            String _specImplVersion,
            String _implVersion,
            String _newImplVersion,
            String _specBuild,
            String _implBuild,
            String _apiPackage,
            String _implNamespace,
            boolean _standaloneImpl,
            boolean _nonFinal) {
        
        this.artifact = _artifact;
        this.specVersion = _specVersion != null ? _specVersion : "";
        this.newSpecVersion = _newSpecVersion != null ? _newSpecVersion : "";
        this.specImplVersion = _specImplVersion != null ? _specImplVersion : "";
        this.implVersion = _implVersion != null ? _implVersion : "";
        this.newImplVersion = _newImplVersion != null ? _newImplVersion : "";
        this.specBuild = _specBuild != null ? _specBuild : "";
        this.implBuild = _implBuild != null ? _implBuild : "";
        this.apiPackage = _apiPackage != null ? _apiPackage : "";
        this.implNamespace = _implNamespace != null ? _implNamespace : "";
        this.standaloneImpl = _standaloneImpl;
        this.nonFinal = _nonFinal;
    }
    
    public void read(JarFile _jar) throws IOException {
        this.jar = _jar;
        this.artifact = Artifact.fromJar(jar);
        this.metadata = Metadata.fromJar(jar);
        this.errors.clear();
        this.errors.addAll(metadata.getErrors());
    }

    private void checkClasses(JarFile jf, String... pkgs) {
        Enumeration<JarEntry> e = jf.entries();
        Set<String> badPackages = new HashSet<String>();
        entries:
        while (e.hasMoreElements()) {
            JarEntry je = e.nextElement();
            if (je.isDirectory()) {
                continue;
            }
            String name = je.getName();
            if (!name.endsWith(".class")) {
                continue;
            }
            name = name.substring(0, name.length() - 6); // remove ".class"
            name = name.replace('/', '.');

            // name is now the class name,
            // is it in one of the allowed packages?
            for (String p : pkgs) {
                if (name.startsWith(p) && name.charAt(p.length()) == '.') {
                    // yes, move on to next entry
                    continue entries;
                }
            }

            // not in an allowed package
            int i = name.lastIndexOf('.');
            if (i > 0) {
                // remove class name
                name = name.substring(0, i);
            }

            // see if we've already complained about it
            if (!badPackages.contains(name)) {
                badPackages.add(name);
                if (name.startsWith("javax.")) {
                    errors.add(new StringBuilder()
                            .append("jar file includes class in wrong ")
                            .append("package (")
                            .append(name)
                            .append(") ")
                            .toString());
                }
            }
        }
    }

    public void verify() {
        this.errors.clear();
        this.errors.addAll(getMetadata().getErrors());
        
        boolean abort=false;
        StringBuilder configIssues = new StringBuilder();
        configIssues.append("ERROR: missing configuration (");
        
        if(nonFinal == null){ 
            configIssues.append(" nonFinal?");
            abort=true;
        }
        if(standaloneImpl == null){
            configIssues.append(" standaloneImpl?");
            abort=true;
        }
        if(specVersion == null || specVersion.isEmpty()){
            configIssues.append(" specification-version");
            abort=true;
        }
        if(apiPackage == null || apiPackage.isEmpty()){
            configIssues.append(" api-package");
            abort=true;
        }
        if(nonFinal!=null 
                && nonFinal.booleanValue() 
                && (newSpecVersion == null || newSpecVersion.isEmpty())){
            configIssues.append(" new-spec-version");
            abort=true;
        }
        if (standaloneImpl!=null && standaloneImpl.booleanValue()) {
            if (implNamespace == null || implNamespace.isEmpty()) {
                configIssues.append(" implementation-namespace");
                abort=true;
            }
            if (implVersion == null || implVersion.isEmpty()) {
                configIssues.append(" implementation-version");
                abort=true;
            }
            if(nonFinal.booleanValue()
                    && (newImplVersion == null || newImplVersion.isEmpty())){
                configIssues.append(" new-implementation-version");
                abort=true;
            }
        }
        
        // no need to continue further...
        if(abort){
            errors.add(configIssues.append(" )").toString());
            return;
        }
        
        // verify that specVersion is <major>.<minor>
        if (!specVersion.matches("[0-9]+\\.[0-9]+")) {
            errors.add(new StringBuilder()
                    .append("WARNING: specification version (")
                    .append(specVersion)
                    .append(") is invalid, JCP specification version number ")
                    .append("must be of the form <major>.<minor>")
                    .toString());
        }

        // verify that Implementation-Version == Maven-Version
        if (!getMetadata().getjarImplementationVersion()
                .equals(artifact.getVersion().toString())) {
            errors.add(new StringBuilder()
                    .append("WARNING: ")
                    .append(Metadata.JAR_IMPLEMENTATION_VERSION)
                    .append(" (")
                    .append(getMetadata().getjarImplementationVersion())
                    .append(") should be equal to ")
                    .append("Maven-Version (")
                    .append(artifact.getVersion().toString())
                    .append(')')
                    .toString());
        }

        if (!nonFinal) {
            // verify Bundle-Version
            if (!getMetadata().getBundleVersion()
                    .equals(artifact.getVersion().toString())) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append(Metadata.BUNDLE_VERSION)
                        .append(" (")
                        .append(metadata.getBundleVersion())
                        .append(") should be ")
                        .append(artifact.getVersion().toString())
                        .toString());
            }

            // TODO check BundleSpecVersion == JarSpec
        } else {
            
            ArtifactVersion av = new DefaultArtifactVersion(specVersion);

            // verify Bundle-Version
            String bundleVersion =
                    new StringBuilder()
                    .append(av.getMajorVersion())
                    .append('.')
                    .append(av.getMinorVersion())
                    .append(NONFINAL_BUILD_SEPARATOR)
                    .append(standaloneImpl? implBuild : specBuild)
                    .toString();

            if (!getMetadata().getBundleVersion().equals(bundleVersion)) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append(Metadata.BUNDLE_VERSION)
                        .append(" (")
                        .append(metadata.getBundleVersion())
                        .append(") should be ")
                        .append(bundleVersion)
                        .toString());
            }

            // TODO check BundleSpecVersion == JarSpec (with no b)
        }

        if (!standaloneImpl) {
            // verify that groupId starts with javax.
            if (!artifact.getGroupId().startsWith("javax.")) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append("groupId (")
                        .append(artifact.getGroupId())
                        .append(") must start with \"javax.\"")
                        .toString());
            }

            // verify that artifactId does end with -api
            if (!artifact.getArtifactId().endsWith(API_SUFFIX)) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append("artifactId (")
                        .append(artifact.getArtifactId())
                        .append(") should end with ")
                        .append(API_SUFFIX)
                        .toString());
            }

            // verify that apiPackage starts with javax.
            if (!apiPackage.startsWith("javax.")) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append("API packages (")
                        .append(apiPackage)
                        .append(") must start with \"javax.\"")
                        .toString());
            }

            // verify that Bundle-SymbolicName == implNamespace.apiPackage
            String symbolicName =
                    new StringBuilder(apiPackage)
                    .append(API_SUFFIX)
                    .toString();
            if (!symbolicName.equals(getMetadata().getBundleSymbolicName())) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append(Metadata.BUNDLE_SYMBOLIC_NAME)
                        .append(" (")
                        .append(apiPackage)
                        .append(") should be ")
                        .append(symbolicName)
                        .toString());
            }

            // verify that Extension-Name == apiPackage
            if (!getMetadata().getJarExtensionName().equals(apiPackage)) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append(Metadata.JAR_EXTENSION_NAME)
                        .append(" (")
                        .append(getMetadata().getJarExtensionName())
                        .append(") should be ")
                        .append(apiPackage)
                        .toString());
            }

            if (jar != null) {
                checkClasses(jar, apiPackage);
            }
            
            // verify that implementation version starts with spec version
            String sv = nonFinal? newSpecVersion : specVersion;
            if (!(specImplVersion.equals(sv)
                    || specImplVersion.startsWith(sv + ".")
                    || specImplVersion.startsWith(sv + "-"))) {
                errors.add(new StringBuilder()
                        .append("WARNING: spec-implementation-version (")
                        .append(specImplVersion)
                        .append(") must start with ")
                        .append("JCP specification version number (")
                        .append(sv)
                        .append(')')
                        .toString());
            }            

            if (nonFinal) {
                // verify new spec version
                if (!newSpecVersion.matches("[0-9]+\\.[0-9]+")) {
                    errors.add(new StringBuilder()
                            .append("WARNING: new-specification-version (")
                            .append(newSpecVersion)
                            .append(") is invalid, ")
                            .append("JCP specification version number ")
                            .append("must be of the form <major>.<minor>")
                            .toString());
                }
                
                // verify that specVersion != newSpecVersion
                if (specVersion.equals(newSpecVersion)) {
                    errors.add(new StringBuilder()
                            .append("WARNING: specification-version (")
                            .append(specVersion)
                            .append(") can't be equal to ")
                            .append("new-specification-version (")
                            .append(newSpecVersion)
                            .append(") for non final artifacts")
                            .toString());
                } else {
                    ArtifactVersion specAV = new DefaultArtifactVersion(specVersion);
                    ArtifactVersion newSpecAV = new DefaultArtifactVersion(newSpecVersion);

                    // verify that specVersion < newSpecVersion
                    if (specAV.compareTo(newSpecAV) > 0) {
                        errors.add(new StringBuilder()
                                .append("new-specification-version (")
                                .append(newSpecVersion)
                                .append(") must be greater than ")
                                .append("specification-version (")
                                .append(specVersion)
                                .append(')')
                                .toString());
                    } else {
                        // verify offset between specVersion and newSpecVersion
                        if (newSpecAV.getMajorVersion() - specAV.getMajorVersion() > 1
                                || newSpecAV.getMinorVersion() - specAV.getMinorVersion() > 1) {
                            errors.add(new StringBuilder()
                                    .append("WARNING: offset between ")
                                    .append("new-specification-version (")
                                    .append(newSpecVersion)
                                    .append(") and specification-version (")
                                    .append(specVersion)
                                    .append(") can't be greater than 1")
                                    .toString());
                        }
                    }
                }
            }
        } else {
            // verify that groupId starts with javax.
            if (artifact.getGroupId().startsWith("javax.")) {
                errors.add(new StringBuilder()
                        .append("WARNING: groupId (")
                        .append(artifact.getGroupId())
                        .append(") should not start with \"javax.\"")
                        .toString());
            }

            // verify that artifactId does not end with -api
            if (artifact.getArtifactId().endsWith(API_SUFFIX)) {
                errors.add(new StringBuilder()
                        .append("WARNING: artifactId (")
                        .append(artifact.getArtifactId())
                        .append(") should not end with ")
                        .append(API_SUFFIX)
                        .toString());
            }

            // verify that Extension-Name == apiPackage
            if (!getMetadata().getJarExtensionName().equals(apiPackage)) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append(Metadata.JAR_EXTENSION_NAME)
                        .append(" (")
                        .append(getMetadata().getJarExtensionName())
                        .append(") should be ")
                        .append(apiPackage)
                        .toString());
            }

            // verify that apiPackage starts with javax.
            if (!apiPackage.startsWith("javax.")) {
                errors.add(new StringBuilder()
                        .append("WARNING: API packages (")
                        .append(apiPackage)
                        .append(") must start with \"javax.\"")
                        .toString());
            }

            // verify that Bundle-SymbolicName == implNamespace.apiPackage
            String symbolicName =
                    new StringBuilder(implNamespace)
                    .append('.')
                    .append(apiPackage)
                    .toString();

            if (!getMetadata().getBundleSymbolicName()
                    .equals(symbolicName)) {
                errors.add(new StringBuilder()
                        .append("WARNING: ")
                        .append(Metadata.BUNDLE_SYMBOLIC_NAME)
                        .append("( ")
                        .append(getMetadata().getBundleSymbolicName())
                        .append(") should be ")
                        .append(symbolicName)
                        .toString());
            }

            if (jar != null) {
                checkClasses(jar, apiPackage, implNamespace);
            }

            if (nonFinal) {
                // verify that implVersion != newImplVersion
                if (implVersion.equals(newImplVersion)) {
                    errors.add(new StringBuilder()
                            .append("WARNING: implementation-version (")
                            .append(implVersion)
                            .append(") can't be equal to ")
                            .append("new-implementation-version (")
                            .append(newImplVersion)
                            .append(") for non final artifacts")
                            .toString());
                } else {
                    ArtifactVersion implAV = new DefaultArtifactVersion(implVersion);
                    ArtifactVersion newImplAV = new DefaultArtifactVersion(newImplVersion);

                    // verify that implVersion < newImplVersion
                    if (implAV.compareTo(newImplAV) > 0) {
                        errors.add(new StringBuilder()
                                .append("WARNING: new-implementation-version (")
                                .append(newImplVersion)
                                .append(") must be greater than ")
                                .append("implementation-version (")
                                .append(implVersion)
                                .append(')')
                                .toString());
                    } else {
                        // verify offset between implVersion and newImplVersion
                        if (newImplAV.getMajorVersion() - implAV.getMajorVersion() > 1
                                || newImplAV.getMinorVersion() - implAV.getMinorVersion() > 1) {

                            errors.add(new StringBuilder()
                                    .append("WARNING: offset between ")
                                    .append("new-implementation-version (")
                                    .append(newImplVersion)
                                    .append(") and implementation-version (")
                                    .append(implVersion)
                                    .append(") can't be greater than 1")
                                    .toString());
                        }
                    }
                }
            }
        }
    }

    public Artifact getArtifact() {
        return artifact;
    }

    public Metadata getMetadata() {
        if (metadata != null) {
            return metadata;
        }

        if (!standaloneImpl) {
            if (!nonFinal) {
                //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
                //  OSGi bundle specversion:	${SPEC_VERSION}
                //  OSGi Bundle-Version:	${SPEC_IMPL_VERSION}
                //  jar Extension-Name:		${API_PACKAGE}
                //  jar Specification-Version:	${SPEC_VERSION}
                //  jar Implementation-Version:	${SPEC_IMPL_VERSION}

                metadata = new Metadata(
                        apiPackage + Spec.API_SUFFIX,
                        specVersion,
                        specImplVersion,
                        apiPackage,
                        specVersion,
                        specImplVersion);

            } else {
                //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
                //  OSGi bundle specversion:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                //  OSGi Bundle-Version:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                //  jar Extension-Name:		${API_PACKAGE}
                //  jar Specification-Version:	${SPEC_VERSION}.99.${SPEC_BUILD}
                //  jar Implementation-Version:	${NEW_SPEC_VERSION}-b${SPEC_BUILD}
                
                String osgiVersion = new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR)
                        .append(specBuild)
                        .toString();
                
                metadata = new Metadata(
                        apiPackage + Spec.API_SUFFIX,
                        osgiVersion,
                        osgiVersion,
                        apiPackage,
                        new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR_SPEC)
                        .append(specBuild)
                        .toString(),
                        artifact.getVersion().toString());
            }
        } else {
            String symbolicName = implNamespace + "." + apiPackage;

            if (!nonFinal) {
                //  OSGi Bundle-SymbolicName:	${IMPL_NAMESPACE}.${API_PACKAGE}
                //  OSGi bundle specversion:	${SPEC_VERSION}
                //  OSGi Bundle-Version:	${IMPL_VERSION}
                //  jar Extension-Name:		${API_PACKAGE}
                //  jar Specification-Version:	${SPEC_VERSION}
                //  jar Implementation-Version:	${IMPL_VERSION}

                metadata = new Metadata(
                        symbolicName,
                        specVersion,
                        implVersion,
                        apiPackage,
                        specVersion,
                        artifact.getVersion().toString());
            } else {

                //  OSGi Bundle-SymbolicName:	${IMPL_NAMESPACE}.${API_PACKAGE}
                //  OSGi bundle specversion:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                //  OSGi Bundle-Version:	${OSGI_IMPL_VERSION}.99.b${IMPL_BUILD}
                //  jar Extension-Name:		${API_PACKAGE}
                //  jar Specification-Version:	${SPEC_VERSION}.99.${SPEC_BUILD}
                //  jar Implementation-Version:	${NEW_IMPL_VERSION}-b${IMPL_BUILD}

                ArtifactVersion implAv = new DefaultArtifactVersion(implVersion);

                metadata = new Metadata(
                        symbolicName,
                        new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR)
                        .append(implBuild)
                        .toString(),
                        new StringBuilder()
                        .append(implAv.getMajorVersion())
                        .append('.')
                        .append(implAv.getMinorVersion())
                        .append(NONFINAL_BUILD_SEPARATOR)
                        .append(implBuild)
                        .toString(),
                        apiPackage,
                        new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR_SPEC)
                        .append(implBuild)
                        .toString(),
                        artifact.getVersion().toString());
            }
        }
        return metadata;
    }

    public List<String> getErrors() {
        return errors;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public void setImplNamespace(String implNamespace) {
        this.implNamespace = implNamespace;
    }

    public void setImplVersion(String implVersion) {
        this.implVersion = implVersion;
    }

    public void setSpecVersion(String specVersion) {
        this.specVersion = specVersion;
    }

    public void setNewImplVersion(String newImplVersion) {
        this.newImplVersion = newImplVersion;
    }

    public void setSpecBuild(String specBuild) {
        this.specBuild = specBuild;
    }

    public void setSpecImplVersion(String specImplVersion) {
        this.specImplVersion = specImplVersion;
    }

    public void setNewSpecVersion(String newSpecVersion) {
        this.newSpecVersion = newSpecVersion;
    }

    public void setImplBuild(String implBuild) {
        this.implBuild = implBuild;
    }

    public void setArtifact(Artifact artifact) {
        this.artifact = artifact;
    }

    public void setNonFinal(boolean nonFinal) {
        this.nonFinal = nonFinal;
    }

    public void setStandaloneImpl(boolean standaloneImpl) {
        this.standaloneImpl = standaloneImpl;
    }

    public void setMetadata(Metadata metadata) {
        this.metadata = metadata;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if(nonFinal == null || standaloneImpl == null){
            return sb.toString();
        }
        sb.append("[ ");
        if (apiPackage!= null && !apiPackage.isEmpty()) {
            sb.append("apiPackage=");
            sb.append(apiPackage);
            sb.append(", ");
        }
        if (specVersion!= null && !specVersion.isEmpty()){
            sb.append("specification version=");
            sb.append(specVersion);
            sb.append(", ");
        }
        if(standaloneImpl.booleanValue()){
            sb.append("standalone impl, ");
            sb.append("implementation-namespace=");
            sb.append(implNamespace);
            sb.append(", ");            
            if(nonFinal){
                sb.append("non final, ");
                sb.append("new-specification-version=");
                sb.append(newSpecVersion);
                sb.append(", ");
                sb.append("new-implementation-version=");
                sb.append(newSpecVersion);
                sb.append(", ");
                sb.append("implementation-build=");
                sb.append(implBuild);
            } else {
                sb.append("final, ");
            }
            sb.append("implementation-version=");
            sb.append(implVersion);
        } else {
            sb.append("API, ");
            if(nonFinal){
                sb.append("non final, ");
                sb.append("new-specification-version=");
                sb.append(newSpecVersion);
                sb.append(", ");
                sb.append("specification-build=");
                sb.append(specBuild);        
                sb.append(", ");        
            } else {
                sb.append("final, ");
            }
            sb.append("specification-implementation-version=");
            sb.append(specImplVersion);
        }
        sb.append(" ]");
        return sb.toString();
    }
}