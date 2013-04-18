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
    private boolean nonFinal = false;
    private JarType jarType = JarType.api;
    
    public static enum JarType {
        api,
        impl
    }
    
    private List<String> errors = new LinkedList<String>();
    private static final String NONFINAL_BUILD_SEPARATOR_SPEC = ".99.";
    private static final String NONFINAL_BUILD_SEPARATOR = NONFINAL_BUILD_SEPARATOR_SPEC + "b";
    private static final String JCP_VERSION_RULE = "JCP spec version number must be of the form <major>.<minor>";
    public static final String API_SUFFIX = "-api";

    public Spec() {
        
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
                    errors.add(String.format(
                            "ERROR: jar file includes class in wrong package (%s)",
                            name));
                }
            }
        }
    }

    public void verify() {
        this.errors.clear();
        this.errors.addAll(getMetadata().getErrors());
        
        StringBuilder configIssues = new StringBuilder();
        if(specVersion == null || specVersion.isEmpty()){
            configIssues.append(" spec-version");
        }
        if(apiPackage == null || apiPackage.isEmpty()){
            configIssues.append(" api-package");
        }
        if(nonFinal && (newSpecVersion == null || newSpecVersion.isEmpty())){
            configIssues.append(" new-spec-version");
        }
        if (jarType.equals(JarType.impl)) {
            if (implNamespace == null || implNamespace.isEmpty()) {
                configIssues.append(" impl-namespace");
            }
            if (implVersion == null || implVersion.isEmpty()) {
                configIssues.append(" impl-version");
            }
            if (nonFinal && (newImplVersion == null || newImplVersion.isEmpty())){
                configIssues.append(" new-impl-version");
            }
        } else if (!nonFinal){
            if (specImplVersion == null || specImplVersion.isEmpty()) {
                configIssues.append(" spec-impl-version");
            }
        }
        
        // no need to continue further...
        if(configIssues.length() > 0){
            configIssues.insert(0, "ERROR: missing configuration (");
            configIssues.append(" )");
            errors.add(configIssues.toString());
            return;
        }
        
        // verify that specVersion is <major>.<minor>
        if (!specVersion.matches("[0-9]+\\.[0-9]+")) {
            errors.add(String.format(
                    "WARNING: spec-version (%s) is invalid, %s",
                    specVersion,
                    JCP_VERSION_RULE));
        }

        // verify that Implementation-Version == Maven-Version
        if (!getMetadata().getjarImplementationVersion().isEmpty()
                && !getMetadata().getjarImplementationVersion()
                .equals(artifact.getAbsoluteVersion())) {
            errors.add(String.format(
                    "WARNING: %s (%s) should be equal to Maven-Version (%s)",
                    Metadata.JAR_IMPLEMENTATION_VERSION,
                    getMetadata().getjarImplementationVersion(),
                    artifact.getAbsoluteVersion()));
        }
        
        // verify that Extension-Name == apiPackage
        if (!getMetadata().getJarExtensionName().equals(apiPackage)) {
            errors.add(String.format(
                    "WARNING: %s (%s) should be %s",
                    Metadata.JAR_EXTENSION_NAME,
                    getMetadata().getJarExtensionName(),
                    apiPackage));
        }

        if (!nonFinal) {
            // verify Bundle-Version
            if (!getMetadata().getBundleVersion()
                    .equals(artifact.getAbsoluteVersion())) {
                errors.add(String.format(
                        "WARNING: %s (%s) should be %s",
                        Metadata.BUNDLE_VERSION,
                        metadata.getBundleVersion(),
                        artifact.getAbsoluteVersion()));
            }
            
            if (!getMetadata().getJarSpecificationVersion().equals(specVersion)){
                errors.add(String.format(
                        "WARNING: %s (%s) should be %s",
                        Metadata.JAR_SPECIFICATION_VERSION,
                        metadata.getJarSpecificationVersion(),
                        specVersion));
            }

            // TODO check BundleSpecVersion == JarSpec
        } else {
            
            // verify Bundle-Version
            ArtifactVersion av = new DefaultArtifactVersion(specVersion);
            String bundleVersion = av.getMajorVersion()
                    + "."
                    + av.getMinorVersion()
                    + NONFINAL_BUILD_SEPARATOR
                    + (jarType.equals(JarType.impl) ? implBuild : specBuild);

            if (!getMetadata().getBundleVersion().equals(bundleVersion)) {
                errors.add(String.format(
                        "WARNING: %s (%s) should be %s",
                        Metadata.BUNDLE_VERSION,
                        metadata.getBundleVersion(),
                        bundleVersion));
            }
            
            String expectedJarSpecVersion = 
                    specVersion+NONFINAL_BUILD_SEPARATOR_SPEC+specBuild;
            if (!getMetadata().getJarSpecificationVersion().equals(
                    expectedJarSpecVersion)) {
                errors.add(String.format(
                        "WARNING: %s (%s) should be %s",
                        Metadata.JAR_SPECIFICATION_VERSION,
                        metadata.getJarSpecificationVersion(),
                        expectedJarSpecVersion));
            }

            // TODO check BundleSpecVersion == JarSpec (with no b)
        }

        if (jarType.equals(JarType.api)) {
            // verify that groupId starts with javax.
            if (!artifact.getGroupId().startsWith("javax.")) {
                errors.add(String.format(
                        "WARNING: groupId (%s) must start with \"javax\"",
                        artifact.getGroupId()));
            }

            // verify that artifactId does end with -api
            if (!artifact.getArtifactId().endsWith(API_SUFFIX)) {
                errors.add(String.format(
                        "WARNING: artifactId (%s) should en with %s",
                        artifact.getArtifactId(),
                        API_SUFFIX));
            }

            // verify that apiPackage starts with javax.
            if (!apiPackage.startsWith("javax.")) {
                errors.add(String.format(
                        "WARNING: API packages (%s) must start with \"javax\"",
                        apiPackage));
            }

            // verify that Bundle-SymbolicName == apiPackage-api
            String symbolicName = apiPackage.concat(API_SUFFIX);
            
            if (!getMetadata().getBundleSymbolicName().isEmpty()
                    && !symbolicName.equals(getMetadata().getBundleSymbolicName())) {
                errors.add(String.format(
                        "WARNING: %s (%s) should be %s",
                        Metadata.BUNDLE_SYMBOLIC_NAME,
                        getMetadata().getBundleSymbolicName(),
                        symbolicName));
            }

            if (jar != null) {
                checkClasses(jar, apiPackage);
            }
            
            if (nonFinal) {
                // verify new spec version
                if (!newSpecVersion.matches("[0-9]+\\.[0-9]+")) {
                    errors.add(String.format(
                            "WARNING: new-spec-version (%s) is invalid, %s",
                            newSpecVersion,
                            JCP_VERSION_RULE));
                }
                
                // verify that specVersion != newSpecVersion
                if (specVersion.equals(newSpecVersion)) {
                    errors.add(String.format(
                            "WARNING: spec-version (%s) can't be equal to new-spec-version (%s) for non final artifacts",
                            specVersion,
                            newSpecVersion));
                } else {
                    ArtifactVersion specAV = new DefaultArtifactVersion(specVersion);
                    ArtifactVersion newSpecAV = new DefaultArtifactVersion(newSpecVersion);

                    // verify that specVersion < newSpecVersion
                    if (specAV.compareTo(newSpecAV) > 0) {
                        errors.add(String.format(
                                "WARNING: new-spec-version (%s) must be greater than spec-version (%s)",
                                newSpecVersion,
                                specVersion));
                    } else {
                        // verify offset between specVersion and newSpecVersion
                        if (newSpecAV.getMajorVersion() - specAV.getMajorVersion() > 1
                                || newSpecAV.getMinorVersion() - specAV.getMinorVersion() > 1) {
                            errors.add(String.format(
                                    "WARNING offset between new-spec-version (%s) and spec-version (%s) can't be greater than 1",
                                    newSpecVersion,
                                    specVersion));
                        }
                    }
                }
            } else {
                // verify that implementation version starts with spec version
                if (!(specImplVersion.equals(specVersion)
                        || specImplVersion.startsWith(specVersion + ".")
                        || specImplVersion.startsWith(specVersion + "-"))) {
                    errors.add(String.format(
                            "WARNING: spec-impl-version (%s) must start with JCP spec-version number (%s)",
                            specImplVersion,
                            specVersion));
                }
            }
        } else {
            // verify that groupId starts with javax.
            if (artifact.getGroupId().startsWith("javax.")) {
                errors.add(String.format(
                        "WARNING: groupId (%s) should not start with \"javax.\"",
                        artifact.getGroupId()));
            }

            // verify that artifactId does not end with -api
            if (artifact.getArtifactId().endsWith(API_SUFFIX)) {
                errors.add(String.format(
                        "WARNING: artifactId (%s) should not end with %s",
                        artifact.getArtifactId(),
                        API_SUFFIX));
            }

            // verify that apiPackage starts with javax.
            if (!apiPackage.startsWith("javax.")) {
                errors.add(String.format(
                        "WARNING: API packages (%s) must start with \"javax\"",
                        apiPackage));
            }

            // verify that Bundle-SymbolicName == implNamespace.apiPackage
            String symbolicName = implNamespace + '.' + apiPackage;

            if (!getMetadata().getBundleSymbolicName()
                    .equals(symbolicName)) {
                errors.add(String.format(
                        "WARNING: %s (%s) should be %s",
                        Metadata.BUNDLE_SYMBOLIC_NAME,
                        getMetadata().getBundleSymbolicName(),
                        symbolicName));
            }

            if (jar != null) {
                checkClasses(jar, apiPackage, implNamespace);
            }

            if (nonFinal) {
                // verify that implVersion != newImplVersion
                if (implVersion.equals(newImplVersion)) {
                    errors.add(String.format(
                            "WARNING: impl-version (%s) can't be equal to new-impl-version (%s) for non final artifacts",
                            implVersion,
                            newImplVersion));
                } else {
                    ArtifactVersion implAV = new DefaultArtifactVersion(implVersion);
                    ArtifactVersion newImplAV = new DefaultArtifactVersion(newImplVersion);

                    // verify that implVersion < newImplVersion
                    if (implAV.compareTo(newImplAV) > 0) {
                        errors.add(String.format(
                                "WARNING: new-impl-version (%s) must be greater than impl-version (%s)",
                                newImplVersion,
                                implVersion));
                    } else {
                        // verify offset between implVersion and newImplVersion
                        if (newImplAV.getMajorVersion() - implAV.getMajorVersion() > 1
                                || newImplAV.getMinorVersion() - implAV.getMinorVersion() > 1) {

                            errors.add(String.format(
                                    "WARNING: offset between new-impl-version (%s) and impl-version (%s) can't be greater than 1",
                                    newImplVersion,
                                    implVersion));
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

        if (jarType.equals(JarType.api)) {
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
                
                String osgiVersion = 
                        specVersion + NONFINAL_BUILD_SEPARATOR + specBuild;
                metadata = new Metadata(
                        apiPackage + Spec.API_SUFFIX,
                        osgiVersion,
                        osgiVersion,
                        apiPackage,
                        specVersion + NONFINAL_BUILD_SEPARATOR_SPEC + specBuild,
                        artifact.getAbsoluteVersion());
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
                        artifact.getAbsoluteVersion());
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
                        specVersion + NONFINAL_BUILD_SEPARATOR + implBuild,
                        implAv.getMajorVersion() + "." + implAv.getMinorVersion() + NONFINAL_BUILD_SEPARATOR + implBuild,
                        apiPackage,
                        specVersion + NONFINAL_BUILD_SEPARATOR_SPEC + implBuild,
                        artifact.getAbsoluteVersion());
            }
        }
        return metadata;
    }

    public List<String> getErrors() {
        return errors;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage != null ? apiPackage : "";
    }

    public void setImplNamespace(String implNamespace) {
        this.implNamespace = implNamespace != null ? implNamespace : "";
    }

    public void setImplVersion(String implVersion) {
        this.implVersion = implVersion != null ? implVersion : "";
    }

    public void setSpecVersion(String specVersion) {
        this.specVersion = specVersion != null ? specVersion : "";
    }

    public void setNewImplVersion(String newImplVersion) {
        this.newImplVersion = newImplVersion != null ? newImplVersion : "";
    }

    public void setSpecBuild(String specBuild) {
        this.specBuild = specBuild != null ? specBuild : "";
    }

    public void setSpecImplVersion(String specImplVersion) {
        this.specImplVersion = specImplVersion != null ? specImplVersion : "";
    }

    public void setNewSpecVersion(String newSpecVersion) {
        this.newSpecVersion = newSpecVersion != null ? newSpecVersion : "";
    }

    public void setImplBuild(String implBuild) {
        this.implBuild = implBuild != null ? implBuild : "";
    }

    public void setArtifact(Artifact artifact) {
        this.artifact = artifact;
    }

    public void setNonFinal(boolean nonFinal) {
        this.nonFinal = nonFinal;
    }

    public void setJarType(String jarType) {
        this.jarType = JarType.valueOf(jarType);
    }

    public void setMetadata(Metadata metadata) {
        this.metadata = metadata;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if(jarType == null){
            return sb.toString();
        }
        sb.append("{");
        if (specVersion!= null && !specVersion.isEmpty()){
            sb.append(" spec-version=");
            sb.append(specVersion);
        }        
        if (apiPackage!= null && !apiPackage.isEmpty()) {
            sb.append(" apiPackage=");
            sb.append(apiPackage);
        }
        if(jarType.equals(JarType.impl)){
            sb.append(" standalone-impl");
            sb.append(" impl-namespace=");
            sb.append(implNamespace);
            if(nonFinal){
                sb.append(" non-final");
                sb.append(" new-spec-version=");
                sb.append(newSpecVersion);
                sb.append(" new-impl-version=");
                sb.append(newSpecVersion);
                sb.append(" impl-build=");
                sb.append(implBuild);
            } else {
                sb.append(" final");
            }
            sb.append(" impl-version=");
            sb.append(implVersion);
        } else {
            sb.append(" API");
            if(nonFinal){
                sb.append(" non-final");
                sb.append(" new-spec-version=");
                sb.append(newSpecVersion);
                sb.append(" spec-build=");
                sb.append(specBuild);        
            } else {
                sb.append(" final");
                sb.append(" spec-impl-version=");
                sb.append(specImplVersion);
            }
        }
        sb.append(" }");
        return sb.toString();
    }
}