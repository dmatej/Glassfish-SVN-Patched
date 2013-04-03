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
public final class Spec {

    private Artifact artifact;
    private Metadata metadata;
    private JarFile jar;
    private String specVersion;
    private String newVersion;
    private String implVersion;
    private String apiPackage;
    private String implNamespace;
    private List<String> errors = new LinkedList<String>();
    private static final String NONFINAL_BUILD_SEPARATOR_SPEC = ".99.";
    private static final String NONFINAL_BUILD_SEPARATOR = NONFINAL_BUILD_SEPARATOR_SPEC + "b";
    public static final String API_SUFFIX = "-api";

    public Spec(JarFile _jar) throws IOException {
        this.jar = _jar;
        this.artifact = Artifact.fromJar(jar);
        this.metadata = Metadata.fromJar(jar);
        
        if(artifact.isAPI()){
            this.apiPackage = artifact.getGroupId();
        } else {
            this.implNamespace = artifact.getGroupId();
            this.apiPackage = artifact.getArtifactId();
        }
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

        if(artifact.isAPI()){
            this.apiPackage = artifact.getGroupId();
        } else {
            this.implNamespace = artifact.getGroupId();
            this.apiPackage = artifact.getArtifactId();
        }
    }
    
    public void resolve() {
        if (artifact.isFinal()) {
            if (!metadata.getJarSpecificationVersion().isEmpty()) {
                this.specVersion = metadata.getJarSpecificationVersion();
            }
            this.implVersion = artifact.getVersion().toString();
        } else {
            // resolve specVersion
            if (!metadata.getJarSpecificationVersion().isEmpty()) {

                if (metadata.getJarSpecificationVersion()
                        .contains(NONFINAL_BUILD_SEPARATOR_SPEC)) {

                    int idx = metadata.getJarSpecificationVersion()
                            .lastIndexOf(NONFINAL_BUILD_SEPARATOR_SPEC);
                    this.specVersion =
                            metadata.getJarSpecificationVersion().substring(0, idx);
                } else {
                    this.specVersion = metadata.getJarSpecificationVersion();
                }
            } else {
                // if specification-version is not empty, lets try
                this.specVersion = null;
            }

            // implVersion == Maven-Version
            this.implVersion = artifact.getVersion().toString();

            // resolve newVersion from Maven-Version
            this.newVersion = implVersion.substring(0, implVersion.indexOf('-'));
        }
    }

    private static StringBuilder keyAndValue(String key, String value) {
        return new StringBuilder(key).append(" (").append(value).append(") ");
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
                    StringBuilder sb = new StringBuilder("jar file includes class in wrong ");
                    sb.append(keyAndValue("package", name));
                    errors.add(sb.toString());
                }
            }
        }
    }

    public void verify() {
        this.errors.clear();
        this.errors.addAll(getMetadata().getErrors());
        
        // verification stoppers
        if(specVersion == null){
            errors.add("no specification-version, stopping verification");
            return;
        }
        if(apiPackage == null){
            errors.add("no api-package, stopping verification");
            return;
        }
        if (!artifact.isAPI()) {
            if (implNamespace == null) {
                errors.add("no implementation namespace, stopping verification");
                return;
            }
            if (implVersion == null) {
                errors.add("no implementation-version, stopping verification");
                return;
            }
        }
        if (!artifact.isFinal() && newVersion == null) {
            errors.add("no implementation-version, stopping verification");
            return;
        }
        
        // verify that specVersion is <major>.<minor>
        if (!specVersion.matches("[0-9]+\\.[0-9]+")) {
            StringBuilder sb =
                    keyAndValue("specification version", specVersion);
            sb.append("is invalid, JCP specification version number ");
            sb.append("must be of the form <major>.<minor>");
            errors.add(sb.toString());
        }

        // verify that Implementation-Version == Maven-Version
        if (!getMetadata().getjarImplementationVersion()
                .equals(artifact.getVersion().toString())) {
            StringBuilder sb = keyAndValue(
                    Metadata.JAR_IMPLEMENTATION_VERSION,
                    getMetadata().getjarImplementationVersion());
            sb.append("should be equal to ");
            sb.append(keyAndValue(
                    "Maven-Version",
                    artifact.getVersion().toString()));
            errors.add(sb.toString());
        }

        if (artifact.isFinal()) {
            // verify Bundle-Version
            if (!getMetadata().getBundleVersion()
                    .equals(artifact.getVersion().toString())) {
                StringBuilder sb = new StringBuilder();
                sb.append(keyAndValue(
                        Metadata.BUNDLE_VERSION,
                        metadata.getBundleVersion()));
                sb.append("should be ");
                sb.append(artifact.getVersion().toString());
                errors.add(sb.toString());
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
                    .append(artifact.getBuildNumber())
                    .toString();

            if (!getMetadata().getBundleVersion().equals(bundleVersion)) {
                StringBuilder sb = new StringBuilder();
                sb.append(keyAndValue(
                        Metadata.BUNDLE_VERSION,
                        metadata.getBundleVersion()));
                sb.append("should be ");
                sb.append(bundleVersion);
                errors.add(sb.toString());
            }

            // TODO check BundleSpecVersion == JarSpec (with no b)
        }

        if (artifact.isAPI()) {
            // verify that groupId starts with javax.
            if (!artifact.getGroupId().startsWith("javax.")) {
                StringBuilder sb =
                        keyAndValue("groupId", artifact.getGroupId());
                sb.append("must start with \"javax.\"");
                errors.add(sb.toString());
            }

            // verify that artifactId does end with -api
            if (!artifact.getArtifactId().endsWith(API_SUFFIX)) {
                StringBuilder sb =
                        keyAndValue("artifactId", artifact.getArtifactId());
                sb.append(" should end with ");
                sb.append(API_SUFFIX);
                errors.add(sb.toString());
            }

            // verify that apiPackage starts with javax.
            if (!apiPackage.startsWith("javax.")) {
                StringBuilder sb = keyAndValue("API packages", apiPackage);
                sb.append("must start with \"javax.\"");
                errors.add(sb.toString());
            }

            // verify that Bundle-SymbolicName == implNamespace.apiPackage
            String symbolicName =
                    new StringBuilder(apiPackage)
                    .append(API_SUFFIX)
                    .toString();
            if (!symbolicName.equals(getMetadata().getBundleSymbolicName())) {
                StringBuilder sb =
                        keyAndValue(
                        Metadata.BUNDLE_SYMBOLIC_NAME,
                        apiPackage);
                sb.append("should be ");
                sb.append(symbolicName);
                errors.add(sb.toString());
            }

            // verify that Extension-Name == apiPackage
            if (!getMetadata().getJarExtensionName().equals(apiPackage)) {
                StringBuilder sb =
                        keyAndValue(
                        Metadata.JAR_EXTENSION_NAME,
                        apiPackage);
                sb.append("should be ");
                sb.append(apiPackage);
                errors.add(sb.toString());
            }

            if (jar != null) {
                checkClasses(jar, apiPackage);
            }
            
            // verify that implementation version starts with spec version
            String sv = artifact.isFinal() ? specVersion : newVersion;
            if (!(implVersion.equals(sv)
                    || implVersion.startsWith(sv + ".")
                    || implVersion.startsWith(sv + "-"))) {
                StringBuilder sb =
                        keyAndValue("spec-implementation-version", implVersion);
                sb.append("must start with");
                sb.append(keyAndValue("JCP specification version number", sv));
                errors.add(sb.toString());
            }            

            if (!artifact.isFinal()) {
                // verify new spec version
                if (!newVersion.matches("[0-9]+\\.[0-9]+")) {
                    StringBuilder sb =
                            keyAndValue("new-specification-version", specVersion);
                    sb.append("is invalid, JCP specification version number ");
                    sb.append("must be of the form <major>.<minor>");
                    errors.add(sb.toString());
                }
                
                // verify that specVersion != newSpecVersion
                if (specVersion.equals(newVersion)) {
                    StringBuilder sb = keyAndValue("specification-version", specVersion);
                    sb.append(" can't be equal to ");
                    sb.append(keyAndValue("new specification-version", newVersion));
                    sb.append("for non final artifacts");
                    errors.add(sb.toString());
                } else {
                    ArtifactVersion specAV = new DefaultArtifactVersion(specVersion);
                    ArtifactVersion newSpecAV = new DefaultArtifactVersion(newVersion);

                    // verify that specVersion < newSpecVersion
                    if (specAV.compareTo(newSpecAV) > 0) {
                        StringBuilder sb = keyAndValue("new-specification-version", newVersion);
                        sb.append("must be greater than ");
                        sb.append(keyAndValue("specification-version", specVersion));
                        errors.add(sb.toString());
                    } else {
                        // verify offset between specVersion and newSpecVersion
                        if (newSpecAV.getMajorVersion() - specAV.getMajorVersion() > 1
                                || newSpecAV.getMinorVersion() - specAV.getMinorVersion() > 1) {

                            StringBuilder sb = new StringBuilder("offset between ");
                            sb.append(keyAndValue("new-specification-version", newVersion));
                            sb.append("and ");
                            sb.append(keyAndValue("specification-version", specVersion));
                            sb.append("can't be greater than 1");
                            errors.add(sb.toString());
                        }
                    }
                }
            }
        } else {
            // verify that groupId starts with javax.
            if (artifact.getGroupId().startsWith("javax.")) {
                StringBuilder sb =
                        keyAndValue("groupId", artifact.getGroupId());
                sb.append("should not start with \"javax.\"");
                errors.add(sb.toString());
            }

            // verify that artifactId does not end with -api
            if (artifact.getArtifactId().endsWith(API_SUFFIX)) {
                StringBuilder sb =
                        keyAndValue("artifactId", artifact.getArtifactId());
                sb.append("should not end with ");
                sb.append(API_SUFFIX);
                errors.add(sb.toString());
            }

            // verify that Extension-Name == apiPackage
            if (!getMetadata().getJarExtensionName().equals(apiPackage)) {
                StringBuilder sb =
                        keyAndValue(
                        Metadata.JAR_EXTENSION_NAME,
                        apiPackage);
                sb.append("should be ");
                sb.append(apiPackage);
                errors.add(sb.toString());
            }

            // verify that apiPackage starts with javax.
            if (!apiPackage.startsWith("javax.")) {
                StringBuilder sb = keyAndValue("API packages", apiPackage);
                sb.append("must" + "start with \"javax.\"");
                errors.add(sb.toString());
            }

            // verify that Bundle-SymbolicName == implNamespace.apiPackage
            String symbolicName =
                    new StringBuilder(implNamespace)
                    .append('.')
                    .append(apiPackage)
                    .toString();

            if (!getMetadata().getBundleSymbolicName()
                    .equals(symbolicName)) {
                StringBuilder sb =
                        keyAndValue(
                        Metadata.BUNDLE_SYMBOLIC_NAME,
                        apiPackage);
                sb.append("should be ");
                sb.append(symbolicName);
                errors.add(sb.toString());
            }

            if (jar != null) {
                checkClasses(jar, apiPackage, implNamespace);
            }

            if (!artifact.isFinal()) {
                // verify that implVersion != newImplVersion
                if (implVersion.equals(newVersion)) {
                    StringBuilder sb = keyAndValue(
                            "implementation-version",
                            implVersion);
                    sb.append(" can't be equal to");
                    sb.append(keyAndValue(
                            "new-implementation-version",
                            newVersion));
                    sb.append("for non final artifacts");
                    errors.add(sb.toString());
                } else {
                    ArtifactVersion implAV = new DefaultArtifactVersion(implVersion);
                    ArtifactVersion newImplAV = new DefaultArtifactVersion(newVersion);

                    // verify that implVersion < newImplVersion
                    if (implAV.compareTo(newImplAV) > 0) {
                        StringBuilder sb = keyAndValue(
                                "new-implementation-version",
                                newVersion);
                        sb.append("must be greater than ");
                        sb.append(keyAndValue(""
                                + "implementation-version",
                                implVersion));
                        errors.add(sb.toString());
                    } else {
                        // verify offset between implVersion and newImplVersion
                        if (newImplAV.getMajorVersion() - implAV.getMajorVersion() > 1
                                || newImplAV.getMinorVersion() - implAV.getMinorVersion() > 1) {

                            StringBuilder sb = new StringBuilder("offset between ");
                            sb.append(keyAndValue("new-implementation-version", newVersion));
                            sb.append("and ");
                            sb.append(keyAndValue("implementation-version", implVersion));
                            sb.append("can't be greater than 1");
                            errors.add(sb.toString());
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

        if (artifact.isAPI()) {
            if (artifact.isFinal()) {
                //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
                //  OSGi bundle specversion:	${SPEC_VERSION}
                //  OSGi Bundle-Version:	${SPEC_IMPL_VERSION}
                //  jar Extension-Name:		${API_PACKAGE}
                //  jar Specification-Version:	${SPEC_VERSION}
                //  jar Implementation-Version:	${SPEC_IMPL_VERSION}

                metadata = new Metadata(
                        apiPackage + Spec.API_SUFFIX,
                        specVersion,
                        implVersion,
                        apiPackage,
                        specVersion,
                        artifact.getVersion().toString());

            } else {
                //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
                //  OSGi bundle specversion:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                //  OSGi Bundle-Version:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                //  jar Extension-Name:		${API_PACKAGE}
                //  jar Specification-Version:	${SPEC_VERSION}.99.${SPEC_BUILD}
                //  jar Implementation-Version:	${NEW_SPEC_VERSION}-b${SPEC_BUILD}
                
                String osgiVersion = new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR)
                        .append(artifact.getBuildNumber())
                        .toString();
                
                String jarSpecVersion = new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR_SPEC)
                        .append(artifact.getBuildNumber())
                        .toString();

                metadata = new Metadata(
                        apiPackage + Spec.API_SUFFIX,
                        osgiVersion,
                        osgiVersion,
                        apiPackage,
                        jarSpecVersion,
                        artifact.getVersion().toString());
            }
        } else {
            String symbolicName = implNamespace + "." + apiPackage;

            if (artifact.isFinal()) {
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
                
                String osgiSpecVersion = new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR)
                        .append(artifact.getBuildNumber())
                        .toString();
                
                String osgiVersion = new StringBuilder()
                        .append(implAv.getMajorVersion())
                        .append('.')
                        .append(implAv.getMinorVersion())
                        .append(NONFINAL_BUILD_SEPARATOR)
                        .append(artifact.getBuildNumber())
                        .toString();
                
                String jarSpecVersion = new StringBuilder(specVersion)
                        .append(NONFINAL_BUILD_SEPARATOR_SPEC)
                        .append(artifact.getBuildNumber())
                        .toString();                

                metadata = new Metadata(
                        symbolicName,
                        osgiSpecVersion,
                        osgiVersion,
                        apiPackage,
                        jarSpecVersion,
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

    public void setNewVersion(String newVersion) {
        this.newVersion = newVersion;
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
}