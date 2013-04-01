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
import java.io.InputStream;
import java.util.Properties;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

/**
 *
 * @author Romain Grecourt
 */
public final class Metadata {

    private String bundleSymbolicName;
    private String bundleSpecVersion;
    private String bundleVersion;
    private String jarExtensionName;
    private String jarSpecificationVersion;
    private String jarImplementationVersion;
    private Properties properties;
    
    public static final String BUNDLE_SYMBOLIC_NAME = "Bundle-SymbolicName";
    public static final String BUNDLE_SPEC_VERSION = "BundleSpecVersion";
    public static final String BUNDLE_VERSION = "Bundle-Version";
    public static final String JAR_EXTENSION_NAME = "Extension-Name";
    public static final String JAR_SPECIFICATION_VERSION = "Specification-Version";
    public static final String JAR_IMPLEMENTATION_VERSION = "Implementation-Version";
    private static final String NONFINAL_BUILD_SEPARATOR_SPEC = ".99.";
    private static final String NONFINAL_BUILD_SEPARATOR = NONFINAL_BUILD_SEPARATOR_SPEC+"b";
    
    private static final String[] KEYS = {
        BUNDLE_SYMBOLIC_NAME,
        BUNDLE_SPEC_VERSION,
        BUNDLE_VERSION,
        JAR_EXTENSION_NAME,
        JAR_SPECIFICATION_VERSION,
        JAR_IMPLEMENTATION_VERSION};

    private Metadata(
            String bundleSymbolicName,
            String bundleSpecVersion,
            String bundleVersion,
            String jarExtensionName,
            String jarSpecificationVersion,
            String jarImplementationVersion) {  

        this.bundleSymbolicName = bundleSymbolicName;
        this.bundleSpecVersion = bundleSpecVersion;
        this.bundleVersion = bundleVersion;
        this.jarExtensionName = jarExtensionName;
        this.jarSpecificationVersion = jarSpecificationVersion;
        this.jarImplementationVersion = jarImplementationVersion;

        this.properties = new Properties();
        properties.put("spec.bundle.symbolic-name", bundleSymbolicName);
        properties.put("spec.bundle.spec.version", "");
        properties.put("spec.bundle.version", bundleVersion);
        properties.put("spec.extension.name", jarExtensionName);
        properties.put("spec.specification.version", jarSpecificationVersion);
        properties.put("spec.implementation.version", jarImplementationVersion);
    }

    public void verify() {
        boolean isAPI = false;
        if (bundleSymbolicName.contains(Artifact.API_SUFFIX)) {
            isAPI = true;
        }
        boolean isFinal = true;
        if (bundleVersion.contains(NONFINAL_BUILD_SEPARATOR)) {
            isFinal = false;
        }

        ComplianceException cex = new ComplianceException();
        String apiPackage = jarExtensionName;
        Spec.checkApiPackage(apiPackage,cex);

        if (isAPI) {
            //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
            if (!apiPackage.equals(
                    bundleSymbolicName.substring(0,
                    bundleSymbolicName.lastIndexOf(Artifact.API_SUFFIX)))) {
                cex.addBreaker(BUNDLE_SYMBOLIC_NAME+" should be"
                        + " \"" + apiPackage + Artifact.API_SUFFIX + "\"");
            }
        } else {
            //  OSGi Bundle-SymbolicName:	${IMPL_NAMESPACE}.${API_PACKAGE}
            if (!bundleSymbolicName.contains("." + apiPackage)) {
                cex.addBreaker(BUNDLE_SYMBOLIC_NAME + " (" + bundleSymbolicName + ") "
                        + "should end with \"."+ apiPackage + "\"");
            }
        }

        if (isFinal) {
            //  OSGi Bundle-Version:    ${SPEC_IMPL_VERSION}
            //  jar Implementation-Version:	${SPEC_IMPL_VERSION}
            if (!bundleVersion.equals(jarImplementationVersion)) {
                cex.addBreaker(BUNDLE_VERSION
                        + " (" + bundleVersion + ") should be equal"
                        + " to "+JAR_IMPLEMENTATION_VERSION
                        + " (" + jarImplementationVersion + ")");
            }

            // Specification-Version:   ${SPEC_VERSION}
            Spec.checkSpecVersion(jarSpecificationVersion, cex);
        } else {
            // Specification-Version can't use '.b' for the build number
            if (jarSpecificationVersion.contains(".b")) {
                cex.addBreaker(JAR_SPECIFICATION_VERSION
                        +" cannot contain '.b': "+ jarSpecificationVersion);
            }

            // Specification-Version and Bundle-Version should be similar
            // however one uses .b. as a separator, the other does not
            if (!bundleVersion.replace(
                    NONFINAL_BUILD_SEPARATOR,
                    NONFINAL_BUILD_SEPARATOR_SPEC).equals(jarSpecificationVersion)){
                cex.addBreaker(BUNDLE_VERSION+" and "
                        + JAR_SPECIFICATION_VERSION+" mismatch");
            }

            //  jar Specification-Version:	${SPEC_VERSION}.99.${SPEC_BUILD}
            int idx = jarSpecificationVersion.lastIndexOf(NONFINAL_BUILD_SEPARATOR_SPEC);
            Spec.checkSpecVersion(jarSpecificationVersion.substring(0, idx), cex);

            String buildNumberJarSpec = jarSpecificationVersion.substring(idx);
            if (isAPI) {
                // verify that build number is the same
                String buildNumberbundleV = bundleVersion.substring(idx);
                if (!buildNumberbundleV.equals(buildNumberJarSpec)) {
                    cex.addBreaker(BUNDLE_VERSION + " and " + JAR_SPECIFICATION_VERSION
                            + " should use the same build number: "
                            + buildNumberbundleV + " != " + buildNumberJarSpec);
                }
            }
        }
        
        if(!cex.isCompliant()){
            throw cex;
        }
    }

    public static Metadata fromJar(JarFile jar) throws IOException {
        ZipEntry e = jar.getEntry("META-INF/MANIFEST.MF");
        InputStream is = jar.getInputStream(e);
        Manifest manifest = new Manifest(is);

        ComplianceException cex = new ComplianceException();
        
        String[] mdata = new String[KEYS.length];
        for (int i=0 ; i<KEYS.length ; i++) {
            // bundleSpecVersion does not appear as an explicit MANIFEST entry
            if (KEYS[i].equals(BUNDLE_SPEC_VERSION)){
                continue;
            }
            mdata[i] = manifest.getMainAttributes().getValue(KEYS[i]);
            if (mdata[i] == null) {
                cex.addBreaker(KEYS[i]+" not found in MANIFEST");
            }
        }
        if(!cex.isCompliant()){
            throw cex;
        }
        return new Metadata(mdata[0],mdata[1],mdata[2],mdata[3],mdata[4],mdata[5]);
    }

    public static Metadata generate(
            Artifact artifact,
            String specVersion,
            String newVersion,
            String implVersion) {

        if(artifact == null){
            throw new IllegalArgumentException("artifact can't be null!");
        }
        if(specVersion == null || specVersion.isEmpty()){
            throw new IllegalArgumentException("specVersion can't be null or empty!");
        }
        
        Metadata metadata = null;
        ComplianceException cex = new ComplianceException();
        
        try {
            if (artifact.isFinal()) {
                if (artifact.isAPI()) {
                    //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
                    //  OSGi bundle specversion:	${SPEC_VERSION}
                    //  OSGi Bundle-Version:	${SPEC_IMPL_VERSION}
                    //  jar Extension-Name:		${API_PACKAGE}
                    //  jar Specification-Version:	${SPEC_VERSION}
                    //  jar Implementation-Version:	${SPEC_IMPL_VERSION}

                    metadata = new Metadata(
                            artifact.getApiPackage() + Artifact.API_SUFFIX,
                            specVersion,
                            implVersion,
                            artifact.getApiPackage(),
                            specVersion,
                            implVersion);

                } else {
                    //  OSGi Bundle-SymbolicName:	${IMPL_NAMESPACE}.${API_PACKAGE}
                    //  OSGi bundle specversion:	${SPEC_VERSION}
                    //  OSGi Bundle-Version:	${IMPL_VERSION}
                    //  jar Extension-Name:		${API_PACKAGE}
                    //  jar Specification-Version:	${SPEC_VERSION}
                    //  jar Implementation-Version:	${IMPL_VERSION}

                    metadata = new Metadata(
                            artifact.getImplNamespace() + "." + artifact.getApiPackage(),
                            specVersion,
                            implVersion,
                            artifact.getApiPackage(),
                            specVersion,
                            implVersion);
                }
            } else {
                if(specVersion.equals(newVersion)){
                    // specVersion must be != newVersion
                    cex.addBreaker("specVersion and newVersion can't be equal for non final artifacts");
                } else {
                    ArtifactVersion specV = new DefaultArtifactVersion(specVersion);
                    ArtifactVersion newV = new DefaultArtifactVersion(newVersion);
                    if(specV.compareTo(newV) > 0){
                        // specVersion must be < to new Version
                        cex.addBreaker("specVersion ("+specVersion+") > newVersion("+newVersion+")");
                    } else {
                        if(newV.getMajorVersion() - specV.getMajorVersion() > 1
                                || newV.getMinorVersion() - specV.getMinorVersion() > 1){
                            // offset between major and minor can't be > 1
                            cex.addBreaker("offset between major and minor can't be > 1 (specVersion="+specVersion+" - newVersion="+newVersion+")");
                        }
                    }
                }
                
                String osgiVersionSuffix = NONFINAL_BUILD_SEPARATOR + artifact.getBuildNumber();
                String jarSpecVersion = specVersion + NONFINAL_BUILD_SEPARATOR_SPEC + artifact.getBuildNumber();
                String jarImplVersion = newVersion + "-b" + artifact.getBuildNumber();

                if (artifact.isAPI()) {
                    //  OSGi Bundle-SymbolicName:	${API_PACKAGE}-api
                    //  OSGi bundle specversion:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                    //  OSGi Bundle-Version:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                    //  jar Extension-Name:		${API_PACKAGE}
                    //  jar Specification-Version:	${SPEC_VERSION}.99.${SPEC_BUILD}
                    //  jar Implementation-Version:	${NEW_SPEC_VERSION}-b${SPEC_BUILD}

                    metadata = new Metadata(
                            artifact.getApiPackage() + Artifact.API_SUFFIX,
                            specVersion + osgiVersionSuffix,
                            specVersion + osgiVersionSuffix,
                            artifact.getApiPackage(),
                            jarSpecVersion,
                            jarImplVersion);
                } else {
                    //  OSGi Bundle-SymbolicName:	${IMPL_NAMESPACE}.${API_PACKAGE}
                    //  OSGi bundle specversion:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                    //  OSGi Bundle-Version:	${OSGI_IMPL_VERSION}.99.b${IMPL_BUILD}
                    //  jar Extension-Name:		${API_PACKAGE}
                    //  jar Specification-Version:	${SPEC_VERSION}.99.${SPEC_BUILD}
                    //  jar Implementation-Version:	${NEW_IMPL_VERSION}-b${IMPL_BUILD}

                    metadata = new Metadata(
                            artifact.getImplNamespace() + "." + artifact.getApiPackage(),
                            specVersion + NONFINAL_BUILD_SEPARATOR + artifact.getBuildNumber(),
                            implVersion + NONFINAL_BUILD_SEPARATOR + artifact.getBuildNumber(),
                            artifact.getApiPackage(),
                            specVersion + NONFINAL_BUILD_SEPARATOR_SPEC + artifact.getBuildNumber(),
                            newVersion + "-b" + artifact.getBuildNumber());
                }
            }
        } catch (ComplianceException ex) {
            cex.addBreaker(ex);
        }

        if (!cex.isCompliant()) {
            throw cex;
        }
        return metadata;
    }

    public String getBundleSymbolicName() {
        return bundleSymbolicName;
    }

    public String getBundleSpecVersion() {
        return bundleSpecVersion;
    }

    public String getBundleVersion() {
        return bundleVersion;
    }

    public String getJarExtensionName() {
        return jarExtensionName;
    }

    public String getJarSpecificationVersion() {
        return jarSpecificationVersion;
    }

    public String getjarImplementationVersion() {
        return jarImplementationVersion;
    }

    public Properties getProperties() {
        return properties;
    }
}