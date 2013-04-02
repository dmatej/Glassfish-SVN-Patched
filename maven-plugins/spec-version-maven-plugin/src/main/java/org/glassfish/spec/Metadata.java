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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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
    private Properties properties = null;
    
    public static final String BUNDLE_SYMBOLIC_NAME = "Bundle-SymbolicName";
    public static final String BUNDLE_SPEC_VERSION = "BundleSpecVersion";
    public static final String BUNDLE_VERSION = "Bundle-Version";
    public static final String JAR_EXTENSION_NAME = "Extension-Name";
    public static final String JAR_SPECIFICATION_VERSION = "Specification-Version";
    public static final String JAR_IMPLEMENTATION_VERSION = "Implementation-Version";
    private static final String NONFINAL_BUILD_SEPARATOR_SPEC = ".99.";
    private static final String NONFINAL_BUILD_SEPARATOR = NONFINAL_BUILD_SEPARATOR_SPEC+"b";
    private List<String> errors = new LinkedList<String>();
    
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
            String jarImplementationVersion,
            List<String> _errors) {

        this.bundleSymbolicName = bundleSymbolicName;
        this.bundleSpecVersion = bundleSpecVersion;
        this.bundleVersion = bundleVersion;
        this.jarExtensionName = jarExtensionName;
        this.jarSpecificationVersion = jarSpecificationVersion;
        this.jarImplementationVersion = jarImplementationVersion;
        this.errors = _errors;

        this.properties = new Properties();
        properties.put("spec.bundle.symbolic-name", String.valueOf(bundleSymbolicName));
        properties.put("spec.bundle.spec.version", String.valueOf(bundleSpecVersion));
        properties.put("spec.bundle.version", String.valueOf(bundleVersion));
        properties.put("spec.extension.name", String.valueOf(jarExtensionName));
        properties.put("spec.specification.version", String.valueOf(jarSpecificationVersion));
        properties.put("spec.implementation.version", String.valueOf(jarImplementationVersion));
        
        // TODO, pass isAPI and isFinal as parameters.
    }
    
    // TODO extract exported package version
    // to use with the fromJar approach
    private static Map<String, List<String>> processOsgiHeader(String headers) {
        Map<String, List<String>> res = new HashMap<String, List<String>>();

        String[] headersTokens = headers.split(";");
        if (headersTokens.length > 1) {
            ArrayList<String> curHeader = new ArrayList<String>();
            String key = "";
            for (int i = 0; i < headersTokens.length; i++) {
                if (!(headersTokens[i].startsWith("uses:=")
                        || headersTokens[i].startsWith("version="))) {
                    key = headersTokens[i];
                } else {
                    if (headersTokens[i].startsWith("version=")) {
                        String[] lastToken = headersTokens[i].split(",");
                        curHeader.add(lastToken[0]);
                        res.put(key, new ArrayList<String>(curHeader));
                        
                        if (headersTokens[i].length() > lastToken[0].length()) {
                            key = headersTokens[i].substring(lastToken[0].length() + 1);
                            curHeader.clear();
                        }
                    } else if (headersTokens[i].startsWith("uses:=")) {
                        if (i != headersTokens.length - 1 && !headersTokens[i+1].startsWith("version=")) {
                            String[] lastToken = headersTokens[i].split(",");
                            curHeader.add(headersTokens[i].substring(0, headersTokens[i].length() - (lastToken[lastToken.length - 1].length())));
                            res.put(key, new ArrayList<String>(curHeader));

                            key = lastToken[lastToken.length - 1];
                            curHeader.clear();
                        } else {
                            curHeader.add(headersTokens[i]);
                        }
                    }
                }
            }
        } else {
            res.put(headers, Collections.EMPTY_LIST);
        }
        return res;
    }    

    public static Metadata fromJar(JarFile jar) throws IOException {
        ZipEntry e = jar.getEntry("META-INF/MANIFEST.MF");
        InputStream is = jar.getInputStream(e);
        Manifest manifest = new Manifest(is);

        List<String> errors = new LinkedList<String>();
        String[] mdata = new String[KEYS.length];
        for (int i=0 ; i<KEYS.length ; i++) {
            // bundleSpecVersion does not appear as an explicit MANIFEST entry
            if (KEYS[i].equals(BUNDLE_SPEC_VERSION)){
                continue;
            }
            mdata[i] = manifest.getMainAttributes().getValue(KEYS[i]);
            if (mdata[i] == null) {
                errors.add(KEYS[i]+" not found in MANIFEST");
            }
        }
        return new Metadata(mdata[0],mdata[1],mdata[2],mdata[3],mdata[4],mdata[5],errors);
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
        
        Metadata metadata;
        List<String> errors = artifact.getErrors();

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
                        implVersion,
                        errors);

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
                        implVersion,
                        errors);
            }
        } else {
            if (specVersion.equals(newVersion)) {
                // specVersion must be != newVersion
                errors.add("specVersion and newVersion can't be equal for non final artifacts");
            } else {
                ArtifactVersion specV = new DefaultArtifactVersion(specVersion);
                ArtifactVersion newV = new DefaultArtifactVersion(newVersion);
                if (specV.compareTo(newV) > 0) {
                    // specVersion must be < to new Version
                    errors.add("specVersion (" + specVersion + ") > newVersion(" + newVersion + ")");
                } else {
                    if (newV.getMajorVersion() - specV.getMajorVersion() > 1
                            || newV.getMinorVersion() - specV.getMinorVersion() > 1) {
                        // offset between major and minor can't be > 1
                        errors.add("offset between major and minor can't be > 1 (specVersion=" + specVersion + " - newVersion=" + newVersion + ")");
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
                        jarImplVersion,
                        errors);
            } else {
                //  OSGi Bundle-SymbolicName:	${IMPL_NAMESPACE}.${API_PACKAGE}
                //  OSGi bundle specversion:	${SPEC_VERSION}.99.b${SPEC_BUILD}
                //  OSGi Bundle-Version:	${OSGI_IMPL_VERSION}.99.b${IMPL_BUILD}
                //  jar Extension-Name:		${API_PACKAGE}
                //  jar Specification-Version:	${SPEC_VERSION}.99.${SPEC_BUILD}
                //  jar Implementation-Version:	${NEW_IMPL_VERSION}-b${IMPL_BUILD}

                ArtifactVersion implAv = new DefaultArtifactVersion(implVersion);

                metadata = new Metadata(
                        artifact.getImplNamespace() + "." + artifact.getApiPackage(),
                        specVersion + NONFINAL_BUILD_SEPARATOR + artifact.getBuildNumber(),
                        implAv.getMajorVersion() + "." + implAv.getMinorVersion() + NONFINAL_BUILD_SEPARATOR + artifact.getBuildNumber(),
                        artifact.getApiPackage(),
                        specVersion + NONFINAL_BUILD_SEPARATOR_SPEC + artifact.getBuildNumber(),
                        newVersion + "-b" + artifact.getBuildNumber(),
                        errors);
            }
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

    public List<String> getErrors() {
        return errors;
    }
}