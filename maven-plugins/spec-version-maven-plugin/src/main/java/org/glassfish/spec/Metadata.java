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
    private List<String> errors = new LinkedList<String>();
    
    private static final String[] KEYS = {
        BUNDLE_SYMBOLIC_NAME,
        BUNDLE_SPEC_VERSION,
        BUNDLE_VERSION,
        JAR_EXTENSION_NAME,
        JAR_SPECIFICATION_VERSION,
        JAR_IMPLEMENTATION_VERSION
    };

    Metadata(
            String _bundleSymbolicName,
            String _bundleSpecVersion,
            String _bundleVersion,
            String _jarExtensionName,
            String _jarSpecificationVersion,
            String _jarImplementationVersion) {

        this.bundleSymbolicName = 
                _bundleSymbolicName != null ? _bundleSymbolicName : "";
        this.bundleSpecVersion = 
                _bundleSpecVersion != null ? _bundleSpecVersion: "";
        this.bundleVersion =
                _bundleVersion != null ? _bundleVersion : "";
        this.jarExtensionName = 
                _jarExtensionName != null ? _jarExtensionName : "";
        this.jarSpecificationVersion = 
                _jarSpecificationVersion != null ? _jarSpecificationVersion : "";
        this.jarImplementationVersion = 
                _jarImplementationVersion != null ? _jarImplementationVersion : "";

        this.properties = new Properties();
        properties.put("spec.bundle.symbolic-name", bundleSymbolicName);
        properties.put("spec.bundle.spec.version", bundleSpecVersion);
        properties.put("spec.bundle.version", bundleVersion);
        properties.put("spec.extension.name", jarExtensionName);
        properties.put("spec.specification.version", jarSpecificationVersion);
        properties.put("spec.implementation.version", jarImplementationVersion);
    }
    
    Metadata(
            String _bundleSymbolicName,
            String _bundleSpecVersion,
            String _bundleVersion,
            String _jarExtensionName,
            String _jarSpecificationVersion,
            String _jarImplementationVersion,
            List<String> _errors) {
        
        this(_bundleSymbolicName,
                _bundleSpecVersion,
                _bundleVersion,
                _jarExtensionName,
                _jarSpecificationVersion,
                _jarImplementationVersion);
        this.errors = _errors;
    }
    
    // TODO extract exported package version
    // to use with the fromJar approach
    private static String getBundleSpecVersion(String headers) {
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
        return "";
    }    

    public static Metadata fromJar(JarFile jar) throws IOException {
        ZipEntry e = jar.getEntry("META-INF/MANIFEST.MF");
        InputStream is = jar.getInputStream(e);
        Manifest manifest = new Manifest(is);

        List<String> errors = new LinkedList<String>();
        String[] mdata = new String[KEYS.length];
        for (int i=0 ; i<KEYS.length ; i++) {
            if (KEYS[i].equals(BUNDLE_SPEC_VERSION)){
                // skip bundleSpecVersion
                continue;
            }
            mdata[i] = manifest.getMainAttributes().getValue(KEYS[i]);
            if (mdata[i] == null) {
                errors.add(new StringBuilder()
                        .append("ERROR: ")
                        .append(KEYS[i])
                        .append(" not found in MANIFEST")
                        .toString());
            }
        }
        
        // TODO parse exported-packages to resolve bundleSpecVersion
        return new Metadata(
                mdata[0],
                mdata[1],
                mdata[2],
                mdata[3],
                mdata[4],
                mdata[5],
                errors);
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