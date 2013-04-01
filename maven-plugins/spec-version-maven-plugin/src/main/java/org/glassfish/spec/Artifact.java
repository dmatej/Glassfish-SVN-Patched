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
import java.util.zip.ZipEntry;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

/**
 *
 * @author Romain Grecourt
 */
public class Artifact {

    private String groupId;
    private String artifactId;
    // version is specVersion
    private ArtifactVersion version;
    private String absoluteVersion;
    private String buildNumber;
    private String apiPackage;
    private String implNamespace;
    private static final String[] buildNumberSeparators = new String[] {"m", "b"};
    public static final String SNAPSHOT_QUALIFIER = "SNAPSHOT";
    public static final String API_SUFFIX = "-api";
    
    private static String stripSnapshotQualifier(String qualifier) {
        if (qualifier != null) {
            if (qualifier.endsWith("-" + SNAPSHOT_QUALIFIER)) {
                qualifier = qualifier.replace("-" + SNAPSHOT_QUALIFIER, "");
            }
            return qualifier;
        }
        return null;
    }
    
    private static String getBuildNumber(String qualifier) {
        if (qualifier != null) {
            qualifier = stripSnapshotQualifier(qualifier);
            for (String c : buildNumberSeparators) {
                if (qualifier.contains(c)) {
                    return qualifier.substring(qualifier.lastIndexOf(c) + 1);
                }
            }
        }
        return null;
    }

    private static String getAbsoluteVersion(ArtifactVersion version) {
        String absoluteV =
                version.getMajorVersion() + "." + version.getMinorVersion();
        String qualifier = stripSnapshotQualifier(version.getQualifier());
        if (qualifier != null && !qualifier.isEmpty()){
            absoluteV += "-" + qualifier;
        }
        return absoluteV;
    }

    public Artifact(String _groupId, String _artifactId, String _version) {
        if (_groupId == null || _groupId.isEmpty()
                || _artifactId == null || _artifactId.isEmpty()
                || _version == null || _version.isEmpty()) {
            throw new IllegalArgumentException("groupId, artifactId and version"
                    + " can't be null or empty");
        }
        this.groupId = _groupId;
        this.artifactId = _artifactId;
        this.version = new DefaultArtifactVersion(_version);
        this.buildNumber = getBuildNumber(version.getQualifier());
        this.absoluteVersion = getAbsoluteVersion(version);
        
        if (isAPI()) {
            apiPackage = groupId;
        } else {
            apiPackage = artifactId;
        }
        
        ComplianceException cex = new ComplianceException();
        Spec.checkApiPackage(apiPackage,cex);
        
        implNamespace = null;
        if (!isAPI()) {
            implNamespace = groupId;
            if (implNamespace.startsWith("javax.")) {
                String msg = "Implementation packages must NOT "+Spec.START_WITH_JAVAX;
                cex.addBreaker(msg);
            }
        }
        
        if(!cex.isCompliant()){
            throw cex;
        }
    }

    public String getArtifactId() {
        return artifactId;
    }

    public String getGroupId() {
        return groupId;
    }

    public ArtifactVersion getVersion() {
        return version;
    }

    public boolean isFinal() {
        return buildNumber == null;
    }

    public boolean isAPI() {
        return artifactId.equals(groupId.concat(API_SUFFIX));
    }

    public String getAbsoluteVersion() {
        return absoluteVersion;
    }
    
    public boolean specVersionEquals(String version){
        for(String c : buildNumberSeparators){
            if(absoluteVersion.contentEquals(version +"-"+c+buildNumber)){
                return true;
            }
        }
        return false;
    }

    public String getBuildNumber() {
        return buildNumber;
    }

    public String getApiPackage() {
        return apiPackage;
    }

    public String getImplNamespace() {
        return implNamespace;
    }
    
    public static Artifact fromJar(JarFile jar) throws IOException {
        ZipEntry entry = jar.getEntry("pom.properties");
        if (entry == null) {
            throw new RuntimeException("unable to find pom.properties "
                    + "files inside " + jar.getName());
        }
        InputStream is = jar.getInputStream(entry);
        Properties pomProps = new Properties();
        pomProps.load(is);

        return new Artifact(
                pomProps.getProperty("groupId"),
                pomProps.getProperty("artifactId"),
                pomProps.getProperty("version"));
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if(!isFinal()){
            sb.append("non ");
        }
        sb.append("final ");
        if(isAPI()){
            sb.append("API ");
        } else {
            sb.append("standalone ");
        }
        sb.append("jar ");
        sb.append('(');
        sb.append(groupId);
        sb.append(':');
        sb.append(artifactId);
        sb.append(':');
        sb.append(version);
        sb.append(')');
        return sb.toString();
    }
}
