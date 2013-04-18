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
import java.util.Enumeration;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;


/**
 *
 * @author Romain Grecourt
 */
public final class Artifact {

    private String groupId;
    private String artifactId;
    private ArtifactVersion version;
    private String buildNumber;
    
    private static final String[] buildNumberSeparators = new String[] {"m", "b"};
    public static final String SNAPSHOT_QUALIFIER = "SNAPSHOT";
    
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
        qualifier = stripSnapshotQualifier(qualifier);
        if (qualifier != null) {
            for (String c : buildNumberSeparators) {
                if (qualifier.contains(c)) {
                    return qualifier.substring(qualifier.lastIndexOf(c) + 1);
                }
            }
        }
        return null;
    }
    
    public Artifact(){
    }

    public Artifact(String _groupId, String _artifactId, String _version) {
        this.groupId = _groupId;
        this.artifactId = _artifactId;
        this.version = new DefaultArtifactVersion(_version);
        this.buildNumber = getBuildNumber(version.getQualifier());
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
    
    public String getAbsoluteVersion(){
        return stripSnapshotQualifier(version.toString());
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public void setVersion(String version) {
        this.version = new DefaultArtifactVersion(version);
        this.buildNumber = getBuildNumber(this.version.getQualifier());
    }
    
    public String getBuildNumber() {
        return buildNumber;
    }
    
    private static ZipEntry getPomPropertiesFile(JarFile jar){
        Enumeration<JarEntry> entries = jar.entries();
        while(entries.hasMoreElements()){
            JarEntry entry = entries.nextElement();
            if(entry.getName().endsWith("pom.properties")){
                return entry;
            }
        }
        return null;
    }
    
    public static Artifact fromJar(JarFile jar) throws IOException {
        ZipEntry entry = getPomPropertiesFile(jar);
        if (entry == null) {
            throw new RuntimeException(
                    "unable to find pom.properties "
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
        sb.append("[ ");
        sb.append(groupId);
        sb.append(':');
        sb.append(artifactId);
        sb.append(':');
        sb.append(version);
        sb.append(" ]");
        return sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Artifact other = (Artifact) obj;
        if ((this.groupId == null) ? (other.groupId != null) : !this.groupId.equals(other.groupId)) {
            return false;
        }
        if ((this.artifactId == null) ? (other.artifactId != null) : !this.artifactId.equals(other.artifactId)) {
            return false;
        }
        if (this.version != other.version && (this.version == null || !this.version.equals(other.version))) {
            return false;
        }
        return true;
    }
}
