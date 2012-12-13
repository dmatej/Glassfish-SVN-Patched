/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
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

package org.glassfish.nexus.client.beans;

import java.io.File;
import javax.xml.bind.annotation.XmlType;

/**
 *
 * @author Romain Grecourt
 */
@XmlType
final public class MavenArtifactInfo {

    private String groupId;
    private String artifactId;
    private String version;
    private String classifier = null;
    private String extension = null;
    private File file;

    public MavenArtifactInfo(String groupId, String artifactId, String version, String classifier, String extension, File file) {
        this.groupId = groupId;
        this.artifactId = artifactId;
        this.version = version;
        this.classifier = classifier;
        this.extension = extension;
        this.file = file;
    }

    public MavenArtifactInfo() {
    }

    public File getFile(){
        return file;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public String getArtifactId() {
        return artifactId;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getClassifier() {
        return classifier;
    }

    public void setClassifier(String classifier) {
        this.classifier = classifier;
    }

    public String getExtension() {
        return extension;
    }

    public void setExtension(String extension) {
        this.extension = extension;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(groupId);
        sb.append(':');
        sb.append(artifactId);
        sb.append(':');
        if(extension != null && !extension.isEmpty()){
            sb.append(extension);
            sb.append(':');
        }
        if(classifier != null && !classifier.isEmpty()){
            sb.append(classifier);
            sb.append(':');
        }
        sb.append(version);
        return sb.toString();
    }

    public String getRepositoryDirRelativePath(){
        StringBuilder sb = new StringBuilder();
        sb.append(groupId.replace('.', '/'));
        sb.append('/');
        sb.append(artifactId);
        sb.append('/');
        sb.append(version);
        return sb.toString();
    }

    public String getRepositoryRelativePath(){
        StringBuilder sb = new StringBuilder(getRepositoryDirRelativePath());
        sb.append('/');
        sb.append(artifactId);
        sb.append('-');
        sb.append(version);
        sb.append('.');
        sb.append(extension);
        return sb.toString();
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 97 * hash + (this.groupId != null ? this.groupId.hashCode() : 0);
        hash = 97 * hash + (this.artifactId != null ? this.artifactId.hashCode() : 0);
        hash = 97 * hash + (this.version != null ? this.version.hashCode() : 0);
        hash = 97 * hash + (this.classifier != null ? this.classifier.hashCode() : 0);
        hash = 97 * hash + (this.extension != null ? this.extension.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final MavenArtifactInfo other = (MavenArtifactInfo) obj;
        if ((this.groupId == null) ? (other.groupId != null) : !this.groupId.equals(other.groupId)) {
            return false;
        }
        if ((this.artifactId == null) ? (other.artifactId != null) : !this.artifactId.equals(other.artifactId)) {
            return false;
        }
        if ((this.version == null) ? (other.version != null) : !this.version.equals(other.version)) {
            return false;
        }
        if ((this.classifier == null) ? (other.classifier != null) : !this.classifier.equals(other.classifier)) {
            return false;
        }
        if ((this.extension == null) ? (other.extension != null) : !this.extension.equals(other.extension)) {
            return false;
        }
        return true;
    }
}