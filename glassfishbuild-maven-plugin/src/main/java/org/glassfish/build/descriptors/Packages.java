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
package org.glassfish.build.descriptors;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

/**
 *
 * @author Romain Grecourt
 */
@XmlRootElement
public class Packages {
    public static final String DEFAULT_TYPE = "zip";
    private static HashSet<String> artifactsIds;

    @XmlType
    public static class Package {
        
        @XmlAttribute(name="groupId")
        String packageGroupId;
        
        @XmlAttribute(name="artifactId")
        String packageArtifactId;
        
        @XmlAttribute(name="version")
        String packageVersion;
        private List<PackageItem> packageItems;

        public Package(){}
        
        public Package(String gId, String aId, String version){
            packageItems = new ArrayList<PackageItem>();
            packageGroupId = gId;
            packageArtifactId = aId;
            packageVersion = version;
        }
        
        public void setPackageArtifactId(String packageArtifactId) {
            this.packageArtifactId = packageArtifactId;
        }

        public void setPackageGroupId(String packageGroupId) {
            this.packageGroupId = packageGroupId;
        }

        public void setPackageVersion(String packageVersion) {
            this.packageVersion = packageVersion;
        }        

        public void setPackageItems(List<PackageItem> packageItems) {
            this.packageItems = packageItems;
        }

        
        public String getGroupId() {
            return packageGroupId;
        }
        
        public String getArtifactId() {
            return packageArtifactId;
        }

        public String getVersion() {
            return packageVersion;
        }
        
        @XmlElement(name="packageItem")
        public List<PackageItem> getPackageItems() {
            return packageItems;
        }        

        public void addPackageItem(PackageItem pkgMbr) {
            packageItems.add(pkgMbr);
        }
        
        public boolean isInPackages(String gId, String aId, String version){
            Iterator<PackageItem> it = getPackageItems().iterator();
            while(it.hasNext()){
                PackageItem pkgItem = it.next();
                if(gId == null || pkgItem.getGroupId().equals(gId)){
                    if(version==null || pkgItem.getVersion().equals(version)){
                        if(pkgItem.getArtifactId().equals(aId))
                            return true;
                    }
                }
            }
            return false;
        }

        @XmlType
        public static class PackageItem {
            
            @XmlAttribute(name="groupId")
            private String memberGroupId;
            
            @XmlAttribute(name="artifactId")
            private String memberArtifactId;
            
            @XmlAttribute(name="version")
            private String memberVersion;

            public PackageItem() {}
            
            public PackageItem(String gId, String aId, String version) throws DuplicatedArtifactIdEx {
                if(artifactsIds.contains(aId))
                    throw new DuplicatedArtifactIdEx("artifactId("+aId+") is duplicated in package");
                memberGroupId = gId;
                memberArtifactId = aId;
                memberVersion = version;
            }

            public void setMemberArtifactId(String memberArtifactId) {
                this.memberArtifactId = memberArtifactId;
            }

            public void setMemberGroupId(String memberGroupId) {
                this.memberGroupId = memberGroupId;
            }

            public void setMemberVersion(String memberVersion) {
                this.memberVersion = memberVersion;
            }
            
            public String getGroupId() {
                return memberGroupId;
            }
            
            public String getArtifactId() {
                return memberArtifactId;
            }

            public String getVersion() {
                return memberVersion;
            }
        }
    }

    public static class DuplicatedArtifactIdEx extends Exception {
        public DuplicatedArtifactIdEx(String msg) {
            super(msg);
        }
    }
    
    private List<Package> packages;
    private String groupId;
    private String artifactId;
    private String version;

    public Packages() { }
    
    public Packages(MavenProject p){
        artifactsIds = new HashSet<String>();
        packages = new ArrayList<Package>();
        groupId = p.getGroupId();
        artifactId = p.getArtifactId();
        version = p.getVersion();
    }
    
    @XmlElement(name="package")
    public List<Package> getPackages() {
        return packages;
    }
    
    @XmlAttribute
    public String getGroupId() {
        return groupId;
    }
    
    @XmlAttribute
    public String getArtifactId() {
        return artifactId;
    }

    @XmlAttribute
    public String getVersion() {
        return version;
    }    

    public void addPackage(Package p) {
        packages.add(p);
    }
    
    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public void setPackages(List<Package> packages) {
        this.packages = packages;
    }

    public void setVersion(String version) {
        this.version = version;
    }    

    public void writeXML(File output) throws MojoExecutionException, JAXBException {
        JAXBContext ctxt = JAXBContext.newInstance(Packages.class);
        Marshaller m = ctxt.createMarshaller();
        m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        m.marshal(this, output);
    }
    
    public static Packages readXML(File input) throws JAXBException{
        JAXBContext ctxt = JAXBContext.newInstance(Packages.class);
        Unmarshaller m = ctxt.createUnmarshaller();
        return (Packages) m.unmarshal(input);
    }
    
    public static Package getPackageWithDependency(
            List<Packages> packagesList, String gid, String aId, String version){
        
        Package pkg = null;
        for (Packages pkgs : packagesList) {
            if ((pkg = pkgs.getPackageWithDependency(gid,aId,version)) != null) {
                return pkg;
            }
        }
        return pkg;
    }
    
    public Package getPackageWithDependency(String gid, String aId, String version){
        Iterator<Package> it = getPackages().iterator();
        while(it.hasNext()){
            Package curPkg = it.next();
            if(curPkg.isInPackages(gid, aId, version)){
                return curPkg;
            }
        }
        return null;
    }
}