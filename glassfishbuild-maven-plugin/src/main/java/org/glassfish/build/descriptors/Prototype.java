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

/**
 *
 * @author romano
 */
public class Prototype {

    public static class Attribute {

        private String key;
        private String value;

        public String getKey() {
            return key;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public Attribute() { }
        
        public String toPythonString(){
            return "\""+key+"\" : \""+value+"\",";
        }
    }

    public static class Default {

        private String type;
        private String mode;
        private String group;
        private String owner;

        public String getGroup() {
            return group;
        }

        public void setGroup(String group) {
            this.group = group;
        }

        public String getMode() {
            return mode;
        }

        public void setMode(String mode) {
            this.mode = mode;
        }

        public String getOwner() {
            return owner;
        }

        public void setOwner(String owner) {
            this.owner = owner;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public Default() { }
        
        public String toPythonString(){
            return "\""+type+"\" : {\"mode\" \""+mode+"\", \"group\" : \""+group+"\", \"owner\" : \""+owner+"\",}";
        }
    }

    public static class Depend {

        String pkg;
        String version;
        String type;

        public String getPkg() {
            return pkg;
        }

        public void setPkg(String pkg) {
            this.pkg = pkg;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getVersion() {
            return version;
        }

        public void setVersion(String version) {
            this.version = version;
        }

        public Depend() { }
        
        public String toPythonString(){
            return "\"pkg:/"+pkg+"@"+version+" : {\"type\" : \""+type+"\"}";
        }
    }

    public static class File {

        String path = null;
        String mode = null;
        String owner = null;
        String group = null;
        String os = null;
        String preserve = null;

        public String getPreserve() {
            return preserve;
        }

        public void setPreserve(String preserve) {
            this.preserve = preserve;
        }

        public String getGroup() {
            return group;
        }

        public void setGroup(String group) {
            this.group = group;
        }

        public String getMode() {
            return mode;
        }

        public void setMode(String mode) {
            this.mode = mode;
        }

        public String getOs() {
            return os;
        }

        public void setOs(String os) {
            this.os = os;
        }

        public String getOwner() {
            return owner;
        }

        public void setOwner(String owner) {
            this.owner = owner;
        }

        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }

        public File() { }
  
        public String toPythonString() {
            String pythonStr = "\"" + path + "\" : {";

            if (mode != null) {
                pythonStr += "\"mode\" : \"" + mode + "\", ";
            }
            if (os != null) {
                pythonStr += "\"os\" : \"" + os + "\",  ";
            }
            if (owner != null) {
                pythonStr += "\"owner\" : \"" + owner + "\", ";
            }
            if (group != null) {
                pythonStr += "\"group\" : \"" + group + "\", ";
            }
            if (preserve != null) {
                pythonStr += "\"preserve\" : \"" + preserve + "\", ";
            }

            return pythonStr + "}";
        }
    }

    public static class Dir extends File {

        public Dir() {
        }
    }

    public static class DirTree extends File {

        public DirTree() { }
    }

    public static class ExcludeFile {

        String path;

        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }

        public ExcludeFile() { }
        
        public String toPythonString(){
            return "\"pkg/"+path+"\"";
        }
    }

    public static class ExcludesDir extends ExcludeFile {

        public ExcludesDir() {
        }
    }

    public static class License {

        String file;
        String name;

        public String getFile() {
            return file;
        }

        public void setFile(String file) {
            this.file = file;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public License() { }
        
        public String toPythonString(){
            return "\"pkg/"+file+"\" : {\"licence\" : \""+name+"\"},";
        }
    }
    private String name = null;
    private String version = null;
    private Attribute[] attributes = null;
    private Default[] defaults = null;
    private Depend[] depends = null;
    private Dir[] dirs = null;
    private DirTree[] dirTrees = null;
    private File[] files = null;
    private ExcludeFile[] excludeFiles = null;
    private ExcludesDir[] excludeDirs = null;
    private License[] licences = null;

    public Attribute[] getAttributes() {
        return attributes;
    }

    public void setAttributes(Attribute[] attributes) {
        this.attributes = attributes;
    }

    public Default[] getDefaults() {
        return defaults;
    }

    public void setDefaults(Default[] defaults) {
        this.defaults = defaults;
    }

    public Depend[] getDepends() {
        return depends;
    }

    public void setDepends(Depend[] depends) {
        this.depends = depends;
    }

    public DirTree[] getDirTrees() {
        return dirTrees;
    }

    public void setDirTrees(DirTree[] dirTrees) {
        this.dirTrees = dirTrees;
    }

    public Dir[] getDirs() {
        return dirs;
    }

    public void setDirs(Dir[] dirs) {
        this.dirs = dirs;
    }

    public ExcludesDir[] getExcludeDirs() {
        return excludeDirs;
    }

    public void setExcludsDirs(ExcludesDir[] excludeDirs) {
        this.excludeDirs = excludeDirs;
    }

    public ExcludeFile[] getExcludeFiles() {
        return excludeFiles;
    }

    public void setExcludeFiles(ExcludeFile[] excludeFiles) {
        this.excludeFiles = excludeFiles;
    }

    public File[] getFiles() {
        return files;
    }

    public void setFiles(File[] files) {
        this.files = files;
    }

    public License[] getLicences() {
        return licences;
    }

    public void setLicences(License[] licences) {
        this.licences = licences;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String toPythonString() {
        StringBuilder sb = new StringBuilder("pkg = {\n");
        if(name != null){
            sb.append("\"name\" : \"");
            sb.append(name);
            sb.append("\",\n");
        }
        if(version != null){
            sb.append("\"version\" : \"");
            sb.append(version);
            sb.append("\",\n");
        }
        if(attributes != null){
            sb.append("\"attributes\" : {\n");
            for(Attribute a : attributes){
                sb.append(a.toPythonString());
                sb.append(",\n");
            }
            sb.append("},\n");
        }
        if(defaults != null){
            sb.append("\"defaults\" : {\n");
            for(Default d : defaults){
                sb.append(d.toPythonString());
                sb.append(",\n");
            }
            sb.append("},\n");
        }
        if(depends != null){
            sb.append("\"depends\" : {\n");
            for(Depend d : depends){
                sb.append(d.toPythonString());
                sb.append(",\n");
            }
            sb.append("},\n");
        }
        if(dirs != null){
            sb.append("\"dirs\" : {\n");
            for(Dir d : dirs){
                sb.append(d.toPythonString());
                sb.append(",\n");
            }
            sb.append("},\n");
        }
        if(dirTrees != null){
            sb.append("\"dirtrees\" : {\n");
            for(DirTree d : dirTrees){
                sb.append(d.toPythonString());
                sb.append(",\n");
            }
            sb.append("},\n");
        }
        if(files != null){
            sb.append("\"files\" : {\n");
            for(File f : files){
                sb.append(f.toPythonString());
                sb.append(",\n");
            }
            sb.append("},\n");
        }
        if(excludeFiles != null){
            sb.append("\"excludesfiles\" : [\n");
            for(ExcludeFile e : excludeFiles){
                sb.append(e.toPythonString());
                sb.append(",\n");
            }
            sb.append("],\n");
        }
        if(excludeDirs != null){
            sb.append("\"excludedirs\" : [\n");
            for(ExcludeFile e : excludeDirs){
                sb.append(e.toPythonString());
                sb.append(",\n");
            }
            sb.append("],\n");
        }
        if(licences != null){
            sb.append("\"licences\" : {\n");
            for(License l : licences){
                sb.append(l.toPythonString());
                sb.append(",\n");
            }
            sb.append("},\n");
        }
        sb.append("}");
        return sb.toString();
    }
}
