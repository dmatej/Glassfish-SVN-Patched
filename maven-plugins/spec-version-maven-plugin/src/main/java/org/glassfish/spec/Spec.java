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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.Console;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 *
 * @author Romain Grecourt
 */
public final class Spec {
    private Artifact artifact;
    private Metadata metadata;
    static final String START_WITH_JAVAX = "start with \"javax.\"";
    
    public static void checkSpecVersion(String s, ComplianceException ex) {
        if (!s.matches("[0-9]+\\.[0-9]+")) {
            String msg = "invalid specVersion (" + s + "):"
                    + " JCP specification version number "
                    + "must be of the form <major>.<minor>";
            if (ex != null) {
                ex.addBreaker(msg);
            } else {
                throw new ComplianceException(msg);
            }
        }
    }
    
    public static void checkApiPackage(String s, ComplianceException ex) {
        if (!s.startsWith("javax.")) {
            String msg = "API packages must" + START_WITH_JAVAX + " but is \"" + s + "\"";
            if (ex != null) {
                ex.addBreaker(msg);
            } else {
                throw new ComplianceException(msg);
            }
        }
    }
    
    public static void check(
            Artifact _artifact,
            String specVersion,
            String newVersion,
            String implVersion){
        
        if(_artifact == null 
                || specVersion == null || specVersion.isEmpty()
                || implVersion == null || implVersion.isEmpty()){
            
             throw new IllegalArgumentException("artifact, specVersion,"
                     + " newSpecVersion and implVersion can't be null or empty");
        }
        
        if (!_artifact.isFinal()) {
            if (newVersion == null) {
                
                throw new IllegalArgumentException("new version is required for nonfinal");
                
            } else if (!_artifact.specVersionEquals(newVersion)) {
                
                throw new IllegalArgumentException(
                        "maven version should be "
                        + newVersion + "-[b|m]" + _artifact.getBuildNumber()
                        + "[-SNAPSHOT]");
            }
        } else {
            if (newVersion == null) {
                
                throw new IllegalArgumentException(
                        "newspecversion must not be specified for final specification");
            }
        }
        
        ComplianceException cex = new ComplianceException();
        checkSpecVersion(specVersion,cex);
        checkSpecVersion(newVersion,cex);

        if (_artifact.isAPI()) {
            String sv = _artifact.isFinal() ? specVersion : newVersion;
            
            if (!(implVersion.equals(sv)
                    || implVersion.startsWith(sv + ".")
                    || implVersion.startsWith(sv + "-"))) {
                cex.addBreaker("spec implementation version must"
                        + " start with JCP specification version number");
            }
        }

        if (!cex.isCompliant()) {
            throw cex;
        }
    }

    public Spec(
            Artifact _artifact,
            String specVersion,
            String newVersion,
            String implVersion) {
        
        ComplianceException cex = new ComplianceException();
        try {
            check(_artifact, specVersion, newVersion, implVersion);
        } catch (ComplianceException ex) {
            cex.addBreaker(ex);
        } finally {
            this.artifact = _artifact;
        }

        try {
            this.metadata = Metadata.generate(
                    artifact,
                    specVersion,
                    newVersion,
                    implVersion);
        } catch (ComplianceException ex) {
            cex.addBreaker(ex);
        }

        if (!cex.isCompliant()) {
            throw cex;
        }
    }
    
//    public Spec(JarFile jar) throws IOException {
//        this.artifact = Artifact.fromJar(jar);
//        this.metadata = Metadata.fromJar(jar);
//    }

    public Artifact getArtifact() {
        return artifact;
    }

    public Metadata getMetadata() {
        return metadata;
    }
    
    
    private static Console cons;
    
    /**
     * Prompt with the string and return the user's input.
     */
    private static String prompt(String p) {
	if (cons == null)
	    return null;
	String s = cons.readLine("%s: ", p);
	if (s == null || s.length() == 0)
	    return null;
	return s;
    }

    /**
     * Print error and exit.
     */
    private static void fail(String s) {
	System.err.println("ERROR: " + s);
	System.exit(1);
    }
    
    /**
     * Print and count an error.
     */
    private static void err(String s) {
	System.out.println("ERROR: " + s);
//	errs++;
    }

    /**
     * Print a warning.
     */
    private static void warn(String s) {
	System.out.println("WARNING: " + s);
    }
    
    /**
     * Return the build "name" with a leading "b" or whatever.
     */
    private static String buildname(String b) {
	if (Character.isDigit(b.charAt(0)))
	    return "b" + b;
	else
	    return b;
    }

    /**
     * Returnthe build number, without any leading "b".
     */
    private static String buildnum(String b) {
	if (Character.isDigit(b.charAt(0)))
	    return b;
	else
	    return b.substring(1);
    }

    /**
     * Does the jar manifest attribute have the expected value?
     */
    private static void acheck(Attributes a, String name, String expected) {
	String value = a.getValue(name);
	if (value == null)
	    err("Attribute " + name + " is missing");
	else if (!value.equals(expected))
	    err("Attribute " + name + " is " + value +
		" but should be " + expected);
    }

    /**
     * Does the Maven pom.properties file on the InputStream represent
     * this artifact?
     */
    private static void mavenCheck(InputStream is, String version,
			String groupId, String artifactId) throws IOException {
	Properties p = new Properties();
	p.load(is);
	pcheck(p, "version", version);
	pcheck(p, "groupId", groupId);
	pcheck(p, "artifactId", artifactId);
	is.close();
    }

    /**
     * Does the specified property have the expected value?
     */
    private static void pcheck(Properties p, String name, String expected) {
	String value = p.getProperty(name);
	if (value == null)
	    err("Maven property " + name + " is missing");
	else if (!value.equals(expected))
	    err("Maven property " + name + " is " + value +
		" but should be " + expected);
    }

    /**
     * Copy the URL to a temp file and return the File.
     */
    private static File copyUrl(String url) throws IOException {
	File tmp = File.createTempFile("vc.", ".jar");
	tmp.deleteOnExit();
	FileOutputStream fos = new FileOutputStream(tmp);
	InputStream is = new URL(url).openStream();
	BufferedInputStream bis = new BufferedInputStream(is);
	BufferedOutputStream bos = new BufferedOutputStream(fos);
	byte[] buf = new byte[16*1024];
	int n;
	while ((n = bis.read(buf)) > 0)
	    bos.write(buf, 0, n);
	bis.close();
	bos.close();
	return tmp;
    }

    /**
     * Return the final filename part of a URL.
     */
    private static String nameOf(String url) {
	return url.substring(url.lastIndexOf('/') + 1);
    }

    private static boolean getBooleanProperty(Properties p, String name,
						boolean def) {
	String s = p.getProperty(name);
	if (s == null)
	    return def;
	return Boolean.parseBoolean(s);
    }

    /**
     * Check that the classes in the jar file are in one of the
     * specified packages.
     */
    private static void checkClasses(JarFile jf, String... pkgs) {
	Enumeration<JarEntry> e = jf.entries();
	Set<String> badPackages = new HashSet<String>();
	entries:
	while (e.hasMoreElements()) {
	    JarEntry je = e.nextElement();
	    if (je.isDirectory())
		continue;
	    String name = je.getName();
	    if (!name.endsWith(".class"))
		continue;
	    name = name.substring(0, name.length() - 6); // remove ".class"
	    name = name.replace('/', '.');

	    // name is now the class name,
	    // is it in one of the allowed packages?
	    for (String p : pkgs) {
		if (name.startsWith(p) && name.charAt(p.length()) == '.')
		    continue entries;	// yes, move on to next entry
	    }

	    // not in an allowed package
	    int i = name.lastIndexOf('.');
	    if (i > 0)
		name = name.substring(0, i);	// remove class name

	    // see if we've already complained about it
	    if (!badPackages.contains(name)) {
		badPackages.add(name);
		if (name.startsWith("javax."))
		    err("jar file includes class in wrong package: " + name);
		else
		    warn("jar file includes class in wrong package: " + name);
	    }
	}
    }
    
    private static void printParam(String arg, String desc){
        StringBuilder sb = new StringBuilder("\t--");
        System.out.println(sb.append(arg).append(' ').append(desc).toString());
    }
    
    /**
     * Spec - check that version information in jar files is correct.
     *
     * Usage: java VersionCheck --properties file	read settings from property
     * file --nonfinal	non-final specification --standalone	API has a standalone
     * implementation --apijar api.jar	API jar file --impljar impl.jar
     * implementation jar file --apipackage package	API package --implpackage
     * package	implementation package --specversion version	version number of
     * the JCP specification --specimplversion vers	version number of the API
     * classes --implversion version	version number of the implementation
     * --newspecversion vers	version number of the spec under development
     * --specbuild num	build number of spec API jar file --newimplversion vers
     * version number of the implementation when final --implbuild num	build
     * number of implementation jar file --repo url	maven repo URL
     *
     * java -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 ...
     */
    public static void main(String[] argv) throws Exception {
        int optind;
        String properties = null;
        boolean isFinal = true;
        boolean isAPI = true;
        String jar = null;
        String apipackage = null;
        String implpackage = null;
        String specversion = null;
        String implversion = null;
        String newversion = null;
        String build = null;

        for (optind = 0; optind < argv.length; optind++) {
            if (argv[optind].equals("--properties")) {
                properties = argv[++optind];
            } else if (argv[optind].equals("--nonfinal")) {
                isFinal = false;
            } else if (argv[optind].equals("--standalone")) {
                isAPI = false;
            } else if (argv[optind].equals("--apijar")) {
                jar = argv[++optind];
            } else if (argv[optind].equals("--impljar")) {
                jar = argv[++optind];
                isAPI = false;
            } else if (argv[optind].equals("--apipackage")) {
                apipackage = argv[++optind];
            } else if (argv[optind].equals("--implpackage")) {
                implpackage = argv[++optind];
            } else if (argv[optind].equals("--specversion")) {
                specversion = argv[++optind];
            } else if (argv[optind].equals("--specimplversion")) {
                specversion = argv[++optind];
            } else if (argv[optind].equals("--implversion")) {
                implversion = argv[++optind];
            } else if (argv[optind].equals("--newspecversion")) {
                newversion = argv[++optind];
            } else if (argv[optind].equals("--specbuild")) {
                build = argv[++optind];
            } else if (argv[optind].equals("--newimplversion")) {
                newversion = argv[++optind];
            } else if (argv[optind].equals("--implbuild")) {
                build = argv[++optind];
            } else if (argv[optind].equals("--")) {
                optind++;
                break;
            } else if (argv[optind].startsWith("-")) {
                System.out.println("Usage: java "+Spec.class.getSimpleName()+" [options]");
                printParam("properties","file\tread settings from property file");
                printParam("nonfinal","\t\tnon-final specification");
                printParam("standalone","\t\tAPI has a standalone implementation");
                printParam("apijar","api.jar\tAPI jar file");
                printParam("impljar","impl.jar\timplementation jar file");
                printParam("apipackage","package\tAPI package");
                printParam("implpackage","package\timplementation package");
                printParam("specversion","version\tversion number of the JCP specification");
                printParam("specimplversion","vers\tversion number of the API classes");
                printParam("implversion","version\tversion number of the implementation");
                printParam("newspecversion","vers\tversion number of the spec under development");
                printParam("specbuild","num\tbuild number of spec API jar file");
                printParam("newimplversion","vers\tversion number of the implementation when final");
                printParam("implbuild","num\tbuild number of implementation jar file");
                System.exit(1);
            } else {
                break;
            }
        }

        if (properties != null) {
            Properties p = new Properties();
            FileInputStream fis = new FileInputStream(properties);
            p.load(fis);
            fis.close();
            apipackage = p.getProperty("API_PACKAGE", apipackage);
            implpackage = p.getProperty("IMPL_NAMESPACE", implpackage);
            isAPI = !getBooleanProperty(p, "STANDALONE_IMPL", isAPI);
            if(isAPI){
                implversion = p.getProperty("SPEC_IMPL_VERSION", implversion);
                build = p.getProperty("SPEC_BUILD", build);
                newversion = p.getProperty("NEW_SPEC_VERSION", newversion);
                jar = p.getProperty("API_JAR", jar);
            } else {
                implversion = p.getProperty("IMPL_VERSION", implversion);
                build = p.getProperty("IMPL_BUILD", build);
                newversion = p.getProperty("NEW_IMPL_VERSION", newversion);
                jar = p.getProperty("IMPL_JAR", jar);
            }
            specversion = p.getProperty("SPEC_VERSION", specversion);
            isFinal = newversion == null;	// really, any of the above 4
        }

        cons = System.console();

        // if no options, prompt for everything
        if (argv.length == 0) {
            String s;
            s = prompt("Is this a non-final specification?");
            isFinal = !(s.charAt(0) == 'y');
            s = prompt("Is there a standalone implementation of this specification?");
            isAPI = !(s.charAt(0) == 'y');
        }

        if (apipackage == null) {
            apipackage = prompt("Enter the main API package (e.g., javax.wombat)");
        }

        if (!isAPI) {
            // prompt for missing args
            if (implpackage == null) {
                implpackage = prompt("Enter the main implementation package (e.g., com.sun.wombat)");
            }
        } else {
            if (jar != null) {
                fail("--impljar must not be specified if no standalone implementation");
            }
            if (implpackage != null) {
                fail("--implpackage must not be specified if no standalone implementation");
            }
            if (implversion != null) {
                fail("--implversion must not be specified if no standalone implementation");
            }
            if (newversion != null) {
                fail("--newimplversion must not be specified if no standalone implementation");
            }
        }

        if (implpackage != null && implpackage.startsWith("javax.")) {
            fail("Implementation packages must NOT start with \"javax.\"");
        }

        if (specversion == null) {
            specversion = prompt("Enter the version number of the JCP specification");
        }
        if (implversion == null) {
            implversion = prompt("Enter the version number of the API jar file");
        }
        if (!isAPI && implversion == null) {
            implversion = prompt("Enter the version number of the implementation");
        }

        if (!isFinal) {
            // prompt for missing args
            if (newversion == null) {
                newversion = prompt(
                        "Enter the version number of"
                        + " the JCP specification under development");
            }
            if (build == null) {
                build = prompt("Enter the build number of the API jar file");
            }
            if (!isAPI && newversion == null) {
                newversion = prompt(
                        "Enter the version number of the "
                        + "implementation that will be used\n"
                        + "when the implementation is final");
            }
            if (!isAPI && build == null) {
                build = prompt("Enter the build number of the implementation jar file");
            }
        } else {
            if (newversion != null) {
                fail("--newspecversion must not be specified for final specification");
            }
            if (isAPI && build != null) {
                fail("--specbuild must not be specified for final specification");
            }
            if (newversion != null) {
                fail("--newimplversion must not be specified for final specification");
            }
            if (!isAPI && build != null) {
                fail("--implbuild must not be specified for final specification");
            }
        }
        
        // the CLI may not work as expected currently...
        // TODO write some integration tests for command line
        Artifact artifact = Artifact.fromJar(new JarFile(jar));
        Spec spec = new Spec(artifact, specversion, newversion, implversion);
    }
}