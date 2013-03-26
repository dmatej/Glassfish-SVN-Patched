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

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.jar.*;
import java.util.zip.*;

/**
 * VersionCheck - check that version information in jar files is correct.
 *
 * Usage: java VersionCheck
 *	--properties file	read settings from property file
 *	--nonfinal		non-final specification
 *	--standalone		API has a standalone implementation
 *	--apijar api.jar	API jar file
 *	--impljar impl.jar	implementation jar file
 *	--apipackage package	API package
 *	--implpackage package	implementation package
 *	--specversion version	version number of the JCP specification
 *	--specimplversion vers	version number of the API classes
 *	--implversion version	version number of the implementation
 *	--newspecversion vers	version number of the spec under development
 *	--specbuild num		build number of spec API jar file
 *	--newimplversion vers	version number of the implementation when final
 *	--implbuild num		build number of implementation jar file
 *	--repo url		maven repo URL
 *
 * java -Dhttps.proxyHost=www-proxy.us.oracle.com -Dhttps.proxyPort=80 ...
 */
public class Main {
    private static String properties;
    private static boolean maven;
    private static boolean nonfinal;
    private static boolean standalone;
    private static String apijar;
    private static String impljar;
    private static String apipackage;
    private static String implpackage;
    private static String specversion;
    private static String specimplversion;
    private static String implversion;
    private static String newspecversion;
    private static String specbuild;
    private static String newimplversion;
    private static String implbuild;
    private static String repo;

    private static Console cons;

    private static int errs;

    private static final String REPO =
	"http://gf-maven.us.oracle.com/nexus/content/groups/internal-gf-nexus/";

    public static void main(String[] argv) throws Exception {
	int optind;
	for (optind = 0; optind < argv.length; optind++) {
	    if (argv[optind].equals("--properties")) {
		properties = argv[++optind];
	    } else if (argv[optind].equals("--maven")) {
		maven = true;
	    } else if (argv[optind].equals("--nonfinal")) {
		nonfinal = true;
	    } else if (argv[optind].equals("--standalone")) {
		standalone = true;
	    } else if (argv[optind].equals("--apijar")) {
		apijar = argv[++optind];
	    } else if (argv[optind].equals("--impljar")) {
		impljar = argv[++optind];
		standalone = true;
	    } else if (argv[optind].equals("--apipackage")) {
		apipackage = argv[++optind];
	    } else if (argv[optind].equals("--implpackage")) {
		implpackage = argv[++optind];
	    } else if (argv[optind].equals("--specversion")) {
		specversion = argv[++optind];
	    } else if (argv[optind].equals("--specimplversion")) {
		specimplversion = argv[++optind];
	    } else if (argv[optind].equals("--implversion")) {
		implversion = argv[++optind];
	    } else if (argv[optind].equals("--newspecversion")) {
		newspecversion = argv[++optind];
	    } else if (argv[optind].equals("--specbuild")) {
		specbuild = argv[++optind];
	    } else if (argv[optind].equals("--newimplversion")) {
		newimplversion = argv[++optind];
	    } else if (argv[optind].equals("--implbuild")) {
		implbuild = argv[++optind];
	    } else if (argv[optind].equals("--repo")) {
		repo = argv[++optind];
	    } else if (argv[optind].equals("--")) {
		optind++;
		break;
	    } else if (argv[optind].startsWith("-")) {
		System.out.println(
"Usage: java VersionCheck [options]");
		System.out.println(
"\t--properties file\tread settings from property file");
		System.out.println(
"\t--maven\t\tfetch artifacts from Maven repo");
		System.out.println(
"\t--nonfinal\t\tnon-final specification");
		System.out.println(
"\t--standalone\t\tAPI has a standalone implementation");
		System.out.println(
"\t--apijar api.jar\tAPI jar file");
		System.out.println(
"\t--impljar impl.jar\timplementation jar file");
		System.out.println(
"\t--apipackage package\tAPI package");
		System.out.println(
"\t--implpackage package\timplementation package");
		System.out.println(
"\t--specversion version\tversion number of the JCP specification");
		System.out.println(
"\t--specimplversion vers\tversion number of the API classes");
		System.out.println(
"\t--implversion version\tversion number of the implementation");
		System.out.println(
"\t--newspecversion vers\tversion number of the spec under development");
		System.out.println(
"\t--specbuild num\tbuild number of spec API jar file");
		System.out.println(
"\t--newimplversion vers\tversion number of the implementation when final");
		System.out.println(
"\t--implbuild num\tbuild number of implementation jar file");
		System.out.println(
"\t--repo url\tmaven repo URL");
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
	    maven = getBooleanProperty(p, "MAVEN", maven);
	    apipackage = p.getProperty("API_PACKAGE", apipackage);
	    implpackage = p.getProperty("IMPL_NAMESPACE", implpackage);
	    standalone = getBooleanProperty(p, "STANDALONE_IMPL", standalone);
	    specversion = p.getProperty("SPEC_VERSION", specversion);
	    specimplversion = p.getProperty("SPEC_IMPL_VERSION",
					    specimplversion);
	    implversion = p.getProperty("IMPL_VERSION", implversion);
	    newspecversion = p.getProperty("NEW_SPEC_VERSION", newspecversion);
	    specbuild = p.getProperty("SPEC_BUILD", specbuild);
	    newimplversion = p.getProperty("NEW_IMPL_VERSION", newimplversion);
	    implbuild = p.getProperty("IMPL_BUILD", implbuild);
	    nonfinal = newspecversion != null;	// really, any of the above 4
	    apijar = p.getProperty("API_JAR", apijar);
	    impljar = p.getProperty("IMPL_JAR", impljar);
	    repo = p.getProperty("REPO", repo);
	}

	if (repo == null)
	    repo = REPO;

	cons = System.console();

	// if no options, prompt for everything
	if (argv.length == 0) {
	    String s;
	    s = prompt(
		"Should artifacts be fetched from the Maven repository?");
	    maven = s.charAt(0) == 'y';
	    s = prompt("Is this a non-final specification?");
	    nonfinal = s.charAt(0) == 'y';
	    s = prompt("Is there a standalone implementation " +
			"of this specification?");
	    standalone = s.charAt(0) == 'y';
	}

	if (apipackage == null)
	    apipackage = prompt(
			"Enter the main API package (e.g., javax.wombat)");
	if (!apipackage.startsWith("javax."))
	    fail("API packages must start with \"javax.\"");

	if (standalone) {
	    // prompt for missing args
	    if (implpackage == null)
		implpackage = prompt(
		"Enter the main implementation package (e.g., com.sun.wombat)");
	} else {
	    if (impljar != null)
		fail("--impljar must not be specified " +
				    "if no standalone implementation");
	    if (implpackage != null)
		fail("--implpackage must not be specified " +
				    "if no standalone implementation");
	    if (implversion != null)
		fail("--implversion must not be specified " +
				    "if no standalone implementation");
	    if (newimplversion != null)
		fail("--newimplversion must not be specified " +
				    "if no standalone implementation");
	}

	if (implpackage != null && implpackage.startsWith("javax."))
	    fail("Implementation packages must NOT start with \"javax.\"");

	if (specversion == null)
	    specversion = prompt(
			"Enter the version number of the JCP specification");
	if (!specversion.matches("[0-9]+\\.[0-9]+"))
	    fail("JCP specification version number must be " +
				"of the form <major>.<minor>");
	if (specimplversion == null)
	    specimplversion = prompt(
			"Enter the version number of the API jar file");
	String sv = nonfinal ? newspecversion : specversion;
	if (!(specimplversion.equals(sv) ||
		specimplversion.startsWith(sv + ".") ||
		specimplversion.startsWith(sv + "-")))
	    fail("API jar file version must start with " +
				"JCP specification version number");
	if (standalone && implversion == null)
	    implversion = prompt(
			"Enter the version number of the implementation");

	if (nonfinal) {
	    // prompt for missing args
	    if (newspecversion == null)
		newspecversion = prompt("Enter the version number of the " +
					"JCP specification under development");
	    if (specbuild == null)
		specbuild = prompt("Enter the build number of the " +
					"API jar file");
	    if (standalone && newimplversion == null)
		newimplversion = prompt("Enter the version number of the " +
					"implementation that will be used\n" +
					"when the implementation is final");
	    if (standalone && implbuild == null)
		implbuild = prompt("Enter the build number of the " +
					"implementation jar file");
	} else {
	    if (newspecversion != null)
		fail("--newspecversion must not be specified " +
				    "for final specification");
	    if (specbuild != null)
		fail("--specbuild must not be specified " +
				    "for final specification");
	    if (newimplversion != null)
		fail("--newimplversion must not be specified " +
				    "for final specification");
	    if (implbuild != null)
		fail("--implbuild must not be specified " +
				    "for final specification");
	}

	String osgispecimplversion = specimplversion.replace('-', '.');
	String osgiimplversion = "";
	String shortosgiimplversion = "";

	System.out.printf("API jar file:\t\t\t%s-api.jar%n", apipackage);
	System.out.printf("  OSGi Bundle-SymbolicName:\t%s-api%n", apipackage);
	if (nonfinal) {
	    System.out.printf("  OSGi bundle specversion:\t%s.99.%s%n",
		specversion, buildname(specbuild));
	    System.out.printf("  OSGi Bundle-Version:\t\t%s.99.%s%n",
		specversion, buildname(specbuild));
	} else {
	    System.out.printf("  OSGi bundle specversion:\t%s%n", specversion);
	    System.out.printf("  OSGi Bundle-Version:\t\t%s%n",
		osgispecimplversion);
	}
	System.out.printf("  Maven group ID, artifact ID:\t%s:%s-api%n",
	    apipackage, apipackage);
	if (nonfinal)
	    System.out.printf("  Maven version:\t\t%s-%s%n",
		newspecversion, buildname(specbuild));
	else
	    System.out.printf("  Maven version:\t\t%s%n", specimplversion);
	System.out.printf("  Maven API jar file:\t\t%s-api-%s.jar%n",
	    apipackage,
	    (nonfinal ?
		(newspecversion + "-" + buildname(specbuild)) : specimplversion));
	System.out.printf("  Jar Extension-Name:\t\t%s%n", apipackage);
	if (nonfinal) {
	    System.out.printf("  jar Specification-Version:\t%s.99.%s%n",
		specversion, buildnum(specbuild));
	    System.out.printf("  jar Implementation-Version:\t%s-%s%n",
		newspecversion, buildname(specbuild));
	} else {
	    System.out.printf("  jar Specification-Version:\t%s%n",
		specversion);
	    System.out.printf("  jar Implementation-Version:\t%s%n",
		specimplversion);
	}

	System.out.println();

	if (standalone) {

	osgiimplversion = implversion.replace('-', '.');
	shortosgiimplversion = osgiimplversion.substring(0,
	    osgiimplversion.indexOf('.', osgiimplversion.indexOf('.') + 1));

	System.out.printf("Implementation jar file:\t%s.jar%n", apipackage);
	System.out.printf("  OSGi Bundle-SymbolicName:\t%s.%s%n",
	    implpackage, apipackage);
	if (nonfinal) {
	    System.out.printf("  OSGi bundle specversion:\t%s.99.%s%n",
		specversion, buildname(specbuild));
	    System.out.printf("  OSGi Bundle-Version:\t\t%s.99.%s%n",
		shortosgiimplversion, buildname(implbuild));
	} else {
	    System.out.printf("  OSGi bundle specversion:\t%s%n", specversion);
	    System.out.printf("  OSGi Bundle-Version:\t\t%s%n",
		osgiimplversion);
	}
	System.out.printf("  Maven group ID, artifact ID:\t%s:%s%n",
	    implpackage, apipackage);
	if (nonfinal)
	    System.out.printf("  Maven version:\t\t%s-%s%n",
		newimplversion, buildname(implbuild));
	else
	    System.out.printf("  Maven version:\t\t%s%n", implversion);
	System.out.printf("  Maven impl jar file:\t\t%s-%s.jar%n",
	    apipackage,
	    (nonfinal ?
		(newimplversion + "-" + buildname(implbuild)) : implversion));
	System.out.printf("  jar Extension-Name:\t\t%s%n", apipackage);
	if (nonfinal) {
	    System.out.printf("  jar Specification-Version:\t%s.99.%s%n",
		specversion, buildnum(specbuild));
	    System.out.printf("  jar Implementation-Version:\t%s-%s%n",
		newimplversion, buildname(implbuild));
	} else {
	    System.out.printf("  jar Specification-Version:\t%s%n",
		specversion);
	    System.out.printf("  jar Implementation-Version:\t%s%n",
		implversion);
	}

	}

	/*
	 * Now read jar files and verify contents.
	 */
	if (maven && apijar == null)
	    apijar = repoUrl(apipackage, apipackage + "-api", nonfinal ?
		    (newspecversion + "-" + buildname(specbuild)) :
		    specimplversion);
	if (maven && standalone && impljar == null)
	    impljar = repoUrl(implpackage, apipackage,
		nonfinal ? (newimplversion + "-" + buildname(implbuild)) :
		    implversion);

	if (apijar == null)
	    apijar = prompt("Enter the API jar file name");
	if (apijar != null) {
	    System.out.println();
	    System.out.println("Checking API jar file: " + apijar);
	    File f;
	    String name;
	    if (apijar.startsWith("http")) {
		f = copyUrl(apijar);
		name = nameOf(apijar);
	    } else {
		f = new File(apijar);
		name = f.getName();
	    }
	    String apijarname = apipackage + "-api.jar";
	    String apijarmavenname = apipackage + "-api-" +
		(nonfinal ? (newspecversion + "-" + buildname(specbuild)) :
			    specimplversion) +
		".jar";
	    if (!(name.equals(apijarname) || name.equals(apijarmavenname)))
		err("API jar file name should be " + apijarname +
		    " or " + apijarmavenname);
	    JarFile jf = new JarFile(f);
	    Manifest m = jf.getManifest();
	    Attributes a = m.getMainAttributes();
	    acheck(a, "Bundle-SymbolicName", apipackage + "-api");
	    acheck(a, "Bundle-Version", nonfinal ?
		    (specversion + ".99." + buildname(specbuild)) :
		    osgispecimplversion);

	    // check Maven group ID, artifact ID, version
	    ZipEntry ze = jf.getEntry("META-INF/maven/" +
			apipackage + "/" + apipackage + "-api/pom.properties");
	    if (ze == null) {
		err("Missing pom.properties file");
		// see if it's there but with a different name, e.g.,
		// because the groupId or artifactId is wrong
		Enumeration<JarEntry> e = jf.entries();
		while (e.hasMoreElements()) {
		    JarEntry je = e.nextElement();
		    String ename = je.getName();
		    if (ename.startsWith("META-INF/maven/") &&
			ename.endsWith("/pom.properties")) {
			mavenCheck(jf.getInputStream(je),
			    nonfinal ? (newspecversion + "-" + buildname(specbuild)) :
				specimplversion,
			    apipackage, apipackage + "-api");
			break;
		    }
		}
	    } else {
		mavenCheck(jf.getInputStream(ze),
		    nonfinal ? (newspecversion + "-" + buildname(specbuild)) :
			specimplversion,
		    apipackage, apipackage + "-api");
	    }

	    acheck(a, "Extension-Name", apipackage);
	    if (nonfinal) {
		acheck(a, "Specification-Version",
		    specversion + ".99." + buildnum(specbuild));
		acheck(a, "Implementation-Version",
		    newspecversion + "-" + buildname(specbuild));
	    } else {
		acheck(a, "Specification-Version", specversion);
		acheck(a, "Implementation-Version", specimplversion);
	    }

	    checkClasses(jf, apipackage);

	    jf.close();
	}

	if (standalone && impljar == null)
	    impljar = prompt("Enter the implementation jar file name");
	if (impljar != null) {
	    System.out.println();
	    System.out.println("Checking implementation jar file: " + impljar);
	    File f;
	    String name;
	    if (impljar.startsWith("http")) {
		f = copyUrl(impljar);
		name = nameOf(impljar);
	    } else {
		f = new File(impljar);
		name = f.getName();
	    }
	    String impljarname = apipackage + ".jar";
	    String impljarmavenname =
		apipackage + "-" +
		    (nonfinal ? (newimplversion + "-" + buildname(implbuild)) :
				implversion) +
		    ".jar";
	    if (!(name.equals(impljarname) || name.equals(impljarmavenname)))
		err("Implementation jar file name should be " + impljarname +
		    " or " + impljarmavenname);
	    JarFile jf = new JarFile(f);
	    Manifest m = jf.getManifest();
	    Attributes a = m.getMainAttributes();
	    acheck(a, "Bundle-SymbolicName", implpackage + "." + apipackage);
	    acheck(a, "Bundle-Version", nonfinal ?
		(shortosgiimplversion + ".99." + buildname(implbuild)) :
		osgiimplversion);

	    // check Maven group ID, artifact ID, version
	    ZipEntry ze = jf.getEntry("META-INF/maven/" +
			implpackage + "/" + apipackage + "/pom.properties");
	    if (ze == null) {
		err("Missing pom.properties file");
		// see if it's there but with a different name, e.g.,
		// because the groupId or artifactId is wrong
		Enumeration<JarEntry> e = jf.entries();
		while (e.hasMoreElements()) {
		    JarEntry je = e.nextElement();
		    String ename = je.getName();
		    if (ename.startsWith("META-INF/maven/") &&
			ename.endsWith("/pom.properties")) {
			mavenCheck(jf.getInputStream(je),
			    nonfinal ? (newimplversion + "-" +
					    buildname(implbuild)) :
					implversion,
			    implpackage, apipackage);
			break;
		    }
		}
	    } else {
		mavenCheck(jf.getInputStream(ze),
		    nonfinal ? (newimplversion + "-" + buildname(implbuild)) :
				implversion,
		    implpackage, apipackage);
	    }

	    acheck(a, "Extension-Name", apipackage);
	    if (nonfinal) {
		acheck(a, "Specification-Version",
		    specversion + ".99." + buildnum(specbuild));
		acheck(a, "Implementation-Version",
		    newimplversion + "-" + buildname(implbuild));
	    } else {
		acheck(a, "Specification-Version", specversion);
		acheck(a, "Implementation-Version", implversion);
	    }

	    checkClasses(jf, apipackage, implpackage);

	    jf.close();
	}

	System.exit(errs);
    }

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
	errs++;
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

    /**
     * Return a Maven repo URL for the artifact.
     */
    private static String repoUrl(String groupId, String artifactId,
				    String version) {
	return repo + groupId.replace('.', '/') + "/" +
	    artifactId + "/" + version + "/" +
	    artifactId + "-" + version + ".jar";
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
}