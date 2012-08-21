/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2010-2012 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
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

/**
 * Check that the copyright/license notice in a file is the correct one.
 * Optionally repair any that are wrong.
 *
 * Usage: java -jar copyright.jar
 *		[-w] -[y] [-r] [-n] [-s] [-m] [-g] [-c] [-q] [-j] [-x] [-p] [-t]
 *		[-O] [-X pat] [-C file] [-V] [files ...]
 *
 * Options:
 *	-w	suppress warnings
 *	-y	don't check that year is correct (much faster)
 *	-r	repair files that are wrong
 *	-n	with -r, leave the updated file in file.new
 *	-s	skip files not under SVN (slower)
 *	-m	use Mercurial instead of SVN
 *	-g	use git instead of SVN
 *	-c	count errors and print summary
 *	-q	don't print errors for each file
 *	-j	check Java syntax files
 *	-x	check XML syntax files
 *	-p	check properties syntax files
 *	-t	check other text files
 *	-O	use comma instead of dash in years when repairing files
 *	-X	exclude files matching pat (substring only)
 *	-C	file containing correct copyright template, using Java syntax
 *	-V	print version number
 *
 * @author	Bill Shannon
 */

package org.glassfish.copyright;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class Copyright {

    public boolean debug = false;
    public boolean warn = true;
    public boolean ignoreYear = false;
    public boolean useComma = false;
    public boolean doRepair = false;
    public boolean dontUpdate = false;
    public boolean skipNoSVN = false;
    public boolean mercurial = false;
    public boolean git = false;
    public static boolean count = false;
    public boolean quiet = false;
    public boolean doJava = false;
    public boolean doXml = false;
    public boolean doProps = false;
    public boolean doText = false;
    public File correctTemplate;

    public int nMissing;
    public int nEmpty;
    public int nSun;
    public int nSunApache;
    public int nSunBSD;
    public int nOldCDDL;
    public int nNoCE;
    public int nWrong;
    public int nNoYear;
    public int nDate;
    public int errors;

    public List<String> excludes = new ArrayList<String>();

    private AbstractCopyright javaCopyright;
    private AbstractCopyright xmlCopyright;
    private AbstractCopyright textCopyright;
    private AbstractCopyright propsCopyright;
    private AbstractCopyright batCopyright;
    private AbstractCopyright jspCopyright;

    public static final List<String> ignoredDirs =
		    Collections.unmodifiableList(
			Arrays.asList(".m2", ".svn", ".hg", ".git", "target"));

    private void init() {
	if (javaCopyright == null) {
	    javaCopyright = new JavaCopyright(this);
	    xmlCopyright = new XmlCopyright(this);
	    textCopyright = new TextCopyright(this);
	    propsCopyright = new PropertiesCopyright(this);
	    batCopyright = new BatCopyright(this);
	    jspCopyright = new JspCopyright(this);

	    if (!doJava && !doXml && !doProps && !doText)
		// by default, do them all
		doJava = doXml = doProps = doText = true;
	}
    }

    /**
     * Check the file.  If the file is a directory, recurse.
     */
    public void check(File file) throws IOException {
	init();
	check(file, false);
    }

    /**
     * Check a Maven project directory.
     * Skip subdirectories that contain a pom.xml file.
     */
    public void checkMaven(File file) throws IOException {
	init();
	if (!file.exists()) {
	    System.out.println(file + ": doesn't exist");
	    return;
	}
	if (!file.canRead()) {
	    System.out.println(file + ": can't read");
	    return;
	}
	if (file.isDirectory()) {
	    String name = file.getName();
	    if (ignoredDirs.contains(name))
		return;
	    File[] files = file.listFiles();
	    for (File f : files)
		check(f, true);
	} else
	    checkFile(file);
    }

    /**
     * Check the file.  If the file is a directory, recurse.
     * If skipMavenDir is true, skip directories that contain
     * a pom.xml file.
     */
    private void check(File file, boolean skipMavenDir) throws IOException {
	if (!file.exists()) {
	    System.out.println(file + ": doesn't exist");
	    return;
	}
	if (!file.canRead()) {
	    System.out.println(file + ": can't read");
	    return;
	}
	if (file.isDirectory()) {
	    String name = file.getName();
	    if (ignoredDirs.contains(name))
		return;
	    if (skipMavenDir) {
		File pom = new File(file, "pom.xml");
		if (pom.exists())
		    return;
	    }
	    File[] files = file.listFiles();
	    for (File f : files)
		check(f);
	} else
	    checkFile(file);
    }

    /**
     * Check the copyright in the named file.
     */
    private void checkFile(File file) throws IOException {
	// ignore empty files
	if (file.length() == 0)
	    return;

	String pname = file.getPath();
	for (String ex : excludes) {
	    if (pname.indexOf(ex) >= 0)
		return;
	}
	if (javaCopyright.supports(file)) {
	    if (debug)
		System.out.println("File " + file + " is a Java file");
	    if (doJava)
		javaCopyright.checkCopyright(file);
	} else if (jspCopyright.supports(file)) {
	    if (debug)
		System.out.println("File " + file + " is a JSP file");
	    if (doXml)
		jspCopyright.checkCopyright(file);
	} else if (xmlCopyright.supports(file)) {
	    if (debug)
		System.out.println("File " + file + " is an XML file");
	    if (doXml)
		xmlCopyright.checkCopyright(file);
	} else if (batCopyright.supports(file)) {
	    if (debug)
		System.out.println("File " + file + " is a BAT file");
	    if (doText)
		batCopyright.checkCopyright(file);
	} else if (propsCopyright.supports(file)) {
	    if (debug)
		System.out.println("File " + file + " is a properties file");
	    if (doProps)
		propsCopyright.checkCopyright(file);
	} else {
	    if (debug)
		System.out.println("File " + file + " is a text file");
	    if (doText)
		textCopyright.checkCopyright(file);
	}
    }

    public void addExclude(String ex) {
	if (ex == null || ex.length() == 0)
	    return;
	if (debug)
	    System.out.println("Add exclude: " + ex);
	excludes.add(ex);
    }

    public void addExcludes(String file) throws IOException {
	BufferedReader r = null;
	try {
	    r = new BufferedReader(new FileReader(file));
	    String line;
	    while ((line = r.readLine()) != null)
		addExclude(line);
	} finally {
	    try {
		if (r != null)
		    r.close();
	    } catch (IOException ioex) { }
	}
    }

    public static void main(String[] argv) throws Exception {
	Copyright c = new Copyright();

	int optind;
	for (optind = 0; optind < argv.length; optind++) {
	    if (argv[optind].equals("-d")) {
		c.debug = true;
	    } else if (argv[optind].equals("-w")) {
		c.warn = false;
	    } else if (argv[optind].equals("-y")) {
		c.ignoreYear = true;
	    } else if (argv[optind].equals("-O")) {
		c.useComma = true;
	    } else if (argv[optind].equals("-r")) {
		c.doRepair = true;
	    } else if (argv[optind].equals("-n")) {
		c.dontUpdate = true;
	    } else if (argv[optind].equals("-s")) {
		c.skipNoSVN = true;
	    } else if (argv[optind].equals("-m")) {
		c.mercurial = true;
	    } else if (argv[optind].equals("-g")) {
		c.git = true;
	    } else if (argv[optind].equals("-c")) {
		count = true;
	    } else if (argv[optind].equals("-q")) {
		c.quiet = true;
	    } else if (argv[optind].equals("-j")) {
		c.doJava = true;
	    } else if (argv[optind].equals("-x")) {
		c.doXml = true;
	    } else if (argv[optind].equals("-p")) {
		c.doProps = true;
	    } else if (argv[optind].equals("-t")) {
		c.doText = true;
	    } else if (argv[optind].equals("-X")) {
		String ex = argv[++optind];
		if (ex.startsWith("@"))
		    c.addExcludes(ex.substring(1));
		else
		    c.addExclude(ex);
	    } else if (argv[optind].equals("-C")) {
		c.correctTemplate = new File(argv[++optind]);
	    } else if (argv[optind].equals("-V")) {
		System.out.println("Version: " + Version.getVersion());
		System.exit(0);
	    } else if (argv[optind].equals("--")) {
		optind++;
		break;
	    } else if (argv[optind].startsWith("-")) {
		System.out.println("Usage: copyright " +
		    "[-w] [-y] [-r] [-n] [-s] [-m] [-c] [-q] [-j] " +
		    "[-x] [-p] [-t] [-O] [-V] [-X pat] [-C file] [files...]");
		System.out.println("\t-w\tsuppress warnings");
		System.out.println("\t-y\tdon't check that year is correct " +
				    "(much faster)");
		System.out.println("\t-r\trepair files that are wrong");
		System.out.println("\t-n\twith -r, leave the updated file in " +
				    "file.new");
		System.out.println("\t-s\tskip files not under SVN (slower)");
		System.out.println("\t-m\tuse Mercurial instead of SVN");
		System.out.println("\t-g\tuse Git instead of SVN");
		System.out.println("\t-c\tcount errors and print summary");
		System.out.println("\t-q\tdon't print errors for each file");
		System.out.println("\t-j\tcheck Java syntax files");
		System.out.println("\t-x\tcheck XML syntax files");
		System.out.println("\t-p\tcheck properties syntax files");
		System.out.println("\t-t\tcheck other text files");
		System.out.println("\t-O\tcomma instead of dash between years");
		System.out.println("\t-X\texclude files matching pat " +
				    "(substring only)");
		System.out.println("\t-C\tfile containing correct copyright " +
				    "template, using Java syntax");
		System.out.println("\t-V\tprint version number");
		System.exit(-1);
	    } else {
		break;
	    }
	}

	if (optind >= argv.length)
	    c.check(new File("."));
	else
	    while (optind < argv.length)
		c.check(new File(argv[optind++]));

	if (count)
	    summary(c);
	System.exit(c.errors);
    }

    /**
     * Print a summary of errors.
     */
    private static void summary(Copyright c) {
	if (c.errors == 0) {
	    System.out.println("No errors");
	    return;
	}

	if (!c.quiet)
	    System.out.println();

	if (c.nMissing > 0)
	    System.out.println("No Copyright:\t\t" + c.nMissing);
	if (c.nEmpty > 0)
	    System.out.println("Empty Copyright:\t" + c.nEmpty);
	if (c.nSun > 0)
	    System.out.println("Sun Copyright:\t" + c.nSun);
	if (c.nSunApache > 0)
	    System.out.println("Sun+Apache Copyright:\t" + c.nSunApache);
	if (c.nSunBSD > 0)
	    System.out.println("Sun BSD Copyright:\t" + c.nSunBSD);
	if (c.nOldCDDL > 0)
	    System.out.println("Old CDDL Copyright:\t" + c.nOldCDDL);
	if (c.nNoCE > 0)
	    System.out.println("Copyright without CE:\t" + c.nNoCE);
	if (c.nWrong > 0)
	    System.out.println("Wrong Copyright:\t" + c.nWrong);
	if (c.nNoYear > 0)
	    System.out.println("No Copyright Year:\t" + c.nNoYear);
	if (!c.ignoreYear && c.nDate > 0)
	    System.out.println("Wrong Copyright Date:\t" + c.nDate);
    }
}
