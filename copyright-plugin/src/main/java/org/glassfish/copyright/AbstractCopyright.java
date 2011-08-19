/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2010-2011 Oracle and/or its affiliates. All rights reserved.
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
 *
 * @author	Bill Shannon
 */

package org.glassfish.copyright;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public abstract class AbstractCopyright {
    protected Copyright c;	// our context and configuration

    private String correctCopyright;
    private Pattern cpat;

    private static String correctBSDCopyright;
    private static Pattern sunpat;
    private static Pattern apat;
    private static Pattern sunapat;
    private static Pattern anewpat;
    private static Pattern sunanewpat;
    private static Pattern mitspat;
    private static Pattern bpat;
    private static Pattern anpat;
    private static Pattern oapat;
    private static Pattern sunbpat;
    private static Pattern cnocepat;
    private static Pattern ocpat;
    private static Pattern oc2pat;
    private static Pattern njcpat;
    // find a valid copyright line
    protected static Pattern ypat =
	Pattern.compile("Copyright (\\(c\\) )?([-0-9, ]+) [A-Z]");
    // find the ending "*/" line
    private static Pattern endPat = Pattern.compile(" *\\*/");
    // find a secondary license (e.g., Apache)
    private static Pattern secLicPat =
	Pattern.compile("^.*This file incorporates work covered by",
	Pattern.MULTILINE);
    // find the copyright date place holder, and the rest of the line
    private static Pattern crpat =
	Pattern.compile("YYYY.*$", Pattern.MULTILINE);
    // find the word "copyright" or "(c)" in the text
    private static Pattern cspat =
	Pattern.compile("(\\b[Cc]opyright\\b|\\([Cc]\\))", Pattern.MULTILINE);
    // a pattern to detect an existing BSD copyright
    private static Pattern bsdpat = Pattern.compile(
	"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS",
	Pattern.MULTILINE);

    protected static final String licensor = "Oracle and/or its affiliates";
    protected static final String allrights = "All rights reserved.";
    protected static final String thisYear =
	"" + Calendar.getInstance().get(Calendar.YEAR);

    static {
	try {
	    correctBSDCopyright = getCopyrightText("bsd-copyright.txt");
	    sunpat = getCopyrightPattern("sun-cddl+gpl+ce-copyright.txt");
	    apat = getCopyrightPattern("cddl+gpl+ce+apache-copyright.txt");
	    sunapat = getCopyrightPattern("sun-cddl+gpl+ce+apache-copyright.txt");
	    anewpat = getCopyrightPattern("cddl+gpl+ce+apachenew-copyright.txt");
	    sunanewpat = getCopyrightPattern("sun-cddl+gpl+ce+apachenew-copyright.txt");
	    mitspat = getCopyrightPattern("cddl+gpl+ce+mitsallings-copyright.txt");
	    bpat = getCopyrightPattern("bsd-copyright.txt");
	    anpat = getCopyrightPattern("apache-copyright.txt");
	    oapat = getCopyrightPattern("oracle-apache-copyright.txt");
	    sunbpat = getCopyrightPattern("sun-bsd-copyright.txt");
	    cnocepat = getCopyrightPattern("cddl+gpl-copyright.txt");
	    ocpat = getCopyrightPattern("cddl-copyright.txt");
	    oc2pat = getCopyrightPattern("cddl2-copyright.txt");
	    njcpat = getNonjavaCopyrightPattern("cddl+gpl+ce-copyright.txt");
	} catch (IOException ex) {
	    throw new RuntimeException("Copyright resource missing", ex);
	}
    }

    public AbstractCopyright(Copyright c) {
	this.c = c;
	try {
	    if (c.correctTemplate != null) {
		correctCopyright = getCopyrightText(c.correctTemplate);
		cpat = getCopyrightPattern(c.correctTemplate);
	    } else {
		correctCopyright = getCopyrightText("cddl+gpl+ce-copyright.txt");
		cpat = getCopyrightPattern("cddl+gpl+ce-copyright.txt");
	    }
	} catch (IOException ex) {
	    throw new RuntimeException("Can't load copyright template", ex);
	}
    }

    /**
     * Does this class support this file?
     * Subclasses will use the file name, and possibly examine the
     * content of the file, to determine whether it's supported.
     */
    protected abstract boolean supports(File file);

    /**
     * Check a file for the correct copyright notice.
     */
    protected void checkCopyright(File file) throws IOException {
	String lc = null;
	if (c.skipNoSVN) {
	    lc = lastChanged(file.getPath());
	    if (lc.length() == 0)
		return;
	}

	BufferedReader r = null;
	String comment = null;
	try {
	    r = new BufferedReader(
		new InputStreamReader(new FileInputStream(file), "iso-8859-1"));
	    comment = readComment(r);
	    if (c.debug) {
		System.out.println("Comment for: " + file);
		System.out.println("---");
		System.out.println(comment);
		System.out.println("---");
	    }
	    if (c.warn && !c.quiet)
		warnCopyright(file, r);
	} finally {
	    if (r != null)
		r.close();
	}

	if (comment == null) {
	    err(file + ": No copyright");
	    c.nMissing++;
	    if (c.doRepair)
		repair(file, comment, RepairType.MISSING);
	    return;
	}
	if (comment.trim().length() == 0) {
	    err(file + ": Empty copyright");
	    c.nEmpty++;
	    if (c.doRepair)
		repair(file, comment, RepairType.MISSING);
	    return;
	}
	if (!cspat.matcher(comment).find()) {
	    err(file + ": No copyright");
	    c.nMissing++;
	    if (c.doRepair)
		repair(file, comment, RepairType.MISSING);
	    return;
	}
	if (!matches(cpat, comment) &&
		!matches(apat, comment) &&
		!matches(anewpat, comment) &&
		!matches(mitspat, comment) &&
		!matches(bpat, comment) &&
		!matches(anpat, comment) &&
		!matches(oapat, comment)) {
	    if (matches(sunpat, comment)) {
		err(file + ": Sun copyright");
		c.nSun++;
	    } else if (matches(sunapat, comment) ||
		    matches(sunanewpat, comment)) {
		err(file + ": Sun+Apache copyright");
		c.nSunApache++;
	    } else if (matches(sunbpat, comment)) {
		err(file + ": Sun BSD copyright");
		c.nSunBSD++;
	    } else if (matches(ocpat, comment) ||
		    matches(oc2pat, comment)) {
		err(file + ": Old CDDL copyright");
		c.nOldCDDL++;
	    } else if (matches(cnocepat, comment)) {
		err(file + ": CDDL+GPL-CE copyright");
		c.nNoCE++;
	    } else {
		err(file + ": Wrong copyright");
		c.nWrong++;
	    }
	    if (c.doRepair)
		repair(file, comment, RepairType.WRONG);
	    return;
	}

	// plain Apache header doesn't include a copyright notice
	if (matches(anpat, comment))
	    return;

	Matcher m = ypat.matcher(comment);
	if (!m.find()) {
	    err(file + ": No copyright year");
	    c.nNoYear++;
	    return;
	}
	if (c.ignoreYear)
	    return;
	String year = m.group(2);
	if (isModified(file.getPath()))
	    lc = thisYear;
	else if (lc == null)
	    lc = lastChanged(file.getPath());
	String lastYear = year.substring(year.length() - 4);
	if (!lastYear.equals(lc)) {
	    err(file + ": Copyright year is wrong; is " +
				lastYear + ", should be " + lc);
	    c.nDate++;
	    if (c.doRepair)
		repair(file, comment, RepairType.DATE);
	    return;
	}
    }

    /**
     * Does the string match the pattern?
     */
    protected boolean matches(Pattern pat, String s) {
	return pat.matcher(s).matches();
    }

    enum RepairType { MISSING, WRONG, DATE };

    /**
     * Repair the c.errors in the file.
     *
     * Repair cases and strategy:
     *
     *	Missing copyright
     *		Insert correct copyright
     *
     *	Wrong copyright
     *		Try to extract copyright date.
     *		Insert correct copyright.
     *
     *	Wrong date
     *		Update existing date in existing copyright.
     */
    protected void repair(File file, String comment, RepairType type)
				throws IOException {
	File newfile = new File(file.getParent(), file.getName() + ".new");
	BufferedReader in = null;
	BufferedWriter out = null;
	try {
	    in = new BufferedReader(new InputStreamReader(
				new FileInputStream(file), "iso-8859-1"));
	    out = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(newfile), "iso-8859-1"));
	    switch (type) {
	    case MISSING:
		replaceCopyright(in, out, null, thisYear);
		break;

	    case WRONG:
		replaceCopyright(in, out, comment, thisYear);
		break;

	    case DATE:
		updateCopyright(in, out, thisYear);
		break;
	    }
	    copy(in, out, true);

	    if (!c.dontUpdate) {
		in.close();
		out.close();

		// now copy the updated file back to the original
		in = new BufferedReader(new InputStreamReader(
				new FileInputStream(newfile), "iso-8859-1"));
		out = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(file), "iso-8859-1"));
		copy(in, out, false);
	    }
	} finally {
	    if (in != null)
		in.close();
	    if (out != null)
		out.close();
	    if (!c.dontUpdate)
		newfile.delete();
	}
    }

    /**
     * Write the correct copyright to "out", using "year" as the copyright date.
     * The original license may be supplied as "comment", from which a secondary
     * license may be extracted, and which determines whether the new license
     * is a BSD license or not.
     */
    protected void writeCopyright(BufferedWriter out, String year,
				String comment) throws IOException {
	String copyright = correctCopyright;
	String secondaryLicense = null;
	if (comment != null) {
	    if (bsdpat.matcher(comment).find())
		copyright = correctBSDCopyright;
	    else {	// no secondary license allowed with BSD license
		Matcher m = secLicPat.matcher(comment);
		if (m.find()) {
		    secondaryLicense = comment.substring(m.start());
		    if (secondaryLicense.length() > 0)
			copyright += "\n\n" + secondaryLicense;
		}
	    }
	}
	out.write(toComment(fixCopyright(copyright, year, licensor)));
    }

    /**
     * Skip the first comment block, replacing it with the correct copyright.
     * If the file starts with a "package" statement,
     * save it and write it out after the new copyright.
     */
    protected abstract void replaceCopyright(BufferedReader in,
			BufferedWriter out, String comment, String lastChanged)
			throws IOException;

    /**
     * Update the existing copyright statement, changing the copyright
     * year to include lastChanged.
     */
    protected abstract void updateCopyright(BufferedReader in,
				BufferedWriter out, String lastChanged)
				throws IOException;

    /**
     * Convert the comment text to the appropriate syntax.
     */
    protected abstract String toComment(String comment);

    /**
     * Read the first comment block in the file.
     */
    protected abstract String readComment(BufferedReader r) throws IOException;

    /**
     * Warn if there's another copyright statement in the file.
     */
    protected void warnCopyright(File file, BufferedReader in)
				throws IOException {
	String line;
	while ((line = in.readLine()) != null) {
	    Matcher m = ypat.matcher(line);
	    if (m.find()) {
		if (line.indexOf(licensor) < 0)
		    System.out.println(file +
				    ": WARNING: extra copyright: " + line);
	    }
	    /*
	     * XXX - too many false positives for this one
	    else if (line.indexOf("Copyright") >= 0)
		System.out.println(file +
				    ": WARNING: extra copyright word: " + line);
	    */
	}
    }

    /**
     * Copy "in" to "out", skipping blank lines at the beginning of "in" if
     * skipBlanks is true, and canonicalizing the line terminators to '\n'.
     */
    protected void copy(BufferedReader in, BufferedWriter out,
				boolean skipBlanks) throws IOException {
	String line;
	while ((line = in.readLine()) != null) {
	    if (skipBlanks) {
		if (line.trim().length() == 0)
		    continue;
		skipBlanks = false;
	    }
	    out.write(line);
	    out.write('\n');	// canonicalize line separator
	}
    }

    /**
     * Update the given copyright date to include the lastChanged date
     * (assumed to be a date greater than any date included in "date").
     * Return the updated date string.
     */
    protected String addCopyrightDate(String date, String lastChanged) {
	if (date.length() == 4) {	// singe year
	    if (!date.equals(lastChanged))
		date = date + "-" + lastChanged;
	} else {	// "2001-2007" or "2001,2003,2007"
	    String lastDate = date.substring(
			    date.length() - 4, date.length());
	    if (!lastDate.equals(lastChanged)) {
		if (date.charAt(date.length() - 5) == '-')
		    // strip off last year and replace it with lastChanged
		    date = date.substring(0, date.length() - 4) + lastChanged;
		else
		    // add range from last year to lastChanged
		    date = date + "-" + lastChanged;
	    }
	}
	return date;
    }

    /**
     * Update the copyright line in cr to use "date" as the copyright date
     * and "lic" as the licensor.
     */
    protected String fixCopyright(String cr, String date, String lic) {
	Matcher m = crpat.matcher(cr);
	return m.replaceFirst(date + " " + lic + ". " + allrights);
    }

    /**
     * Strip any trailing whitespace.
     */
    protected static String strip(String line) {
	for (int i = line.length() - 1; i >= 0; i--) {
	    char c = line.charAt(i);
	    if (c == ' ' || c == '\t')
		continue;
	    return line.substring(0, i + 1);
	}
	return "";
    }

    /**
     * Find the prefix (if any) for the current line.
     */
    protected static String findPrefix(String line) {
	for (int i = 0; i < line.length(); i++) {
	    char c = line.charAt(i);
	    if (Character.isLetterOrDigit(c) ||
		    c == '\"' || c == '[' || c == '(')	// end of prefix
		return line.substring(0, i);
	}
	return "";
    }

    /**
     * Like String.startsWith, but ignores case and skips leading whitespace.
     */
    protected static boolean startsWith(String s, String prefix) {
	return s.trim().regionMatches(true, 0, prefix, 0, prefix.length());
    }

    /**
     * Does the file start with the specified prefix?
     */
    protected static boolean startsWith(File file, String prefix) {
	BufferedReader r = null;
	try {
	    r = new BufferedReader(new FileReader(file));
	    int len = prefix.length();
	    for (int i = 0; i < len; i++) {
		int c;
		while ((c = r.read()) == '\r')
		    ;	// skip CR
		if (c != prefix.charAt(i))
		    return false;
	    }
	    /*
	    if (c.debug)
		System.out.println("File " + file + " starts with " + prefix);
	    */
	    return true;
	} catch (IOException ex) {
	    return false;
	} finally {
	    if (r != null)
		try {
		    r.close();
		} catch (IOException ioex) { }
	}
    }

    /**
     * Read a copyright regular expression from the named resource.
     * Assume the pattern is formatted as a Java comment, but canonicalize
     * the pattern to ignore language-specific comment characters.
     */
    private static Pattern getCopyrightPattern(String name) throws IOException {
	return copyrightToPattern(readCopyright(name, true));
    }

    /**
     * Read a copyright regular expression from the file.
     */
    private static Pattern getCopyrightPattern(File file) throws IOException {
	return copyrightToPattern(readCopyright(file, true));
    }

    private static Pattern copyrightToPattern(String comment) {
	StringBuilder copyright = new StringBuilder();
	// ignore stupid NetBeans template text
	copyright.append(
	    "(\\QTo change this template, choose Tools | Templates\n" +
	    "and open the template in the editor.\n" +
	    "\n\\E)?");
	copyright.append(comment);
	return Pattern.compile(copyright.toString(), Pattern.MULTILINE);
    }

    private static String readCopyright(String name, boolean pattern)
				throws IOException {
	BufferedReader r = null;
	try {
	    InputStream is = Copyright.class.getResourceAsStream(
				"/META-INF/copyright-templates/" + name);
	    if (is == null)
		is = Copyright.class.getResourceAsStream(name);
	    r = new BufferedReader(new InputStreamReader(is));
	    return readCopyrightStream(r, pattern);
	} finally {
	    if (r != null)
		r.close();
	}
    }

    private static String readCopyright(File file, boolean pattern)
				throws IOException {
	BufferedReader r = null;
	try {
	    r = new BufferedReader(new FileReader(file));
	    return readCopyrightStream(r, pattern);
	} finally {
	    if (r != null)
		r.close();
	}
    }

    private static String readCopyrightStream(BufferedReader r, boolean pattern)
				throws IOException {
	StringBuilder copyright = new StringBuilder();
	String line = r.readLine();	// read the "/*" line
	while ((line = r.readLine()) != null) {
	    if (line.equals(" */"))	// ending comment line
		break;
	    if (line.length() > 2)
		line = line.substring(3);	// strip " * "
	    else
		line = "";			// empty line
	    if (pattern) {
		line = Pattern.quote(line);
		if (line.indexOf("YYYY") >= 0)
		    line = line.replace("YYYY", "\\E[-0-9, ]+\\Q");
	    }
	    copyright.append(line).append('\n');
	}
	return copyright.toString();
    }

    /**
     * Read a copyright regular expression from the named resource.
     */
    private static Pattern getNonjavaCopyrightPattern(String name)
				throws IOException {
	String copyright = readCopyright(name, true);
	/*
	if (c.debug) {
	    System.out.println("Non-java pattern for " + name + ":");
	    System.out.println(copyright);
	    System.out.println();
	}
	*/
	return Pattern.compile(copyright.toString(), Pattern.MULTILINE);
    }

    /**
     * Read the copyright text from the named resource.
     */
    private static String getCopyrightText(String name) throws IOException {
	return readCopyright(name, false);
    }

    /**
     * Read the copyright text from the file.
     */
    private static String getCopyrightText(File file) throws IOException {
	return readCopyright(file, false);
    }

    /**
     * Exec the "svn info" command to get the date the file was
     * last changed.
     */
    protected String lastChanged(String file) throws IOException {
	if (c.mercurial)
	    return lastChangedHg(file);
	else if (c.git)
	    return lastChangedGit(file);
	else
	    return lastChangedSvn(file);
    }

    private static String lastChangedSvn(String file) throws IOException {
	final String lastChangedDate = "Last Changed Date: ";
	ProcessBuilder pb = new ProcessBuilder("svn", "info", file);
	pb.redirectErrorStream(true);
	Process p = pb.start();
	p.getOutputStream().close();
	BufferedReader r = new BufferedReader(new InputStreamReader(
						p.getInputStream()));
	String lcd = "";
	String line;
	while ((line = r.readLine()) != null) {
	    if (line.startsWith(lastChangedDate))
		lcd = line.substring(lastChangedDate.length(),
					lastChangedDate.length() + 4);
	}
	p.getInputStream().close();
	try {
	    p.waitFor();
	} catch (InterruptedException ex) {
	}
	return lcd;
    }

    private static String lastChangedHg(String file) throws IOException {
	ProcessBuilder pb = new ProcessBuilder("hg", "log", "--limit", "1",
				    "--template", "{date|shortdate}", file);
	pb.redirectErrorStream(true);
	Process p = pb.start();
	p.getOutputStream().close();
	BufferedReader r = new BufferedReader(new InputStreamReader(
						p.getInputStream()));
	String lcd = "";
	String line;
	// date returned in the form 2006-09-04
	while ((line = r.readLine()) != null) {
	    if (line.length() == 10 && Character.isDigit(line.charAt(0)))
		lcd = line.substring(0, 4);
	}
	p.getInputStream().close();
	try {
	    p.waitFor();
	} catch (InterruptedException ex) {
	}
	return lcd;
    }

    private static String lastChangedGit(String file) throws IOException {
        ProcessBuilder pb = new ProcessBuilder("git", "log", "-n", "1",
            "--date=local", file);
        pb.redirectErrorStream(true);
        Process p = pb.start();
        p.getOutputStream().close();
        BufferedReader r = new BufferedReader(new InputStreamReader(
            p.getInputStream()));
        String lcd = "";
        String line;
        // date returned in the form 2006-09-04
        while ((line = r.readLine()) != null) {
            if (line.startsWith("Date:")) {
                final String[] split = line.split(" ");
                lcd = split[split.length - 1];
            }
        }
        p.getInputStream().close();
        try {
            p.waitFor();
        } catch (InterruptedException ex) {
        }
        return lcd;
    }

    /**
     * Exec the "svn status" command to find out if the file has been
     * modified locally.
     */
    protected boolean isModified(String file) throws IOException {
	ProcessBuilder pb;
	if (c.mercurial)
	    pb = new ProcessBuilder("hg", "status", file);
	else
	    pb = new ProcessBuilder("svn", "status", file);
	pb.redirectErrorStream(true);
	Process p = pb.start();
	p.getOutputStream().close();
	BufferedReader r = new BufferedReader(new InputStreamReader(
						p.getInputStream()));
	boolean modified = false;
	String line;
	while ((line = r.readLine()) != null) {
	    if (line.startsWith("M"))
		modified = true;
	}
	p.getInputStream().close();
	try {
	    p.waitFor();
	} catch (InterruptedException ex) {
	}
	return modified;
    }

    protected void err(String s) {
	if (!c.quiet)
	    System.out.println(s);
	c.errors++;
    }
}
