/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011-2012 Oracle and/or its affiliates. All rights reserved.
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

package org.glassfish.copyright;

import java.io.*;
import java.util.*;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.model.Resource;
import org.codehaus.plexus.resource.ResourceManager;
import org.codehaus.plexus.resource.loader.FileResourceCreationException;
import org.codehaus.plexus.resource.loader.FileResourceLoader;
import org.codehaus.plexus.resource.loader.ResourceNotFoundException;
import org.codehaus.plexus.util.FileUtils;

/**
 * Check copyrights of files.
 */
public abstract class AbstractCopyrightMojo extends AbstractMojo {
    /**
     * File(s) containing exclude patterns. <p>
     *
     * This parameter is resolved as a resource, then a URL, then a file.
     * This is a comma-separated list.
     *
     * @parameter expression="${copyright.exclude}"
     */
    protected String excludeFile;

    /**
     * Exclude pattern list.
     *
     * @parameter
     */
    protected String[] exclude;

    /**
     * Base directory for project.
     * Should not need to be set.
     *
     * @parameter default-value="${project.basedir}"
     */
    protected File baseDirectory;

    /**
     * Source directory.
     *
     * @parameter default-value="${project.build.sourceDirectory}"
     */
    protected File sourceDirectory;

    /**
     * Resources.
     *
     * @parameter default-value="${project.resources}"
     */
    protected ArrayList<Resource> resources;

    /**
     * Check all files in project or just the source and resource
     * files maven knows about?
     *
     * @parameter expression="${copyright.mavenonly}"
     */
    protected boolean mavenOnly;

    /**
     * Select SCM system - svn (default), mercurial, git.
     *
     * @parameter expression="${copyright.scm}"
     */
    protected String scm;

    /**
     * Turn on debugging.
     *
     * @parameter expression="${copyright.debug}"
     */
    protected boolean debug;

    /**
     * Turn off warnings.
     *
     * @parameter expression="${copyright.warn}" default-value="true"
     */
    protected boolean warn = true;

    /**
     * Don't check that the year is correct?
     *
     * @parameter expression="${copyright.ignoreyear}"
     */
    protected boolean ignoreYear;

    /**
     * Normalize format of repaired copyright to match template?
     *
     * @parameter expression="${copyright.normalize}"
     */
    protected boolean normalize;

    /**
     * Use comma instead of dash between years?
     *
     * @parameter expression="${copyright.usecomma}"
     */
    protected boolean useComma;

    /**
     * Skip files not under SCM?
     *
     * @parameter expression="${copyright.scmonly}"
     */
    protected boolean scmOnly;

    /**
     * Copyright template file.
     *
     * @parameter expression="${copyright.template}"
     */
    protected String templateFile;

    /**
     * Alternate copyright template file.
     *
     * @parameter expression="${copyright.alternatetemplate}"
     */
    protected String alternateTemplateFile;

    /**
     * Log output, initialize this in the execute method.
     */
    protected Log log;

    /**
     * @component
     * @required
     * @readonly
     */
    private ResourceManager resourceManager;

    /**
     * Initialize the Copyright object with the options from this mojo.
     */
    protected void initializeOptions(Copyright c) {
	if (excludeFile != null) {
	    String[] files = excludeFile.split(",");
	    for (String file : files) {
		log.debug("copyright: exclude file: " + file);
		String rfile = getResourceFile(file).getPath();
		try {
		    c.addExcludes(rfile);
		} catch (IOException ex) {
		    log.warn("Failed to add excludes from file: " + file, ex);
		}
	    }
	}
	if (exclude != null) {
	    for (String ex : exclude) {
		log.debug("copyright: exclude pattern: " + ex);
		c.addExclude(ex);
	    }
	}

	if (scm == null || scm.equalsIgnoreCase("svn"))
	    ;	// nothing to do, default case
	else if (scm.equalsIgnoreCase("mercurial") ||
		    scm.equalsIgnoreCase("hg"))
	    c.mercurial = true;
	else if (scm.equalsIgnoreCase("git"))
	    c.git = true;
	else
	    log.warn("Unknown SCM system ignored: " + scm);

	c.debug = debug;
	c.warn = warn;
	c.ignoreYear = ignoreYear;
	c.normalize = normalize;
	c.useComma = useComma;
	c.skipNoSVN = scmOnly;

	if (templateFile != null)
	    c.correctTemplate = 
		new File(getResourceFile(templateFile).getPath());
	if (alternateTemplateFile != null)
	    c.alternateTemplate = 
		new File(getResourceFile(alternateTemplateFile).getPath());
    }

    /**
     * Run the copyright checker using the specified options
     * on the specified files in this project.
     */
    protected void check(Copyright c) throws MojoExecutionException {
	try {
	    if (mavenOnly)
		checkMaven(c);
	    else
		checkAll(c);
	} catch (IOException ioex) {
	    log.error("IOException: " + ioex);
	    throw new MojoExecutionException(
			    "IOException while checking copyrights", ioex);
	}
    }

    /**
     * Only check the source files and resource files, and the pom.xml.
     */
    private void checkMaven(Copyright c) throws IOException {
	/*
	 * This seems like the right way, but it misses many files
	 * in the project directory.
	 */
	log.debug("copyright: base directory: " + baseDirectory);
	if (baseDirectory.exists()) {
	    File pom = new File(baseDirectory, "pom.xml");
	    c.check(pom);
	}
	log.debug("copyright: source directory: " + sourceDirectory);
	if (sourceDirectory.exists())
	    c.check(sourceDirectory);

	if (resources != null) {
	    /*
	     * Iterate over all the resources, and all the files in each
	     * resource, taking into account any includes and excludes.
	     */
	    for (Resource r : resources) {
		File dir = new File(r.getDirectory());
		List<String> incl = r.getIncludes();
		List<String> excl = r.getExcludes();
		if (log.isDebugEnabled()) {
		    log.debug("copyright: resource directory: " + dir);
		    log.debug("copyright:   includes: " + incl);
		    log.debug("copyright:   excludes: " + excl);
		}
		// XXX - need to add the ignored directories to the exclude
		// list, otherwise FileUtils.getFiles will return files in
		// those directories
		for (String ig : Copyright.ignoredDirs)
		    excl.add("**/" + ig + "/**");
		if (dir.exists()) {
		    List<File> files = FileUtils.getFiles(dir,
						commaSeparated(incl),
						commaSeparated(excl), true);
		    if (log.isDebugEnabled())
			log.debug("copyright:   files: " + files);
		    for (File f : files)
			c.check(f);
		}
	    }
	}
    }

    /**
     * Check all the files in the project, skipping files in
     * subdirectories that are also maven projects.
     */
    private void checkAll(Copyright c) throws IOException {
	/*
	 * The simple way - just check every file in the project.
	 */
	log.debug("copyright: base directory: " + baseDirectory);
	if (baseDirectory.exists())
	    c.checkMaven(baseDirectory);
    }

    /**
     * Get the File reference for a File passed in as a string reference.
     *
     * @param resource
     *            The file for the resource manager to locate
     * @return The File of the resource
     *
     */
    protected File getResourceFile(String resource) {

	assert resource != null;

	log.debug("resource is " + resource);

	try {
	    File resourceFile = resourceManager.getResourceAsFile(resource);
	    log.debug("copyright: location of file is " + resourceFile);
	    return resourceFile;
	} catch (ResourceNotFoundException ex) {
	    return new File(resource);
	} catch (FileResourceCreationException ex) {
	    return new File(resource);
	}
    }

    /**
     * Convert a list of strings into a comma separated list in a single string.
     */
    private static String commaSeparated(List<String> l) {
	if (l == null || l.size() == 0)
	    return null;
	StringBuilder sb = new StringBuilder();
	for (String s : l) {
	    if (sb.length() > 0)
		sb.append(',');
	    sb.append(s);
	}
	return sb.toString();
    }
}
