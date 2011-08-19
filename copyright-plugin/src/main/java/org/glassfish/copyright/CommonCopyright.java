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
 * Common support for files with comment syntax that uses separate
 * being and end markers.
 *
 * @author	Bill Shannon
 */

package org.glassfish.copyright;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public abstract class CommonCopyright extends AbstractCopyright {
    public CommonCopyright(Copyright c) {
	super(c);
    }

    // must be initialized by subclass
    protected String commentStart;
    protected String commentEnd;
    protected String commentPrefix;
    // Include blank lines after the start comment and before the end comment?
    protected boolean blankLines = true;
    // Move the preamble to after the copyright comment block?
    protected boolean movePreamble = false;

    /**
     * Read the first comment block in the file.
     */
    protected String readComment(BufferedReader r) throws IOException {
	StringBuilder comment = new StringBuilder();
	String line;
	// skip blank lines at beginning of file
	while ((line = r.readLine()) != null) {
	    line = strip(line);
	    if (isPreamble(line))
		continue;
	    if (line.length() != 0)
		break;
	}
	if (line == null || line.indexOf(commentStart) < 0)
	    return null;
	String prefix = null;
	while ((line = r.readLine()) != null) {
	    if (line.indexOf("/*") >= 0)
		continue;
	    // have we figured out what the prefix is for this block?
	    if (prefix == null) {
		if (line.length() == 0)
		    continue;
		prefix = findPrefix(line);
	    }
	    if (line.indexOf(commentEnd.trim()) >= 0)
		break;		// end of comment
	    if (line.indexOf("*/") >= 0)
		break;		// end of comment
	    if (line.length() >= prefix.length()) {
		if (line.startsWith(prefix))
		    line = line.substring(prefix.length());
	    } else {
		if (prefix.startsWith(line))
		    line = "";
	    }
	    comment.append(strip(line)).append('\n');
	}
	int len = comment.length();
	if (len >= 2 && comment.charAt(len - 1) == '\n' &&
		comment.charAt(len - 2) == '\n')
	    comment.setLength(len - 1);
	return comment.toString();
    }

    /**
     * Should this line be allowed before the first comment line?
     */
    protected boolean isPreamble(String line) {
	return false;
    }

    /**
     * Skip the first comment block, replacing it with the correct copyright.
     */
    protected void replaceCopyright(BufferedReader in,
			BufferedWriter out, String comment, String lastChanged)
			throws IOException {
	String line;
	StringBuilder header = new StringBuilder();
	// skip blank lines at beginning of file
	while ((line = in.readLine()) != null) {
	    line = strip(line);
	    if (isPreamble(line)) {
		header.append(line).append('\n');
		continue;
	    }
	    if (line.length() != 0)
		break;
	}

	if (header.length() > 0 && !movePreamble)
	    out.write(header.toString());
	if (comment != null && line != null &&
					line.indexOf(commentStart) >= 0) {
	    boolean sawCopyright = false;
	    String trailer = "";
	    while ((line = in.readLine()) != null) {
		if (!sawCopyright && line.indexOf("Copyright") >= 0) {
		    Matcher m = ypat.matcher(line);
		    if (m.find()) {
			lastChanged = addCopyrightDate(m.group(2), lastChanged);
			sawCopyright = true;
		    }
		}
		int i = line.indexOf(commentEnd.trim());
		if (i >= 0) {
		    i += commentEnd.trim().length();
		    trailer = line.substring(i).trim();
		    break;		// end of comment
		}
	    }
	    writeCopyright(out, lastChanged, comment);
	    out.write(trailer);
	    out.write("\n\n");	// line terminator and blank line
	    if (header.length() > 0 && movePreamble) {
		out.write(header.toString());
		out.write('\n');
	    }
	} else {
	    writeCopyright(out, lastChanged, comment);
	    out.write("\n\n");		// line terminator and blank line
	    if (header.length() > 0 && movePreamble) {
		out.write(header.toString());
		out.write('\n');
	    }
	    if (line != null)
		out.write(line);
	    out.write('\n');		// line terminator
	    copy(in, out, false);
	}
    }

    /**
     * Update the existing copyright statement, changing the copyright
     * year to include lastChanged.
     */
    protected void updateCopyright(BufferedReader in,
				BufferedWriter out, String lastChanged)
				throws IOException {
	String line;
	StringBuilder header = new StringBuilder();
	// skip blank lines at beginning of file
	while ((line = in.readLine()) != null) {
	    line = strip(line);
	    if (isPreamble(line)) {
		header.append(line).append('\n');
		continue;
	    }
	    if (line.length() != 0)
		break;
	}
	if (line == null)
	    throw new IOException("NO CONTENT, repair failed");

	if (header.length() > 0)
	    out.write(header.toString());
	out.write(line);
	out.write('\n');
	if (line.indexOf(commentStart) >= 0) {
	    boolean updated = false;
	    while ((line = in.readLine()) != null) {
		if (!updated && line.indexOf("Copyright") >= 0) {
		    Matcher m = ypat.matcher(line);
		    if (m.find()) {
			String y = addCopyrightDate(m.group(2), lastChanged);
			line = line.substring(0, m.start(2)) + y +
						line.substring(m.end(2));
			updated = true;
		    }
		}
		out.write(line);
		out.write('\n');
		if (line.indexOf(commentEnd.trim()) >= 0)
		    break;		// end of comment
	    }
	    out.write('\n');		// blank line
	}
    }

    /**
     * Convert the comment text to the appropriate syntax.
     */
    protected String toComment(String comment) {
	BufferedReader r = new BufferedReader(new StringReader(comment));
	StringBuilder out = new StringBuilder();
	try {
	    out.append(commentStart).append("\n");
	    if (blankLines)
		out.append(strip(commentPrefix)).append("\n");
	    String line;
	    while ((line = r.readLine()) != null)
		out.append(strip(commentPrefix + line)).append('\n');
	    if (blankLines)
		out.append(strip(commentPrefix)).append("\n");
	    //out.append(commentEnd).append("\n\n");
	    out.append(commentEnd);
	} catch (IOException ioex) {
	    // can't happen
	} finally {
	    try {
		r.close();
	    } catch (IOException ex) { }
	}
	return out.toString();
    }
}
