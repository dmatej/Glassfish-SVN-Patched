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
 * Support for Windows .bat files.
 * No repair for now.
 *
 * @author	Bill Shannon
 */

package org.glassfish.copyright;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class BatCopyright extends AbstractCopyright {
    public BatCopyright(Copyright c) {
	super(c);
    }

    /**
     * Is this a Windows .bat file?
     */
    protected boolean supports(File file) {
	return file.getName().endsWith(".bat");
    }

    /**
     * Read the first comment block in the file.
     */
    protected String readComment(BufferedReader r) throws IOException {
	StringBuilder comment = new StringBuilder();
	String line;
	// skip blank lines at beginning of file
	while ((line = r.readLine()) != null) {
	    line = strip(line);
	    if (line.startsWith("@echo"))
		continue;
	    if (line.equals("REM"))
		continue;
	    if (line.length() != 0)
		break;
	}
	if (line == null || !line.startsWith("REM"))
	    return null;
	String prefix = "REM  ";
	do {
	    if (line.length() == 0)
		break;		// end of comment
	    if (!line.startsWith("REM"))
		break;		// end of comment
	    if (line.length() >= prefix.length()) {
		if (line.startsWith(prefix))
		    line = line.substring(prefix.length());
	    } else {
		if (prefix.startsWith(line))
		    line = "";
	    }
	    comment.append(strip(line)).append('\n');
	} while ((line = r.readLine()) != null);
	int len = comment.length();
	if (len >= 2 && comment.charAt(len - 1) == '\n' &&
		comment.charAt(len - 2) == '\n')
	    comment.setLength(len - 1);
	return comment.toString();
    }

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
	// no repair for bat files until CRLF is handled above
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
	    if (line.startsWith("@echo")) {
		header.append(line).append('\n');
		continue;
	    }
	    if (comment != null && line.equals("REM"))
		continue;
	    if (line.length() != 0)
		break;
	}

	if (header.length() > 0)
	    out.write(header.toString());
	if (comment != null && line != null && line.startsWith("#")) {
	    boolean sawCopyright = false;
	    do {
		if (line.length() == 0)
		    break;		// end of comment
		if (!line.startsWith("REM"))
		    break;		// end of comment
		if (!sawCopyright && line.indexOf("Copyright") >= 0) {
		    Matcher m = ypat.matcher(line);
		    if (m.find()) {
			lastChanged = addCopyrightDate(m.group(2), lastChanged);
			sawCopyright = true;
		    }
		}
	    } while ((line = in.readLine()) != null);
	}
	writeCopyright(out, lastChanged, comment);

	if (line != null) {
	    // the new copyright ends with a blank line so don't write another
	    if (line.length() > 0) {
		out.write(line);
		out.write('\n');		// line terminator
	    }
	    // have to copy the rest here so that blanks aren't skipped
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
	    if (line.startsWith("@echo")) {
		header.append(line).append('\n');
		continue;
	    }
	    if (line.equals("REM"))
		continue;
	    if (line.length() != 0)
		break;
	}
	if (line == null)
	    throw new IOException("NO CONTENT, repair failed");

	if (header.length() > 0)
	    out.write(header.toString());
	out.write(line);
	out.write('\n');		// line terminator
	if (line.startsWith("REM")) {
	    boolean updated = false;
	    do {
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
		if (line.length() == 0)
		    break;		// end of comment
		if (!line.startsWith("REM"))
		    break;		// end of comment
	    } while ((line = in.readLine()) != null);
	}
	if (line != null) {
	    out.write(line);
	    out.write('\n');		// line terminator
	    // have to copy the rest here so that blanks aren't skipped
	    copy(in, out, false);
	}
    }

    /**
     * Convert the comment text to .properties syntax.
     */
    protected String toComment(String comment) {
	BufferedReader r = new BufferedReader(new StringReader(comment));
	StringBuilder out = new StringBuilder();
	try {
	    out.append("REM\n");
	    String line;
	    while ((line = r.readLine()) != null)
		out.append(strip("REM  " + line)).append('\n');
	    out.append("REM\n\n");
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
