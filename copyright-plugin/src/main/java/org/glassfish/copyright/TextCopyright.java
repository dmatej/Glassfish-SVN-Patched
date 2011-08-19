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
 * Support for arbitrary text files.
 * No repair, because we don't know where the comment ends.
 *
 * @author	Bill Shannon
 */

package org.glassfish.copyright;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class TextCopyright extends AbstractCopyright {
    public TextCopyright(Copyright c) {
	super(c);
    }

    /**
     * Is this a plain text file?
     */
    protected boolean supports(File file) {
	return true;	// XXX - should check for text content
    }

    /**
     * Read the first comment block in a non-Java file.
     * Don't know how to do this so just return up to the first 100 lines.
     */
    protected String readComment(BufferedReader r) throws IOException {
	StringBuilder comment = new StringBuilder();
	String line;
	int nlines = 0;
	while ((line = r.readLine()) != null) {
	    String cline = canon(line);
	    if (comment.length() == 0) {
		// skip shell lines
		if (line.startsWith("#!"))
		    continue;
		// skip empty lines
		if (cline.length() == 0)
		    continue;
	    }
	    comment.append(cline).append('\n');
	    if (++nlines >= 100)
		break;
	}
	return comment.toString();
    }

    /**
     * Does the string match the pattern?
     * Since we don't know where the comment text might end,
     * we just insist that it match starting at the beginning
     * of the text.
     */
    protected boolean matches(Pattern pat, String s) {
	Matcher m = pat.matcher(s);
	return m.find() && m.start() == 0;
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
	// no repair for text files
    }

    /**
     * Skip the first comment block, replacing it with the correct copyright.
     * If the file starts with a "package" statement,
     * save it and write it out after the new copyright.
     */
    protected void replaceCopyright(BufferedReader in,
			BufferedWriter out, String comment, String lastChanged)
			throws IOException {
    }

    /**
     * Update the existing copyright statement, changing the copyright
     * year to include lastChanged.
     */
    protected void updateCopyright(BufferedReader in,
				BufferedWriter out, String lastChanged)
				throws IOException {
    }

    /**
     * Convert the comment text to the appropriate syntax.
     */
    protected String toComment(String comment) {
	return comment;
    }

    /**
     * Canonicalize line by removing leading special characters.
     * (Don't remove special characters that are known to occur
     * in text we care about.)
     */
    private static String canon(String line) {
	for (int i = 0; i < line.length(); i++) {
	    char c = line.charAt(i);
	    if (Character.isLetterOrDigit(c) ||
		    c == '\"' || c == '[' || c == '(')
		return line.substring(i).trim();
	}
	return "";
    }
}
