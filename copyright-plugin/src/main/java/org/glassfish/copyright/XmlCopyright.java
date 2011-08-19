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
 * Support for files with XML syntax.
 *
 * @author	Bill Shannon
 */

package org.glassfish.copyright;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class XmlCopyright extends CommonCopyright {
    public XmlCopyright(Copyright c) {
	super(c);
	commentStart = "<!--";
	commentEnd = "-->";
	commentPrefix = "    ";
    }

    /**
     * Is this an XML file?
     */
    protected boolean supports(File file) {
	String fname = file.getName();
	if ((fname.endsWith(".xml") || fname.endsWith(".xsl") ||
		    fname.endsWith(".html") || fname.endsWith(".xhtml") ||
		    fname.endsWith(".dtd") || fname.endsWith(".xsd") ||
		    fname.endsWith(".wsdl") || fname.endsWith(".inc") ||
		    fname.endsWith(".jnlp") || fname.endsWith(".tld") ||
		    fname.endsWith(".xcs") || fname.endsWith(".jsf")) ||
		    fname.endsWith(".hs") || fname.endsWith(".jhm")) {
	    return true;
	}
	if (startsWith(file, "<?xml"))
	    return true;
	return false;
    }

    protected boolean isPreamble(String line) {
	return startsWith(line, "<?xml ") || startsWith(line, "<!DOCTYPE") ||
		startsWith(line, "<html") || startsWith(line, "<head>");
    }
}
