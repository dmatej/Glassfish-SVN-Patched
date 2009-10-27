/*
 * @(#)file      HttpBody.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.15
 * @(#)lastedit  07/03/08
 *
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 *
 */


package com.sun.jdmk.comm;



// java import
//
import java.io.*;

/**
 * This class represents the body of an HTTP message.
 *
 * An HttpBody can return the size of its content and the content itself.
 * It can be also filled in from an input stream with readFrom() and it
 * can be written to an output stream with writeTo().
 */

class HttpBody {

    /**
     * Initializes an empty body.
     */
    public HttpBody() {
	this.body = null;
    }
  
    /**
     * Initializes this body with the specified content.
     */
    public HttpBody(byte[] body) {
	this.body = body;
    }

    /**
     * Returns the body content.
     */
    public byte[] getBytes() {
	return body;
    }

    /**
     * Returns the content length.
     */
    public int getByteCount() {
  	if (body == null) {
	    return 0;
	} else {
	    return body.length;
	}
    }

    /**
     * Reads the data from the specified input stream and initializes this body.
     */
    public void readFrom(InputStream s, int byteCount) throws IOException {
	body = new byte[byteCount];
        int n = 0;
        while (n < byteCount) {
            int c = s.read(body, n, byteCount - n);
            if (c < 0) {
                throw new EOFException();
	    }
	    n += c;
	}
    }

    /**
     * Writes this body to the specified output stream.
     */
    public void writeTo(OutputStream s) throws IOException {
  	if (body != null)
	    s.write(body);
    }

    /**
     * The byte array that contains the body.
     */
    protected byte[] body = null;
}
