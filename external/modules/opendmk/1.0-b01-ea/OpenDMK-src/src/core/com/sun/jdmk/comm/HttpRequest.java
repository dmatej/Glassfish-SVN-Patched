/*
 * @(#)file      HttpRequest.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.17
 * @(#)lastedit      07/03/08
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
 * This class represents an HTTP request.
 *
 * readFrom() and writeTo() implements the parsing
 * and the formatting of an HTTP request.
 */

class HttpRequest extends HttpMessage {

    //
    // Request line
    //
    public int method = 0 ;
    public String requestURI = null ;

    //
    // The HTTP methods used in JDMK
    //
    public static final int METHOD_GET  = 1 ;
    public static final int METHOD_POST = 2 ;

    /**
     * Initializes an empty request.
     */
    public HttpRequest(HttpBody body) {
        super(body);
    }

    /**
     * Extracts the abs-path from the requestURI.
     */
    public String getURIPath() {
        return requestURI ;
        // REMIND: It might be an absolute URL, so in this case we
        //         should have to remove the 'http://host' prefix...
    }

    /**
     * Read the specified input stream and fill this message with 
     * the read data.
     */
    public void readFrom(InputStream s)
        throws IOException, MalformedHttpException {

        //
        // Reset all the header members
        //
        method = 0 ;
        requestURI = null ;
        httpVersion = null ;

	//
	// Skip the possible leading CRLFs as suggested in HTTP 1.1 draft.
	//
	String requestLine;
	do {
	    requestLine = readLine(s);
	} while (requestLine.length() == 0);

	//
	// Request line (method, requestURI and httpVersion)
	//
	int space1Index = requestLine.indexOf(' ') ;
	int space2Index = requestLine.indexOf(' ', space1Index+1) ;
	if ((space1Index == -1) || (space2Index == -1)) {
	    throw new MalformedHttpException(MALFORMED_REQUEST_LINE) ;
	}
	String methodStr = requestLine.substring(0, space1Index) ;
	if (methodStr.equals("GET")) {
	    method = METHOD_GET ;
	} else if (methodStr.equals("POST")) {
	    method = METHOD_POST ;
	} else {
	    throw new MalformedHttpException(UNSUPPORTED_METHOD) ;
	}
	requestURI = requestLine.substring(space1Index+1, space2Index) ;
	httpVersion = requestLine.substring(space2Index+1) ;
	if ((requestURI.length() == 0) || (httpVersion.length() == 0)) {
	    throw new MalformedHttpException(MALFORMED_REQUEST_LINE) ;
	}

	//
	// Header lines
	//
	while (true) {
	    String headerLine = readLine(s);
	    if (headerLine.length() == 0)
		break;
	    int colon = headerLine.indexOf(':');
	    if (colon < 0)
		throw new MalformedHttpException(MALFORMED_HEADER_LINE);
	    final String header = headerLine.substring(0, colon);

	    for (int i = 0; i < KNOWN_HEADERS.length; i++) {
		if (header.equalsIgnoreCase(KNOWN_HEADERS[i])) {
		    setHeader(i, headerLine.substring(colon + 1).trim());
		    break;
		}
	    }
	}

	//
	// Entity Body
	//
	final String contentLengthString =
	    getHeader(CONTENT_LENGTH_HEADER);
	if (contentLengthString != null) {
	    int contentLength;
	    try {
		contentLength = Integer.parseInt(contentLengthString);
	    } catch (NumberFormatException e) {
		throw new MalformedHttpException(e.toString());
	    }
	    if (contentLength > 0)
		readBodyFrom(s, contentLength);
	}
    }

    /**
     * Write this header to the specified output stream.
     */
    public void writeTo(OutputStream s) throws IOException {
	String methodString;
	switch (method) {
	case METHOD_GET: methodString = "GET"; break;
	case METHOD_POST: methodString = "POST"; break;
	default:
            throw new IllegalArgumentException("Unsupported method " + method);
        }
	final String firstLine =
	    methodString + " " + requestURI + " " + httpVersion;

	writeHeadersAndBodyTo(s, firstLine);
    }

    /**
     * Read one line from the input stream.
     * This method reads until CRLF is found. If EOF is reached,
     * EOFException is thrown.
     * The returned string does not include the CRLF chars.
     */
    static String readLine(InputStream s) throws IOException {
        StringBuffer result = new StringBuffer() ;
        int prevCh =  s.read() ;
        int ch = s.read() ;
        while ((ch != -1) && !((prevCh == '\r') && (ch == '\n'))) {
            result.append((char)prevCh) ;
            prevCh = ch ;
            ch = s.read() ;
        }
        if (ch == -1) {
            throw new EOFException() ;
        }
        return result.toString() ;
    }

    //
    // Exception messages
    //
    static final String MALFORMED_REQUEST_LINE = "Malformed request line" ;
    static final String MALFORMED_HEADER_LINE  = "Malformed header line" ;
    static final String UNSUPPORTED_METHOD     = "Unsupported HTTP method" ;

    //
    // Request header fields
    //
    static final String PREFIX_AUTHORIZATION   = "Authorization: " ;
}
