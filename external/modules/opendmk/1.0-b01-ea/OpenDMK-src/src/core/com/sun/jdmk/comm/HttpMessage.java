/*
 * @(#)file      HttpMessage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.16
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
 * This abstract class represents an HTTP message. It is the base class 
 * of HttpRequest and HttpResponse.
 *
 * Fields of the HTTP message are represented by string and integer attributes.
 * The body of the HTTP message is represented by the HttpBody attribute.
 *
 * The HttpBody instance is specified in HttpMessage constructor: by
 * subclassing HttpBody it is possible to deal with different kinds
 * of body.
 *
 * An HttpMessage can be filled from an input stream using readFrom().
 * It can be written to an output stream using writeTo().
 *
 * readFrom() reads an HTTP PDU from a stream, extracts the data 
 * meaningful for JDMK and initializes the attributes of the HttpMessage.
 *
 * writeTo() formats an HTTP PDU using the attributes of HttpMessage
 * and writes this PDU on the specified stream. If an attribute is null,
 * the corresponding HTTP field is not inserted in the PDU.
 */

abstract class HttpMessage {

    //
    // Status line
    //
    String httpVersion = null ;

    /**
     * Initializes an empty message
     */
    HttpMessage(HttpBody body) {
	if (body == null)
	    throw new IllegalArgumentException("HttpBody cannot be null") ;
	this.body = body;
	setContentLength();
    }

    /**
     * Returns true if this message includes the keep-alive flag.
     */
    boolean hasKeepAliveFlag() {
	final String connection = getHeader(CONNECTION_HEADER);
	if (connection == null) {
	    return false;
	} else {
	    return connection.toLowerCase().equals("keep-alive");
	}
    }

    /**
     * Read the specified input stream and fill this message with 
     * the read data.
     */
    abstract void readFrom(InputStream s)
	throws IOException, MalformedHttpException ;

    /**
     * Write this header to the specified output stream.
     */	 
    abstract void writeTo(OutputStream s)
	throws IOException ;

    void setHeader(int headerNumber, String value) {
	headers[headerNumber] = value;
    }

    String getHeader(int headerNumber) {
	return headers[headerNumber];
    }

    void writeHeadersAndBodyTo(OutputStream s, String firstLine)
	    throws IOException {
	StringBuffer buffer = new StringBuffer();

	buffer.append(firstLine).append(CRLF);

	for (int i = 0; i < KNOWN_HEADERS.length; i++) {
	    final String header = getHeader(i);
	    if (header != null) {
		buffer.append(KNOWN_HEADERS[i]).append(": ").append(header)
		    .append(CRLF);
	    }
	}

        buffer.append(CRLF) ; // The mandatory empty line

	s.write(buffer.toString().getBytes());

	body.writeTo(s);
    }

    void readBodyFrom(InputStream s, int contentLength) throws IOException {
	body.readFrom(s, contentLength);
	setContentLength();
    }

    int getContentLength() {
	return body.getByteCount();
    }

    byte[] getContentBytes() {
	return body.getBytes();
    }

    private void setContentLength() {
	final int contentLength = body.getByteCount();
	final String contentLengthString =
	    (contentLength > 0) ? Integer.toString(contentLength) : null;
	setHeader(CONTENT_LENGTH_HEADER, contentLengthString);
    }

    //
    // Literals used to parse/format protocol data
    //
    static final String CRLF = "\r\n";

    /* YOU MUST UPDATE THE INTEGER CONSTANTS IF YOU UPDATE THIS ARRAY.  */
    static final String[] KNOWN_HEADERS = {
	"Content-Length",
	"Content-Type",
	"Date",
	"Connection",
	"WWW-Authenticate",
	"Authorization",
    };
    static final int
	CONTENT_LENGTH_HEADER = 0,
	CONTENT_TYPE_HEADER = 1,
	DATE_HEADER = 2,
	CONNECTION_HEADER = 3,
	WWW_AUTHENTICATE_HEADER = 4,
	AUTHORIZATION_HEADER = 5;

    //
    // The headers of the message, as unparsed strings.  Leading and trailing
    // blanks are suppressed.
    //
    private String[] headers = new String[KNOWN_HEADERS.length];

    //
    // This is the body of the message
    //
    private HttpBody body;
}
