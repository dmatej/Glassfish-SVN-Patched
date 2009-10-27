/*
 * @(#)file      SASLInputStream.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.10
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
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
 */

package com.sun.jmx.remote.opt.security;

import javax.security.sasl.Sasl;
import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslServer;
import java.io.IOException;
import java.io.EOFException;
import java.io.InputStream;

import com.sun.jmx.remote.opt.util.ClassLogger;

/**
 * This class is used by clients of the Java SASL that need
 * to create streams using SaslClient's wrap/unwrap.
 */
public class SASLInputStream extends InputStream {

    private int recvMaxBufSize = 65536;
    private byte[] saslBuffer;	         // buffer for storing raw bytes
    private byte[] lenBuf = new byte[4]; // buffer for storing length
    private byte[] buf = new byte[0];    // buffer for storing processed bytes
    private int bufPos = 0;		 // read position in buf
    private InputStream in;		 // underlying input stream
    private SaslClient sc;
    private SaslServer ss;

    public SASLInputStream(SaslClient sc, InputStream in) throws IOException {
	super();
	this.in = in;
	this.sc = sc;
	this.ss = null;

	String str = (String) sc.getNegotiatedProperty(Sasl.MAX_BUFFER);
	if (str != null) {
	    try {
		recvMaxBufSize = Integer.parseInt(str);
	    } catch (NumberFormatException e) {
		throw new IOException(Sasl.MAX_BUFFER + 
		    " property must be numeric string: " + str);
	    }
	}
	saslBuffer = new byte[recvMaxBufSize];
    }

    public SASLInputStream(SaslServer ss, InputStream in) throws IOException {
	super();
	this.in = in;
	this.ss = ss;
	this.sc = null;

	String str = (String) ss.getNegotiatedProperty(Sasl.MAX_BUFFER);
	if (str != null) {
	    try {
		recvMaxBufSize = Integer.parseInt(str);
	    } catch (NumberFormatException e) {
		throw new IOException(Sasl.MAX_BUFFER + 
		    " property must be numeric string: " + str);
	    }
	}
	saslBuffer = new byte[recvMaxBufSize];
    }

    public int read() throws IOException {
	byte[] inBuf = new byte[1];
	int count = read(inBuf, 0, 1);
	if (count > 0) {
	    return inBuf[0];
	} else {
	    return -1;
	}
    }

    public int read(byte[] inBuf, int start, int count) throws IOException {

	if (bufPos >= buf.length) {
	    int actual = fill();   // read and unwrap next SASL buffer
	    while (actual == 0) {  // ignore zero length content
		actual = fill();
	    }
	    if (actual == -1) {
		return -1;    // EOF
	    }
	}

	int avail = buf.length - bufPos;
	if (count > avail) {
	    // Requesting more that we have stored
	    // Return all that we have; next invocation of read() will
	    // trigger fill()
	    System.arraycopy(buf, bufPos, inBuf, start, avail);
	    bufPos = buf.length;
	    return avail;
	} else {
	    // Requesting less than we have stored
	    // Return all that was requested
	    System.arraycopy(buf, bufPos, inBuf, start, count);
	    bufPos += count;
	    return count;
	}
    }

    /**
     * Fills the buf with more data by reading a SASL buffer, unwrapping it,
     * and leaving the bytes in buf for read() to return.
     * @return The number of unwrapped bytes available
     */
    private int fill() throws IOException {
	// Read in length of buffer
	int actual = readFully(lenBuf, 4);
	if (actual != 4) {
	    return -1;
	}
	int len = networkByteOrderToInt(lenBuf, 0, 4);

	if (len > recvMaxBufSize) {
	    throw new IOException(
		len + "exceeds the negotiated receive buffer size limit:" + 
		recvMaxBufSize);
	}

	if (logger.traceOn()) {
	    logger.trace("fill", "reading " + len + " bytes from network");
	}

	// Read SASL buffer
	actual = readFully(saslBuffer, len);
	if (actual != len) {
	    throw new EOFException("Expecting to read " + len +
		" bytes but got " + actual + " bytes before EOF");
	}

	// Unwrap
	if (sc != null)
	    buf = sc.unwrap(saslBuffer, 0, len);
	else
	    buf = ss.unwrap(saslBuffer, 0, len);

	bufPos = 0;

	return buf.length;
    }

    /**
     * Read requested number of bytes before returning.
     * @return The number of bytes actually read; -1 if none read
     */
    private int readFully(byte[] inBuf, int total) throws IOException {
	int count, pos = 0;

	if (logger.traceOn()) {
	    logger.trace("readFully", "readFully " + total + " from " + in);
	}

	while (total > 0) {
	    count = in.read(inBuf, pos, total);

	    if (logger.traceOn()) {
		logger.trace("readFully", "readFully read " + count);
	    }

	    if (count == -1 ) {
		return (pos == 0? -1 : pos);
	    }
	    pos += count;
	    total -= count;
	}
	return pos;
    }

    public int available() throws IOException {
	return buf.length - bufPos;
    }

    public void close() throws IOException {
	if (sc != null)
	    sc.dispose();
	else
	    ss.dispose();
	in.close();
    }

    /**
     * Returns the integer represented by 4 bytes in network byte order.
     */
    private int networkByteOrderToInt(byte[] buf, int start, int count) {
	if (count > 4) {
	    throw new IllegalArgumentException("Cannot handle more " +
					       "than 4 bytes");
	}
	int answer = 0;
        for (int i = 0; i < count; i++) {
	    answer <<= 8;
	    answer |= ((int)buf[start+i] & 0xff);
	}
	return answer;
    }

    private static final ClassLogger logger =
	new ClassLogger("javax.management.remote.misc", "SASLInputStream");
}
