/*
 * @(#)BASE64Encoder.java	1.9 07/03/08
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

import java.io.OutputStream;
import java.io.IOException;

/**
 * Implements a BASE64 Character encoder as specified in RFC1521.
 * This RFC is part of the MIME specification as published by the Internet 
 * Engineering Task Force (IETF). Unlike some other encoding schemes, there 
 * is nothing in this encoding that indicates
 * where a buffer starts or ends.
 *
 * This means that the encoded text will simply start with the first line
 * of encoded text and end with the last line of encoded text.
 *
 * @see		CharacterEncoder
 * @see		BASE64Decoder
 */

class BASE64Encoder extends CharacterEncoder {
	
    /** This class encodes three bytes per atom. */
    protected int bytesPerAtom() {
	return (3);
    }

    /** 
     * This class encodes 57 bytes per line. This results in a maximum
     * of 57/3 * 4 or 76 characters per output line, not counting the
     * line termination.
     */
    protected int bytesPerLine() {
	return (57);
    }

    /** This array maps the characters to their 6 bit values */
    private final static char pem_array[] = {
	//       0   1   2   3   4   5   6   7
		'A','B','C','D','E','F','G','H', // 0
		'I','J','K','L','M','N','O','P', // 1
		'Q','R','S','T','U','V','W','X', // 2
		'Y','Z','a','b','c','d','e','f', // 3
		'g','h','i','j','k','l','m','n', // 4
		'o','p','q','r','s','t','u','v', // 5
		'w','x','y','z','0','1','2','3', // 6
		'4','5','6','7','8','9','+','/'  // 7
	};

    /** 
     * Takes three bytes of input and encodes it as 4
     * printable characters. Note that if the length in len is less
     * than three is encodes either one or two '=' signs to indicate
     * padding characters.
     */
    protected void encodeAtom(OutputStream outStream, byte data[], int offset, int len) 
	throws IOException {
	byte a, b, c;

	if (len == 1) {
	    a = data[offset];
	    b = 0;
	    c = 0;
	    outStream.write(pem_array[(a >>> 2) & 0x3F]);
	    outStream.write(pem_array[((a << 4) & 0x30) + ((b >>> 4) & 0xf)]);
	    outStream.write('=');
	    outStream.write('=');
	} else if (len == 2) {
	    a = data[offset];
	    b = data[offset+1];
	    c = 0;
	    outStream.write(pem_array[(a >>> 2) & 0x3F]);
	    outStream.write(pem_array[((a << 4) & 0x30) + ((b >>> 4) & 0xf)]);
	    outStream.write(pem_array[((b << 2) & 0x3c) + ((c >>> 6) & 0x3)]);
	    outStream.write('=');
	} else {
	    a = data[offset];
	    b = data[offset+1];
	    c = data[offset+2];
	    outStream.write(pem_array[(a >>> 2) & 0x3F]);
	    outStream.write(pem_array[((a << 4) & 0x30) + ((b >>> 4) & 0xf)]);
	    outStream.write(pem_array[((b << 2) & 0x3c) + ((c >>> 6) & 0x3)]);
	    outStream.write(pem_array[c & 0x3F]);
	}
    }
}
