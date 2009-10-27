/*
 * @(#)file      HttpSendOutputStream.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.5
 * @(#)date      07/04/04
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
 * The HttpSendOutputStream class is used by the HttpSendSocket class as
 * a layer on the top of the OutputStream it returns so that it can be
 * notified of attempts to write to it.  This allows the HttpSendSocket
 * to know when it should construct a new message.
 */
class HttpSendOutputStream extends FilterOutputStream {

    /**
     * The HttpSendSocket object that is providing this stream.
     */
    HttpSendSocket owner;

    /**
     * Create new filter on a given output stream.
     * @param out the OutputStream to filter from 
     * @param owner the HttpSendSocket that is providing this stream
     */
    public HttpSendOutputStream(OutputStream out, HttpSendSocket owner) throws IOException {
        super(out);
        this.owner = owner;
    }

    /**
     * Mark this stream as inactive for its owner socket, so the next time
     * a write is attempted, the owner will be notified and a new underlying
     * output stream obtained.
     */
    public void deactivate() {
        out = null;
    }

    /**
     * Write a byte of data to the stream.
     */
    public void write(int b) throws IOException {
        if (out == null)
            out = owner.writeNotify();
        out.write(b);
    }

    /**
     * Write a subarray of bytes.
     * @param b the buffer from which the data is to be written
     * @param off the start offset of the data
     * @param len the number of bytes to be written
     */
    public void write(byte b[], int off, int len) throws IOException {
        if (len == 0)
            return;
        if (out == null)
            out = owner.writeNotify();
        out.write(b, off, len);
    }

    /**
     * Flush the stream.
     */
    public void flush() throws IOException {
        if (out != null)
            out.flush();
    }

    /**
     * Close the stream.
     */
    public void close() throws IOException {
        flush();
        owner.close();
    }
}
