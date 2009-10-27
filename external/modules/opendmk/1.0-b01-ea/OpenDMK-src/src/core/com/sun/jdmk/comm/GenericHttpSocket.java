/*
 * @(#)file      GenericHttpSocket.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.14
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
import java.net.*;

/**
 * The <CODE>GenericHttpSocket</CODE> class provides a wrap-up of the 
 * socket to be used for HTTP-based adaptors.
 *
 */

abstract class GenericHttpSocket implements Cloneable {
  
    // Adaptor Socket actions
    //-----------------------

    /**
     * Creates a new object of the same class as this object.
     * It then initializes each of the new object's fields by
     * assigning it the same value as the corresponding field
     * in this object. No constructor is called.
     * @return The new object.
     */
    public Object clone() {
        Object newAdp = null;
        try {
            newAdp = super.clone();
        } catch(CloneNotSupportedException e) {
        }
        return newAdp;
    }

    /**
     * Creates a socket to be used on the server side with the given port.
     * @param port The port number.
     * @return The adaptor socket.
     */
    public abstract GenericHttpSocket createServerSocket(int port);

    /**
     * Creates a socket to be used on the server side with the given port.
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to .
     * @return The adaptor socket.
     */
    // NPCTE fix for bug 4873785
    public abstract GenericHttpSocket createServerSocket(int port, InetAddress bindAddr);
    // end NPCTE fix for bugId 4873785

    /**
     * Creates a socket to be used on the client side. No port is provided
     * as the client does not "bind".
     * @return The adaptor socket.
     */
    public abstract GenericHttpSocket createClientSocket();

    /**
     * Gets the name of the protocol used.
     * @return The name of the protocol.
     */
    public abstract String getProtocol();

    /**
     * Binds to receive requests (usually used on server side).
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public abstract void doBind() throws IOException;

    /**
     * Unbinds (usually used on server side).
     * @exception IOException Signals that an I/O exception of some sort 
     * has occurred.
     */
    public abstract void doUnbind() throws IOException;

    /**
     * Connects to send a request (usually used on client side).
     * @param serverName The name of the server to connect the socket to.
     * @param serverPort The port number of the specified server.
     * @exception UnknownHostException The IP address of the specified host 
     *            could not be determined.
     * @exception IOException Signals that an I/O exception of some sort 
     *            has occurred.
     * @exception CommunicationException A communications problem occurred.
     */
    public abstract void doConnect(String serverName, int serverPort)
        throws UnknownHostException, IOException, CommunicationException;

    /**
     * Can be used on both server and client sides.
     * <P>On the client side, disconnects the socket used when connecting.
     * <P>On the server side, disconnects the socket involved in the
     * communication with the client; it's not usually the socket
     * used for binding.
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     * @exception CommunicationException A communications problem occurred.
     */
    public abstract void doDisconnect()
        throws IOException, CommunicationException;

    /**
     * Sends the given header and content to the peer.
     * On the client side, it's usually the initiated request,
     * and on the server side, it's the reply to the client's request.
     * @param header The header to be sent.
     * @param content The content to be sent.
     * @exception IOException Signals that an I/O exception of some sort 
     *            has occurred.
     */
    public abstract void doSend(String header, byte[] content) 
	throws IOException;

    /**
     * Waits for an incoming message.
     * <P>On the server side, waits for a request from the client,
     * and on the client side, waits for the reply to the client's request.
     * @return The input stream for this adaptor socket.
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public abstract InputStream doReceive() throws IOException;

    /**
     * Returns an input stream for this socket.
     * @return The input stream for this adaptor socket.
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public abstract InputStream doGetInputStream() throws IOException;

    /**
     * Returns an output stream for this socket.
     * @return The output stream for this adaptor socket.
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public abstract OutputStream doGetOutputStream() throws IOException;

    /**
     * Gets the local IP address.
     * @return The local IP address.
     */
    public abstract InetAddress getLocalAddress();

    /**
     * Gets the local port number.
     * @return The local port number.
     */
    public abstract int getLocalPort();

    /**
     * Gets the remote IP address.
     * @return The remote IP address.
     */
    public abstract InetAddress getRemoteAddress();

    /**
     * Gets the remote port number.
     * @return The remote port number.
     */
    public abstract int getRemotePort();
  
    /**
     * Gets setting for <CODE>Timeout</CODE>.
     * <p>This method returns <code>0</code> if timeout is disabled.
      * (i.e. timeout of infinity).</p>
     * <P>
     * The default value for timeout is 60000 milliseconds.
     * </P>
     * @return The current value of the <CODE>Timeout</CODE> property.
     */
    public int getTimeout() {
        return timeout;
    }
  
    /** 
     * Enables/disables <CODE>Timeout</CODE> with the specified timeout, 
     * in milliseconds.
     * <p>
     * With this option set to a non-zero timeout, a <CODE>read()</CODE> call
     * on the <CODE>InputStream</CODE> associated with this 
     * <CODE>Socket</CODE> will block for only this amount of time. If the
     * timeout expires, a <CODE>java.io.InterruptedIOException</CODE> is 
     * raised, though the <CODE>Socket</CODE> is still valid. 
     * The option must be enabled prior to entering the blocking
     * operation to have effect. The timeout must be > 0. A timeout of zero is
     * interpreted as an infinite timeout.
     * </p>
     * The default value for timeout is 60000 milliseconds.
     *
     * @param value The timeout value.
     */
    public void setTimeout(int value) {
        if ( value < 0 )
            timeout = 0;
        else 
            timeout = value;
    }

    // PRIVATE VARIABLES
    //------------------

    /**
     * The timeout
     */
    private int timeout = 60000;

    /**
     * The local port number.
     */
    protected int port = -1;
    // NPCTE fix for bug 4873785
    protected InetAddress  bindAddr = null;
    // end NPCTE fix for bugId 4873785
}
