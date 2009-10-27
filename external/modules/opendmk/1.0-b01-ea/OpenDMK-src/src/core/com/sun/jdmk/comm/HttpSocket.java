/*
 * @(#)file      HttpSocket.java
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
 * The <CODE>HttpSocket</CODE> class provides a wrap-up of the 
 * socket to be used for a HTTP-based adaptor using TCP.
 *
 */
class HttpSocket extends GenericHttpSocket {

    /**
     * Constructs a HTTP/TCP adaptor socket.
     */
    public HttpSocket() {
    }

    /**
     * Constructs a HTTP/TCP adaptor socket.
     * 
     * @param port The port number.
     */
    public HttpSocket(int port) {
        this.port = port;
    }

    /**
     * Constructs a HTTP/TCP adaptor socket.
     * 
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to .
     */
    // NPCTE fix for bug 4873785
    public HttpSocket(int port, InetAddress bindAddr) {
        this.port = port;
	this.bindAddr = bindAddr;
    }
    // end NPCTE fix for bugId 4873785

    // Adaptor Socket actions
    //-----------------------

    /**
     * Creates a socket to be used on the server side with the given port.
     *
     * @param port The port number.
     *
     * @return The adaptor socket.
     */
    public GenericHttpSocket createServerSocket(int port) {
        HttpSocket newSocket = new HttpSocket(port);
        newSocket.setTimeout(getTimeout());
        return newSocket;
    }

    /**
     * Creates a socket to be used on the server side with the given port.
     *
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to .
     *
     * @return The adaptor socket.
     */
    // NPCTE fix for bug 4873785
    public GenericHttpSocket createServerSocket(int port, InetAddress bindAddr) {
	    HttpSocket newSocket = new HttpSocket(port, bindAddr);
        newSocket.setTimeout(getTimeout());
        return newSocket;
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Creates a socket to be used on the client side.
     * <P>
     * No port is provided as the client does not "bind".
     *
     * @return The adaptor socket.
     */
    public GenericHttpSocket createClientSocket() {
        HttpSocket newSocket = new HttpSocket(0);
        newSocket.setTimeout(getTimeout());
        return newSocket;
    }

    /**
     * Returns the name of the "protocol" used ("http").
     *
     * @return The string "http".
     */
    public String getProtocol() {
        return "http";
    }

    /**
     * Binds to receive requests (usually used on server side).
     *
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public void doBind()
        throws IOException {

	// NPCTE fix for bug 4873785
	if (bindAddr == null) {
            serverSocket = new ServerSocket(port, backlog);
        } else {
            serverSocket = new ServerSocket(port, backlog, bindAddr);
	}
	// end NPCTE fix for bugId 4873785
    }

    /**
     * Unbinds (usually used on server side).
     *
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public void doUnbind()
        throws IOException {

        serverSocket.close();
    }

    /**
     * Connects to send a request (usually used on client side).
     *
     * @param serverName The name of the server to connect the socket to.
     * @param serverPort The port number of the specified server.
     *
     * @exception UnknownHostException The IP address of the specified 
     *            host could not be determined.
     * @exception IOException Signals that an I/O exception of some sort 
     *            has occurred.
     * @exception CommunicationException A communications problem occurred.
     */
    public void doConnect(String serverName, int serverPort)
        throws UnknownHostException, IOException, CommunicationException {

        clientSocket = new Socket(serverName, serverPort);
        clientSocket.setSoTimeout(getTimeout());
    }

    /**
     * Disconnects the adaptor socket.
     * <P>
     * Can be used on both server and client sides.
     * On the client side, disconnects the socket used when connecting.
     * On the server side, disconnects the socket involved in the
     * communication with the client; it's not usually the socket
     * used for binding.
     *
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     * @exception CommunicationException A communications problem occurred.
     */
    public void doDisconnect()
        throws IOException, CommunicationException {

        if (serverSocket != null) {
            // Server side : disconnect from the client
            // The socket regarding the connection with the client is that
            // returned from the accept() call
            sckAccept.close();
            sckAccept = null;
        } else if (clientSocket != null) {
            // Client side : disconnect connection from the server
            // The socket regarding the connection with the server is that
            // returned from the doConnect() call
            clientSocket.close();
            clientSocket = null;
        }
    }

    /**
     * Sends the given header and content to the peer.
     * <P>
     * On the client side, it's usually the initiated request,
     * and on the server side, it's the reply to the client's request.
     *
     * @param header  The header to be sent.
     * @param content The content to be sent.
     *
     * @exception IOException Signals that an I/O exception of some sort 
     *            has occurred.
     */
    public void doSend(String header, byte[] content)
        throws IOException {

        if (serverSocket != null) {
            outputStream = sckAccept.getOutputStream();        
        } else {
            outputStream = clientSocket.getOutputStream();
        }
        outputStream.write(header.getBytes());
        if (content != null) {
            outputStream.write(content);
        }
        outputStream.flush();
    }

    /**
     * Waits for an incoming message.
     * <P>
     * On the server side, wait for a request from the client,
     * and on the client side, wait for the reply to the client's request.
     *
     * @return The input stream for this adaptor socket.
     *
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public InputStream doReceive()
        throws IOException {

        if (serverSocket != null) {
            sckAccept = serverSocket.accept();
            sckAccept.setSoTimeout(getTimeout());
            inputStream = sckAccept.getInputStream();
        } else {
            inputStream = clientSocket.getInputStream();
        }
        return inputStream;
    }

    /**
     * Returns an input stream for this adaptor socket.
     *
     * @return The input stream from this adaptor socket.
     *
     * @exception IOException An I/O error occurred.
     */
    public InputStream doGetInputStream()
        throws IOException {

        if (serverSocket != null) {
            inputStream = sckAccept.getInputStream();
        } else {
            inputStream = clientSocket.getInputStream();
        }
        return inputStream;
    }

    /**
     * Returns an output stream for this adaptor socket.
     *
     * @return The output stream from this adaptor socket.
     *
     * @exception IOException An I/O error occurred.
     */
    public OutputStream doGetOutputStream()
        throws IOException {

        if (serverSocket != null) {
            outputStream = sckAccept.getOutputStream();
        } else {
            outputStream = clientSocket.getOutputStream();
        }
        return outputStream;
    }

    /**
     * Returns the local IP address.
     *
     * @return The local IP address.
     */
    public InetAddress getLocalAddress() {
        if (serverSocket != null) {
            return serverSocket.getInetAddress();
        } else {
            return clientSocket.getLocalAddress();
        }
    }

    /**
     * Returns the local port number.
     *
     * @return The local port number.
     */
    public int getLocalPort() {
        if (serverSocket != null) {
            return serverSocket.getLocalPort();
        } else {
            return clientSocket.getLocalPort();
        }
    }

    /**
     * Returns the remote IP address.
     *
     * @return The remote IP address.
     */
    public InetAddress getRemoteAddress() {
        if (serverSocket != null) {
            return sckAccept.getInetAddress();
        } else {
            return clientSocket.getInetAddress();
        }
    }

    /**
     * Returns the remote port number.
     *
     * @return The remote port number.
     */
    public int getRemotePort() {
        if (serverSocket != null) {
            return sckAccept.getPort();
        } else {
            return clientSocket.getPort();
        }
    }

    /**
     * Returns the implementation address and implementation port of this socket as a string.
     * 
     * @return A string containing the implementation address and implementation port of this socket.
     */
    public String toString() {
        return serverSocket.toString();
    }

    // PRIVATE VARIABLES
    //------------------

    private int          backlog      = 10;
    private Socket       sckAccept    = null;
    private Socket       clientSocket = null;
    private InputStream  inputStream  = null;
    private OutputStream outputStream = null;
    private ServerSocket serverSocket = null;
}
