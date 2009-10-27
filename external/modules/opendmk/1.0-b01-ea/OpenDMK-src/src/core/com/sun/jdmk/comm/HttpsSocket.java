/*
 * @(#)file      HttpsSocket.java
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
import java.util.*;
import java.lang.reflect.*;

// ssl import
//
import javax.net.*;
import javax.net.ssl.*;

// jdmk import
//
import com.sun.jdmk.*;
import com.sun.jdmk.internal.ClassLogger;

/**
 * The <CODE>HttpsSocket</CODE> class provides a wrap-up of the 
 * socket to be used for a HTTP-based connector using SSL.
 *
 */
class HttpsSocket extends GenericHttpSocket {

    /**
     * Constructs a HTTP/SSL connector socket.
     */
    public HttpsSocket() {
    }

    /**
     * Constructs a HTTP/SSL connector socket.
     * 
     * @param port The port number.
     */
    public HttpsSocket(int port) {
        this.port = port;
    }

    /**
     * Constructs a HTTP/SSL connector socket.
     * 
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to .
     */
    // NPCTE fix for bug 4873785
    public HttpsSocket(int port, InetAddress bindAddr) {
        this.port = port;
	this.bindAddr = bindAddr;
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Constructs a HTTP/SSL connector socket.
     * 
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to .
     * @param needClientAuth <code>true</code> to require client
     * authentication on SSL connections accepted by the server
     * socket created by this connector server; <code>false</code>
     * to not require client authentication.
     *
     * @since Java DMK 5.1
     */
    public HttpsSocket(int port, InetAddress bindAddr, boolean needClientAuth) {
        this.port = port;
	this.bindAddr = bindAddr;
        this.needClientAuth = needClientAuth;
    }

    // Connector Socket actions
    //-----------------------

    /**
     * Creates a socket to be used on the server side with the given port.
     *
     * @param port The port number.
     *
     * @return The connector socket.
     */
    public GenericHttpSocket createServerSocket(int port) {
        HttpsSocket newSocket = new HttpsSocket(port, null, needClientAuth);
        newSocket.setTimeout(getTimeout());
        return newSocket;
    }

    /**
     * Creates a socket to be used on the server side with the given port.
     *
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to .
     *
     * @return The connector socket.
     */
    // NPCTE fix for bug 4873785
    public GenericHttpSocket createServerSocket(int port, InetAddress bindAddr) {
        HttpsSocket newSocket = new HttpsSocket(port, bindAddr, needClientAuth);
        newSocket.setTimeout(getTimeout());
        return newSocket;
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Creates a socket to be used on the client side.
     * <P>
     * No port is provided as the client does not "bind".
     *
     * @return The connector socket.
     */
    public GenericHttpSocket createClientSocket() {
        HttpsSocket newSocket = new HttpsSocket(0);
        newSocket.setTimeout(getTimeout());
        return newSocket;
    }

    /**
     * Returns the name of the "protocol" used ("https").
     *
     * @return The string "https".
     */
    public String getProtocol() {
        return "https";
    }

    /**
     * Binds to receive requests (usually used on server side).
     *
     * @exception IOException Signals that an I/O exception of some sort has occurred.
     */
    public void doBind()
        throws IOException {

        int i;
        String cipher_suite;
        StringBuffer str_buffer;

        // Get SSL Server Socket Factory
        //
        serverSocketFactory = (SSLServerSocketFactory) SSLServerSocketFactory.getDefault();

        // Create SSL Server Socket
        //
	// NPCTE fix for bug 4873785
	if (bindAddr == null) {
            serverSocket = (SSLServerSocket) serverSocketFactory.createServerSocket(port, backlog);
        } else {
            serverSocket = (SSLServerSocket) serverSocketFactory.createServerSocket(port, backlog, bindAddr);
	}
	// end NPCTE fix for bugId 4873785

        // Set NeedClientAuthentication
        //
        serverSocket.setNeedClientAuth(needClientAuth);

        // Show the SSL Server Socket Supported Cipher Suites
        //
        String[] supported_cs = serverSocket.getSupportedCipherSuites();
        if (logger.finerOn()) {
            logger.finer("doBind","Supported Cipher Suites");
        }
        if (supported_cs != null) {
            str_buffer = new StringBuffer();
            for (i = 0; i < supported_cs.length; i++) {
                str_buffer.append(supported_cs[i]);
                if (i+1 < supported_cs.length)
                    str_buffer.append(", ");
            }
            if (logger.finerOn()) {
                logger.finer("doBind","[" + str_buffer + "]");
            }
        } else {
            if (logger.finerOn()) {
                logger.finer("doBind","[]");
            }
        }

        // Get User Specified Cipher Suites
        //
        Vector user_specified_cs_vector = new Vector();
        for (i = 1; (cipher_suite = System.getProperty(JdmkProperties.SSL_CIPHER_SUITE + String.valueOf(i))) != null; i++) {
            user_specified_cs_vector.addElement(cipher_suite);
        }
        if (logger.finerOn()) {
            logger.finer("doBind","User Specified Cipher Suites");
            logger.finer("doBind",user_specified_cs_vector.toString());
        }

        // Remove Non Supported Cipher Suites from User Specified Cipher Suites
        //
        Vector supported_user_specified_cs_vector = new Vector();
        if ((user_specified_cs_vector != null) && (user_specified_cs_vector.size() > 0)) {
            for (Enumeration e = user_specified_cs_vector.elements(); e.hasMoreElements(); ) {
                cipher_suite = (String) e.nextElement();
                if (supported_cs != null) {
                    for (i = 0; i < supported_cs.length; i++) {
                        if (supported_cs[i].equals(cipher_suite)) {
                            supported_user_specified_cs_vector.addElement(cipher_suite);
                            break;
                        }
                    }
                }
            }
        }
        if (logger.finerOn()) {
            logger.finer("doBind","Supported User Specified Cipher Suites");
            logger.finer("doBind",supported_user_specified_cs_vector.toString());
        }

        // Convert Supported Cipher Suites Vector to Array and Set SSL Server Socket Enabled Cipher Suites
        //
        if ((supported_user_specified_cs_vector != null) && (supported_user_specified_cs_vector.size() > 0)) {
            i = 0;
            String[] ecs = (String[]) Array.newInstance(String.class, supported_user_specified_cs_vector.size());
            for (Enumeration e = supported_user_specified_cs_vector.elements(); e.hasMoreElements(); ) {
                cipher_suite = (String) e.nextElement();
                Array.set(ecs, i, cipher_suite);
                i++;
            }
            serverSocket.setEnabledCipherSuites(ecs);
        }

        // Show the SSL Server Socket Enabled Cipher Suites
        //
        String[] enabled_cs = serverSocket.getEnabledCipherSuites();
        if (logger.finerOn()) {
            logger.finer("doBind","Enabled Cipher Suites");
        }
        if (enabled_cs != null) {
            str_buffer = new StringBuffer();
            for (i = 0; i < enabled_cs.length; i++) {
                str_buffer.append(enabled_cs[i]);
                if (i+1 < enabled_cs.length)
                    str_buffer.append(", ");
            }
            if (logger.finerOn()) {
                logger.finer("doBind","[" + str_buffer + "]");
            }
        } else {
            if (logger.finerOn()) {
                logger.finer("doBind","[]");
            }
        }
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
     * @exception UnknownHostException The IP address of the specified host 
     *            could not be determined.
     * @exception IOException Signals that an I/O exception of some sort 
     *            has occurred.
     * @exception CommunicationException A communications problem occurred.
     */
    public void doConnect(String serverName, int serverPort)
        throws UnknownHostException, IOException, CommunicationException {

        int i;
        String cipher_suite;
        StringBuffer str_buffer;

        // Get SSL Client Socket Factory
        //
        clientSocketFactory = (SSLSocketFactory) SSLSocketFactory.getDefault();

        // Create SSL Client Socket
        //
        clientSocket = (SSLSocket) clientSocketFactory.createSocket(serverName, serverPort);
        clientSocket.setSoTimeout(getTimeout());

        // Show the SSL Client Socket Supported Cipher Suites
        //
        String[] supported_cs = clientSocket.getSupportedCipherSuites();
        if (logger.finerOn()) {
            logger.finer("doConnect","Supported Cipher Suites");
        }
        if (supported_cs != null) {
            str_buffer = new StringBuffer();
            for (i = 0; i < supported_cs.length; i++) {
                str_buffer.append(supported_cs[i]);
                if (i+1 < supported_cs.length)
                    str_buffer.append(", ");
            }
            if (logger.finerOn()) {
                logger.finer("doConnect","[" + str_buffer + "]");
            }
        } else {
            if (logger.finerOn()) {
                logger.finer("doConnect","[]");
            }
        }

        // Get User Specified Cipher Suites
        //
        Vector user_specified_cs_vector = new Vector();
        for (i = 1; (cipher_suite = System.getProperty(JdmkProperties.SSL_CIPHER_SUITE + String.valueOf(i))) != null; i++) {
            user_specified_cs_vector.addElement(cipher_suite);
        }
        if (logger.finerOn()) {
            logger.finer("doConnect","User Specified Cipher Suites");
            logger.finer("doConnect",user_specified_cs_vector.toString());
        }

        // Remove Non Supported Cipher Suites from User Specified Cipher Suites
        //
        Vector supported_user_specified_cs_vector = new Vector();
        if ((user_specified_cs_vector != null) && (user_specified_cs_vector.size() > 0)) {
            for (Enumeration e = user_specified_cs_vector.elements(); e.hasMoreElements(); ) {
                cipher_suite = (String) e.nextElement();
                if (supported_cs != null) {
                    for (i = 0; i < supported_cs.length; i++) {
                        if (supported_cs[i].equals(cipher_suite)) {
                            supported_user_specified_cs_vector.addElement(cipher_suite);
                            break;
                        }
                    }
                }
            }
        }
        if (logger.finerOn()) {
            logger.finer("doConnect","Supported User Specified Cipher Suites");
            logger.finer("doConnect",supported_user_specified_cs_vector.toString());
        }

        // Convert Supported Cipher Suites Vector to Array and Set SSL Client Socket Enabled Cipher Suites
        //
        if ((supported_user_specified_cs_vector != null) && (supported_user_specified_cs_vector.size() > 0)) {
            i = 0;
            String[] ecs = (String[]) Array.newInstance(String.class, supported_user_specified_cs_vector.size());
            for (Enumeration e = supported_user_specified_cs_vector.elements(); e.hasMoreElements(); ) {
                cipher_suite = (String) e.nextElement();
                Array.set(ecs, i, cipher_suite);
                i++;
            }
            clientSocket.setEnabledCipherSuites(ecs);
        }

        // Show the SSL Client Socket Enabled Cipher Suites
        //
        String[] enabled_cs = clientSocket.getEnabledCipherSuites();
        if (logger.finerOn()) {
            logger.finer("doConnect","Enabled Cipher Suites");
        }
        if (enabled_cs != null) {
            str_buffer = new StringBuffer();
            for (i = 0; i < enabled_cs.length; i++) {
                str_buffer.append(enabled_cs[i]);
                if (i+1 < enabled_cs.length)
                    str_buffer.append(", ");
            }
            if (logger.finerOn()) {
                logger.finer("doConnect","[" + str_buffer + "]");
            }
        } else {
            if (logger.finerOn()) {
                logger.finer("doConnect","[]");
            }
        }
    }

    /**
     * Disconnects the connector socket.
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
     * @return The input stream for this connector socket.
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
     * Returns an input stream for this connector socket.
     *
     * @return The input stream from this connector socket.
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
     * Returns an output stream for this connector socket.
     *
     * @return The output stream from this connector socket.
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

    // TRACE STUFF
    //------------
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_LEGACY_HTTPS,
			"HttpsSocket");

    // PRIVATE VARIABLES
    //------------------

    private int backlog = 10;
    private Socket sckAccept = null;
    private SSLSocket clientSocket = null;
    private InputStream inputStream  = null;
    private OutputStream outputStream = null;
    private SSLServerSocket serverSocket = null;
    private SSLSocketFactory clientSocketFactory = null;
    private SSLServerSocketFactory serverSocketFactory = null;
    /**
     * The SSL need client authentication flag
     */
    private boolean needClientAuth = true;
}
