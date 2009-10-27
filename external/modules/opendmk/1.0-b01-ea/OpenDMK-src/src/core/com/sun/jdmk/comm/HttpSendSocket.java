/*
 * @(#)file      HttpSendSocket.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.19
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

// NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
import java.util.Properties;
// End NPCTE fix for bug 4653805

// NPCTE fix for bug 4653805, escalation 535848, SD, 11 Jun 02
import com.sun.jdmk.*;
// End NPCTE fix for bug 4653805

import com.sun.jdmk.internal.ClassLogger;

class HttpSendSocket extends Socket {

    /**
     * The host to connect to.
     */
    protected String host;

    /**
     * The port to connect to.
     */
    protected int port;

    /**
     * The URL to forward through.
     */
    protected URL url;

    /**
     * The connector client that requests the creation of this socket.
     */
    protected GenericHttpConnectorClient connector;

    /**
     * The object managing this connection through the URL.
     */
    protected URLConnection conn = null;

    // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
    protected TimedURLConnection timedConn = null;
    static private boolean got_timedProperty = false;
    static private boolean useTimedURLConnection = false;
    static private Integer urlConn_timeoutvalue = null;
    // End NPCTE fix for bug 4653805

    /**
     * Internal input stream for this socket.
     */
    protected InputStream in = null;

    /**
     * Internal output stream for this socket.
     */
    protected OutputStream out = null;

    /**
     * The notifying input stream returned to users.
     */
    protected HttpSendInputStream inNotifier;

    /**
     * The notifying output stream returned to users.
     */
    protected HttpSendOutputStream outNotifier;

    /**
     * Create a stream socket and connect it to the specified port on
     * the specified host.
     *
     * @param host the host
     * @param port the port
     * @param url the url
     * @param connector the HttpConnectorClient.
     */
    public HttpSendSocket(String host, int port, URL url, 
			  GenericHttpConnectorClient connector) 
	throws IOException {
        // super(null); // No underlying SocketImpl for this object

        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        if (got_timedProperty == false) {
            Properties p = System.getProperties();
            String tmpUrlConn_timeoutvalue = null;
            if ((tmpUrlConn_timeoutvalue = p.getProperty("com.sun.jdmk.urlconnectionTimeout")) != null) {
                useTimedURLConnection = true;
                try {
                    urlConn_timeoutvalue = new Integer(tmpUrlConn_timeoutvalue);
                } catch (Exception e) {
                    // use default value, 10s
                    urlConn_timeoutvalue = new Integer(10000);
                }
            }
            got_timedProperty = true;       
        } 
        // End NPCTE fix for bug 4653805

        this.host = host;
        this.port = port;
        this.url = url;
        this.connector = connector;

        inNotifier = new HttpSendInputStream(null, this);
        outNotifier = new HttpSendOutputStream(writeNotify(), this);
    }

    /**
     * Create a stream socket and connect it to the specified port on
     * the specified host.
     *
     * @param host the host
     * @param port the port
     */
    public HttpSendSocket(String host, int port) throws IOException {
        this(host, port, new URL("http", host, port, "/"), null);

        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        if (got_timedProperty == false) {
            Properties p = System.getProperties();
            String tmpUrlConn_timeoutvalue = null;
            if ((tmpUrlConn_timeoutvalue = p.getProperty("com.sun.jdmk.connectionTimeout")) != null) {
                useTimedURLConnection = true;
                try {
                    urlConn_timeoutvalue = new Integer(tmpUrlConn_timeoutvalue);
                } catch (Exception e) {
                    // use default value, 10s
                    urlConn_timeoutvalue = new Integer(10000);
                }
            }
            got_timedProperty = true;       
        } 
        // End NPCTE fix for bug 4653805
    }

    /**
     * Create a stream socket and connect it to the specified address on
     * the specified port.
     *
     * @param address the address
     * @param port the port
     */
    public HttpSendSocket(InetAddress address, int port) throws IOException {
        this(address.getHostName(), port);

        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        if (got_timedProperty == false) {
            Properties p = System.getProperties();
            String tmpUrlConn_timeoutvalue = null;
            if ((tmpUrlConn_timeoutvalue = p.getProperty("USE_TIMED_URLCONN")) != null) {
                useTimedURLConnection = true;
                try {
                    urlConn_timeoutvalue = new Integer(tmpUrlConn_timeoutvalue);
                } catch (Exception e) {
                    // use default value, 10s
                    urlConn_timeoutvalue = new Integer(10000);
                }
            }
            got_timedProperty = true;       
        } 
        // End NPCTE fix for bug 4653805
    }

    /**
     * Create a new socket connection to host (or proxy), and prepare to
     * send HTTP transmission.
     */
    public synchronized OutputStream writeNotify() throws IOException {
        if (conn != null) {
            throw new IOException("Attempt to write on HttpSendSocket after request has been sent");
        }

        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        if (useTimedURLConnection == false) {
            conn = url.openConnection();
            conn.setDoOutput(true);
            conn.setUseCaches(false);
            conn.setRequestProperty("Accept", "application/octet-stream");
            conn.setRequestProperty("User-Agent", HttpDef.jdmkVersion);
            conn.setRequestProperty("Content-Type", "application/octet-stream");
        } else {
            timedConn = new TimedURLConnection(url, urlConn_timeoutvalue.intValue());
            if (timedConn.getURLConnection() == null) {
                throw new IOException("Failed to write notification (no connection)");
            }
            timedConn.getURLConnection().setDoOutput(true);
            timedConn.getURLConnection().setUseCaches(false);
            timedConn.getURLConnection().setRequestProperty("Accept", "application/octet-stream");
            timedConn.getURLConnection().setRequestProperty("User-Agent", HttpDef.jdmkVersion);
            timedConn.getURLConnection().setRequestProperty("Content-Type", "application/octet-stream");
        }
        // End NPCTE fix for bug 4653805

	if (connector.authSchemeInfoList != null) {
	    AuthSchemeInfo authSchemeInfo = (AuthSchemeInfo) connector.authSchemeInfoList.get(Thread.currentThread());
	    if (authSchemeInfo != null) {
		String response = authSchemeInfo.getResponse();
		if (response != null) {
                    // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
                    if (useTimedURLConnection == false) {
		        conn.setRequestProperty("Authorization", response);
                    } else {
                        timedConn.getURLConnection().setRequestProperty("Authorization",response);
                    }
                    // End NPCTE fix for bug 4653805
		    authSchemeInfo.setResponse(null);
		}
	    }
	}

        inNotifier.deactivate();
        in = null;

        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        if (useTimedURLConnection == false) {
            out = conn.getOutputStream();
        } else {
            out = timedConn.getOutputStream();
        }

        return out ;
        // End NPCTE fix for bug 4653805
    }

    /**
     * Send HTTP output transmission and prepare to receive response.
     */
    public synchronized InputStream readNotify() throws IOException {

        outNotifier.deactivate();
        out.close();
        out = null;

        // NPCTE fix for bug 4653805, escalation 535848, SD, 11 Jun 02
	try {
            if (useTimedURLConnection == false) {
	        ((HttpURLConnection)conn).getResponseCode();
	        in = ((HttpURLConnection)conn).getErrorStream();
            } else {
                timedConn.getResponseCode();
                in = timedConn.getErrorStream();
            }
	} catch (IOException ie) {
            if (logger.finerOn()) {
                logger.finer("readNotify", "got Exception:");
                ie.printStackTrace();
            } 
	}

	if (in == null) {
            try {
                if (useTimedURLConnection == false) {
                    in = conn.getInputStream();
                } else {
                    in = timedConn.getInputStream();
                }
            } catch (IOException e) {
                throw new IOException("HTTP request failed");
            }
	}
        // End NPCTE fix for bug 4653805

        /*
         * If an HTTP error response is returned, sometimes an IOException
         * is thrown, which is handled above, and other times it isn't, and
         * the error response body will be available for reading.
         * As a safety net to catch any such unexpected HTTP behavior, we
         * verify that the content type of the response is what the
         * HttpOutputStream generates: "application/octet-stream".
         * (Servers' error responses will generally be "text/html".)
         */
        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        String contentType = null; 
        if (useTimedURLConnection == false) {
            contentType = conn.getContentType();
            if (contentType == null || !conn.getContentType().equals("application/octet-stream")) {
                throw new IOException("HTTP request failed");
            }
        } else {
            contentType = timedConn.getURLConnection().getContentType();
            if (contentType == null || !timedConn.getURLConnection().getContentType().equals("application/octet-stream")) {
                throw new IOException("HTTP request failed");
            }
        }
        // End NPCTE fix for bug 4653805

	/*
	 * Get challenge and store it somewhere for next time.
	 */
        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        String challenge = null; 
        if (useTimedURLConnection == false) {
	    challenge = conn.getHeaderField("WWW-Authenticate");
        } else {
            challenge = timedConn.getURLConnection().getHeaderField("WWW-Authenticate");
        }
        // End NPCTE fix for bug 4653805
	if (challenge != null) {
	    if (connector.authSchemeInfoList != null) {
		AuthSchemeInfo authSchemeInfo = (AuthSchemeInfo) connector.authSchemeInfoList.get(Thread.currentThread());
		if (authSchemeInfo != null) {
		    authSchemeInfo.setChallenge(challenge);
		}
	    }
	}

	/*
	 * Check for unauthorized access to server
	 */
        // NPCTE fix for bug 4653805, escalation 535848, SD, 02 May 02
        int statusCode = 0;
        if (useTimedURLConnection == false) { 
	    statusCode = ((HttpURLConnection)conn).getResponseCode();
        } else {
             statusCode = timedConn.getResponseCode();
        }
        // End NPCTE fix for bug 4653805
	if (statusCode == 401) {
	    throw new UnauthorizedSecurityException(HttpDef.HTTP_ERROR_UNAUTHORIZED_REQUEST);
	}

        return in;
    }

    /**
     * Get the address to which the socket is connected.
     */
    public InetAddress getInetAddress() {
        try {
            return InetAddress.getByName(host);
        } catch (UnknownHostException e) {
            return null; // Null if couldn't resolve destination host
        }
    }

    /**
     * Get the local address to which the socket is bound.
     */
    public InetAddress getLocalAddress() {
        try {
            return InetAddress.getLocalHost();
        } catch (UnknownHostException e) {
            return null; // Null if couldn't determine local host
        }
    }

    /**
     * Get the remote port to which the socket is connected.
     */
    public int getPort() {
        return port;
    }

    /**
     * Get the local port to which the socket is connected.
     */
    public int getLocalPort() {
        return -1; // Request not applicable to this socket type
    }

    /**
     * Get an InputStream for this socket.
     */
    public InputStream getInputStream() throws IOException {
        return inNotifier;
    }

    /**
     * Get an OutputStream for this socket.
     */
    public OutputStream getOutputStream() throws IOException {
        return outNotifier;
    }

    /**
     * Enable/disable TCP_NODELAY.
     * This operation has no effect for an HttpSendSocket.
     */
    public void setTcpNoDelay(boolean on) throws SocketException {
    }

    /**
     * Retrieve whether TCP_NODELAY is enabled.
     */
    public boolean getTcpNoDelay() throws SocketException {
        return false; // Imply option is disabled
    }

    /**
     * Enable/disable SO_LINGER with the specified linger time.  
     * This operation has no effect for an HttpSendSocket.
     */
    public void setSoLinger(boolean on, int val) throws SocketException {
    }

    /**
     * Retrieve setting for SO_LINGER.
     */
    public int getSoLinger() throws SocketException {
        return -1; // Imply option is disabled
    }

    /**
     * Enable/disable SO_TIMEOUT with the specified timeout
     * This operation has no effect for an HttpSendSocket.
     */
    public synchronized void setSoTimeout(int timeout) throws SocketException {
    }

    /**
     * Retrieve setting for SO_TIMEOUT.
     */
    public synchronized int getSoTimeout() throws SocketException {
        return 0; // Imply option is disabled
    }

    /**
     * Close the socket.
     */
    public synchronized void close() throws IOException {
        if (out != null) // Push out transmission if not done
            out.close();
    }

    /**
     * Return string representation of this pseudo-socket.
     */
    public String toString() {
        return "HttpSendSocket[host=" + host + ",port=" + port + ",url=" + url + "]";
    }

    private final static ClassLogger logger =
	new ClassLogger(ClassLogger.LOGGER_LEGACY_HTTP,
			"HttpSendSocket");
}
