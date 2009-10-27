/*
 * @(#)file      GenericHttpConnectorServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.44
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
import java.util.Vector;
import java.util.Enumeration;
import java.security.*;

// jmx import
//
import javax.management.ObjectName;
import javax.management.MBeanServer;

/**
 * This class implements the common behavior for the server part of the
 * HTTP-based connectors. The HTTP/TCP connector extends this class and
 * inherits this behavior.
 * <p>
 * The HTTP-based connectors differ by the socket type they use to communicate
 * between the server and the client:
 * <UL>
 *      <LI>HTTP/TCP connector uses TCP sockets.</LI>
 * </UL>
 * However the features and the behavior of these connectors are the same. They 
 * are implemented in this class and described below.
 * <p>
 * The data transmitted between the client and server parts is the same in 
 * both connectors: these are serialized Java objects encoded as HTTP requests 
 * and responses.
 * <p>
 * The two connectors can perform user authentication. The add/remove user
 * authentication info methods are used to add/remove users and their
 * corresponding authentication information. If this server carries out client
 * authentication then clients connecting to this server are authenticated
 * using the 'CRAM-MD5 Access Authentication Scheme' as defined in RFCs 2104 and 2195.
 * <p>
 * An HTTP-based connector server may serve several clients concurrently. The 
 * number of concurrent clients can be limited using the property <CODE>maxActiveClientCount</CODE>.
 * <p>
 * The HTTP/TCP connector specifies a default value (10) for the
 * <CODE>maxActiveClientCount</CODE> property. When a connector is stopped, the 
 * active requests are interrupted and an error result is sent to the clients.
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This class may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 * @see com.sun.jdmk.comm.GenericHttpConnectorClient
 *
 */

public abstract class GenericHttpConnectorServer extends CommunicatorServer {

    /**
     * Gets an instance of the socket factory used by this connector.
     */
    abstract GenericHttpSocket createSocket();

    /**
     * Gets the notification forwarder used by this connector.
     */
    abstract GenericHttpNotificationForwarder getNotificationForwarder(GenericHttpConnectorAddress address);

    /**
     * Initializes this <CODE>GenericHttpConnectorServer</CODE> with the 
     * default port.
     * The default port is protocol-specific: its value is defined by the 
     * derived classes.
     *
     * @param connectorType Indicates the connector type. Possible values are:
     *        <CODE>CommunicatorServer.HTTP_TYPE</CODE> or 
     *        <CODE>CommunicatorServer.HTTPS_TYPE</CODE>.
     */
    public GenericHttpConnectorServer(int connectorType) 
        throws IllegalArgumentException {
        super(connectorType);
        maxActiveClientCount = 10; // This overrides the inherited default (1)
    }

    /**
     * Initializes this <CODE>GenericHttpConnectorServer</CODE> with the 
     * specified port.
     *
     * @param connectorType Indicates the connector type. Possible values are:
     *        <CODE>CommunicatorServer.HTTP_TYPE</CODE> or 
     *        <CODE>CommunicatorServer.HTTPS_TYPE</CODE>.
     * @param port The port number.
     */
    public GenericHttpConnectorServer(int connectorType, int port) 
        throws IllegalArgumentException {
        this(connectorType);
        this.port = port;
    }

    /**
     * Initializes this <CODE>GenericHttpConnectorServer</CODE> with the 
     * specified port and user authentication information list.
     *
     * @param connectorType Indicates the connector type. 
     *        Possible values are:
     *        <CODE>CommunicatorServer.HTTP_TYPE</CODE> or 
     *        <CODE>CommunicatorServer.HTTPS_TYPE</CODE>.
     * @param port The port number.
     * @param authInfoList The user authentication information list.
     */
    public GenericHttpConnectorServer(int connectorType, int port, 
                                      AuthInfo[] authInfoList)
        throws IllegalArgumentException {

        this(connectorType, port);
        if (authInfoList != null) {
            for (int i = 0; i < authInfoList.length; i++) {
                addUserAuthenticationInfo(authInfoList[i]);
            }
        }
    }

    /**
     * Initializes this <CODE>GenericHttpConnectorServer</CODE> with the specified port.
     *
     * @param connectorType Indicates the connector type. Possible values are:
     * <CODE>CommunicatorServer.HTTP_TYPE</CODE> or <CODE>CommunicatorServer.HTTPS_TYPE</CODE>.
     * @param port The port number.
     * @param bindAddr The local InetAddress the server will bind to .
     */
    // NPCTE fix for bug 4873785
    public GenericHttpConnectorServer(int connectorType, int port, java.net.InetAddress bindAddr) throws IllegalArgumentException {
        this(connectorType);
        this.port = port;
        this.bindAddr = bindAddr;
    }
    // end NPCTE fix for bugId 4873785

    /**
     * Initializes this <CODE>GenericHttpConnectorServer</CODE> with the specified port and
     * user authentication information list.
     *
     * @param connectorType Indicates the connector type. Possible values are:
     * <CODE>CommunicatorServer.HTTP_TYPE</CODE> or <CODE>CommunicatorServer.HTTPS_TYPE</CODE>.
     * @param port The port number.
     * @param authInfoList The user authentication information list.
     * @param bindAddr The local InetAddress the server will bind to .
     */
    // NPCTE fix for bug 4873785
    public GenericHttpConnectorServer(int connectorType, int port, AuthInfo[] authInfoList,  InetAddress bindAddr)
        throws IllegalArgumentException {

        this(connectorType, port, bindAddr);
        if (authInfoList != null) {
            for (int i = 0; i < authInfoList.length; i++) {
                addUserAuthenticationInfo(authInfoList[i]);
            }
        }
    }
    // end NPCTE fix for bugId 4873785

    // ServerNotificationHandlerInternal interface implementation
    //-----------------------------------------------------------

    /**
     * This method is used to start the "push" mode. A server connector should create a client
     * connector which allows communication from the server to the client, by using the address
     * provided as a parameter.
     *
     * @param addr the client connector address used by the server connector to establish a connection.
     */
    NotificationBackConnector startPush(ConnectorAddress addr) {
        // Check that parameter is not null.
        //
        if (addr == null) {
            throw new IllegalArgumentException("ConnectorAddress cannot be null");
        }

        // Check that parameter is an instance of GenericHttpConnectorAddress.
        //
        if (!(addr instanceof GenericHttpConnectorAddress)) {
            throw new IllegalArgumentException("ConnectorAddress must be an instance of GenericHttpConnectorAddress");
        }

        // Create the NotificationForwarder.
        //
        GenericHttpNotificationForwarder notificationForwarder = null;
        try {
            if (logger.finestOn()) {
                logger.finest("startPush","Create new NotificationForwarder");
            }
            notificationForwarder = getNotificationForwarder((GenericHttpConnectorAddress) addr);
        } catch (CommunicationException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }

        // Start forwarding events
        //
        if (logger.finestOn()) {
            logger.finest("startPush","Start forwarding events");
        }
        notificationForwarder.connect();

        return notificationForwarder;
    }

    /**
     * This method is used to tell a server connector to stop the "push" mode and to change to the "pull" mode.
     *
     * @param connector the connector which is used for "push" mode.
     */
    void stopPush(NotificationBackConnector connector) {
        // Check that parameter is an instance of GenericHttpConnectorAddress.
        //
        if (!(connector instanceof GenericHttpNotificationForwarder)) {
            throw new IllegalArgumentException("NotificationBackConnector must be an instance of GenericHttpNotificationForwarder");
        }
        ((GenericHttpNotificationForwarder)connector).disconnect();
    }

    // PRIVATE CLASSES
    //----------------

    private class NotificationHandlerInternal implements ServerNotificationHandlerInternal {

        /**
         * Ctor.
         */
        public NotificationHandlerInternal(GenericHttpConnectorServer connector) {
            this.connector = connector;
        }

        /**
         * This method is used to start the "push" mode. A server connector should create a client
         * connector which allows communication from the server to the client, by using the address
         * provided as a parameter.
         *
         * @param addr the client connector address used by the server connector to establish a connection.
         */
        public NotificationBackConnector startPush(ConnectorAddress addr) {
            return connector.startPush(addr);
        }

        /**
         * This method is used to tell a server connector to stop the "push" mode and to change to the "pull" mode.
         *
         * @param connector the connector which is used for "push" mode.
         */
        public void stopPush(NotificationBackConnector connector) {
            this.connector.stopPush(connector);
        }

        private GenericHttpConnectorServer connector;
    }

    // MBeanRegistration INTERFACE
    //----------------------------

    /**
     * Performs pre-registration initialization.
     * This method is not intended to be called directly by the user. 
     */
    public ObjectName preRegister(MBeanServer server, ObjectName name) throws Exception {
        return super.preRegister(server, name);
    }

    /**
     * Allows the MBean to perform any operations needed after having been
     * registered in the MBeanServer or after the registration has failed.
     *
     * @param registrationDone Indicates whether or not the MBean has been successfully registered
     * in the MBeanServer. The value false means that the registration phase has failed.
     */
    public void postRegister(Boolean registrationDone) {
        super.postRegister(registrationDone);
        NotificationHandlerInternal handler =
            new NotificationHandlerInternal(this);
        serverNotificationDispatcher =
            new ServerNotificationDispatcher(handler, topMBS);
        heartbeatServerHandler =
            new HeartBeatServerHandler(topMBS, serverNotificationDispatcher);
    }

    /**
     * Allows the MBean to perform any operations it needs before being de-registered
     * by the MBean server.
     *
     * @exception java.lang.Exception This exception should be caught by the MBean server and re-thrown
     * as an <CODE>MBeanRegistrationException</CODE>.
     */
    public void preDeregister() throws Exception {
        super.preDeregister();
    }

    /**
     * Allows the MBean to perform any operations needed after having been
     * de-registered from the MBean server.
     */
    public void postDeregister() {
        super.postDeregister();
    }

    // SUBCLASSING OF CommunicatorServer
    //----------------------------------
  
    /**
     */
    protected void doError(Exception e) throws CommunicationException {
        return;
    }

    /**
     */
    protected void doBind()
        throws InterruptedException, CommunicationException {
        try {
            // NPCTE fix for bug 4873785
            if (bindAddr == null) {
                if (logger.finerOn())
                    logger.finer("doBind", "about to bind to port " + port);
                sockListen = createSocket().createServerSocket(port);
            } else {
                if (logger.finerOn())
                    logger.finer("doBind", "about to bind to address:port " +
                                 bindAddr + ":"+ port);
                sockListen = createSocket().createServerSocket(port, bindAddr);
            }
            // end NPCTE fix for bugId 4873785
            sockListen.doBind();
            port = sockListen.getLocalPort();
            dbgTag = makeDebugTag();
            if (logger.finerOn())
                logger.finer("doBind",
                      sockListen.toString()+" bound to "+sockListen.getLocalAddress()+" port "+sockListen.getLocalPort());
        } catch (SocketException e) {
            if (logger.finerOn())
                logger.finer("doBind", "EXCEPTION MSG = '" + e.getMessage() + "'");
            if (e.getMessage().equals(InterruptSysCallMsg))
                throw new InterruptedException(e.toString());
            else
                throw new CommunicationException(e);
        } catch (InterruptedIOException e) {
            throw new InterruptedException(e.getMessage());
        } catch (IOException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     */
    protected void doReceive()
        throws InterruptedException, CommunicationException {
        try {
            sockListen.doReceive();
            addrLastClient = sockListen.getRemoteAddress();
        } catch (SocketException e) {
            if (e.getMessage().equals(InterruptSysCallMsg))
                throw new InterruptedException(e.toString());
            else
                throw new CommunicationException(e);
        } catch (InterruptedIOException e) {
            throw new InterruptedException(e.getMessage());
        } catch (IOException e) {
            if (e.getMessage().equals(InterruptSysCallMsg))
                throw new InterruptedException(e.toString());
            else
                throw new CommunicationException(e);
        } catch (CommunicationException e) {
            throw e;
        } catch (Exception e) {
            if (logger.finerOn())
                logger.finer("doReceive", "EXCEPTION MSG = '" + e.getMessage() + "'");
            throw new InterruptedException();
        }
    }

    /**
     */
    protected void doProcess()
        throws InterruptedException, CommunicationException {
        if (logger.finerOn())
            logger.finer("doProcess", "Address of last connected client ["+addrLastClient+"]");
        GenericHttpRequestHandler server =
            new GenericHttpRequestHandler(this, getServedClientCount(),
                                          (GenericHttpSocket) sockListen.clone(),
                                          topMBS, objectName);
    }

    /**
     */
    protected void doUnbind()
        throws InterruptedException, CommunicationException {
        try {
            if (sockListen != null) {
                if (logger.finerOn())
                    logger.finer("doUnbind", "Port ["+port+"] has been definitively closed");
                sockListen.doUnbind();
            }
        } catch (InterruptedIOException e) {
            throw new InterruptedException(e.getMessage());
        } catch (IOException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Returns the string used in debug traces.
     */
    String makeDebugTag() {
        return "GenericHttpConnectorServer[" + getProtocol() + ":" + getPort() + "]";
    }

    /**
     * Stops this connector server.
     * <p>
     * Has no effect if this HTTP connector server is
     * <CODE>OFFLINE</CODE> or <CODE>STOPPING</CODE>.
     */
    public void stop() {
        if ((state == ONLINE) || (state == STARTING)) {
            super.stop();
            heartbeatServerHandler.cleanupClientResources();
            try{
                GenericHttpSocket sn = sockListen.createClientSocket();
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null)
                    sn.doConnect(System.getProperty("jdmk.hostname") , port);
                else
                    sn.doConnect(java.net.InetAddress.getLocalHost().getHostAddress(),port);
                // end of NPCTE fix for bugId 4770217
                sn.doSend("",null);
                sn.doDisconnect();
            } catch (Throwable e) {
            } 
        }
    }

    // GETTERS/SETTERS
    //----------------

    public synchronized void setMBeanServer(MBeanServer newMBS)
        throws IllegalArgumentException, IllegalStateException {
        super.setMBeanServer(newMBS);
        serverNotificationDispatcher.setMBeanServer(newMBS);
    }

    /**
     * Gets the number of clients that have been processed by this <CODE>GenericHttpConnectorServer</CODE> 
     * since its creation.
     *
     * @return The number of clients handled by this <CODE>GenericHttpConnectorServer</CODE>
     *         since its creation. This counter is not reset by the <CODE>stop</CODE> method.
     */
    public int getServedClientCount() {
        return super.getServedClientCount();
    }

    /**
     * Gets the number of clients currently being processed by this 
     * <CODE>GenericHttpConnectorServer</CODE>.
     *
     * @return The number of clients currently being processed by this 
     *         <CODE>GenericHttpConnectorServer</CODE>.
     */
    public int getActiveClientCount() {
        return super.getActiveClientCount();
    }

    /**
     * Gets the maximum number of clients that this 
     * <CODE>GenericHttpConnectorServer</CODE> can process concurrently.
     *
     * @return The maximum number of clients that this 
     *      <CODE>GenericHttpConnectorServer</CODE> can process concurrently.
     */
    public int getMaxActiveClientCount() {
        return super.getMaxActiveClientCount();
    }

    /**
     * Sets the maximum number of clients this 
     * <CODE>GenericHttpConnectorServer</CODE> can process concurrently.
     *
     * @param c The number of clients.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the communicator was ONLINE or STARTING.
     */
    public void setMaxActiveClientCount(int c) 
        throws java.lang.IllegalStateException {
        super.setMaxActiveClientCount(c);
    }

    /**
     * Gets the IP address of the last connected client.
     * This function uses the string representation of 
     * {@link java.net.InetAddress}.
     *
     * @return The IP address of the last connected client.
     *
     * @see java.net.InetAddress
     */
    public String getLastConnectedClient() {
        if (addrLastClient == null) {
            return "unknown";
        }
        return addrLastClient.toString();
    }
  
    /**
     * Adds user authentication information to this server.
     * In order to populate the list of users supported by this server, 
     * invoke this method for each user you want to add. If the user already
     * exists, then update his authentication information.
     *
     * @param authinfo The user authentication information.
     */
    public synchronized void addUserAuthenticationInfo(AuthInfo authinfo) {
        if (authinfo != null) {
            // Check if user already exists. If true, update his password.
            String username = authinfo.getLogin();
            for (Enumeration e = authInfo.elements(); e.hasMoreElements(); ) {
                AuthInfo ai = (AuthInfo) e.nextElement();
                if (ai.getLogin().equals(username)) {
                    authInfo.removeElement(ai);
                    break;
                }
            }
            authInfo.addElement(authinfo);
        }
    }

    /**
     * Removes user authentication information from this server.
     *
     * @param authinfo The user authentication information.
     *
     * @see #addUserAuthenticationInfo
     */
    public synchronized void removeUserAuthenticationInfo(AuthInfo authinfo) {
        if (authinfo != null) {
            // Check if user exists.
            String username = authinfo.getLogin();
            for (Enumeration e = authInfo.elements(); e.hasMoreElements(); ) {
                AuthInfo ai = (AuthInfo) e.nextElement();
                if (ai.getLogin().equals(username)) {
                    authInfo.removeElement(ai);
                    break;
                }
            }
        }
    }

    /**
     * Returns true if the list of users supported by this server is not empty.
     *
     * @return True, if the list of users supported by this server is not empty. False, if
     * the list of supported users is empty so no authentication is performed by this server.
     */
    public boolean isAuthenticationOn() {
        return !authInfo.isEmpty();
    }

    // CRAM-MD5 SPECIFIC METHODS
    //--------------------------

    /**
     * For Java DMK internal use only.
     * <p>
     * Generate the server's challenge.
     * <p>
     * Message Format: "&lt;CurrentTime@Hostname&gt;"
     */
    synchronized String generateChallengeResponse() {
        String hostname = null;
        try {
            // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
            if (System.getProperty("jdmk.hostname") != null)
                hostname = System.getProperty("jdmk.hostname");
            else {
                InetAddress localhost = InetAddress.getLocalHost();
                hostname = localhost.getHostName();
            }
            if (logger.finerOn())
                logger.finer("generateChallengeResponse", "hostname="+hostname);
            // end of NPCTE fix for bugId 4770217
        } catch (UnknownHostException e) {
            hostname = "UnknownHost";
        }
        String challenge = "<"+System.currentTimeMillis()+"@"+hostname+">";
        if (index == MAX_CHALLENGES) {
            index = 0;
        }
        challengeList[index] = challenge;
        index++;
        return challenge;
    }

    /**
     * For Java DMK internal use only.
     * <p>
     * Check if the response sent by the client matches any of the challenges stored in the server.
     * If the match is successful then the client has been authenticated and the method returns the successful AuthInfo.
     * Otherwise, the result is null.
     */
    synchronized AuthInfo checkChallengeResponse(String response) {

        // Check we've got a response
        if (response == null) {
            return null;
        }

        // Extract the username and the keyed MD5 digest from response
        String username = response.substring(0, response.lastIndexOf(' '));
        String digest = response.substring(response.lastIndexOf(' ') + 1);

        // Get password for given username
        AuthInfo ai = null;
        boolean found = false;
        for (Enumeration e = authInfo.elements(); e.hasMoreElements(); ) {
            ai = (AuthInfo) e.nextElement();
            if (ai.getLogin().equals(username)) {
                found = true;
                break;
            }
        }
        if (!found) {
            return null;
        }

        final byte[] keyBytes = ai.getPassword().getBytes();
        // Compare client and server digests
        for (int i = 0; i < MAX_CHALLENGES; i++) {
            String challenge = challengeList[i];
            if (challenge == null) {
                continue;
            }
            String hmac = HMAC_MD5(challenge.getBytes(), keyBytes);
            if (digest.equals(hmac)) {
                challengeList[i] = null;
                return ai;
            }
        }
        return null;
    }

    /**
     * Generates the CRAM-MD5 digest for the given challenge with the given key.
     */
    private String HMAC_MD5(byte[] challenge, byte[] key) {

        final int MD5_BLOCKSIZE = 64;

        MessageDigest md5 = null;

        try {
            md5 = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException nsae) {
            // Should never get this exception ...
        }

        /* digest the key if longer than 64 bytes */
        if (key.length > 64) {
            key = md5.digest(key);
        }

        byte[] ipad = new byte[MD5_BLOCKSIZE];  /* inner padding */
        byte[] opad = new byte[MD5_BLOCKSIZE];  /* outer padding */
        byte[] digest;
        int i;

        /* store key in pads */
        for (i = 0; i < MD5_BLOCKSIZE; i++) {
            for (; i < key.length; i++) {
                ipad[i] = key[i];
                opad[i] = key[i];
            }
            ipad[i] = 0x00;
            opad[i] = 0x00;
        }

        /* XOR key with pads */
        for (i = 0; i < MD5_BLOCKSIZE; i++) {
            ipad[i] ^= 0x36;
            opad[i] ^= 0x5c;
        }

        /* inner MD5 */
        md5.update(ipad);
        md5.update(challenge);
        digest = md5.digest();

        /* outer MD5 */
        md5.update(opad);
        md5.update(digest);
        digest = md5.digest();

        StringBuffer digestString = new StringBuffer();
        for (i = 0; i < digest.length; i++) {
            if ((digest[i] & 0xff) < 0x10) {
                digestString.append("0" + Integer.toHexString(digest[i] & 0xff));
            } else {
                digestString.append(Integer.toHexString(digest[i] & 0xff));
            }
        }
        return digestString.toString();
    }

    // PROTECTED VARIABLES
    //--------------------

    /**
     */
    transient GenericHttpSocket sockListen = null;

    /**
     */
    InetAddress addrLastClient = null;

    /**
     */
    Vector authInfo = new Vector();

    /**
     */
    int index = 0;

    /**
     */
    final int MAX_CHALLENGES = 100;

    /**
     */
    String[] challengeList = new String[MAX_CHALLENGES];

    /**
     */
    ServerNotificationDispatcher serverNotificationDispatcher = null;

    /**
     */
    HeartBeatServerHandler heartbeatServerHandler = null;

    /**
     */
    // NPCTE fix for bug 4873785
    InetAddress bindAddr = null;
    // end NPCTE fix for bugId 4873785

    // PRIVATE VARIABLES
    //------------------

    /**
     * Name of the naming attributes
     */
    private static final String InterruptSysCallMsg = "Interrupted system call";
}
