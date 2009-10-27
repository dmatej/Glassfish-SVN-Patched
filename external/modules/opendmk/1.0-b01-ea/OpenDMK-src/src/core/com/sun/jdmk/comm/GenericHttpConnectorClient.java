/*
 * @(#)file      GenericHttpConnectorClient.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.109
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

import com.sun.jdmk.OperationContext;
import com.sun.jdmk.ProxyMBeanInstantiationException;

import com.sun.jdmk.internal.ClassLogger;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.io.Serializable;
import java.net.InetAddress;
import java.net.Socket;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Hashtable;
import java.util.Set;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.InvalidAttributeValueException;
import javax.management.JMRuntimeException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

// jmx import
//
import javax.management.QueryExp;
import javax.management.ReflectionException;

/**
 * The <CODE>GenericHttpConnectorClient</CODE> class provides an implementation of the 
 * {@link com.sun.jdmk.comm.RemoteMBeanServer RemoteMBeanServer} interface based on the
 * HTTP protocol.
 * <p>
 * Querying a Java Dynamic Management agent with the HTTP connector implies that an instance
 * of {@link com.sun.jdmk.comm.GenericHttpConnectorServer GenericHttpConnectorServer} is
 * running on the remote Java Dynamic Management agent.
 * <p>
 * In order to identify the Java Dynamic Management agent the connector needs to communicate
 * with, the method {@link #connect connect} needs to be invoked.
 * <p>
 * It is possible to request the use of a specific proxy through the java properties
 * <CODE>http.proxyHost=</CODE><VAR>host</VAR> and <CODE>http.proxyPort=</CODE><VAR>port</VAR> for HTTP, and
 * <CODE>https.proxyHost=</CODE><VAR>host</VAR> and <CODE>https.proxyPort=</CODE><VAR>port</VAR> for HTTPS.
 * <p>
 * The authentication information required to configure the connector is provided by the ConnectorAddress
 * parameter in the method {@link #connect connect}.
 * <p>
 * The following port numbers are used by default:
 * <UL>
 * <LI><b>8081</b> for the HTTP/TCP connector
 * <LI><b>8084</b> for the HTTP/SSL connector
 * </UL>
 * <p>
 * This class implements the {@link com.sun.jdmk.comm.ClientNotificationHandler} interface
 * to receive notifications from a remote MBean, and the {@link com.sun.jdmk.comm.HeartBeatClientHandler}
 * interface to be able to detect any communication problem with the connector server and
 * notify it to the manager that created it.
 *
 * @see com.sun.jdmk.comm.RemoteMBeanServer
 * @see com.sun.jdmk.comm.HeartBeatClientHandler
 * @see com.sun.jdmk.comm.GenericHttpConnectorServer
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This class may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 */

public abstract class GenericHttpConnectorClient implements RemoteMBeanServer, HeartBeatClientHandler, Serializable {

    /**
     * Gets an instance of the socket factory used by this connector.
     */
    abstract GenericHttpSocketFactory getSocketFactory();

    /**
     * Gets the notification receiver used by this connector.
     */
    abstract GenericHttpNotificationReceiver getNotificationReceiver(GenericHttpConnectorClient connector,
                                                                     ClientNotificationDispatcher dispatcher);

    /**
     * <p>Set the OperationContext of this connector.  This context will be
     * sent along with each request and can be recovered by the server,
     * which can make it available to the operations it invokes.</p>
     *
     * <p>The saved OperationContext will be a clone of the object
     * <code>c</code> made using its <code>clone</code> method.</p>
     *
     * @param c the new OperationContext.  It may be null to indicate that
     * there is no context.  The previous OperationContext, if any, is lost.
     *
     * @exception CommunicationException if the context cannot be set for
     * some reason.  One common reason is that the object <code>c</code>
     * does not implement the <code>Cloneable</code> interface.
     */
    public void setOperationContext(OperationContext c) {
        try {
            // NPCTE fix for bugId 4497571, esc 0, MR 03 September 2001
            if (c != null)
                c = (OperationContext) c.clone();
            // end of NPCTE fix for bugId 4497571
        } catch (CloneNotSupportedException e) {
            throw new CommunicationException(e);
        }
        // NPCTE fix for bugId 4497571, esc 0, MR 03 September 2001
        this.operationContext = c;
        // end of NPCTE fix for bugId 4497571
    }

    /**
     * Get the OperationContext that was last given to setOperationContext,
     * or null if setOperationContext was never called.
     * @return the current OperationContext.
     */
    public OperationContext getOperationContext() {
        return operationContext;
    }

    /**
     * Constructs a connector client.
     * <P>This constructor will send
     * the default local host name to a server for receiving
     * notifications.
     */
    public GenericHttpConnectorClient() {
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (System.getProperty("jdmk.hostname") != null)
            localHost = System.getProperty("jdmk.hostname");
        else {
            try {
                localHost = InetAddress.getLocalHost().getHostName();
            } catch (Exception e) {
                localHost = "localhost";
            }
        }
        if (logger.finerOn())
            logger.finer("GenericHttpConnectorClient", "localHost="+localHost);
        // end of NPCTE fix for bugId 4770217
        Initialize();
    }

    /**
     * Constructs a connector client.
     *
     * @param addr local address sent to a server for receiving
     * notifications.  This address will be saved internally using
     * InetAddress.getHostAddress.
     *
     * @since Java DMK 5.0
     */
    public GenericHttpConnectorClient(InetAddress addr) {
        try {
            if (addr == null) {
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null)
                    localHost = System.getProperty("jdmk.hostname");
                else
                    localHost = InetAddress.getLocalHost().getHostAddress();
                // end of NPCTE fix for bugId 4770217
            } else {
                localHost = addr.getHostAddress();
            }
        } catch (Exception e) {
            localHost = "localhost";
        }
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (logger.finerOn())
            logger.finer("GenericHttpConnectorClient", "localHost="+localHost);
        // end of NPCTE fix for bugId 4770217
        Initialize();
    }

    /**
     * Constructs a connector client.
     *
     * @param addr local address sent to a server for receiving notifications.
     *
     * @since Java DMK 5.0
     */
    public GenericHttpConnectorClient(String addr) {
        if (addr != null) {
            localHost = addr;
        } else {
            try {
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null)
                    localHost = System.getProperty("jdmk.hostname");
                else 
                    localHost = InetAddress.getLocalHost().getHostAddress();
                // end of NPCTE fix for bugId 4770217
            } catch (Exception e) {
                localHost = "localhost";
            }
        }
        // NPCTE fix for bugId 477021, esc 541597, MR, Oct 2002
        if (logger.finerOn())
            logger.finer("GenericHttpConnectorClient", "localHost="+localHost);
        // end of NPCTE fix for bugId 4770217
        Initialize();
    }

    private void Initialize() {

        //
        // Initialize the socket factory.
        //
        factory = getSocketFactory();

        //
        // By default, use the default mapper.
        //
        defaultMapper = new DefaultMapper();
        mapper = defaultMapper;

        // Initialize notification stuff
        //
        notificationClientHandler = new ClientNotificationDispatcher(new NotificationHandlerInternal(this));

        // Initialize heartbeat stuff
        //
        heartbeatClientHandler = new HeartBeatClientHandlerImpl(new HeartBeatInternalClientHandlerImpl(this), notificationClientHandler);
    }


    // CRAM-MD5 SPECIFIC METHODS
    //--------------------------

    /**
     * Generates the CRAM-MD5 digest for the given challenge with the given key
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
            for ( ; i < key.length; i++) {
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

    // HTTP SPECIFIC METHODS
    //----------------------

    /**
     * Sends the formatted HTTP request with its content.
     * @return an InputStream from which the body of the HTTP reply (the
     * part after the headers) can be read.
     */
    private InputStream sendHttp(Object[] opList, boolean retry)
            throws Exception {

        //
        //  If authentication is required, update security stuff
        //
        if (authSchemeInfoList != null) {
            //
            // Create thread entry in AuthSchemeInfo table
            //
            if (authSchemeInfoList.get(Thread.currentThread()) == null) {
                authSchemeInfoList.put(Thread.currentThread(), new AuthSchemeInfo("CRAM-MD5"));
            }

            //
            // Generate response if challenge has been provided
            //
            AuthSchemeInfo authSchemeInfo = (AuthSchemeInfo) authSchemeInfoList.get(Thread.currentThread());
            if (authSchemeInfo != null) {
                String challenge = authSchemeInfo.getChallenge();
                if (challenge != null) {
                    //
                    // Generate response
                    //
                    String encoded_response = "";
                    try {
                        // Decode Base64 CRAM-MD5 server challenge
                        challenge = challenge.substring((authSchemeInfo.getAuthScheme() + " ").length());
                        byte decoding[] = new BASE64Decoder().decodeBuffer(challenge);
                        // Generate client response and encode it using Base64
                        // Response = Base64 ( username + " " + digest )
                        String username = httpConnAddr.getAuthInfo().getLogin();
                        byte key[] = httpConnAddr.getAuthInfo().getPassword().getBytes();
                        String digest = HMAC_MD5(decoding,key);
                        String unencoded_response = username + " " + digest;
                        // The maximum number of bytes encoded at a time by the BASE64Encoder is 57.
                        // This results in encoded lines being no more than 76 characters long.
                        final int maxBytesPerLine = 57;
                        String chunk = null;
                        int quotient = unencoded_response.length() / maxBytesPerLine;
                        int modulus  = unencoded_response.length() % maxBytesPerLine;
                        for (int i = 0; i < quotient; i++) {
                            chunk = unencoded_response.substring((i*maxBytesPerLine),(i+1)*maxBytesPerLine);
                            encoded_response += new BASE64Encoder().encode(chunk.getBytes());
                        }
                        if (modulus > 0) {
                            chunk = unencoded_response.substring(quotient*maxBytesPerLine);
                            encoded_response += new BASE64Encoder().encode(chunk.getBytes());
                        }
                    } catch (IOException ioe) {
                        // Ignore ... - Should we throw UnauthorizedSecurityException instead ???
                    }

                    //
                    // Add response to AuthSchemeInfo table
                    //
                    authSchemeInfo.setResponse(authSchemeInfo.getAuthScheme()+ " " + encoded_response);

                    //
                    // Remove challenge from AuthSchemeInfo table
                    //
                    authSchemeInfo.setChallenge(null);
                }
            }
        }

        //
        // Establish connection to the server, send HTTP request and wait for HTTP response.
        //
        Socket socket = null;
        try {
            //
            // Format entity body for HTTP request
            //
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream objOut = new ObjectOutputStream(baos);
            if (operationContext != null && supportsOperationContext)
                objOut.writeObject(operationContext);

            for (int i = 0; i < opList.length; i++)
                objOut.writeObject(opList[i]);

            //
            // Open connection
            //
            try {
                socket = factory.createSocket(httpConnAddr.getHost(), httpConnAddr.getPort(), this);
            } catch (java.net.SocketException se) {
                // sometime a socket is free but not ready to be used, need to wait a little
                try {
                    Thread.sleep(100);
                } catch (Exception ee) {}
                socket = factory.createSocket(httpConnAddr.getHost(), httpConnAddr.getPort(), this);
            }

            //
            // Send HTTP request
            //
            OutputStream outputStream = socket.getOutputStream();
            outputStream.write(baos.toByteArray());
            outputStream.flush();

            //
//          // Format entity body for HTTP request
//          //
//          ByteArrayOutputStream out = new ByteArrayOutputStream();
//          ObjectOutputStream objOut = new ObjectOutputStream(outputStream);
//          if (operationContext != null && supportsOperationContext)
//              objOut.writeObject(operationContext);

//          for (int i = 0; i < opList.length; i++)
//              objOut.writeObject(opList[i]);
//          objOut.flush();
//             outputStream.flush();

            /* Return HTTP response.  We first read one byte with a pushback
               so we can detect an UnauthorizedSecurityException.  */
            PushbackInputStream pushback =
                new PushbackInputStream(socket.getInputStream());
            int x = pushback.read();
            if (x >= 0)
                pushback.unread(x);
            /* If x < 0, we got an EOF; we trust the underlying InputStream
               to give us an EOF again the next time we read, so we don't
               need to "push back EOF".  Not throwing EOFException here
               leads to more understandable stack traces when the EOF is
               detected elsewhere.  */
            return pushback;
        } catch (UnauthorizedSecurityException e) {
            if (authSchemeInfoList == null || retry == true) {
                //
                // If auth info not provided or second try, then return exception
                //
                throw e;
            } else {
                //
                // Otherwise, resend HTTP request
                //
                return sendHttp(opList, true);
            }
        } finally {
            //
            // Close connection
            //
            if (socket != null) {
                try {
                    socket.close();
                } catch (IOException e) {
                } finally {
                    socket = null;
                }
            }
        }
    }

    // IMPLEMENTATION OF THE RemoteMBeanServer INTERFACE
    //--------------------------------------------------

    // ----------------------
    // Communication handling
    // ----------------------

    /**
     * Initializes the communication with the remote MBeanServer. All
     * the information needed for identifying the MBeanServer to
     * contact and the protocol to be used is contained in the object
     * of type ConnectorAddress passed as a parameter. If a
     * communication problem occurs this method will throw a
     * CommunicationException (JMRuntimeException).  If the
     * RemoteMBeanServer had already been connected to and
     * disconnected from an MBeanServer identified by its
     * MBeanServerId, and if the MBeanServer reachable by the
     * MBeanServerAddress parameter doesn't have the same
     * MBeanServerId, then java.lang.IllegalAccessException is thrown.
     *
     * @param mbeanServerAddress The address for the MBeanServer to
     * contact (MBeanServer identification, protocol specification).
     *
     * @exception IllegalArgumentException The
     * <CODE>RemoteMBeanServer</CODE> has already been connected and
     * disconnected and the specified <CODE>ConnectorAddress</CODE>
     * doesn't identify the same <CODE>MBeanServer</CODE>.
     * @exception CommunicationException The
     * <CODE>RemoteMBeanServer</CODE> was already connected or a
     * problem was encountered in the connection to the connector
     * server.
     * @exception UnauthorizedSecurityException The authentication
     * information sent to the <CODE>ConnectorServer</CODE> was not
     * correct. Login based authentication failed.
     *
     * @return A String identifying the MBeanServer with which the
     * communication is established.
     */
    public String connect(ConnectorAddress mbeanServerAddress) {

        if (logger.finerOn())
            logger.finer("connect", "connect");

        // Check that parameter is not null.
        //
        if (mbeanServerAddress == null) {
            throw new IllegalArgumentException("mbeanServerAddress cannot be null");
        }

        // Check that parameter is an instance of GenericHttpConnectorAddress.
        //
        if (!(mbeanServerAddress instanceof GenericHttpConnectorAddress)) {
            throw new IllegalArgumentException("mbeanServerAddress must be an instance of GenericHttpConnectorAddress");
        }

        // Check that the factory is not already connected.
        //
        if (connected == true) {
            throw new CommunicationException("ConnectorClient already connected to RemoteMBeanServer");
        } else {
            // Initialize the info related to server communication.
            //
            httpConnAddr = (GenericHttpConnectorAddress)((GenericHttpConnectorAddress)mbeanServerAddress).clone();

            // Initialize authentication info.
            //
            AuthInfo authInfo = httpConnAddr.getAuthInfo();
            if (authInfo != null) {
                // Verify non null values for login/password
                //
                if (authInfo.getLogin() == null || authInfo.getPassword() == null) {
                    throw new IllegalArgumentException("Login/Password cannot be null");
                }

                // Initialize security hashtable.
                //
                authSchemeInfoList = new Hashtable();
            }

            supportsOperationContext = supports("OperationContext");

            // Get MBeanServerId and force client
            // authentication at connection time.
            //
            String mbeanServerId2 = null;
            try {
                mbeanServerId2 = getMBeanServerId2();
            } catch (UnauthorizedSecurityException e) {
                throw e;
            } catch (CommunicationException e) {
                throw e;
            } catch (Exception e) {
                throw new CommunicationException(e, "Connection failed");
            }

            // Check that if the client already connected to and disconnected from a RemoteMBeanServer
            // it will be allowed to connect to the same RemoteMBeanServer but not to a different one.
            //
            if (mbeanServerId != null) {
                if (!mbeanServerId.equals(mbeanServerId2)) {
                    throw new IllegalArgumentException("ConnectorClient trying to connect to a different RemoteMBeanServer");
                }
            } else {
                mbeanServerId = mbeanServerId2;
            }

            // Set connection flag to true.
            //
            connected = true;

            // Send connection established notification
            //
            heartbeatClientHandler.notifyConnectionEstablished();

            // Start heartbeat ping
            //
            heartbeatClientHandler.startPinging();
        }
        return mbeanServerId;
    }

    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    public void disconnect() {
        disconnect(false);
    }
    // end NPCTE fix for bugId 4783766

    /**
     * Terminates the communication with the MBeanServer.
     */
    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    public void disconnect(boolean local) {
    // end NPCTE fix for bugId 4783766
        if (logger.finerOn())
            logger.finer("disconnect", "disconnect");

        if (connected) {
            // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
            notificationClientHandler.stopListening(local);
            heartbeatClientHandler.stopPinging(-1, local);
            // end NPCTE fix for bugId 4783766
            heartbeatClientHandler.notifyConnectionTerminated();
            httpConnAddr = null;
            authSchemeInfoList = null;
            connected = false;
        }
    }

    /**
     * Checks whether a communication with the MBeanServer is established.
     *
     * @return  True, if the communication is established, otherwise false.
     */
    public boolean isConnected() {

        if (logger.finerOn())
            logger.finer("isConnected", "isConnected");

        return connected;
    }

    /**
     * Returns the exact address of the MBeanServer to which the ConnectorClient is
     * connected. The address is of type ConnectorAddress.
     *
     * @return  The exact address of the remote MBeanServer, or null if the ConnectorClient is
     * not connected.
     */
    public ConnectorAddress getMBeanServerAddress() {
        if (logger.finerOn())
            logger.finer("getMBeanServerAddress", "getMBeanServerAddress");

        if (!connected) {
            return null;
        } else {
            return httpConnAddr;
        }
    }

    /**
     * Return a string which represents the MBeanServer
     * identification. This String comes from the MBeanServerDelegate
     * MBean.  If the Connector Client hasn't been connected yet, it
     * returns null.  If the connector Client has been connected and
     * disconnected, getMbeanServerId still returns the previous
     * MbeanServer identification.
     */
    public String getMBeanServerId() {
        if (logger.finerOn())
            logger.finer("getMBeanServerId", "getMBeanServerId");

        return mbeanServerId;
    }

    /**
     * Remote implementation of getMBeanServerId().
     */
    private String getMBeanServerId2() {
        // Set the parameters
        //
        Object[] opList = {
            "getMBeanServerId",
        };

        return (String) invokeRemoteOperationNoExceptions(opList);
    }

    //-------------------------------------------
    // MBean creation and registration operations
    //-------------------------------------------

    /**
     * Creates an registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It
     * returns an ObjectInstance representing the remote MBean created. 
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating the MBean.
     * @param name The name of the MBean to be created.
     *
     * @return An ObjectInstance representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public ObjectInstance createMBean(String className, ObjectName name)
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException {

        // Set the parameters
        //
        Object[] opList = new Object[] {
            "createMBean",
            className,  
            name,
        };

        return createMBeanWithoutLoader(opList);
    }

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns  an ObjectInstance representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating the MBean.
     * @param name The name of the MBean to be created.
     * @param loaderName The name of the class loader to be used by the MBeanServer.
     *
     * @return An ObjectInstance representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     * @exception InstanceNotFoundException The specified loader is not registered in the MBeanServer
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName)
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException, InstanceNotFoundException {

        // Set the parameters
        //
        Object[] opList = new Object[] {
            "createMBeanLoader",
            className,  
            name,
            loaderName,
        };

        return createMBeanWithLoader(opList);
    }

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It
     * returns an ObjectInstance representing the remote MBean created. 
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating
     * the MBean.
     * @param name The name of the MBean to be created.
     * @param params An array containing the parameters of the constructor to be invoked.
     * A parameter can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     * @return An ObjectInstance representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      Object params[], String signature[])
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException {

        // Set the parameters
        //
        Object[] opList = new Object[] {
            "createMBeanParams",
            className,  
            name,
            params,
            signature,
        };

        return createMBeanWithoutLoader(opList);
    }

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns an ObjectInstance representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating
     * the MBean.
     * @param name The name of the MBean to be created.
     * @param loaderName The name of the class loader to be used by the MBeanServer.
     * @param params An array containing the parameters of the constructor to be invoked.
     * A parameter can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     * @return An ObjectInstance representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     * @exception InstanceNotFoundException The specified loader is not registered in the MBeanServer
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName,  Object params[],
                                      String signature[])
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException, InstanceNotFoundException {

        // Set the parameters
        //
        Object[] opList = new Object[] {
            "createMBeanLoaderParams",
            className,  
            name,
            loaderName,
            params,
            signature,
        };

        return createMBeanWithLoader(opList);
    }

    private ObjectInstance createMBeanWithoutLoader(Object[] opList)
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Invoke the remote operation
        //
        try {
            return (ObjectInstance) invokeRemoteOperation(opList);
        } catch (ReflectionException e) {
            throw e;
        } catch (InstanceAlreadyExistsException e) {
            throw e;
        } catch (MBeanRegistrationException e) {
            throw e;
        } catch (MBeanException e) {
            throw e;
        } catch (NotCompliantMBeanException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    private ObjectInstance createMBeanWithLoader(Object[] opList)
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException, InstanceNotFoundException {

        if (!connected)
            throw new CommunicationException("ConnectorClient not connected");

        // Invoke the remote operation
        //
        try {
            return (ObjectInstance) invokeRemoteOperation(opList);
        } catch (ReflectionException e) {
            throw e;
        } catch (InstanceAlreadyExistsException e) {
            throw e;
        } catch (MBeanRegistrationException e) {
            throw e;
        } catch (MBeanException e) {
            throw e;
        } catch (NotCompliantMBeanException e) {
            throw e;
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    //--------------------------------
    // MBean unregistration operations
    //--------------------------------

    /**
     * Deletes an instance of an MBean in the remote MBean server.
     * It also removes its local proxy (ProxyMBean and/or GenericProxy) object from the ProxyFactory.
     *
     * @param name The name of the MBean to be deleted.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception MBeanRegistrationException The preDeregister (MBeanRegistration interface) method of the MBean
     * has thrown an exception.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void unregisterMBean(ObjectName name) throws InstanceNotFoundException, MBeanRegistrationException {
        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set the parameters
        //
        Object[] opList = new Object[] {
            "unregisterMBean",
            name,
        };

        // Invoke the remote operation
        //
        try {
            invokeRemoteOperation(opList);
            proxyHandles.remove(name);
            genericHandles.remove(name);
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (MBeanRegistrationException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    //--------------------------------------------
    // ProxyMBean/GenericProxy creation operations
    //--------------------------------------------

    /**
     * ---------------------------------------------------------
     * Query operations
     * ---------------------------------------------------------
     */

    /**
     * Gets the names of MBeans controlled by the MBeanServer. This method
     * allows any of the following to be obtained: The names of all MBeans,
     * the names of a set of MBeans specified by pattern matching on the
     * ObjectName and/or a Query expression, a specific MBean name (equivalent
     * to testing whether an MBean is registered). When the object name is
     * null or empty, all the objects are to be selected (and filtered if
     * a query is specified). It returns the set of ObjectNames for the
     * MBeans selected.
     *
     * @param name The object name pattern identifying the MBean names to be retrieved. If
     * null or empty, the names of all the registered MBeans will be retrieved.
     * @param query The query expression to be applied for selecting MBeans.
     *
     * @return A set containing the ObjectNames for the MBeans selected.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public Set queryNames(ObjectName name, QueryExp query) {
        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "queryNames",
            name,
            query,
        };

        return (Set) invokeRemoteOperationNoExceptions(opList);
    }

    /**
     * Gets MBeans controlled by the MBeanServer. This method allows any
     * of the following to be obtained: All MBeans, a set of MBeans specified
     * by pattern matching on the ObjectName and/or a Query expression, a
     * specific MBean. When the object name is null or empty, all objects are
     * to be selected (and filtered if a query is specified). It returns the
     * set of ObjectInstances (containing the ObjectName and the Java Class name)
     * for the selected MBeans.
     *
     * @param name The object name pattern identifying the MBeans to be retrieved. If
     * null or empty all the MBeans registered will be retrieved.
     * @param query The query expression to be applied for selecting MBeans.
     *
     * @return A set containing the ObjectInstances for the MBeans selected.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public Set queryMBeans(ObjectName name, QueryExp query) {
        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "queryMBeans",
            name,
            query,
        };

        // Invoke the remote operation
        //
        return (Set) invokeRemoteOperationNoExceptions(opList);
    }

    //--------------------------------
    // Management operations on MBeans
    //--------------------------------

    /**
     * Checks whether an MBean, identified by its object name, is already registered
     * with the MBeanServer.
     *
     * @param name The object name of the MBean to be checked.
     *
     * @return True if the MBean is already registered in the MBeanServer, false otherwise.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public boolean isRegistered(ObjectName name) {
        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "isRegistered",
            name,
        };

        // Invoke the remote operation
        //
        Boolean result = (Boolean) invokeRemoteOperationNoExceptions(opList);
        return result.booleanValue();
    }

    /**
     * Gets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The object name of the MBean from which the attribute is to be retrieved.
     * @param attribute The name of the attribute to be retrieved.
     *
     * @return The value of the retrieved attribute.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception AttributeNotFoundException The specified attribute is not accessible in the MBean.
     * @exception MBeanException  Wraps an exception thrown by the MBean's getter.
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception ReflectionException Wraps an exception thrown while trying to instantiate
     * and apply the operator specified in Modification.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public Object getAttribute(ObjectName name, String attribute)
        throws MBeanException, AttributeNotFoundException, InstanceNotFoundException, ReflectionException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "getAttribute",
            name,
            attribute,
        };

        // Invoke the remote operation
        //
        try {
            return (Object) invokeRemoteOperation(opList);
        } catch (MBeanException e) {
            throw e;
        } catch (AttributeNotFoundException e) {
            throw e;
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (ReflectionException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Allows to retrieve the values of several attributes of an MBean.
     *
     * @param name The object name of the MBean from within which the
     * attributes are to be retrieved.
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return The values of the attributes retrieved.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
        throws InstanceNotFoundException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "getAttributes",
            name,
            attributes,
        };

        // Invoke the remote operation
        //
        return (AttributeList) invokeRemoteOperationOnInstance(opList);
    }

    /**
     * Sets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The name of the MBean within which the attribute is to be set.
     * @param attribute The modification to be performed: The identification of the attribute to
     * be set, the value it is to be set to, and the operator to apply.
     * The value of the attribute can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception AttributeNotFoundException The specified attribute is not accessible in the MBean.
     * @exception InvalidAttributeValueException The specified value for the attribute is not valid. 
     * @exception MBeanException Wraps an exception thrown by the MBean's setter.
     * @exception ReflectionException Wraps an exception thrown while trying to instantiate and apply the
     * operator specified in Modification.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
        throws InstanceNotFoundException, AttributeNotFoundException, InvalidAttributeValueException,
               MBeanException, ReflectionException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "setAttribute",
            name,
            attribute,
        };

        // Invoke the remote operation
        //
        try {
            invokeRemoteOperation(opList);
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (AttributeNotFoundException e) {
            throw e;
        } catch (InvalidAttributeValueException e) {
            throw e;
        } catch (MBeanException e) {
            throw e;
        } catch (ReflectionException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Allows to modify the values of several attributes of an MBean.
     *
     * @param name The object name of the MBean from within which the attributes are
     * to be set.
     * @param attributes A list of the attributes to be set, their values and, optionally, the
     * operators to apply.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @return The values of the attributes that were set.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public AttributeList setAttributes(ObjectName name, AttributeList attributes)
        throws InstanceNotFoundException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "setAttributes",
            name,
            attributes,
        };

        // Invoke the remote operation
        //
        return (AttributeList) invokeRemoteOperationOnInstance(opList);
    }

    /**
     * Invokes a method of an MBean.
     *
     * @param name The name of the MBean on which the method is to be invoked.
     * @param methodName The name of the method to be invoked.
     * @param arguments An array containing the arguments to be set when the method is invoked.
     * An argument can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the method.
     *
     * @return  The object returned by the invocation of the given method.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception MBeanException  Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps an java.lang.Exception thrown while trying to invoke the method.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public Object invoke(ObjectName name, String methodName, Object arguments[], String signature[])
        throws InstanceNotFoundException, MBeanException, ReflectionException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "invoke",
            name,
            methodName,
            arguments,
            signature,
        };

        // Invoke the remote operation
        //
        try {
            return (Object) invokeRemoteOperation(opList);
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (MBeanException e) {
            throw e;
        } catch (ReflectionException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * This method supplies the exposed attributes and actions of the MBean.
     * It provides this information using an MBeanInfo object.
     *
     * @param name The name of the MBean whose attributes and actions will be returned.
     *
     * @return An instance of MBeanInfo which allows all methods and actions of this
     * MBean to be retrieved.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception IntrospectionException An exception occurs during introspection.
     * @exception ReflectionException Wraps a java.lang.Exception thrown while trying
     * to invoke the getMBeanInfo method.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public MBeanInfo getMBeanInfo(ObjectName name)
        throws InstanceNotFoundException, IntrospectionException, ReflectionException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "getMBeanInfo",
            name,
        };

        // Invoke the remote operation
        //
        try {
            return (MBeanInfo) invokeRemoteOperation(opList);
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (IntrospectionException e) {
            throw e;
        } catch (ReflectionException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Gets the ObjectInstance for a given MBean registered with the MBeanServer.
     *
     * @param name The object name of the MBean.
     *
     * @return The ObjectInstance associated to the MBean specified by <VAR>name</VAR>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBeanServer.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public ObjectInstance getObjectInstance(ObjectName name) throws InstanceNotFoundException {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "getObjectInstance",
            name,
        };

        // Invoke the remote operation
        //
        return (ObjectInstance) invokeRemoteOperationOnInstance(opList);
    }

    /**
     * Returns the number of MBeans controlled by the MBeanServer.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public Integer getMBeanCount() {

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "getMBeanCount",
        };

        return (Integer) invokeRemoteOperationNoExceptions(opList);
    }

    /**
     * Returns the default domain used for the MBean naming.
     *
     * @return The default domain used by the MBeanServer.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public String getDefaultDomain() {
        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "getDefaultDomain",
        };

        return (String) invokeRemoteOperationNoExceptions(opList);
    }



    /** Returns true if the MBean specified is an instance of the specified class, false otherwise.
     * 
     * @param name The <CODE>ObjectName</CODE> of the MBean.
     * @param className The name of the class.
     *
     * @return true if the MBean specified is an instance of the specified class, false otherwise.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.          
     */
    public boolean isInstanceOf(ObjectName name, String className) throws InstanceNotFoundException {
        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "isInstanceOf",
            name,
            className,
        };

        // Invoke the remote operation
        //
        Boolean result = (Boolean) invokeRemoteOperationOnInstance(opList);
        return result.booleanValue();
    }

    /**
     * Returns true if the connector server supports the given feature,
     * false otherwise.
     */
    public boolean supports(String feature) {
        Object[] opList = new Object[] {
            "supports",
            feature,
        };

        try {
            Boolean result =
                (Boolean) invokeRemoteOperationNoExceptions(opList);
            return result.booleanValue();
        } catch (CommunicationException e) {
            return false;
            /* This is not great, since genuine communications problems
               will return "false" here instead of throwing an exception.
               But it is hard to get the HTTP error status from the
               underlying layers without considerable reworking, so we
               leave this as it is for now.  */
        }
    }

    //-----------------
    // Local operations
    //-----------------

    /**
     * Given the object name and the Java class name of the MBean(ObjectInstance), this
     * method returns the name of the Java class of the corresponding ProxyMBean.
     * The returned name can be null, if there is no Java class corresponding to
     * the needed ProxyMBean.
     *
     * @param instance The ObjectInstance (ObjectName, Java class name) of the MBean which
     * is represented by the ProxyMBean.
     *
     * @return The name of the Java class of the ProxyMBean.
     */
    public String getClassForProxyMBean(ObjectInstance instance) throws ProxyMBeanInstantiationException {
        if (logger.finerOn())
            logger.finer("getClassForProxyMBean", "getClassForProxyMBean");

        if (instance == null) {
            throw new IllegalArgumentException("ObjectInstance cannot be null");
        }

        return mapper.getClassForProxyMBean(instance);
    }

    //------------------------
    // Notification operations
    //------------------------

    /**
     * Sets the notification forwarding mode, it is the agent to push notifications to the client, if set to
     * <CODE>PULL_MODE</CODE>, it is the client to retrieve notifications from the agent.
     * <P>The default value is <CODE>PUSH_MODE</CODE>.
     *   
     * @param mode set to <CODE>PUSH_MODE</CODE> or <CODE>PULL_MODE</CODE>.
     *
     * @exception IllegalArgumentException Thrown if the mode is not equal to
     * <CODE>PUSH_MODE</CODE> nor <CODE>PULL_MODE</CODE>.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void setMode(int mode) throws IllegalArgumentException {
        if (logger.finerOn())
            logger.finer("setMode", "setMode");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        notificationClientHandler.setMode(mode);
    }

    /**
     * Gets the notification forwarding mode.
     * If set to <CODE>PUSH_MODE</CODE>, it is the agent to push notifications to the client, if set to
     * <CODE>PULL_MODE</CODE>, it is the client to retrieve notifications from the agent.
     * <P>The default value is <CODE>PUSH_MODE</CODE>.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public int getMode() {
        if (logger.finerOn())
            logger.finer("getMode", "getMode");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        return notificationClientHandler.getMode();
    }

    /**
     * Retrieves all notifications in the cache.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void getNotifications() {
        if (logger.finerOn())
            logger.finer("getNotifications", "getNotifications");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        notificationClientHandler.getNotifications();
    }

    /**
     * Specifies the period for notification forwarding in milliseconds.
     * <P>
     * If set to equal to or less than zero and the pull mode is used, no pull will be done. A user should
     * explicitly call <CODE>getNotifications</CODE> to retrieve all notifications in the cache.
     * This method has no effect in the push mode in the current implementation.
     * <P>
     * The default value is 1000 milliseconds.
     *
     * @param period The period in milliseconds.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void setPeriod(int period) {
        if (logger.finerOn())
            logger.finer("setPeriod", "setPeriod");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        notificationClientHandler.setPeriod(period);
    }

    /**
     * Gets the period for notification forwarding in milliseconds.
     * <P>
     * The default value is 1000 milliseconds.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public int getPeriod() {
        if (logger.finerOn())
            logger.finer("getPeriod", "getPeriod");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        return notificationClientHandler.getPeriod();
    }

    /**
     * Clear the notification cache. All notifications saved in the cache then will be discarded
     * without being sent.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void clearCache() {
        if (logger.finerOn())
            logger.finer("clearCache", "clearCache");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        notificationClientHandler.clearCache();
    }

    /**
     * Set the cache size of notifications waiting to be forwarded.
     * <P>If set to <CODE>NO_CACHE_LIMIT</CODE>, notifications will never be discarded,
     * but this may lead to OutOfMemory errors under stressed conditions. If set to zero, any
     * notification will be discarded without being sent.
     * <P>The default value is <CODE>NO_CACHE_LIMIT</CODE>.
     *
     * @param size the maximum number of notifications in the cache.
     * @param discardOverflow effective only if current number of cached notifications exceeds the new size: 
     * if true, discard excess notifications; if false, the cache size will not be changed. 
     *
     * @return The cache size currently set.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public int setCacheSize(int size, boolean discardOverflow) throws JMRuntimeException {
        if (logger.finerOn())
            logger.finer("setCacheSize", "setCacheSize");

        if (!connected)
            throw new CommunicationException("ConnectorClient not connected");

        return notificationClientHandler.setCacheSize(size, discardOverflow);
    }
          
    /**
     * Get the cache size of notifications waiting to be forwarded.
     * <P>If set to <CODE>NO_CACHE_LIMIT</CODE> or a negative value, notifications will never be discarded,
     * but this may lead to OutOfMemory errors under stressed conditions.
     * <P>The default value is <CODE>NO_CACHE_LIMIT</CODE>.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public int getCacheSize() {
        if (logger.finerOn())
            logger.finer("getCacheSize", "getCacheSize"); 
 
        if (!connected)
            throw new CommunicationException("ConnectorClient not connected"); 
 
        return notificationClientHandler.getCacheSize(); 
    }

    /**
     * Sets the number of notifications discarded, this number indicates the number
     * of notifications discarded because the cache limit has been reached.
     * <P>This count will be reset to zero if no more listener exists at the client side,
     * because in this case the notification server will remove all information about 
     * this notification client.
     *
     * @param count The new value to set to overflow count.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void setOverflowCount(int count) {
        if (logger.finerOn())
            logger.finer("setOverflowCount", "setOverflowCount");

        if (!connected)
            throw new CommunicationException("ConnectorClient not connected");

        notificationClientHandler.setOverflowCount(count);
    }

    /**
     * Get the number of notifications discarded since last forwarding because the cache limit has been reached.
     * This value can be reset by calling the method setOverflowCount.
     * <P>This count will be reset to zero if no more listener exists at the client side,
     * because in this case the notification server will remove all information about 
     * this notification client.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public int getOverflowCount() {
        if (logger.finerOn()) 
            logger.finer("getOverflowCount", "getOverflowCount"); 
  
        if (!connected)
            throw new CommunicationException("ConnectorClient not connected");  
  
        return notificationClientHandler.getOverflowCount();
    }

    /**
     * Specify whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the
     * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds.
     * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
     *
     * @param of The overflow mode to specify.
     *
     * @exception IllegalArgumentException Thrown if the mode is not <CODE>DISCARD_NEW</CODE>        
     * nor <CODE>DISCARD_OLD</CODE>.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void setOverflowMode(int of) throws IllegalArgumentException {
        if (logger.finerOn())
            logger.finer("setOverflowMode", "setOverflowMode");

        if (!connected)
            throw new CommunicationException("ConnectorClient not connected");

        notificationClientHandler.setOverflowMode(of);
    }

    /**
     * Returns whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the
     * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds.
     * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
     *
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public int getOverflowMode() {
        if (logger.finerOn()) 
            logger.finer("getOverflowMode", "getOverflowMode");

        if (!connected) 
            throw new CommunicationException("ConnectorClient not connected");

        return notificationClientHandler.getOverflowMode();
    }

    /**
     * Adds a listener to a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be added.
     * @param listener The listener object which will handle the notifications emitted by the registered MBean.
     * @param filter The filter object. If filter is null, no filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void addNotificationListener(ObjectName name, NotificationListener listener, NotificationFilter filter, Object handback)
        throws InstanceNotFoundException {
        if (logger.finerOn())
            logger.finer("addNotificationListener", "addNotificationListener");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        notificationClientHandler.addNotificationListener(name, listener, filter, handback);
    }

    /**
     * Removes a listener from a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be removed.
     * @param listener The listener object which will handle the notifications emitted by the registered MBean.
     * This method will remove all the information related to this listener.
     *
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not registered in the MBean.
     * @exception CommunicationException The connector client is not connected to connector server
     * or a problem was encountered in the connection to the connector server.
     * @exception UnauthorizedSecurityException The authentication information sent to the <CODE>ConnectorServer</CODE> was
     * not correct. Login based authentication failed.
     */
    public void removeNotificationListener(ObjectName name, NotificationListener listener)
        throws InstanceNotFoundException, ListenerNotFoundException {
        if (logger.finerOn())
            logger.finer("removeNotificationListener", "removeNotificationListener");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        notificationClientHandler.removeNotificationListener(name, listener);
    }

    //-----------------------------------------------------------
    // ClientNotificationHandlerInternal interface implementation
    //-----------------------------------------------------------

    /**
     * This method is used to ask a client connector to transfer a
     * request to the agent side. The client connector only needs to
     * forward this request to its server connector, then the server
     * connector will forward this request to its
     * ServerNotificationDispatcher.
     *
     * @param opType an integer specified by the ClientNotificationDispatcher.
     * @param params an array of objects provided by the
     * ClientNotificationDispatcher.
     * @return an array of Objects.
     */
    Object[] remoteRequest(int opType, Object[] params) throws Exception {
        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Set parameters
        //
        Object[] opList = new Object[] {
            "remoteRequest",
            new Integer(opType),
            params,
        };

        // Invoke the remote operation
        //
        try {
            return (Object[]) invokeRemoteOperation(opList);
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (ListenerNotFoundException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * This method is used to start the "push" mode. A client connector should return
     * a ConnectorAddress object which can be used by a server connector to establish
     * a connection. This connection will allow the communication from the server side
     * to client side.
     *
     * @return the connector address used by a server to connect with this client.
     */
    ConnectorAddress startPush() {
        if (logger.finerOn())
            logger.finer("startPush", "startPush");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Create, if required, the NotificationReceiver.
        //
        if (notificationReceiver == null) {
            try {
                if (logger.finestOn()) {
                    logger.finest("startPush","Create new NotificationReceiver");
                }
                notificationReceiver = getNotificationReceiver(this, notificationClientHandler);
            } catch (CommunicationException e) {
                throw e;
            } catch (Exception e) {
                throw new CommunicationException(e);
            }
        }

        // Start receiving events
        //
        if (logger.finestOn()) {
            logger.finest("startPush","Start receiving events");
        }
        notificationReceiver.startListening();

        // Return NotificationReceiver's connector address.
        //
        return notificationReceiver.getConnectorAddress();
    }

    /**
     * This method is used to stop the "push" mode.
     */
    void stopPush() {
        if (logger.finerOn())
            logger.finer("stopPush", "stopPush");

        if (!connected) {
            throw new CommunicationException("ConnectorClient not connected");
        }

        // Stop listening
        //
        if (notificationReceiver != null) {
            notificationReceiver.stopListening();
        }
    }

    // IMPLEMENTATION OF THE HeartBeatClientHandler INTERFACE
    //-------------------------------------------------------

    /**
     * Gets the heartbeat period in milliseconds.
     * <P>
     * The default value is 10000 milliseconds.
     */
    public int getHeartBeatPeriod() {
        if (logger.finerOn()) 
            logger.finer("getHeartBeatPeriod", "getHeartBeatPeriod");

        return heartbeatClientHandler.getHeartBeatPeriod();
    }

    /**
     * Specifies the heartbeat period in milliseconds.
     * <P>
     * If set to zero no check will be carried out for the associated connector server being alive.
     * As the heartbeat is driven by the manager this would also prevent the connector server from
     * being aware of the aliveness of this connector client.
     * <P>
     * The default value is 10000 milliseconds.
     *
     * @param period The heartbeat period in milliseconds.
     */
    public void setHeartBeatPeriod(int period) {
        if (logger.finerOn()) 
            logger.finer("setHeartBeatPeriod", "setHeartBeatPeriod");

        heartbeatClientHandler.setHeartBeatPeriod(period);
    }

    /**
     * Gets the number of retries. This number specifies how many times a connector client must retry
     * the connection to the connector server before sending the heartbeat notification indicating that
     * the connector server has died. If number of retries equals zero then no retries are carried out.
     * <P>
     * The default value is 6 times.
     */
    public int getHeartBeatRetries() {
        if (logger.finerOn()) 
            logger.finer("getHeartBeatRetries", "getHeartBeatRetries");

        return heartbeatClientHandler.getHeartBeatRetries();
    }

    /**
     * Sets the number of retries. This number specifies how many times a connector client must retry
     * the connection to the connector server before sending the heartbeat notification indicating that
     * the connector server has died. If number of retries equals zero then no retries are carried out.
     * <P>
     * The default value is 6 times.
     *
     * @param nretries The number of retries.
     */
    public void setHeartBeatRetries(int nretries) {
        if (logger.finerOn()) 
            logger.finer("setHeartBeatRetries", "setHeartBeatRetries");

        heartbeatClientHandler.setHeartBeatRetries(nretries);
    }

    /**
     * Adds the specified heartbeat listener to receive heartbeat notifications from this connector client.
     * Heartbeat notifications occur when the connector client connects to or disconnects from the connector
     * server or when the connector server associated to this connector client dies or is temporarily unreachable.
     *
     * @param listener The heartbeat listener which will handle the notifications emitted by the connector client.
     * @param filter The filter object. If filter is null, no filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a notification is emitted.
     */
    public void addHeartBeatNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) {
        if (logger.finerOn()) 
            logger.finer("addHeartBeatNotificationListener", "addHeartBeatNotificationListener");

        heartbeatClientHandler.addHeartBeatNotificationListener(listener, filter, handback);
    }

    /**
     * Removes the specified heartbeat listener so that it no longer receives heartbeat notifications from
     * this connector client.
     * Heartbeat notifications occur when the connector client connects to or disconnects from the connector
     * server or when the connector server associated to this connector client dies or is temporarily unreachable.
     *
     * @param listener The heartbeat listener which will handle the notifications emitted by the connector client.
     */
    public void removeHeartBeatNotificationListener(NotificationListener listener) {
        if (logger.finerOn()) 
            logger.finer("removeHeartBeatNotificationListener", "removeHeartBeatNotificationListener");

        heartbeatClientHandler.removeHeartBeatNotificationListener(listener);
    }

    // IMPLEMENTATION OF THE HeartBeatInternalClientHandler INTERFACE
    //---------------------------------------------------------------

    /**
     * Ping heartbeat server.
     */
    String pingHeartBeatServer(String sessionId, int period, int nretries, Long notifSessionId) {
        // Set parameters
        //
        Object[] opList = new Object[] {
            "pingHeartBeatServer",
            sessionId,
            new Integer(period),
            new Integer(nretries),
            notifSessionId,
        };

        return (String) invokeRemoteOperationNoExceptions(opList);
    }

    // package stuff
    String getHost() {
        return localHost;
    }

    // PRIVATE CLASSES
    //----------------

    /**
     * Notification
     */
    private class NotificationHandlerInternal implements ClientNotificationHandlerInternal {

        /**
         * Ctor
         */
        public NotificationHandlerInternal(GenericHttpConnectorClient connector) {
            this.connector = connector;
        }

        /**
         * This method is used to ask a client connector to transfer a request to the agent side. The
         * client connector only needs to forward this request to its server connector, then the server
         * connector will forward this request to its ServerNotificationDispatcher.
         *
         * @param opType an integer specified by the ClientNotificationDispatcher.
         * @param params a set of objects provided by the ClientNotificationDispatcher.
         * @return a set of Objects.
         */
        public Object[] remoteRequest(int opType, Object[] params) throws Exception {
            return connector.remoteRequest(opType, params);
        }

        /**
         * This method is used to start the "push" mode. A client connector should return
         * a ConnectorAddress object which can be used by a server connector to establish
         * a connection. This connection will allow the communication from the server side
         * to client side.
         *
         * @return the connector address used by a server to connect with this client.
         */
        public ConnectorAddress startPush() {
            return connector.startPush();
        }

        /**
         * This method is used to stop the "push" mode and change to the "pull" mode.
         *
         * @param address the connector address used by a server to connect with this client.
         */
        public void stopPush(ConnectorAddress address) {
            connector.stopPush();
        }

        private GenericHttpConnectorClient connector;
    }

    /**
     * HeartBeat
     */
    private class HeartBeatInternalClientHandlerImpl implements HeartBeatInternalClientHandler {

        /**
         * Ctor
         */
        public HeartBeatInternalClientHandlerImpl(GenericHttpConnectorClient connector) {
            this.connector = connector;
        }

        /**
         * Get remote MBean server.
         */
        public RemoteMBeanServer getRemoteMBeanServer() {
            return connector;
        }

        /**
         * Ping heartbeat server.
         */
        public String pingHeartBeatServer(String sessionId, int period, int nretries, Long notifSessionId) {
            return connector.pingHeartBeatServer(sessionId, period, nretries, notifSessionId);
        }

        private GenericHttpConnectorClient connector;
    }

    // PRIVATE METHODS
    //----------------

    /**
     * This method formats the remote operation into an HTTP Request
     * PDU, sends it to the server and waits for the HTTP Response PDU
     * in order to extract the result.
     */
    private Object invokeRemoteOperation(Object[] opList) throws Exception {
        if (logger.finerOn()) {
            final String what = (String) opList[0];
            logger.finer("invokeRemoteOperation", what);
        }
        InputStream in = sendHttp(opList, false);
        try {
            return readObjectValue(in);
        } finally {
            in.close();
        }
    }

    /**
     * Call invokeRemoteOperation and wrap its exceptions.  The remote
     * operation must be a method in MBeanServer that does not declare
     * any exceptions in its "throws" clause.
     */
    private Object invokeRemoteOperationNoExceptions(Object[] opList) {
        try {
            return invokeRemoteOperationOnInstance(opList);
        } catch (InstanceNotFoundException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Call invokeRemoteOperation and wrap its exceptions.  The remote
     * operation must be a method in MBeanServer that declares only
     * InstanceNotFoundException in its "throws" clause.
     */
    private Object invokeRemoteOperationOnInstance(Object[] opList)
            throws InstanceNotFoundException {
        try {
            return invokeRemoteOperation(opList);
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new CommunicationException(e);
        }
    }


    /**
     * This method extracts the object from the entity body of an HTTP response.
     */
    private Object readObjectValue(InputStream in) throws Exception {
        String typeStr;
        ObjectInputStream objIn;

        objIn = new ObjectInputStream(in);
        typeStr = (String) objIn.readObject();

        if (logger.finestOn())
            logger.finest("readObjectValue", "Received object of type " + 
                          typeStr);

        Object result = objIn.readObject();
        if (logger.finestOn())
            logger.finest("readObjectValue", "Received object [" + 
                          result  + "]");
        if (result instanceof Exception && typeStr.equals("Exception")) {
            if (logger.finestOn())
                logger.finest("readObjectValue", "Throw exception :" + 
                              result);
            throw (Exception) result;
        }
        else {
            if (logger.finestOn())
                logger.finest("readObjectValue", "Return value : " + 
                              result);
            return result;
        }
    }
    
    // DEBUG STUFF
    //------------
    private static final ClassLogger logger = 
        new ClassLogger(ClassLogger.LOGGER_COMM,
                        "GenericHttpConnectorClient");

    // VARIABLES
    //----------

    /**
     * MBean Operation Context
     */
    private OperationContext operationContext = null;

    /**
     * Authentication
     */
    transient Hashtable authSchemeInfoList = null;

    /**
     * ConnectorAddress
     */
    transient GenericHttpConnectorAddress httpConnAddr = null;

    /**
     * Socket factory
     */
    transient GenericHttpSocketFactory factory = null;

    /**
     * Connection flag
     */
    private transient boolean connected = false;

    /**
     * Whether the remote end supports OperationContexts.
     */
    private transient boolean supportsOperationContext = false;

    /**
     * MBeanServerId
     */
    private String mbeanServerId = null;

    /**
     * Mapper
     */
    private transient Mapper mapper = null;
    private transient Mapper defaultMapper = null;

    /**
     * ClassLoader
     */
    private transient ClassLoader classloader = null;

    /**
     * Proxies
     */
    private transient Hashtable proxyHandles = new Hashtable();
    private transient Hashtable genericHandles = new Hashtable();

    /**
     * Notifications
     */
    private GenericHttpNotificationReceiver notificationReceiver = null;
    private ClientNotificationDispatcher notificationClientHandler = null;

    /**
     * HeartBeat
     */
    private HeartBeatClientHandlerImpl heartbeatClientHandler = null;

    /**
     * localHost 
     */
    // NPCTE fix for bugId 4523504, esc 0, MR, 28-Nov-2001
    private String localHost = null ;
    // end of NPCTE fix for bugId 4523504
}
