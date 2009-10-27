/*
 * @(#)file      GenericHttpNotificationReceiver.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.18
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
import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;

/**
 * This class implements the common behavior for the notification
 * receivers in the HTTP-based connectors.
 *
 */

abstract class GenericHttpNotificationReceiver extends CommunicatorServer {

    /**
     * Gets an instance of the socket factory used by this connector.
     */
    abstract GenericHttpSocket createSocket();

    /**
     * Initializes this <CODE>GenericHttpNotificationReceiver</CODE>.
     */
    public GenericHttpNotificationReceiver(int connectorType, int port,
                                           GenericHttpConnectorClient connector,
                                           ClientNotificationDispatcher dispatcher) {
        super(connectorType);
        this.port = port;
        this.connector = connector;
        this.dispatcher = dispatcher;
        maxActiveClientCount = 10; // This overrides the inherited default (1)
    }

    // Notification mechanism specific methods
    //----------------------------------------

    public void startListening() {
        start();
        if (!waitState(ONLINE,60000)) {
	    throw new CommunicationException("Notification Server could not be started");
	}

	connAddr = new HttpConnectorAddress(connector.getHost(), sockListen.getLocalPort());
    }

    public void stopListening() {
        stop();
    }

    public GenericHttpConnectorAddress getConnectorAddress() {
        return connAddr;
    }

    // SUBCLASSING OF CommunicatorServer
    //----------------------------------

    /**
     */
    protected void doError(Exception e) throws CommunicationException {
	if (e instanceof CommunicationException) {
	    throw (CommunicationException) e;
	} else {
	    throw new CommunicationException(e);
	}
    }

    /**
     */
    protected void doBind()
        throws InterruptedException, CommunicationException {
        try {
            sockListen = createSocket().createServerSocket(port);
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
        GenericHttpNotificationReceiverHandler server = new GenericHttpNotificationReceiverHandler(this, getServedClientCount(),
                                                                                                   (GenericHttpSocket) sockListen.clone());
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
    protected String makeDebugTag() {
	return "GenericHttpNotificationReceiver[" + getProtocol() + ":" + getPort() + "]";
    }

    /**
     * Stops this server.
     */
    public void stop() {
        if (isActive()) {
            super.stop();
            try{
                GenericHttpSocket sn = sockListen.createClientSocket();
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null)
                    sn.doConnect(System.getProperty("jdmk.hostname"), port);
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
  
    /**
     * Gets the IP address of the last connected client.
     * This function uses the string representation of <CODE>java.net.InetAddress</CODE>.
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

    // PROTECTED VARIABLES
    //--------------------

    /**
     */
    protected transient GenericHttpSocket sockListen = null;

    /**
     */
    protected InetAddress addrLastClient = null;

    // PRIVATE VARIABLES
    //------------------

    /**
     * A reference to the connector client that uses this receiver.
     */
    private GenericHttpConnectorClient connector = null;

    /**
     * The Receiver Connector Address
     */
    private GenericHttpConnectorAddress connAddr = null;

    /**
     * The current dispatcher
     */
    ClientNotificationDispatcher dispatcher = null;

    // PRIVATE VARIABLES
    //------------------

    /**
     * Name of the naming attributes
     */
    private static final String InterruptSysCallMsg = "Interrupted system call";
}
