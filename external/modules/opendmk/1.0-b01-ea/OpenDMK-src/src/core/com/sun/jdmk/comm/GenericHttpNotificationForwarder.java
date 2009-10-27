/*
 * @(#)file      GenericHttpNotificationForwarder.java
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
import java.net.Socket;
import java.util.Iterator;
import java.util.ArrayList;

// jmx import
//
import javax.management.InstanceNotFoundException;
import javax.management.ListenerNotFoundException;

// jdmk import
//
import com.sun.jdmk.internal.ClassLogger;

/**
 * This class implements the common behavior for the notification
 * forwarders in the HTTP-based connectors.
 *
 */

abstract class GenericHttpNotificationForwarder 
    extends GenericHttpConnectorClient implements NotificationBackConnector {

    /**
     * Size of the buffer used to read from the socket. 
     **/
    private final static int BUFFER_SIZE = 256;

    /**
     * Initializes this <CODE>GenericHttpNotificationForwarder</CODE>.
     */
    public GenericHttpNotificationForwarder(GenericHttpConnectorAddress address) {
        //
        // Initialize internal variables.
        //
        httpConnAddr = address;

        //
        // Initialize the socket factory.
        //
        factory = getSocketFactory();
    }

    // HTTP SPECIFIC METHODS
    //----------------------

    /**
     * Sends the formatted HTTP request with its content.
     */
    private byte[] sendHttp(final Object[] al) throws Exception {
        //
        // Format entity body for HTTP request
        //
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        final ObjectOutputStream objOut = new ObjectOutputStream(out);
	final int len = al.length;
	for (int i=0; i < len ; i++) { objOut.writeObject(al[i]); }
        final byte[] request_content = out.toByteArray();

        //
        // Establish connection to the server, send HTTP request and wait 
	// for HTTP response.
        //
        Socket socket = null;
        ByteArrayOutputStream response_content = null;
        try {
            //
            // Open connection
            //
            socket = factory.createSocket(httpConnAddr.getHost(), 
					  httpConnAddr.getPort(), this);

            //
            // Send HTTP request
            //
            final OutputStream outputStream = socket.getOutputStream();
            if (request_content.length != 0) {
                outputStream.write(request_content);
            }
            outputStream.flush();

            //
            // Wait for HTTP response
            //
            final InputStream inputStream = socket.getInputStream();

            //
            // Retrieve entity body from HTTP response
            //
	    final byte[] buffer = new byte[BUFFER_SIZE];
            int byte_count;
            response_content = new ByteArrayOutputStream(BUFFER_SIZE);
            while ((byte_count=inputStream.read(buffer,0,BUFFER_SIZE))!=-1) {
                response_content.write(buffer,0,byte_count);
            }
        } catch (Exception e) {
            throw e;
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

        return response_content.toByteArray();
    }

    // ----------------------
    // Communication handling
    // ----------------------

    public String connect() {
        if (logger.finerOn())
            logger.finer("connect", "connect");

        //
        // Set connection flag to true.
        //
        connected = true;

        return null;
    }

    /**
     * Terminates the communication with the NotificationReceiver.
     */
    public void disconnect() {
        if (logger.finerOn())
            logger.finer("disconnect", "disconnect");

        //
        // Set connection flag to false.
        //
        if (connected) {
            httpConnAddr = null;
            connected = false;
        }
    }

    //---------------------------------------------------
    // NotificationBackConnector interface implementation
    //---------------------------------------------------

    /**
     * This method is used to ask a client connector to transfer a request to the agent side. The
     * client connector only needs to forward this request to its server connector, then the server
     * connector will forward this request to its ServerNotificationDispatcher.
     *
     * @param opType an integer specified by the ClientNotificationDispatcher.
     * @param params a set of objects provided by the ClientNotificationDispatcher.
     * @return a set of Objects.
     */
    public Object[] remoteRequest(int opType, Object[] params) 
	throws Exception {
        if (logger.finerOn())
            logger.finer("remoteRequest", "remoteRequest");

        if (!connected) throw new 
	    CommunicationException("NotificationForwarder not connected");

        // Set parameters
        //
        final Object[] aList = {
	    "remoteRequest",
	    new Integer(opType),
	    params 
	};

        try {
            final byte[] responseEntityBody = sendHttp(aList);
            if (responseEntityBody.length == 0) {
                throw new IllegalAccessException("Entity Body in HTTP Response is empty");
            }
            final Object result = readObjectValue(responseEntityBody);
            if (result instanceof Exception) {
                throw (Exception) result;
            } else {
                return (Object[]) result;
            }
        } catch (InstanceNotFoundException e) {
            throw e;
        } catch (ListenerNotFoundException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (IOException e) {
            if (logger.finestOn())
                logger.finest("remoteRequest", "Cannot get ObjectInputStream");
            throw new CommunicationException(e);
        } catch (Exception e) {
            if (logger.finestOn())
                logger.finest("remoteRequest", "Unexpected exception");
            throw new CommunicationException(e);
        }
    }

    // PRIVATE METHODS
    //----------------

    /**
     * This method extracts the object from the entity body of an HTTP response.
     */
    private Object readObjectValue(byte[] entityBody) 
	throws IOException, ClassNotFoundException {
        if (entityBody == null) {
            return null;
        }

        final ByteArrayInputStream bIn = 
	    new ByteArrayInputStream(entityBody);
        final ObjectInputStream objIn = 
	    new ObjectInputStream(bIn);
        final String typeStr = (String) objIn.readObject();

        if (logger.finestOn())
            logger.finest("readObjectValue", "Received object of type " + 
			  typeStr);

        return objIn.readObject();
    }

    // DEBUG STUFF
    //------------
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_COMM,
		        "GenericHttpNotificationForwarder");
    // VARIABLES
    //----------

    /**
     * Connection flag
     */
    private transient boolean connected = false;
}
