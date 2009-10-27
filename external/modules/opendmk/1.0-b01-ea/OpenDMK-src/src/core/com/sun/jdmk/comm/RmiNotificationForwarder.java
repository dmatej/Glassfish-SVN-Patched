/*
 * @(#)file      RmiNotificationForwarder.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.16
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



import java.util.*;
import java.net.*;
import java.lang.reflect.*;

// rmi import
//
import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;

// jdmk/jmx import
//
import javax.management.*;
import com.sun.jdmk.*;

import com.sun.jdmk.internal.ClassLogger;

/**
 * The <CODE>RmiNotificationForwarder</CODE> class provides an implementation of
 * a notification handler based on the remote method invocation (RMI) system.
 * It handles notifications emitted through a listener on behalf of a specific client.
 *
 */

class RmiNotificationForwarder implements NotificationBackConnector {

    /**
     * Instantiates an event handler for forwarding events from a specific 
     * object.
     *
     * @param receiverAddress The address of the RMI service to be called when
     *        the notification is forwarded.
     * @param rmiConnectorServer The Rmi connector Server
     */
    public RmiNotificationForwarder(RmiConnectorAddress receiverAddress, 
				    RmiConnectorServer rmiConnectorServer)
	throws RemoteException,IllegalAccessException {

	this.receiverAddress= receiverAddress;
	this.rmiConnectorServer= rmiConnectorServer;

        // ---------------------------
        // build the RMI url 
        // ---------------------------
        String receiverName = new String ("rmi://" + receiverAddress.getHost() + ":" + receiverAddress.getPort()  + "/" +  receiverAddress.getName() );
        if (logger.finestOn()) {
	    logger.finest("Constructor","looking for " + receiverName);
        }

        // ---------------------------
        // We are going to try to get a reference on the receiver
        // ---------------------------
        try
	    {
                this.rmiNotificationReceiver = (RmiNotificationReceiver)Naming.lookup(receiverName);
                connected = true ;
                if ( this.rmiNotificationReceiver == null)
		    {
                        if (logger.finestOn())
			    {
                                logger.finest("handleEvent","receiver is null");
			    }
                        throw new RemoteException("Can't contact RMI Notification receive server at " + receiverName);
		    }
	    }
        catch (Exception x)
	    {
                throw new RemoteException("Can't contact RMI Notification receive server at " + receiverName);
	    }
    }

    public void disconnect () {
        connected = false ;
    }

    // ===================================================================
    //
    // NotificationBackConnector interface implementation
    //
    // ===================================================================

    /**
     * used to ask an agent connector to transfer a request to client side. The agent connector
     * only needs to forwards this request to its client connector, then the client connector will
     * forward this request to its ClientNotificationDispatcher.
     *
     * @param opType an integer specified by a ServerNotificationDispatcher.
     * @param params a set of objects provided by a ServerNotificationHandler.
     * @return a set of Objects.
     */
    public Object[] remoteRequest(int opType, Object[] params)
        throws Exception {
        if ( connected )
            {
                return rmiNotificationReceiver.remoteRequest(opType,params) ;
            }
        throw new CommunicationException("NotificationForwarder not connected");
    }

    // ===================================================================
    //
    // PRIVATE METHODS
    //
    // ===================================================================

    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_LEGACY_RMI,
		        "com.sun.jdmk.comm.RmiNotificationForwarder");
    // PRIVATE VARIABLES
    //------------------
    private boolean connected= false ;
    private RmiConnectorAddress receiverAddress= null ;
    private RmiConnectorServer rmiConnectorServer =null ;
    private RmiNotificationReceiver rmiNotificationReceiver =null ;
}
