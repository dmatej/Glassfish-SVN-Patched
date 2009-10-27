/*
 * @(#)file      RmiNotificationReceiverImplV2.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.9
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

// jdmk/jmx import
//
import javax.management.*;
import com.sun.jdmk.*;
import com.sun.jdmk.internal.ClassLogger;


// rmi import
//
import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;



/**
 * The <CODE>RmiNotificationReceiverImplV2</CODE> class provides an
 * implementation of the <CODE>RmiNotificationReceiverImpl</CODE>
 * interface based on the Java remote method invocation (RMI) system.
 *
 *
 * @since Java DMK 5.0
 */

class RmiNotificationReceiverImplV2 extends UnicastRemoteObject implements NotificationBackConnector {
    private static final long serialVersionUID = -2564173411662310647L;

    public RmiNotificationReceiverImplV2 (RmiConnectorClient client, RmiConnectorAddress rmiAddress, ClientNotificationDispatcher dispatcher) throws RemoteException, CommunicationException {
    
        super();
    
        myClient= client;
        this.dispatcher= dispatcher;
    
	localAddress = new RmiConnectorAddressV2() ; 
	localAddress.setRemoteObj(this);
    }
  
    public void stopListening() {
        offline= true;
    }

    public void startListening() {
        offline= false;
    }

    public Object[] remoteRequest(int opType, Object[] params) throws Exception {
    
        if (offline)
            // do not do anything ...
            //
            return null;
    
        if (logger.finestOn()) {
        	logger.finest("RmiNotificationReceiverImpl::handleEvent","Start event dispatching");
        }
     
        // Identify the source of the event. Find the local object associated
        // to the source
        //   
	return dispatcher.remoteRequest(opType,params) ;
    }
        
    // GETTERS AND SETTERS
    //--------------------


    /**
     * Returns the name of the service exposed.
     */
    public RmiConnectorAddress getAddress() {
        return localAddress;
    }

    // PRIVATE METHODS
    //------------------
    private String localClassName = "com.sun.jdmk.comm.RmiConnectorClient" ;
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_LEGACY_RMI,
			"RmiConnectorClient");
 

    // PRIVATE VARIABLES
    //------------------

    /**
     * A reference to the client that uses the implementation.
     */
    private transient RmiConnectorClient myClient= null;

    /**
     * The Receiver Address
     */
    private RmiConnectorAddressV2 localAddress = null ;

    /**
     * The service name
     */
    private String serviceName = null ;

    /**
     * The actual dispatcher
     */
    private ClientNotificationDispatcher dispatcher = null ;

    /**
     * The host name.
     */
    private String localHost= "localhost";

    /**
     * List of dispatcher
     */
    private ThreadGroup group;

    /**
     * Indicates if the event dispatcher should be offline.
     */
    private boolean offline= false;

}
