/*
 * @(#)file      ActualMonitor.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.18
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

package com.sun.jdmk.discovery;

// ----------------------------------------------------------
// java import
// ----------------------------------------------------------
import java.util.*;
import java.io.*;
import java.lang.*;
import java.net.*;

// ----------------------------------------------------------
// jmx-remote
// ----------------------------------------------------------
import javax.management.remote.JMXServiceURL;

// ----------------------------------------------------------
// jdmk import
// ----------------------------------------------------------
import javax.management.*;
import com.sun.jdmk.* ; 
import com.sun.jdmk.internal.ClassLogger;



class ActualMonitor extends DiscoveryCommon implements java.io.Serializable {
    private static final long serialVersionUID = 3370752883861960954L;
    
    
    // ----------------------------------------------------------
    // Constructor
    // ----------------------------------------------------------
    
    public ActualMonitor (String multicastGroup, int multicastPort,DiscoveryMonitor monitor )
	throws IOException 
    {
	// ------------------------
	// multicast socket initialization
	// ------------------------
	super (multicastGroup,multicastPort) ;
	localClassName = "com.sun.jdmk.discovery.ActualMonitor" ;
	if (logger.finerOn())
	    {
		logger.finer("constructor " , "initialize multicast socket");
	    }
	
	// ------------------------
	// set Default multicastPort/multicastGroup
	// ------------------------
	this.multicastGroup = multicastGroup ;
	this.multicastPort  = multicastPort ;
	
	// ------------------------
	// set DiscoveryMonitor
	// ------------------------
	this.monitor = monitor ;
	
    }
    
    
    // ----------------------------------------------------------
    // Run method
    // ----------------------------------------------------------
    
    /**
     * For Java DMK internal use only.
     * <p>
     * <CODE>run</CODE> method executed by the ActualMonitor's main thread.
     */
    public void run ()
    {
	if (logger.finerOn())
	    {
		logger.finer("run " , "start") ;
	    }
	// ------------------------
	// join multicast group
	// ------------------------
	try {
	    connectToGroup () ;
	} catch (IOException e ) {
	    synchronized(lock) {
		stopRequested = true ;
	    }
	}
	
	// ------------------------
	// Infinite loop
	// ------------------------
	while (!stopped()) {
	    // ------------------------
	    // Set state
	    // ------------------------
	    monitor.changeState(DiscoveryMonitor.ONLINE) ;
	
	    try {
		// ------------------------
		// Reveice msg
		// ------------------------
		if (logger.finerOn()) {
		    logger.finer("run " , "Start Waiting ");
		}
		ResponsePDU pdu = (ResponsePDU) receiveMsg((DatagramSocket)this) ;
		
		// ------------------------
		// Process msg
		// ------------------------
		processMsg(pdu) ;
	    } catch (Exception e) {
		if (stopped()) {

		    break;
		} else {
		    if (logger.finestOn()) {
			logger.finest("run " , "Got exception: "+e.toString(), e) ;
		    }
		}	    
	    }
	}
	    
	// ------------------------
	// leave multicast group
	// ------------------------
	try {
	    monitor.changeState(DiscoveryMonitor.OFFLINE) ;

	    disconnectFromGroup () ;
	    close() ;
	} catch (IOException e ) {
	    if (!stopped()) {
		if (logger.finestOn()) {
		    logger.finest("run " , e) ;
		}
	    }
	}

	if (logger.finestOn()) {
	    logger.finest("run ", "stopped.") ;
	}
    }


    // ----------------------------------------------------------
    // stopMonitor
    // ----------------------------------------------------------
    public void stopMonitor() {
	synchronized(lock) {
	    if (stopRequested) {
		return;
	    }

	    stopRequested = true ;
	}

	try {
	    this.close();
	} catch (Exception e) {
	    if (logger.finestOn()) {
		logger.finest("stopMonitor " , "Got exception during closing.", e) ;
	    }
	}
    }
    
    // ----------------------------------------------------------
    // Private processMsg
    // ----------------------------------------------------------
    private void processMsg (ResponsePDU rcvPDU)
	throws IOException
    {
	
	if ( rcvPDU ==  null )
	    {
		if (logger.finerOn())
		    {
			logger.finer("processMsg " , "received a nul pdu '") ;
		    }
		return ;
	    }
	if (logger.finerOn())
	    {
		logger.finer("processMsg " , "received pdu '" + rcvPDU.printState() + "'") ;
	    }
	
        // ------------------------
        // Check if it's an event msg
        // ------------------------
	if ( rcvPDU.getEvent() ==  false )
	    {
		if (logger.finerOn())
		    {
			logger.finer("processMsg " , "not an event message") ;
		    }
		return ;
	    }
	
        // ------------------------
        // Create and initialize a new DiscoveryResponse object 
        // ------------------------
        DiscoveryResponse result = new DiscoveryResponse() ;
        result.host =  rcvPDU.getHost() ;
        result.mbeanServerId         = rcvPDU.getMbeanServerId() ;
        result.specificationName     = rcvPDU.getSpecificationName () ;
        result.specificationVendor   = rcvPDU.getSpecificationVendor () ;
        result.specificationVersion  = rcvPDU.getSpecificationVersion () ;
        result.implementationName    = rcvPDU.getImplementationName () ;
        result.implementationVendor  = rcvPDU.getImplementationVendor () ;
        result.implementationVersion = rcvPDU.getImplementationVersion () ;
        result.objectList            = rcvPDU.getObjectList () ;
	result.userData              = rcvPDU.getUserData () ;

	// jmx-remote
	if (rcvPDU.serverAddresses != null) {
	    result.serverAddresses = rcvPDU.serverAddresses;
	} else {
	    result.serverAddresses = new JMXServiceURL[0];
	}

        // ------------------------
        // Create and initialize a new DiscoveryResponderNotification object 
        // ------------------------
	DiscoveryResponderNotification event = new DiscoveryResponderNotification(monitor ,rcvPDU.getAgentState(), result, sequenceNumber++ ) ;
	
        // ------------------------
        // Deliver event
        // ------------------------
	monitor.sendNotification(event) ;
	
    }
    
    private final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
			"ActualMonitor");

    private boolean stopped() {
	synchronized(lock) {
	    return stopRequested;
	}
    }

    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    private static long             sequenceNumber          = 0  ;
    private static int              defaultMulticastPort    = 9000  ;
    private static String           defaultMulticastGroup   = "224.224.224.224" ;
    private int                     multicastPort           ;
    private String                  multicastGroup          ;
    
    private Thread			listeningThread 	= null ;
    private volatile boolean	stopRequested		= false ;
    private transient int 		state			= DiscoveryMonitor.OFFLINE ;
    
    private transient DiscoveryMonitor monitor		=  null ;
    
    private Vector			listeners		= new Vector();

    private final int[] lock = new int[0];
}
