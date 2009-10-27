/*
 * @(#)file      ActualDiscovery.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.19
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

import java.util.*;
import java.net.*;
import java.io.*;

import javax.management.remote.JMXServiceURL;

// ------------------------
// jdmk import
// ------------------------
import javax.management.*;
import com.sun.jdmk.* ; 
import com.sun.jdmk.internal.ClassLogger;



class ActualDiscovery extends DiscoveryCommon {

    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    private boolean PointToPointResponse = true ;
    private int	RETURN_PORT_MIN_VAL	 = 1024 ;
    private int	RETURN_PORT_MAX_VAL	 = RETURN_PORT_MIN_VAL * 10 ;
    private int	ttl			 = 1 ;
    private int	defaultTtl		 = 1 ;
    private int	timeOut			 = 1000 ;
    private String timeStamp		 = null ;
    public Vector returnValue		 = new Vector() ;
 
    // Constructor
    // ----------------------------------------------------------
    public ActualDiscovery (String multicastGroup , 
			    int multicastPort) 
	throws IOException {
						
	// ------------------------
	// Set multicastSocket parameters
	// ------------------------
	super(multicastGroup,multicastPort) ;

	localClassName = "com.sun.jdmk.discovery.ActualDiscovery" ;
	if (logger.finerOn()) {
	    logger.finer("constructor" , "group = " + multicastGroup  + 
			 ", " + "port  = " + multicastPort ) ;
	}
    }

    // ----------------------------------------------------------
    // Get/set time out
    // ----------------------------------------------------------
    public void setTimeOut(int timeOut) throws IOException {
	if (logger.finerOn()) {
	    logger.finer("setTimeOut" , "set to " + timeOut ) ;
	}
	setSoTimeout(timeOut) ;
	this.timeOut = timeOut ;
    }

    public int getTimeOut() {
	if (logger.finerOn()) {
	    logger.finer("getTimeOut" , "Value = " + timeOut ) ;
	}
	return timeOut ;
    }

    // ----------------------------------------------------------
    // ttl
    // ----------------------------------------------------------
    /**
     * Set the default time-to-live for multicast packets sent out on this 
     * socket. The TTL sets the IP time-to-live for
     * DatagramPackets sent to a MulticastGroup, which specifies how many 
     * "hops" that the packet will be
     * forwarded on the network before it expires. 
     *
     * The ttl must be in the range 0 < ttl <= 255 or an 
     * IllegalArgumentException will be thrown.
     */
    public void setTimeToLive(int ttl) throws IOException {
	try {
	    super.setTimeToLive(ttl) ;
	} catch (IOException e) {
	    if (logger.finestOn()) {
		logger.finest("setTimeToLive" , 
			      "Unable to set Time-to-live to " + ttl ) ;
	    }
	    throw e ;
	}

	if (logger.finerOn()) {
	    logger.finer("Time-to-live" , "Set Time-to-live to " + ttl ) ;
	}
        this.ttl = ttl ;
    }
  
    public int getTimeToLive() {
        return ttl ;
    }

    // ----------------------------------------------------------
    // set/get response port 
    // ----------------------------------------------------------
    public boolean getPointToPointResponse() {
	return PointToPointResponse ;
    }

    public void setPointToPointResponse(boolean PointToPointResponse) {
	if (logger.finerOn()) {
	    logger.finer("setPointToPointResponse" , 
			 "setPointToPointResponse to " + 
			 PointToPointResponse ) ;
	}
	this.PointToPointResponse = PointToPointResponse ;
    }

    // ----------------------------------------------------------
    // findCommunicators
    // ----------------------------------------------------------
    public Vector findCommunicators (boolean adp , String selectedHost) 
	throws IOException {
        // ------------------------
        // return part
        // ------------------------
	return  getObject(adp , selectedHost) ;

    }


    // ----------------------------------------------------------
    // getObject
    // ----------------------------------------------------------
    private Vector getObject (boolean objectRequired, String selectedHost) 
	throws IOException {

        // ------------------------
        // Set the return object
        // ------------------------
	returnValue = new Vector() ;

        // ------------------------
        // If PointToPoint response is required, create a DatagramSocket
        // ------------------------
        DatagramSocket socket = null ;
        int         returnPort = RETURN_PORT_MIN_VAL ;
	
        InetAddress returnAddr;
	String localHostName = null;
	if (!getInterface().getHostAddress().equals("0.0.0.0")) {
	    returnAddr = getInterface();
	    localHostName = returnAddr.getHostAddress();
	}
	else {
	    returnAddr = getLocalHostAddr();
	    localHostName = getLocalHostName();
	}
	
	if (logger.finerOn()) {
	    logger.finer("getObject" , 
			 " returnAddr is set to "+  returnAddr) ;
	}

        if ( getPointToPointResponse() ) {
	    if (logger.finerOn()) {
		logger.finer("getObject" , 
			     "Create datagram socket for response" ) ;
	    }
	    for ( ; returnPort < RETURN_PORT_MAX_VAL ; returnPort++ ) {
		try {
		    socket =  new DatagramSocket(returnPort,returnAddr) ;
		    socket.setSoTimeout(getTimeOut() ) ;
		    break ;
		} catch (BindException e) {
		    if (logger.finestOn()) {
			logger.finest("getObject" , 
				      " try port " + returnPort ) ;
		    }
		}
	    }
	    
	    if ( returnPort == RETURN_PORT_MAX_VAL ) {
		throw new IOException("Address already in use") ;
	    }
        } else {
	    if (logger.finerOn()) {
		logger.finer("getObject" , 
			     "Use multicast socket for response" ) ;
	    }
	    socket = (DatagramSocket) this ;
        }
	
	// ------------------------
	// pdu formating
	// ------------------------
	if (logger.finerOn()) {
	    logger.finer("getObject" , "Format msg") ;
	}
	DiscoveryPDU pdu = new DiscoveryPDU();
	formatPdu(pdu, 
		  objectRequired, 
		  selectedHost,
		  returnPort, 
		  returnAddr, 
		  localHostName);
	
	// ------------------------
	// Create a thread to get responses
	// ------------------------
	if (logger.finerOn()) {
	    logger.finer("getObject" , "Create a thread to get responses") ;
	}
	ReceivedMsgObj rcvMsg    = new ReceivedMsgObj(socket,this) ;
	Thread         rcvThread = new Thread (rcvMsg) ;

	// ------------------------
	// Now, we can send the msg
	// ------------------------
	if (logger.finerOn()) {
	    logger.finer("getObject" , "Send the multicast query") ;
	}
	sendMsg((DiscoveryMsg)pdu, multicastGroup , multicastPort) ;

	// ------------------------
	// start thread to get responses
	// ------------------------
	rcvThread.start() ;

	// ------------------------
	// Sleep before stopping receiving msg
	// ------------------------
	if (logger.finerOn()) {
	    logger.finer("getObject" , "Sleep before receiving") ;
	}
	try {
	    Thread.sleep(getTimeOut()) ;
	} catch (InterruptedException e) {
	    if (logger.finestOn()) {
		logger.finest("getObject" , e) ;
	    }
	}

	// ------------------------
	// Stop receiving msg
	// ------------------------
	if (logger.finerOn()) {
	    logger.finer("getObject" , "Stop receiving") ;
	}
	rcvMsg.stopRequested =  true ;

	synchronized(rcvMsg.interrupted) {
	    if (!rcvMsg.interrupted.booleanValue()) {	
		rcvThread.interrupt() ;
	    }
	}

        // ------------------------
        // return part
        // ------------------------
	return returnValue ;

    }

    // ----------------------------------------------------------
    // formatPdu
    // ----------------------------------------------------------
    private void formatPdu (DiscoveryPDU pdu , 
			    boolean objectRequired, 
			    String selectedHost, 
			    int responsePort,
			    InetAddress returnAddr,
			    String localHostName) {
	// ------------------------
	// Set timeStamp
	// ------------------------
	String stamp= new Long((new Date()).getTime()).toString();
	
        timeStamp = localHostName + ":" + stamp ;
	pdu.setTimeStamp(localHostName + ":" + stamp ) ;
	
	// ------------------------
	// Set emittedGroup
	// ------------------------
	pdu.setEmittedGroup(multicastGroup) ;

	// ------------------------
	// Set point to point reply
	// ------------------------
	if (getPointToPointResponse() ) {
	    if (logger.finerOn()) {
		logger.finer("formatPdu" , "set return addr to " + 
			     returnAddr + ":" + responsePort ) ;
	    }
	    pdu.setReturnaddr(returnAddr, responsePort);
	}
	
	// ------------------------
	// Set specific host
	// ------------------------
	pdu.setHost(selectedHost) ;
	
	// ------------------------
	// Set timeOut
	// ------------------------
	pdu.setTimeOut(getTimeOut());
	
	// ------------------------
	// Set ttl
	// ------------------------
	pdu.setTimeToLive(getTimeToLive());
	
	// ------------------------
	// Set object required 
	// ------------------------
	pdu.setObjectRequired(objectRequired);
	
    }

    // ----------------------------------------------------------
    // Inner class
    // ----------------------------------------------------------
    class ReceivedMsgObj implements Runnable {

	// ------------------------
	// Variables
	// ------------------------
	public  Vector          result    	= new Vector () ;
	public  volatile boolean stopRequested  = false ;
        public  Boolean	         interrupted    = new Boolean(false) ;


	private ActualDiscovery discovery                 ;
	private DatagramSocket  socket                    ;

	// ------------------------
	// Constructor
	// ------------------------
	ReceivedMsgObj (DatagramSocket socket, ActualDiscovery discovery) {
	    this.socket    = socket ;
	    this.discovery = discovery ;
	}

	// ------------------------
	// run
	// ------------------------
	public void run() {

	    // ------------------------
	    // Create Vector
	    // ------------------------

	    ResponsePDU pdu ;
	    DiscoveryResponse translatedPDU ;
	    int	nbRsp = 0 ;

	    try {
		while (! stopRequested ) {
		    try {
			// ------------------------
			// Retrieve msg
			// ------------------------
			DiscoveryMsg revMsg = (DiscoveryMsg)receiveMsg(socket);
			if (  revMsg == null ) {
			    if (logger.finerOn()) {
				logger.finer("receivedMsg" , 
					     "Null msg - continue - " );
			    }
			    continue ;
			}
		    
			// ------------------------
			// Cast stuff
			// ------------------------
			pdu = (ResponsePDU) revMsg ;
		    
			// ------------------------
			// Check Time Stamp
			// ------------------------
			if(discovery.timeStamp.compareTo(pdu.getTimeStamp()) 
			   != 0 ) {
			    if (logger.finerOn()) {
				logger.finer("receivedMsg" , 
					     "Msg is not for us : local "+
					     "timeStamp = " + 
					     discovery.timeStamp + 
					     " ; received timeStamp =  " +  
					     pdu.getTimeStamp());
			    }
			    continue ;
			}
		    
			// ------------------------
			// Translate PDU
			// ------------------------
			translatedPDU = TranslatePDU(pdu) ;
		    
			// ------------------------
			// Add pdu in result
			// ------------------------
			discovery.returnValue.
			    addElement((Object)translatedPDU);
			nbRsp++ ;
		    } catch (InterruptedIOException e ) {
			stopRequested = true ;
			if (logger.finerOn()) {
			    logger.finer("receivedMsg", 
					 "timeOut expiration");
			    logger.finer("receivedMsg", "Got " + nbRsp + 
					 " responses");
			}
		    
		    } catch (InvalidClassException e ) {
			if (logger.finestOn()) {
			    logger.finest("receivedMsg", 
					  "Not a response msg - continue ");
			    logger.finest("receivedMsg", e) ;
			}
		    } catch (ClassCastException e ) {
			if (logger.finestOn()) {
			    logger.finest("receivedMsg", 
					  "Not a response msg - continue ");
			    logger.finest("receivedMsg", e) ;
			}
		    } catch (ClassNotFoundException e ) {
			if (logger.finestOn()) {
			    logger.finest("receivedMsg", 
					  "Not a response msg - continue ");
			    logger.finest("receivedMsg", e) ;
			}
		    } catch (IOException e ) {
			if (logger.finestOn()) {
			    logger.finest("receivedMsg", 
					  "Not a response msg - continue ");
			    logger.finest("receivedMsg", e);
			}
		    }
		}
	    }  finally {
		synchronized(interrupted) {
		    interrupted = new Boolean(true);
		    Thread.currentThread().interrupted();
		}

		// ------------------------
		// Close PointToPoint socket
		// ------------------------
		if (logger.finerOn()) {
		    logger.finer("receivedMsg" , "timeOut expiration") ;
		    logger.finer("receivedMsg" , "Got " + nbRsp + 
				 " responses");
		}
	    
		if ( discovery.getPointToPointResponse() ) {
		    if (logger.finerOn()) {
			logger.finer("receivedMsg" ,
				     "Close datagram socket");
		    }
		    socket.close() ; 
		}
	    }
	    
	    // ------------------------
	    // Return part
	    // ------------------------
	    return ;
	}

	// ----------------------------------------------------------
	// private TranslatePDU
	// ----------------------------------------------------------
	private DiscoveryResponse TranslatePDU (ResponsePDU pdu )
	{
	    // ------------------------
	    // New object
	    // ------------------------
	    DiscoveryResponse result = new DiscoveryResponse() ;

	    // ------------------------
	    // Set host
	    // ------------------------
	    result.host =  pdu.getHost() ;

	    // ------------------------
	    // Set mbeanServer info
	    // ------------------------
	    result.mbeanServerId         =  pdu.getMbeanServerId() ;
	    result.specificationName     =  pdu.getSpecificationName () ;
	    result.specificationVendor   =  pdu.getSpecificationVendor () ;
	    result.specificationVersion  =  pdu.getSpecificationVersion () ;
	    result.implementationName    =  pdu.getImplementationName () ;
	    result.implementationVendor  =  pdu.getImplementationVendor () ;
	    result.implementationVersion =  pdu.getImplementationVersion () ;
	    result.userData              =  pdu.getUserData () ;

	    // ------------------------
	    // Set adaptor List
	    // ------------------------
	    result.objectList = pdu.getObjectList () ; 

	    // jmx-remote
	    if (pdu.serverAddresses != null) {
		result.serverAddresses = pdu.serverAddresses;
	    } else {
		result.serverAddresses = new JMXServiceURL[0];
	    }

	    // ------------------------
	    // Return part
	    // ------------------------
	    return result ;
	}
    }

    private final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"ActualDiscovery");


}
