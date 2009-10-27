/*
 * @(#)file      ActualResponder.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.30
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
import java.lang.*;
import java.net.*;
import java.io.*;

// jmx
import javax.management.remote.JMXConnectorServerMBean;
import javax.management.remote.JMXServiceURL;

// ------------------------
// jdmk import
// ------------------------
import javax.management.*;
import com.sun.jdmk.* ; 

import com.sun.jdmk.comm.RmiConnectorAddress ; 
import com.sun.jdmk.comm.HttpConnectorAddress ; 
import com.sun.jdmk.comm.HttpsConnectorAddress ; 
import com.sun.jdmk.comm.AuthInfo ;
import com.sun.jdmk.internal.ClassLogger;


class ActualResponder extends DiscoveryCommon {

    private DiscoveryResponder discResponder = null;

    // ----------------------------------------------------------
    // Constructor
    // ----------------------------------------------------------
    public ActualResponder (String multicastGroup, int multicastPort ,int ttl , MBeanServer cmf, String spy, DiscoveryResponder discResponder)
	throws IOException {

        // ------------------------
        // Set multicastSocket parameters
        // ------------------------
        super(multicastGroup,multicastPort) ;

	this.discResponder = discResponder;

	if (logger.finerOn()) {
	    logger.finer("constructor " , "group = " + multicastGroup  + ", " + "port  = " + multicastPort) ;
	}

        // ------------------------
        // Set Logging stuff
        // ------------------------
	localClassName = "com.sun.jdmk.discovery.ActualResponder" ;

        // ------------------------
        // Set ttl
        // ------------------------
        this.ttl = ttl ;

        // ------------------------
        // Set spy stuff
        // ------------------------
	if ( spy != null ) {
	    if (logger.finerOn()) {
		logger.finer("constructor " , "constructor spy = '" + spy + "'") ;
	    }
	    if (spy.compareTo("act_as_a_spy") == 0 ) {
		this.spy = true ;
	    }
	}

        // ------------------------
        // Set Framework
        // ------------------------
        this.cmf = cmf ;

        // ------------------------
        // Set MbeanServer Id
        // ------------------------
	try {
	    ObjectName delegateName = new ObjectName (ServiceName.DELEGATE) ;
	    mbeanServerName       = (String) cmf.getAttribute(delegateName, "MBeanServerId") ;
	    SpecificationName     = (String) cmf.getAttribute(delegateName, "SpecificationName") ;
	    SpecificationVendor   = (String) cmf.getAttribute(delegateName, "SpecificationVendor") ;
	    SpecificationVersion  = (String) cmf.getAttribute(delegateName, "SpecificationVersion") ;
	    ImplementationName    = (String) cmf.getAttribute(delegateName, "ImplementationName") ;
	    ImplementationVendor  = (String) cmf.getAttribute(delegateName, "ImplementationVendor") ;
	    ImplementationVersion = (String) cmf.getAttribute(delegateName, "ImplementationVersion") ;

	} catch (Exception e) {
	    if (logger.finestOn()) {
		logger.finest("constructor " , e) ;
	    }
	}

    }

    public void noEvent() {
	sendEvent = false ;
    }

    public void setLocalHost(String local) {
        if (local != null) {
            try {
                localHost = InetAddress.getByName(local);
                localHostName = local;
            } catch (UnknownHostException ue) {}
        }
    }


    // ----------------------------------------------------------
    // run
    // ----------------------------------------------------------
    public void run() {

	DiscoveryPDU		pdu ;

	// ------------------------
	// Send an event to say that we are here
	// ------------------------
	if ( ! spy ) {
	    if ( sendEvent ) sendEvent(DiscoveryResponder.ONLINE);
	}
	// ------------------------
	// Infinite loop
	// ------------------------
	try {
	    while (! stopRequested ) {
		try {
		    // ------------------------
		    // Receive msg
		    // ------------------------
		    if (logger.finerOn()) {
			logger.finer("run " , " -------------------- Start Waiting -------------------- ") ;
		    }
		    if ( spy ) {
			DiscoveryMsg revMsg = (DiscoveryMsg) receiveMsg((DatagramSocket)this) ;
			if (logger.finerOn()) {
			    logger.finer("run " , "(SPY) receive : " + revMsg.printState() ) ;
			}
			continue ;
		    }
		
		    pdu = (DiscoveryPDU) receiveMsg((DatagramSocket)this) ;
		
		    // ------------------------
		    // Process msg
		    // ------------------------
		    processMsg(pdu) ;
		
		} catch (InterruptedIOException e) {
		    if (logger.finerOn()) {
			logger.finer("run " , "Stop execution " ) ;
		    }
		    stopRequested = true ;
		} catch (IOException e) {
		    if (logger.finerOn()) {
			logger.finer("run " , "Not a discovering msg - continue - ");
		    }
		    if ( spy ) {
			if (logger.finestOn()) {
			    logger.finest("run " , e) ;
			}
		    } 
		} catch (ClassCastException e) {
		    if (logger.finerOn()) {
			logger.finer("run " , "Not a discovering msg - continue - ");
		    }
		    if ( spy ) { 
			if (logger.finestOn()) {
			    logger.finest("run " , e) ;
			}
		    }
		} catch (ClassNotFoundException e) {
		    if (logger.finerOn()) {
			logger.finer("run " , "Not a discovering msg - continue - ");
		    }
		    if ( spy ) {
			if (logger.finestOn()) {
			    logger.finest("run " , e) ;
			}
		    }
		}
	    }
 
	} finally {
	    synchronized(interrupted) {
		interrupted = new Boolean(true);
		Thread.currentThread().interrupted();
	    }
	    
	

	    // ------------------------
	    // Send an event to say that we are away
	    // ------------------------
	    if ( sendEvent )  {
		sendEvent(DiscoveryResponder.OFFLINE);
		try { Thread.sleep(100) ; } catch (Exception e) {}
		try { Thread.sleep(100) ; } catch (Exception e) {}
		try { Thread.sleep(100) ; } catch (Exception e) {}
		try { Thread.sleep(100) ; } catch (Exception e) {}
		try { Thread.sleep(100) ; } catch (Exception e) {}
	    }
	    
	    // ------------------------
	    // Leave group
	    // ------------------------
	    if (logger.finerOn()) {
		logger.finer("run " , "Call leave group");
	    }
	    try {
		disconnectFromGroup() ;
	    } catch (IOException e) { 
		// ------------------------
		// Keep exception for us
		// ------------------------
		if (logger.finestOn()) {
		    logger.finest("start " , e) ;
		}
	    } catch (Exception e) { 
		// ------------------------
		// Keep exception for us
		// ------------------------
		if (logger.finestOn()) {
		    logger.finest("start " , e) ;
		}
	    }
	    
	    // ------------------------
	    // close socket
	    // ------------------------
	    close() ;
	}
    }

    // ----------------------------------------------------------
    // Private sendEvent
    // ----------------------------------------------------------
    private void sendEvent (int state) {


	// ------------------------
	// Create a new ResponsePDU
	// ------------------------
	if (logger.finerOn()) {
	    logger.finer("sendEvent " , "Format Event PDU") ;
	}
	ResponsePDU pdu = new ResponsePDU(	mbeanServerName ,
						SpecificationName ,
						SpecificationVendor ,
						SpecificationVersion ,
						ImplementationName ,
						ImplementationVendor ,
						ImplementationVersion,
						discResponder.getUserData());
	formatPdu ( pdu , null , true) ;
	pdu.setAgentState(state) ;
	try {
	    sendMsg((DiscoveryMsg) pdu, multicastGroup , multicastPort ) ;

	} catch (IOException e) {
	    if (logger.finerOn()) {
		logger.finer("sendEvent " , "Unable to send event !") ;
		logger.finer("sendEvent " , e) ;
	    }
	}
	discResponder.changeState(state);
    }

    // ----------------------------------------------------------
    // Private processMsg
    // ----------------------------------------------------------
    private void processMsg (DiscoveryPDU rcvPDU)
	throws IOException
    {

	if ( rcvPDU ==  null ) {
	    if (logger.finerOn()) {
		logger.finer("processMsg " , "received null pdu") ;
	    }
	    return ;
	} else {
	    if (logger.finerOn()) {
		logger.finer("processMsg " , "received pdu '" + rcvPDU.printState() + "'") ;
	    }
	}

	// ------------------------
	// If host is given, check if msg is for local host
	// ------------------------
	if ( rcvPDU.getHost() != null ) {
	    // ------------------------
	    // Compare host name
	    // ------------------------
	    try {
		// 		if (!localHost.equals(InetAddress.getByName(rcvPDU.getHost()))) {
		if (!com.sun.jdmk.internal.Useful.isLocalHost(rcvPDU.getHost())) {
		    if (logger.finerOn()) {
			// 			logger.finer("processMsg","local host ('" + localHostName + "') is not '" + rcvPDU.getHost() + "' - No answer is required ");
			logger.finer("processMsg", "Got a message looking for MBeanServers on other machine: "+rcvPDU.getHost()+", No answer is required ");
		    }
		    return ;
		} else {
		    if (logger.finerOn()) {
			logger.finer("processMsg"," Request is for this host ('" + localHostName + "')") ;
		    }
		}
	    } catch (Exception ee) {
		if (logger.finerOn()) {
		    logger.finer("processMsg","Cannot decide whether the requested host "+localHostName + "is same as the local host. "+ee);
		}
	    }
		
	} else {
	    if (logger.finerOn()) {
		logger.finer("processMsg"," Request is for all host ") ;
	    }
	}

	// ------------------------
	// Create a new ResponsePDU
	// ------------------------
	if (logger.finerOn())
	    {
		logger.finer("processMsg " , "Format response PDU") ;
	    }
	ResponsePDU pdu = new ResponsePDU(	mbeanServerName,
						SpecificationName ,
						SpecificationVendor ,
						SpecificationVersion ,
						ImplementationName ,
						ImplementationVendor ,
						ImplementationVersion,
						discResponder.getUserData());
	formatPdu ( pdu , rcvPDU , false) ;

	// ------------------------
	// If PointToPoint response is required, Set return Addr
	// ------------------------
	InetAddress    receiverInetAddr;
	int            receiverPort ;

	if (logger.finerOn()) {
	    logger.finer("processMsg " , "Check point to point response") ;
	}
	if ( rcvPDU.getReturnAddr() == true ) {
	    if (logger.finerOn()) {
		logger.finer("processMsg " , "Point to point response using  Port " + rcvPDU.getReturnPort() + " on Host " + rcvPDU.getReturnHost()) ;
	    }
	    receiverInetAddr = rcvPDU.getReturnHost() ;
	    receiverPort     = rcvPDU.getReturnPort() ;
		
	} else {
	    if (logger.finerOn()) {
		logger.finer("processMsg" , "Use multicast socket for response" ) ;
	    }
	    receiverInetAddr = multicastGroup ;
	    receiverPort     = multicastPort ;
	}

	// ------------------------
	// Send the msg
	// ------------------------
	try {
	    setTimeToLive(pdu.getTimeToLive()) ;
	    sendMsg((DiscoveryMsg) pdu, receiverInetAddr , receiverPort ) ;
	} catch ( IOException e) {
	    if (logger.finerOn()) {
		logger.finer("processMsg " , "Unable to send response !") ;
	    }
	    throw e ;
	}
	

    }

    // ----------------------------------------------------------
    // Private formatPdu
    // ----------------------------------------------------------
    private void formatPdu (ResponsePDU pdu , DiscoveryPDU rcvPDU, boolean evt)
    {
	// ------------------------
	// Set time stamp 
	// ------------------------
	if (evt == false ) {
	    pdu.setTimeStamp(rcvPDU.getTimeStamp());
	} else {
	    pdu.setTimeStamp("") ;
	}

	// ------------------------
	// Set event flag 
	// ------------------------
	pdu.setEvent(evt) ;

	// ------------------------
	// Set ttl 
	// ------------------------
	if (evt == false ) {
	    pdu.setTimeToLive(rcvPDU.getTimeToLive());
	} else {
	    pdu.setTimeToLive(ttl) ;
	}

        // ------------------------
        // Set emittedGroup
        // ------------------------
	if (evt == false ) {
	    pdu.setEmittedGroup(rcvPDU.getEmittedGroup()) ;
	} else {
	    pdu.setEmittedGroup(multicastGroup) ;
	}

	// ------------------------
	// Set local Host
	// ------------------------
	if (evt == false ) {
	    if ( rcvPDU.getHost() != null ) {
		pdu.setHost(rcvPDU.getHost()) ;
	    } else {
		pdu.setHost(localHostName) ;
	    }
	} else {
	    pdu.setHost(localHostName) ;
	}

	// ------------------------
	// Should we add object list?
	// yes if is an event of is obj list required
	// ------------------------
	boolean setAdp = evt ;
	QueryExp query = null ; 
	if ( rcvPDU != null ) {
	    setAdp  |= rcvPDU.getObjectRequired() ;
	}

	if ( setAdp == true ) {
	    java.util.Set result  ;
	    ObjectName name = null ;
	    try {
		name = new ObjectName ("*:*" ) ;
	    } catch (MalformedObjectNameException e) {
		if (logger.finestOn()) {
		    logger.finest("formatPdu " , e) ;
		}
		return ;
	    }

	    // ------------------------
	    // Looking for RMI connector server
	    // ------------------------
	    {
		String val = "com.sun.jdmk.comm.RmiConnectorServer" ;
		try {
		    result = cmf.queryNames(name, Query.match(Query.classattr(),Query.value(val)));

		    // ------------------------
		    // Print object list
		    // ------------------------
		    if (logger.finerOn()) {
			for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			    ObjectName o = (ObjectName) i.next();
			    logger.finer("formatPdu " , "Add\t'" + o + "' in response msg")  ;
			}
		    }
		    // ------------------------
		    // Add object list in response
		    // ------------------------
		    for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			ObjectName objectName = (ObjectName) i.next();
			try {
			    String host = (String) cmf.getAttribute(objectName,"Host") ;
			    int port = (int) ((Integer)cmf.getAttribute(objectName,"Port")).intValue() ;
			    String serviceName = (String) cmf.getAttribute(objectName,"ServiceName") ;
			    RmiConnectorAddress addr = new RmiConnectorAddress(host,port,serviceName) ;
			    pdu.addObjectList(objectName,addr);
			} catch (Exception e) {
			    if (logger.finestOn()) {
				logger.finest("formatPdu " , e) ;
			    }
			}
		    }
		} catch (Exception e) {
		    if (logger.finerOn()) {
			logger.finer("formatPdu " , "Unable to get Mbean for " + val) ;
			logger.finer("formatPdu " , e) ;
		    }
		}
	    }

	    // ------------------------
	    // Looking for HTTP connector server
	    // ------------------------
	    {
		String val = "com.sun.jdmk.comm.HttpConnectorServer" ;
		try
		    {
			result = cmf.queryNames(name, Query.match(Query.classattr(),Query.value(val)));

			// ------------------------
			// Print object list
			// ------------------------
			if (logger.finerOn()) {
			    for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
				ObjectName o = (ObjectName) i.next();
				logger.finer("formatPdu " , "Add\t'" + o + "' in response msg")  ;
			    }
			}
			// ------------------------
			// Add object list in response
			// ------------------------
			for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			    ObjectName objectName = (ObjectName) i.next();
			    try {
				String host = (String) cmf.getAttribute(objectName,"Host") ;
				int port = (int) ((Integer)cmf.getAttribute(objectName,"Port")).intValue() ;
				HttpConnectorAddress addr = new HttpConnectorAddress(host,port) ;
				pdu.addObjectList(objectName,addr);
			    } catch (Exception e) {
				if (logger.finestOn()) {
				    logger.finest("formatPdu " , e) ;
				}
			    }
			}
		    } catch (Exception e) {
			if (logger.finerOn()) {
			    logger.finer("formatPdu " , "Unable to get Mbean for " + val) ;
			    logger.finer("formatPdu " , e) ;
			}
		    }

	    }



	    // ------------------------
	    // Looking for HTTPS connector server
	    // ------------------------
	    {
		String val = "com.sun.jdmk.comm.HttpsConnectorServer" ;
		try
		    {
			result = cmf.queryNames(name, Query.match(Query.classattr(),Query.value(val)));

			// ------------------------
			// Print object list
			// ------------------------
			if (logger.finerOn()) {
			    for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
				ObjectName o = (ObjectName) i.next();
				logger.finer("formatPdu " , "Add\t'" + o + "' in response msg")  ;
			    }
			}
			// ------------------------
			// Add object list in response
			// ------------------------
			for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			    ObjectName objectName = (ObjectName) i.next();
			    try {
				String host = (String) cmf.getAttribute(objectName,"Host") ;
				int port = (int) ((Integer)cmf.getAttribute(objectName,"Port")).intValue() ;
				HttpsConnectorAddress addr = new HttpsConnectorAddress(host,port) ;
				pdu.addObjectList(objectName,addr);
			    } catch (Exception e) {
				if (logger.finestOn()) {
				    logger.finest("formatPdu " , e) ;
				}
			    }
			}
		    } catch (Exception e) {
			if (logger.finerOn()) {
			    logger.finer("formatPdu " , "Unable to get Mbean for " + val) ;
			    logger.finer("formatPdu " , e) ;
			}
		    }

	    }


	    // ------------------------
	    // Looking for HTML adaptor server
	    // ------------------------
	    {
		String val = "com.sun.jdmk.comm.HtmlAdaptorServer" ;
		try {
		    result = cmf.queryNames(name, Query.match(Query.classattr(),Query.value(val)));

		    // ------------------------
		    // Print object list
		    // ------------------------
		    if (logger.finerOn()) {
			for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			    ObjectName o = (ObjectName) i.next();
			    logger.finer("formatPdu " , "Add\t'" + o + "' in response msg")  ;
			}
		    }
		    // ------------------------
		    // Add object list in response
		    // ------------------------
		    for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			ObjectName objectName = (ObjectName) i.next();
			try {
					
			    pdu.addObjectList(objectName,null);
			} catch (Exception e) {
			    if (logger.finestOn()) {
				logger.finest("formatPdu " , e) ;
			    }
			}
		    }
		} catch (Exception e) {
		    if (logger.finerOn()) {
			logger.finer("formatPdu " , "Unable to get Mbean for " + val) ;
			logger.finer("formatPdu " , e) ;
		    }
		}

	    }

	    // ------------------------
	    // Looking for SNMP adaptor server
	    // ------------------------
	    {
		String val = "com.sun.jdmk.comm.SnmpAdaptorServer" ;
		try {
		    result = cmf.queryNames(name, Query.match(Query.classattr(),Query.value(val)));

		    // ------------------------
		    // Print object list
		    // ------------------------
		    if (logger.finerOn()) {
			for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			    ObjectName o = (ObjectName) i.next();
			    logger.finer("formatPdu " , "Add\t'" + o + "' in response msg")  ;
			}
		    }
		    // ------------------------
		    // Add object list in response
		    // ------------------------
		    for (java.util.Iterator i  = result.iterator(); i.hasNext(); ) {
			ObjectName objectName = (ObjectName) i.next();
			try {
			    pdu.addObjectList(objectName,null);
			} catch (Exception e) {
			    if (logger.finestOn()) {
				logger.finest("formatPdu " , e) ;
			    }
			}
		    }
		} catch (Exception e) {
		    if (logger.finerOn()) {
			logger.finer("formatPdu " , "Unable to get Mbean for " + val) ;
			logger.finer("formatPdu " , e) ;
		    }
		}

		}

		// --------------------------------
		// Looking for jmx-remote servers
		// --------------------------------
		final String s = "javax.management.remote.JMXConnectorServerMBean";

		Set ns = cmf.queryNames(null, null);
		ArrayList list = new ArrayList();
		for (Iterator iter=ns.iterator(); iter.hasNext();) {
		    ObjectName on = (ObjectName)iter.next();
		    try {
			if (cmf.isInstanceOf(on, s)) {
			    JMXServiceURL url = (JMXServiceURL)cmf.getAttribute(on, "Address");
			    list.add(url);

			    if (logger.finerOn()) {
				logger.finer("formatPdu " , "Add\t'" + url + "' in response msg")  ;
			    }
			}
		    } catch (InstanceNotFoundException infe) {
			// OK.
			// possible that the mbean has been unregistered.
		    } catch (MBeanException me) {
			if (logger.traceOn()) {
			    logger.trace("formatPdu",
				   "Failed to get address from the jmx remote server: "+on, me);
			}
		    } catch (Exception e) {
			if (logger.traceOn()) {
			    logger.trace("formatPdu",
				   "Failed to get address from the jmx remote server: "+on, e);
			}
		    }
		}
		    
		pdu.serverAddresses = (JMXServiceURL[])list.toArray(new JMXServiceURL[list.size()]);

	}

	// ------------------------
	// return part
	// ------------------------
	return ;
    }


    // ----------------------------------------------------------
    // public variables
    // ----------------------------------------------------------
    public volatile boolean	stopRequested		= false ;
    public  Boolean	interrupted		= new Boolean(false) ;

    private final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"ActualResponder");


    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    private MBeanServer            cmf                     = null ;
    private String                 mbeanServerName         = null ;
    private String                 ImplementationName      = null ;
    private String                 ImplementationVendor    = null ;
    private String                 ImplementationVersion   = null ;
    private String                 SpecificationName       = null ;
    private String                 SpecificationVendor     = null ;
    private String                 SpecificationVersion    = null ;
    private int	               ttl                     = 1 ;
    private boolean	               sendEvent               = true ;
    private boolean	               spy                     = false ;
}
