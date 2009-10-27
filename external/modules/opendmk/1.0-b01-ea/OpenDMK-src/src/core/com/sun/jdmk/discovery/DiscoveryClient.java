/*
 * @(#)file      DiscoveryClient.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.44
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
import java.io.*;
import java.net.*;


// ------------------------
// jdmk import
// ------------------------
import javax.management.*;
import com.sun.jdmk.* ;
import com.sun.jdmk.comm.CommunicationException ;
import com.sun.jdmk.internal.ClassLogger;


/**
 * Provides methods to discover Java DMK agents.
 * A Java DMK agent can only discovered if it has a 
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} registered in its MBean server.
 * A discovery operation is executed in two steps:
 * <UL>
 *   <LI>a <CODE>DiscoveryClient</CODE> broadcasts a discovery request on a
 *       multicast group</LI>
 *   <LI>Registered {@link com.sun.jdmk.discovery.DiscoveryResponder} MBeans listening on the multicast group 
 *       catch the request. Each {@link com.sun.jdmk.discovery.DiscoveryResponder}
 *       sends a discovery response to the <CODE>DiscoveryClient</CODE></LI>
 * </UL>
 * <p>
 * A <CODE>DiscoveryClient</CODE> can only reach the 
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} objects that listen on the same multicast group and 
 * port. The default group is 224.224.224.224 and the default port is 9000. Other values 
 * can be used by configuring the <CODE>multicastGroup</CODE> and
 * <CODE>multicastPort</CODE> properties on the <CODE>DiscoveryClient</CODE> 
 * and {@link com.sun.jdmk.discovery.DiscoveryResponder} objects.
 * <p>
 * The scope of the discovery depends on the time-to-live used by
 * the <CODE>MulticastSocket</CODE>. By default, the time-to-live is 1. It 
 * can be changed by setting the property <CODE>timeToLive</CODE> on the 
 * <CODE>DiscoveryClient</CODE>.
 * <p>
 * After it has sent its discovery request, a <CODE>DiscoveryClient</CODE> 
 * waits a finite time for responses. The default is 1 second. 
 * This can be customized by setting the <CODE>timeOut</CODE> property on the 
 * <CODE>DiscoveryClient</CODE>.
 * <p>
 * An application triggers a discovery operation by invoking either the 
 * <CODE>findMBeanServers</CODE> method or the <CODE>findCommunicators</CODE> 
 * method on a <CODE>DiscoveryClient</CODE> object.
 * These methods represent the discovery result by a <CODE>Vector</CODE> of 
 * {@link com.sun.jdmk.discovery.DiscoveryResponse}. A {@link com.sun.jdmk.discovery.DiscoveryResponse} is included
 * for each discovered Java DMK agent. It provides the host name and the MBean server information of the
 * agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) and optionally
 * the list of communicator (Adaptor and connector) available in the agent.
 * <p>
 * A {@link com.sun.jdmk.discovery.DiscoveryResponder} can send back responses using two modes:
 * <UL>
 *   <LI> Unicast mode. A datagram socket is sent from the {@link com.sun.jdmk.discovery.DiscoveryResponder}
 *        to the <CODE>DiscoveryClient</CODE>. The response is NOT multicasted to the group.
 *        The default datagram socket port is 9001.
 *        To enable unicast mode, set the <CODE>pointToPointResponse</CODE> property to <CODE>true</CODE>.
 *        (Unicast mode is enabled by default.)
 *   </LI>
 *   <LI> Multicast mode. A multicast socket is used between the {@link com.sun.jdmk.discovery.DiscoveryResponder}
 *        and the <CODE>DiscoveryClient</CODE>. The response is multicasted to the group. This 
 *        behavior allows {@link com.sun.jdmk.discovery.DiscoveryMonitor} objects to be aware of changes.
 *        To enable multicast mode, set the <CODE>pointToPointResponse</CODE> property to <CODE>false</CODE>.
 *   </LI>
 * </UL>
 * <p>
 * It is possible to instantiate multiple <CODE>DiscoveryClient</CODE> objects with 
 * different groups and ports for multicast responses and datagram sockets for unicast responses.
 *
 */


public class DiscoveryClient implements java.io.Serializable, DiscoveryClientMBean, MBeanRegistration{
    private static final long serialVersionUID = 2030335665289155729L;
    
    // ----------------------------------------------------------
    // States of a DiscoveryResponder
    // ----------------------------------------------------------
    
    /** Marks the "state" property as running. */
    public static final int ONLINE = 0 ;
    /** Marks the "state" property as stopped. */
    public static final int OFFLINE = 1 ;
    
    // ----------------------------------------------------------
    // Constructor
    // ----------------------------------------------------------
    
    /**
     * Constructs a <CODE>DiscoveryClient</CODE>.
     * <P>
     * This constructor initializes multicast group and  a multicast port to 
     * the default values (224.224.224.224 and 9000).
     * No check is done on the default values. Check will be performed by the start method.
     */
    public DiscoveryClient () {
	// ------------------------
	// set Default multicastPort/multicastGroup
	// ------------------------
	this.multicastGroup = defaultMulticastGroup ;
	this.multicastPort  = defaultMulticastPort ;
    }
    
    /**
     * Constructs a <CODE>DiscoveryClient</CODE>.
     * <P>
     * This constructor initialize multicast group and  a multicast port.
     * No check is done on the parameter values. Check will be performed by the start method.
     *
     * @param multicastGroup The multicast group name.
     * @param multicastPort The multicast port number.
     */
    public DiscoveryClient (String multicastGroup, int multicastPort) {
	// ------------------------
	// set Default multicastPort/multicastGroup
	// ------------------------
	this.multicastGroup = multicastGroup ;
	this.multicastPort  = multicastPort ;
    }
    
    // NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
    /**
     * Constructs a <CODE>DiscoveryClient</CODE>.
     * <P>
     * This constructor initialize multicast group and  a multicast port.
     * No check is done on the parameter values. Check will be performed by the start method.
     *
     * @param multicastGroup The multicast group name.
     * @param multicastPort The multicast port number.
     * @param inf the interface used by a MulticastSocket.
     *
     * @since Java DMK 5.0
     */
    public DiscoveryClient (String multicastGroup, int multicastPort, InetAddress inf) {
	// ------------------------
	// set Default multicastPort/multicastGroup
	// ------------------------
	this.multicastGroup = multicastGroup ;
	this.multicastPort  = multicastPort ;
	
	usrInet = inf;
	
	if (logger.finerOn()) {
	    logger.finer("constructor " , "set interface to " + inf ) ;
	}
    }
    // end of NPCTE fix for bugId 4499338
    
    // ----------------------------------------------------------
    // Mbean Server interaction
    // ----------------------------------------------------------
    
    /**
     * Allows the MBean to perform any operations it needs before being registered
     * in the MBean server. If the name of the MBean is not specified, the
     * MBean can provide a name for its registration. If any exception is
     * raised, the MBean will not be registered in the MBean server.
     *   
     * @param server The MBean server in which the MBean will be registered.
     * @param name The object name of the MBean.
     *   
     * @return  The name of the MBean registered.
     *   
     * @exception java.lang.Exception This exception should be caught by the MBean server and re-thrown
     * as an {@link javax.management.MBeanRegistrationException}.
     */
    public ObjectName preRegister (MBeanServer server, ObjectName name) throws java.lang.Exception {
	if (logger.finerOn()) {
	    logger.finer("preRegister " , "object name   = " + name ) ;
	}
	
	// ----------------
	// return part
	// ----------------
	return name ;
    }
    
    /**
     * Allows the MBean to perform any operations needed after having been
     * registered in the MBean server or after the registration has failed.
     *
     * @param registrationDone Indicates whether or not the MBean has been successfully registered in
     * the MBean server. The value false means that the registration phase has failed.
     */
    public void postRegister (Boolean registrationDone) { 
	if (registrationDone == Boolean.FALSE ) {
	    return ;
	}
    }
    
    /**
     * Allows the MBean to perform any operations it needs before being unregistered
     * by the MBean server.
     *
     * @exception java.langException  This exception should be caught by the MBean server and re-thrown
     * as an {@link javax.management.MBeanRegistrationException}.
     */
    public void preDeregister() throws java.lang.Exception {
	// ------------------------
	// Stop receiving multicast msg
	// ------------------------
	if (logger.finerOn()) {
	    logger.finer("preDeregister " , "stopping ") ;
	}
        // ------------------------
        // Call stop
        // ------------------------
        if ( state == ONLINE) {
	    stop() ;
	}
	
    }
    
    /**
     * Allows the MBean to perform any operations needed after having been
     * unregistered in the MBean server.
     */
    public void postDeregister() {
	
	// ------------------------
	// free 'remained' allocated resource
	// ------------------------
	System.runFinalization() ;
    }
    
    
    
    // ----------------------------------------------------------
    // Start/stop implementation
    // ----------------------------------------------------------
    /**
     * Create a multicast socket and join the multicast group.
     * This method creates a multicast socket that is used to broadcast
     * The <CODE>DiscoveryClient</CODE> will then join the multicast group.
     *
     * @exception IOException The creation of the Multicast socket failed.
     */
    
    public void start() throws IOException {
	
	if (state == OFFLINE) {
	    // ------------------------
	    // Create a new multicast socket
	    // ------------------------
	    try {
		if (logger.finerOn()) {
		    logger.finer("start " , "Create a new multicast socket");
		    logger.finer("start " , "Set group to '" + multicastGroup + "'" );
		    logger.finer("start " , "Set port  to '" + multicastPort + "'" );
		}
		discovery = new ActualDiscovery(multicastGroup, multicastPort) ;
		
		// NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
		if (usrInet != null) {
		    discovery.setInterface(usrInet);
		    
		    if (logger.finerOn()) {
			logger.finer("start " , "Set to the interface "+usrInet);
		    }
		}
		// end of NPCTE fix for bugId 4499338
	    } catch (SocketException e) {
		if (logger.finerOn()) {
		    logger.finer("start","Can't start discoveryClient: Unable to create multicast socket " + multicastGroup + ":" + multicastPort) ;
		    logger.finer("start",e) ;
		}
		throw new IOException(e.getMessage()) ;
	    } catch (IOException e) {
		if (logger.finerOn()) {
		    logger.finer("start " , "Can't start discoveryClient: Unable to create multicast socket " + multicastGroup + ":" + multicastPort) ;
		    logger.finer("start" , e) ;
		}
		throw e ;
	    }
	    
	    // ----------------
	    // Create a new obj to send multicast msg ;
	    // ----------------
	    try {
		discovery.connectToGroup() ;
		discovery.setTimeOut(timeOut) ;
		discovery.setTimeToLive(ttl) ;
		discovery.setPointToPointResponse(pointToPointResponse) ;
	    } catch (SocketException e) {
		if (logger.finerOn()) {
		    logger.finer("start","Can't start discoveryClient: Unable to create multicast socket " + multicastGroup + ":" + multicastPort) ;
		    logger.finer("start",e) ;
		}
		throw new IOException(e.getMessage()) ;
	    } catch (IOException e) {
		if (logger.finerOn()) {
		    logger.finer("start","Can't start discoveryClient: Unable to create multicast socket " + multicastGroup + ":" + multicastPort) ;
		    logger.finer("start",e) ;
		}
		throw e ;
	    }
	    
	    // ------------------------
	    // Update state
	    // ------------------------
	    state = ONLINE ;
        } else {
	    if (logger.finerOn()) {
		logger.finer("start " , "Client is not OFFLINE") ;
	    }
        }
	
    }
    
    /**
     * Leaves the multicast group.
     * The <CODE>DiscoveryClient</CODE> leaves its multicast group,
     * and the multicast socket is released.
     *
     */
    public void stop() {
	
	// ----------------
	//  Check that the factory is not already connected
	// ----------------
	if (state == ONLINE) {
	    
	    // ------------------------
	    // Stop receiving multicast msg
	    // ------------------------
	    if (logger.finerOn()) {
		logger.finer("stop" , "Call leave group");
	    }
	    try {
		discovery.disconnectFromGroup() ;
		discovery =  null ;
	    } catch (IOException e) {
		if (logger.finestOn()) {
		    logger.finest("stop " , e) ;
		}
	    }
	    
	    // ------------------------
	    // free 'remained' allocated resource
	    // ------------------------
	    System.runFinalization() ;
	    
	    // ------------------------
	    // Update state
	    // ------------------------
	    state = OFFLINE ;
	} else { 
	    if (logger.finerOn()) {
		logger.finer("stop" , "Client is not ONLINE") ;
	    }
        } 
    }
    
    /**
     * Tests if the <CODE>DiscoveryClient</CODE> is active.
     * <CODE>True</CODE> is returned if the <CODE>DiscoveryClient</CODE> is started (<CODE>DiscoveryClient</CODE>
     * has join the multicast group).
     */
    public boolean isActive() {
	return (state == ONLINE);
    }
    
    // ----------------------------------------------------------
    // Interface implementation
    // ----------------------------------------------------------
    /**
     * Discovers all Java DMK agents.
     * <P>
     * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent.
     * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} contains the host name and the MBean server information of the
     * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}). 
     * The communicators list is not relevant: it is set to null.
     *
     * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
     *
     * @exception CommunicationException An error occurred during the discovery.
     *     
     */   
    public Vector findMBeanServers() throws CommunicationException {
	return findCommunicators ( false , null ) ;
    }
    
    /**
     * Discovers whether Java DMK agents with a {@link com.sun.jdmk.discovery.DiscoveryResponder}
     * registered in any MBean server is on a host.
     * <P>
     * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent on the specified host.
     * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} only contains the host name and the MBean server information of the 
     * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) of the specified host.
     * The communicators list is not relevant: it is set to null. 
     *
     * @param SelectedHost The host on which the discovery is to be performed.
     * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
     *
     * @exception CommunicationException An error occurred during the discovery.
     *
     */
    public Vector findMBeanServers(String SelectedHost) throws CommunicationException {
	return findCommunicators ( false , SelectedHost  ) ;
    }
    
    /** 
     * Discovers all Java DMK agents and associated communicators (adaptors or connectors).  
     * <P>  
     * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent. 
     * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} contains the host name, the MBean server information of the 
     * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) and
     * the communicators list.
     * 
     * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
     *
     * @exception CommunicationException An error occurred during the discovery. 
     *      
     */
    public Vector findCommunicators() throws CommunicationException {
	return findCommunicators ( true , null  ) ;
    }
    
    /**
     * Discovers all Java DMK agents and associated communicators (adaptors or connectors) present on an host.
     * <P>
     * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent.
     * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} contains the host name, the MBean server information of the
     * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) and
     * the communicators list.
     *
     * @param SelectedHost The host on which the discovery is to be performed.
     *
     * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
     *
     * @exception CommunicationException An error occurred during the discovery.
     *
     */
    public Vector findCommunicators(String SelectedHost) throws CommunicationException {
	return findCommunicators ( true , SelectedHost  ) ;
    }
    
    
    // ----------------------------------------------------------
    // Getters and Setters
    // ----------------------------------------------------------
    
    // --------------------------
    // multicast group
    // --------------------------
    /**
     * Returns the multicast group. 
     * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     */
    public String getMulticastGroup() {
	return multicastGroup;
    }
    
    /**
     * Sets the multicast group name.
     * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255
     * <P>
     * Only available if the state is OFFLINE
     *
     * @param multicastGroup The multicast group name.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryClient</CODE> was ONLINE or STARTING.
     */
    public void setMulticastGroup(String multicastGroup) throws java.lang.IllegalStateException {
        if ( state == OFFLINE ) {
	    this.multicastGroup = multicastGroup ;
        } else {
	    throw new java.lang.IllegalStateException() ;
	}
    }
    
    
    // --------------------------
    // multicast port
    // --------------------------
    
    /**
     * Returns the multicast port.
     * It can be any standard UDP port number.
     */
    public int getMulticastPort() {
	return multicastPort;
    }
    
    /**
     * Sets the multicast port.
     * It can be any standard UDP port number.
     * <P>
     * Only available if the state is OFFLINE
     *
     * @param multicastPort The multicast port.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryClient</CODE> was ONLINE or STARTING.
     */
    public void setMulticastPort(int multicastPort) throws java.lang.IllegalStateException {
        if ( state == OFFLINE ) {
	    this.multicastPort = multicastPort ;
        } else {
	    throw new java.lang.IllegalStateException() ;
	}
    }
    
    
    // ----------------------------------------------------------
    // time out
    // ----------------------------------------------------------
    /**
     * Sets the time during which the <CODE>DiscoveryClient</CODE> waits
     * for discovery responses.
     * <P>
     * This time is expressed in milliseconds. The default value is 1000.
     * If the specified argument is negative or zero, the <CODE>timeOut</CODE> is 
     * reset to the default value.
     * <p>
     * The methods <CODE>findMBeanServers</CODE> and <CODE>findCommunicators</CODE> block until this
     * time elapsed.
     * 
     * @param timeOut The <CODE>timeOut</CODE> in milliseconds.
     * 
     * @exception IOException If the socket rejected the specified value (See <CODE> java.net.MulticastSocket</CODE>).
     *            This exception can be thrown only if the state in ONLINE: the actual <CODE>java.net.MulticastSocket</CODE> setting
     *            is done when the <CODE>DiscoveryClient</CODE> is <CODE>ONLINE</CODE>.
     *
     */
    public void setTimeOut(int timeOut) throws IOException {
	if (timeOut <= 0 ) {
	    this.timeOut = defaultTimeOut ;
	} else {
	    this.timeOut = timeOut ;
	}
	if (state == ONLINE) {
	    discovery.setTimeOut(timeOut) ;
	}
    }
    
    /**
     * Returns the time to wait for discovery responses in milliseconds.
     *
     * @return The <CODE>timeOut</CODE> in milliseconds.
     *
     */
    public int getTimeOut() {
	return timeOut ;
    }
    
    // ----------------------------------------------------------
    // ttl
    // ----------------------------------------------------------
    /**
     * Sets the default time-to-live for this <CODE>DiscoveryClient</CODE>.
     * <P>
     * The time-to-live is the number of 'hops' that the multicast packet is 
     * forwarded on the network.
     *
     * @param ttl A number between 1 and 255.
     *
     * @exception java.lang.IllegalArgumentException The input ttl value is not in the 1 to 255 range.
     * @exception IOException If the socket rejected the specified value (See <CODE>java.net.MulticastSocket</CODE>).
     *            This exception can be thrown only if the state in ONLINE: the actual <CODE>java.net.MulticastSocket</CODE> setting
     *            is done when the <CODE>DiscoveryClient</CODE> is <CODE>ONLINE</CODE>.
     */
    public void setTimeToLive(int ttl) throws IOException, java.lang.IllegalArgumentException {
	
        if (  ( ttl <= 0 ) || ( ttl > 255 ) ) {
	    throw new java.lang.IllegalArgumentException() ;
        }
	this.ttl = ttl ;
	
	if (state == ONLINE) {
	    discovery.setTimeToLive(ttl) ;
	}
	
    }
    
    /**
     * Get the time-to-live. The default value is returned if the <CODE>TimeToLive</CODE> has not
     * been set.
     * 
     *
     */
    public int getTimeToLive() {
    	return ttl ;
    }
    
    // ----------------------------------------------------------
    // set/unset/get response port
    // ----------------------------------------------------------
    /**
     * Get the unicast datagram socket mode for the Java DMK agent response.
     * The default value is <CODE>true</CODE>.
     * 
     * @return <code>true</code> indicates that unicast datagram socket is 
     *         being used. 
     *         <code>false</code> indicates that the multicast response mode 
     *         is being used.
     */
    public boolean getPointToPointResponse() {
        return pointToPointResponse;
    }
    
    /**
     * Set unicast datagram socket mode for the Java DMK agent response.
     * <P>
     * The client sends a request for a unicast response in each discovery 
     * request. 
     * The multicast group Inet address is used for the unicast response.
     * 
     * @param pointToPointResponse The datagram socket mode.
     * false value unsets the use of unicast socket for the response,
     * multicast is used instead.
     *
     */
    public void setPointToPointResponse(boolean pointToPointResponse) {
       	this.pointToPointResponse = pointToPointResponse ;
	if (state == ONLINE) {
	    discovery.setPointToPointResponse(pointToPointResponse) ;
	}
    }
    
    // ----------------------------------------------------------
    // State stuff
    // ----------------------------------------------------------
    /**
     * Returns the state of this <CODE>DiscoveryClient</CODE>.
     *
     * @return <CODE>ONLINE</CODE>, <CODE>OFFLINE</CODE>.
     */
    public Integer getState() {
	return new Integer(state) ;
    }
    
    /**
     * Returns the state of this <CODE>DiscoveryClient</CODE> in string form.
     * 
     * @return One of the strings "ONLINE", "OFFLINE".
     */
    public String getStateString() {
	String result = "UNKNOWN" ;
	
	switch(state) {             
	case ONLINE: result = "ONLINE" ; break ;
	case OFFLINE: result = "OFFLINE" ; break ;
	}
	return result ;
    }  
    
    // ----------------------------------------------------------
    // Private findCommunicators
    // ----------------------------------------------------------
    private Vector findCommunicators(boolean adp, String host) throws CommunicationException {
	// ----------------
	//  Check that the factory is not already connected
	// ----------------
	if (state != ONLINE) {
	    throw new CommunicationException( "Not connected to " + multicastGroup + ":" + multicastPort ) ;
	}
	
	// ----------------
	// Call actual method
	// ----------------
	try {
	    return discovery.findCommunicators(adp,host) ;
	} catch (IOException e) {
	    if (logger.finestOn()) {
		logger.finest("findCommunicators " , "Get IOException exception " + e.getMessage() ) ;
	    }
	    throw new CommunicationException ("Unable to join group on " + multicastGroup + ":" + multicastPort) ;
	}
    }
    
    // ----------------------------------------------------------
    // Logging stuff
    // ----------------------------------------------------------
    private String localClassName = "com.sun.jdmk.discovery.DiscoveryClient" ;
    
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"DiscoveryClient");
    
    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    private static final String	sccs_id			= "@(#)DiscoveryClient.java 4.44 07/03/08 SMI";
    
    private static int		defaultMulticastPort	= 9000	;
    private static String		defaultMulticastGroup	= "224.224.224.224" ;
    
    /**
     * The multicast port number.
     *
     * @serial
     */
    private int			multicastPort		;
    
    /**
     * The multicast group name.
     *
     * @serial
     */
    private String 			multicastGroup		;
    
    /**
     * The default time-to-live.
     *
     * @serial
     */
    private int			ttl			= 1 ;
    
    /**
     * The unicast datagram socket mode.
     *
     * @serial
     */
    private boolean			pointToPointResponse    = true;
    
    /**
     * The timeout after which we give up waiting for discovery responses.
     *
     * @serial
     */
    private int			timeOut			= 1000 ;
    
    private static int			defaultTimeOut		= 1000 ;
    
    private transient ActualDiscovery	discovery	= null ;
    
    private static final String     GROUP                   = "group";
    private static final String     PORT                    = "port";
    
    /** Reflects the current state of the discovery client. */
    protected transient  volatile  int       state     = OFFLINE ;
    
    // NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
    InetAddress usrInet = null;
    //end of NPCTE fix for bugId 4499338
}
