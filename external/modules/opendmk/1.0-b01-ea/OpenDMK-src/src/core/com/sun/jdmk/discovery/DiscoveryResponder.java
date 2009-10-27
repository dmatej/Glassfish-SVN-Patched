/*
 * @(#)file      DiscoveryResponder.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.42
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


// ------------------------
// jdmk import
// ------------------------
import javax.management.*;
import com.sun.jdmk.* ; 
import com.sun.jdmk.internal.ClassLogger;



/**
 * Implements the MBean that responds to the discovery requests. Any agent 
 * that needs to be discovered must instantiate and register a 
 * <CODE>DiscoveryResponder</CODE> in its MBean server.
 *
 * <p>
 * When the <CODE>DiscoveryResponder</CODE> <CODE>start</CODE> method is 
 * called, the MBean creates a multicast socket. 
 * The <CODE>DiscoveryResponder</CODE> then sends a join message
 * to the multicast group.
 * When a <CODE>DiscoveryResponder</CODE> is unregistered from the MBean server,
 * or when <CODE>stop</CODE> method is called, the MBean
 * sends a leave message to the multicast group. The format of these messages
 * is not exposed. These messages allow {@link 
 * com.sun.jdmk.discovery.DiscoveryMonitor} objects to
 * maintain a list of agents with <CODE>DiscoveryResponder</CODE> objects
 * registered in their MBean server.
 * When <CODE>start</CODE> method is called, and when a
 * join message has been sent, the <CODE>DiscoveryResponder</CODE> starts to 
 * listen for discovery requests.
 * <p>   
 * The multicast socket uses the group and port specified by the
 * properties <CODE>multicastGroup</CODE> and <CODE>multicastPort</CODE>.
 * The default values for the group and the port are 224.224.224.224 and 9000.
 * These values can be changed using appropriate constructor.
 * These values can be also changed using <CODE>setMulticastGroup</CODE> and
 * <CODE>setMulticastPort</CODE> methods when the 
 * <CODE>DiscoveryResponder</CODE> is OFFLINE.
 * <p>   
 * When join/leave message are sent to the multicast group, a default
 * time-to-live (see <CODE>java.net.MulticastSocket</CODE>) value is used. The
 * time-to-live value specifies how many "hops" that the packet is forwarded on
 * the network before it expires.  
 * <CODE>DiscoveryResponder</CODE> objects use a time-to-live specified by 
 * the property <CODE>ttl</CODE>.
 * The default time-to-live value is 1. It can be changed using 
 * <CODE>setTimeToLive</CODE> method when the <CODE>DiscoveryResponder</CODE> 
 * is OFFLINE.
 *
 */

public class DiscoveryResponder 
    implements java.io.Serializable, DiscoveryResponderMBean, 
	       MBeanRegistration {
    private static final long serialVersionUID = -3259829717493969016L;

    // ----------------------------------------------------------
    // States of an DiscoveryResponder
    // ----------------------------------------------------------
    /** Marks the "state" property as running. */
    public static final int ONLINE = 0 ;
    /** Marks the "state" property as stopped. */
    public static final int OFFLINE = 1 ;
    /** Marks the "state" property as in-transition from ONLINE to OFFLINE. */
    public static final int STOPPING = 2 ;
    /** Marks the "state" property as in-transition from OFFLINE to ONLINE. */
    public static final int STARTING = 3 ;

    // ----------------------------------------------------------
    // Constructor
    // ----------------------------------------------------------
    /**
     * Constructs a <CODE>DiscoveryResponder</CODE>.
     * <P>
     * Creates a discovery responder using the
     * default multicast group (224.224.224.224) and the 
     * default multicast port (9000).
     * No check is done on the default values: check will be performed by the start method.
     * The multicast group and port values can be overwritten using setter 
     * the properties <CODE>multicastGroup</CODE> and <CODE>multicastPort</CODE>.
     */
    public DiscoveryResponder () {
	this(defaultMulticastGroup, defaultMulticastPort, null , null);
    }

    /**
     * Constructs a <CODE>DiscoveryResponder</CODE>.
     * <P>
     * Creates a discovery responder using the specified multicast group and the 
     * specified multicast port.
     * The multicast group and port values can be overwritten using setter 
     * the properties <CODE>multicastGroup</CODE> and <CODE>multicastPort</CODE>.
     * No check is done on the parameter values: check will be performed by the start method.
     * 
     * @param multicastGroup The multicast group name.
     * @param multicastPort The multicast port number.
     */
    public DiscoveryResponder (String multicastGroup, int multicastPort) {
	this(multicastGroup, multicastPort, null , null);
    }

    // NPCTE fix for bugId 4499338, esc 0, Sept 2001
  /**
   * Constructs a <CODE>DiscoveryResponder</CODE>.
   * <P>
   * This constructor initialize multicast group and  a multicast port.
   * No check is done on the parameter values. Check will be performed by the start method.
   *
   * @param multicastGroup The multicast group name.
   * @param multicastPort The multicast port number.
   * @param inf the interface used by a MulticastSocket.
   */
    public DiscoveryResponder (String multicastGroup, int multicastPort, InetAddress inf) {
	this(multicastGroup, multicastPort, inf, null);
    }
    // end NPCTE  fix for bugId 4499338

  /**
   * Constructs a <CODE>DiscoveryResponder</CODE>.
   * <P>
   * This constructor initialize multicast group and  a multicast port.
   * No check is done on the parameter values. Check will be performed by the start method.
   *
   * @param multicastGroup The multicast group name.
   * @param multicastPort The multicast port number.
   * @param inf the interface used by a MulticastSocket.
   * @param local the local host address used to answer a request.
   */
    public DiscoveryResponder (String multicastGroup, int multicastPort, InetAddress inf, String local) {
        // ----------------
        // initialization
        // ----------------
        this.multicastGroup =  multicastGroup;
        this.multicastPort  =  multicastPort ;

	if (inf != null &&
	    !com.sun.jdmk.internal.Useful.isLocalHost(inf)) {

	    throw new IllegalArgumentException("The user specified multi cast group is not a local interface: "+inf);
	}
	usrInet = inf;

	if (local != null &&
	    !com.sun.jdmk.internal.Useful.isLocalHost(local)) {

	    throw new IllegalArgumentException("The user specified local host is not a local: "+local);
	}
	this.local = local;

	if (logger.finerOn()) {
		logger.finer("constructor " , "Set group to '" + multicastGroup + "'" );
		logger.finer("constructor " , "Set port  to '" + multicastPort + "'" );

		if (inf != null) {
		    logger.finer("constructor " , "Set interface  to '" + inf + "'" );
		}

		if (local != null) {
		    logger.finer("constructor " , "Set the local host to '" + local + "'" );
		}
	}
    }

    // ----------------------------------------------------------
    // MBeanServer interaction
    // ----------------------------------------------------------

    /**  
     * Allows the MBean to perform any operations it needs before being 
     * registered in the MBean server. 
     * If the name of the MBean is not specified, the
     * MBean can provide a name for its registration. If any exception is
     * raised, the MBean will not be registered in the MBean server.
     *   
     * @param server The MBean server in which the MBean will be registered.
     * @param name The object name of the MBean.
     *   
     * @return  The name of the MBean registered.
     *   
     * @exception java.lang.Exception This exception should be caught by the 
     *            MBean server and re-thrown as an {@link 
     *            javax.management.MBeanRegistrationException}.
     */  
    public ObjectName preRegister(MBeanServer server, ObjectName name) 
	throws java.lang.Exception {
	if (logger.finerOn()) {
		logger.finer("preRegister " , "object name   = " + name );
	}
        
        responderObjectName = name ;

        // ----------------
        // Should we act as a spy ?
        // ----------------
        spy = (String)name.getKeyProperty(SPY) ;

        // ----------------
        // Should we Send event
        // ----------------
        noEvent = (String)name.getKeyProperty("PRIVATE_NO_EVENT") ;

        // ----------------
        // Initialise local pointer to the Core Management MBeanServer
        // ----------------
        this.cmf = server;

        // ----------------
        // Return part
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
        // Call stop
        // ------------------------
        if (state == ONLINE) {
                stop () ;
            }

    }


    /**
     * Allows the MBean to perform any operations needed after having been
     * unregistered in the MBean server.
     */
    public void postDeregister() {
    }


    // ----------------------------------------------------------
    // start/stop implementation
    // ----------------------------------------------------------
    
    /**
     * Create a multicast socket and join the multicast group.
     * This method creates a multicast socket that is used to broadcast 
     * The <CODE>DiscoveryResponder</CODE> will then join the multicast group and send a join message.
     * This method has no effect if the <CODE>DiscoveryResponder</CODE> is <CODE>ONLINE</CODE> or
     * <CODE>STOPPING</CODE> or <CODE>STARTING</CODE>.
     *   
     * @exception IOException The creation of the Multicast socket failed.
     */
    public void start() throws IOException {
        if (state == OFFLINE) {
	    changeState(STARTING) ;
         	if ( cmf == null ) {
			if (logger.finerOn()) {
				logger.finer("start " , "Can't start discoveryResponder: JDMK MBeanServer is not set") ;
			}
                        return ;
                }

               // ----------------
               // Create a new obj to receive multicast msg ;
               // ----------------
               try {
			if (logger.finerOn()) {
				logger.finer("start " , "Create a new responder");
			}
                        responder = new ActualResponder(multicastGroup,multicastPort,getTimeToLive(),cmf,spy, this);

			if (local != null) {
			    responder.setLocalHost(local);
			}

 			// NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
			if (usrInet != null) {
			    responder.setInterface(usrInet);

			    if (logger.finerOn()) {
				logger.finer("start " , "set interface to "+usrInet);
			    }
			}
			// end of NPCTE fix for bugId 4499338

                       if (noEvent != null ) responder.noEvent() ;
    
			if (logger.finerOn()) {
				logger.finer("start " , "call responder connect");
			}
                        responder.connectToGroup () ;
    
	                // ----------------
	                // Create a new thread to receive multicast msg ;
	                // ----------------
	                // responderThread = cmf.getThreadAllocatorSrvIf().obtainThread(responderObjectName, responder);
	                responderThread = new Thread(responder);
	                responderThread.setName("Multicast responder");
	    
	                // ----------------
	                // Start thread
	                // ----------------
	                responderThread.start () ;
	
	                // ----------------
	                // Update state
	                // ----------------
	                //changeState(ONLINE) ;
                } catch (SocketException e) {
                        if (logger.finestOn()) {
                                logger.finest("start",e) ;
                        }
                        throw new IOException(e.getMessage()) ;

               } catch (IOException e) {
                        // ------------------------
                        // Keep exception for us
                        // ------------------------
			if (logger.finestOn()) {
				logger.finest("start " , e) ;
			}
			throw e ;
               } catch (NullPointerException e) {
                        // ------------------------
                        // the set group did not worked
                        // ------------------------
			if (logger.finestOn()) {
				logger.finest("start " , e) ;
			}
			throw new IOException(e.getMessage() ) ;
                }
        } else {
		if (logger.finerOn()) {
			logger.finer("start " , "Responder is not OFFLINE") ;
		}
            } 
    }


    /**  
     * Sends a leave message to the multicast group and leaves it.
     * The <CODE>DiscoveryResponder</CODE> leaves its multicast group.
     * This method has no effect if the <CODE>DiscoveryResponder</CODE> is <CODE>OFFLINE</CODE> or
     * <CODE>STOPPING</CODE> or <CODE>STARTING</CODE>.
     */ 
    public void stop() {
        if (state == ONLINE) {
	    changeState(STOPPING);
                // ------------------------
                // Stop corresponding thread
                // ------------------------

	    responder.stopRequested =  true ;

	    synchronized(responder.interrupted) {
		if (!responder.interrupted.booleanValue()) {		    
		    responderThread.interrupt() ;
		}
	    }

	    // Fix for cases when the interrupt does not work (Windows NT)
	    try {
		MulticastSocket ms = new MulticastSocket(multicastPort);

		// NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
		if (usrInet != null) {
		    ms.setInterface(usrInet);

		    	if (logger.finerOn()) {
			    logger.finer("stop " , "use the interface " + usrInet) ;
			}
		}
		// end of NPCTE fix for bugId 4499338

		InetAddress group = InetAddress.getByName(multicastGroup);
		ms.joinGroup(group);
		ms.send(new DatagramPacket(new byte[1], 1, group, multicastPort));
	        ms.leaveGroup(group);
	    } catch (Exception e) {
		if (logger.finerOn()) {
			logger.finer("stop " , "Unexpected exception occurred trying to send empty message " + e.getMessage()) ;
		}
	    }


	    // ------------------------
	    // free 'remained' allocated resource
	    // ------------------------
	    responder = null ;
	    System.runFinalization() ;
	    
	    // ----------------
	    // Update state
	    // ----------------
	    //changeState(OFFLINE) ;
        } else {
	    if (logger.finerOn()) {
		logger.finer("stop " , "Responder is not ONLINE") ;
	    }
        }
    }
    

    /**
     * Tests if the <CODE>DiscoveryResponder</CODE> is active.
     * <CODE>True</CODE> is returned if the <CODE>DiscoveryResponder</CODE> is started (<CODE>DiscoveryResponder</CODE>
     * has join the multicast group).
     */
    public boolean isActive() {
        return (state == ONLINE);
    }



   /**
     * Waits until either the State attribute of this MBean equals the 
     * specified <VAR>state</VAR> parameter, or the specified 
     * <VAR>timeout</VAR> has elapsed. The method <CODE>waitState</CODE> 
     * returns with a boolean value indicating whether the specified 
     * <VAR>state</VAR> parameter equals the value of this MBean's State 
     * attribute at the time the method terminates.
     * <p>
     * Two special cases for the <VAR>timeout</VAR> parameter value are:
     * <UL><LI> if <VAR>timeout</VAR> is negative then <CODE>waitState</CODE> 
     *     returns immediately (i.e. does not wait at all),</LI>
     * <LI> if <VAR>timeout</VAR> equals zero then <CODE>waitState</CODE> 
     *     waits until the value of this MBean's State attribute 
     *     is the same as the <VAR>state</VAR> parameter (i.e. will wait 
     *     indefinitely if this condition is never met).</LI></UL>
     * 
     * @param state The value of this MBean's State attribute to wait for. 
     *        <VAR>state</VAR> can be one of:
     *        <CODE>DiscoveryResponder.OFFLINE</CODE>, 
     *        <CODE>DiscoveryResponder.ONLINE</CODE>,
     *        <CODE>DiscoveryResponder.STARTING</CODE>, 
     *        <CODE>DiscoveryResponder.STOPPING</CODE>.
     * @param timeout The maximum time to wait for, in 
     *        milliseconds, if positive. 
     *        Infinite time out if 0, or no waiting at all if negative.
     *
     * @return <code>true</code> if the value of this MBean's State attribute
     *         is the same as the <VAR>state</VAR> parameter; 
     *         <code>false</code> otherwise.
     */
    public boolean waitState(int state, long timeout) {
        if (logger.finerOn()) {
            logger.finer("waitState",state + "(0on,1off,2st) TO=" + timeout  + 
		  " ; current state = " + getStateString()) ;
        }
        // ------------
        // Test timeout
        // ------------
        if (timeout < 0) {
            return (this.state == state) ;
        }
        
        boolean done = (this.state == state) ;
        Date currentDate ;
        long currentTimeOut = -1;

        // -----------------
        // Date start 
        // -----------------
        Date endDate = new Date((new Date()).getTime() + timeout) ;     
        while ( ! done ) {
            // -----------------
            // Find out timeout
            // ----------------
            if (timeout != 0){
                currentTimeOut = endDate.getTime() - ( new Date()).getTime() ;
                if (currentTimeOut <= 0 ) {
                    done = true ;
                    break ;
                }
            }

            try {
                synchronized(this) {
                    if (timeout == 0){
                        if (logger.finerOn()) {
                            logger.finer("waitState",
                            "Start waiting infinite, current state = "+
                            this.state) ;
                        }
                        done = (this.state == state)  ;
                        while ( !done )
                            {
                                done = (this.state == state)  ;
                                try { wait(1000); } catch (Exception e) {} 
                            }
                    } else {
                        if (logger.finerOn()) {
                            logger.finer("waitState","Start waiting "+currentTimeOut+" current state = "+this.state) ;
                        }
                        wait (currentTimeOut) ;
                    }
                }
                done = (this.state == state) ;
            }
            catch (InterruptedException e) {
                done = (this.state == state) ;
            }
        }
        if (logger.finerOn()) {
            logger.finer("waitState","End, TO="+currentTimeOut ) ;
        }
        return (this.state == state) ;
    }

    
    // ----------------------------------------------------------
    // Getters and Setters
    // ----------------------------------------------------------
 

    /**
     * Returns the multicast group.
     * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     *
     * @return A string containing the multicast group name.
     */
    public String getMulticastGroup() {
        return multicastGroup;
    }

    /**
     * Sets the multicast group name.
     * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     * <P>
     * Only available if state in OFFLINE
     *
     * @param multicastGroup The multicast group name.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryResponder</CODE> was ONLINE or STARTING.
     */
    public void setMulticastGroup(String multicastGroup)
    throws java.lang.IllegalStateException  {
        if ( state == OFFLINE ) {
                this.multicastGroup = multicastGroup ;
        } else {
		throw new java.lang.IllegalStateException() ;
	}
    }


    /**
     * Returns the multicast port.
     * It can be any standard UDP port number.
     *
     * @return The multicast port number.
     */
    public int getMulticastPort() {
        return multicastPort ;
    }

    /**
     * Sets the multicast port.
     * It can be any standard UDP port number.
     * <P>
     * Only available if state in OFFLINE
     *
     * @param multicastPort The multicast port.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryResponder</CODE> was ONLINE or STARTING.
     */
    public void setMulticastPort(int multicastPort) 
    throws java.lang.IllegalStateException {
        if ( state == OFFLINE ) {
                this.multicastPort = multicastPort ;
        } else {
		throw new java.lang.IllegalStateException() ;
	}
    }

    /**
     * Returns the time-to-live value.
     *
     * @return The time-to-live value.
     */
    public int getTimeToLive() {
        return ttl ;
    }

    /**
     * Sets the default Time to Live to be used to send join and leave message to the Multicast group.
     * <P>
     * Time to Live should an integer verifying the following condition: 0 < ttl <= 255
     * Only available if state in OFFLINE
     *
     * @param ttl The Time to live value.
     *
     * @exception java.lang.IllegalArgumentException The input ttl value is not
     *            in the 1 to 255 range.
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryResponder</CODE> was ONLINE or STARTING.
     */
    public void setTimeToLive(int ttl)
    throws java.lang.IllegalStateException  {
        if ( state == OFFLINE ) {
                if (  ( ttl <= 0 ) || ( ttl > 255 ) ) {
                        throw new java.lang.IllegalArgumentException() ;
                }
                this.ttl = ttl ;
        } else {
		throw new java.lang.IllegalStateException() ;
	}
    }

    /**
     * Returns the state of this <CODE>DiscoveryResponder</CODE>.
     *
     * @return <CODE>ONLINE</CODE>, <CODE>OFFLINE</CODE> or <CODE>STOPPING</CODE> or <CODE>STARTING</CODE> .
     */
    public Integer getState() {
        return new Integer(state) ;
    }
 
    /**
     * Returns the state of this <CODE>DiscoveryResponder</CODE> in string form.
     * 
     * @return One of the strings "ONLINE", "OFFLINE" or "STOPPING" or "STARTING".
     */
    public String getStateString() {
        String result = "UNKNOWN" ;
 
        switch(state) {
        case ONLINE: result = "ONLINE" ; break ;
        case OFFLINE: result = "OFFLINE" ; break ;
        case STOPPING: result = "STOPPING" ; break ;
	case STARTING: result = "STARTING" ; break ;
        }
        return result ;
    }


    /**
     * Allows the user to specify additional information in the
     * <CODE>DiscoveryResponse</CODE> message.
     *
     * The following limitation applies to the length of the byte array
     * parameter:
     * The length of a UDP packet is maximum 64 Kbytes. In addition to the
     * specified user data, the discovery response UDP packet contains the
     *  following :
     * <UL> 
     * <LI>  All string attributes of the MBean server delegate MBean (in
     * the following order: the MBeanServerId; the specification
     * name, vendor and version; and the implementation name, vendor
     * and version).
     * <LI> The ObjectName and the ConnectorAddresses for each of the 
     * connectors/adaptors registered in the MBean server.
     * </UL>
     * The user should take into consideration this content whose size is
     * dependent upon the delegate's string attributes and the number of
     * connectors/adaptors registered in an MBean server. The provided byte
     * array should not exceed the space remaining up to the UDP packet's 64
     * Kbyte limit. Otherwise, the packet is truncated and information will be
     * lost.
     *
     * For example, if you wish to allow enough space to include up to 100
     * connectors/adaptors in the discovery response message, the data byte
     * array should not exceed 40 KBytes, approximately.
     *
     */
    public void setUserData(byte[] data) {
	this.userData = data;
    }

    /**
     * Returns a byte[] containing the additional information that the user added in the 
     * <CODE>DiscoveryResponse</CODE>.
     * If no additional information has been added, this method returns <CODE>null</CODE>.
     */
    public byte[] getUserData() {
       return(userData);
    }



    /**
     * For Java DMK internal use only.
     */
    synchronized void changeState(int s) {
        if ( state == s ) {
            return;
        }       
        int old = state;
        state = s ;
        notifyAll() ;
    }
    

    // ----------------------------------------------------------
    // Logging stuff
    // ----------------------------------------------------------

    private String localClassName = 
	"com.sun.jdmk.discovery.DiscoveryResponder" ;
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"DiscoveryResponder");
    



    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    private static final String sccs_id         = "@(#)DiscoveryResponder.java 4.42 07/03/08 SMI";

    /**
     * The multicast port number.
     *
     * @serial
     */
    private int          multicastPort       ;
    
    /**
     * The multicast group name.
     *
     * @serial
     */
    private String       multicastGroup      ;

    private static int          defaultMulticastPort    = 9000 ;
    private static String       defaultMulticastGroup   = "224.224.224.224";

    /**
     * The default time-to-live.
     *
     * @serial
     */
    private int          ttl                 = 1 ;

    /**
     * This field holds a reference to the core management framework.
     *
     * @serial
     */
    private MBeanServer    cmf                 = null;

    /**
     * The object name of the discovery responder.
     *
     * @serial
     */
    private ObjectName   responderObjectName = null;

    private transient ActualResponder responder        = null ;
    private transient Thread          responderThread  = null ;

    private static final String HOST        = "host";
    private static final String GROUP       = "group";
    private static final String PORT        = "port";
    private static final String TTL         = "ttl";
    private static final String SPY         = "spy";
    
    /**
     * For Java DMK internal use only.
     *
     */
    private              String spy         = "" ;

    /**
     * For Java DMK internal use only.
     *
     */
    private              String noEvent     = null ;
    
    /** Reflects the current state of the discovery responder. */
    private transient volatile int   state       = OFFLINE ;
    
    /** The additional data that the user added in the Discovery Response message */
    private byte[] userData;
    // NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
    private InetAddress usrInet = null;
    // end of NPCTE fix for bugId 4499338

    private String local = null;
}
