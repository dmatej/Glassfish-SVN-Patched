/*
 * @(#)file      DiscoveryMonitor.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.37
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
// jdmk import
// ----------------------------------------------------------
import javax.management.*;
import com.sun.jdmk.* ; 
import com.sun.jdmk.internal.ClassLogger;



 /**
 * Describe an MBean that listens for registering and unregistering information sent by
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} objects on a given multicast group.
 * Any agent that is to use multicast discovery must have a
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} registered in its MBean server.
 * When a {@link com.sun.jdmk.discovery.DiscoveryResponder} is registered in an MBean server and when its start or stop methods
 * are called, it informs the rest of the multicast group by sending
 * a multicast message. The format of this message is not exposed.
 * Whenever a <CODE>DiscoveryMonitor</CODE> receives a registration or
 * unregistration message, it sends a {@link com.sun.jdmk.discovery.DiscoveryResponderNotification}
 * to its notification listener.
 * <p>
 * A <CODE>DiscoveryMonitor</CODE> can be instantiated either in stand alone
 * mode (Client side) or added to an MBean Server. In the first case, the client should
 * call the appropriate constructor to initialize the <CODE>multicastGroup</CODE>
 * and <CODE>multicastPort</CODE> parameters.
 * The default values for the group and the port are 224.224.224.224 and
 * 9000. 
 *
 * A <CODE>DiscoveryMonitor</CODE> can be stopped by calling the <CODE>stop</CODE> method. When it is stopped, the
 * <CODE>DiscoveryMonitor</CODE> no longer listens for registering and
 * unregistering messages from {@link com.sun.jdmk.discovery.DiscoveryResponder} objects.
 * A <CODE>DiscoveryMonitor</CODE> can be restarted by invoking the <CODE>start</CODE> method.
 * <p>   
 * A <CODE>DiscoveryMonitor</CODE> has a <CODE>state</CODE> property which reflects its
 * activity.
 * <TABLE> 
 * <TR><TH>DiscoveryMonitor</TH>                 <TH>State</TH></TR>
 * <TR><TD><CODE>running</CODE></TD>          <TD><CODE>ONLINE</CODE></TD></TR>
 * <TR><TD><CODE>stopped</CODE></TD>          <TD><CODE>OFFLINE</CODE></TD></TR>
 * <TR><TD><CODE>stopping</CODE></TD>         <TD><CODE>STOPPING</CODE></TD></TR>
 * <TR><TD><CODE>starting</CODE></TD>         <TD><CODE>STARTING</CODE></TD></TR>
 * </TABLE>
 * <p>    
 * The transition between <CODE>ONLINE</CODE> and <CODE>OFFLINE</CODE> may not
 * be immediate. The <CODE>DiscoveryMonitor</CODE> may need some time to finish
 * or interrupt the active requests. During this time the state of the
 * <CODE>DiscoveryMonitor</CODE> is <CODE>STOPPING</CODE>.
 * When a <CODE>DiscoveryMonitor</CODE> is removed from a Java DMK agent, it is automatically stopped.
 *
 */

public class DiscoveryMonitor extends NotificationBroadcasterSupport 
    implements java.io.Serializable, DiscoveryMonitorMBean, MBeanRegistration {
    private static final long serialVersionUID = 4303049868337627283L;

    // ----------------------------------------------------------
    // States of an DiscoveryMonitor
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
     * Constructs a <CODE>DiscoveryMonitor</CODE>.
     * <P>
     * This constructor creates and initializes a 
     * multicast socket used to listen for {@link com.sun.jdmk.discovery.DiscoveryResponder}
     * objects registering or unregistering.
     * The default group (224.224.224.224) and port (9000) are used.
     * No check is done on the default values: check will be performed by the start method.
     *
     */
    public DiscoveryMonitor () {
        // ------------------------
        // set Default multicastPort/multicastGroup
        // ------------------------
        this.multicastGroup = defaultMulticastGroup ;
        this.multicastPort  = defaultMulticastPort ;
	if (logger.finerOn()) {
		logger.finer("constructor " , "Set group to '" + multicastGroup + "'" );
		logger.finer("constructor " , "Set port  to '" + multicastPort + "'" );
	}
    }

    /**
     * Constructs a <CODE>DiscoveryMonitor</CODE>.
     * <P>
     * This constructor creates and initializes a 
     * multicast socket used to listen for {@link com.sun.jdmk.discovery.DiscoveryResponder}
     * objects registering or unregistering.
     * No check is done on the parameter values: check will be performed by the start method.
     *
     * @param multicastGroup The multicast group name.
     * @param multicastPort The multicast port number.
     * 
     */
    public DiscoveryMonitor (String multicastGroup, int multicastPort) {
        // ------------------------
        // set Default multicastPort/multicastGroup
        // ------------------------
        this.multicastGroup = multicastGroup ;
        this.multicastPort  = multicastPort ;
	if (logger.finerOn()) {
		logger.finer("constructor " , "Set group to '" + multicastGroup + "'" );
		logger.finer("constructor " , "Set port  to '" + multicastPort + "'" );
	}
    }

    /**
     * Constructs a <CODE>DiscoveryMonitor</CODE>.
     * <P>
     * This constructor creates and initializes a 
     * multicast socket used to listen for {@link com.sun.jdmk.discovery.DiscoveryResponder}
     * objects registering or unregistering.
     * No check is done on the parameter values: check will be performed by the start method.
     *
     * @param multicastGroup The multicast group name.
     * @param multicastPort The multicast port number.
     * @param inf The interface used by a MulticastSocket.
     * 
     * @since Java DMK 5.0
     */
    public DiscoveryMonitor (String multicastGroup, int multicastPort, InetAddress inf) {
        // ------------------------
        // set Default multicastPort/multicastGroup
        // ------------------------
        this.multicastGroup = multicastGroup ;
        this.multicastPort  = multicastPort ;

	usrInet = inf;

	if (logger.finerOn()) {
	    logger.finer("constructor " , "Set group to '" + multicastGroup + "'" );
	    logger.finer("constructor " , "Set port  to '" + multicastPort + "'" );
	    logger.finer("constructor " , "Set interface  to '" + inf + "'" );
	}
    }

    // ----------------------------------------------------------
    // MBeanServer interaction
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
		logger.finer("preRegister " , "object name   = " + name );
	}
        // ----------------
        // Identify the host for multicast
        // ----------------
        String grp ;
        if ( (grp = (String)name.getKeyProperty(GROUP)) != null) {
                multicastGroup =  grp;
        }
	if (logger.finerOn()) {
		logger.finer("preRegister " , "Set group to '" + multicastGroup + "'" );
	}

        // ----------------
        // Identify the port for multicast
        // ----------------
        String port ;
        if ( (port = name.getKeyProperty(PORT)) != null) {
                multicastPort  = Integer.parseInt(port) ;
        }
	if (logger.finerOn()) {
		logger.finer("preRegister " , "Set Port  to '" + multicastPort + "'" );
	}
        monitorObjectName = name;

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
	if ( registrationDone == Boolean.FALSE ) {
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
        // Stop corresponding thread
        // ------------------------
        stop () ;
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
    // start method
    // ----------------------------------------------------------
    /**
     * Starts listening for {@link com.sun.jdmk.discovery.DiscoveryResponder} objects registering/unregistering.
     * <P>
     * This method has no effect if the <CODE>DiscoveryMonitor</CODE> is <CODE>ONLINE</CODE> or
     * <CODE>STOPPING</CODE> or <CODE>STARTING</CODE>.
     *
     * @exception IOException The creation of the Multicast socket failed.
     */

    public void start() throws IOException {
        // ----------------
        // start actual monitor
        // ----------------
        if (state == OFFLINE) {
                try {
		    changeState(STARTING);
                        // ----------------
                        // Create a new ActualMonitor 
                        // ----------------
                        monitor = new ActualMonitor(multicastGroup,multicastPort,this);
 		
			// NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
			if (usrInet != null) {
			    monitor.setInterface(usrInet);
			    if (logger.finerOn()) {
                        	logger.finer("start" , "set to the interface "+usrInet) ;
			    }
                	}
			// end of NPCTE fix for bugId 4499338
   
                        // ----------------
                        // Create a new thread to receive multicast msg ;
                        // ----------------
                        if ( cmf == null ){
                            monitorThread = new Thread(monitor);
                        } else {
                            // monitorThread = cmf.getThreadAllocatorSrvIf().obtainThread(monitorObjectName, monitor);
                            monitorThread = new Thread( monitor);
                        }
                        monitorThread.setName("Multicast monitor");
                } catch ( IOException e) {
                	if (logger.finestOn()) {
                        	logger.finest("start" , e) ;
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

                monitorThread.start() ;
   
            } else {
		if (logger.finerOn()) {
			logger.finer("start " , "not OFFLINE ") ;
		}
            }
    }

    // ----------------------------------------------------------
    // stop method
    // ----------------------------------------------------------
    /**
     * Stops this <CODE>DiscoveryMonitor</CODE>.
     * <P>
     * This method has no effect if the monitor is <CODE>OFFLINE</CODE> or
     * <CODE>STOPPING</CODE> or <CODE>STARTING</CODE>.
     */
 
    public void stop() {
	if (state == ONLINE) {
	    changeState(STOPPING);

	    // ------------------------
	    // Stop the monitor thread
	    // ------------------------
	    monitor.stopMonitor() ;
	} else {
	    if (logger.finerOn()) {
		logger.finer("stop " , "not ONLINE") ;
            }
	
	}
    }

    // ----------------------------------------------------------
    // getter / setter
    // ----------------------------------------------------------
    /**
     * Returns the state of this <CODE>DiscoveryMonitor</CODE>.
     *
     * @return <CODE>ONLINE</CODE>,<CODE>OFFLINE</CODE>, <CODE>STOPPING</CODE> or <CODE>STARTING</CODE>.
     */

    public Integer getState() {
        return new Integer(state) ;
    }

    /**
     * Returns the state of this <CODE>DiscoveryMonitor</CODE> in string form.
     *
     * @return One of the strings "ONLINE", "OFFLINE", "STOPPING" or "STARTING".
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
     * Returns the multicast group.
     ** A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     *
     * @return A string containing the multicast group name.
     */
    public String getMulticastGroup() {
        return multicastGroup;
    }

    /**
     * Sets the multicast group name.
     ** A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     * <P>
     * Only available if state in OFFLINE
     *
     * @param multicastGroup The multicast group name.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryMonitor</CODE> was ONLINE or STARTING.
     */
    public void setMulticastGroup(String multicastGroup) 
    throws java.lang.IllegalStateException {
        if ( state == OFFLINE ) {
            this.multicastGroup = multicastGroup ;
        }
	else {
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
     *            the <CODE>DiscoveryMonitor</CODE> was ONLINE or STARTING.
     */
    public void setMulticastPort(int multicastPort)
    throws java.lang.IllegalStateException  {
        if ( state == OFFLINE ) {
            this.multicastPort = multicastPort ;
        } else {
		throw new java.lang.IllegalStateException() ;
	}
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
     *    returns immediately (i.e. does not wait at all),</LI>
     * <LI> if <VAR>timeout</VAR> equals zero then <CODE>waitState</CODE> 
     *    waits until the value of this MBean's State attribute is the same 
     *    as the <VAR>state</VAR> parameter (i.e. will wait indefinitely 
     *    if this condition is never met).</LI></UL>
     * </p>
     * @param state The value of this MBean's State attribute to wait for. 
     *        <VAR>state</VAR> can be one of:
     *        <CODE>DiscoveryMonitor.OFFLINE</CODE>,
     *        <CODE>DiscoveryMonitor.ONLINE</CODE>,
     *        <CODE>DiscoveryMonitor.STARTING</CODE>, 
     *        <CODE>DiscoveryMonitor.STOPPING</CODE>.
     * @param timeout The maximum time to wait for, in milliseconds, if 
     *        positive. 
     *        Infinite time out if 0, or no waiting at all if negative.
     *
     * @return <code>true</code> if the value of this MBean's State attribute 
     *         is the same as the <VAR>state</VAR> parameter; 
     *         <code>false</code> otherwise.
     */
    public boolean waitState(int state, long timeout) {
        if (logger.finerOn()) {
            logger.finer("waitState",state + "(0on,1off,2st) TO=" + timeout  + " ; current state = " + getStateString()) ;
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
    private String localClassName = "com.sun.jdmk.discovery.DiscoveryMonitor" ;
    
    private String dbgTag = localClassName ;                                 
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"DiscoveryMonitor");
    
    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    
    /**
     * The object name of the discovery monitor.
     *
     * @serial
     */
    private ObjectName              monitorObjectName       = null;

    /**
     * This field holds a reference to the core management framework.
     *
     * @serial
     */
    private MBeanServer               cmf                     = null;
    
    private static final String     sccs_id                 = "@(#)DiscoveryMonitor.java 4.37 07/03/08 SMI" ;

    private static int              defaultMulticastPort    = 9000  ;
    private static String           defaultMulticastGroup   = "224.224.224.224" ;

    /**
     * The multicast port number.
     *
     * @serial
     */
    private int                     multicastPort           ;

    /**
     * The multicast group name.
     *
     * @serial
     */
    private String                  multicastGroup          ;

    private transient ActualMonitor monitor         = null ;
    private transient Thread        monitorThread   = null ;
    /** Reflects the current state of the discovery monitor. */
    private transient volatile int           state  = OFFLINE ;

    private transient Vector        listeners       = new Vector();

    private static final String     GROUP           = "group";
    private static final String     PORT            = "port";

    // NPCTE fix for bugId 4499338, esc 0, 04 Sept 2001
    private InetAddress usrInet = null;
    // end of NPCTE fix for bugId 4499338

}
