/*
 * @(#)file      CommunicatorServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.58
 * @(#)lastedit      07/03/08
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
import java.io.ObjectInputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.util.Date;
import java.util.Vector;
import java.util.Enumeration;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.MBeanRegistration;
import javax.management.ObjectName;
import javax.management.NotificationListener;
import javax.management.NotificationFilter;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationBroadcasterSupport;
import javax.management.MBeanNotificationInfo;
import javax.management.AttributeChangeNotification;
import javax.management.ListenerNotFoundException;
import javax.management.loading.ClassLoaderRepository;
import javax.management.MBeanServerFactory;

// jmx RI import
//
import com.sun.jdmk.internal.ClassLogger;


// jdmk import
//
import com.sun.jdmk.MBeanServerForwarder;

/**
 * Defines generic behavior for the server part of a connector or an adaptor.
 * Most connectors or adaptors extend <CODE>CommunicatorServer</CODE>
 * and inherit this behavior. Connectors or adaptors that do not fit into 
 * this model do not extend 
 * <CODE>CommunicatorServer</CODE>.
 * <p>
 * A <CODE>CommunicatorServer</CODE> is an active object, it listens for 
 * client requests and processes them in its own thread. When necessary, 
 * a <CODE>CommunicatorServer</CODE>
 * creates other threads to process multiple requests concurrently.
 * <p>
 * A <CODE>CommunicatorServer</CODE> object can be stopped by calling the 
 * <CODE>stop</CODE> method. When it is stopped, the 
 * <CODE>CommunicatorServer</CODE> no longer listens to client 
 * requests and no longer holds any thread or communication resources.
 * It can be started again by calling the <CODE>start</CODE> method.
 * <p>
 * A <CODE>CommunicatorServer</CODE> has a <CODE>State</CODE> attribute 
 * which reflects its  activity.
 * <p>
 * <TABLE>
 * <TR><TH>CommunicatorServer</TH>     <TH>State</TH></TR>
 * <TR><TD><CODE>stopped</CODE></TD>   <TD><CODE>OFFLINE</CODE></TD></TR>
 * <TR><TD><CODE>starting</CODE></TD>  <TD><CODE>STARTING</CODE></TD></TR>
 * <TR><TD><CODE>running</CODE></TD>   <TD><CODE>ONLINE</CODE></TD></TR>
 * <TR><TD><CODE>stopping</CODE></TD>  <TD><CODE>STOPPING</CODE></TD></TR>
 * </TABLE>
 * <p>
 * The <CODE>STARTING</CODE> state marks the transition from 
 * <CODE>OFFLINE</CODE> to <CODE>ONLINE</CODE>.
 * <p>
 * The <CODE>STOPPING</CODE> state marks the transition from 
 * <CODE>ONLINE</CODE> to <CODE>OFFLINE</CODE>. 
 * This occurs when the <CODE>CommunicatorServer</CODE> is 
 * finishing or interrupting active requests.
 * <p>
 * When a <CODE>CommunicatorServer</CODE> is unregistered from the 
 * MBeanServer, it is stopped automatically.
 * <p>
 * When the value of the <CODE>State</CODE> attribute changes the 
 * <CODE>CommunicatorServer</CODE> sends a
 * <tt>{@link javax.management.AttributeChangeNotification}</tt> to the 
 * registered listeners, if any.
 *
 */

public abstract class CommunicatorServer 
    implements Runnable, MBeanRegistration, NotificationBroadcaster, 
    CommunicatorServerMBean {
    
    //
    // States of a CommunicatorServer
    //
    
    /**
     * Represents an <CODE>ONLINE</CODE> state.
     */
    public static final int ONLINE = 0 ;
    
    /**
     * Represents an <CODE>OFFLINE</CODE> state.
     */
    public static final int OFFLINE = 1 ;
    
    /**
     * Represents a <CODE>STOPPING</CODE> state.
     */
    public static final int STOPPING = 2 ;
    
    /**
     * Represents a <CODE>STARTING</CODE> state.
     */
    public static final int STARTING = 3 ;

    //
    // Types of connectors.
    //
    
    /**
     * Indicates that it is an RMI connector type.
     */
    public static final int RMI_TYPE = 1 ;
    
    /**
     * Indicates that it is an HTTP connector type.
     */
    public static final int HTTP_TYPE = 2 ;
    
    /**
     * Indicates that it is an HTML connector type.
     */
    public static final int HTML_TYPE = 3 ;
    
    /**
     * Indicates that it is an SNMP connector type.
     */
    public static final int SNMP_TYPE = 4 ;
    
    /**
     * Indicates that it is an HTTPS connector type.
     */
    public static final int HTTPS_TYPE = 5 ;
    
    //
    // Package variables
    //
    
    /**
     * The state of the connector server.
     */
    transient volatile int state = OFFLINE ;
    
    /**
     * The object name of the connector server.
     * @serial
     */
    ObjectName objectName ;
    
    MBeanServer topMBS;
    MBeanServer bottomMBS;
    
    /**
     */
    transient String dbgTag = null ;
    
    /**
     * The maximum number of clients that the CommunicatorServer can 
     * process concurrently.
     * @serial 
     */
    int maxActiveClientCount = 1 ;
    
    /**
     */
    transient int servedClientCount = 0 ;
    
    /**
     * The host name used by this CommunicatorServer.
     * @serial
     */
    String host = null ;
    
    /**
     * The port number used by this CommunicatorServer.
     * @serial
     */
    int port = -1 ;
    
    
    //
    // Private fields
    //
    
    /* This object controls access to the "state" and "interrupted" variables.
       If held at the same time as the lock on "this", the "this" lock must
       be taken first.  */
    private transient Object stateLock = new Object();
    
    private transient Vector clientHandlerVector = new Vector() ;
    
    private transient Thread fatherThread = Thread.currentThread() ;
    private transient Thread mainThread = null ;
    
    private volatile boolean stopRequested = false ;
    private boolean interrupted = false;
    
    // Notifs count, broadcaster and info
    private transient long notifCount = 0;
    private transient NotificationBroadcasterSupport notifBroadcaster = 
	new NotificationBroadcasterSupport();
    private transient MBeanNotificationInfo[] notifInfos = null;
    transient ClassLogger logger;
    
    /**
     * Instantiates a <CODE>CommunicatorServer</CODE>.
     *
     * @param connectorType Indicates the connector type. Possible values are:
     * RMI_TYPE, HTTP_TYPE, HTML_TYPE, SNMP_TYPE, HTTPS_TYPE.
     *
     * @exception <CODE>java.lang.IllegalArgumentException</CODE> 
     * This connector type is not correct.
     */  
    public CommunicatorServer(int connectorType) 
	throws java.lang.IllegalArgumentException {
        switch (connectorType) {
        case RMI_TYPE :
            infoType = RMI_TYPE ;
            break;
        case HTTP_TYPE :
            infoType = HTTP_TYPE ;
            break;
        case HTML_TYPE :
            infoType = HTML_TYPE ;
            break;
        case SNMP_TYPE :
            infoType = SNMP_TYPE ;
            break;
        case HTTPS_TYPE :
            infoType = HTTPS_TYPE ;
            break;
        default:
            throw new IllegalArgumentException("Invalid connector Type") ;
        }
        dbgTag = makeDebugTag() ;
	logger = makeLogger(dbgTag);
    }
    
    // Ideally this method should be implemented in subclasses, so that 
    // we can get rid of references to Trace.*;
    //
    ClassLogger makeLogger(String dbgTag)
	throws IllegalArgumentException {
	switch (infoType) {
	case RMI_TYPE:
	    return new ClassLogger(ClassLogger.LOGGER_LEGACY_RMI,dbgTag);
	case HTTP_TYPE:
	    return new ClassLogger(ClassLogger.LOGGER_LEGACY_HTTP,dbgTag);
	case HTML_TYPE:
	    return new ClassLogger(ClassLogger.LOGGER_ADAPTOR_HTML,dbgTag);
        case SNMP_TYPE:
	    return new ClassLogger(ClassLogger.LOGGER_ADAPTOR_SNMP,dbgTag);
        case HTTPS_TYPE:
	    return new ClassLogger(ClassLogger.LOGGER_LEGACY_HTTPS,dbgTag);
        default:
            throw new IllegalArgumentException("Invalid connector Type") ;
        }
    }
    
    /**
     * Starts this <CODE>CommunicatorServer</CODE>.
     * <p>
     * Has no effect if this <CODE>CommunicatorServer</CODE> is 
     * <CODE>ONLINE</CODE> or <CODE>STOPPING</CODE>.
     */
    public void start() {
	boolean start;
	
	synchronized (stateLock) {
	    if (state == STOPPING) { 
		// Fix for bug 4352451: 
		// "java.net.BindException: Address in use".
		waitState(OFFLINE, 60000);
	    }
	    start = (state == OFFLINE);
	    if (start) {
		changeState(STARTING);
		stopRequested = false;
		interrupted = false;
	    }
	}
	
	if (!start) {
            if (logger.finerOn())
                logger.finer("start","Connector is not OFFLINE") ;
	    return;
	}
	
	if (logger.finerOn())
	    logger.finer("start","--> Start connector ") ;
	
	mainThread = new Thread (this, makeThreadName());
	//
	// We increase thread priority !
	// Fix for bug 4289885.
	//
	if (Thread.currentThread().getPriority() < Thread.MAX_PRIORITY) {
	    mainThread.setPriority(Thread.currentThread().getPriority() + 1);
	}
	mainThread.start() ;
    }
    
    /**
     * Stops this <CODE>CommunicatorServer</CODE>.
     * <p> 
     * Has no effect if this <CODE>CommunicatorServer</CODE> is 
     * <CODE>OFFLINE</CODE> or <CODE>STOPPING</CODE>.
     */
    public void stop() {
	synchronized (stateLock) {
	    if (state == OFFLINE || state == STOPPING) {
		if (logger.finerOn())
		    logger.finer("stop","Connector is not ONLINE") ;
		return;
	    }
	    changeState(STOPPING);
	    //
	    // Stop the connector thread
	    //
	    if (logger.finerOn())
		logger.finer("stop","Interrupt main thread") ;
	    stopRequested = true ;
	    if (!interrupted) {		    
		interrupted = true;
		mainThread.interrupt();
	    }
	}
	
	//
	// Call terminate on each active client handler
	//
	if (logger.finerOn()) {
	    logger.finer("stop","terminateAllClient") ;
	}
	terminateAllClient() ;
	
	// ----------------------
	// changeState
	// ----------------------
	if (state == STARTING)
	    changeState(OFFLINE);
    }
    
    /**
     * Tests whether the <CODE>CommunicatorServer</CODE> is active.
     *
     * @return True if connector is <CODE>ONLINE</CODE>; false otherwise.
     */
    public boolean isActive() {
        return (state == ONLINE);
    }
    
    /**
     * <p>Waits until either the State attribute of this MBean equals the 
     * specified <VAR>wantedState</VAR> parameter, or the specified 
     * <VAR>timeout</VAR> has elapsed. The method <CODE>waitState</CODE> 
     * returns with a boolean value indicating whether the specified 
     * <VAR>wantedState</VAR> parameter equals the value of this MBean's 
     * State  attribute at the time the method terminates.</p>
     *
     * <p>Two special cases for the <VAR>timeout</VAR> parameter value are:</p>
     * <UL><LI> if <VAR>timeout</VAR> is negative then <CODE>waitState</CODE> 
     *     returns immediately (i.e. does not wait at all),</LI>
     * <LI> if <VAR>timeout</VAR> equals zero then <CODE>waitState</CODE> 
     *      waits until the value of this MBean's State attribute 
     *      is the same as the <VAR>wantedState</VAR> parameter (i.e. will 
     *      wait indefinitely if this condition is never met).</LI></UL>
     * 
     * @param wantedState The value of this MBean's State attribute to wait 
     *        for. <VAR>wantedState</VAR> can be one of:
     * <CODE>CommunicatorServer.OFFLINE</CODE>, 
     * <CODE>CommunicatorServer.ONLINE</CODE>,
     * <CODE>CommunicatorServer.STARTING</CODE>, 
     * <CODE>CommunicatorServer.STOPPING</CODE>.
     * @param timeout The maximum time to wait for, in milliseconds, if
     *        positive. Infinite time out if 0, or no waiting at all if 
     *        negative.
     *
     * @return <code>true</code> if the value of this MBean's State attribute
     *         is the same as the <VAR>wantedState</VAR> parameter; 
     *         <code>false</code> otherwise.
     */
    public boolean waitState(int wantedState, long timeout) {
        if (logger.finerOn())
            logger.finer("waitState", wantedState + "(0on,1off,2st) TO=" + 
			 timeout + " ; current state = " + getStateString());
	
	long endTime = 0;
	if (timeout > 0)
	    endTime = System.currentTimeMillis() + timeout;
	
	synchronized (stateLock) {
	    while (state != wantedState) {
		if (timeout < 0) {
		    if (logger.finerOn())
			logger.finer("waitState", 
				     "timeout < 0, return without wait");
		    return false;
		} else {
		    try {
			if (timeout > 0) {
			    long toWait = endTime-System.currentTimeMillis();
			    if (toWait <= 0) {
				if (logger.finerOn())
				    logger.finer("waitState", "timed out");
				return false;
			    }
			    stateLock.wait(toWait);
			} else {  // timeout == 0
			    stateLock.wait();
			}
		    } catch (InterruptedException e) {
			if (logger.finerOn())
			    logger.finer("waitState", "wait interrupted");
			return (state == wantedState);
		    }
		}
	    }
	    if (logger.finerOn())
		logger.finer("waitState", "returning in desired state");
	    return true;
	}
    }
    
    /**
     * Gets the state of this <CODE>CommunicatorServer</CODE> as an integer.
     *
     * @return <CODE>ONLINE</CODE>, <CODE>OFFLINE</CODE>, 
     *         <CODE>STARTING</CODE> or <CODE>STOPPING</CODE>.
     */
    public int getState() {
        return state ;
    }
    
    /**
     * Gets the state of this <CODE>CommunicatorServer</CODE> as a string.
     *
     * @return One of the strings "ONLINE", "OFFLINE", "STARTING" or 
     *         "STOPPING".
     */
    public String getStateString() {
        return getStringForState(state) ;
    }
    
    /**
     * Gets the host name used by this <CODE>CommunicatorServer</CODE>.
     *
     * @return The host name used by this <CODE>CommunicatorServer</CODE>.
     */
    public String getHost() {
        try {
	    host = System.getProperty("jdmk.hostname");
	    if (host == null)
		host = InetAddress.getLocalHost().getHostName();
	} catch (Exception e) {
	    host = "Unknown host";
	}
	return host ;
    }
    
    /**
     * Gets the port number used by this <CODE>CommunicatorServer</CODE>.
     *
     * @return The port number used by this <CODE>CommunicatorServer</CODE>.
     */
    public int getPort() {
        return port ;
    }
    
    /**
     * Sets the port number used by this <CODE>CommunicatorServer</CODE>.
     *
     * @param port The port number used by this <CODE>CommunicatorServer</CODE>.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the communicator was ONLINE or STARTING.
     */
    public void setPort(int port) throws java.lang.IllegalStateException {
	synchronized (stateLock) {
	    if ((state == ONLINE) || (state == STARTING))
		throw new IllegalStateException("Stop server before " +
					    "carrying out this operation");
	    this.port = port;
	    dbgTag = makeDebugTag();
	}
    }
    
    /**
     * Gets the protocol being used by this <CODE>CommunicatorServer</CODE>.
     * @return The protocol as a string.
     */
    public abstract String getProtocol() ;

    /**
     * Gets the number of clients that have been processed by this 
     * <CODE>CommunicatorServer</CODE> since its creation.
     *
     * @return The number of clients handled by this 
     *         <CODE>CommunicatorServer</CODE> since its creation. 
     *         This counter is not reset by the <CODE>stop</CODE> method.
     */
    int getServedClientCount() {
        return servedClientCount ;
    }
    
    /**
     * Gets the number of clients currently being processed by this 
     * <CODE>CommunicatorServer</CODE>.
     *
     * @return The number of clients currently being processed by this 
     *         <CODE>CommunicatorServer</CODE>.
     */
    int getActiveClientCount() {
        int result = clientHandlerVector.size() ;
        return result ;
    }
    
    /**
     * Gets the maximum number of clients that this 
     * <CODE>CommunicatorServer</CODE> can  process concurrently.
     *
     * @return The maximum number of clients that this 
     *         <CODE>CommunicatorServer</CODE> can process concurrently.
     */
    int getMaxActiveClientCount() {
        return maxActiveClientCount ;
    }
    
    /**
     * Sets the maximum number of clients this 
     * <CODE>CommunicatorServer</CODE> can process concurrently.
     *
     * @param c The number of clients.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the communicator was ONLINE or STARTING.
     */
    void setMaxActiveClientCount(int c) 
	throws java.lang.IllegalStateException {
        if ((state == ONLINE) || (state == STARTING)) {
            throw new IllegalStateException(
		  "Stop server before carrying out this operation");
        }
        maxActiveClientCount = c ;
    }
    
    /**
     * For Java DMK internal use only.
     */
    void notifyClientHandlerCreated(ClientHandler h) {
        clientHandlerVector.addElement(h) ;
    }
    
    /**
     * For Java DMK internal use only.
     */
    synchronized void notifyClientHandlerDeleted(ClientHandler h) {
        clientHandlerVector.removeElement(h);
	notifyAll();
    }

    /**
     * For Java DMK internal use only.
     * <p>
     * The <CODE>run</CODE> method executed by this connector's main thread.
     */
    public void run() {
        
        // Fix jaw.00667.B
        // It seems that the init of "i" and "success"
        // need to be done outside the "try" clause...
        // A bug in Java 2 production release ?
        //
        int i = 0;
        boolean success = false;
        
        // ----------------------
        // Bind 
        // ----------------------
        try {
            // Suspend father thread
            // fatherThread.suspend() ;
	    
            // Fix for bug 4352451: "java.net.BindException: Address in use".
            //
            while (i < 50 && !success) {
                try {
                    // Try socket connection.
                    //
                    doBind();
                    success = true;
                } catch (CommunicationException ce) {
                    i++;
                    try {
                        Thread.sleep(100);
                    } catch (InterruptedException ie) {
			throw ie;
                    }
                }
            }
            // Retry last time to get correct exception.
            //
            if (!success) {
                // Try socket connection.
                //
                doBind();
            }
	    
            // Resume father thread
            // fatherThread.resume() ;
        }
        catch(Exception x) {
            if (logger.finestOn()) {
                logger.finest("run","Unexpected exception = "+x) ;
            }
            changeState(OFFLINE);
            if (logger.finerOn()) {
                logger.finer("run","State is OFFLINE") ;
            }
	    doError(x);
	    return;
        }
	
        java.lang.ThreadDeath  threadDeath  =  null  ;
        try {
            // ----------------------
            // State change
            // ----------------------
            changeState(ONLINE) ;
            if (logger.finerOn()) {
                logger.finer("run","State is ONLINE") ;
            }
	    
            // ----------------------
            // Main loop
            // ----------------------
            while (!stopRequested) {
                servedClientCount++;	
                doReceive() ;
                waitIfTooManyClients() ;
                doProcess() ;
            }
            if (logger.finerOn()) {
                logger.finer("run","Stop has been requested") ;
            }
	    
        }
        catch(InterruptedException x) {
            if (logger.finerOn()) {
                logger.finer("run","Interrupt caught") ;
            }
            changeState(STOPPING);
        }
        catch(Exception x) {
            if (logger.finestOn()) {
                logger.finest("run","Unexpected exception = "+x) ;
            }
            changeState(STOPPING);
        }
        catch(java.lang.ThreadDeath x) {
            if (logger.finestOn()) {
                logger.finest("run","ThreadDeath caught") ;
            }
            threadDeath =  x ;
            changeState(STOPPING);
        }
	finally {
	    synchronized (stateLock) {
		interrupted = true;
		Thread.currentThread().interrupted();
	    }

	    // ----------------------
	    // unBind
	    // ----------------------
	    try {
		doUnbind() ;
		waitClientTermination() ;
		changeState(OFFLINE);
		if (logger.finerOn()) {
		    logger.finer("run","State is OFFLINE") ;
		}
	    }
	    catch(Exception x) {
		if (logger.finestOn()) {
		    logger.finest("run","Unexpected exception = "+x) ;
		}
		changeState(OFFLINE);
	    }
	    
	    // ----------------------
	    // Return part
	    // ----------------------
	    if ( threadDeath !=  null ) {
		throw threadDeath ;
	    }
	}
    }

    /**
     */
    protected abstract void doError(Exception e) 
	throws CommunicationException;

    //
    // To be defined by the subclass.
    //
    // Each method below is called by run() and must be subclassed.
    // If the method sends an exception (Communication or Interrupt), this
    // will end up the run() method and switch the connector offline.
    //
    // If it is a CommunicationException, run() will call 
    // Debug.printException().
    //
    // All these methods should propagate the InterruptedException to inform
    // run() that the connector must be switch OFFLINE.
    //
    //
    //
    // doBind() should do all what is needed before calling doReceive().
    // If doBind() throws an exception, doUnbind() is not to be called and 
    // run() ends up.
    //

    /**
     */
    protected abstract void doBind() 
	throws CommunicationException, InterruptedException ;

    /**
     * <CODE>doReceive()</CODE> should block until a client is available.
     * If this method throws an exception, <CODE>doProcess()</CODE> is not 
     * called but <CODE>doUnbind()</CODE> is called then <CODE>run()</CODE> 
     * stops.
     */
    protected abstract void doReceive() 
	throws CommunicationException, InterruptedException ;

    /**
     * <CODE>doProcess()</CODE> is called after <CODE>doReceive()</CODE>: 
     * it should process the requests of the incoming client.
     * If it throws an exception, <CODE>doUnbind()</CODE> is called and 
     * <CODE>run()</CODE> stops.
     */
    protected abstract void doProcess() 
	throws CommunicationException, InterruptedException ;

    /**
     * <CODE>doUnbind()</CODE> is called whenever the connector goes 
     * <CODE>OFFLINE</CODE>, except if <CODE>doBind()</CODE> has 
     * thrown an exception.
     */
    protected abstract void doUnbind() 
	throws CommunicationException, InterruptedException ;
    
    /**
     * Get the <code>MBeanServer</code> object to which incoming requests are
     * sent.  This is either the MBean server in which this connector is
     * registered, or an <code>MBeanServerForwarder</code> leading to that
     * server.
     */
    public synchronized MBeanServer getMBeanServer() {
        return topMBS;
    }
    
    /**
     * Set the <code>MBeanServer</code> object to which incoming
     * requests are sent.  This must be either the MBean server in
     * which this connector is registered, or an
     * <code>MBeanServerForwarder</code> leading to that server.  An
     * <code>MBeanServerForwarder</code> <code>mbsf</code> leads to an
     * MBean server <code>mbs</code> if
     * <code>mbsf.getMBeanServer()</code> is either <code>mbs</code>
     * or an <code>MBeanServerForwarder</code> leading to
     * <code>mbs</code>.
     *
     * @exception IllegalArgumentException if <code>newMBS</code> is neither
     * the MBean server in which this connector is registered nor an
     * <code>MBeanServerForwarder</code> leading to that server.
     *
     * @exception IllegalStateException This method has been invoked
     * while the communicator was ONLINE or STARTING.
     */
    public synchronized void setMBeanServer(MBeanServer newMBS)
	throws IllegalArgumentException, IllegalStateException {
	synchronized (stateLock) {
	    if (state == ONLINE || state == STARTING)
		throw new IllegalStateException("Stop server before " +
					"carrying out this operation");
	}
	final String error =
	    "MBeanServer argument must be MBean server where this " +
	    "server is registered, or an MBeanServerForwarder " +
	    "leading to that server";
	Vector seenMBS = new Vector();
	for (MBeanServer mbs = newMBS;
	     mbs != bottomMBS;
	     mbs = ((MBeanServerForwarder) mbs).getMBeanServer()) {
	    if (!(mbs instanceof MBeanServerForwarder))
		throw new IllegalArgumentException(error);
	    if (seenMBS.contains(mbs))
		throw new IllegalArgumentException("MBeanServerForwarder " +
						   "loop");
	    seenMBS.addElement(mbs);
	}
	topMBS = newMBS;
    }

    //
    // To be called by the subclass if needed
    //
    /**
     * For JDMK internal use only.
     */
    ObjectName getObjectName() {
        return objectName ;
    }

    /**
     * For JDMK internal use only.
     */
    void changeState(int newState) {
	int oldState;
	synchronized (stateLock) {
	    if (state == newState)
		return;
	    oldState = state;
	    state = newState;
	    stateLock.notifyAll();		
	    sendStateChangeNotification(oldState, newState);
	}
    }
    
    /**
     * Returns the string used in logging.
     */
    String makeDebugTag() {
        return "CommunicatorServer["+ getProtocol() + ":" + getPort() + "]" ;
    }

    /**
     * Returns the string used to name the connector thread.
     */
    String makeThreadName() {
        String result ;

        if (objectName == null)
            result = "CommunicatorServer" ;
        else
            result = objectName.toString() ;
        
        return result ;
    }
  
    /**
     * This method blocks if there are too many active clients.
     * Call to <CODE>wait()</CODE> is terminated when a client handler 
     * thread calls <CODE>notifyClientHandlerDeleted(this)</CODE> ;
     */
    private synchronized void waitIfTooManyClients() 
	throws InterruptedException {
        while (getActiveClientCount() >= maxActiveClientCount) {
            if (logger.finerOn()) {
                logger.finer("waitIfTooManyClients",
			     "Waiting for a client to terminate") ;
            }
            wait();
        }
    }

    /**
     * This method blocks until there is no more active client.
     */
    private void waitClientTermination() {
        int s = clientHandlerVector.size() ;
        if (logger.finerOn()) {        
            if (s >= 1) {
                logger.finer("waitClientTermination","waiting for " +
                      s + " clients to terminate") ;
            }
        }

        for (Enumeration e = clientHandlerVector.elements() ; 
	     e.hasMoreElements();){
            ClientHandler h = (ClientHandler)e.nextElement() ;
            h.join() ;
        }
	
        if (logger.finerOn()) {  
            if (s >= 1) {
                logger.finer("waitClientTermination","Ok, let's go...") ;
            }
        }
    }
  
    /**
     * Call <CODE>interrupt()</CODE> on each pending client.
     */
    private void terminateAllClient() {
        int s = clientHandlerVector.size() ;
        if (logger.finerOn()) {
            if (s >= 1) {
                logger.finer("terminateAllClient","Interrupting " + s + 
			     " clients") ;
            }
        }
    
        for (Enumeration e = clientHandlerVector.elements() ; 
	     e.hasMoreElements();){
            ClientHandler h = (ClientHandler)e.nextElement() ;
            h.interrupt() ;
        }
    }

    /**
     * Controls the way the CommunicatorServer service is deserialized.
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException {
      
        // Call the default deserialization of the object.
        //
        stream.defaultReadObject();
      
        // Call the specific initialization for the CommunicatorServer 
	// service.
        // This is for transient structures to be initialized to specific 
	// default values.
        //
	stateLock = new Object();
	state = OFFLINE;
        stopRequested = false;
        servedClientCount = 0;
        clientHandlerVector = new Vector();
	fatherThread = Thread.currentThread();
	mainThread = null;
	notifCount = 0;
	notifInfos = null;
	notifBroadcaster = new NotificationBroadcasterSupport();
	dbgTag = makeDebugTag();
	logger = makeLogger(dbgTag);
    }
  

    //
    // NotificationBroadcaster
    //

    /**
     * Adds a listener for the notifications emitted by this 
     * CommunicatorServer.
     * There is only one type of notifications sent by the 
     * CommunicatorServer:
     * they are <tt>{@link javax.management.AttributeChangeNotification}</tt>,
     * sent when the <tt>State</tt> attribute of this CommunicatorServer 
     * changes.
     *
     * @param listener The listener object which will handle the emitted 
     *        notifications.
     * @param filter The filter object. If filter is null, no filtering will 
     *        be performed before handling notifications.
     * @param handback An object which will be sent back unchanged to the 
     *        listener when a notification is emitted.
     *
     * @exception IllegalArgumentException Listener parameter is null.
     */
    public void addNotificationListener(NotificationListener listener, 
					NotificationFilter filter, 
					Object handback)
        throws java.lang.IllegalArgumentException {

	if (logger.finestOn()) {
	    logger.finest("addNotificationListener","Adding listener "+ 
			  listener +" with filter "+ filter + 
			  " and handback "+ handback);
	}
	notifBroadcaster.addNotificationListener(listener, filter, handback); 
    }
    
    /**
     * Removes the specified listener from this CommunicatorServer. 
     * Note that if the listener has been registered with different
     * handback objects or notification filters, all entries corresponding 
     * to the listener will be removed.
     *
     * @param listener The listener object to be removed.
     *
     * @exception ListenerNotFoundException The listener is not registered.
     */
    public void removeNotificationListener(NotificationListener listener) 
        throws ListenerNotFoundException {

	if (logger.finestOn()) {
	    logger.finest("removeNotificationListener","Removing listener "+
			  listener);
	}
	notifBroadcaster.removeNotificationListener(listener); 
    }
    
    /**
     * Returns an array of MBeanNotificationInfo objects describing the 
     * notification types sent by this CommunicatorServer. 
     * There is only one type of notifications sent by the 
     * CommunicatorServer: 
     * it is <tt>{@link javax.management.AttributeChangeNotification}</tt>,  
     * sent when the <tt>State</tt> attribute of this CommunicatorServer 
     * changes.
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
	
	// Initialize notifInfos on first call to getNotificationInfo()
	//
	if (notifInfos == null) {
	    notifInfos = new MBeanNotificationInfo[1];
	    String[] notifTypes = {
		AttributeChangeNotification.ATTRIBUTE_CHANGE};
	    notifInfos[0] = new MBeanNotificationInfo( notifTypes,
		AttributeChangeNotification.class.getName(),
	        "Sent to notify that the value of the State attribute "+
	        "of this CommunicatorServer instance has changed.");
	}

	return notifInfos;
    }
    
    /**
     *
     */
    private void sendStateChangeNotification(int oldState, int newState) {

	String oldStateString = getStringForState(oldState);
	String newStateString = getStringForState(newState);
	String message = new StringBuffer().append(dbgTag)
	    .append(" The value of attribute State has changed from ")
	    .append(oldState).append(" (").append(oldStateString)
	    .append(") to ").append(newState).append(" (")
	    .append(newStateString).append(").").toString(); 

	notifCount++;
	AttributeChangeNotification notif =
	    new AttributeChangeNotification(this,	  // source
					    notifCount,	  // sequence number
					    System.currentTimeMillis(), 
					                  // time stamp
					    message,	  // message
					    "State",	  // attribute name
					    "int",	  // attribute type
					    new Integer(oldState), 
					                  // old value
					    new Integer(newState) );	
	                                                  // new value
	
	if (logger.finestOn()) {
	    logger.finest("sendStateChangeNotification",
			  "Sending AttributeChangeNotification #"+ 
			  notifCount +" with message: "+ message);	    
	}
	notifBroadcaster.sendNotification(notif);
    }

    /**
     *
     */
    private static String getStringForState(int s) {
        switch (s) {
        case ONLINE:   return "ONLINE";
        case STARTING: return "STARTING";
        case OFFLINE:  return "OFFLINE";
        case STOPPING: return "STOPPING";
	default:       return "UNDEFINED";
        }
    }


    //
    // MBeanRegistration
    //

    /**
     * Preregister method of connector.
     *
     *@param server The <CODE>MBeanServer</CODE> in which the MBean will be 
     *       registered.
     *@param name The object name of the MBean.
     *
     *@return  The name of the MBean registered.
     *
     *@exception Exception This exception should be caught by 
     *           the <CODE>MBeanServer</CODE> and re-thrown
     *as an <CODE>MBeanRegistrationException</CODE>.
     */
    public ObjectName preRegister(MBeanServer server, ObjectName name)
	    throws java.lang.Exception {
        objectName = name;
	synchronized (this) {
	    if (bottomMBS != null) {
		throw new IllegalArgumentException("connector already " +
						   "registered in an MBean " +
						   "server");
	    }
	    topMBS = bottomMBS = server;
	}
        dbgTag = makeDebugTag(); 
        return name;
    }

    /**
     *
     *@param registrationDone Indicates whether or not the MBean has been 
     *       successfully registered in the <CODE>MBeanServer</CODE>. 
     *       The value false means that the registration phase has failed.
     */
    public void postRegister(Boolean registrationDone) {
	if (!registrationDone.booleanValue()) {
	    synchronized (this) {
		topMBS = bottomMBS = null;
	    }
	}
    } 
    
    /**
     * Stop the connector.
     *
     *@exception Exception This exception should be caught by 
     *           the <CODE>MBeanServer</CODE> and re-thrown
     *           as an <CODE>MBeanRegistrationException</CODE>.
     */
    public void preDeregister() throws java.lang.Exception {
	synchronized (this) {
	    topMBS = bottomMBS = null;
	}
        objectName = null ;
        if ((state == ONLINE) || ( state == STARTING)) {
            stop() ;
        }
    }

    /**
     * Do nothing.
     */
    public void postDeregister(){
    }
    
    int infoType;
}
