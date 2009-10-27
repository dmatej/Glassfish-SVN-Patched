/*
 * @(#)file      CascadingService.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.10
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
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
 */

package com.sun.jdmk.remote.cascading;

import javax.management.*;
import javax.management.remote.*;

import java.util.*;
import java.io.*;

import com.sun.jdmk.remote.cascading.proxy.ProxyCascadingAgent;
import com.sun.jdmk.defaults.Utils;

/**
 * The <tt>CascadingServiceMBean</tt> is a high level service MBean that
 * makes it possible to remotely configure <tt>CascadingAgents</tt>.
 * This MBean makes it possible to <i>mount</i> a partial view of 
 * a <i>source MBeanServer</i> known by its <tt>JMXServiceURL</tt> into
 * the <i>target MBeanServer</i> of this <tt>CascadingServiceMBean</tt>.
 * <p>The target <tt>MBeanServer</tt> of a <tt>CascadingServiceMBean</tt> is
 * usually the <tt>MBeanServer</tt> in which that 
 * <tt>CascadingServiceMBean</tt> is registered.
 * It is recommended to instantiate at most one 
 * <tt>CascadingServiceMBean</tt> per target MBeanServer. If no 
 * <tt>ObjectName</tt> is specified when registering the 
 * <tt>CascadingServiceMBean</tt>, then
 * the <tt>CascadingServiceMBean</tt> will supply its own default name:
 * <tt>{@link #CASCADING_SERVICE_DEFAULT_NAME 
 * CASCADING_SERVICE_DEFAULT_NAME}=new
 * ObjectName("com.sun.jdmk:type=CascadingService")</tt>.
 * </p>
 * <p>
 * If the JMX Connection with a source MBeanServer fails or is permanently
 * closed, the  <tt>CascadingServiceMBean</tt> will emit a 
 * {@link JMXConnectionNotification} with the following parameters: 
 * <ul>
 * <li>The <var>type</var> is <tt>{@link #CASCADING_FAILED_NOTIFICATION 
 *     "com.sun.jdmk.remote.cascading.failed"}</tt></li>
 * <li>The <var>source</var> is the <tt>CascadingServiceMBean</tt>.</tt></li>
 * <li>The <var>connectionId</var> is the <var>mountPointID</var> 
 *     that was returned by the the {@link #mount mount} method</li>
 * <li>The <var>userData</var> is the source 
 *     <tt>JMXConnectionNotification</tt></li>
 * </ul>
 * </p>
 * <p>
 * If the cascading is unmounted ({@link #unmount unmount} was called) then
 * the <tt>CascadingServiceMBean</tt> will emit a 
 * {@link JMXConnectionNotification} with the following parameters: 
 * <ul>
 * <li>The <var>type</var> is <tt>{@link #CASCADING_STOPPED_NOTIFICATION 
 *     "com.sun.jdmk.remote.cascading.stopped"}</tt></li>
 * <li>The <var>source</var> is the <tt>CascadingServiceMBean</tt>.</tt></li>
 * <li>The <var>connectionId</var> is the <var>mountPointID</var> 
 *     that was returned by the the {@link #mount mount} method</li>
 * <li>The <var>userData</var> is either <tt>null</tt>, or the exception 
 *     thrown by {@link #unmount unmount}</li>
 * </ul>
 * </p>
 * <p><b>Note:</b> In this implementation, the {@link #mount mount} 
 *    operation does not perform any checks with regards to the coherency
 *    of the supplied <var>targetPath</var>. It is under the responsibility of
 *    the application to conform to the rules documented in {@link
 *    com.sun.jdmk.remote.cascading#The_File_System_Analogy 
 *    The File System Analogy}
 * </p>
 *
 * In this implementation, the CascadingService relies on the 
 * {@link ProxyCascadingAgent}.
 *
 * @since Java DMK 5.1
 **/
// Note: To make this class extensible it is possible to change the
//       protection of createCascadingAgent, connectSource, and 
//       terminate from "package" to "protected"
//
public class CascadingService implements CascadingServiceMBean,
					 NotificationEmitter,
					 MBeanRegistration {

    static String makeID(JMXServiceURL sourceURL,
		  ObjectName sourcePattern, 
		  String targetPath) {
	final String url = 
	    (sourceURL==null)?"???":String.valueOf(sourceURL);
	final String filter = 
	    (sourcePattern==null)?"*:*":String.valueOf(sourcePattern);
	return "mount: " + url + " " + sourcePattern + " " + targetPath;
    }

    class MountPoint {

	public final JMXServiceURL           sourceURL;
	public final ObjectName              sourcePattern;
	public final String                  targetPath;
	public final String                  mountPointID;
	
	private JMXConnector                 sourceConnector=null;
	private MBeanServerConnectionFactory sourceConnectionFactory=null;
	private CascadingAgent               agent=null;
	
	
	public MountPoint(JMXServiceURL sourceURL,
			  ObjectName sourcePattern, 
			  String targetPath) 
	    throws IOException {
	    this.mountPointID  = makeID(sourceURL,sourcePattern,targetPath);
	    this.sourceURL     = sourceURL;
	    this.sourcePattern = sourcePattern;
	    this.targetPath    = targetPath;
	}

	// Must be called from a synchronized block on the containing
	// CascadingService object.
	//
	public void mount(JMXConnector sourceConnector, MBeanServer targetMBS) 
	    throws IOException,
		   InstanceAlreadyExistsException {
	    this.sourceConnector = sourceConnector;
	    agent = createCascadingAgent(sourceConnector,
					 sourcePattern,
					 targetPath,
					 targetMBS,
					 mountPointID);
	    sourceConnectionFactory = agent.getConnectionFactory();
	    agent.start(false);
	    sourceConnectionFactory.
		addConnectionNotificationListener(listener,null,this);
	    checkConnection();
	}

	// Must be called from a synchronized block on the containing
	// CascadingService object.
	//
	public void unmount() 
	    throws IOException {
	    Exception failure=null;

	    try {
		sourceConnectionFactory.
		    removeConnectionNotificationListener(listener,null,this);
	    } catch (Exception x) {
		failure = x;
		// OK let's proceed anyway...
	    }
	    
	    try {
		terminate(agent,sourceConnector,mountPointID);
	    } catch (Exception x) {
		failure = x;
	    }
	
	    // Ugly...
	    //
	    if (failure instanceof IOException) 
		throw (IOException) failure;
	    if (failure instanceof RuntimeException) 
		throw (RuntimeException) failure;
	    if (failure != null) {
		final IOException io = 
		    new IOException(mountPointID+": "+failure);
		Utils.initCause(io,failure);
		throw io;
	    }
	}

	// Must be called from a synchronized block on the containing
	// CascadingService object.
	//
	public boolean isClosed() {
	    if (agent != null) {
		try {
		    agent.getConnectionFactory().
			getMBeanServerConnection().getDefaultDomain();
		    // System.err.println("CascadingService: not closed");
		    return false;
		} catch (Exception x) {
		    // System.err.println("CascadingService: closed");
		    return true;
		}
	    }
	    
	    // System.err.println("CascadingService: bizarely closed");
	    return true;
	}

	// Must be called from a synchronized block on the containing
	// CascadingService object.
	//
	public void checkConnection() throws IOException {
	    if (sourceConnector == null) 
		throw new IOException("not connected");
	    if (sourceConnectionFactory == null)
		throw new IOException("not connected");
	    final MBeanServerConnection mbs = 
		sourceConnectionFactory.getMBeanServerConnection();
	    if (mbs == null)
		throw new IOException("not connected");
	    mbs.getDefaultDomain();
	}
    }

    /**
     * Instantiate a new <tt>CascadingService</tt>. The target 
     * <tt>MBeanServer</tt> for this service will be the <tt>MBeanServer</tt>
     * in which this object is registered.
     * 
     **/
    public CascadingService() {
	this(null);
    }

    /**
     * Instantiate a new <tt>CascadingService</tt> for the specified 
     * target <tt>MBeanServer</tt>. The target 
     * <tt>MBeanServer</tt> for this service will be the specified 
     * <var>targetMBS</var>.
     * @param targetMBS The <i>target MBeanServer</i> for this cascading
     *        service.
     **/
    public CascadingService(MBeanServer targetMBS) {
	this.targetMBS = targetMBS;
	this.listener = new NotificationListener() {
	     public void handleNotification(Notification nt,Object handback) {
		 handleJMXCN(nt,handback);
	     };
	    };
	this.emitter = new NotificationBroadcasterSupport();
	this.mountMap = new HashMap();
    }

    // from CascadingServiceMBean
    //
    public final synchronized String mount(JMXServiceURL sourceURL, 
					   Map sourceMap,
					   ObjectName sourcePattern,
					   String targetPath) 
	throws IOException, InstanceAlreadyExistsException {
	
	final MountPoint mpt = 
	    new MountPoint(sourceURL,sourcePattern,targetPath);
	
	if (isMounted(mpt.mountPointID)) 
	    throw new IOException(mpt.mountPointID +": already mounted.");
 
	final JMXConnector sourceConnector = 
	    connectSource(sourceURL,sourceMap,mpt.mountPointID);
	
	try {

	    mountMap.put(mpt.mountPointID,mpt);
	    mpt.mount(sourceConnector,getTargetMBeanServer());
	    return mpt.mountPointID;

	} catch (Exception x) {

	    try {

		// This will close the sourceConnector if needed.
		//
		mountMap.remove(mpt.mountPointID);
		mpt.unmount();

	    } catch (Exception xx) {
		// OK: drop it...
	    }

	    // This is ugly...
	    //
	    if (x instanceof IOException) 
		throw (IOException) x;
	    if (x instanceof InstanceAlreadyExistsException) 
		throw (InstanceAlreadyExistsException)x;
	    if (x instanceof RuntimeException) 
		throw (RuntimeException)x;
	    IOException io = new IOException(mpt.mountPointID+": "+x);
	    Utils.initCause(io,x);
	    throw io;
	}
    }

    // from CascadingServiceMBean
    //
    public final boolean unmount(String mountPointID) 
	throws IOException {
	MountPoint mpt     = null;
	Exception  failure = null;
	try {
	    synchronized (this) {
		mpt = (MountPoint) mountMap.remove(mountPointID);
		if (mpt == null) return false;
		
		mpt.unmount();
	    }
	} catch (IOException x) {
	    failure = x; throw x;
	} catch (RuntimeException r) {
	    failure = r; throw r;
	} finally {
	    if (mpt != null) {
		// send notif...
		//
		final String type = CASCADING_STOPPED_NOTIFICATION;
		final String message = 
		    (failure==null)?": succesfully unmounted":
		    (": unmounted with errors: " + failure);
		JMXConnectionNotification stopped = 
		    new JMXConnectionNotification(type,this,mpt.mountPointID,
						  newSequenceNumber(),
						  mpt.mountPointID + 
						  message,
						  null);
		sendNotification(stopped);
	    }
	}
	return true;
    }

    // from CascadingServiceMBean
    //
    public synchronized boolean isMounted(String mountPointID) {
	return mountMap.containsKey(mountPointID);
    }

    // from CascadingServiceMBean
    //
    public synchronized String[] getMountPointIDs() {
	return (String[])
	    mountMap.keySet().toArray(new String[mountMap.size()]);
    }
    
    // from NotificationEmitter
    //
    public final void addNotificationListener(NotificationListener listener,
					      NotificationFilter filter,
					      Object handback)
	throws java.lang.IllegalArgumentException {
	emitter.addNotificationListener(listener,filter,handback);
    }
    
    // from NotificationEmitter
    //
    public final void removeNotificationListener(NotificationListener listener)
 	throws ListenerNotFoundException {
	emitter.removeNotificationListener(listener);
    }
    
     
    // from NotificationEmitter
    //
    public final void removeNotificationListener(NotificationListener listener,
					   NotificationFilter filter,
					   Object handback)
	throws ListenerNotFoundException {
	emitter.removeNotificationListener(listener,filter,handback);
    }


    // from NotificationEmitter
    //
    public MBeanNotificationInfo[] getNotificationInfo() {
	final MBeanNotificationInfo[] info = {
	    jmxConnectionNotificationInfo
	};
	return info;
    }

    /**
     * The <i>target MBeanServer</i> in which the source 
     * MBeans will be mounted under the <var>target path</var>.
     * @return the target <tt>MBeanServer</tt>.
     **/
    public final MBeanServer getTargetMBeanServer() {
	return (targetMBS != null)?targetMBS:myMBS;
    }

    /**
     * Allows the MBean to perform any operations it needs before
     * being registered in the MBean server. 
     * If the target MBeanServer supplied at construction time was null,
     * then  <var>server</var> becomes the target <tt>MBeanServer</tt>.
     *
     * @param server The MBean server in which the MBean will be registered.
     *
     * @param name The object name of the MBean. If <var>name</var>
     *    is <tt>null</tt> then {@link
     *    CascadingServiceMBean#CASCADING_SERVICE_DEFAULT_NAME} is returned.
     *
     * @return The given <var>name</var>, or if <tt>null</tt> the default 
     *    name for the <tt>CascadingService</tt>.
     *         
     * 
     * @exception IllegalArgumentException if no target <tt>MBeanServer</tt> 
     *         was specified in the constructor and this object is already 
     *         registered in an <tt>MBeanServer</tt>.
     *
     * @see MBeanRegistration#preRegister
     */
    public ObjectName preRegister(MBeanServer server,
				  ObjectName name) 
	throws java.lang.Exception {
	synchronized (this) {
	    if ((targetMBS == null) && (myMBS != null && myMBS != server))
		throw new IllegalArgumentException("Already registered");
	    myMBS = server;
	}
	if (name != null) return name;
	else return CascadingServiceMBean.CASCADING_SERVICE_DEFAULT_NAME;
    }

    // from MBeanRegistration
    //
    public void postRegister(Boolean registrationDone) {
    }


    // from MBeanRegistration
    //
    public void preDeregister() throws java.lang.Exception {
    }

    // from MBeanRegistration
    //
    public void postDeregister() { 
	myMBS = null;
    }

    // Subclassing Hooks
    // -----------------

    /**
     * Creates a new cascading agent for implementing the {@link #mount 
     * mount} operation. The returned <tt>CascadingAgent</tt> is not
     * expected to be already started: {@link #mount mount} will later 
     * call <tt>start(false)</tt> on this object.
     * <p>
     * By default this method creates a {@link 
     * BasicMBeanServerConnectionFactory} from the <var>sourceConnector</var>
     * and then instantiate a new {@link ProxyCascadingAgent}.
     * 
     * @param sourceConnector A connected <tt>JMXConnector</tt> for 
     *        communicating with the source <tt>MBeanServer</tt>.
     *        This is the <tt>JMXConnector</tt> that was returned by
     *        {@link #connectSource connectSource}.
     *        <p>
     * @param sourcePattern An <tt>ObjectName</tt> pattern that must be 
     *        satisfied by the <tt>ObjectName</tt>s of the source MBeans. 
     *        This is the <var>sourcePattern</var> that was passed to
     *        {@link #mount mount}.
     *        <p>
     * @param targetPath The <i>domain path</i> under which the source
     *        MBeans will be mounted in the target <tt>MBeanServer</tt>.
     *        This is the <var>targetPath</var> that was passed to
     *        {@link #mount mount}.
     *        <p>
     * @param mountPointID The <var>mountPointID</var> identifying the mount 
     *        operation.
     *        This is the <var>mountPointID</var> that
     *        will be returned by the {@link #mount mount} operation.
     *        <p>
     * @return A new <tt>CascadingAgent</tt> implementing the mount operation.
     *         {@link #mount mount} will later call <tt>start(false)</tt> on
     *         this object.
     * @exception IOException If an IOException occurs while creating
     *         the cascading agent.
     **/
    // protected 
    CascadingAgent createCascadingAgent(JMXConnector  sourceConnector,
					ObjectName    sourcePattern,
					String        targetPath,
					MBeanServer   targetMBS,
					String        mountPointID) 
	throws IOException {
	final MBeanServerConnectionFactory sourceConnectionFactory =
	    BasicMBeanServerConnectionFactory.newInstance(sourceConnector);
	return new ProxyCascadingAgent(sourceConnectionFactory,sourcePattern,
				       null,targetPath,targetMBS,mountPointID);
    }

    /**
     * Creates a new connected <tt>JMXConnector</tt> for communicating with the
     * source <tt>MBeanServer</tt>. This is the <var>sourceConnector</var>
     * that will be passed to {@link #createCascadingAgent 
     * createCascadingAgent} and {@link #terminate terminate}.
     * <br>By default this method simply returns 
     * <tt>JMXConnectorFactory.newJMXConnector(sourceURL,sourceMap);</tt>
     * 
     * @param sourceURL A <tt>JMXServiceURL</tt> from which a 
     *        <tt>JMXConnector</tt> to the source <tt>MBeanServer</tt> can 
     *        be obtained.
     *        <p>
     * @param sourceMap A Map object that will be passed to the 
     *        {@link JMXConnectorFactory#connect(JMXServiceURL,Map)}
     *        method, in order to connect to the source <tt>MBeanServer</tt>.
     *        This parameter can be null.
     *        <p>
     * @param mountPointID The <var>mountPointID</var> identifying the mount
     *        point for which this connector is created.
     *        This is the <var>mountPointID</var> that
     *        will be returned by the {@link #mount mount} operation.
     *        <p>
     * @exception IOException if the JMXConnector cannot be connected -
     *        see {@link JMXConnectorFactory#connect 
     *        JMXConnectorFactory.connect}.
     **/
    // protected 
    JMXConnector connectSource(JMXServiceURL sourceURL,
					 Map sourceMap,
					 String mountPointID) 
	throws IOException {
	return JMXConnectorFactory.connect(sourceURL,sourceMap);
    }

    /**
     * Stops the <tt>CascadingAgent</tt> and closes the <tt>JMXConnector</tt>
     * associated to the mount point being unmounted.
     *
     * @param agent The <tt>CascadingAgent</tt> created for this mount point
     *        by {@link #createCascadingAgent createCascadingAgent}.
     *        Can be <tt>null</tt> if <tt>terminate</tt> is called before
     *        the agent was created.
     *        <br>When not <tt>null</tt>, the <tt>terminate</tt> method 
     *        calls <tt>agent.stop()</tt>.
     *        <p>
     * @param sourceConnector The <tt>JMXConnector</tt> created for this 
     *        mount point by {@link #connectSource
     *        connectSource}.
     *        <br>If <var>sourceConnector</var> is not <tt>null</tt>, 
     *        the <tt>terminate</tt> method calls 
     *        <tt>sourceConnector.close()</tt>.
     *        <p>
     * @param mountPointID The <var>mountPointID</var> identifying the mount
     *        point being unmounted. This is the <var>mountPointID</var> that
     *        was returned by the corresponding {@link #mount mount} operation.
     *        <p>
     * @exception IOException If an IOException exception occurs while 
     *        stopping the agent or closing the connector. 
     * <br><b>Note:</b> the <tt>terminate</tt> method should always attempt to 
     *              close the JMXConnector. <tt>terminate</tt> will never be 
     *              called twice for the same mount point.
     **/
    // protected 
    void terminate(CascadingAgent agent,
		   JMXConnector   sourceConnector,
		   String         mountPointID) 
	throws IOException {
	try {
	    if (agent != null) agent.stop();
	} finally {
	    if (sourceConnector != null)
		sourceConnector.close();
	}
    }

    /**
     * Sends a notification.
     *   
     * @param notification The notification to send.
     * @see NotificationBroadcasterSupport#sendNotification
     */
    // protected 
    void sendNotification(Notification notification) {
	emitter.sendNotification(notification);
    }

    /**
     * Increments and returns this object's notification sequence number.
     **/
    // protected 
    final synchronized long newSequenceNumber() {
	return sequenceNumber++;
    }


    private void handleJMXCN(Notification nt, Object handback) {
	final MountPoint mpt = (MountPoint) handback;
	
	// System.err.println("CascadingService: received " + nt.getType());

	if (mpt == null) return;
	
	boolean closed = false;

	synchronized (this) {
	    if ((JMXConnectionNotification.CLOSED.equals(nt.getType()) &&
		 mpt.isClosed()) 
		|| JMXConnectionNotification.FAILED.equals(nt.getType())) {
		try {
		    if (mountMap.remove(mpt.mountPointID) != null) 
			mpt.unmount();
		} catch (Exception x) {
		    // OK, don't be bothered.
		}
		closed = true;
	    }
	}

	if (closed) {
	    //  send notification
	    final String type  = CASCADING_FAILED_NOTIFICATION;
	    final String cause = 
		JMXConnectionNotification.FAILED.equals(nt.getType())?"failed":
		"closed";

	    JMXConnectionNotification failed = 
		new JMXConnectionNotification(type,this,mpt.mountPointID,
					      newSequenceNumber(),
					      mpt.mountPointID + 
					      ": connection " + cause,
					      nt);
	    sendNotification(failed);
	}   
    }


    private static final String[] jmxConnectionNotificationTypes = {
	CASCADING_FAILED_NOTIFICATION,
	CASCADING_STOPPED_NOTIFICATION
    };

    private static final MBeanNotificationInfo jmxConnectionNotificationInfo =
	new MBeanNotificationInfo(jmxConnectionNotificationTypes,
				  JMXConnectionNotification.class.getName(),
				  "Notifications relating to the underlying "+
				  "CascadingAgent.");

    private   final NotificationBroadcasterSupport emitter;
    private   final MBeanServer                    targetMBS;
    private   final NotificationListener           listener;
    private   final HashMap                        mountMap;
    private   MBeanServer myMBS     = null;
    private   long sequenceNumber   = 0;
}
