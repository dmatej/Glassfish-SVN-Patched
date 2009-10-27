/*
 * @(#)file      ServerNotificationDispatcher.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.31
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
 */



package com.sun.jdmk.comm;

// java import
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Set;
import java.util.Iterator;
import java.util.List;
import java.util.Collections;
import java.lang.*;

import java.rmi.Remote;


// jmx import
import javax.management.*;

// jdmk import
import com.sun.jdmk.internal.ClassLogger;



/**
 * This class is used by an agent connector to send notifications to a client connector.
 * <P>This class is never used directly by a user, it is declared as public in order to allow
 * a user specific connector to use this class. A user specific connector only needs to
 * implement the interface NotifConnector, and to pass a remote request from the client connector
 * to this object by calling the method "remoteRequest".
 */
class ServerNotificationDispatcher {
    // public static variables
    //
    public final static int NEW_CLIENT			= 0;
    public final static int REMOTE_TERMINATE		= 1;
    public final static int REGISTER_TO_MBEAN		= 2;
    public final static int UNREGISTER_FROM_MBEAN      	= 3;
    public final static int GET_NOTIFICATIONS		= 4;
    public final static int CLEAR_NOTIFICATIONS		= 5;
    public final static int SET_PUSH_MODE      		= 6;
    public final static int SET_OVERFLOW_MODE		= 7;
    public final static int GET_OVERFLOW_MODE		= 8;
    public final static int SET_CACHE_SIZE     		= 9;
    public final static int GET_CACHE_SIZE     		= 10;
    public final static int SET_OVERFLOW_COUNT		= 11;
    public final static int GET_OVERFLOW_COUNT		= 12;
    public final static int GET_NOTIF_SERVER_VERSION    = 13;
    

    // constructors
    // ------------
    /**
     * Construct a ServerNotificationDispatcher object.
     */
    public ServerNotificationDispatcher(ServerNotificationHandlerInternal connector, MBeanServer server) throws IllegalArgumentException {
	if (connector == null)
	    throw new IllegalArgumentException("It should specify a connector.");

	if (server == null)
	    throw new IllegalArgumentException("It should specify a MBeanServer.");

	this.connector = connector;
	mbServer = server;

	pushNotif = new PushNotification();

	if (logger.finerOn())
	    logger.finer("Constructor", "create a ServerNotificationDispatcher object.");
    }

    // public methods
    // --------------
    /**
     * Be called by a connector to receive a remote operation.
     *
     * @param opType an integer specified by the client.
     * @param params a set of objects provided by the client.
     */
    public Object[] remoteRequest(int opType, Object[] params) throws Exception {
	Object[] ret = null;

	switch (opType) {
	case NEW_CLIENT :
	    ret = newClientNotificationDispatcher(params);
	    break;
	case REMOTE_TERMINATE :
	    ret = remoteTerminate((Long)params[0]);
	    break;
	case REGISTER_TO_MBEAN :
	    ret = registerToMBean((Long)params[0],
				  (ObjectName)params[1],
				  (NotificationFilter)params[2]);
	    break;
	case UNREGISTER_FROM_MBEAN :
	    ret = unregisterFromMBean((Long)params[0], (Long)params[1]);
	    break;
	case GET_NOTIFICATIONS :
	    ret = getNotifications((Long)params[0]);
	    break;
	case CLEAR_NOTIFICATIONS :
	    ret = clearNotifications(params);
	    break;
	case SET_PUSH_MODE :
	    ret = setPushMode((Long)params[0], (Integer)params[1], (ConnectorAddress)params[2]);
	    break;
	case SET_OVERFLOW_MODE :
	    ret = setOverflowMode(params);
	    break;
	case GET_OVERFLOW_MODE :
	    ret = getOverflowMode(params);
	    break;
	case SET_CACHE_SIZE :
	    ret = setCacheSize(params);
	    break;
	case GET_CACHE_SIZE :
	    ret = getCacheSize(params);
	    break;
	case SET_OVERFLOW_COUNT :
	    ret = setOverflowCount(params);
	    break;
	case GET_OVERFLOW_COUNT :
	    ret = getOverflowCount(params);
	    break;
	case GET_NOTIF_SERVER_VERSION :
	    ret = new Object[1];
	    ret[0] = SERVER_VERSION;
	    break;
	default :
	    throw new JMRuntimeException("The request is unknown.");
	}

	return ret;
    }

    /**
     * used to terminate this object.
     */
    public void terminate() {
	if (logger.finerOn())
	    logger.finer("terminate", "the object is being terminated.");

	clientList.clear();
	isTerminated = true;
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
     */
    public void setMBeanServer(MBeanServer newMBS) {
	mbServer = newMBS;
    }

    // protected methods
    // -----------------
    /**
     * Be informed the creation of a new ClientNotificationDispatcher object.
     */
    protected Object[] newClientNotificationDispatcher(Object[] params) {
	isTerminated();

	Long id = getID();

	if (logger.finerOn())
	    logger.finer("newClientNotificationDispatcher", "New client is coming, its id is " +id);

	synchronized (clientList) {
	    clientList.put(id, new ClientInfo(id, ((Integer)params[0]).intValue(), ((Integer)params[1]).intValue()));
	}

	Object[] ret = {id};

	return ret;
    }

    /**
     * Be informed the termination of an ClientNotificationDispatcher.
     */
    protected Object[] remoteTerminate(Long id) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("newClientNotificationDispatcher", "A client is leaving, its id is " +id);

	ClientInfo ci = (ClientInfo)clientList.remove(id);

	if (ci != null)
	    ci.terminate();
			
	return new Object[0];
    }

    /**
     * Register the listener of the remote ClientNotificationDispatcher to a specified MBean
     */
    protected Object[] registerToMBean(Long id,
				       ObjectName mbean,
				       NotificationFilter filter)
	throws InstanceNotFoundException {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("registerToMBean", "Add a remote listener from the client " +id+ " to the mbean " + mbean.toString());

	Object ret[] = new Object[1];

	ClientInfo ci = (ClientInfo)clientList.get(id);

	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	ret[0] = ci.addListener(mbean, filter);
	return ret;
    }

    /**
     * Remove a remote listener.
     */
    protected Object[] unregisterFromMBean(Long clientID, Long listenerID)
	throws InstanceNotFoundException, ListenerNotFoundException {

	isTerminated();

	if (logger.finerOn())
	    logger.finer("unregisterFromMBean", "Remove a remote listener " +listenerID+ " from the client " +clientID);

	ClientInfo ci = (ClientInfo)clientList.get(clientID);

	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	ci.removeListener(listenerID);

	return new Object[0];
    }

    /**
     * Get all saved notifications.
     */
    protected RemoteNotification[] getNotifications(Long clientID) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("getNotifications", "Send back all saved notification to the client " +clientID);

	ClientInfo ci = (ClientInfo)clientList.get(clientID);

	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	return ci.getAllNotifs();
		
    }

    /**
     * clear the cache of that client.
     */
    protected Object[] clearNotifications(Object[] params) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("clearNotifications", "clear the notification cache.");

	ClientInfo ci = (ClientInfo)clientList.get((Long)params[0]);

	if (ci != null) {
	    synchronized(ci.notifLog) {
		ci.notifLog.clear();
	    }
	}

	return new Object[0];
    }

    /**
     * Set communication mode: "push" or "pull"
     */
    protected Object[] setPushMode(Long id, Integer push, ConnectorAddress addr) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("setPushMode", "The client "+id+" asks to change push mode to "+push);

	// get the client
	ClientInfo ci = (ClientInfo)clientList.get(id);
	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	synchronized(ci) {
	    int p = push.intValue();
	    if (ci.forwardMode != p) {
		if (p == ClientNotificationHandler.PUSH_MODE) {
		    // change to push

		    // 4532159: Java DMK could stop using rmiregistry on client side
		    if (addr instanceof RmiConnectorAddressV2) {
			Remote obj = ((RmiConnectorAddressV2)addr).getRemoteObj();
			ci.backConnector = (NotificationBackConnector)obj;
		    } else {
			ci.backConnector = connector.startPush(addr);
		    }

		    ci.forwardMode = ClientNotificationHandler.PUSH_MODE;

		    // push back all saved notifs to remote client.
		    pushNotif.newNotif(ci);
		} else if (p == ClientNotificationHandler.PULL_MODE)  {
		    connector.stopPush(ci.backConnector);
		    ci.backConnector = null;
		}
		ci.forwardMode = p;
	    }
	}

	return new Object[0];
    }

    protected Object[] setOverflowMode(Object[] params) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("setOverflowMode", "Set the overflow mode to "+(Integer)params[1]+" for the client "+(Long)params[0]);

	ClientInfo ci = (ClientInfo)clientList.get((Long)params[0]);

	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	ci.discard_mode = ((Integer)params[1]).intValue();

	return new Object[0];
    }

    protected Object[] getOverflowMode(Object[] params) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("getOverflowMode", "Get the overflow mode of the client "+(Long)params[0]);

	ClientInfo ci = (ClientInfo)clientList.get((Long)params[0]);
		
	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	Object[] ret = {new Integer(ci.discard_mode)};
	return ret;
    }

    protected Object[] setCacheSize(Object[] params) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("setCacheSize", "Set the cache size of the client "+(Long)params[0]);

	ClientInfo ci = (ClientInfo)clientList.get((Long)params[0]);
	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	int newSize = ((Integer)params[1]).intValue();
	synchronized(ci.notifLog) {
	    if (ci.notifLog.size() > newSize) {
		// if do not discard the excessed, the size will not be changed.
		if (((Boolean)params[2]).booleanValue()) {

		    ci.overflow_count += ci.notifLog.size()-newSize;
		    // discard excessed
		    if (ci.discard_mode == ClientNotificationHandler.DISCARD_OLD) {
			for (int i=0; i<ci.notifLog.size()-newSize; i++) {
			    ci.notifLog.remove(0);
			}
		    } else if (ci.discard_mode == ClientNotificationHandler.DISCARD_NEW) {
			for (int i=ci.notifLog.size()-1; i>=newSize; i--) {
			    ci.notifLog.remove(i);
			}
		    }
		    ci.cache_size = ((Integer)params[1]).intValue();
		}
		// else, the size is not be changed !
	    } else {
		ci.cache_size = ((Integer)params[1]).intValue();
	    }
	}

	Object[] ret = {new Integer(ci.cache_size)};
	return ret;
    }

    protected Object[] getCacheSize(Object[] params) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("getCacheSize", "Get the cache size of the client "+(Long)params[0]);

	ClientInfo ci = (ClientInfo)clientList.get((Long)params[0]);

	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	Object[] ret = {new Integer(ci.cache_size)};
	return ret;
    }

    protected Object[] setOverflowCount(Object[] params) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("setOverflowCount", "Set the overflow count of the client "+(Long)params[0]);

	ClientInfo ci = (ClientInfo)clientList.get((Long)params[0]);

	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	ci.overflow_count = ((Integer)params[1]).intValue();

	return new Object[0];
    }

    protected Object[] getOverflowCount(Object[] params) {
	isTerminated();

	if (logger.finerOn())
	    logger.finer("getOverflowCount", "Get the overflow count of the client "+(Long)params[0]);

	ClientInfo ci = (ClientInfo)clientList.get((Long)params[0]);
	if (ci == null)
	    throw new JMRuntimeException("Do not know your client.");

	Object[] ret = {new Integer(ci.overflow_count)};
	return ret;
    }

    // private classes
    // ---------------
    // used as a client listener to represent a remote ClientNotificationDispatcher object.
    //
    private class ClientInfo {
	public Long clientID;
	public int forwardMode = ClientNotificationHandler.PULL_MODE;
	public NotificationBackConnector backConnector;
	public List notifLog = Collections.synchronizedList(new ArrayList());

	public int cache_size;
	public int discard_mode;
	public int overflow_count = 0;

	public ClientInfo(Long id, int discardMode, int cacheSize) {
	    clientID = id;
	    discard_mode = discardMode;
	    cache_size = cacheSize;
	}

	public Long addListener(ObjectName mbean,
				NotificationFilter filter)
	    throws InstanceNotFoundException {

	    Long id = getID();

	    RemoteListenerInfo rli = new RemoteListenerInfo(mbean, filter, id, this);

	    mbServer.addNotificationListener(mbean, rli, filter, null);

	    rliList.put(id, rli);

	    return id;
	}

	public synchronized void removeListener(Long id) throws InstanceNotFoundException, ListenerNotFoundException {
	    RemoteListenerInfo rli = (RemoteListenerInfo)rliList.remove(id);

	    if (rli != null) {
		mbServer.removeNotificationListener(rli.mbean, rli);
	    } else {
		throw new ListenerNotFoundException("The listener does not find.");
	    }
	}

	// give back all notifs saved to the client.
	public synchronized RemoteNotification[] getAllNotifs() {
	    // attension: problem of IndexOutOfBoundsException
	    ArrayList tmp = new ArrayList(notifLog.size());
	    while(!notifLog.isEmpty()) {
		tmp.add(notifLog.remove(0));
	    }

	    RemoteNotification[] ret = new RemoteNotification[tmp.size()];
	    tmp.toArray(ret);

	    return ret;
	}

	// ready to remove this object.
	public void terminate() {

	    if (forwardMode == ClientNotificationHandler.PUSH_MODE) {
		try {
		    connector.stopPush(backConnector);
		} catch (Exception e) {
		    if (ciLogger.finestOn())
			ciLogger.finest("terminate", e);
		}
	    }
	    backConnector = null;

	    synchronized (rliList) {
		for (Iterator iter = rliList.values().iterator(); iter.hasNext();) {
		    RemoteListenerInfo rli = (RemoteListenerInfo)iter.next();
		    try {
			mbServer.removeNotificationListener(rli.mbean, rli);
		    } catch (Exception e) {
			if (ciLogger.finestOn())
			    ciLogger.finest("terminate", e);
		    }
		}
	    }

	    notifLog.clear();
	    rliList.clear();
	    //		rliList = null;
	}

	private HashMap rliList = new HashMap();
    }

    // used to save info about a remote listener.
    //
    private class RemoteListenerInfo implements NotificationListener {
	public ObjectName mbean;
	public NotificationFilter filter;
	public Long listenerID;

	public RemoteListenerInfo(
				  ObjectName mbean,
				  NotificationFilter filter,
				  Long listenerID,
				  ClientInfo client) {
	    this.mbean = mbean;
	    this.filter = filter;
	    this.listenerID = listenerID;

	    this.client = client;
	}

	// called by Notif sender
	public void handleNotification(Notification n, Object o) {
	    if (rliLogger.finestOn()) {
		String info = "Receive a notification for remote listener "
		    +listenerID+
		    " of the client " +client.clientID;
		rliLogger.finest("handleNotification", info);
	    }

	    synchronized(client.notifLog) {
		// if pull mode and cache size is limited...
		if (client.forwardMode == ClientNotificationHandler.PULL_MODE
		    && client.cache_size >= 0) {
		    if (client.notifLog.size() < client.cache_size) {
			client.notifLog.add(new RemoteNotification(listenerID, n));
		    } else {
			// discard the oldest or the newest. Attension if cache_size == 0.
			if (client.discard_mode == ClientNotificationHandler.DISCARD_OLD
			    && client.cache_size>0 ) {
			    client.notifLog.remove(0);
			    client.notifLog.add(new RemoteNotification(listenerID, n));
			} else {
			    // nothing to do
			}
			client.overflow_count++;
		    }
		} else {
		    client.notifLog.add(new RemoteNotification(listenerID, n));
		}
	    }

	    if (client.forwardMode == ClientNotificationHandler.PUSH_MODE) {
		pushNotif.newNotif(client);
	    }
	}

	private ClientInfo client;
    }

    // Used to send a notification to its listener in push mode.
    //
    private class PushNotification {
	private List		waitingClientList = Collections.synchronizedList(new ArrayList());
	private TaskThread	taskThread = null;

	private boolean tobeTerminated = false;

	public PushNotification() {
	    super();	
	}

	// put a client to the list if a notif needs to send to it, start the thread if necessary
	public synchronized void newNotif(ClientInfo ci) {
	    if (!waitingClientList.contains(ci)) {
		waitingClientList.add(ci);
	    }

	    if(taskThread == null || !taskThread.isAlive() ) {
		taskThread =  new TaskThread();
		taskThread.start();
	    }
	}

	public void terminate() {
	    boolean tobeTerminated = true;
	    taskThread = null;
	}

	// used to send notifs to clients
	private class TaskThread extends Thread {
	    public TaskThread() {
		super("taskThread");
		//setDaemon(true);
	    }

	    public void run() {
		while (!tobeTerminated) {
		    ClientInfo ci = null;

		    synchronized(waitingClientList) {
			if (!waitingClientList.isEmpty()) {
			    ci = (ClientInfo)waitingClientList.remove(0);
			} else {
			    // no more notifs
			    break;
			}
		    }
					
		    try {
			Object[] params = ci.getAllNotifs();
			ci.backConnector.remoteRequest(ClientNotificationDispatcher.HANDLE_NOTIFICATION, params);
		    } catch (Exception e) {
			// TODO remove this client?
			if (logger.finestOn()) {
			    logger.finest("TaskThread", "run", e);
			}
		    }
		}
	    }
	}
    }


    // private methods
    // ---------------
    //
    private void isTerminated() {
	if (isTerminated)
	    throw new JMRuntimeException("This ServerNotificationDispatcher object has been terminated and is waiting to be destroyed.");
    }

    // used to get a unique id
    //
    private static Long getID() {
	if (localCounter == Long.MAX_VALUE)
	    localCounter = 0;

	return new Long(localCounter++);
    }

    // stuff for Tracing

    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_NOTIFICATION,
		        "ServerNotificationDispatcher");
    private static final ClassLogger ciLogger = 
	new ClassLogger(ClassLogger.LOGGER_NOTIFICATION,
			"ClientInfo");
    private static final ClassLogger rliLogger = 
	new ClassLogger(ClassLogger.LOGGER_NOTIFICATION,
			"RemoteListenerInfo");

    // private variables
    // -----------------
    private MBeanServer mbServer;
    private ServerNotificationHandlerInternal connector;
    
    private PushNotification pushNotif;
    
    private HashMap clientList		= new HashMap();

    private static long localCounter	= 0;

    private boolean isTerminated		= false;

    private static final Integer SERVER_VERSION = new Integer(2);
}
