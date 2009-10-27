/*
 * @(#)file      ClientNotificationDispatcher.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.51
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

import java.lang.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Collections;

import javax.management.*;

import com.sun.jdmk.*;
import com.sun.jdmk.comm.*;
import com.sun.jdmk.internal.ClassLogger;

/**
 * This class is used by a client connector to receive notifications from remote MBean objects.
 * It will be also used by a ProxyBean object to register a local notification listener.
 */
class ClientNotificationDispatcher implements ClientNotificationHandler {
    // public static variables
    //
    public final static int HANDLE_NOTIFICATION     = 0;
    public final static int CONNECTOR_TEST          = 1;

    /**
     * Construct an ClientNotificationDispatcher object.
     * The push mode is used as default mode.
     */
    public ClientNotificationDispatcher(ClientNotificationHandlerInternal connector)
        throws IllegalArgumentException {

        if (connector == null)
            throw new IllegalArgumentException("A connector should be specified.");

        if (logger.finerOn())
            logger.finer("Constructor", "Create a new ClientNotificationDispatcher object.");

        this.connector = connector;

        // start push mode
        setMode(ClientNotificationHandler.PUSH_MODE);
    }

    // public methods.
    // ---------------

    /**
     * Get remote ID.
     */
    public Long getNotificationClientId() {
	if (isConnected) {
	    return remoteID;
	}
	return null;
    }

    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    public synchronized void stopListening()  { 
        stopListening(false);
    }
    // end NPCTE fix for bugId 4783766

    /** 
     * Stop listening notification. 
     */ 
    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    public synchronized void stopListening(boolean local)  { 
    // end NPCTE fix for bugId 4783766
        if (logger.finerOn())
            logger.finer("stopListening", "This object is stopping listening...");

        listenerList.clear();
        try {
            // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
            disconnect(local);
            // end NPCTE fix for bugId 4783766
        } catch (Exception e) {}
    }

    /**
     * Gets the notification forwarding mode.
     * <P>
     * The default value is <CODE>PUSH_MODE</CODE>.
     */
    public int getMode() {
        return forwardMode ;
    }

    /**
     * Sets the notification forwarding mode.
     * <P>
     * The default value is <CODE>PUSH_MODE</CODE>.
     *   
     * @param mode set to <CODE>PUSH_MODE</CODE> or <CODE>PULL_MODE</CODE>.
     */
    public synchronized void setMode(int mode) throws IllegalArgumentException {
        if (mode != ClientNotificationHandler.PUSH_MODE && mode != ClientNotificationHandler.PULL_MODE) {
            throw new IllegalArgumentException("The mode is illegal.");
        }

        if (logger.finerOn())
            logger.finer("setMode", "A user asks to set mode to "+mode);

        if (forwardMode != mode) {
            forwardMode = mode;

            if (isConnected)
                internalSetMode(forwardMode);
        }
    }

    /**
     * Adds a listener to a registered MBean.
     *
     * @param mbean The name of the MBean on which the listener should be 
     *        added.
     * @param listener The listener object which will handle the 
     *        notifications emitted by the registered MBean.
     * @param filter The filter object. If filter is null, no filtering will 
     *        be performed before handling notifications.
     * @param handback The context to be sent to the listener when a 
     *        notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name provided does not 
     *            match any of the registered MBeans.
     * @exception IllegalArgumentException Thrown if the listener is null.
     */
    public void addNotificationListener(ObjectName mbean,
                                        NotificationListener listener,
                                        NotificationFilter filter,
                                        Object handback)
        throws InstanceNotFoundException {

	if (listener == null) {
		throw new RuntimeOperationsException(
                      new IllegalArgumentException("Null listener"),
                                                   "Null listener");
	}

        // establis connection with the server if necessary
        connect();

        if (logger.finerOn())
            logger.finer("addNotificationListener", "Ask the server to add a listener.");

        Object[] ret = null;

        // ask the ServerNotificationDispatcher to add a listener to the mbean
        Object[] params = {remoteID, mbean, filter};
        try {
            ret = connector.remoteRequest(ServerNotificationDispatcher.REGISTER_TO_MBEAN, params);
	} catch (javax.management.RuntimeOperationsException roe) {
		throw roe;
        } catch (InstanceNotFoundException infe) {
            throw infe;
        } catch (CommunicationException ce) {
            throw ce;
        } catch (JMRuntimeException jmre) {
            throw jmre;
	} catch (IllegalArgumentException iae) {
		throw iae;
        } catch (Exception e) {
            // TODO
            e.printStackTrace();
        }

        Long listenerID = (Long)ret[0];

        listenerList.put(listenerID, new ListenerInfo(mbean, listener, filter, handback, listenerID));
    }

    /**
     * Removes a listener from a registered MBean.
     *
     * @param mbean The name of the MBean on which the listener should be 
     *        removed.
     * @param listener The listener object which will handle the 
     *        notifications emitted by the registered MBean.
     * This method will remove all the information related to this listener.
     *
     * @exception InstanceNotFoundException The MBean name provided does not
     *            match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not registered 
     *            in the MBean.
     */
    public void removeNotificationListener(ObjectName mbean, 
					   NotificationListener listener)
        throws InstanceNotFoundException, ListenerNotFoundException {

        if (logger.finerOn())
            logger.finer("removeNotificationListener", 
		  "Ask the server to remove a listener.");

	if (mbean == null) {
	    throw new InstanceNotFoundException(
		   "The MBean name doesn't correspond to a registered MBean.");
	}

	ListenerInfo li;
	ArrayList removed = new ArrayList(1);

	// possible that a same listener has been added more than one 
	// time to a same mbean.
	synchronized(listenerList) {
	    for (Iterator iter = listenerList.values().iterator(); 
		 iter.hasNext();) {

	        li = (ListenerInfo)iter.next();
		if (mbean.equals(li.mbean) && listener == li.listener) {
		    removed.add(li);
		}
            }

	    if (removed.size() !=0) {
		for (int i=0; i<removed.size(); i++) {
		    li = (ListenerInfo)removed.get(i);
		    listenerList.remove(li.id);
		}
	    } else {
		throw new ListenerNotFoundException(
				  "Do not know your listener.");
	    }
        }

	// ask ServerNotificationDispatcher to remove the listener
	// from that mbean.
	if (removed.size() !=0 && isConnected) {
	    while(removed.size()>0) {
		li = (ListenerInfo)removed.remove(0);
		Object[] params = {remoteID, li.id};
		try {
		    connector.remoteRequest(ServerNotificationDispatcher.
					    UNREGISTER_FROM_MBEAN, params);
		} catch (Exception e) {
		    if (e instanceof InstanceNotFoundException) {
			throw (InstanceNotFoundException)e;
		    } else if (e instanceof ListenerNotFoundException) {
			throw (ListenerNotFoundException)e;
		    } else if (e instanceof CommunicationException) {
			throw (CommunicationException)e;
		    } else if (e instanceof JMRuntimeException) {
			throw (JMRuntimeException)e;
		    } else {
			if (logger.finestOn())
			    logger.finest("removeNotificationListener exception:", e);
		    }
		}
	    }
        }

        // check if need to disconnect...
        if (listenerList.size() == 0) {
            try {
                // NPCTE fix for bugId 4783766, esc 542324, MR, Dec 2002
                // disconnect();
                if (System.getProperty("com.sun.jdmk.notification.termination") == null)
                    disconnect(false);      // default case
                else
                    disconnect(true);
                // end NPCTE fix for bugId 4783766
            } catch (Exception e) {
                // TODO debug info
                if (logger.finerOn())
                    logger.finer("removeNotificationListener", e);
            }
        }
    }

    /**
     * Specifies the period for notification forwarding in milliseconds.
     * <P>
     * If set to equal to or less than zero and the pull mode is used, no pull will be done. A user should
     * explicitly call <CODE>getNotifications</CODE> to retrieve all notifications in the cache.
     * This method has no effect in the push mode in the current implementation.
     * <P>
     * The default value is 1000 milliseconds.
     *   
     * @param period The period in milliseconds.
     */
    public void setPeriod(int period) {
        if (logger.finerOn())
            logger.finer("setPullPeriod", "A user asks to set pull period to "+period);

        forwardPeriod = period;

        // stop pull anyway, need to use new period.
        try {
            jobOfGet.terminate();
            jobOfGet = null;
        } catch (Exception e) {}

        if (period > 0 && isConnected && forwardMode == ClientNotificationHandler.PULL_MODE) {
            // start pulling
            jobOfGet = new JobOfGetNotif();
            
            jobOfGet.start();
        }
    }

    /**
     * Gets the period for notification forwarding in milliseconds.
     * <P>
     * The default value is 1000 milliseconds.
     */
    public int getPeriod() {
        return forwardPeriod;
    }

    /**  
     * Retrieves all notifications in the cache.
     */
    public void getNotifications() {
        if (logger.finerOn())
            logger.finer("getNotification", "A user asks to get all notifications.");

        // if no listener or in push mode, no need to do it.
        if (!isConnected || forwardMode == ClientNotificationHandler.PUSH_MODE)
            return;

        Object[] ret = null;
        try {
            Object[] params = {remoteID};
            ret = connector.remoteRequest(ServerNotificationDispatcher.GET_NOTIFICATIONS, params);
        } catch (CommunicationException ce) {
            throw ce;
        } catch (JMRuntimeException jmre) {
            throw jmre;
        } catch (Exception e) {
            // TODO
            if (logger.finestOn())
                logger.finest("getNotifications", e);
        }

        if (ret != null) {
            for (int i=0; i<ret.length; i++) {
                RemoteNotification ne = (RemoteNotification)ret[i];
                synchronized(listenerList) {
                    ListenerInfo li = (ListenerInfo)listenerList.get(ne.id);
                    if (li == null) {
                        // TODO remove this listener
                    } else {
                        try {
                            li.listener.handleNotification(ne.notif, li.handback);
                        } catch (Exception ee) {
                            // TODO: remove the listener
                            ee.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    /**
     * Clear the notification cache. All notifications saved in the cache then will be discarded.
     */
    public void clearCache() {
        if (logger.finerOn())
            logger.finer("clearCache", "Ask the server to clear the notification cache.");

        // if not connected or push mode, nothing to do.
        if (!isConnected || forwardMode == ClientNotificationHandler.PUSH_MODE) {
            return;
        }

        Object[] params = {remoteID};
        try {
            connector.remoteRequest(ServerNotificationDispatcher.CLEAR_NOTIFICATIONS, params);
        } catch (Exception e) {
            if (logger.finestOn())
                logger.finest("clearNotifications", e);
        }
    }

    /**
     * Set the cache size of notifications waiting to be forwarded.
     * <P>If set to <CODE>NO_CACHE_LIMIT</CODE> or a negative value, notifications will never be discarded,
     * but this may lead to OutOfMemory errors under stressed conditions.
     * <P>The default value is <CODE>NO_CACHE_LIMIT</CODE>.
     *
     * @param size the maximum number of notifications in the cache.
     */
    public int setCacheSize(int size, boolean discardOverflow) {
        if (logger.finerOn())
            logger.finer("setCacheSize", "Ask the server to set size of the cache.");

        if (isConnected) {
            Object[] params = {remoteID, new Integer(size), new Boolean(discardOverflow)};
            Object ret[] = null;
            try {
                ret = connector.remoteRequest(ServerNotificationDispatcher.SET_CACHE_SIZE, params);
            } catch (CommunicationException e) {
                throw e;
            } catch (JMRuntimeException e) {
                throw e;
            } catch (Exception e) {
                if (logger.finestOn())
                    logger.finest("setCacheSize", e);
            }
            cacheSize = ((Integer)ret[0]).intValue();
        } else {
            cacheSize = size;
        }

        return cacheSize;
    }

    /**
     * Get the cache size of notifications waiting to be forwarded.
     * <P>If set to <CODE>NO_CACHE_LIMIT</CODE> or a negative value, notifications will never be discarded,
     * but this may lead to OutOfMemory errors under stressed conditions.
     * <P>The default value is <CODE>NO_CACHE_LIMIT</CODE>.
     */
    public int getCacheSize() {
        if (logger.finerOn())
            logger.finer("getCacheSize", "Ask the server to return the size of the cache.");

        return cacheSize;

        //              Object[] params = {remoteID};
        //              Object[] ret = new Object[1];
        //              try {
        //                      ret = connector.remoteRequest(ServerNotificationDispatcher.GET_CACHE_SIZE, params);
        //              } catch (CommunicationException e) {
        //                      throw e;
        //              } catch (JMRuntimeException e) {
        //                      throw e;
        //              } catch (Exception e) {
        //                      if (logger.finestOn())
        //                              logger.finest("getCacheSize", e);
        //              }
        //              return ((Integer)ret[0]).intValue();
    }

    /**
     * Sets the number of notifications discarded, this number indicates the number
     * of notifications discarded because the cache limit has been reached.
     *
     * @param count The new value to set to overflow count.
     */
    public void setOverflowCount(int count) {
        if (logger.finerOn())
            logger.finer("setOverflowCount", "Set the overflow count.");

        if (isConnected) {
            Object[] params = {remoteID, new Integer(count)};
            try {
                connector.remoteRequest(ServerNotificationDispatcher.SET_OVERFLOW_COUNT, params);
            } catch (CommunicationException e) {
                throw e;
            } catch (JMRuntimeException e) {
                throw e;
            } catch (Exception e) {
                if (logger.finestOn())
                    logger.finest("setOverflowCount", e);
            }
        }
    }

    /**
     * Get the number of notifications discarded since last forwarding because the cache limit has been reached.
     */
    public int getOverflowCount() {
        if (logger.finerOn())
            logger.finer("getOverflowCount", "Get the overflow count.");

        int count = 0;

        if (isConnected) {
            Object[] params = {remoteID};
            Object[] ret = null;
            try {
                ret = connector.remoteRequest(ServerNotificationDispatcher.GET_OVERFLOW_COUNT, params);
            } catch (CommunicationException e) {
                throw e;
            } catch (JMRuntimeException e) {
                throw e;
            } catch (Exception e) {
                if (logger.finestOn())
                    logger.finest("getOverflowCount", e);
            }

            count = ((Integer)ret[0]).intValue();
        }

        return count;
    }

    /** 
     * Specify whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the 
     * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds. 
     * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
     * 
     * @param of The overflow mode to specify. 
     * @exception IllegalArgumentException Thrown if the mode is not <CODE>DISCARD_NEW</CODE> 
     * nor <CODE>DISCARD_OLD</CODE>. 
     */
    public void setOverflowMode(int of) throws IllegalArgumentException {
        if (logger.finerOn())
            logger.finer("setOverflowMode", "Set overflow mode.");

        if (of != ClientNotificationHandler.DISCARD_NEW
            && of != ClientNotificationHandler.DISCARD_OLD) {

            throw new IllegalArgumentException("Illegal overflow mode.");
        }

        if (isConnected) {
            Object[] params = {remoteID, new Integer(of)};
            try {
                connector.remoteRequest(ServerNotificationDispatcher.SET_OVERFLOW_MODE, params);
            } catch (CommunicationException e) {
                throw e;
            } catch (JMRuntimeException e) {
                throw e;
            } catch (Exception e) {
                if (logger.finestOn())
                    logger.finest("setOverflowMode", e);
            }
        }

        discardMode = of;
    }

    /** 
     * Return whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the 
     * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds. 
     * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
     */ 
    public int getOverflowMode() {
        if(logger.finerOn())
            logger.finer("getOverflowMode", "Get the overflow mode.");

        return discardMode;

        //              Object[] params = {remoteID};
        //              Object[] ret = null;
        //              try {
        //                      ret = connector.remoteRequest(ServerNotificationDispatcher.GET_OVERFLOW_MODE, params);
        //              } catch (CommunicationException e) {
        //                      throw e;
        //              } catch (JMRuntimeException e) {
        //                      throw e;
        //              } catch (Exception e) {
        //                      if (logger.finestOn())
        //                              logger.finest("getOverflowMode", e);
        //              }
        //              return ((Integer)ret[0]).intValue();
    }

    /**
     * Be called by a connector to receive a remote operation.
     *
     * @param opType an integer specified by the server.
     * @param params a set of objects provided by the server.
     */
    public Object[] remoteRequest(int opType, Object[] params) throws Exception {
        Object[] ret = null;

        switch (opType) {
        case    HANDLE_NOTIFICATION :
            forwardNotifs.addNotifs(params);
            ret = new Object[0];
            break;
        case    CONNECTOR_TEST :
            ret = backConnectorTest();
            break;
        default :
            throw new JMRuntimeException("The request is unknown.");
        }

        return ret;
    }

    /**
     *
     */
    protected void finalize() throws Throwable {
        listenerList.clear();
        // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
        disconnect(true);
        // end NPCTE fix for bugId 4783766
        listenerList = null;

        super.finalize();
    }

    // used to receive notifs.
    private class ForwardNotifs {
        private TaskThread taskThread   = null;
        private List notifList  = Collections.synchronizedList(new ArrayList());

        public ForwardNotifs() {
            super();
        }

        public synchronized void addNotifs(Object[] notifs) {
            notifList.add((RemoteNotification[])notifs);

            if (taskThread == null ) {
                taskThread =  new TaskThread();
                taskThread.start();
            }
        }

        public void terminate() {
            if (taskThread != null) {
                taskThread.toBeTerminated = true;
            }
        }

        private class TaskThread extends Thread {
            boolean toBeTerminated = false;

            public TaskThread() {
                super("taskThread");
                setDaemon(true);
            }

            public void run() {
                while (!toBeTerminated) {
                    RemoteNotification[] notifs = null;
                    synchronized(notifList) {
                        if (notifList.isEmpty())
                            break;
                        notifs = (RemoteNotification[])notifList.remove(0);
                    }

                    if (notifs == null)
                        continue;

                    synchronized(listenerList) {
                        for (int i=0; i<notifs.length; i++) {
                            ListenerInfo li = (ListenerInfo)listenerList.get(notifs[i].id);
                            if (li == null) {
                                // TODO tell the server to remove this listener
                                if (logger.finestOn()) {
                                    logger.finest("forwardNotif", "Receive a notification not waited.");
                                }
                            } else {
                                try {
                                    li.listener.handleNotification(notifs[i].notif, li.handback);
                                } catch (Exception e) {
                                    // TODO remove the listener?
                                    if (logger.finestOn()) {
                                        logger.finest("forwardNotif", e);
                                    }
                                }
                            }
                        }
                    }
                }
                taskThread = null;
            }
        }
    }

    // protected methods
    // -----------------

    // is called by a ServerNotificationDispatcher to test a new connector.
    //
    protected Object[] backConnectorTest() {
        return new Object[0];
    }

    // is used to establish communication with the server.
    // 
    protected synchronized void connect() { 
        if (isConnected)
            return;

        if (logger.finerOn())
            logger.finer("connect", "Connecting to the server ...");

        // inform the remote ServerNotificationDispatcher the creation of this ClientNotificationDispatcher object.
        Object[] params = {new Integer(discardMode), new Integer(cacheSize)};
        Object[] ret = null;
        try {
            ret = connector.remoteRequest(ServerNotificationDispatcher.NEW_CLIENT, params);
            remoteID = (Long)ret[0];
            
            if (logger.finestOn()) {
                logger.finest("connect", "Id=" + remoteID );
            }
        } catch (CommunicationException ce) {
            throw ce;
        } catch (JMRuntimeException jmre) {
            throw jmre;
        } catch (Exception e) {
            // TODO
            if (logger.finestOn())
                logger.finest("connect", e);
        }

        // tell the server communication mode
        internalSetMode(forwardMode);

        isConnected = true;
    }

    // used to set communication mode
    protected synchronized void internalSetMode(int mode) {

        Object[] params = null;
        if (mode == ClientNotificationHandler.PUSH_MODE) {
            // use push mode
            clientAddress = connector.startPush();
            Object[] pushP = {remoteID, new Integer(ClientNotificationHandler.PUSH_MODE), clientAddress};
            params = pushP;
        } else if (mode == ClientNotificationHandler.PULL_MODE) {
            // use pull mode
            Object[] pullP = {remoteID, new Integer(ClientNotificationHandler.PULL_MODE), null};
            params = pullP;
        }

        try {
            //tell server to change mode
            connector.remoteRequest(ServerNotificationDispatcher.SET_PUSH_MODE, params);
        } catch (CommunicationException ce) {
            throw ce;
        } catch (JMRuntimeException jmre) {
            throw jmre;
        } catch (Exception e) {
            // TODO
            if (logger.finestOn())
                logger.finest("internalSetMode", e);
        }

        // tell the client to stop push if necessary
        if (mode == ClientNotificationHandler.PULL_MODE && forwardMode == ClientNotificationHandler.PUSH_MODE) {
            connector.stopPush(clientAddress);
        }

        // stop or start pulling
        if (mode == ClientNotificationHandler.PUSH_MODE) {
            // stop pulling anyway
            try {
                jobOfGet.terminate();
                jobOfGet = null;
            } catch (Exception e) {}
        }

        if (mode == ClientNotificationHandler.PULL_MODE && forwardPeriod > 0
            && (jobOfGet == null || !jobOfGet.isAlive() )) {
            jobOfGet = new JobOfGetNotif();
            jobOfGet.start();
        }

        forwardMode = mode;
    }

    // used to cut the connection with the sever, it will remove all listeners.
    //
    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    protected synchronized void disconnect() {
        disconnect(false);
    }
    // end NPCTE fix for bugId 4783766

    // used to cut the connection with the sever, it will remove all listeners.
    // 
    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    protected synchronized void disconnect(boolean local) { 
    // end NPCTE fix for bugId 4783766
        if (!isConnected)
            return;

        if (logger.finerOn())
            logger.finer("disconnect", "Disconnecting with the server...");
        // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
        if (!local) {
        // end NPCTE fix for bugId 4783766
            if (logger.finerOn())
                logger.finer("disconnect", "Send Request to the server...");
            Object[] params = {remoteID};
            try {
                connector.remoteRequest(ServerNotificationDispatcher.REMOTE_TERMINATE, params);
            } catch (Exception e) {
                if (logger.finerOn())
                    logger.finer("disconnect", e);
            }
        }

        // stop push if necessary
        if (forwardMode == ClientNotificationHandler.PUSH_MODE)
            connector.stopPush(clientAddress);

        // stop pull anyway
        try {
            jobOfGet.terminate();
            jobOfGet = null;
        } catch (Exception e) {}

        listenerList.clear();
        isConnected = false;
    }

    // private classes
    // ---------------

    // used to save info about a listener
    //
    private class ListenerInfo {
        public ObjectName mbean;
        public NotificationListener listener = null;
        public NotificationFilter filter;
        public Object handback;
        public int time = DEFAULT_TIME;
        public Long id;

        public ListenerInfo(ObjectName mbean,
                            NotificationListener listener,
                            NotificationFilter filter,
                            Object handback,
                            Long id) {

            this.mbean              = mbean;
            this.listener           = listener;
            this.filter             = filter;
            this.handback           = handback;
            this.id                 = id;
        }
    }

    private static final ClassLogger jobLogger = 
	new ClassLogger(ClassLogger.LOGGER_NOTIFICATION,
			"JobOfGetNotif");


    // used to get notifications periodically from the ServerNotificationDispatcher if the mode is set to false.
    //
    private class JobOfGetNotif extends Thread {

        public JobOfGetNotif() {
            setDaemon(true);
        }

        public void run() {
            if (jobLogger.finerOn())
                jobLogger.finer("run", "Start pulling...");

            while (!tobeTerminated && forwardPeriod > 0) {
                RemoteNotification[] ret = null;

                try {
                    sleep(forwardPeriod);

                    // check again
                    if (tobeTerminated || forwardPeriod <= 0) {
                        break;
                    }

                    Object[] params = {remoteID};
                    ret = (RemoteNotification[]) connector.remoteRequest(
			  ServerNotificationDispatcher.GET_NOTIFICATIONS, 
                          params);
                } catch (Exception e) {
                    if (tobeTerminated) {
                        if (jobLogger.finestOn())
                            jobLogger.finest("run", "Disconnected");
                        break;
                    }
                    if (jobLogger.finestOn())
                        jobLogger.finest("run", e);
                    continue;
                }
                if (ret != null) {
                    for (int i=0; i<ret.length; i++) {
                        RemoteNotification ne = (RemoteNotification)ret[i];
                        synchronized(listenerList) {
                            try {
                                ListenerInfo li = (ListenerInfo)
				    listenerList.get(ne.id);
                                li.listener.handleNotification(ne.notif, 
							       li.handback);
                            } catch (Exception ee) {
                                // TODO: remove the listener?
                                if (jobLogger.finestOn())
                                    jobLogger.finest("run", ee);
                            }
                        }
                    }
                }
            }
            //jobOfGet = null;
        }

        public void terminate() {
            if (jobLogger.finerOn())
                jobLogger.finer("terminate", "Stop pulling...");

            tobeTerminated = true;
        }

        boolean tobeTerminated = false;
    }

    // private methods
    // ---------------

    // find a listener info
    private ListenerInfo findListenerInfo(ObjectName mbean, 
					  NotificationListener listener) {

        ListenerInfo li = null;

        for (Iterator iter = listenerList.values().iterator(); iter.hasNext();) {
            ListenerInfo lis = (ListenerInfo)iter.next();
            if (mbean.equals(lis.mbean) && listener == lis.listener) {
                li = lis;
                break;
            }
        }

        return li;
    }

    // stuff for Tracing

    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_NOTIFICATION,
		        "ClientNotificationDispatcher");
    // private variables
    // -----------------

    private static int DEFAULT_TIME         = 1000;

    private ClientNotificationHandlerInternal connector;

    private ConnectorAddress clientAddress  = null;
    private Long remoteID;

    private int forwardPeriod               = DEFAULT_TIME;

    private HashMap listenerList            = new HashMap();

    private JobOfGetNotif jobOfGet          = null;
    private ForwardNotifs forwardNotifs     = new ForwardNotifs();

    private int forwardMode                 = ClientNotificationHandler.PUSH_MODE;
    private int discardMode                 = ClientNotificationHandler.DISCARD_OLD;
    private int cacheSize                   = ClientNotificationHandler.NO_CACHE_LIMIT;

    private boolean isConnected             = false;
}
