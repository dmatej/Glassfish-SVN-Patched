/*
 * @(#)file      RmiConnectorClient.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.97
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
//
import java.lang.reflect.Constructor;
import java.rmi.ServerRuntimeException;
import java.rmi.RemoteException;
import java.rmi.NotBoundException;
import java.rmi.ServerException;
import java.rmi.UnmarshalException;
import java.net.*;
// NPCTE fix for bugId 4624028, esc 0, MR, feb 2003
import java.rmi.server.RMIClientSocketFactory;
// end of NPCTE fix for bugId 4624028

// jdmk import
//
import com.sun.jdmk.*;
import javax.management.*;
import com.sun.jdmk.internal.ClassLogger;

/**
 * Provides an implementation of the <CODE>RemoteMBeanServer</CODE>
 * interface based on the Java remote method invocation (RMI) system. <p>
 * Querying a Java Dynamic Management agent with this connector implies that an instance of
 * {@link com.sun.jdmk.comm.RmiConnectorServer RmiConnectorServer}
 * is running on the remote Java Dynamic Management agent.
 *
 * In order to identify the Java Dynamic Management agent the connector needs to communicate with,
 * the method {@link #connect connect} needs to be invoked with the
 * RMI identity of the <CODE>RMI Connector Server</CODE>.
 *
 * <P>This class implements the {@link com.sun.jdmk.comm.ClientNotificationHandler} interface to
 * receive notifications from a remote MBean and the {@link com.sun.jdmk.comm.HeartBeatClientHandler}
 * interface to detect any problem that could be encountered in the connection with the
 * <CODE>RMI Connector Server</CODE>
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  The legacy RMI connector,
 * including this class, may be removed in a future version of Java
 * DMK.  See {@link javax.management.remote.rmi} and {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 */

public class RmiConnectorClient
    implements RemoteMBeanServer, HeartBeatClientHandler {

    // ===================================================================
    //
    // CONSTRUCTOR
    //
    // ===================================================================

    // NPCTE fix for bugId 4624028, esc 0, MR, feb 2003
    public RmiConnectorClient(RMIClientSocketFactory csf) {
        this.csf = csf;
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (System.getProperty("jdmk.hostname") != null )
            localHost = System.getProperty("jdmk.hostname");
        else {
            try {
                localHost = InetAddress.getLocalHost().getHostName();
            } catch (Exception e) {
            localHost = "localhost";
            }
        }
        if (logger.finerOn())
            logger.finer("RmiConnectorClient", "localHost="+localHost);
        // end of NPCTE fix for bugId 4770217
        Initialize();

    }
    // end NPCTE fix for bugId 4624028

    /**
     * Default constructor for RmiConnectorClient.
     */
    public RmiConnectorClient() {
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (System.getProperty("jdmk.hostname") != null )
            localHost = System.getProperty("jdmk.hostname");
        else {
            try {
                localHost = InetAddress.getLocalHost().getHostName();
            } catch (Exception e) {
                localHost = "localhost";
            }
        }
        if (logger.finerOn())
            logger.finer("RmiConnectorClient", "localHost="+localHost);
        // end of NPCTE fix for bugId 4770217
        Initialize();    
    }

    /** 
     * Constructs RmiConnectorClient.
     * @deprecated Only used for a Client with Java DMK 5.0 to connect to a server with a Java DMK 4.2 or earlier version.
     *
     *@param addr a user specified local host address to receive notifications from the server.
     */
    public RmiConnectorClient(InetAddress addr) {
        try {
            if (addr == null) {
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null )
                    localHost = System.getProperty("jdmk.hostname");
                else
                    localHost = InetAddress.getLocalHost().getHostAddress();
                // end of NPCTE fix for bugId 4770217
            } else {
                localHost = addr.getHostAddress();
            }
        } catch (Exception e) {
            localHost = "localhost";
        }
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (logger.finerOn())
            logger.finer("RmiConnectorClient", "localHost="+localHost);
        // end of NPCTE fix for bugId 4770217
        Initialize();
    }

    // NPCTE fix for bugId 4626073, esc 534403, MR, April 2002
    public RmiConnectorClient(InetAddress add, int commTimeout) {
        try {
            if (add == null) {
                if (System.getProperty("jdmk.hostname") != null )
                    localHost = System.getProperty("jdmk.hostname");
                else
                    localHost = InetAddress.getLocalHost().getHostAddress();
            } else
                localHost = add.getHostAddress();
        } catch (Exception e) {
            localHost = "localhost";
        }
        try {
            java.rmi.server.RMISocketFactory.setSocketFactory(new TimedRmiSocketFactory(commTimeout));
        } catch (java.io.IOException e) {
        // TO BE COMPLETED
        }

        Initialize();
   }
   // end of NPCTE fix for bugId 4626073

   // NPCTE fix for bugId 4626073, esc 534403, MR, April 2002
   public void setTimeout(int timeout) {
        try {
            java.rmi.server.RMISocketFactory.setSocketFactory(new TimedRmiSocketFactory(timeout));
        } catch (java.io.IOException e) {
        // TO BE COMPLETED
        }

   } 
   // end of NPCTE fix for bugId 4626073

    /**
     * <p>Constructs an RmiConnectorClient.  This constructor is only
     * intended to be used for a Client with Java DMK 5.0 or later to
     * connect to a server with a Java DMK 4.2 or earlier version.
     *
     * @param addr a local host address to receive notifications from
     * the server.
     *
     * @since Java DMK 5.0
     */
    public RmiConnectorClient(String addr) {
        if (addr != null) {
            localHost = addr;
        } else {
            try {
                // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
                if (System.getProperty("jdmk.hostname") != null )
                    localHost = System.getProperty("jdmk.hostname");
                else
                    localHost = InetAddress.getLocalHost().getHostName();
                // end of NPCTE fix for bugId 4770217
            } catch (Exception e) {
                localHost = "localhost";
            }
        }
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (logger.finerOn())
            logger.finer("RmiConnectorClient", "localHost="+localHost);
        // end of NPCTE fix for bugId 4770217
        Initialize();
    }


    private void Initialize() {

        // ------------------------------
        // By default, use the default mapper ...
        // ------------------------------
        defaultMapper = new DefaultMapper() ;
        mapper = defaultMapper ;

        // ------------------------------
        // Initialize notification stuff
        // ------------------------------
        notificationClientHandler = new ClientNotificationDispatcher(new NotificationHandlerInternal(this));

        // ------------------------------
        // Initialize heartbeat stuff
        // ------------------------------
        heartbeatClientHandler = new HeartBeatClientHandlerImpl(new HeartBeatInternalClientHandlerImpl(this), notificationClientHandler);
    }

    // ===================================================================
    // 
    // ClientNotificationHandlerInternal interface implementation
    //
    // ===================================================================

    /**
     * Used to ask a client connector to transfer a request to agent side. The client connector
     * only needs to forwards this request to its agent connector, then the agent connector will
     * forward this request to its NotifAgent.
     *
     * @param opType an integer specified by a NotifClient.
     * @param params a set of objects provided by a NotifClient.
     * @return a set of Objects.
     * @exception Exception thrown if an exception appears in the server side.
     */
    Object[] remoteRequest(int opType, Object[] params) throws Exception {
        stopIfNotConnected("createMBean(className,name)");

        if (logger.finerOn())
            logger.finest("remoteRequest(opType,params)", "");
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.remoteRequest(opType, params,
                                                       operationContext);
            else
                return connectorServerV1.remoteRequest(opType, params);
        } catch (ServerRuntimeException e) {
//          throw (javax.management.JMRuntimeException) e.detail ;

            e.detail.fillInStackTrace();
            if (e.detail instanceof javax.management.RuntimeOperationsException ) {
                throw (javax.management.RuntimeOperationsException)e.detail;
            } else if (e.detail instanceof IllegalArgumentException) {
                throw (IllegalArgumentException)e.detail;
            } else if (e.detail instanceof InstanceNotFoundException) {
                throw (InstanceNotFoundException)e.detail;
            } else {
                throw e;
            }
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Used to start the mode "push". A client connector should return a ConnectorAddress
     * object which can be used by an agent connector to establish a connection, this connection
     * will allow the communication from the agent side to client side.
     */
    ConnectorAddress startPush() {
        stopIfNotConnected("startPush()");

        // ------------------------------
        // Create, if required, the RmiNotificationReceiverImpl
        // ------------------------------
        if ( rmiNotificationReceiver == null && rmiNotificationReceiverV2 == null) {
            try {
                if (logger.finestOn()) {
                    logger.finest("startPush","Create new RmiNotificationReceiverImpl") ;
                }

                try {
                    // check if new version
                    Object[] r = remoteRequest(ServerNotificationDispatcher.GET_NOTIF_SERVER_VERSION, null);

                    rmiNotificationReceiverV2 = new RmiNotificationReceiverImplV2(this,connectorAddress,notificationClientHandler) ;

                    rmiNotificationReceiverV2.startListening();
                } catch (JMRuntimeException jmr) {
                    // old version, need to use a local register
                    rmiNotificationReceiverV2 = null;
                    rmiNotificationReceiver =  new RmiNotificationReceiverImpl(this,connectorAddress,notificationClientHandler) ;

                    rmiNotificationReceiver.startListening();
                }
            } catch (ServerRuntimeException e) {
                throw (javax.management.JMRuntimeException) e.detail ;
            } catch (RemoteException e) {
                throw new CommunicationException(e) ;
            } catch (Exception e) {
                throw new CommunicationException(e) ;
            }
        }

        if (rmiNotificationReceiverV2 != null) {
            return rmiNotificationReceiverV2.getAddress() ;
        } else {
            return rmiNotificationReceiver.getAddress() ;
        }
    }

    /**
     * Used to stop the mode "push", and change to the mode "pull".
     */
    void stopPush() {
        stopIfNotConnected("stopPush()");

        // ------------------------------
        // Stop listening
        // ------------------------------
        if ( rmiNotificationReceiver != null ) {
            rmiNotificationReceiver.stopListening();
            rmiNotificationReceiver = null;
        } else if (rmiNotificationReceiverV2 != null ) {
            rmiNotificationReceiverV2.stopListening();
            rmiNotificationReceiverV2 = null;
        }
    }

    // ===================================================================
    // 
    // ClientNotificationHandler interface implementation
    //
    // ===================================================================

    /**  
     * Sets the notification forwarding mode.
     * If set to <CODE>PUSH_MODE</CODE>, it is the agent to push notifications to the client, if set to
     * <CODE>PULL_MODE</CODE>, it is the client to retrieve notifications from the agent.
     * <P>The default value is <CODE>PUSH_MODE</CODE>.
     *   
     * @param mode set to <CODE>PUSH_MODE</CODE> or <CODE>PULL_MODE</CODE>.
     * @exception IllegalArgumentException Thrown if the mode is not equal to
     * <CODE>PUSH_MODE</CODE> nor <CODE>PULL_MODE</CODE>.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void setMode(int mode) throws IllegalArgumentException {
        stopIfNotConnected("setMode()");

        notificationClientHandler.setMode(mode) ;
    }

    /**
     * Gets the notification forwarding mode.
     * If set to <CODE>PUSH_MODE</CODE>, it is the agent to push notifications to the client, if set to
     * <CODE>PULL_MODE</CODE>, it is the client to retrieve notifications from the agent.
     * <P>The default value is <CODE>PUSH_MODE</CODE>.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public int getMode() {
        stopIfNotConnected("getMode()");

        return notificationClientHandler.getMode() ;
    }

    /**
     * Retrieves all notifications in the cache.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void getNotifications() {
        stopIfNotConnected("getNotifications()");

        notificationClientHandler.getNotifications() ;
    }

    /**
     * Clear the notification cache. All notifications stored in the cache then will be discarded
     * without being sent.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void clearCache() {
        stopIfNotConnected("clearCache()");

        notificationClientHandler.clearCache();
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
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */  
    public void setPeriod(int period) {
        stopIfNotConnected("setPeriod()");

        notificationClientHandler.setPeriod(period);
    }

    /**
     * Gets the period for notification forwarding in milliseconds.
     * <P>
     * The default value is 1000 milliseconds.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public int getPeriod() {
        stopIfNotConnected("getPeriod()");

        return notificationClientHandler.getPeriod() ;
    }

    /**
     * Sets the cache size of notifications waiting to be forwarded.
     * <P>If set to <CODE>NO_CACHE_LIMIT</CODE>, notifications will never be discarded,
     * but this may lead to OutOfMemory errors under stressed conditions. If set to zero, any
     * notification will be discarded without being sent.
     * <P>The default value is <CODE>NO_CACHE_LIMIT</CODE>.
     *
     * @param size the maximum number of notifications in the cache.
     * @param discardOverflow effective only if current number of cached notifications exceeds the new size: 
     * if true, discard excess notifications; if false, the cache size will not be changed. 
     *
     * @return The cache size currently set.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public int setCacheSize(int size, boolean discardOverflow) {
        stopIfNotConnected("setCacheSize()");

        return notificationClientHandler.setCacheSize(size, discardOverflow);
    }

    /**
     * Get the cache size of notifications waiting to be forwarded.
     * <P>If set to <CODE>NO_CACHE_LIMIT</CODE> or a negative value, notifications will never be discarded,
     * but this may lead to OutOfMemory errors under stressed conditions.
     * <P>The default value is <CODE>NO_CACHE_LIMIT</CODE>.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public int getCacheSize() {
        stopIfNotConnected("getCacheSize()");

        return notificationClientHandler.getCacheSize();
    }
    
    /**
     * Sets the number of notifications discarded, this number indicates the number
     * of notifications discarded because the cache limit has been reached.
     * <P>This count will be reset to zero if no more listener exists at the client side,
     * because in this case the notification server will remove all information about 
     * this notification client.
     *
     * @param count The new value to set to overflow count.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void setOverflowCount(int count) {
        stopIfNotConnected("setOverflowCount()");

        notificationClientHandler.setOverflowCount(count);
    }

    /**
     * Get the number of notifications discarded since last forwarding because the cache limit has been reached.
     * This value can be reset by calling the method setOverFlowCount.
     * <P>This count will be reset to zero if no more listener exists at the client side,
     * because in this case the notification server will remove all information about 
     * this notification client.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public int getOverflowCount() {
        stopIfNotConnected("getOverflowCount()");

        return notificationClientHandler.getOverflowCount();
    }

    /**
     * Specify whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the
     * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds.
     * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
     *
     * @param of The overflow mode to specify.
     *
     * @exception IllegalArgumentException Thrown if the mode is not <CODE>DISCARD_NEW</CODE>
     * nor <CODE>DISCARD_OLD</CODE>.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void setOverflowMode(int of) throws IllegalArgumentException {
        stopIfNotConnected("setOverflowMode()");

        notificationClientHandler.setOverflowMode(of);
    }

    /**
     * Return whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the
     * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds.
     * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public int getOverflowMode() {
        stopIfNotConnected("getOverflowMode()");

        return notificationClientHandler.getOverflowMode();
    }

    /**
     * Adds a listener to a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be added.
     * @param listener The listener object which will handle the notifications emitted by the registered MBean.
     * @param filter The filter object. If filter is null, no filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void addNotificationListener(ObjectName name, NotificationListener listener, NotificationFilter filter, Object handback)
        throws InstanceNotFoundException {
        stopIfNotConnected("addNotificationListener()");

        notificationClientHandler.addNotificationListener(name,listener,filter,handback) ;
    }

    /**
     * Removes a listener from a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be removed.
     * @param listener The listener object which will handle the notifications emitted by the registered MBean.
     * This method will remove all the information related to this listener.
     *
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not registered in the MBean.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void removeNotificationListener(ObjectName name, NotificationListener listener )
        throws InstanceNotFoundException, ListenerNotFoundException  {
        stopIfNotConnected("removeNotificationListener()");

        notificationClientHandler.removeNotificationListener(name,listener) ;
    }

    // ===================================================================
    // 
    // HeartBeatInternalClientHandler interface implementation
    //
    // ===================================================================

    /**
     * Ping heartbeat server.
     */
    String pingHeartBeatServer(String sessionId, int period, int nretries, Long notifSessionId) {
        if (logger.finerOn())
            {
                logger.finest("pingHeartBeatServer","pingHeartBeatServer");
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.pingHeartBeatServer(sessionId,
                                                             period, nretries,
                                                             notifSessionId,
                                                             operationContext);
            else
                return connectorServerV1.pingHeartBeatServer(sessionId, period,
                                                             nretries,
                                                             notifSessionId);
            } catch (ServerRuntimeException e) {
                throw (javax.management.JMRuntimeException) e.detail ;
            } catch (RemoteException e) {
                throw new CommunicationException(e);
            }
    }

    // ===================================================================
    // 
    // HeartBeatClientHandler interface implementation
    //
    // ===================================================================

    /**
     * Gets the heartbeat period in milliseconds.
     * <P>
     * The default value is 10000 milliseconds.
     */
    public int getHeartBeatPeriod() {
        if (logger.finerOn()) 
            logger.finer("getHeartBeatPeriod", "getHeartBeatPeriod");

        return heartbeatClientHandler.getHeartBeatPeriod();
    }

    /**
     * Specifies the heartbeat period in milliseconds.
     * <P>
     * If set to zero no check will be carried out for the associated connector server being alive.
     * As the heartbeat is driven by the manager this would also prevent the connector server from
     * being aware of the aliveness of this connector client.
     * <P>
     * The default value is 10000 milliseconds.
     *
     * @param period The heartbeat period in milliseconds.
     */
    public void setHeartBeatPeriod(int period) {
        if (logger.finerOn()) 
            logger.finer("setHeartBeatPeriod", "setHeartBeatPeriod");

        heartbeatClientHandler.setHeartBeatPeriod(period);
    }

    /**
     * Gets the number of retries. This number specifies how many times a connector client must retry
     * the connection to the connector server before sending the heartbeat notification indicating that
     * the connector server has died. If number of retries equals zero then no retries are carried out.
     * <P>
     * The default value is 6 times.
     */
    public int getHeartBeatRetries() {
        if (logger.finerOn()) 
            logger.finer("getHeartBeatRetries", "getHeartBeatRetries");

        return heartbeatClientHandler.getHeartBeatRetries();
    }

    /**
     * Sets the number of retries. This number specifies how many times a connector client must retry
     * the connection to the connector server before sending the heartbeat notification indicating that
     * the connector server has died. If number of retries equals zero then no retries are carried out.
     * <P>
     * The default value is 6 times.
     *
     * @param nretries The number of retries.
     */
    public void setHeartBeatRetries(int nretries) {
        if (logger.finerOn()) 
            logger.finer("setHeartBeatRetries", "setHeartBeatRetries");

        heartbeatClientHandler.setHeartBeatRetries(nretries);
    }

    /**
     * Adds the specified heartbeat listener to receive heartbeat notifications from this connector client.
     * Heartbeat notifications occur when the connector client connects to or disconnects from the connector
     * server or when the connector server associated to this connector client dies or is temporarily unreachable.
     *
     * @param listener The heartbeat listener which will handle the notifications emitted by the connector client.
     * @param filter The filter object. If filter is null, no filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a notification is emitted.
     */
    public void addHeartBeatNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) {
        if (logger.finerOn()) 
            logger.finer("addHeartBeatNotificationListener", "addHeartBeatNotificationListener");

        heartbeatClientHandler.addHeartBeatNotificationListener(listener, filter, handback);
    }

    /**
     * Removes the specified heartbeat listener so that it no longer receives heartbeat notifications from
     * this connector client.
     * Heartbeat notifications occur when the connector client connects to or disconnects from the connector
     * server or when the connector server associated to this connector client dies or is temporarily unreachable.
     *
     * @param listener The heartbeat listener which will handle the notifications emitted by the connector client.
     */
    public void removeHeartBeatNotificationListener(NotificationListener listener) {
        if (logger.finerOn()) 
            logger.finer("removeHeartBeatNotificationListener", "removeHeartBeatNotificationListener");

        heartbeatClientHandler.removeHeartBeatNotificationListener(listener);
    }

    // ===================================================================
    // 
    // RemoteMBeanServer interface implementation
    //
    // ===================================================================

    /**
     * ---------------------------------
     * MBean Operation Context
     * ---------------------------------
     */

    /**
     * <p>Set the OperationContext of this connector.  This context will be
     * sent along with each request and can be recovered by the server,
     * which can make it available to the operations it invokes.</p>
     *
     * <p>The saved OperationContext will be a clone of the object
     * <code>c</code> made using its <code>clone</code> method.</p>
     *
     * @param c the new OperationContext.  It may be null to indicate that
     * there is no context.  The previous OperationContext, if any, is lost.
     *
     * @exception CommunicationException if the context cannot be set for
     * some reason.  One common reason is that the object <code>c</code>
     * does not implement the <code>Cloneable</code> interface.
     */
    public void setOperationContext(OperationContext c) {
        try {
            // NPCTE fix for bugId 4497571, esc 0, MR, 03 September 2001 
            if (c != null)      
                c = (OperationContext) c.clone();
            // end of NPCTE fix for bugId 4497571
        } catch (CloneNotSupportedException e) {
            throw new CommunicationException(e);
        }
        // NPCTE fix for bugId 4497571, esc 0, MR, 03 September 2001
        this.operationContext = c;
        // end of NPCTE fix for bugId 4497571
    }

    /**
     * Get the OperationContext that was last given to setOperationContext,
     * or null if setOperationContext was never called.
     * @return the current OperationContext.
     */
    public OperationContext getOperationContext() {
        return operationContext;
    }

    /**
     * ---------------------------------
     * Communication handling
     * ---------------------------------
     */

    /**
     * Initializes the communication with the remote <CODE>MBeanServer</CODE>. All the information
     * needed to identify the <CODE>MBeanServer</CODE> to contact and the protocol to
     * be used is contained in the object of the type <CODE>ConnectorAddress</CODE> passed as
     * a parameter. For then <CODE>RmiconnectotClient</CODE>, the <CODE>ConnectorAddress</CODE> should 
     * of class {@link com.sun.jdmk.comm.RmiConnectorAddress RmiConnectorAddress}. If a communication problem occurs this method will throw
     * a <CODE>CommunicationException</CODE> (<CODE>JMRuntimeException</CODE>).
     * If the <CODE>RemoteMBeanServer</CODE> had already been connected and disconnected with an <CODE>MBeanServer</CODE>
     * identified by its <CODE>MBeanServerId</CODE>, and if the <CODE>MBeanServer</CODE> reachable by the
     * <CODE>MBeanServerAddress</CODE> parameter doesn't
     * have to same <CODE>MBeanServerId</CODE>, the <CODE>java.lang.IllegalAccessException</CODE> is thrown.
     *
     * @param MBeanServerAddress The exact <CODE>MBeanServer</CODE> address to contact (<CODE>MBeanServer</CODE>
     * identification, protocol specification).
     *
     * @exception CommunicationException The <CODE>RemoteMBeanServer</CODE> was already connected or a problem was
     * encountered in the connection to the <CODE>RMI Connector Server</CODE>.
     * @exception IllegalArgumentException The <CODE>RemoteMBeanServer</CODE> has already been connected and disconnected
     * and the specified <CODE>ConnectorAddress</CODE> doesn't identify the same <CODE>MBeanServer</CODE>.
     *
     * @return A String identifying the <CODE>MBeanServer</CODE> with which the communication is established.
     */
    public String connect(ConnectorAddress MBeanServerAddress) {

        // ------------------------------
        // Check address class
        // ------------------------------
        if ( ! ( MBeanServerAddress instanceof RmiConnectorAddress) ) {
            final String msg =
                "Invalid ConnectorAddress class: Expected '" +
                RmiConnectorAddress.class + "', got'" +
                MBeanServerAddress.getClass().getName() + "'";
            if (logger.finerOn())
                logger.finer("connect",msg) ;
            throw new IllegalArgumentException (msg) ;
        }

        // ------------------------------
        // Check that the connectorServer is not already connected
        // ------------------------------
        if (isConnected) {
            final String msg =
                "ConnectorClient already connected to RemoteMBeanServer";
            if (logger.finerOn())
                logger.finer("connect",msg);
            throw new CommunicationException(msg);
        }

        // ------------------------------
        // Set Address
        // ------------------------------
        connectorAddress = (RmiConnectorAddress) MBeanServerAddress;
        String host = connectorAddress.getHost();
        int    port = connectorAddress.getPort();
        String name = connectorAddress.getName();

        // ------------------------------
        // looking for the registry
        // ------------------------------
        java.rmi.registry.Registry r;
        String connectorName = null ;
        try {
            // NPCTE fix for bugId 4624028, esc 0, MR, feb 2003
            //r = java.rmi.registry.LocateRegistry.getRegistry(host, port);
            r = java.rmi.registry.LocateRegistry.getRegistry(host, port, csf);
            // end NPCTE fix for bugId 4624028
        } catch (Exception e) {
            final String msg =
                "Can't contact RMI registry at " + host + ":" + port;
            if (logger.finerOn())
                logger.finer("connect",msg);
            throw new CommunicationException(e, msg);
        }
        try {
            try {
                final String v2name =
                    RmiConnectorServer.serviceNameForVersion(name, 2);
                connectorServerV2 =
                    (RmiConnectorServerObjectV2) r.lookup(v2name);
            } catch (NotBoundException e) {
                final String v1name =
                    RmiConnectorServer.serviceNameForVersion(name, 1);
                connectorServerV1 =
                    (RmiConnectorServerObject) r.lookup(v1name);
            }
        } catch (Exception e) {
            final String msg =
                "Can't contact RMI Connector Server with name " + name ;
            if (logger.finerOn())
                logger.finer("connect",msg);
            throw new CommunicationException(e, msg);
        }

        // ------------------------------
        // Get Current MBeanServerId
        // ------------------------------
        ObjectName delegateName ;

        String currentMBeanServerId = null ;
        try {
            delegateName = new ObjectName("JMImplementation:type=MBeanServerDelegate");
            if (connectorServerV2 != null)
                currentMBeanServerId =
                    (String) connectorServerV2.getAttribute(delegateName,
                                                            "MBeanServerId",
                                                            operationContext);
            else
                currentMBeanServerId =
                    (String) connectorServerV1.getAttribute(delegateName,
                                                          "MBeanServerId");
        } catch (CommunicationException e) {
            final String msg =
                "Can't contact RMI Connector Server to get MBeanServerId";
            if (logger.finerOn())
                logger.finer("connect",msg);
            throw e;
        } catch (Exception e) {
            final String msg =
                "Can't contact RMI Connector Server to get MBeanServerId";
            if (logger.finerOn())
                logger.finer("connect",msg);
            throw new CommunicationException(e, msg);
        }

        // ------------------------------
        // check Current MBeanServerId
        // ------------------------------
        if (MBeanServerId != null ) {
            if (!MBeanServerId.equals(currentMBeanServerId)) {
                final String msg = "Invalid MBeanServerId";
                if (logger.finerOn())
                    logger.finer("connect",msg);
                throw new IllegalArgumentException(msg);
            }
        }
        MBeanServerId = currentMBeanServerId;

        // ------------------------------
        // Set isConnected
        // ------------------------------
        isConnected = true;
        dbgTag = makeDebugTag();

        // ------------------------------
        // Send connection established notification
        // ------------------------------
        heartbeatClientHandler.notifyConnectionEstablished();

        // ------------------------------
        // Start heartbeat ping
        // ------------------------------
        heartbeatClientHandler.startPinging();

        // ------------------------------
        // Return part
        // ------------------------------
        if (logger.finerOn())
            logger.finer("connect","Connection Ok" );
        return MBeanServerId;
    }

    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    /**
     * Terminates the communication with the <CODE>MBeanServer</CODE>.
     */
    public void disconnect() {
        disconnect(false);
    }
    // end NPCTE fix for bugId 4783766

    /**
     * Terminates the communication with the <CODE>MBeanServer</CODE>.
     */
    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    public void disconnect(boolean local) {
    // end NPCTE fix for bugId 4783766
        if (isConnected) {
            // ------------------------------
            // Unset notification stuff
            // ------------------------------
            stopPush();
            // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
            notificationClientHandler.stopListening(local);
            // end NPCTE fix for bugId 4783766

            // ------------------------------
            // Stop hearbeat ping
            // ------------------------------
            // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
            heartbeatClientHandler.stopPinging(-1,local);
            // end NPCTE fix for bugId 4783766

            heartbeatClientHandler.notifyConnectionTerminated();

            // ------------------------------
            // Forget about the rmi server object and the RmiConnectorAddress
            // ------------------------------
            connectorServerV1 = null;
            connectorServerV2 = null;
            connectorAddress = null;
            dbgTag = localClassName;

            // ------------------------------
            // Set isConnected
            // ------------------------------
            if (logger.finerOn())
                logger.finer("connect","Disconnection Ok" );
            isConnected = false;
        }
    }

    /**
     * Checks whether a communication with the <CODE>MBeanServer</CODE> is established.
     * 
     * @return  True, if the communication is established, otherwise false.
     */
    public boolean isConnected() {
        return isConnected;
    }

    /**
     * Returns the exact address of the <CODE>MBeanServer</CODE> to which the ConnectorClient is
     * connected. The address is of the type <CODE>ConnectorAddress</CODE>.
     *
     * @return  The exact address of the remote <CODE>MBeanServer</CODE>, or null if the ConnectorClient is
     * not connected.
     */
    public ConnectorAddress getMBeanServerAddress() {
        return (ConnectorAddress) connectorAddress;
    }

    /**
     * Returns a string which represents the <CODE>MBeanServer</CODE> identification. This String comes from
     * the <CODE>MBeanServerDelegate</CODE> Mbean.
     * If the Connector Client has not already been connected, it returns null.
     * If the Connector Client has been connected and disconnected, <CODE>getMBeanServerId</CODE> still returns
     * the previous <CODE>MBeanServer</CODE> identification.
     */
    public String getMBeanServerId () {
        return MBeanServerId;
    }

    /**
     * ---------------------------------------------------------
     * MBean creation and registration operations
     * ---------------------------------------------------------
     */

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It
     * returns an <CODE>ObjectInstance</CODE> representing the remote MBean created. 
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating the MBean.
     * @param name The name of the MBean to be created.
     *
     * @return  An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the exception that occurred when trying to invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server. 
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public ObjectInstance createMBean(String className, ObjectName name)
        throws ReflectionException, InstanceAlreadyExistsException,
               MBeanRegistrationException, MBeanException,
               NotCompliantMBeanException {

        stopIfNotConnected("createMBean(className,name)");

        if (logger.finerOn())
            logger.finer("createMBean(className,name)",
                  "MBean class name = " + className + " ; MBean name = " +
                  name);
        ObjectInstance result;
        try {
            if (connectorServerV2 != null)
                result = connectorServerV2.createMBean(className, name,
                                                       operationContext);
            else
                result = connectorServerV1.createMBean(className,name);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
        return result;
    }

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns  an <CODE>ObjectInstance</CODE> representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating the MBean.
     * @param name The name of the MBean to be created.
     * @param loaderName The name of the class loader to be used by the <CODE>MBeanServer</CODE>.
     *
     * @return  An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the exception that occurred when trying to invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     * @exception InstanceNotFoundException The specified loader is not registered in the <CODE>MBeanServer</CODE>.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName)
        throws ReflectionException, InstanceAlreadyExistsException,
               MBeanRegistrationException, MBeanException,
               NotCompliantMBeanException, InstanceNotFoundException {

        stopIfNotConnected("createMBean(className,name,loaderName)");

        if (logger.finerOn())
            logger.finer("createMBean(className,name,loaderName)",
                  "MBean class name = " + className + " ; MBean name = " +
                  name);
        ObjectInstance result;
        try {
            if (connectorServerV2 != null)
                result = connectorServerV2.createMBean(className, name,
                                                       loaderName,
                                                       operationContext);
            else
                result = connectorServerV1.createMBean(className, name,
                                                       loaderName);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }

        return result;
    }

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It
     * returns an <CODE>ObjectInstance</CODE> representing the remote MBean created. 
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating
     * the MBean.
     * @param name The name of the MBean to be created.
     * @param params An array containing the parameters of the constructor to be invoked.
     * A parameter can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     * @return  An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the exception that occurred when trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public ObjectInstance createMBean(String className, ObjectName name, Object params[], String signature[])
        throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
        MBeanException, NotCompliantMBeanException {

        stopIfNotConnected("createMBean(className,name,params,signature)");

        if (logger.finerOn())
            {
                logger.finer("createMBean(className,name,params,signature)","MBean class name = " + className + " ; MBean name = " + name);
            }
        ObjectInstance result;
        try {
            if (connectorServerV2 != null)
                result = connectorServerV2.createMBean(className, name, params,
                                                       signature,
                                                       operationContext);
            else
                result = connectorServerV1.createMBean(className, name, params,
                                                       signature);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }

        return result;
    }

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns an <CODE>ObjectInstance</CODE> representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating
     * the MBean.
     * @param name The name of the MBean to be created.
     * @param loaderName The name of the class loader to be used by the <CODE>MBeanServer</CODE>.
     * @param params An array containing the parameters of the constructor to be invoked.
     * A parameter can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     * @return  An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the exception that occurred when trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     * @exception InstanceNotFoundException The specified loader is not registered in the <CODE>MBeanServer</CODE>.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName,  Object params[], String signature[])
        throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
        MBeanException, NotCompliantMBeanException, InstanceNotFoundException {

        stopIfNotConnected("createMBean(className,name,loaderName,params,signature)");

        if (logger.finerOn())
            {
                logger.finer("createMBean(className,name,loaderName,params,signature)","MBean class name = " + className + " ; MBean name = " + name);
            }
        ObjectInstance result;
        try {
            if (connectorServerV2 != null)
                result = connectorServerV2.createMBean(className, name,
                                                       loaderName, params,
                                                       signature,
                                                       operationContext);
            else
                result = connectorServerV1.createMBean(className, name,
                                                       loaderName, params,
                                                       signature);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }

        return result;
    }

    /**
     * ---------------------------------------------------------
     * MBean unregistration operations
     * ---------------------------------------------------------
     */

    /**
     * Deletes an instance of an MBean in the remote MBean server.
     * It also removes its local proxy (<CODE>ProxyMBean</CODE> and/or <CODE>GenericProxy</CODE>) object from the ProxyFactory.
     *
     * @param name The name of the MBean to be deleted.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the <CODE>MBeanServer</CODE>. 
     * @exception MBeanRegistrationException The <CODE>preDeregister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void unregisterMBean(ObjectName name) throws InstanceNotFoundException, MBeanRegistrationException {
        // ------------------------------
        // Check connection
        // ------------------------------
        stopIfNotConnected("unregisterMBean(name)");

        // ------------------------------
        // Remove the object from the cache if it is in the cache ...
        // ------------------------------
        if (logger.finerOn())
            {
                logger.finer("unregisterMBean","Remove ProxyMBean and GenericProxy, if required, for name " + name) ;
            }

        // ------------------------------
        // Remove corresponding MBean
        // ------------------------------
        if (logger.finerOn())
            {
                logger.finer("unregisterMBean(name)","Remove MBean with name " + name) ;
            }
        try {
            if (connectorServerV2 != null)
                connectorServerV2.unregisterMBean(name, operationContext);
            else
                connectorServerV1.unregisterMBean(name);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }

        proxyHandles.remove(name);
        genericHandles.remove(name);
    }

    /**
     * ---------------------------------------------------------
     * ProxyMBean/GenericProxy creation operations
     * ---------------------------------------------------------
     */

    /**
     * ---------------------------------------------------------
     * Miscelleneous operations
     * ---------------------------------------------------------
     */

    /**
     * Checks whether an MBean, identified by its object name, is already registered
     * with the <CODE>MBeanServer</CODE>.
     *   
     * @param name The object name of the MBean to be checked.
     *   
     * @return  True if the MBean is already registered in the <CODE>MBeanServer</CODE>, false otherwise.
     *   
     * @exception RuntimeOperationsException Wraps an IllegalArgumentException: The object name in parameter is null.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public boolean isRegistered(ObjectName name) {
        // ------------------------------
        // Check connection
        // ------------------------------
        stopIfNotConnected("isRegistered(name)");

        // ------------------------------
        // Call remote MBean server
        // ------------------------------
        if (logger.finerOn())
            {
                logger.finer("isRegistered(name)","Call remote MBean server") ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.isRegistered(name, operationContext);
            else
                return connectorServerV1.isRegistered(name);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Gets the names of MBeans controlled by the <CODE>MBeanServer</CODE>. This method
     * allows any of the following to be obtained: The names of all MBeans,
     * the names of a set of MBeans specified by pattern matching on the
     * <CODE>ObjectName</CODE> and/or a Query expression, a specific MBean name (equivalent to
     * testing whether an MBean is registered). When the object name is
     * null or empty, all the objects are to be selected (and filtered if
     * a query is specified). It returns the set of <CODE>ObjectName</CODE>s for the
     * MBeans selected.
     *
     * @param name The object name pattern identifying the MBean names to be retrieved. If
     * null or empty, the names of all the registered MBeans will be retrieved.
     * @param query The query expression to be applied for selecting MBeans.
     *
     * @return  A set containing the <CODE>ObjectName</CODE>s for the MBeans selected.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public java.util.Set queryNames(ObjectName name, QueryExp query) {
        // ------------------------------
        // Check connection
        // ------------------------------
        stopIfNotConnected("queryNames(name,query)");

        // ------------------------------
        // Call remote MBean server
        // ------------------------------
        if (logger.finerOn())
            {
                logger.finer("queryNames(name,query)","Call remote MBean server") ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.queryNames(name, query,
                                                    operationContext);
            else
                return connectorServerV1.queryNames(name,query);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Gets MBeans controlled by the <CODE>MBeanServer</CODE>. This method allows any
     * of the following to be obtained: All MBeans, a set of MBeans specified
     * by pattern matching on the <CODE>ObjectName</CODE> and/or a Query expression, a
     * specific MBean. When the object name is null or empty, all objects are
     * to be selected (and filtered if a query is specified). It returns the
     * set of <CODE>ObjectInstance</CODE> for the selected MBeans.
     *   
     * @param name The object name pattern identifying the MBeans to be retrieved. If
     * null or empty all the MBeans registered will be retrieved.
     * @param query The query expression to be applied for selecting MBeans.
     *   
     * @return A set containing the <CODE>ObjectInstance</CODE> for the MBeans selected.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */  
    public java.util.Set queryMBeans(ObjectName name, QueryExp query) {
        stopIfNotConnected("queryMBeans(name,query)") ;

        if (logger.finerOn())
            {
                logger.finer("queryMBeans(name,query)","MBean name = " + name);
            }
        java.util.Set objectInstances ;
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.queryMBeans(name, query,
                                                     operationContext);
            else
                return connectorServerV1.queryMBeans(name, query);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Returns the number of MBeans controlled by the <CODE>MBeanServer</CODE>.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public Integer getMBeanCount() {
        // ------------------------------
        // Check connection
        // ------------------------------
        stopIfNotConnected("getMBeanCount()");

        // ------------------------------
        // Call remote MBean server
        // ------------------------------
        if (logger.finerOn())
            {
                logger.finer("getMBeanCount()","") ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.getMBeanCount(operationContext);
            else
                return connectorServerV1.getMBeanCount();
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Returns the default domain used for the MBean naming.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public String getDefaultDomain() {
        // ------------------------------
        // Check connection
        // ------------------------------
        stopIfNotConnected("getDefaultDomain()");

        // ------------------------------
        // Call remote MBean server
        // ------------------------------
        if (logger.finerOn())
            {
                logger.finer("getDefaultDomain()","") ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.getDefaultDomain(operationContext);
            else
                return connectorServerV1.getDefaultDomain();
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail ;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }


    /** Returns true if the MBean specified is an instance of the specified class, false otherwise.
     * 
     * @param name The <CODE>ObjectName</CODE> of the MBean.
     * @param className The name of the class.
     *
     * @return true if the MBean specified is an instance of the specified class, false otherwise.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.          
     */
    public boolean isInstanceOf(ObjectName name, String className)
            throws InstanceNotFoundException {
        stopIfNotConnected("isInstanceOf(name)");
        if (logger.finerOn()) {
            logger.finer("isInstanceOf(name)","MBean object name = " + name + " class name = " + className) ;
        }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.isInstanceOf(name, className,
                                                      operationContext);
            else {
                final String msg = "isInstanceOf unsupported with this server";
                throw new IllegalArgumentException(msg);
            }
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail ;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }


    /**
     * Gets the <CODE>ObjectInstance</CODE> for a given MBean registered with the <CODE>MBeanServer</CODE>.
     *   
     * @param name The object name of the MBean.
     *   
     * @return The <CODE>ObjectInstance</CODE> associated to the MBean specified by <VAR>name</VAR>.
     *   
     * @exception InstanceNotFoundException The specified MBean is not registered in the <CODE>MBeanServer</CODE>.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */  
    public ObjectInstance getObjectInstance(ObjectName name) throws InstanceNotFoundException {
        stopIfNotConnected("getObjectInstance(name)");

        if (logger.finerOn())
            {
                logger.finer("getObjectInstance(name)","MBean object name = " + name ) ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.getObjectInstance(name,
                                                           operationContext);
            else
                return connectorServerV1.getObjectInstance(name);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail ;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * ---------------------------------------------------------
     * Management operations on MBean
     * ---------------------------------------------------------
     */

    /**
     * Gets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The object name of the MBean from which the attribute is to be retrieved.
     * @param attribute The name of the attribute to be retrieved.
     *
     * @return  The value of the retrieved attribute.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception AttributeNotFoundException The attribute specified is not accessible in the MBean.
     * @exception MBeanException Wraps an exception thrown by the MBean's getter.
     * @exception InstanceNotFoundException The specified MBean is not registered in the <CODE>MBeanServer</CODE>.
     * @exception ReflectionException An exception occurred when trying to invoke the getAttributes of a Dynamic MBean.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public Object getAttribute(ObjectName name, String attribute)
        throws MBeanException, AttributeNotFoundException, InstanceNotFoundException, ReflectionException {

        stopIfNotConnected("getAttribute(name,attribute)");

        if (logger.finerOn())
            {
                logger.finer("getAttribute(name,attribute)","MBean object name = " + name + " ; attribute name = " + attribute) ;
            }
        try {
            if (connectorServerV2 != null) {
                return connectorServerV2.getAttribute(name, attribute,
                                              operationContext); }
            else
                return connectorServerV1.getAttribute(name, attribute);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail ;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Allows you to retrieve the values of several attributes of an MBean.
     *
     * @param name The object name of the MBean from within which the attributes are
     * to be retrieved.
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return  The values of the attributes retrieved.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundExceptionThe MBean specified is not registered in the MBean server. 
     * @exception ReflectionException An exception occurred when trying to invoke the getAttributes of a Dynamic MBean.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
        throws InstanceNotFoundException, ReflectionException {
        stopIfNotConnected("getAttributes(name,attributes)");

        if (logger.finerOn())
            {
                logger.finer("getAttribute(name,attributes)","MBean object name = " + name ) ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.getAttributes(name, attributes,
                                                       operationContext);
            else
                return connectorServerV1.getAttributes(name, attributes);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail ;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Sets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The name of the MBean within which the attribute is to be set.
     * @param attribute The modification to be performed: The identification of the attribute to
     * be set, the value it is to be set to, and the operator to apply.
     * The value of the attribute can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception AttributeNotFoundException  The attribute specified is not accessible in the MBean.
     * @exception InvalidAttributeValueException The value specified for the attribute is not valid.
     * @exception MBeanException Wraps an exception thrown by the MBean's setter.
     * @exception ReflectionException Wraps an exception thrown while trying to instantiate and apply the
     * operator specified in Modification.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
        throws InstanceNotFoundException, AttributeNotFoundException,
        InvalidAttributeValueException, MBeanException, ReflectionException {

        stopIfNotConnected("setAttribute(name,attribute)");

        if (logger.finerOn() && (attribute != null)) {
            logger.finer("setAttribute(name,attribute)","MBean object name = " + name + " ; attribute name = " + attribute.getName()) ;
        }
        try {
            if (connectorServerV2 != null)
                connectorServerV2.setAttribute(name, attribute, operationContext);
            else
                connectorServerV1.setAttribute(name, attribute);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Allows you to modify the values of several attributes of an MBean.
     *
     * @param name The object name of the MBean from within which the attributes are
     * to be set.
     * @param attributes A list of the attributes to be set, their values and, optionally, the
     * operators to apply.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @return  The values of the attributes that were set.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException An exception occurred when trying to invoke the setAttributes of a Dynamic MBean.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public AttributeList setAttributes(ObjectName name, AttributeList attributes)
        throws InstanceNotFoundException, ReflectionException {

        stopIfNotConnected("setAttributes(name,attributes)");

        if (logger.finerOn())
            {
                logger.finer("setAttribute(name,attributes)","MBean object name = " + name ) ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.setAttributes(name, attributes,
                                                       operationContext);
            else
                return connectorServerV1.setAttributes(name, attributes);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * Invokes a method of an MBean.
     *
     * @param name The name of the MBean on which the method is to be invoked.
     * @param methodName The name of the method to be invoked.
     * @param arguments An array containing the arguments to be set when the method is invoked.
     * An argument can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the method.
     *
     * @return  The object returned by the invocation of the given method.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception MBeanException Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke the method.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public Object invoke(ObjectName name, String methodName, Object arguments[], String signature[])
        throws InstanceNotFoundException, MBeanException, ReflectionException {

        stopIfNotConnected("invoke(name,methodName,arguments,signature)");

        if (logger.finerOn())
            {
                logger.finer("invoke(name,methodName,arguments,signature)","") ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.invoke(name, methodName, arguments,
                                                signature, operationContext);
            else
                return connectorServerV1.invoke(name, methodName, arguments,
                                                signature);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * This method supplies the exposed attributes and actions of the MBean.
     * It provides this information using an <CODE>MBeanInfo</CODE> object. 
     *
     * @param name The names of the MBean whose attributes and actions will be returned.
     *
     * @return  An instance of <CODE>MBeanInfo</CODE> which allows all methods and actions of
     * this MBean to be retrieved.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception IntrospectionException An exception occurs during introspection.
     * @exception ReflectionException Wraps an <CODE>java.lang.Exception</CODE> thrown while
     * trying to invoke the <CODE>getMBeanInfo</CODE> method.
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server
     * or a problem was encountered in the connection to the RMI Connector Server.
     */
    public MBeanInfo getMBeanInfo(ObjectName name)
        throws InstanceNotFoundException, IntrospectionException, ReflectionException {

        stopIfNotConnected("getMBeanInfo(name)") ;

        if (logger.finerOn())
            {
                logger.finer("getMBeanInfo(name)","") ;
            }
        try {
            if (connectorServerV2 != null)
                return connectorServerV2.getMBeanInfo(name, operationContext);
            else
                return connectorServerV1.getMBeanInfo(name);
        } catch (ServerRuntimeException e) {
            throw (javax.management.JMRuntimeException) e.detail ;
        } catch (RemoteException e) {
            throw new CommunicationException(e);
        }
    }

    /**
     * ---------------------------------------------------------
     * Local operations
     * ---------------------------------------------------------
     */


    /**
     * Given the object name and the Java class name of the MBean (<CODE>ObjectInstance</CODE>), this
     * method returns the name of the Java class of the corresponding Proxy MBean.
     * The returned name can be null, if there is no Java class corresponding to
     * the needed Proxy MBean.
     *
     * @param instance The <CODE>ObjectInstance</CODE> of the MBean which is represented by the <CODE>ProxyMBean</CODE>.
     *
     * @return The name of the Java class of the <CODE>ProxyMBean</CODE>.
     *
     * @exception ProxyMBeanInstantiationException An error occurs.
     */
    public String getClassForProxyMBean (ObjectInstance instance)
        throws ProxyMBeanInstantiationException {

        if (instance == null) {
            throw new IllegalArgumentException("ObjectInstance cannot be null");
        }
        return mapper.getClassForProxyMBean(instance);
    }

    /**
     * ---------------------------------------------------------
     * Mapper stuff 
     * ---------------------------------------------------------
     */

    // package tuff
    String getHost() {
        return localHost;
    }

    // ===================================================================
    //
    // Private method
    //
    // ===================================================================

    /**
     * Throws a CommunicationException if the connector is not connected.
     *
     * @exception CommunicationException The RMI Connector Client is not connected to RMI Connector Server.
     */
    private void stopIfNotConnected(String methodName) throws CommunicationException {
        if (isConnected == false)
            {
                if (logger.finerOn())
                    {
                        logger.finer("methodName"," Not connected") ;
                    }
                throw new CommunicationException("ConnectorClient not connected to RMI Connector Server");
            }
    }

    private String localClassName = "com.sun.jdmk.comm.RmiConnectorClient" ;

    private String dbgTag = localClassName ;
    private final ClassLogger logger =
        new ClassLogger(ClassLogger.LOGGER_LEGACY_RMI,
                        makeDebugTag());

    private String makeDebugTag() {
        if ( connectorAddress != null )
            {
                return localClassName+"["+ connectorAddress.getPort() + ":" + connectorAddress.getName() + "]" ;
            }
        else
            {
                return localClassName ;
            }
    }

    // ===================================================================
    //
    // Private class
    //
    // ===================================================================

    /**
     * Notifications
     */
    private class NotificationHandlerInternal implements ClientNotificationHandlerInternal {

        /**
         * Ctor.
         */
        public NotificationHandlerInternal(RmiConnectorClient connector) {
            this.connector = connector ;
        }

        /**
         * Used to ask a client connector to transfer a request to agent side. The client connector
         * only needs to forwards this request to its agent connector, then the agent connector will
         * forward this request to its NotifAgent.
         *
         * @param opType an integer specified by a NotifClient.
         * @param params a set of objects provided by a NotifClient.
         * @return a set of Objects.
         * exception CommunicationException thrown if the communication between the server and
         * the client is failed.
         * @exception RemoteRuntimeException thrown if an exception appears in the server side.
         */
        public Object[] remoteRequest(int opType, Object[] params) throws CommunicationException, Exception {
            return connector.remoteRequest(opType,params);
        }

        /**
         * Used to start the mode "push". A client connector should return a ConnectorAddress
         * object which can be used by an agent connector to establish a connection, this connection
         * will allow the communication from the agent side to client side.
         *
         * @exception CommunicationException thrown if the communication between the server and
         * the client is failed.
         * @exception RemoteRuntimeException thrown if an exception appears in the server side.
         */
        public ConnectorAddress startPush() throws CommunicationException, RemoteRuntimeException {
            return connector.startPush() ;
        }

        /**
         * Used to stop the mode "push", and change to the mode "pull".
         *
         * @exception CommunicationException thrown if the communication between the server and
         * the client is failed.
         * @exception RemoteRuntimeException thrown if an exception appears in the server side.
         */
        public void stopPush(ConnectorAddress address) {
            connector.stopPush() ;
        }

        private RmiConnectorClient connector ;
    }

    /**
     * HeartBeat
     */
    private class HeartBeatInternalClientHandlerImpl implements HeartBeatInternalClientHandler {

        /**
         * Ctor
         */
        public HeartBeatInternalClientHandlerImpl(RmiConnectorClient connector) {
            this.connector = connector;
        }

        /**
         * Get remote MBean server.
         */
        public RemoteMBeanServer getRemoteMBeanServer() {
            return connector;
        }

        /**
         * Ping heartbeat server.
         */
        public String pingHeartBeatServer(String sessionId, int period, int nretries, Long notifSessionId) {
            return connector.pingHeartBeatServer(sessionId, period, nretries, notifSessionId);
        }

        private RmiConnectorClient connector;
    }

    // ===================================================================
    //
    // Private variables
    //
    // ===================================================================

    /**
     * MBean Operation Context
     */
    private OperationContext operationContext = null;

    /**  
     * Indicates if the client is connected to the managed object server.
     */  
    private transient boolean isConnected = false;

    /**
     * The MBean server address
     * @serial Host name of the server.
     */  
    private transient RmiConnectorAddress connectorAddress = null ;

    /**  
     * RMI reference to the remote managed object server (RmiConnectorServer).
     */  
    private transient RmiConnectorServerObject connectorServerV1 = null ;

    /**
     * RMI reference to the remote managed object server, in the version
     * where every operation has an OperationContext parameter.  Null if
     * the remote server does not support this version.
     */
    private transient RmiConnectorServerObjectV2 connectorServerV2 = null;

    /**
     * The <CODE>MBeanServer</CODE> Identifier
     */
    private transient String MBeanServerId = null ;

    /**
     * ClassLoader used
     */  
    private transient ClassLoader classLoader = null;

    /**
     * Mapper used
     */  
    private transient Mapper mapper = null;

    /**
     * Default mapper 
     */  
    private transient Mapper defaultMapper = null;

    /**
     * Notification handling
     */  
    private ClientNotificationDispatcher notificationClientHandler = null ;
    private RmiNotificationReceiverImpl rmiNotificationReceiver = null ;

    private RmiNotificationReceiverImplV2 rmiNotificationReceiverV2 = null ;


    /**
     * HeartBeat
     */
    private HeartBeatClientHandlerImpl heartbeatClientHandler = null;

    /**
     * Cache of object handles already created by the adaptor.
     */  
    private transient java.util.Hashtable proxyHandles   = new java.util.Hashtable();
    private transient java.util.Hashtable genericHandles = new java.util.Hashtable();

    /**
     * localHost
     */  
    private String localHost = null ;

    // NPCTE fix for bugId 4624028, esc 0, MR, feb 2003
    private RMIClientSocketFactory csf = null ;
    // end NPCTE fix for bugId 4624028
}
