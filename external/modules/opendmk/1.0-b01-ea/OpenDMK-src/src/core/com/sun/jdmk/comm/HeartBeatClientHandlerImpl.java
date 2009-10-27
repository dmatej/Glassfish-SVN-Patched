/*
 * @(#)file      HeartBeatClientHandlerImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.22
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

package com.sun.jdmk.comm;

// java import
//
import java.io.InterruptedIOException;
import java.util.Date;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;

// jmx import
//
import javax.management.Notification;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;

// jdmk import
//
import com.sun.jdmk.internal.ClassLogger;


/**
 * This class implements the HeartBeatClientHandler interface.
 *
 */

class HeartBeatClientHandlerImpl {

    /**
     * Ctor.
     */
    public HeartBeatClientHandlerImpl(HeartBeatInternalClientHandler heartbeatConnector,
				      ClientNotificationDispatcher notifsConnector) {

	if (heartbeatConnector == null) {
	    throw new IllegalArgumentException("heartbeatConnector cannot be null");
	}

        if (logger.finerOn())
	    logger.finer("Constructor", "Create HeartBeatClientHandler.");

	this.heartbeatConnector = heartbeatConnector;
	this.notifsConnector = notifsConnector;
    }

    /**
     * Gets the heartbeat period in milliseconds.
     * <P>
     * The default value is 10000 milliseconds.
     */
    public int getHeartBeatPeriod() {
	return period;
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
    public synchronized void setHeartBeatPeriod(int period) {
	// Update period value
	//
	int old_period = this.period;
	this.period = period;

	// Check if connection has been established
	//
	if (heartbeatConnector.getRemoteMBeanServer().isConnected()) {
	    if (period > 0 && old_period <= 0) {
		// Start pinging
		//
		startPinging();
	    } else if (period <= 0 && old_period > 0) {
		// Stop pinging
		//
                // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
		stopPinging(0, false);
                // end NPCTE fix for bugId 4783766
	    } else if (period > 0 && old_period > 0 && period != old_period) {
		// Stop and restart pinging
		//
                // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
		stopPinging(0, false);
                // end NPCTE fix for bugId 4783766
		startPinging();
	    }
	}
    }

    /**
     * Gets the number of retries. This number specifies how many times a connector client must retry
     * the connection to the connector server before sending the heartbeat notification indicating that
     * the connector server has died. If number of retries equals zero then no retries are carried out.
     * <P>
     * The default value is 6 times.
     */
    public int getHeartBeatRetries() {
	return nretries;
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
    public synchronized void setHeartBeatRetries(int nretries) {
	this.nretries = nretries;
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
    public synchronized void addHeartBeatNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) {
        // Check listener
        //
        if (listener == null) {
            throw new IllegalArgumentException("Listener cannot be null");
        }

        // Looking for listener in handbackTable
        //
        Vector handbackList = (Vector) handbackTable.get(listener);
        Vector filterList = (Vector) filterTable.get(listener);
        if (handbackList == null) {
	    handbackList = new Vector();
	    filterList = new Vector();
	    handbackTable.put(listener, handbackList);
	    filterTable.put(listener, filterList);
        }

        // Add the handback
	//
	handbackList.addElement(handback);
	filterList.addElement(filter);
    }

    /**
     * Removes the specified heartbeat listener so that it no longer receives heartbeat notifications from
     * this connector client.
     * Heartbeat notifications occur when the connector client connects to or disconnects from the connector
     * server or when the connector server associated to this connector client dies or is temporarily unreachable.
     *
     * @param listener The heartbeat listener which will handle the notifications emitted by the connector client.
     */
    public synchronized void removeHeartBeatNotificationListener(NotificationListener listener) {
        // Looking for listener in handbackTable
        //
        Vector handbackList = (Vector) handbackTable.get(listener);
        Vector filterList = (Vector) filterTable.get(listener);
        if (handbackList == null) {
            throw new IllegalArgumentException("Listener not found");
        }

        // If handback is null, remove the listener entry
	//
        handbackTable.remove(listener);
        filterTable.remove(listener);
    }

    /**
     * Start pinging the server.
     */
    public synchronized void startPinging() {
	if (period > 0 && (ping == null || !ping.isAlive())) {
	    if (plogger.finerOn())
		plogger.finer("run", "Start pinging connector server...");

	    if (notifsConnector != null) {
		notifsClientId = notifsConnector.getNotificationClientId();
	    } else {
		notifsClientId = null;
	    }

	    try {
		heartbeatClientSessionId = heartbeatConnector.pingHeartBeatServer
		    (heartbeatClientSessionId, period, nretries, notifsClientId);
	    } catch (Exception ee) {
		// failed to start
		logger.warning("startPinging", "Failed to start pinging.", ee);

		return;		
	    }

	    ping = new Ping();
	    if (Thread.currentThread().getPriority() < Thread.MAX_PRIORITY) {
		ping.setPriority(Thread.currentThread().getPriority() + 1) ;
	    }
	    ping.start();
	}
    }

    /**
     * Stop pinging the server.
     */
    // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
    public synchronized void stopPinging(int action, boolean local) {
    // end NPCTE fix for bugId 4783766
	if (ping != null) {
	    if (plogger.finerOn())
		plogger.finer("terminate", 
			      "Stop pinging connector server...");
            // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
	    ping.terminate(action, local);
            // end NPCTE fix for bugId 4783766
	    // Reset global variables if disconnect has been invoked, i.e. "action = -1".
	    //
	    if (action == -1) {
		notifsClientId = null;
		heartbeatClientSessionId = null;
	    }
	    // Interrupt Ping thread only if disconnection has been required by the manager.
	    // If disconnection comes from a notifying lost action, then the Ping thread will
	    // do everything required to die internally, i.e. inside the run() method.
	    //
	    if (!ping.notifyingLost) {
		ping.interrupt();
	    }
	    ping = null;
	}
    }

    /**
     * Send a Connection Established notification.
     */
    public void notifyConnectionEstablished() {
        if (logger.finerOn())
	    logger.finer("notifyConnectionEstablished", "Send a Connection Established notification");

	// Send notification
	//
	sendNotification(new HeartBeatNotification(HeartBeatNotification.CONNECTION_ESTABLISHED,
						   heartbeatConnector.getRemoteMBeanServer(), getSequenceNumber(), new Date().getTime(),
						   "Notify Connection Established", heartbeatConnector.getRemoteMBeanServer().getMBeanServerAddress()));
    }

    /**
     * Send a Connection Retrying notification.
     */
    public void notifyConnectionRetrying() {
        if (logger.finerOn())
	    logger.finer("notifyConnectionRetrying", "Send a Connection Retrying notification");

	// Send notification
	//
	sendNotification(new HeartBeatNotification(HeartBeatNotification.CONNECTION_RETRYING,
						   heartbeatConnector.getRemoteMBeanServer(), getSequenceNumber(), new Date().getTime(),
						   "Notify Connection Retrying", heartbeatConnector.getRemoteMBeanServer().getMBeanServerAddress()));
    }

    /**
     * Send a Connection Lost notification.
     */
    public void notifyConnectionLost() {
        if (logger.finerOn())
	    logger.finer("notifyConnectionLost", "Send a Connection Lost notification");

	// Send notification
	//
	sendNotification(new HeartBeatNotification(HeartBeatNotification.CONNECTION_LOST,
						   heartbeatConnector.getRemoteMBeanServer(), getSequenceNumber(), new Date().getTime(),
						   "Notify Connection Lost", heartbeatConnector.getRemoteMBeanServer().getMBeanServerAddress()));

	// Cleanup connector client
	//
	heartbeatConnector.getRemoteMBeanServer().disconnect();
    }

    /**
     * Send a Connection Reestablished notification.
     */
    public void notifyConnectionReestablished() {
        if (logger.finerOn())
	    logger.finer("notifyConnectionReestablished", "Send a Connection Reestablished notification");

	// Send notification
	//
	sendNotification(new HeartBeatNotification(HeartBeatNotification.CONNECTION_REESTABLISHED,
						   heartbeatConnector.getRemoteMBeanServer(), getSequenceNumber(), new Date().getTime(),
						   "Notify Connection Reestablished", heartbeatConnector.getRemoteMBeanServer().getMBeanServerAddress()));
    }

    /**
     * Send a Connection Terminated notification.
     */
    public void notifyConnectionTerminated() {
        if (logger.finerOn())
	    logger.finer("notifyConnectionTerminated", "Send a Connection Terminated notification");

	// Send notification
	//
	sendNotification(new HeartBeatNotification(HeartBeatNotification.CONNECTION_TERMINATED,
						   heartbeatConnector.getRemoteMBeanServer(), getSequenceNumber(), new Date().getTime(),
						   "Notify Connection Terminated", heartbeatConnector.getRemoteMBeanServer().getMBeanServerAddress()));
    }

    /**
     * Send a heartbeat notification.
     */
    private void sendNotification(Notification notification) {
        // Loop on listener
        //
        for (Enumeration k = handbackTable.keys(); k.hasMoreElements(); ) {

            NotificationListener listener = (NotificationListener) k.nextElement();

            // Get the associated handback list and the associated filter list
            //
            Vector handbackList = (Vector) handbackTable.get(listener);
            Vector filterList = (Vector) filterTable.get(listener);

            // Loop on handback
            //
            Enumeration f = filterList.elements();
	    for (Enumeration h = handbackList.elements(); h.hasMoreElements(); ) {
                Object handback = h.nextElement();
                NotificationFilter filter = (NotificationFilter) f.nextElement();
                if ((filter == null) || ((filter != null) && (filter.isNotificationEnabled(notification)))) {
                    listener.handleNotification(notification,handback);
                }
            }
        }
    }

    /**
     * Get unique sequence number for notifications.
     */
    private static long getSequenceNumber() {
	if (counter == Long.MAX_VALUE)
	    counter = 0;
	return counter++;
    }

    // INNER CLASS
    //------------

    /**
     * This class is used to ping periodically the connector server to which this connector client is connected.
     */
    private class Ping extends Thread {

        public void run() {
	    if (period > 0) {
		try {
		    sleep(period);
		} catch (Exception e) {}
	    }

            while (!toBeTerminated && period > 0) {

		// Reset time variables
		//
		sendingTime = 0;
		receivingTime = 0;
		responseTime = 0;

		// Get notification client id.
		//
		if (notifsConnector != null) {
		    notifsClientId = notifsConnector.getNotificationClientId();
		} else {
		    notifsClientId = null;
		}

		// Get current time (SendingTime)
		//
		sendingTime = System.currentTimeMillis();

                try {
		    if (toBeTerminated) {
			break;
		    }
		    heartbeatServerSessionId = heartbeatConnector.pingHeartBeatServer(heartbeatClientSessionId, period, nretries, notifsClientId);
		    if (heartbeatClientSessionId == null) {
			// New session has been established
			//
			if (toBeTerminated) {
			    break;
			}
			heartbeatClientSessionId = heartbeatServerSessionId;
		    } else if (heartbeatServerSessionId == null) {
			// Server has restarted
			//
			if (toBeTerminated) {
			    break;
			}
			notifyingLost = true;
			notifyConnectionLost();
			notifyingLost = false;
			break;
		    }
		    // If retry succeeds, send connection reestablished notification
		    //
		    if (nretry > 0) {
			if (toBeTerminated) {
			    break;
			}
			nretry = 0;
			notifyConnectionReestablished();
		    }
		} catch (Exception e) {
		    // By default retry policy is true
		    //
		    boolean allowRetry = true;

		    // Check if we've been interrupted
		    //
// 		    if (e instanceof CommunicationException) {
// 			Throwable ex = ((CommunicationException)e).getTargetException();
// 			if (ex != null) {
// 			    if (ex instanceof InterruptedIOException) {
// 				if (toBeTerminated) {
// 				    break;
// 				} else {
// 				    allowRetry = false;
// 				}
// 			    }
// 			}
// 		    }

		    // Check if we have to retry
		    //
		    if (allowRetry) {
			if (nretries != 0) {
			    nretry++;
			    if (nretry == 1) {
				// Notify user that retrying is starting
				//
				if (toBeTerminated) {
				    break;
				}
				notifyConnectionRetrying();
			    } else if (nretry > nretries) {
				// Notify user that all retries failed
				//
				if (toBeTerminated) {
				    break;
				}
				notifyingLost = true;
				notifyConnectionLost();
				notifyingLost = false;
				break;
			    }
			} else {
			    if (toBeTerminated) {
				break;
			    }
			    notifyingLost = true;
			    notifyConnectionLost();
			    notifyingLost = false;
			    break;
			}
		    }
		}

		// Get current time (ReceivingTime)
		//
		receivingTime = System.currentTimeMillis();

		// Compare times and see if we have to sleep
		//
		responseTime = receivingTime - sendingTime;
		if (responseTime < period) {
		    if (toBeTerminated) {
			break;
		    }
		    if (logger.finerOn()) {
			plogger.finer("run", "Next heartbeat ping will be sent in " + (period - responseTime) + " ms.");
			plogger.finer("run", "Session Id = " + heartbeatClientSessionId);
		    }
		    try {
			sleep(period - responseTime);
		    } catch (InterruptedException ie) {
			if (toBeTerminated) {
			    break;
			}
		    }
		}
	    }
	}

        // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
        public void terminate(int action, boolean local) {
        // end NPCTE fix for bugId 4783766

	    // Set toBeTerminated flag
	    //
            toBeTerminated = true;

	    // If termination comes from a notifying lost action, then don't send any
	    // stop pinging request to the connector server as it is unreachable.
	    //
            // NPCTE fix for bugId 4783766, esc 542324, MR , Nov 2002
	    if (!local && !notifyingLost) {
            // end NPCTE fix for bugId 4783766
		// Get notification client id.
		//
		if (notifsConnector != null) {
		    notifsClientId = notifsConnector.getNotificationClientId();
		} else {
		    notifsClientId = null;
		}

		// Send stop waiting for ping request to server
		//
		try {
		    heartbeatServerSessionId = heartbeatConnector.pingHeartBeatServer(heartbeatClientSessionId, action, nretries, notifsClientId);
		} catch (Exception e) {
		    // Ignore: Connector server not responding.
		    // Should we retry?
		}
	    }
	}

	public volatile boolean notifyingLost = false;
	private int nretry = 0;
	private long sendingTime = 0;
	private long responseTime = 0;
	private long receivingTime = 0;
	private boolean toBeTerminated = false;
	private String heartbeatServerSessionId = null;
    }

    // TRACE STUFF
    //------------

    private static String localClassName = "HeartBeatClientHandlerImpl";
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_LEGACY_HEARTBEAT,
			localClassName);
    private static final ClassLogger plogger = 
	new ClassLogger(ClassLogger.LOGGER_LEGACY_HEARTBEAT,
			"Ping");


    // VARIABLES
    //----------

    /**
     * HeartBeat period in milliseconds.
     */
    private int period = 10000;

    /**
     * Number of retries for heartbeat.
     */
    private int nretries = 6;

    /**
     * HeartBeat Session ID.
     */
    private String heartbeatClientSessionId;

    /**
     * Notifications Client ID.
     */
    private Long notifsClientId;

    /**
     * Notification sequence number counter.
     */
    private static long counter = 0;

    /**
     * Ping thread.
     */
    private Ping ping;

    /**
     * Heartbeat listeners list containing the handback objects.
     */
    private Hashtable handbackTable = new Hashtable();

    /**
     * Heartbeat listeners list containing the filter objects.
     */
    private Hashtable filterTable = new Hashtable();

    /**
     * Client connector.
     */
    private HeartBeatInternalClientHandler heartbeatConnector;

    /**
     * Notifications connector.
     */
    private ClientNotificationDispatcher notifsConnector;
}
