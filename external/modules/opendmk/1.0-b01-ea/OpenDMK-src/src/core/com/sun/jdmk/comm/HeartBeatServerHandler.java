/*
 * @(#)file      HeartBeatServerHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.19
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
import java.util.Hashtable;
import java.util.Enumeration;

// jmx import
//
import javax.management.ObjectName;
import javax.management.MBeanServer;

// jdmk import
//
import com.sun.jdmk.internal.ClassLogger;
import com.sun.jdmk.ServiceName;

/**
 * This class implements the agent side of the heartbeat in the connector servers.
 *
 */

class HeartBeatServerHandler {

    public HeartBeatServerHandler(MBeanServer mbs, ServerNotificationDispatcher snd) {
        if (logger.finerOn())
	    logger.finer("Constructor", "Create HeartBeatServerHandler.");
	this.mbs = mbs;
	this.snd = snd;
	try {
	    this.mbsId = (String) 
		mbs.getAttribute(new ObjectName(ServiceName.DELEGATE), 
				 "MBeanServerId");
	} catch (Exception e) {
	    this.mbsId = "unknown";
	}
    }

    public String pingHeartBeatServer(String sessionId, int period,
				      int nretries, Long notifSessionId) {
	String id = sessionId;
	if (id == null) {
	    id = getSessionId(); // new client
	} else if (!id.endsWith(mbsId)) {
	    // not mine, possible the server has been restarted
	    return null;
	}

	HBClientInfo ci;
	synchronized(clients) {
	    ci = (HBClientInfo) clients.get(id);
	}

	if (ci == null) { // new client or old one without heartbeat before
	    if (period <= 0) { // without heartbeat
		return id;
	    }

	    // start heartbeat
	    ci = new HBClientInfo(id, period, nretries, notifSessionId);

	    synchronized(clients) {
		clients.put(id, ci);
	    }

	    ci.startWaitPing();

	    return id;
	}

	// for connected client
	if (period <= 0) { // stop heartbeat
	    
	    synchronized(clients) {
		clients.remove(id);
	    }

	    ci.stopWaitPing(-1);

	    return id;
	}
	 
	// Update values
	//
	ci.setPeriod(period);
	ci.setNRetries(nretries);
	ci.setNotifSessionId(notifSessionId);

	// Restart thread
	//
	ci.startWaitPing();

	return id;
    }

    public void cleanup(String sessionId) {
	// Get entry from clients hashtable
	//
	HBClientInfo ci;
	synchronized(clients) {
	    ci = (HBClientInfo) clients.remove(sessionId);
	}

	if (ci != null) {

	    // Get notifSessionId from HBClientInfo entry
	    //
	    Long id = ci.getNotifSessionId();

	    // Clean up associated notification client entry
	    // in notification server dispatcher
	    //
	    if (id != null) {
		snd.remoteTerminate(id);
	    }
	}
    }

    public void cleanupClientResources() {
	synchronized(clients) {
	    for (Enumeration e = clients.elements(); e.hasMoreElements(); ) {
		HBClientInfo ci = (HBClientInfo) e.nextElement();
		ci.stopWaitPing(-1);
	    }
        }
    }

    /**
     * Get unique session id.
     */
    private static String getSessionId() {
	synchronized(HeartBeatServerHandler.class) {
	    return uniqueId = counter++ + "_" + mbsId;
	}
    }

    // INNER CLASS
    //------------

    /**
     * This class contains the info for a given client.
     */
    private class HBClientInfo {

	public HBClientInfo(String hbSessionId, int period, int nretries, Long notifSessionId) {
	    this.hbSessionId = hbSessionId;
	    this.period = period;
	    this.nretries = nretries;
	    this.notifSessionId = notifSessionId;
	}

	public int getPeriod() {
	    return period;
	}

	public void setPeriod(int period) {
	    this.period = period;
	}

	public int getNRetries() {
	    return nretries;
	}

	public void setNRetries(int nretries) {
	    this.nretries = nretries;
	}

	public Long getNotifSessionId() {
	    return notifSessionId;
	}

	public void setNotifSessionId(Long notifSessionId) {
	    this.notifSessionId = notifSessionId;
	}

	public synchronized void startWaitPing() {
	    // Calculate new timeout
	    //
	    if (nretries == 0) {
		timeout = (long) (period + (1.20 * period));
	    } else {
		timeout = (long) (period + (1.20 * (period * nretries)));
	    }

	    // Calculate new thread end time
	    //
	    newThreadEndTime = System.currentTimeMillis() + timeout;

	    // Start waiting ping if not alive
	    //
	    if (threadWaitPing == null || !threadWaitPing.isAlive()) {
		threadWaitPing = new WaitPing();
		threadWaitPing.start();
	    }
	}

	public synchronized void stopWaitPing(int action) {
	    if (threadWaitPing != null) {
		threadWaitPing.terminate(action);

// 		threadWaitPing.interrupt();
		threadWaitPing = null;
	    }
	}

	public void cleanupHBClientInfo() {
	    cleanup(hbSessionId);
	}

	private class WaitPing extends Thread {

	    public void run() {
		while (!toBeTerminated && timeout > 0) {
		    // Sleep for a while
		    //
		    try {
			synchronized(lock) {
			    lock.wait(timeout);
			}
		    } catch (InterruptedException ie) {
			if (toBeTerminated) {
			    break;
			}
		    }

		    // Get actual thread end time
		    //
		    actualThreadEndTime = System.currentTimeMillis();

		    // Compare times and see if we have to clean up
		    //
		    if (actualThreadEndTime > newThreadEndTime) {
			// If toBeTerminated equals true then don't clean up
			// because cleanup was already done by stopWaitPing
			//
			if (!toBeTerminated) {
			    // Clean up
			    //
			    toBeTerminated = true;
			    if (wpLogger.finerOn())
				wpLogger.finer("run", 
                                "Cleaning up: Client with SessionId = " + 
                                hbSessionId + " died.");
			    cleanupHBClientInfo();
			}
		    }
		}
	    }

	    public void terminate(int action) {
		// Clean up
		//
		toBeTerminated = true;
		if (action == -1) {
		    if (wpLogger.finerOn())
			wpLogger.finer("terminate", 
                        "Cleaning up: Client with SessionId = " + 
                        hbSessionId + " disconnected.");
		    cleanupHBClientInfo();

		    synchronized(lock) {
			lock.notify();
		    }

		} else {
		    if (wpLogger.finerOn())
			wpLogger.finer("terminate", 
                        "Ping stopped for client with SessionId = " + 
			hbSessionId);
		}
	    }

	    private long actualThreadEndTime = 0;
	    private boolean toBeTerminated = false;

	    private final int[] lock = new int[0];
	}

	private String hbSessionId;
	private int period;
	private int nretries;
	private Long notifSessionId;
	private long timeout;
	private long newThreadEndTime;
	private WaitPing threadWaitPing;
    }

    // PRIVATE VARIABLES
    //------------------

    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_LEGACY_HEARTBEAT,
			"HeartBeatServerHandler");
    private static final ClassLogger wpLogger = 
	new ClassLogger(ClassLogger.LOGGER_LEGACY_HEARTBEAT,
			"WaitPing");

    /**
     * MBeanServer reference.
     */
    private MBeanServer mbs;

    /**
     * MBeanServerId.
     */
    private static String mbsId;

    /**
     * ServerNotificationDispatcher reference.
     */
    private ServerNotificationDispatcher snd;

    /**
     * HeartBeat Session ID (SeqNumber).
     */
    private static long counter = 0;

    /**
     * HeartBeat Session ID (SeqNumber + MBeanServerId).
     */
    private static String uniqueId;

    /**
     * HeartBeat Client List.
     */
    private Hashtable clients = new Hashtable();
}
