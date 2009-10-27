/*
 * @(#)file      ClientNotificationHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.29
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
 */

package com.sun.jdmk.comm;

// javax import
//
import javax.management.*;


// jdmk import
//
import com.sun.jdmk.NotificationRegistration;

/**
 * This interface specifies the methods for a connector to allow a listener to
 * receive notifications from a remote MBean in an agent.
 * <P>To forward a notification from the agent to the connector client, the user can configure the
 * connector to use either push or pull mode.
 * <UL><LI>In pull mode, the connector server on the agent side
 * buffers all notifications until the connector client requests,
 * or "pulls," them. The caller can configure the buffer cache size
 * and overflow behavior in the connector server. The caller can
 * also set the forwarding period to control how often
 * notifications are pulled from the connector server.</LI>
  
 * <LI>In push mode, the connector server forwards each and every
 * notification to the connector client as it is received. In the
 * current implementation, the cache and forwarding period are not
 * used in push mode--notifications are forwarded immediately.</LI></UL>
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connectors.  This interface may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 */
public interface ClientNotificationHandler extends NotificationRegistration {
	/**
	 * This constant is used to set the push mode for notification forwarding.
	 */
	public static final int PUSH_MODE	= 0;

	/**
	 * This constant is used to set the pull mode for notification forwarding.
	 */
	public static final int PULL_MODE	= 1;

	/**
	 * This constant controls the cache behavior for notification forwarding. If the cache mode is set to this value
	 * and when the notification cache size is exceeded, the older messages will be discarded.
	 * This is default value for the cache behavior for notification forwarding.
	 */
	public static final int DISCARD_OLD	= 10;

	/**
	 * This constant controls the cache behavior for notification
	 * forwarding.  If the cache mode is set to this value and
	 * when the notification cache size is exceeded, the newer
	 * messages will be discarded.
	 */
	public static final int DISCARD_NEW	= 11;

	/**
	 * This constant controls the cache behavior for notification forwarding. If the cache size is set to this value,
	 * there is no limitation of the notification cache size and notifications will never be discarded.
	 * This is default value for notification forwarding.
	 */
	public static final int NO_CACHE_LIMIT	= -1;

    /**
     * Sets the notification forwarding mode.
     * If set to <CODE>PUSH_MODE</CODE>, it is the agent to push notifications to the client, if set to
     * <CODE>PULL_MODE</CODE>, it is the client to retrieve notifications from the agent.
     * <P>The default value is <CODE>PUSH_MODE</CODE>.
     *
     * @param mode set to <CODE>PUSH_MODE</CODE> or <CODE>PULL_MODE</CODE>.
     * @exception IllegalArgumentException Thrown if the mode is not equal to
     * <CODE>PUSH_MODE</CODE> nor <CODE>PULL_MODE</CODE>.
     */
    public void setMode(int mode) throws IllegalArgumentException;

    /**
     * Gets the notification forwarding mode.
     * If set to <CODE>PUSH_MODE</CODE>, it is the agent to push notifications to the client, if set to
     * <CODE>PULL_MODE</CODE>, it is the client to retrieve notifications from the agent.
     * <P>The default value is <CODE>PUSH_MODE</CODE>.
     */
    public int getMode();

    /**
     * Retrieves all notifications in the cache.
     */
    public void getNotifications();

	/**
	 * Clear the notification cache. All notifications stored in the cache then will be discarded
	 * without being sent.
	 */
	public void clearCache();

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
    public void setPeriod(int period);

    /**
     * Gets the period for notification forwarding in milliseconds.
     * <P>
     * The default value is 1000 milliseconds.
     */
    public int getPeriod();

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
	 * @return The cache size currently set.
	 */
	public int setCacheSize(int size, boolean discardOverflow);

	/**
	 * Gets the cache size of notifications waiting to be forwarded.
	 * <P>If set to <CODE>NO_CACHE_LIMIT</CODE>, notifications will never be discarded,
	 * but this may lead to OutOfMemory errors under stressed conditions.
	 * <P>The default value is <CODE>NO_CACHE_LIMIT</CODE>.
	 */
	public int getCacheSize();

	/**
	 * Sets the number of notifications discarded, this number indicates the number
	 * of notifications discarded because the cache limit has been reached.
	 * <P>This count will be reset to zero if no more listener exists at the client side,
	 * because in this case the notification server will remove all information about
	 * this notification client.
	 *
	 * @param count The new value to set to overflow count.
	 */
	public void setOverflowCount(int count);

	/**
	 * Gets the number of notifications discarded because the cache limit has been reached.
	 * This value can be reset by calling the method setOverFlowCount.
	 * <P>This count will be reset to zero if no more listener exists at the client side,
         * because in this case the notification server will remove all information about 
         * this notification client.
	 */
	public int getOverflowCount();

	/**
	 * Specifies whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the
	 * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds.
	 * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
	 *
	 * @param of The mode to specify.
	 * @exception IllegalArgumentException Thrown if the mode is not <CODE>DISCARD_NEW</CODE>
	 * nor <CODE>DISCARD_OLD</CODE>.
	 */
	public void setOverflowMode(int of) throws IllegalArgumentException;

	/**
	 * Returns whether to discard the oldest message (<CODE>DISCARD_OLD</CODE>) or the
	 * the newest message (<CODE>DISCARD_NEW</CODE>), if the cache size exceeds.
	 * <P> The default mode is <CODE>DISCARD_OLD</CODE>.
	 */
	public int getOverflowMode();

}
