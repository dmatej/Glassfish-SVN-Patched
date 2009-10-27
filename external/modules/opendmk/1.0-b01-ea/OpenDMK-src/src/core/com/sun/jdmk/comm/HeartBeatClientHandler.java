/*
 * @(#)file      HeartBeatClientHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.5
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

// jmx import
//
import javax.management.NotificationFilter;
import javax.management.NotificationListener;

/**
 * Interface to be implemented by all the connector clients that want to support
 * the heartbeat mechanism.
 * <P>
 * This interface provides methods for configuring the heartbeat functionality
 * from a manager.
 *
 */

public interface HeartBeatClientHandler {

    /**
     * Gets the heartbeat period in milliseconds.
     */
    public int getHeartBeatPeriod();

    /**
     * Specifies the heartbeat period in milliseconds.
     * <P>
     * If set to zero no check will be carried out for the associated connector server being alive.
     * As the heartbeat is driven by the manager this would also prevent the connector server from
     * being aware of the aliveness of this connector client.
     *
     * @param period The heartbeat period in milliseconds.
     */
    public void setHeartBeatPeriod(int period);

    /**
     * Gets the number of retries. This number specifies how many times a connector client must retry
     * the connection to the connector server before sending the heartbeat notification indicating that
     * the connector server has died. If number of retries equals zero then no retries are carried out.
     */
    public int getHeartBeatRetries();

    /**
     * Sets the number of retries. This number specifies how many times a connector client must retry
     * the connection to the connector server before sending the heartbeat notification indicating that
     * the connector server has died. If number of retries equals zero then no retries are carried out.
     *
     * @param nretries The number of retries.
     */
    public void setHeartBeatRetries(int nretries);

    /**
     * Adds the specified heartbeat listener to receive heartbeat notifications from this connector client.
     * Heartbeat notifications occur when the connector client connects to or disconnects from the connector
     * server or when the connector server associated to this connector client dies or is temporarily unreachable.
     *
     * @param listener The heartbeat listener which will handle the notifications emitted by the connector client.
     * @param filter The filter object. If filter is null, no filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a notification is emitted.
     */
    public void addHeartBeatNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback);

    /**
     * Removes the specified heartbeat listener so that it no longer receives heartbeat notifications from
     * this connector client.
     * Heartbeat notifications occur when the connector client connects to or disconnects from the connector
     * server or when the connector server associated to this connector client dies or is temporarily unreachable.
     *
     * @param listener The heartbeat listener which will handle the notifications emitted by the connector client.
     */
    public void removeHeartBeatNotificationListener(NotificationListener listener);

}
