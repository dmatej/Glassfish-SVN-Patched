/*
 * @(#)file      HeartBeatNotification.java
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
import javax.management.Notification;

/**
 * This notification will be sent by a connector client when the connector client itself connects to
 * or disconnects from the connector server or when the connector server associated to this connector
 * client dies or is temporarily unreachable and after several retries have been carried out unsuccessfully.
 *
 */

public class HeartBeatNotification extends Notification {
    private static final long serialVersionUID = -3316433306248507512L;

    /**
     * Notification type denoting that the connector client specified as the source object has been connected
     * to its corresponding connector server.
     * <P>
     * The value of this notification type is <CODE>jdmk.connector.heartbeat.connection.established</CODE>.
     */
    public static final String CONNECTION_ESTABLISHED = "jdmk.connector.heartbeat.connection.established";

    /**
     * Notification type denoting that the connector server associated to the connector client specified
     * as the source object in this notification is temporarily unreachable but the connector client is
     * trying to reestablish the connection.
     * <P>
     * The value of this notification type is <CODE>jdmk.connector.heartbeat.connection.retrying</CODE>.
     */
    public static final String CONNECTION_RETRYING = "jdmk.connector.heartbeat.connection.retrying";

    /**
     * Notification type denoting that the connector server associated to the connector client specified
     * as the source object in this notification has died.
     * <P>
     * The value of this notification type is <CODE>jdmk.connector.heartbeat.connection.lost</CODE>.
     */
    public static final String CONNECTION_LOST = "jdmk.connector.heartbeat.connection.lost";

    /**
     * Notification type denoting that the connector server associated to the connector client specified
     * as the source object in this notification was temporarily unreachable but connection has been
     * reestablished successfully.
     * <P>
     * The value of this notification type is <CODE>jdmk.connector.heartbeat.connection.reestablished</CODE>.
     */
    public static final String CONNECTION_REESTABLISHED = "jdmk.connector.heartbeat.connection.reestablished";

    /**
     * Notification type denoting that the connector client specified as the source object has been disconnected
     * from its corresponding connector server.
     * <P>
     * The value of this notification type is <CODE>jdmk.connector.heartbeat.connection.terminated</CODE>.
     */
    public static final String CONNECTION_TERMINATED = "jdmk.connector.heartbeat.connection.terminated";

    /**
     * Heartbeat notification connector address.
     */
    private ConnectorAddress connAddr = null;

    /**
     * Constructs a heartbeat notification object.
     * In addition to the information common to all the notifications, the caller must supply
     * the connector address identifying the connector server to which the connector client
     * sending this notification is connected to.
     *
     * @param type The notification type.
     * @param source The notification producer, i.e. the connector client.
     * @param sequenceNumber The notification sequence number within the source object.
     * @param timeStamp The date at which the notification is being sent.
     * @param msg A string containing the message of the notification.
     * @param connAddr A ConnectorAddress object describing the connector server.
     */
    public HeartBeatNotification(String type, Object source, long sequenceNumber, long timeStamp, String msg, ConnectorAddress connAddr) {
	super(type, source, sequenceNumber, timeStamp, msg);
        this.connAddr = connAddr;
    }

    /**
     * Gets the connector address of this heartbeat notification.
     *
     * @return The connector address of the connector server.
     */
    public ConnectorAddress getConnectorAddress() {
	return connAddr;
    }

}
