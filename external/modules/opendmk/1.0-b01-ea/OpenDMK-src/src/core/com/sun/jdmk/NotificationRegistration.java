/* 
 * @(#)file      NotificationRegistration.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.8 
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

package com.sun.jdmk; 

// jmx import
//
import javax.management.ObjectName;
import javax.management.NotificationListener;
import javax.management.NotificationFilter;
import javax.management.InstanceNotFoundException;
import javax.management.ListenerNotFoundException;

/**
 * This interface specifies the methods to add or to remove a notification listener
 * to an MBean via a MBean server (remote or local).
 */

public interface NotificationRegistration { 

    /**
     * Adds a listener to a registered MBean.
     *
     * @param name The objectname of the MBean on which the listener should be added.
     * @param listener The listener which will handle the notifications emitted by the registered MBean.
     * @param filter The filter used to do filtering. If filter is null, no filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     */
    public void addNotificationListener(ObjectName name, NotificationListener listener, NotificationFilter filter, Object handback)
        throws InstanceNotFoundException;

    /**
     * Removes a listener from a registered MBean. It will remove all the information related to this listener.
     *
     * @param name The objectname of the MBean on which the listener should be removed.
     * @param listener The listener which will handle the notifications emitted by the registered MBean.
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not registered in the MBean.
     */
    public void removeNotificationListener(ObjectName name, NotificationListener listener) 
        throws InstanceNotFoundException, ListenerNotFoundException;
}
