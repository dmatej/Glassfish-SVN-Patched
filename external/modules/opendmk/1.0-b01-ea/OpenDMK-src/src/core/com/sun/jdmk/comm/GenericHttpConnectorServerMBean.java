/*
 * @(#)file      GenericHttpConnectorServerMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.11
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


// @(#)GenericHttpConnectorServerMBean.java 1.11 07/03/08 

/**
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This class may be removed in a
 * future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 */
public interface GenericHttpConnectorServerMBean 
    extends CommunicatorServerMBean {

    /**
     * Returns true if the list of users supported by this connector 
     * server is not empty.
     *
     * @return <code>true</code>, if the list of users supported by this 
     *         server is not empty; <code>false</code>, if the list of 
     *         supported users is empty so no authentication is performed 
     *         by this server.
     */
    public boolean isAuthenticationOn();

    /**
     * Gets the IP address of the last connected client.
     * This function uses the string representation of 
     * <CODE>java.net.InetAddress</CODE>.
     *
     * @return The IP address of the last connected client.
     */
    public String getLastConnectedClient();

    /**
     * Gets the number of clients that have been processed by this connector
     * since its creation.
     *
     * @return The number of clients handled by this connector since its 
     *         creation.
     * This counter is not reset by the <CODE>stop</CODE> method.
     */
    public int getServedClientCount();

    /**
     * Gets the number of clients currently being processed by this connector.
     *
     * @return The number of clients currently being processed by 
     * this connector.
     */
    public int getActiveClientCount();

    /**
     * Gets the maximum number of clients that this connector can process 
     * concurrently.
     *
     * @return The maximum number of clients that this connector can
     * process concurrently.
     */
    public int getMaxActiveClientCount();

    /**
     * Sets the maximum number of clients this connector can process 
     * concurrently.
     *
     * @param c The number of clients.
     *
     * @exception java.lang.IllegalStateException This method has been 
     *            invoked while this connector was ONLINE or STARTING.
     */
    public void setMaxActiveClientCount(int c) 
	throws java.lang.IllegalStateException;
}
