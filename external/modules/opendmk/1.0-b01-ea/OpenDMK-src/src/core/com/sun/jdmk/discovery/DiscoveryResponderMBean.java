/*
 * @(#)file      DiscoveryResponderMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.15
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

package com.sun.jdmk.discovery;


import java.util.*;
import java.net.*;
import java.io.*;


// ------------------------
// jdmk import
// ------------------------
import javax.management.*;
import com.sun.jdmk.* ; 



/**
 * Implements the MBean that responds to the discovery requests. Any agent that needs to be discovered
 * must instantiate and register a <CODE>DiscoveryResponder</CODE> in its
 * MBean server.
 *
 * <p>
 * When the <CODE>DiscoveryResponder</CODE> <CODE>start</CODE> method is called, the MBean
 * creates a multicast socket. The <CODE>DiscoveryResponder</CODE> then sends a join message
 * to the multicast group.
 * When a <CODE>DiscoveryResponder</CODE> is unregistered from the MBean server,
 * or when <CODE>stop</CODE> method is called, the MBean
 * sends a leave message to the multicast group. The format of these messages
 * is not exposed. These messages allow {@link com.sun.jdmk.discovery.DiscoveryMonitor} objects to
 * maintain a list of agents with <CODE>DiscoveryResponder</CODE> objects
 * registered in their MBean server.
 * When <CODE>start</CODE> method is called, and when a
 * join message has been sent, the <CODE>DiscoveryResponder</CODE> starts to listen for
 * discovery requests.
 * <p>
 * The multicast socket uses the group and port specified by the
 * properties <CODE>multicastGroup</CODE> and <CODE>multicastPort</CODE>.
 * The default values for the group and the port are 224.224.224.224 and 9000.
 * These values can be changed using appropriate constructor.
 * These values can be also changed using <CODE>setMulticastGroup</CODE> and
 * <CODE>setMulticastPort</CODE> methods when the <CODE>DiscoveryResponder</CODE> is OFFLINE.
 * <p>
 * When join/leave message are sent to the multicast group, a default
 * time-to-live (see <CODE>java.net.MulticastSocket</CODE>) value is used. The
 * time-to-live value specifies how many "hops" that the packet is forwarded on
 * the network before it expires.
 * <CODE>DiscoveryResponder</CODE> objects use a time-to-live specified by the property
 * <CODE>ttl</CODE>.
 * The default time-to-live value is 1. It can be changed using <CODE>setTimeToLive</CODE>
 * method when the <CODE>DiscoveryResponder</CODE> is OFFLINE.
 * 
 */

public interface DiscoveryResponderMBean {


    // ----------------------------------------------------------
    // ActivatableIf implementation
    // ----------------------------------------------------------
    
    /**
     * Create a multicast socket and join the multicast group.
     * This method creates a multicast socket that is used to broadcast
     * The <CODE>DiscoveryResponder</CODE> will then join the multicast group and send a join message.
     *
     * @exception IOException The creation of the Multicast socket failed.
     */
    public void start() throws IOException ;

    /**
     * Sends a leave message to the multicast group and leaves it.
     * The <CODE>DiscoveryResponder</CODE> leaves its multicast group.
     */
    public void stop() ;

    /**
     * Tests if the <CODE>DiscoveryResponder</CODE> is active.
     * <CODE>True</CODE> is returned if the <CODE>DiscoveryResponder</CODE> is started (<CODE>DiscoveryResponder</CODE>
     * has join the multicast group).
     */
    public boolean isActive() ;

    /**
     * Waits until either the State attribute of this MBean equals the 
     * specified <VAR>state</VAR> parameter, or the specified  
     * <VAR>timeout</VAR> has elapsed. The method <CODE>waitState</CODE> 
     * returns with a boolean value indicating whether the specified 
     * <VAR>state</VAR> parameter equals the value of this MBean's State 
     * attribute at the time the method terminates.
     *
     * Two special cases for the <VAR>timeout</VAR> parameter value are:
     * <UL><LI> if <VAR>timeout</VAR> is negative then <CODE>waitState</CODE>
     *     returns immediately (i.e. does not wait at all),</LI>
     * <LI> if <VAR>timeout</VAR> equals zero then <CODE>waitState</CODE> 
     *     waits until the value of this MBean's State attribute 
     *     is the same as the <VAR>state</VAR> parameter (i.e. will wait 
     *     indefinitely if this condition is never met).</LI></UL>
     * 
     * @param state The value of this MBean's State attribute 
     *        to wait for. <VAR>state</VAR> can be one of:
     *        <CODE>CommunicatorServer.OFFLINE</CODE>, 
     *        <CODE>CommunicatorServer.ONLINE</CODE>,
     *        <CODE>CommunicatorServer.STARTING</CODE>, 
     *        <CODE>CommunicatorServer.STOPPING</CODE>.
     * @param timeout The maximum time to wait for, in 
     *        milliseconds, if positive. 
     *        Infinite time out if 0, or no waiting at all if negative.
     *
     * @return <code>true</code> if the value of this MBean's State attribute
     *         is the same as the <VAR>state</VAR> parameter; 
     *         <code>false</code> otherwise.
     */
    public boolean waitState(int state, long timeout) ;

    // ----------------------------------------------------------
    // Getters and Setters
    // ----------------------------------------------------------
 

    /**
     * Returns the multicast group.
     * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     *
     * @return A string containing the multicast group name.
     */
    public String getMulticastGroup() ;


    /**
     * Sets the multicast group name.
     * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     * <P>
     * Only available if state in OFFLINE
     *   
     * @param multicastGroup The multicast group name.
     *   
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryResponder</CODE> was ONLINE or STARTING.
     */  
    public void setMulticastGroup(String multicastGroup)
    throws java.lang.IllegalStateException ;


    /**
     * Returns the multicast port.
     * It can be any standard UDP port number.
     *
     * @return The multicast port number.
     */
    public int getMulticastPort() ;

 
    /**
     * Sets the multicast port.
     * It can be any standard UDP port number.
     * <P>
     * Only available if state in OFFLINE
     *
     * @param multicastPort The multicast port.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryResponder</CODE> was ONLINE or STARTING.
     */
    public void setMulticastPort(int multicastPort) 
    throws java.lang.IllegalStateException ;




    /**
     * Returns the time-to-live value.
     *
     * @return The time-to-live value.
     */
    public int getTimeToLive()  ;

    /**
     * Sets the default Time to Live to be used to send join and leave message to the Multicast group.
     * <P>
     * Time to Live should an integer verifying the following condition: 0 < ttl <= 255
     * Only available if state in OFFLINE
     *
     * @param ttl The Time to live value.
     *
     * @exception java.lang.IllegalArgumentException The input ttl value is not
     *            in the 1 to 255 range.
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryResponder</CODE> was ONLINE or STARTING.
     */
    public void setTimeToLive(int ttl)
    throws java.lang.IllegalStateException ;


    /**
     * Returns the state of this <CODE>DiscoveryResponder</CODE>.
     *
     * @return <CODE>ONLINE</CODE>, <CODE>OFFLINE</CODE> or <CODE>STOPPING</CODE>.
     */
    public Integer getState()  ;
 
    /**
     * Returns the state of this <CODE>DiscoveryResponder</CODE> in string form.
     * 
     * @return One of the strings "ONLINE", "OFFLINE" or "STOPPING".
     */
    public String getStateString()  ;

    /**
     * Allows the user to specify additional information in the
     * <CODE>DiscoveryResponse</CODE> message.
     *
     * The following limitation applies to the length of the byte array
     * parameter:
     * The length of a UDP packet is maximum 64 Kbytes. In addition to the
     * specified user data, the discovery response UDP packet contains the
     *  following :
     * <UL> 
     * <LI>  All string attributes of the MBean server delegate MBean (in
     * the following order: the MBeanServerId; the specification
     * name, vendor and version; and the implementation name, vendor
     * and version).
     * <LI> The ObjectName and the ConnectorAddresses for each of the 
     * connectors/adaptors registered in the MBean server.
     * </UL>
     * The user should take into consideration this content whose size is
     * dependent upon the delegate's string attributes and the number of
     * connectors/adaptors registered in an MBean server. The provided byte
     * array should not exceed the space remaining up to the UDP packet's 64
     * Kbyte limit. Otherwise, the packet is truncated and information will be
     * lost.
     *
     * For example, if you wish to allow enough space to include up to 100
     * connectors/adaptors in the discovery response message, the data byte
     * array should not exceed 40 KBytes, approximately.
     *
     */
    public void setUserData(byte[] data) ;

    /**
     * Returns a byte[] containing the additional information that the user added in the 
     * <CODE>DiscoveryResponse</CODE>.
     */
    public byte[] getUserData();
}
