/*
 * @(#)file      DiscoveryMonitorMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.13
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

// ----------------------------------------------------------
// java import
// ----------------------------------------------------------
import java.util.*;
import java.io.*;
import java.lang.*;
import java.net.*;

// ----------------------------------------------------------
// jdmk import
// ----------------------------------------------------------
import javax.management.*;
import com.sun.jdmk.* ; 


  
/**
 * Describe an MBean that listens for registering and unregistering information sent by
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} objects on a given multicast group.
 * Any agent that is to use multicast discovery must have a
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} registered in its MBean server.
 * When a {@link com.sun.jdmk.discovery.DiscoveryResponder} is registered in an MBean server and when its start or stop methods
 * are called, it informs the rest of the multicast group by sending
 * a multicast message. The format of this message is not exposed.
 * Whenever a <CODE>DiscoveryMonitor</CODE> receives a registration or
 * unregistration message, it sends a {@link com.sun.jdmk.discovery.DiscoveryResponderNotification}
 * to its notification listener.
 * <p>
 * A <CODE>DiscoveryMonitor</CODE> can be instantiated either in stand alone
 * mode (Client side) or added to an MBean Server. In the first case, the client should
 * call the appropriate constructor to initialize the <CODE>multicastGroup</CODE>
 * and <CODE>multicastPort</CODE> parameters.
 * The default values for the group and the port are 224.224.224.224 and
 * 9000.
 *
 * A <CODE>DiscoveryMonitor</CODE> can be stopped by calling the <CODE>stop</CODE> method. When it is stopped, the
 * <CODE>DiscoveryMonitor</CODE> no longer listens for registering and
 * unregistering messages from {@link com.sun.jdmk.discovery.DiscoveryResponder} objects.
 * A <CODE>DiscoveryMonitor</CODE> can be restarted by invoking the <CODE>start</CODE> method.
 * <p>
 * A <CODE>DiscoveryMonitor</CODE> has a <CODE>state</CODE> property which reflects its
 * activity.
 * <TABLE>
 * <TR><TH>DiscoveryMonitor</TH>                 <TH>State</TH></TR>
 * <TR><TD><CODE>running</CODE></TD>          <TD><CODE>ONLINE</CODE></TD></TR>
 * <TR><TD><CODE>stopped</CODE></TD>          <TD><CODE>OFFLINE</CODE></TD></TR>
 * <TR><TD><CODE>stopping</CODE></TD>         <TD><CODE>STOPPING</CODE></TD></TR>
 * </TABLE>
 * <p>
 * The transition between <CODE>ONLINE</CODE> and <CODE>OFFLINE</CODE> may not
 * be immediate. The <CODE>DiscoveryMonitor</CODE> may need some time to finish
 * or interrupt the active requests. During this time the state of the
 * <CODE>DiscoveryMonitor</CODE> is <CODE>STOPPING</CODE>.
 * When a <CODE>DiscoveryMonitor</CODE> is removed from a Java DMK agent, it is automatically stopped.
 *
 */

public interface DiscoveryMonitorMBean {




    // ----------------------------------------------------------
    // start method
    // ----------------------------------------------------------
    /**
     * Starts listening for {@link com.sun.jdmk.discovery.DiscoveryResponder} objects registering/unregistering.
     * <P>
     * This method has no effect if the <CODE>DiscoveryMonitor</CODE> is <CODE>ONLINE</CODE> or
     * <CODE>STOPPING</CODE>.
     *   
     * @exception IOException The creation of the Multicast socket failed.
     */  

    public void start() throws IOException ;


    // ----------------------------------------------------------
    // stop method
    // ----------------------------------------------------------
    /**
     * Stops this <CODE>DiscoveryMonitor</CODE>.
     * <P>
     * This method has no effect if the monitor is <CODE>OFFLINE</CODE> or
     * <CODE>STOPPING</CODE>.
     */
 
    public void stop();

    // ----------------------------------------------------------
    // getter / setter
    // ----------------------------------------------------------
    /**
     * Returns the state of this <CODE>DiscoveryMonitor</CODE>.
     *
     * @return <CODE>ONLINE</CODE>,<CODE>OFFLINE</CODE> or <CODE>STOPPING</CODE>.
     */

    public Integer getState() ; 

    /**
     * Returns the state of this <CODE>DiscoveryMonitor</CODE> in string form.
     *
     * @return One of the strings "ONLINE", "OFFLINE" or "STOPPING".
     */

    public String getStateString() ;

    /**
     * Returns the multicast group.
     *
     * @return A string containing the multicast group name.
     */
    public String getMulticastGroup() ;

    /**
     * Sets the multicast group name.
     ** A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
     * <P>
     * Only available if state in OFFLINE
     *
     * @param multicastGroup The multicast group name.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while
     *            the <CODE>DiscoveryMonitor</CODE> was ONLINE or STARTING.
     */
    public void setMulticastGroup(String multicastGroup) throws java.lang.IllegalStateException ;

    /**
     * Returns the multicast port.
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
     *            the <CODE>DiscoveryMonitor</CODE> was ONLINE or STARTING.
     */
    public void setMulticastPort(int multicastPort) throws java.lang.IllegalStateException  ;

   /**
     * Waits until either the State attribute of this MBean equals the 
     * specified <VAR>state</VAR> parameter,  or the specified  
     * <VAR>timeout</VAR> has elapsed. The method <CODE>waitState</CODE> 
     * returns with a boolean value indicating whether the specified 
     * <VAR>state</VAR> parameter equals the value of this MBean's State 
     * attribute at the time the method terminates.
     *
     * Two special cases for the <VAR>timeout</VAR> parameter value are:
     * <UL><LI> if <VAR>timeout</VAR> is negative then <CODE>waitState</CODE>
     *     returns immediately (i.e. does not wait at all),</LI>
     * <LI> if <VAR>timeout</VAR> equals zero then <CODE>waitState</CODE> 
     *      waits until the value of this MBean's State attribute 
     *      is the same as the <VAR>state</VAR> parameter (i.e. will wait 
     *      indefinitely if this condition is never met).</LI></UL>
     * 
     * @param state The value of this MBean's State attribute to wait for. 
     *        <VAR>state</VAR> can be one of:
     *        <CODE>DiscoveryMonitor.OFFLINE</CODE>, 
     *        <CODE>DiscoveryMonitor.ONLINE</CODE>,
     *        <CODE>DiscoveryMonitor.STARTING</CODE>, 
     *        <CODE>DiscoveryMonitor.STOPPING</CODE>.
     * @param timeout The maximum time to wait for, in 
     *        milliseconds, if positive. Infinite time out if 0, or no 
     *        waiting at all if negative.
     *
     * @return <code>true</code> if the value of this MBean's State attribute
     *         is the same as the <VAR>state</VAR> parameter; 
     *         <code>false</code> otherwise.
     */
    public boolean waitState(int state, long timeout);
}
