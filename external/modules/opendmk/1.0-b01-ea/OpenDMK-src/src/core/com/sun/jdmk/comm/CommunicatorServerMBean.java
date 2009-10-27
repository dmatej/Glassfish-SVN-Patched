/*
 * @(#)file      CommunicatorServerMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.26
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
 *
 */


package com.sun.jdmk.comm;



/**
 * Defines generic behavior for the server 
 * part of a connector or an adaptor. Most connectors or adaptors extend <CODE>CommunicatorServer</CODE>
 * and inherit this behavior. Connectors or adaptors that do not fit into this model do not extend 
 * <CODE>CommunicatorServer</CODE>.
 * <p>
 * An <CODE>CommunicatorServer</CODE> is an active object, it listens for client requests
 * and processes them in its own thread. When necessary, a <CODE>CommunicatorServer</CODE>
 * creates other threads to process multiple requests concurrently.
 * <p>
 * A <CODE>CommunicatorServer</CODE> object can be stopped by calling the <CODE>stop</CODE>
 * method. When it is stopped, the <CODE>CommunicatorServer</CODE> no longer listens to client 
 * requests and no longer holds any thread or communication resources.
 * It can be started again by calling the <CODE>start</CODE> method.
 * <p>
 * A <CODE>CommunicatorServer</CODE> has a <CODE>state</CODE> property which reflects its 
 * activity.
 * <p>
 * <TABLE>
 * <TR><TH>CommunicatorServer</TH>            <TH>State</TH></TR>
 * <TR><TD><CODE>stopped</CODE></TD>          <TD><CODE>OFFLINE</CODE></TD></TR>
 * <TR><TD><CODE>starting</CODE></TD>         <TD><CODE>STARTING</CODE></TD></TR>
 * <TR><TD><CODE>running</CODE></TD>          <TD><CODE>ONLINE</CODE></TD></TR>
 * <TR><TD><CODE>stopping</CODE></TD>         <TD><CODE>STOPPING</CODE></TD></TR>
 * </TABLE>
 * <p>
 * The <CODE>STARTING</CODE> state marks the transition from <CODE>OFFLINE</CODE> to
 * <CODE>ONLINE</CODE>.
 * <p>
 * The <CODE>STOPPING</CODE> state marks the transition from <CODE>ONLINE</CODE> to
 * <CODE>OFFLINE</CODE>. This occurs when the <CODE>CommunicatorServer</CODE> is 
 * finishing or interrupting active requests.
 * <p>
 * A <CODE>CommunicatorServer</CODE> may serve several clients concurrently. The 
 * number of concurrent clients can be limited using the property 
 * <CODE>maxActiveClientCount</CODE>. The default value of this property is
 * defined by the subclasses.
 * <p>
 * When a <CODE>CommunicatorServer</CODE> is unregistered from the MBeanServer,
 * it is stopped automatically.
 *
 */

public interface CommunicatorServerMBean {

    /**
     * Starts this <CODE>CommunicatorServer</CODE>.
     * <p>
     * Has no effect if this <CODE>CommunicatorServer</CODE> is <CODE>ONLINE</CODE> or 
     * <CODE>STOPPING</CODE>.
     */
    public void start() ;

    /**
     * Stops this <CODE>CommunicatorServer</CODE>.
     * <p> 
     * Has no effect if this <CODE>CommunicatorServer</CODE> is <CODE>OFFLINE</CODE> or 
     * <CODE>STOPPING</CODE>.
     */
    public void stop() ;

    /**
     * Tests if the <CODE>CommunicatorServer</CODE> is active.
     *
     * @return True if connector is <CODE>ONLINE</CODE>; false otherwise.
     */
    public boolean isActive() ;

    /**
     * Waits until either the State attribute of this MBean equals the 
     * specified <VAR>state</VAR> parameter, 
     * or the specified  <VAR>timeout</VAR> has elapsed. The method 
     * <CODE>waitState</CODE> returns with a boolean value indicating whether
     * the specified <VAR>state</VAR> parameter equals the value of this 
     * MBean's State attribute at the time the method terminates.
     *
     * Two special cases for the <VAR>timeout</VAR> parameter value are:
     * <UL><LI> if <VAR>timeout</VAR> is negative then <CODE>waitState</CODE> 
     * returns immediately (i.e. does not wait at all),</LI>
     * <LI> if <VAR>timeout</VAR> equals zero then <CODE>waitState</CODE> 
     * waits until the value of this MBean's State attribute 
     * is the same as the <VAR>state</VAR> parameter (i.e. will wait 
     * indefinitely if this condition is never met).</LI></UL>
     * 
     * @param state The value of this MBean's State attribute to wait for. 
     * <VAR>state</VAR> can be one of:
     * <CODE>CommunicatorServer.OFFLINE</CODE>, 
     * <CODE>CommunicatorServer.ONLINE</CODE>,
     * <CODE>CommunicatorServer.STARTING</CODE>, 
     * <CODE>CommunicatorServer.STOPPING</CODE>.
     * @param timeout The maximum time to wait for, in 
     *        milliseconds, if positive. 
     * Infinite time out if 0, or no waiting at all if negative.
     *
     * @return true if the value of this MBean's State attribute is the same
     *         as the <VAR>state</VAR> parameter; false otherwise.
     */
    public boolean waitState(int state , long timeout) ;

    /**
     * Gets the state of this <CODE>CommunicatorServer</CODE> as an integer.
     *
     * @return <CODE>ONLINE</CODE>, <CODE>OFFLINE</CODE>, <CODE>STARTING</CODE> or <CODE>STOPPING</CODE>.
     */
    public int getState() ;

    /**
     * Gets the state of this <CODE>CommunicatorServer</CODE> as a string.
     *
     * @return One of the strings "ONLINE", "OFFLINE", "STARTING" or "STOPPING".
     */
    public String getStateString() ;

    /**
     * Gets the host name used by this <CODE>CommunicatorServer</CODE>.
     *
     * @return The host name used by this <CODE>CommunicatorServer</CODE>.
     */
    public String getHost() ;

    /**
     * Gets the port number used by this <CODE>CommunicatorServer</CODE>.
     *
     * @return The port number used by this <CODE>CommunicatorServer</CODE>.
     */
    public int getPort() ;

    /**
     * Sets the port number used by this <CODE>CommunicatorServer</CODE>.
     *
     * @param port The port number used by this <CODE>CommunicatorServer</CODE>.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the communicator was ONLINE or STARTING.
     */
    public void setPort(int port) throws java.lang.IllegalStateException ;

    /**
     * Gets the protocol being used by this <CODE>CommunicatorServer</CODE>.
     * @return The protocol as a string.
     */
    public abstract String getProtocol() ;
}
