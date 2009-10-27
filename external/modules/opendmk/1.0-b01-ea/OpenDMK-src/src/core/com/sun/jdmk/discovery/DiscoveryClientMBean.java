/*
 * @(#)file      DiscoveryClientMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.17
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
import java.io.*;
import java.net.*;


// ------------------------
// jdmk import
// ------------------------
import javax.management.*;
import com.sun.jdmk.* ;
import com.sun.jdmk.comm.CommunicationException ;


 /**
 * Provides methods to discover Java DMK agents.
 * A Java DMK agent can only discovered if it has a
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} registered in its MBean server.
 * A discovery operation is executed in two steps:
 * <UL>
 *   <LI>a <CODE>DiscoveryClient</CODE> broadcasts a discovery request on a
 *       multicast group</LI>
 *   <LI>Registered {@link com.sun.jdmk.discovery.DiscoveryResponder} MBeans listening on the multicast group
 *       catch the request. Each {@link com.sun.jdmk.discovery.DiscoveryResponder}
 *       sends a discovery response to the <CODE>DiscoveryClient</CODE></LI>
 * </UL>
 * <p>
 * A <CODE>DiscoveryClient</CODE> can only reach the
 * {@link com.sun.jdmk.discovery.DiscoveryResponder} objects that listen on the same multicast group and
 * port. The default group is 224.224.224.224 and the default port is 9000. Other values
 * can be used by configuring the <CODE>multicastGroup</CODE> and
 * <CODE>multicastPort</CODE> properties on the <CODE>DiscoveryClient</CODE>
 * and {@link com.sun.jdmk.discovery.DiscoveryResponder} objects.
 * <p>
 * The scope of the discovery depends on the time-to-live used by
 * the <CODE>MulticastSocket</CODE>. By default, the time-to-live is 1. It
 * can be changed by setting the property <CODE>timeToLive</CODE> on the
 * <CODE>DiscoveryClient</CODE>.
 * <p>
 * After it has sent its discovery request, a <CODE>DiscoveryClient</CODE>
 * waits a finite time for responses. The default is 1 second.
 * This can be customized by setting the <CODE>timeOut</CODE> property on the
 * <CODE>DiscoveryClient</CODE>.
 * <p>
 * An application triggers a discovery operation by invoking either the
 * <CODE>findMBeanServers</CODE> method or the <CODE>findCommunicators</CODE>
 * method on a <CODE>DiscoveryClient</CODE> object.
 * These methods represent the discovery result by a <CODE>Vector</CODE> of
 * {@link com.sun.jdmk.discovery.DiscoveryResponse}. A {@link com.sun.jdmk.discovery.DiscoveryResponse} is included
 * for each discovered Java DMK agent. It provides the host name and the MBean server information of the
 * agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) and optionally
 * the list of communicator (Adaptor and connector) available in the agent.
 * <p>
 * A {@link com.sun.jdmk.discovery.DiscoveryResponder} can send back responses using two modes:
 * <UL>
 *   <LI> Unicast mode. A datagram socket is sent from the {@link com.sun.jdmk.discovery.DiscoveryResponder}
 *        to the <CODE>DiscoveryClient</CODE>. The response is NOT multicasted to the group.
 *        The default datagram socket port is 9001. The datagram socket Inet
 *        address is defined by the local host name. It cannot be customized.
 *        To enable unicast mode, set the <CODE>pointToPointResponse</CODE> property to <CODE>true</CODE>.
 *        (Unicast mode is enabled by default.)
 *   </LI>
 *   <LI> Multicast mode. A multicast socket is used between the {@link com.sun.jdmk.discovery.DiscoveryResponder}
 *        and the <CODE>DiscoveryClient</CODE>. The response is multicasted to the group. This
 *        behavior allows {@link com.sun.jdmk.discovery.DiscoveryMonitor} objects to be aware of changes.
 *        To enable multicast mode, set the <CODE>pointToPointResponse</CODE> property to <CODE>false</CODE>.
 *   </LI>
 * </UL>
 * <p>
 * It is possible to instantiate multiple <CODE>DiscoveryClient</CODE> objects with
 * different groups and ports for multicast responses and datagram sockets for unicast responses.
 *
 */

public interface DiscoveryClientMBean {

// ----------------------------------------------------------
// States of a DiscoveryResponder
// ----------------------------------------------------------

  /** Marks the "state" property as running. */
  public static final int ONLINE = 0 ;
  /** Marks the "state" property as stopped. */
  public static final int OFFLINE = 1 ;
  /** Marks the "state" property as in-transition from ONLINE to OFFLINE. */
  public static final int STOPPING = 2 ;



// ----------------------------------------------------------
// service Start/stop 
// ----------------------------------------------------------
  /**
   * Create a multicast socket and join the multicast group.
   * This method creates a multicast socket that is used to broadcast
   * The <CODE>DiscoveryClient</CODE> will then join the multicast group.
   *
   * @exception IOException The creation of the Multicast socket failed.
   */

  public void start()
  throws IOException ;

  /**
   * Leaves the multicast group.
   * The <CODE>DiscoveryClient</CODE> leaves its multicast group,
   * and the multicast socket is released.
   *
   */

   public void stop() ;

  /**
   * Tests if the <CODE>DiscoveryClient</CODE> is active.
   * <CODE>True</CODE> is returned if the <CODE>DiscoveryClient</CODE> is started (<CODE>DiscoveryClient</CODE>
   * has join the multicast group).
   */
  public boolean isActive() ; 

// ----------------------------------------------------------
// Discovery method 
// ----------------------------------------------------------
  /**
   * Discovers all Java DMK agents.
   * <P>
   * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent.
   * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} contains the host name and the MBean server information of the
   * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}).
   * The communicators list is not relevant: it is set to null.
   *
   * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
   *
   * @exception CommunicationException An error occurred during the discovery.
   *     
   */
   public Vector findMBeanServers()
   throws CommunicationException ;


  /**
   * Discovers whether Java DMK agents with a {@link com.sun.jdmk.discovery.DiscoveryResponder}
   * registered in any MBean server is on a host.
   * <P> 
   * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent on the specified host.
   * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} only contains the host name and the MBean server information of the  
   * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) of the specified host.
   * The communicators list is not relevant: it is set to null. 
   * 
   * @param SelectedHost The host on which the discovery is to be performed.
   * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
   *
   * @exception CommunicationException An error occurred during the discovery.
   *
   */
   public Vector findMBeanServers(String SelectedHost)
   throws CommunicationException ;

  /**
   * Discovers all Java DMK agents and associated communicators (adaptors or connectors).
   * <P> 
   * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent.
   * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} contains the host name, the MBean server information of the
   * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) and
   * the communicators list.
   *
   * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
   *
   * @exception CommunicationException An error occurred during the discovery.
   *     
   */
   public Vector findCommunicators()
   throws CommunicationException ;


  /**
   * Discovers all Java DMK agents and associated communicators (adaptors or connectors) present on an host.
   * <P> 
   * Returns a vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}, one element for each discovered Java DMK agent.
   * Each {@link com.sun.jdmk.discovery.DiscoveryResponse} contains the host name, the MBean server information of the
   * discovered agent ( see {@link javax.management.MBeanServerDelegate} and {@link com.sun.jdmk.discovery.DiscoveryResponse}) and
   * the communicators list.
   *
   * @param SelectedHost The host on which the discovery is to be performed.
   *
   * @return A vector of {@link com.sun.jdmk.discovery.DiscoveryResponse}.
   *
   * @exception CommunicationException An error occurred during the discovery.
   *     
   */
   public Vector findCommunicators(String SelectedHost)
   throws CommunicationException ;



// ----------------------------------------------------------
// Getters and Setters
// ----------------------------------------------------------

// --------------------------
// multicast group
// --------------------------
  /**
   * Returns the multicast group.
   * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255.
   */
  public String getMulticastGroup() ;

  /**
   * Sets the multicast group name.
   * A multicast group is specified by a class D IP address, those in the range 224.0.0.1 to 239.255.255.255
   * <P>
   * Only available if the state is OFFLINE
   *
   * @param multicastGroup The multicast group name.
   *
   * @exception java.lang.IllegalStateException This method has been invoked while
   *            the <CODE>DiscoveryClient</CODE> was ONLINE or STARTING.
   */
  public void setMulticastGroup(String multicastGroup)
  throws java.lang.IllegalStateException ;


// --------------------------
// multicast port
// --------------------------

  /**
   * Returns the multicast port.
   * It can be any standard UDP port number.
   */
  public int getMulticastPort()  ;

  /**
   * Sets the multicast port.
   * It can be any standard UDP port number.
   * <P>
   * Only available if the state is OFFLINE
   *
   * @param multicastPort The multicast port.
   *
   * @exception java.lang.IllegalStateException This method has been invoked while
   *            the <CODE>DiscoveryClient</CODE> was ONLINE or STARTING.
   */
  public void setMulticastPort(int multicastPort)
  throws java.lang.IllegalStateException ;


// ----------------------------------------------------------
// time out
// ----------------------------------------------------------
  /**
   * Sets the time during which the <CODE>DiscoveryClient</CODE> waits
   * for discovery responses.
   * <P>
   * This time is expressed in milliseconds. The default value is 1000.
   * If the specified argument is negative or zero, the <CODE>timeOut</CODE> is
   * reset to 1000.
   * <p>
   * The methods <CODE>findMBeanServers</CODE> and <CODE>findCommunicators</CODE> block until this
   * time elapsed.
   *
   * @param timeOut The <CODE>timeOut</CODE> in milliseconds.
   * 
   * @exception IOException If the socket rejected the specified value (See <CODE> java.net.MulticastSocket</CODE>).
   *            This exception can be thrown only if the state in ONLINE: the actual <CODE>java.net.MulticastSocket</CODE> setting
   *            is done when the <CODE>DiscoveryClient</CODE> is <CODE>ONLINE</CODE>.
   *
   */
  public void setTimeOut(int timeOut) throws IOException;
  
  /**
   * Returns the time to wait for discovery responses in milliseconds.
   * 
   * @return The <CODE>timeOut</CODE> in milliseconds.
   *
   */
  public int getTimeOut() ;

// ----------------------------------------------------------
// ttl
// ----------------------------------------------------------
  /**
   * Sets the default time-to-live for this <CODE>DiscoveryClient</CODE>.
   * <P>
   * The time-to-live is the number of 'hops' that the multicast packet is
   * forwarded on the network.
   *
   * @param ttl A number between 1 and 255.
   *
   * @exception java.lang.IllegalArgumentException The input ttl value is not in the 1 to 255 range.
   * @exception IOException If the socket rejected the specified value. This means the the state in ONLINE.
   */
  public void setTimeToLive(int ttl)
  throws IOException, java.lang.IllegalArgumentException ;

  /**
   * Get the time-to-live. The default value is returned if the <CODE>TimeToLive</CODE> has not
   * been set.
   *
   *
   */
  public int getTimeToLive()  ;
 
// ----------------------------------------------------------
// set/unset/get response port
// ----------------------------------------------------------
  /**
   * Get the unicast datagram socket mode for the Java DMK agent response.
   * 
   * @return True indicates that unicast datagram socket is being used. false indicates 
   *         that the multicast response mode is being used.
   */
  public boolean getPointToPointResponse() ;

  /**
   * Set unicast datagram socket mode for the Java DMK agent response.
   * <P>
   * The client sends a request for a unicast response in each discovery 
   * request. 
   * The multicast group Inet address is used for the unicast response.
   * 
   * @param pointToPointResponse The datagram socket mode.
   * false value unsets the use of unicast socket for the response,
   * multicast is used instead.
   */
  public void setPointToPointResponse(boolean pointToPointResponse) ;

// ----------------------------------------------------------
// State stuff
// ----------------------------------------------------------
 /**
   * Returns the state of this <CODE>DiscoveryClient</CODE>.
   *
   * @return <CODE>ONLINE</CODE>, <CODE>OFFLINE</CODE> or <CODE>STOPPING</CODE>.
   */
  public Integer getState() ;

  /**
   * Returns the state of this <CODE>DiscoveryClient</CODE> in string form.
   * 
   * @return One of the strings "ONLINE", "OFFLINE" or "STOPPING".
   */
  public String getStateString()  ;



}
