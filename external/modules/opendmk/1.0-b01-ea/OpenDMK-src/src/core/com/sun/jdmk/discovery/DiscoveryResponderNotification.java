/*
 * @(#)file      DiscoveryResponderNotification.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.21
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
// Import
// ----------------------------------------------------------
import javax.management.*;
import com.sun.jdmk.* ; 



// ----------------------------------------------------------
// Constructor
// ----------------------------------------------------------
/**
 * The <CODE>DiscoveryResponderNotification</CODE> object is sent by the 
 * {@link com.sun.jdmk.discovery.DiscoveryMonitor} MBean to its listener.
 * The <CODE>DiscoveryResponderNotification</CODE> object is sent by the 
 * {@link com.sun.jdmk.discovery.DiscoveryMonitor} when it receives a 
 * registration or unregistration message from a {@link 
 * com.sun.jdmk.discovery.DiscoveryResponder}.
 *
 *
 */

public class DiscoveryResponderNotification extends Notification {
    private static final long serialVersionUID = 5897053465722225217L;
  


    /** 
     * Notification type denoting that the DiscoveryResponder MBean has 
     * been registered or unregistered from the MBeanServer.
     * <BR>The value of this notification type is 
     * <CODE>com.sun.jdmk.discovery</CODE>.
     */
    public static final String REGISTRATION = "com.sun.jdmk.discovery" ;

    /**
     * Constructs a <CODE>DiscoveryResponderNotification</CODE>. 
     *
     * @param source The object that was registered or the 
     *        object on which the event occurred.
     * @param state Indicates whether a 
     *    {@link com.sun.jdmk.discovery.DiscoveryResponder} object has been
     *    added to an agent (DiscoveryMonitor.ONLINE) or removed 
     *    (DiscoveryMonitor.OFFLINE).
     * @param agtDes This parameter gives information about the agent.
     * @param sequenceNumber The notification sequence number within the 
     *     DiscoveryResponder MBean object.
     */
    public DiscoveryResponderNotification(DiscoveryMonitor source, int state,
					  DiscoveryResponse agtDes, 
					  long sequenceNumber) {

	// --------------------	
	// Call super
	// --------------------	
	super(REGISTRATION, source, sequenceNumber) ;

	// --------------------	
	// state initialization
	// --------------------	
	if ( (state != DiscoveryMonitor.OFFLINE ) && 
	     ( state != DiscoveryMonitor.ONLINE ))
	{
		state = DiscoveryMonitor.OFFLINE ;
	}
	internalState = new Integer (state) ;

	// --------------------	
	// Set DiscoveryResponse object
	// --------------------	
	internalRsp = agtDes ;
  }

  /**
   * Returns the state of the agent.
   * <P>
   * If the state is <CODE>DiscoveryMonitor.ONLINE</CODE>, a {@link com.sun.jdmk.discovery.DiscoveryResponder} object has been
   * added to an agent. 
   * If the state is <CODE>DiscoveryMonitor.OFFLINE</CODE>, a {@link com.sun.jdmk.discovery.DiscoveryResponder} object has been
   * removed from an agent. The <CODE>getEventInfo</CODE> method provides additional information on the agent.
   *
   * @return <CODE>DiscoveryMonitor.ONLINE</CODE> or <CODE>DiscoveryMonitor.OFFLINE</CODE>
   */
  public Integer getState() {
    return internalState;
  }

  /**
   * Returns the hostname and the MBeanServer information of the agent. It
   * also returns a list of communicators ( see {@link com.sun.jdmk.discovery.DiscoveryResponse} description).
   * <P>
   * If state is <CODE>DiscoveryMonitor.OFFLINE</CODE>, only the <CODE>host</CODE>
   * and MBeanServer information are significant in the {@link com.sun.jdmk.discovery.DiscoveryResponse} object.
   *
   * @return A {@link com.sun.jdmk.discovery.DiscoveryResponse} object. This object contains the hostname 
   *         as a string <CODE>host</CODE> and  MBeanServer information.
   *         It also contains a list of communicators ( see {@link com.sun.jdmk.discovery.DiscoveryResponse} description)
   *         if the state is <CODE>DiscoveryMonitor.ONLINE</CODE>.
   */
  public DiscoveryResponse getEventInfo() {
    return internalRsp;
  }


// ----------------------------------------------------------
// public variables
// ----------------------------------------------------------

  /**
   * The state of the agent.
   *
   * @serial
   */
  private Integer	internalState;

  /**
   * The agent response.
   *
   * @serial
   */
  private DiscoveryResponse 	internalRsp;
  
}
