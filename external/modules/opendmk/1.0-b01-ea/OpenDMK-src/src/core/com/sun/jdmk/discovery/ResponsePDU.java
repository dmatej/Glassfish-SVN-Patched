/*
 * @(#)file      ResponsePDU.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.12
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

// jmx-remote
import javax.management.remote.JMXServiceURL;

// ------------------------
// jdmk import
// ------------------------
import javax.management.*;
import com.sun.jdmk.*;
import com.sun.jdmk.comm.ConnectorAddress;
import com.sun.jdmk.comm.RmiConnectorAddress;
import com.sun.jdmk.comm.HttpConnectorAddress;
import com.sun.jdmk.comm.HttpsConnectorAddress;

class ResponsePDU extends DiscoveryMsg {
    
    private static final long serialVersionUID = -1917939025172594153L;


// ----------------------------------------------------------
// Private variables
// ----------------------------------------------------------
	private Hashtable	objectList	      = new Hashtable () ; 
	private boolean		evt		      = false ;
	private int		agtState	      = DiscoveryMonitor.OFFLINE ;
	private String		mbeanServerId	      = null ;
	private String		specificationName     = null ;
	private String		specificationVersion  = null ;
	private String		specificationVendor   = null ;
	private String		implementationName    = null ;
	private String		implementationVersion = null ;
	private String		implementationVendor  = null ;
	private byte[] 	        userData              = null ;



// ----------------------------------------------------------
// Constructor
// ----------------------------------------------------------
  public ResponsePDU (	String mbeanServerId,
			String specificationName,
			String specificationVendor,
			String specificationVersion,
			String implementationName,
			String implementationVendor ,
			String implementationVersion,
			byte[] data) {

	this.mbeanServerId         = mbeanServerId ;
	this.specificationName     = specificationName ;
	this.specificationVersion  = specificationVersion ;
	this.specificationVendor   = specificationVendor ;
	this.implementationName    = implementationName ;
	this.implementationVersion = implementationVersion ;
	this.implementationVendor  = implementationVendor ;
	this.userData              = data;
  }

// ----------------------------------------------------------
// print method
// ----------------------------------------------------------
  public String printState () {
        String state = new String () ; 
 
        // ------------------------ 
        // Time stamp
        // ------------------------ 
        state =  state + "(TimeStamp=" + getTimeStamp() + ")";

        // ------------------------ 
        // Event flag
        // ------------------------ 
        state =  state + "(Event=" + getEvent() + ")";

        // ------------------------ 
        // agent state
        // ------------------------ 
	if ( getEvent() == true ) {
		String s  = null ;
		if ( getAgentState() == DiscoveryMonitor.ONLINE ) s = "ONLINE" ;
		if ( getAgentState() == DiscoveryMonitor.OFFLINE ) s = "OFFLINE" ;
		if ( getAgentState() == DiscoveryMonitor.STOPPING ) s = "STOPPING" ;
        	state =  state + "(Agent State =" + s + ")";
	}
 
        // ------------------------ 
        // Ttl
        // ------------------------ 
        state =  state + "(Ttl=" + getTimeToLive() + ")";

        // ------------------------
        // local Host 
        // ------------------------
        state = state + "(local host = " + getHost() + ")" ;

        // ------------------------
        // mbeanServer info 
        // ------------------------
        state = state + "(mbeanServerId = "         + mbeanServerId + ")" ;
	state = state + "(specificationName = "     + specificationName + ")" ;
	state = state + "(specificationVersion = "  + specificationVersion + ")" ;
	state = state + "(specificationVendor = "   + specificationVendor + ")" ;
	state = state + "(implementationName = "    + implementationName + ")" ;
	state = state + "(implementationVersion = " + implementationVersion + ")" ;
	state = state + "(implementationVendor = "  + implementationVendor + ")" ;

        // ------------------------
        // object list
        // ------------------------
	if ( objectList != null ) {
		state = state + "(objects " ;
		for( Enumeration e= objectList.keys(); e.hasMoreElements(); ) {
			ObjectName o= (ObjectName) e.nextElement();
			state = state + "-  name=" + o + " addr=" + toString(objectList.get(o)) ;
		}
		state = state + ")" ;
	} else {
		state = state + "(No object)" ;
	}
 
        return state ; 
  }


// ----------------------------------------------------------
// MbeanServer information
// ----------------------------------------------------------
  public String getMbeanServerId () {
	return mbeanServerId ;
  }
  public String getSpecificationName() {
	return specificationName ;
  }
  public String getSpecificationVersion() {
	return specificationVersion ;
  }
  public String getSpecificationVendor() {
	return specificationVendor ;
  }
  public String getImplementationName() {
	return implementationName ;
  }
  public String getImplementationVersion() {
	return implementationVersion ;
  }
  public String getImplementationVendor() {
	return implementationVendor ;
  }
  public byte[] getUserData() {
	return userData ;
  }

// ----------------------------------------------------------
// event flag
// ----------------------------------------------------------
  public void setEvent (boolean evt ) {
	this.evt = evt;
  }
  public boolean getEvent () {
	return evt ;
  }

// ----------------------------------------------------------
// agent state flag
// ----------------------------------------------------------
  public void setAgentState (int agtState ) {
	this.agtState = agtState;
  }
  public int getAgentState () {
	return agtState ;
  }

// ----------------------------------------------------------
// adaptor list method
// ----------------------------------------------------------
  public Hashtable getObjectList () {
	return objectList ;
  }

  public void addObjectList (ObjectName name, ConnectorAddress addr) {
		if (addr !=  null ) {
			this.objectList.put(name,addr) ;
		} else {
			this.objectList.put(name,"Not Relevant") ;
		}
  }

// ----------------------------------------------------------
// Private method
// ----------------------------------------------------------
  private String toString(Object addr) {
	if ( addr instanceof RmiConnectorAddress ) {
		RmiConnectorAddress address = (RmiConnectorAddress) addr ;
		return "type:" + address.getConnectorType() + ";host:" + address.getHost() + ";port:" + address.getPort() + ";name:" + address.getName() ;
	}
	
	if ( addr instanceof HttpConnectorAddress ) {
		HttpConnectorAddress address = (HttpConnectorAddress) addr ;
		return "type:" + address.getConnectorType() + ";host:" + address.getHost() + ";port:" + address.getPort() ;
	}

	if ( addr instanceof HttpsConnectorAddress ) {
		HttpsConnectorAddress address = (HttpsConnectorAddress) addr ;
		return "type:" + address.getConnectorType() + ";host:" + address.getHost() + ";port:" + address.getPort() ;
	}

	return addr.toString()  ;
  }

    // used for jmx-remote connectors
    JMXServiceURL[]  serverAddresses;
}
