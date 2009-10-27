/*
 * @(#)file      DiscoveryResponse.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.22
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

// jmx
import javax.management.remote.JMXServiceURL;

/**
 * The <CODE>DiscoveryResponse</CODE> is used by the {@link com.sun.jdmk.discovery.DiscoveryClient}
 * class to represent the result of a discovery operation.
 *
 * @see 	com.sun.jdmk.discovery.DiscoveryClient
 */

public class DiscoveryResponse implements Serializable {


 private static final long serialVersionUID = -1615089845432442933L;

// ----------------------------------------------------------
// Constructor
// ----------------------------------------------------------
  /**
   * Initializes this <CODE>DiscoveryResponse</CODE>.
   * <P>
   * The string <CODE>host</CODE>, the string <CODE>mbeanServerId</CODE>
   *  and the vector <CODE>objectList</CODE> are initialized to null.
   */
  DiscoveryResponse() {
  }

// ----------------------------------------------------------
// public gettter
// ----------------------------------------------------------

    /**
     * Returns the host name.
     *
     * @return the host name.
     */
	public String getHost() {
		return host ;
	}

    /**
     * Returns the identification of the MbeanServer.
     *
     * @return the MbeanServer identification.
     */
	public String getMBeanServerId() {
		return mbeanServerId ;
	}

    /**
     * Returns the full name of the JMX specification implemented by the agent.
     *
     * @return the full name of the JMX specification implemented by the agent.
     */
    public String getSpecificationName() {
		return specificationName ;
    }
 
    /**  
     * Returns the version of the JMX specification implemented by the agent.
     *
     * @return the version of the JMX specification implemented by the agent.
     */
    public String getSpecificationVersion() {
	return specificationVersion ;
    }
 
    /**  
     * Returns the vendor of the JMX specification implemented by the agent.
     *
     * @return the vendor of the JMX specification implemented by the agent.
     */ 
    public String getSpecificationVendor() {
	return specificationVendor ;
    }

  /**
     * Returns the JMX implementation name (the name of the product corresponding to the agent).
     *
     * @return the JMX implementation name.
     */ 
    public String getImplementationName() {
	return implementationName ;
    }
 
    /**
     * Returns the JMX implementation version (the version of the product corresponding to the agent).
     *
     * @return the JMX implementation version.
     */
    public String getImplementationVersion() {
	return implementationVersion ;
    }
 
    /**
     * Returns the JMX implementation vendor (the vendor of the product corresponding to the agent).
     *
     * @return the JMX implementation vendor.
     */
    public String getImplementationVendor() {
	return implementationVendor ;
    }



    /**
     * Returns the list of communicators.
     * It is an Hashtable. Keys are {@link javax.management.ObjectName} and values are
     * {@link com.sun.jdmk.comm.ConnectorAddress} if its relevant.
     * The method <CODE>findMBeanServers</CODE> sets this Hashtable to null.
     *
     * @return the list of communicators.
     */
	public Hashtable  getObjectList() {
	return objectList ;
    }


   /**
    * Returns a byte[] containing the additional information that the user added in the 
    * <CODE>DiscoveryResponse</CODE>.
    * If no additional information has been added, this method returns <CODE>null</CODE>.
    * @return user-specific information.
    */
    public byte[]  getUserData() {
	return userData ;
    }

    /**
     * Returns a list of connector server addresses, a server is one which is registered
     * into an MBeanServer as <code>JMXConnectorServerMBean</code>, either a JMX Remote 
     * <code>JMXConnectorServer</code> or a Java DMK legacy one which is wrapped as a
     * <code>JMXConnectorServer</code>.
     * A Java DMK legacy connector server registered as a legacy MBean (ex: RmiConnectorServerMBean)
     * will not be included in this list.
     * 
     * @return a list of connector server addresses.
     * @since Java DMK 5.1
     */
    public JMXServiceURL[] getServerAddresses() {
	return serverAddresses;
    }

// ----------------------------------------------------------
// package variables
// ----------------------------------------------------------

    /**
     * The host name.
     *
     * @serial
     */
	String	host  = null ;

    /**
     * The identification of the MbeanServer.
     *
     * @serial
     */
	String	mbeanServerId  = null ;

    /**
     * The full name of the JMX specification implemented by the agent.
     *
     * @serial
     */
    String specificationName  = null ;
 
    /**  
     * The version of the JMX specification implemented by the agent.
     *
     * @serial
     */
    String specificationVersion  = null ;
 
    /**  
     * The vendor of the JMX specification implemented by the agent.
     *
     * @serial
     */ 
    String specificationVendor  = null ;

  /**
     * The JMX implementation name (the name of the product corresponding to the agent).
     *
     * @serial
     */ 
    String implementationName  = null ;
 
    /**
     * The JMX implementation version (the version of the product corresponding to the agent).
     *
     * @serial
     */
    String implementationVersion  = null ;
 
    /**
     * The JMX implementation vendor (the version of the product corresponding to the agent).
     *
     * @serial
     */
    String implementationVendor  = null ;



    /**
     * The list of communicators.
     * It is an Hashtable. Keys are {@link javax.management.ObjectName} and values are ConnectorAddress if its relevant.
     * The method <CODE>findMBeanServers</CODE> sets this Hashtable to null.
     *
     * @serial
     */
	Hashtable  objectList = null  ;

    /* The additional data that theuser added in the Discovery Response message */
    byte[] userData;

    // jmx-remote
    JMXServiceURL[] serverAddresses;
}
