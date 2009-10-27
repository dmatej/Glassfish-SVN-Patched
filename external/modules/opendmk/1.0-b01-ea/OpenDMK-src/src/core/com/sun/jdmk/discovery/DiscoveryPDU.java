/*
 * @(#)file      DiscoveryPDU.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.11
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
// jaw import
// ------------------------
import javax.management.*;
import com.sun.jdmk.*;
import com.sun.jdmk.internal.ClassLogger;



class DiscoveryPDU extends DiscoveryMsg {
    
    private static final long serialVersionUID = 8485362476672832834L;

    // ----------------------------------------------------------
    // Constructor
    // ----------------------------------------------------------
    public DiscoveryPDU ()
    {
	localClassName = "com.sun.jdmk.discovery.DiscoveryPDU" ;
    }
    
    // ----------------------------------------------------------
    // print method
    // ----------------------------------------------------------
    public String printState ()
    {
	String state = new String () ;
	
	// ------------------------
	// Time stamp
	// ------------------------
	state =  state + "(TimeStamp=" + getTimeStamp() + ")";
	
	// ------------------------
	// Ttl
	// ------------------------
	state =  state + "(Ttl=" + getTimeToLive() + ")";
	
	// ------------------------
	// Return addr
	// ------------------------
	if (getReturnAddr() ==  true )
	    {
		state =  state + "(Point_to_point_response=" + getReturnHost() + ":" + getReturnPort() + ")";
	    }
	else
	    {
		state =  state + "(Multicast response)" ;
	    }
	
	if ( objectRequired == true) 
	    {
		// ------------------------
		// Object query
		// ------------------------
		state = state + "(object required)" ;
	    }
	else
	    {
		state = state + "(object not required)" ;
	    }
	
	// ------------------------
	// Host selection 
	// ------------------------
	if (getHost() != null )
	    {
		state = state + "(selected host = " + getHost() + ")" ;
	    }
	else
	    {
		state = state + "(all hosts)" ;
	    }
	
	return state ;
    }
    
    // ----------------------------------------------------------
    // Object required
    // ----------------------------------------------------------
    public boolean getObjectRequired ()
    {
	return objectRequired ;
    }
    
    public void setObjectRequired (boolean objectRequired)
    {
	this.objectRequired = objectRequired ;
    }
    
    // ----------------------------------------------------------
    // returnAddr is specified
    // ----------------------------------------------------------
    public void unsetReturnaddr ()
    {
	returnAddr = false ;
    }
    
    public void setReturnaddr (InetAddress host, int port )
    {
	returnAddr = true ;
	returnHost = host ;
	returnPort = port ;
    }
    
    public boolean  getReturnAddr ()
    {
	return returnAddr ;
    }
    
    public InetAddress  getReturnHost ()
    {
	if ( returnAddr ) { return returnHost ; } else { return null ; }
    }
    
    
    public int  getReturnPort ()
    {
	if ( returnAddr ) { return returnPort ; } else { return -1 ; }
    }
    
    // ----------------------------------------------------------
    // Get/set time out
    // ----------------------------------------------------------
    public void setTimeOut(int timeOut)
    {
	if (logger.finerOn())
	    {
		logger.finer("setTimeOut " , "Set to " + timeOut ) ;
	    }
        this.timeOut = timeOut ;
    }
    public int getTimeOut()
    {
	if (logger.finerOn())
	    {
		logger.finer("getTimeOut " , "Value = " + timeOut ) ;
	    }
        return timeOut ;
    }
    
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"DiscoveryPDU");
    
    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    private boolean			objectRequired	= false	;
    private String			objName		= null ;
    
    private boolean			returnAddr	= false	;
    private InetAddress		returnHost	;
    private int			returnPort	;
    
    private int			timeOut	;
}
