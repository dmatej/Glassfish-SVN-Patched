/*
 * @(#)file      DiscoveryMsg.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.15
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


import com.sun.jdmk.internal.ClassLogger;
import java.io.Serializable;
import java.net.InetAddress;

class DiscoveryMsg implements Serializable {
    
    private static final long serialVersionUID = -6894329411689693966L;

    // ----------------------------------------------------------
    // Constructor
    // ----------------------------------------------------------
    public DiscoveryMsg ()
    {
    }
    
    public String printState ()
    {
        String state = new String () ;
	
        // ------------------------
        // Time stamp
        // ------------------------
        state =  state + "(TimeStamp=" + getTimeStamp() + ")";
	
        // ------------------------
        // Time-To_-Live
        // ------------------------
        state =  state + "(Time-To-Live=" + getTimeToLive() + ")";
	
        // ------------------------
        // local Host
        // ------------------------
        state = state + "(local host = " + getHost() + ")" ;
	
        return state ;
	
    }
    
    // ----------------------------------------------------------
    // Specified host is asked
    // ----------------------------------------------------------
    public String  getHost ()
    {
        return host ;
    }
    public void setHost (String host)
    {
	this.host = host ;
    }
    
    
    // ----------------------------------------------------------
    // Time stamp
    // ----------------------------------------------------------
    public void setTimeStamp (String timeStamp)
    {
	this.timeStamp = timeStamp ;
    }
    
    public String getTimeStamp ()
    {
	return timeStamp ;
    }
    
    // ----------------------------------------------------------
    // TimeToLive
    // ----------------------------------------------------------
    public void setTimeToLive(int ttl)
    {
        this.ttl = ttl ;
    }
    
    public int getTimeToLive() {
	return ttl ;
    }
    
    
    // ----------------------------------------------------------
    // emitted group
    // ----------------------------------------------------------
    public void setEmittedGroup (InetAddress emittedGroup)
    {
	this.emittedGroup = emittedGroup ;
    }
    
    public InetAddress getEmittedGroup ()
    {
	return emittedGroup ;
    }
    
    
    // ----------------------------------------------------------
    // Trace stuff
    // ----------------------------------------------------------
    protected String localClassName = "com.sun.jdmk.discovery.DiscoveryMsg" ;
    
    protected String dbgTag = localClassName ;

    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"com.sun.jdmk.discovery.DiscoveryMsg");
    
    
    // ----------------------------------------------------------
    // Local variables
    // ----------------------------------------------------------
    
    private String timeStamp		;
    private String                  host    = null ;
    
    private int                     ttl     = 1 ;
    
    private InetAddress		emittedGroup ;
    
}
