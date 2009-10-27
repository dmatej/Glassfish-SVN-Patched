/*
 * @(#)file      DiscoveryCommon.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.17
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
import com.sun.jdmk.* ; 
import com.sun.jdmk.internal.ClassLogger;



abstract class DiscoveryCommon extends java.net.MulticastSocket
    implements Runnable {

// ----------------------------------------------------------
// Constructor
// ----------------------------------------------------------
  public DiscoveryCommon (String multicastGroup , int multicastPort )
    throws IOException
  {
	// ------------------------
	// Set multicastSocket port
	// ------------------------
	super(multicastPort) ;
	if (logger.finerOn())
	{
		logger.finer("constructor " , "group = " + multicastGroup  + ", " + "port  = " + multicastPort ) ;
	}

	this.multicastGroup = InetAddress.getByName(multicastGroup) ;
	
	// ------------------------
	// Set multicast Port
	// ------------------------
	this.multicastPort  = multicastPort  ;

	// ------------------------
	// Set local host
	// ------------------------
	localHostName = System.getProperty("jdmk.hostname");
	if (localHostName != null) {
	    localHost =  InetAddress.getByName(localHostName);
	    selectedHost = localHost;
	    selectedHostName = localHostName; 
	} else {
	    localHost     = InetAddress.getLocalHost();
	    localHostName = InetAddress.getLocalHost().getHostName();
	    selectedHost     = localHost;
	    selectedHostName = localHostName;
	}
  }
    
// ----------------------------------------------------------
// start/stop listening
// ----------------------------------------------------------
  public void connectToGroup()
  throws IOException
  {
	// ------------------------
	// Join the group
	// ------------------------
	if (logger.finerOn())
	{
		logger.finer("connectToGroup" , "join Group " + multicastGroup) ;
	}
	joinGroup(multicastGroup);
  }

  void disconnectFromGroup()
  throws IOException
  {
	// ------------------------
	// Leave the group
	// ------------------------
	if (logger.finerOn())
	{
		logger.finer("disconnectFromGroup" , "leave Group " + multicastGroup) ;
	}
	leaveGroup(multicastGroup);
  }

// ----------------------------------------------------------
// run
// ----------------------------------------------------------
  public void run()
  {

  }


// ----------------------------------------------------------
// get Host 
// ----------------------------------------------------------
  public String getLocalHostName()
  {
        return localHostName ;
  }

  public InetAddress getLocalHostAddr()
  {
        return localHost ;
  }

  public String getHostName()
  {
        return selectedHostName ;
  }

  public InetAddress getHostAddr()
  {
        return selectedHost ;
  }


// ----------------------------------------------------------
// private sendMsg / receivemsg
// ----------------------------------------------------------
  protected synchronized void sendMsg (DiscoveryMsg msg , InetAddress receiverInetAddr,  int receiverPort)
  throws IOException
  {
	// ------------------------
	// Msg serialization
	// ------------------------
	byte msgToSend[] = null ;
	try
	{
		msgToSend = objectSerialization(msg) ;
	}
	catch (IOException e )
	{
		if (logger.finestOn())
		{
			logger.finest("sendMsg " , e) ;
		}
		throw e ;
	}

	// ------------------------
	// Send the msg
	// ------------------------
	if (logger.finerOn())
	{
		logger.finer("sendMsg" , "destination address = "  + receiverInetAddr + " Port = " + receiverPort );
		logger.finer("sendMsg" , "Send msg '" + msg.printState() + "'") ;
	}
	DatagramPacket packet = new DatagramPacket(msgToSend, msgToSend.length, receiverInetAddr, receiverPort);
	setTimeToLive(msg.getTimeToLive()) ;
	send(packet) ;
  }


  protected synchronized DiscoveryMsg receiveMsg (DatagramSocket socket)
  throws IOException,ClassNotFoundException,InvalidClassException, InterruptedIOException
  {
      byte[]                  buf             = new byte[65536]; // maximum size of a UDP packet = 64Kbytes
	DatagramPacket          packet          = new DatagramPacket(buf, buf.length);
	byte[]                  recvData        ;
	DiscoveryMsg		msg 		;

		// ------------------------
		// Reveice msg
		// ------------------------
		if (logger.finerOn())
		{
			logger.finer("receiveMsg " , "Start waiting") ;
		}
		socket.receive(packet);
		if (logger.finerOn())
		{
			logger.finer("receiveMsg " , "We Receive something") ;
		}

		recvData = packet.getData() ;
		// ------------------------
		// deserialization object
		// ------------------------
		msg = (DiscoveryMsg) objectDeserialization (recvData) ;

		// ------------------------
		// Check if received msg correspond to the current group
		// ------------------------
		InetAddress emittedGroup = msg.getEmittedGroup() ;
		if ( emittedGroup.equals(multicastGroup) != true )
		{
			if (logger.finerOn())
			{
				logger.finer("receiveMsg " , "Receive a msg for another group") ;
			}
			throw new IOException()  ;
		}
		if (logger.finerOn())
		{
			logger.finer("receiveMsg " , "Receive a msg") ;
		}
	return msg ;
  }



// ----------------------------------------------------------
// private Serialization/deserialization
// ----------------------------------------------------------
  /*
  * Extract object representation from bytes
  */
  private java.lang.Object objectDeserialization(byte[] entityBody)
  throws IOException , ClassNotFoundException
  {
	// ------------------------
	// Null object
	// ------------------------
	if (entityBody == null)
	{
		return (null);
	}          

	// ------------------------
	// byte array set to zero
	// ------------------------
	if (entityBody.length == 0)
	{
		return (null);
	}          

	// ------------------------
	// Object deserialization
	// ------------------------
	try
	{
		ByteArrayInputStream bIn;
		ObjectInputStream    objIn;
		String               typeStr;

		bIn   = new ByteArrayInputStream(entityBody);
		objIn = new ObjectInputStream(bIn);

		java.lang.Object result = (java.lang.Object)objIn.readObject();
		return result;
	}
	catch (ClassNotFoundException e )
	{
		if (logger.finestOn())
		{
			logger.finest("objectDeserialization" , e) ;
		}
		throw e ;
	}
	catch (OptionalDataException e )
	{
		if (logger.finestOn())
		{
			logger.finest("objectDeserialization" , e) ;
		}
		throw new IOException(e.getMessage() ) ;
	}
   }

  /*
  * Return the byte representation of object serialization
  */
  private byte [] objectSerialization (java.lang.Object obj)
  throws IOException
  {
	// ------------------------
	// Object serialization
	// ------------------------
       	ByteArrayOutputStream bOut;
       	ObjectOutputStream    objOut;

       	bOut   = new ByteArrayOutputStream();
       	objOut = new ObjectOutputStream(bOut);

       	objOut.writeObject(obj);

	// ------------------------
	// Return part
	// ------------------------
	return bOut.toByteArray() ;
   }


    // ----------------------------------------------------------
    // Logging stuff
    // ----------------------------------------------------------
    protected String localClassName = 
	"com.sun.jdmk.discovery.DiscoveryCommon" ;
    private final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_DISCOVERY,
			"DiscoveryCommon");

    // ----------------------------------------------------------
    // Private variables
    // ----------------------------------------------------------
    protected int                   multicastPort           ;
    protected InetAddress           multicastGroup          ;
    
    protected InetAddress           localHost               ;
    protected String           	    localHostName           ;
    protected InetAddress           selectedHost            ;
    protected String           	    selectedHostName        ;
    
}
