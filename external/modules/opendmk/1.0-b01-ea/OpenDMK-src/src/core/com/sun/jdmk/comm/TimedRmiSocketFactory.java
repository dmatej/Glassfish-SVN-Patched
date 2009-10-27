/*
 * @(#)file      TimedRmiSocketFactory.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.3
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
 */

package com.sun.jdmk.comm;

// java import
//
import java.io.IOException;
import java.net.Socket;
import java.net.ServerSocket;
import java.net.InetAddress;

/**
 * This class defines an RMI Socket Factory that allows to
 * configure a socket object with a specified timeout.
 * NPCTE fix for escalation 534403, bug 4624028.
 */
public class TimedRmiSocketFactory extends java.rmi.server.RMISocketFactory {
	private int commTimeout;

        public TimedRmiSocketFactory(int commTimeout) throws IOException {
                super();
                this.commTimeout = commTimeout;
        }

	public Socket createSocket(String host, int port) throws IOException {
		Socket socket = new Socket(InetAddress.getByName(host), port);
		//Socket socket = TimedSocketMaker.makeSocket(InetAddress.getByName(host), port, creationTimeout);
		socket.setSoTimeout(commTimeout);
		return socket;
	}

	public ServerSocket createServerSocket(int port) throws IOException {
		ServerSocket serverSocket = new ServerSocket(port);
		return serverSocket;
	}	
	


}
