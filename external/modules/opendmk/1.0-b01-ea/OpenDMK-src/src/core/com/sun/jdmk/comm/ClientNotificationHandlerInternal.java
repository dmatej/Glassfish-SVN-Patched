/*
 * @(#)file      ClientNotificationHandlerInternal.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.10
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



package com.sun.jdmk.comm;


import java.lang.*;

/**
 * This interface should be implemented by a client connector in order to use jdmk event mechanism.
 */
interface ClientNotificationHandlerInternal {
	/**
	 * used to ask a client connector to transfer a request to agent side. The client connector
	 * only needs to forwards this request to its server connector, then the server connector will
	 * forward this request to its ServerNotificationDispatcher.
	 *
	 * @param opType an integer specified by ClientNotificationDispatcher.
	 * @param params a set of objects provided by a ClientNotificationDispatcher.
	 * @return a set of Objects.
	 */
	Object[] remoteRequest(int opType, Object[] params) throws Exception;

	/**
	 * used to start the mode "push". A client connector should return a ConnectorAddress
	 * object which can be used by a server connector to establish a connection, this connection
	 * will allow the communication from the server side to client side.
	 *
	 */
	public ConnectorAddress startPush();

	/**
	 * used to stop the mode "push", and change to the mode "pull".
	 *
	 * @param address the connector address used by a server to connect with the client.
	 */
	public void stopPush(ConnectorAddress address) ;
}
