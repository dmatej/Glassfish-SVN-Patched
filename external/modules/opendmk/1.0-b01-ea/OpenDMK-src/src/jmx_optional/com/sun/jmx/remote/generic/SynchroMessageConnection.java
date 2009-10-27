/*
 * @(#)file      SynchroMessageConnection.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.6
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
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


package com.sun.jmx.remote.generic;

import java.io.IOException;
import java.util.Map;
import javax.security.auth.Subject;

import javax.management.remote.message.*;
import javax.management.remote.generic.*;

/**
 * This interface specifies a set of common methods for sychronized connection
 * between a client and a server.
 */
public interface SynchroMessageConnection {

    /**
     * <p>Establish the connection.  This method must be called before
     * any other method of this interface.  The behavior is unspecified
     * if not.</p>
     *
     * @param env the properties of the connection.
     *
     * @exception IOException if the connection cannot be made.
     */
    public void connect(Map env) throws IOException;

    /** 
     * Sends a message to the remote side and does not need a response.
     * This method can be implemented in different ways:
     * <P>1) sends out the message and gets return at once. For example,
     * if the underlying protocol is asynchronous.
     * <P>2) sends out the message and is blocked until arrival of a response.
     * For example, if the underlying protocol supports only synchronous communication.
     * <P>3) throws <code>UnsupportedOperationException</code> to tell the caller
     * that it is not supported by the implementation. For example, if the object is used
     * on a server side and it is not mandatory for the server to send a request spontaneously.
     *
     * @exception UnsupportedOperationException thrown if the operation is not supported
     * by an implementation.
     * @exception IOException if a message could not be sent because of a communication problem.
     */
    public void sendOneWay(Message msg) throws IOException, UnsupportedOperationException;

    /**
     * Returns this connection identifier.
     */
    public String getConnectionId();

    /**
     * Closes this connection.
     */
    public void close() throws IOException;
}
