/*
 * @(#)file      SASLServerHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.24
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

package com.sun.jmx.remote.opt.security;

import java.io.IOException;
import java.net.Socket;
import java.security.AccessController;
import java.security.Principal;
import java.security.PrivilegedAction;
import java.util.Map;

import javax.management.remote.JMXPrincipal;
import javax.management.remote.generic.MessageConnection;
import javax.management.remote.message.ProfileMessage;
import javax.management.remote.message.SASLMessage;
import javax.security.auth.Subject;
import javax.security.auth.callback.CallbackHandler;
import javax.security.sasl.Sasl;
import javax.security.sasl.SaslServer;

import com.sun.jmx.remote.generic.ProfileServer;
import com.sun.jmx.remote.opt.util.ClassLogger;
import com.sun.jmx.remote.socket.SocketConnectionIf;

/**
 * This class implements the server side SASL profile.
 */
public class SASLServerHandler implements ProfileServer {

    //-------------
    // Constructors
    //-------------

    public SASLServerHandler(String profile, Map env) {
        this.profile = profile;
        this.env = env;
    }

    //---------------------------------------
    // ProfileServer interface implementation
    //---------------------------------------

    public void initialize(MessageConnection mc, Subject s) throws IOException {

        this.mc = mc;
	this.subject = s;

        // Check if instance of SocketConnectionIf
        // and retrieve underlying socket
        //
        if (mc instanceof SocketConnectionIf) {
            socket = ((SocketConnectionIf)mc).getSocket();
        } else {
            throw new IOException("Not an instance of SocketConnectionIf");
        }

        // Prepare parameters for creating SASL server
        //
	mechanism = profile.substring(profile.indexOf("SASL/") + 5);
        String server = (String) env.get("jmx.remote.x.sasl.server.name");
        if (server == null)
            server = socket.getLocalAddress().getHostName();
        CallbackHandler cbh =
            (CallbackHandler) env.get("jmx.remote.sasl.callback.handler");

	// Create SASL server to use using SASL package
	//
	saslServer = Sasl.createSaslServer(mechanism, "jmxmp",
					   server, env, cbh);
	if (saslServer == null) {
	    final String detail =
		"Unable to create SASL server connection for " +
		"authentication mechanism [" + mechanism + "]";
	    throw new IOException(detail);
	}
    }

    public ProfileMessage produceMessage() throws IOException {
	int status;
	if (saslServer.isComplete()) {
	    status = SASLMessage.COMPLETE;
	} else {
	    status = SASLMessage.CONTINUE;
	}
	SASLMessage challenge = new SASLMessage(mechanism, status, blob);
	if (logger.traceOn()) {
	    logger.trace("produceMessage",
			 ">>>>> SASL server message <<<<<");
	    logger.trace("produceMessage",
			 "Profile Name : " + challenge.getProfileName());
	    logger.trace("produceMessage",
			 "Status : " + challenge.getStatus());
	}
        return challenge;
    }

    public void consumeMessage(ProfileMessage pm) throws IOException {
        if (!(pm instanceof SASLMessage)) {
            throw new IOException("Unexpected profile message type: " +
                                  pm.getClass().getName());
        }
        SASLMessage response = (SASLMessage) pm;
	if (logger.traceOn()) {
	    logger.trace("consumeMessage",
			 ">>>>> SASL client message <<<<<");
	    logger.trace("consumeMessage",
			 "Profile Name : " + response.getProfileName());
	    logger.trace("consumeMessage",
			 "Status : " + response.getStatus());
	}
        if (response.getStatus() != SASLMessage.CONTINUE) {
            throw new IOException("Unexpected SASL status [" +
                                  response.getStatus() + "]");
        }
	if (!saslServer.isComplete()) {
	    blob = saslServer.evaluateResponse(response.getBlob());
	} else {
	    throw new IOException("SASL authentication complete despite " +
				  "the client claim for non-completion");
	}
    }

    public boolean isComplete() {
	return saslServer.isComplete();
    }

    public Subject activate() throws IOException {
	// If negotiated integrity or privacy
	//
	String qop = (String) saslServer.getNegotiatedProperty(Sasl.QOP);
	if (qop != null &&
	    (qop.equalsIgnoreCase("auth-int") ||
	     qop.equalsIgnoreCase("auth-conf"))) {
	    // Replace the current input/output streams in
	    // MessageConnection by the SASL input/output streams
	    //
	    SASLInputStream saslis =
		new SASLInputStream(saslServer, socket.getInputStream());
	    SASLOutputStream saslos =
		new SASLOutputStream(saslServer, socket.getOutputStream());
	    ((SocketConnectionIf)mc).replaceStreams(saslis, saslos);
	}
	// Retrieve authorization id
	//
	final String authorizationId = saslServer.getAuthorizationID();
	final Principal principal = new JMXPrincipal(authorizationId);
	if (subject == null) subject = new Subject();
	AccessController.doPrivileged(new PrivilegedAction() {
		public Object run() {
		    subject.getPrincipals().add(principal);
		    return null;
		}
	    });
	return subject;
    }

    public void terminate() throws IOException {
        saslServer.dispose();
    }

    public String getName() {
	return profile;
    }

    //--------------------
    // Protected variables
    //--------------------

    private SaslServer saslServer = null;

    //------------------
    // Private variables
    //------------------

    private byte[] blob = null;
    private Map env = null;
    private MessageConnection mc = null;
    private Socket socket = null;
    private String mechanism = null;
    private String profile = null;
    private Subject subject = null;
    private static final ClassLogger logger =
	new ClassLogger("javax.management.remote.misc", "SASLServerHandler");
}
