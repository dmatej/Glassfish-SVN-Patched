/*
 * @(#)file      AdminServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.37
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.WeakHashMap;

import javax.management.remote.JMXAuthenticator;
import javax.management.remote.generic.MessageConnection;
import javax.management.remote.message.Message;
import javax.management.remote.message.HandshakeBeginMessage;
import javax.management.remote.message.HandshakeEndMessage;
import javax.management.remote.message.HandshakeErrorMessage;
import javax.management.remote.message.VersionMessage;
import javax.management.remote.message.ProfileMessage;
import javax.security.auth.Subject;

import com.sun.jmx.remote.generic.CheckProfiles;
import com.sun.jmx.remote.generic.ProfileServer;
import com.sun.jmx.remote.generic.ProfileServerFactory;
import com.sun.jmx.remote.generic.ServerAdmin;
import com.sun.jmx.remote.opt.util.ClassLogger;
import com.sun.jmx.remote.socket.SocketConnectionIf;

/**
 *
 */
public class AdminServer implements ServerAdmin {

    public AdminServer(Map env) {
	this.env = (env != null) ? env : Collections.EMPTY_MAP;
    }

    public MessageConnection connectionOpen(MessageConnection mc)
	    throws IOException {

	boolean sendError = true;

	try {

	    // Initialize the Subject that will be passed to all
	    // the negotiated profiles
	    //
	    Subject subject = null;

	    // Begin Handshake
	    //
	    String serverProfiles = (String) env.get("jmx.remote.profiles");
	    String serverVersion = "1.0";
	    if (logger.traceOn()) {
		logger.trace("connectionOpen", ">>>>> Handshake Begin <<<<<");
		logger.trace("connectionOpen", "Server Supported Profiles [ " +
			     serverProfiles + " ]");
		logger.trace("connectionOpen", "Server JMXMP Version [ " +
			     serverVersion + " ]");
	    }
	    HandshakeBeginMessage begin =
		new HandshakeBeginMessage(serverProfiles, serverVersion);
	    mc.writeMessage(begin);

	    while (true) {
		Message msg = mc.readMessage();
		if (msg instanceof HandshakeErrorMessage) {
		    // Throw exception and let GenericConnectorServer
		    // close the connection
		    //
		    sendError = false;
		    HandshakeErrorMessage error = (HandshakeErrorMessage) msg;
		    AdminClient.throwExceptionOnError(error);
		} else if (msg instanceof HandshakeEndMessage) {
		    HandshakeEndMessage cend = (HandshakeEndMessage) msg;
		    Object ccontext = cend.getContext();
		    if (logger.traceOn()) {
			logger.trace("connectionOpen",
				     ">>>>> Handshake End <<<<<");
			logger.trace("connectionOpen",
				     "Client Context Object [ " +
				     ccontext + " ]");
		    }
		    Object scontext = (Object) env.get("jmx.remote.context");
		    // If MessageConnection is an instance of SocketConnectionIf
		    // then set the authenticated subject.
		    if (mc instanceof SocketConnectionIf) {
			((SocketConnectionIf)mc).setSubject(subject);
		    }
		    String connectionId = mc.getConnectionId();
		    // If the environment includes an authenticator, check
		    // that it accepts the connection id, and replace the
		    // Subject by whatever it returns.
		    JMXAuthenticator authenticator =
			(JMXAuthenticator) env.get("jmx.remote.authenticator");
		    if (authenticator != null) {
			Object[] credentials = {connectionId, subject};
			subject = authenticator.authenticate(credentials);
			if (mc instanceof SocketConnectionIf) {
			    ((SocketConnectionIf)mc).setSubject(subject);
			}
			connectionId = mc.getConnectionId();
		    }
		    if (logger.traceOn()) {
			logger.trace("connectionOpen",
				     "Server Context Object [ " +
				     scontext + " ]");
			logger.trace("connectionOpen",
				     "Server Connection Id [ " +
				     connectionId + " ]");
		    }

		    // Check that the negotiated profiles are acceptable for
		    // the server's defined security policy. This method is
		    // called just before the initial handshake is completed
		    // with a HandshakeEndMessage sent from the server to the
		    // client. If the method throws an exception, then a
		    // HandshakeErrorMessage will be sent instead.
		    //
		    List profileNames = getProfilesByName(mc);
		    CheckProfiles np = (CheckProfiles)
			env.get("com.sun.jmx.remote.profile.checker");
		    if (np != null) {
			np.checkProfiles(env,
					 profileNames,
					 ccontext,
					 connectionId);
		    } else {
			checkProfilesForEquality(serverProfiles,
						 profileNames);
		    }

		    HandshakeEndMessage send =
			new HandshakeEndMessage(scontext, connectionId);
		    mc.writeMessage(send);
		    break;
		} else if (msg instanceof VersionMessage) {
		    VersionMessage cjmxmp = (VersionMessage) msg;
		    String clientVersion = cjmxmp.getVersion();
		    if (clientVersion.equals(serverVersion)) {
			VersionMessage sjmxmp =
			    new VersionMessage(serverVersion);
			mc.writeMessage(sjmxmp);
		    } else {
			throw new IOException("Protocol version " +
					      "mismatch: Client [" +
					      clientVersion +
					      "] vs. Server [" +
					      serverVersion + "]");
		    }
		} else if (msg instanceof ProfileMessage) {
		    ProfileMessage pm = (ProfileMessage) msg;
		    String pn = pm.getProfileName();
		    ProfileServer p = (ProfileServer) getProfile(mc, pn);
		    if (p == null) {
			p = ProfileServerFactory.createProfile(pn, env);
			if (logger.traceOn()) {
			    logger.trace("connectionOpen",
					 ">>>>> Profile " +
					 p.getClass().getName() +
					 " <<<<<");
			}
			p.initialize(mc, subject);
			putProfile(mc, p);
		    }
		    p.consumeMessage(pm);
		    pm = p.produceMessage();
		    mc.writeMessage(pm);
		    if (p.isComplete()) {
			subject = p.activate();
		    }
		} else {
		    throw new IOException("Unexpected message: " +
					  msg.getClass().getName());
		}
	    }
	    putSubject(mc, subject);
	} catch (Exception e) {
	    if (sendError) {
                try {
                    mc.writeMessage(new HandshakeErrorMessage(e.toString()));
                } catch (Exception hsem) {
                    if (logger.debugOn()) {
                        logger.debug(
                           "connectionOpen",
                           "Could not send HandshakeErrorMessage to the client",
                           hsem);
                    }
                }
	    }
	    if (e instanceof RuntimeException) {
		throw (RuntimeException) e;
	    } else if (e instanceof IOException) {
		throw (IOException) e;
	    } else {
		throw new IOException(e.getMessage());
	    }
	}

	return mc;
    }

    public void connectionClosed(MessageConnection mc) {
	removeSubject(mc);
	removeProfiles(mc);
    }

    public Subject getSubject(MessageConnection mc) {
	synchronized (subjectsTable) {
	    return (Subject) subjectsTable.get(mc);
	}
    }

    private void putSubject(MessageConnection mc, Subject s) {
	synchronized (subjectsTable) {
	    subjectsTable.put(mc, s);
	}
    }

    private void removeSubject(MessageConnection mc) {
	synchronized (subjectsTable) {
	    subjectsTable.remove(mc);
	}
    }

    private ProfileServer getProfile(MessageConnection mc, String pn) {
	synchronized (profilesTable) {
	    ArrayList list = (ArrayList) profilesTable.get(mc);
	    if (list == null) {
		return null;
	    }
	    for (Iterator i = list.iterator(); i.hasNext(); ) {
		ProfileServer p = (ProfileServer) i.next();
		if (p.getName().equals(pn)) {
		    return p;
		}
	    }
	    return null;
	}
    }

    private synchronized void putProfile(MessageConnection mc,
					 ProfileServer p) {
	synchronized (profilesTable) {
	    ArrayList list = (ArrayList) profilesTable.get(mc);
	    if (list == null) {
		list = new ArrayList();
		profilesTable.put(mc, list);
	    }
	    if (!list.contains(p)) {
		list.add(p);
	    }
	}
    }

    private ArrayList getProfiles(MessageConnection mc) {
	synchronized (profilesTable) {
	    return (ArrayList) profilesTable.get(mc);
	}
    }

    private ArrayList getProfilesByName(MessageConnection mc) {
	ArrayList profiles = getProfiles(mc);
	if (profiles == null)
	    return null;
	ArrayList profileNames = new ArrayList(profiles.size());
	for (Iterator i = profiles.iterator(); i.hasNext(); ) {
	    ProfileServer p = (ProfileServer) i.next();
	    profileNames.add(p.getName());
	}
	return profileNames;
    }

    private synchronized void removeProfiles(MessageConnection mc) {
	synchronized (profilesTable) {
	    ArrayList list = (ArrayList) profilesTable.get(mc);
	    if (list != null) {
		for (Iterator i = list.iterator(); i.hasNext(); ) {
		    ProfileServer p = (ProfileServer) i.next();
		    try {
			p.terminate();
		    } catch (Exception e) {
			if (logger.debugOn()) {
			    logger.debug("removeProfiles",
			      "Got an exception to terminate a ProfileServer: "+p.getName(), e);
			}
		    }
		}
		list.clear();
	    }
	    profilesTable.remove(mc);
	}
    }

    private void checkProfilesForEquality(String serverProfiles,
					  List clientProfilesList)
	throws IOException {

	// Check for null values. Both the server and the client
	// environment maps did not specified any profile.
	//
	boolean serverFlag =
	    (serverProfiles == null || serverProfiles.equals(""));

	boolean clientFlag =
	    (clientProfilesList == null || clientProfilesList.isEmpty());

	if (serverFlag && clientFlag)
	    return;

	if (serverFlag)
	    throw new IOException("The server does not support any " +
				  "profile but the client requires one");

	if (clientFlag)
	    throw new IOException("The client does not require any " +
				  "profile but the server mandates one");

	// Build ArrayList<String> from server profiles string.
	//
        StringTokenizer sst = new StringTokenizer(serverProfiles, " ");
        ArrayList serverProfilesList = new ArrayList(sst.countTokens());
        while (sst.hasMoreTokens()) {
            String serverToken = sst.nextToken();
            serverProfilesList.add(serverToken);
        }

	// Check for size equality.
	//
	if (serverProfilesList.size() != clientProfilesList.size())
	    throw new IOException("The client negotiated profiles do not " +
				  "match the server required profiles.");

	// Check for content equality.
	//
	if (!clientProfilesList.containsAll(serverProfilesList))
	    throw new IOException("The client negotiated profiles " +
				  clientProfilesList + " do not match " +
				  "the server required profiles " +
				  serverProfilesList + ".");
    }

    private Map env = null;
    private Map subjectsTable = new WeakHashMap();
    private Map profilesTable = new WeakHashMap();
    private static final ClassLogger logger =
	new ClassLogger("javax.management.remote.misc", "AdminServer");
}
