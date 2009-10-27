/*
 * @(#)file      AdminClient.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.29
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.management.remote.generic.MessageConnection;
import javax.management.remote.message.Message;
import javax.management.remote.message.HandshakeBeginMessage;
import javax.management.remote.message.HandshakeEndMessage;
import javax.management.remote.message.HandshakeErrorMessage;
import javax.management.remote.message.VersionMessage;
import javax.management.remote.message.ProfileMessage;

import com.sun.jmx.remote.generic.ClientAdmin;
import com.sun.jmx.remote.generic.ProfileClient;
import com.sun.jmx.remote.generic.ProfileClientFactory;
import com.sun.jmx.remote.generic.SelectProfiles;
import com.sun.jmx.remote.opt.util.ClassLogger;
import com.sun.jmx.remote.opt.util.EnvHelp;

/**
 *
 */
public class AdminClient implements ClientAdmin {

    public AdminClient(Map env) {
        this.env = (env != null) ? env : Collections.EMPTY_MAP;
    }

    public MessageConnection connectionOpen(MessageConnection mc)
        throws IOException {

        boolean sendError = true;
        HandshakeErrorMessage error = null;

        try {

            // Begin Handshake
            //
            HandshakeBeginMessage begin = null;
            Message msg = mc.readMessage();
            if (msg instanceof HandshakeBeginMessage) {
                begin = (HandshakeBeginMessage) msg;
            } else if (msg instanceof HandshakeErrorMessage) {
                // Throw exception and let GenericConnector
                // close the connection
                //
                sendError = false;
                error = (HandshakeErrorMessage) msg;
                throwExceptionOnError(error);
            } else {
                throw new IOException("Unexpected message: " +
                                      msg.getClass().getName());
            }

            String serverProfiles = begin.getProfiles();
            String serverVersion = begin.getVersion();
	    if (logger.traceOn()) {
		logger.trace("connectionOpen", ">>>>> Handshake Begin <<<<<");
		logger.trace("connectionOpen", "Server Supported Profiles [ " +
			     serverProfiles + " ]");
		logger.trace("connectionOpen", "Server JMXMP Version [ " +
			     serverVersion + " ]");
	    }

            // Negotiate JMXMP protocol version
            //
            String clientVersion = "1.0";
            if (!clientVersion.equals(serverVersion)) {
                if (clientVersion.compareTo(serverVersion) > 0) {
                    throw new IOException("The client is already using the " +
                                          "lowest JMXMP protocol version [" +
                                          clientVersion + "]");
                } else {
                    VersionMessage cjmxmp = new VersionMessage(clientVersion);
                    mc.writeMessage(cjmxmp);
                    msg = mc.readMessage();
                    if (msg instanceof VersionMessage) {
                        VersionMessage sjmxmp = (VersionMessage) msg;
                        if (!clientVersion.equals(sjmxmp.getVersion())) {
                            throw new IOException("Protocol version " +
                                                  "mismatch: Client [" +
                                                  clientVersion +
                                                  "] vs. Server [" +
                                                  sjmxmp.getVersion() + "]");
                        }
                    } else if (msg instanceof HandshakeErrorMessage) {
                        // Throw exception and let GenericConnector
                        // close the connection
                        //
                        sendError = false;
                        error = (HandshakeErrorMessage) msg;
                        throwExceptionOnError(error);
                    } else {
                        throw new IOException("Unexpected message: " +
                                              msg.getClass().getName());
                    }
                }
            }

            // Execute client selected profiles
            //
            List profileList = selectProfiles(serverProfiles);
            for (Iterator i = profileList.iterator(); i.hasNext(); ) {
                String profile = (String) i.next();
		ProfileClient p =
		    ProfileClientFactory.createProfile(profile, env);
		if (logger.traceOn()) {
		    logger.trace("connectionOpen",
				 ">>>>> Profile " +
				 p.getClass().getName() +
				 " <<<<<");
		}
		ProfileMessage pm = null;
		p.initialize(mc);
		while (!p.isComplete()) {
		    pm = p.produceMessage();
		    mc.writeMessage(pm);
		    msg = mc.readMessage();
		    if (msg instanceof ProfileMessage) {
			p.consumeMessage((ProfileMessage)msg);
		    } else if (msg instanceof HandshakeErrorMessage) {
			// Throw exception and let GenericConnector
			// close the connection
			//
			sendError = false;
			error = (HandshakeErrorMessage) msg;
			throwExceptionOnError(error);
		    } else {
			throw new IOException("Unexpected message: " +
					      msg.getClass().getName());
		    }
		}
		p.activate();
		profilesList.add(p);
	    }

            // Send client handshake end
            //
            Object ccontext = (Object) env.get("jmx.remote.context");
            HandshakeEndMessage cend = new HandshakeEndMessage(ccontext, null);
	    if (logger.traceOn()) {
		logger.trace("connectionOpen",
			     ">>>>> Handshake End <<<<<");
		logger.trace("connectionOpen",
			     "Client Context Object [ " + ccontext + " ]");
	    }
            mc.writeMessage(cend);

            // Wait for server handshake end
            //
            HandshakeEndMessage send = null;
            msg = mc.readMessage();
            if (msg instanceof HandshakeEndMessage) {
                send = (HandshakeEndMessage) msg;
            } else if (msg instanceof HandshakeErrorMessage) {
                // Throw exception and let GenericConnector
                // close the connection
                //
                sendError = false;
                error = (HandshakeErrorMessage) msg;
                throwExceptionOnError(error);
            } else {
                throw new IOException("Unexpected message: " +
                                      msg.getClass().getName());
            }
            Object scontext = send.getContext();
            connectionId = send.getConnectionId();
	    if (logger.traceOn()) {
		logger.trace("connectionOpen",
			     "Server Context Object [ " + scontext + " ]");
		logger.trace("connectionOpen",
			     "Server Connection Id [ " + connectionId + " ]");
	    }
        } catch (Exception e) {
            if (sendError) {
                try {
                    mc.writeMessage(new HandshakeErrorMessage(e.toString()));
                } catch (Exception hsem) {
                    if (logger.debugOn()) {
                        logger.debug(
                           "connectionOpen",
                           "Could not send HandshakeErrorMessage to the server",
                           hsem);
                    }
                }
            }
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            } else if (e instanceof IOException) {
                throw (IOException) e;
            } else {
		throw (IOException)
		    EnvHelp.initCause(new IOException(e.getMessage()), e);
            }
        }

        return mc;
    }

    public void connectionClosed(MessageConnection mc) {
        for (Iterator i = profilesList.iterator(); i.hasNext(); ) {
            ProfileClient p = (ProfileClient) i.next();
            try {
                p.terminate();
            } catch (Exception e) {
		if (logger.debugOn()) {
		    logger.debug("connectionClosed",
				 "Got an exception to terminate a ProfileClient: "+p.getName(), e);
		}
            }
        }
        profilesList.clear();
    }

    public String getConnectionId() {
        return connectionId;
    }

    /**
     * If a profile selector has been supplied in the map then use it.
     * Otherwise, the default profile selection algorithm implemented
     * works as follows:
     * <p>
     * Upon reception of the HandshakeBeginMessage the client verifies that
     * the profiles he wants to use, i.e. the ones he specified in the
     * environment map through the jmx.remote.profiles property are all
     * present in the server's supported profiles list. If false, the client
     * sends a HandshakeErrorMessage to the server and closes the connection.
     * Otherwise, the client starts exchanging profile messages with the server
     * for the selected profiles following the order specified in the client's
     * profile list.
     */
    private List selectProfiles(String serverProfiles) throws Exception {

	// If a profile selector is provided it takes precedence
	// over the default implementation.
	//
	SelectProfiles profileSelector =
	    (SelectProfiles) env.get("com.sun.jmx.remote.profile.selector");
	if (profileSelector != null) {
	    profileSelector.selectProfiles(env, serverProfiles);
	    String clientProfiles = (String) env.get("jmx.remote.profiles");
	    if (clientProfiles == null) {
		return Collections.EMPTY_LIST;
	    } else {
		StringTokenizer cst = new StringTokenizer(clientProfiles, " ");
		ArrayList clientProfilesList = new ArrayList(cst.countTokens());
		while (cst.hasMoreTokens()) {
		    String clientToken = cst.nextToken();
		    clientProfilesList.add(clientToken);
		}
		return clientProfilesList;
	    }
	}

	// Default implementation: The server supported profiles
	// must contain all the client required profiles.
	//
        String clientProfiles = (String) env.get("jmx.remote.profiles");

	// Check for null values. Both the server and the client
	// environment maps did not specified any profile.
	//
	boolean serverFlag =
	    (serverProfiles == null || serverProfiles.equals(""));

	boolean clientFlag =
	    (clientProfiles == null || clientProfiles.equals(""));

	if (serverFlag && clientFlag)
            return Collections.EMPTY_LIST;

	if (serverFlag)
	    throw new IOException("The server does not support any " +
				  "profile but the client requires one");

	if (clientFlag)
	    throw new IOException("The client does not require any " +
				  "profile but the server mandates one");

	// Neither the client nor the server profiles are null.
	//
        StringTokenizer sst = new StringTokenizer(serverProfiles, " ");
        ArrayList serverProfilesList = new ArrayList(sst.countTokens());
        while (sst.hasMoreTokens()) {
            String serverToken = sst.nextToken();
            serverProfilesList.add(serverToken);
        }
        int serverProfilesListSize = serverProfilesList.size();

        StringTokenizer cst = new StringTokenizer(clientProfiles, " ");
        ArrayList clientProfilesList = new ArrayList(cst.countTokens());
        while (cst.hasMoreTokens()) {
            String clientToken = cst.nextToken();
            clientProfilesList.add(clientToken);
        }
        int clientProfilesListSize = clientProfilesList.size();

        if ((serverProfilesListSize < clientProfilesListSize) ||
	    (!serverProfilesList.containsAll(clientProfilesList)))
	    throw new IOException("The server supported profiles " +
				  serverProfilesList + " do not " +
				  "match the client required profiles " +
				  clientProfilesList + ".");

        return clientProfilesList;
    }

    /**
     * If a HandshakeErrorMessage is received then choose the appropriate
     * exception to throw based on the error string detail.
     */
    static void throwExceptionOnError(HandshakeErrorMessage error)
	throws IOException, SecurityException {
	final String detail = error.getDetail();
	if (detail.startsWith("java.lang.SecurityException") ||
	    detail.startsWith("java.security.") ||
	    detail.startsWith("javax.net.ssl.") ||
	    detail.startsWith("javax.security.")) {
	    throw new SecurityException(detail);
	} else {
	    throw new IOException(detail);
	}
    }

    private Map env = null;
    private String connectionId = null;
    private List profilesList = new ArrayList();
    private static final ClassLogger logger =
	new ClassLogger("javax.management.remote.misc", "AdminClient");
}
