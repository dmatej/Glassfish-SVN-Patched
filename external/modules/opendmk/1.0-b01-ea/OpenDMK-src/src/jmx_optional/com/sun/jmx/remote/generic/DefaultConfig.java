/*
 * @(#)file      DefaultConfig.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.27
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

import java.util.Map;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.InstanceNotFoundException;

import javax.management.remote.JMXConnectorFactory;

import com.sun.jmx.remote.generic.ServerAdmin;
import com.sun.jmx.remote.generic.SynchroMessageConnection;
import com.sun.jmx.remote.generic.SynchroMessageConnectionServer;

import com.sun.jmx.remote.opt.util.EnvHelp;

public class DefaultConfig {

    /**
     * <p>Name of the attribute that specifies the maximum number of
     * threads used at the server side for each client connection.</p>
     */
    public final static String SERVER_MAX_THREADS =
	"jmx.remote.x.server.max.threads";

    /** 
     * Returns the maximum number of threads used at server side for
     * each client connection.  Its default value is 10.
     */
    public static int getServerMaxThreads(Map env) {
	return (int) EnvHelp.getIntegerAttribute(env, SERVER_MAX_THREADS,
						 10, 1, Integer.MAX_VALUE);
    }

    /**
     * <p>Name of the attribute that specifies the minimum number of
     * threads used at the server side for each client connection.</p>
     */
    public final static String SERVER_MIN_THREADS =
	"jmx.remote.x.server.min.threads";

    /** 
     * Returns the minimum number of threads used at server side for
     * each client connection.  Its default value is 1.
     */
    public static int getServerMinThreads(Map env) {
	return (int) EnvHelp.getIntegerAttribute(env, SERVER_MIN_THREADS,
						 1, 1, Integer.MAX_VALUE);
    }

    /**
     * <p>Name of the attribute that specifies the timeout in
     * milliseconds for a client request to wait for its response.
     * The default value is <code>Long.MAX_VALUE</code>.
     */
    public static final String REQUEST_WAITING_TIME =
	"jmx.remote.x.request.timeout";

    /** 
     * Returns the timeout for a client request.
     * Its default value is <code>Long.MAX_VALUE</code>.
     */
    public static long getRequestTimeout(Map env) {
	return EnvHelp.getIntegerAttribute(env, REQUEST_WAITING_TIME,
					   Long.MAX_VALUE, 0, Long.MAX_VALUE);
    }


    /**
     * <p>Name of the attribute that specifies the timeout in
     * milliseconds for a server to finish connecting with a new client.
     * Zero means no timeout.
     * If a user-specified value is less than or equal to zero, or more than the max value,
     * Zero will be used.
     * The default value is zero.
     */
    public static final String SERVER_SIDE_CONNECTING_TIMEOUT =
	"jmx.remote.x.server.side.connecting.timeout";

    /** 
     * Returns the connecting timeout at server side for a new client, zero means no timeout.
     * If a user-specified value is less than zero, zero will be used.
     * The default value is 0.
     */

    public static long getConnectingTimeout(Map env) {
	long l;

	try {
	    l = EnvHelp.getIntegerAttribute(env, SERVER_SIDE_CONNECTING_TIMEOUT,
					   0, 0, Long.MAX_VALUE);
	} catch (IllegalArgumentException iae) {
	    l = 0;
	}

	return l;
    }

    /**
     * <p>Name of the attribute that specifies a ServerAdmin object.
     * The value associated with this attribute is ServerAdmin object</p>
     */
    public static final String SERVER_ADMIN =
	"com.sun.jmx.remote.server.admin";

    /** 
     * Returns an instance of ServerAdmin.  Its default value is a
     * <code>com.sun.jmx.remote.opt.security.AdminServer</code>.
     */
    public static ServerAdmin getServerAdmin(Map env) {
	ServerAdmin admin;
	final Object o = env.get(SERVER_ADMIN);

	if (o == null) {
	    admin = new com.sun.jmx.remote.opt.security.AdminServer(env);
	} else if (o instanceof ServerAdmin) {
	    admin = (ServerAdmin)o;
	} else {
	    final String msg =
		"The specified attribute \"" + SERVER_ADMIN +
		"\" is not a ServerAdmin object.";
	    throw new IllegalArgumentException(msg);
	}

	return admin;
    }

    /**
     * <p>Name of the attribute that specifies a ClientAdmin object.
     * The value associated with this attribute is ClientAdmin object</p>
     */
    public static final String CLIENT_ADMIN =
	"com.sun.jmx.remote.client.admin";

    /** 
     * Returns an instance of ClientAdmin.  Its default value is a
     * <code>com.sun.jmx.remote.opt.security.AdminClient</code>.
     */
    public static ClientAdmin getClientAdmin(Map env) {
	ClientAdmin admin;
	final Object o = env.get(CLIENT_ADMIN);

	if (o == null) {
	    admin = new com.sun.jmx.remote.opt.security.AdminClient(env);
	} else if (o instanceof ClientAdmin) {
	    admin = (ClientAdmin)o;
	} else {
	    final String msg =
		"The specified attribute \"" + CLIENT_ADMIN +
		"\" is not a ClientAdmin object.";
	    throw new IllegalArgumentException(msg);
	}

	return admin;
    }

    /**
     * <p>Name of the attribute that specifies a
     * <code>SynchroMessageConnectionServer</code> object.  The value
     * associated with this attribute is a
     * <code>SynchroMessageConnectionServer</code> object</p>
     */
    public static final String SYNCHRO_MESSAGE_CONNECTION_SERVER =
	"com.sun.jmx.remote.generic.synchro.server";

    /** 
     * Returns a <code>SynchroMessageConnectionServer</code> object
     * specified in the <code>Map</code> object. Returns null if it is
     * not specified in the map.
     */
    public static SynchroMessageConnectionServer
	    getSynchroMessageConnectionServer(Map env) {
	SynchroMessageConnectionServer ret = null;
	if (env != null) {
	    ret = (SynchroMessageConnectionServer)
		env.get(SYNCHRO_MESSAGE_CONNECTION_SERVER);
	}

	return ret;
    }

    /**
     * <p>Name of the attribute that specifies a
     * <code>ClientSynchroMessageConnection</code> object.  The value
     * associated with this attribute is a
     * <code>ClientSynchroMessageConnection</code> object</p>
     */
    public static final String CLIENT_SYNCHRO_MESSAGE_CONNECTION =
	"com.sun.jmx.remote.generic.synchro.client";

    /** 
     * Returns a <code>ClientSynchroMessageConnection</code> object
     * specified in the <code>Map</code> object. Returns null if it is
     * not specified in the map.
     */
    public static ClientSynchroMessageConnection
	    getClientSynchroMessageConnection(Map env) {
	ClientSynchroMessageConnection ret = null;
	if (env != null) {
	    ret = (ClientSynchroMessageConnection)
		env.get(CLIENT_SYNCHRO_MESSAGE_CONNECTION);
	}

	return ret;
    }

    /**
     * <p>Name of the attribute that specifies the timeout in
     * milliseconds for a client to wait for its state to become
     * connected.  The default value is 0.</p>
     */
    public static final String TIMEOUT_FOR_CONNECTED_STATE =
	"jmx.remote.x.client.connected.state.timeout";

    /** 
     * Returns the timeout in milliseconds for a client to wait for
     * its state to become connected.  The default timeout is 1
     * second.</p>
     */
    public static long getTimeoutForWaitConnectedState(Map env) {
	return EnvHelp.getIntegerAttribute(env, TIMEOUT_FOR_CONNECTED_STATE,
					   1000, 1, Long.MAX_VALUE);
    }

    /**
     * <p>Name of the attribute that specifies whether or not we set
     * ReuseAddress flag to a Server Socket. Its default value is false</p>
     */
    public final static String SERVER_REUSE_ADDRESS =
	"jmx.remote.x.server.reuse.address";

    /** 
     * Returns a value telling whether or not we set
     * ReuseAddress flag to a Server Socket.  Its default value is false.
     */
    public static boolean getServerReuseAddress(Map env) {
	final Object o;

	if (env == null || (o = env.get(SERVER_REUSE_ADDRESS)) == null)
	    return false;

	if (o instanceof Boolean) {
	    return ((Boolean)o).booleanValue();
	} else if (o instanceof String) {
	    return Boolean.valueOf((String)o).booleanValue();
	}

	throw new IllegalArgumentException("Attribute "+SERVER_REUSE_ADDRESS+
					   " value must be Boolean or String.");
    }

    /**
     * <p>Name of the attribute that specifies whether or not do reconnection if 
     * the client heartbeat
     * gets an InterruptedIOException because of a request timeout.
     * Its default value is false.</p>
     */
    public static final String TIMEOUT_RECONNECTION =
	"jmx.remote.x.client.timeout.reconnection";

    /** 
     * Returns a value telling whether or not we do reconnection if the client
     * heartbeat gets an InterruptedIOException because of a request timeout.
     * Its default value is false.
     */
    public static boolean getTimeoutReconnection(Map env) {
	final Object o;

	if (env == null || (o = env.get(TIMEOUT_RECONNECTION)) == null)
	    return false;

	if (o instanceof Boolean) {
	    return ((Boolean)o).booleanValue();
	} else if (o instanceof String) {
	    return Boolean.valueOf((String)o).booleanValue();
	}

	throw new IllegalArgumentException("Attribute "+TIMEOUT_RECONNECTION+
					   " value must be Boolean or String.");
    }
}
