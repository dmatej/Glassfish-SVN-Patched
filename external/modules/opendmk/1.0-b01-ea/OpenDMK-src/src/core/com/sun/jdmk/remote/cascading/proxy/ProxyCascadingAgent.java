/*
 * @(#)file      ProxyCascadingAgent.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.26
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
package com.sun.jdmk.remote.cascading.proxy;

// java import
import java.util.Set;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;

// jdmk import
import javax.management.QueryExp;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.MBeanServerNotification;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.Notification;
import javax.management.MBeanRegistration;
import javax.management.MBeanNotificationInfo;
import javax.management.NotificationEmitter;
import javax.management.NotificationListener;
import javax.management.NotificationFilter;
import javax.management.NotificationBroadcasterSupport;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MalformedObjectNameException;

import javax.management.remote.JMXConnectionNotification;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import com.sun.jdmk.remote.cascading.CascadingAgent;
import com.sun.jdmk.remote.cascading.CascadingAgentMBean;
import com.sun.jdmk.remote.cascading.MBeanServerConnectionFactory;
import com.sun.jdmk.remote.cascading.MBeanServerConnectionWrapper;
import com.sun.jdmk.remote.cascading.BasicMBeanServerConnectionFactory;
import com.sun.jdmk.defaults.Utils;
import com.sun.jdmk.internal.ClassLogger;

/**
 * This class is an implementation of {@link CascadingAgent} that relies
 * on MBean proxying. 
 * <p>
 * Using this class directly is discouraged. You should envisage using
 * the {@link com.sun.jdmk.remote.cascading.CascadingService} instead.
 * <p>
 * A <tt>CascadingAgent</tt> is an MBean that is able to <i>mount</i> a partial
 * view of a <i>source MBeanServer</i> into a <i>target MBeanServer</i>.
 * The source <tt>MBeanServer</tt> is also sometimes called the 
 * <i>cascaded MBeanServer</i>, while the target <tt>MBeanServer</tt> is 
 * called the <i>cascading MBeanServer</i>.
 * <p>For each MBean cascaded from the source <tt>MBeanServer</tt>, the 
 * <tt>ProxyCascadingAgent</tt> will register a {@link CascadingProxy} in the 
 * target <tt>MBeanServer</tt>.
 * <p>
 * See {@link CascadingAgent} and {@link com.sun.jdmk.remote.cascading} 
 * for more details on the cascading concepts.
 *
 * @since Java DMK 5.1
 **/
public class ProxyCascadingAgent extends CascadingAgent {

    /**
     * A string describing the MBean state.
     * A usual state transition sequence is:
     * <pre>
     * STOPPED -> STARTING -> STARTED -> SHUTTING_DOWN -> STOPPED
     * </pre>
     * Usually the cascading agent is considered active when it is in
     * the STARTED state.
     * @see #isActive
     **/
    final static class State {
	/**
	 * The <tt>CascadingAgent</tt> state is switched to "Starting" 
	 * at the beginning of the <tt>start()</tt> method, and remains so
	 * as long as the <tt>start()</tt> method is in progress.
	 **/
	public final static String STARTING="Starting";

	/**
	 * The <tt>CascadingAgent</tt> state is switched "Started" at the
	 * end of the <tt>start()</tt> method, and remains so until the 
	 * <tt>CascadingAgent</tt> stops.
	 **/
	public final static String STARTED="Started";
	
	/**
	 * The <tt>CascadingAgent</tt> state is switched to "ShuttingDown" 
	 * at the beginning of the <tt>stop()</tt> method, and remains so
	 * as long as the <tt>stop()</tt> method is in progress.
	 **/
	public final static String SHUTTING_DOWN="ShuttingDown";

	/**
	 * The <tt>CascadingAgent</tt> state is switched "Stopped" at the
	 * end of the <tt>stop()</tt> method, and remains so until the 
	 * <tt>CascadingAgent</tt> is started again.
	 **/
	public final static String STOPPED="Stopped";
    }

    /**
     * Creates a <tt>ProxyCascadingAgent</tt> that will mount MBeans from
     * a source <tt>MBeanServer</tt> under the given <var>targetPath</var>.
     *
     * @param sourceConnection An <tt>MBeanServerConnectionFactory</tt>
     *        providing connections to the source (cascaded) 
     *        <tt>MBeanServer</tt>.
     *        <p>The <tt>CascadingAgent</tt> will call {@link 
     *        MBeanServerConnectionFactory#getMBeanServerConnection() 
     *        sourceConnection.getMBeanServerConnection()} every time it 
     *        needs to access the subagent.
     *        <p>
     * @param sourcePattern An <tt>ObjectName</tt> pattern that must be 
     *        satisfied by the <tt>ObjectName</tt>s of the source MBeans. 
     *        <p>
     *        The sourcePattern is evaluated in the context of the source 
     *        <tt>MBeanServer</tt>. The source pattern is used to perform
     *        a partial mount of the source <tt>MBeanServer</tt> in the target
     *        <tt>MBeanServer</tt>. Only those MBeans that satisfy the pattern
     *        will be mounted. The source pattern is thus a filter
     *        element. A <tt>null</tt> sourcePattern is equivalent to
     *        the wildcard <tt>"*:*"</tt>.
     *        <p>
     * @param sourceQuery A <tt>QueryExp</tt> that must be satisfied by the 
     *        source MBeans. 
     *        <p> The sourceQuery is evaluated in the context of 
     *        the source <tt>MBeanServer</tt>. <b>Using this functionality
     *        is discouraged</b>. It is recommended to always pass a 
     *        <tt>null</tt> parameter. If however, you chose to pass a 
     *        non null <var>sourceQuery</var>, the given <tt>QueryExp</tt> 
     *        should not involve any properties or attributes that vary 
     *        over time.
     *        The evaluation of the <var>sourceQuery</var> against a given 
     *        MBean should be guaranteed to always consistently yield the 
     *        same result. Our implementation does not enforce this 
     *        constraint, but the behavior of the <tt>CascadingAgent</tt> in 
     *        the case where this constraint were not respected is 
     *        unspecified and could be unpredictable.
     *        <p>
     * @param targetPath The <i>domain path</i> under which the source
     *        MBeans will be mounted in the target <tt>MBeanServer</tt>.
     *        <p>If this parameter is not <tt>null</tt>, all source MBean names
     *        will be transformed in the target <tt>MBeanServer</tt> by 
     *        prefixing their domain name with the string 
     *        <tt><i>targetPath</i>+"/"</tt>. An MBean whose name is
     *        <tt>"D:k1=v1,k2=v2"</tt> will thus be mounted as 
     *        <tt>"<i>targetPath</i>/D:k1=v1,k2=v2"</tt>.
     *        <br>
     *        <p>A <tt>null</tt> <var>targetPath</var> means that MBeans are
     *        mounted directly at the root of the hierarchy, that is, as if
     *        they were local MBeans. <b>Using a null <i>targetPath</i> is
     *        thus highly discouraged, as it could cause name conflicts
     *        in the target <tt>MBeanServer</tt></b>. 
     *        <p>
     *        Similarly, MBeans from different sources should not be
     *        mounted under the same <var>targetPath</var>. Moreover,
     *        an application should not attempt to mount source MBeans under
     *        a <var>targetPath</var> that already contain MBeans in the
     *        target <tt>MBeanServer</tt>.
     *        <p>
     *        However, our implementation does not enforce these rules: 
     *        It is the responsibility of the application creating the
     *        <tt>CascadingAgent</tt> to ensure the consistency of
     *        the mounting strategy.
     *        <p>
     *        <b>Note:</b> A zero-length <var>targetPath</var> is treated 
     *        as a null <var>targetPath</var>.
     *        <p>
     * @param description A human readable string describing this 
     *        <tt>CascadingAgent</tt>.
     *        <p>
     * @exception IllegalArgumentException if <var>targetPath</var> is
     *            not syntactically valid (e.g. it contains wildcard 
     *            characters).
     **/
    public ProxyCascadingAgent(MBeanServerConnectionFactory sourceConnection,
			       ObjectName  sourcePattern,
			       QueryExp    sourceQuery,
			       String      targetPath,
			       MBeanServer targetMBS,
			       String      description) {
	super(sourceConnection,sourcePattern,sourceQuery,targetPath,targetMBS);
	mbsNotifHandler   = new NotificationListener() {
		public void handleNotification(Notification notification, 
					       Object handback) {
		    handleMBeanServerNotification(notification,handback);
		}
	    };
	mbeanList = new HashMap();
	wrapper = new MBeanServerConnectionWrapper() {
		protected MBeanServerConnection getMBeanServerConnection() 
		    throws IOException {
		    return getConnectionFactory().getMBeanServerConnection();
		}
	    };
	state = State.STOPPED;
	sequenceNumber = 0;
	this.description = description;
    }

    /**
     * <p> Creates a new <tt>ProxyCascadingAgent</tt> - <b>Using this 
     *     constructor is discouraged</b>.</p>
     *
     * Creates a <tt>ProxyCascadingAgent</tt> that will mount MBeans from
     * a source <tt>MBeanServer</tt> at the root of the domain hierarchy. 
     * This is equivalent to mounting MBeans from a source 
     * <tt>MBeanServer</tt> to a target <tt>MBeanServer</tt> under a 
     * <tt>null</tt> <var>targetPath</var>.
     * <p>
     * A <tt>null</tt> <var>targetPath</var> means that MBeans are
     * mounted directly at the root of the hierarchy, that is, as if
     * they were local MBeans. <b>Using a null <i>targetPath</i> is
     * thus highly discouraged, as it could cause name conflicts
     * in the target <tt>MBeanServer</tt></b>. 
     * <p>
     * When using this constructor, the target <tt>MBeanServer</tt> is the 
     * <tt>MBeanServer</tt> in which this <tt>CascadingAgent</tt> is 
     * registered.
     * <p>
     * You should consider using the {@link 
     * #ProxyCascadingAgent(MBeanServerConnectionFactory,ObjectName,QueryExp,
     * String,MBeanServer,String) constructor} that takes a <i>targetPath</i>
     * and a <i>target MBeanServer</i> instead.
     * <p>
     * See {@link com.sun.jdmk.remote.cascading} description and 
     * {@link CascadingAgent}. 
     *
     * @param sourceConnection An <tt>MBeanServerConnectionFactory</tt>
     *        providing connections to the source (cascaded) 
     *        <tt>MBeanServer</tt>.
     *        <p>The <tt>CascadingAgent</tt> will call {@link 
     *        MBeanServerConnectionFactory#getMBeanServerConnection() 
     *        sourceConnection.getMBeanServerConnection()} every time it 
     *        needs to access the subagent.
     *        <p>
     * @param sourcePattern An <tt>ObjectName</tt> pattern that must be 
     *        satisfied by the <tt>ObjectName</tt>s of the source MBeans. 
     *        <p>The sourcePattern is evaluated in the context of the source 
     *        <tt>MBeanServer</tt>. The source pattern is used to perform
     *        a partial mount of the source <tt>MBeanServer</tt> in the target
     *        <tt>MBeanServer</tt>. Only those MBeans that satisfy the pattern
     *        will be mounted. The source pattern is thus a filter
     *        element. A <tt>null</tt> sourcePattern is equivalent to
     *        the wildcard <tt>"*:*"</tt>.
     *        <p>
     * @param sourceQuery A <tt>QueryExp</tt> that must be satisfied by the 
     *        source MBeans. 
     *        <p>The sourceQuery is evaluated in the context of 
     *        the source <tt>MBeanServer</tt>. <b>Using this functionality
     *        is discouraged</b>. It is recommended to always pass a 
     *        <tt>null</tt> parameter. If however, you chose to pass a 
     *        non null <var>sourceQuery</var>, the given <tt>QueryExp</tt> 
     *        should not involve any properties or attributes that vary 
     *        over time.
     *        The evaluation of the <var>sourceQuery</var> against a given 
     *        MBean should be guaranteed to always consistently yield the 
     *        same result. Our implementation does not enforce this 
     *        constraint, but the behavior of the <tt>CascadingAgent</tt> in 
     *        the case where this constraint were not respected is 
     *        unspecified and could be unpredictable.
     *        <p>
     * @param description A human readable string describing this 
     *        <tt>CascadingAgent</tt>.
     *        <p>
     **/
    public ProxyCascadingAgent(MBeanServerConnectionFactory sourceConnection,
			       ObjectName  sourcePattern,
			       QueryExp    sourceQuery,
			       String      description) {
	this(sourceConnection,sourcePattern,sourceQuery,null,null,description);
    }

    // from CascadingAgentMBean
    //
    public synchronized void start() 
	throws IOException {
	try { start(true); } catch (InstanceAlreadyExistsException iae) {
	    // Not possible, unless there's a bug...
	    throw new UndeclaredThrowableException(iae);
	}
    }

    /**
     * Starts this cascading agent.
     * <p>
     * When this method successfully completes, the source MBeans from the 
     * source (cascaded) MBeanServer which satisfy the source 
     * <tt>ObjectName</tt> {@link #getPattern pattern} filter and the source 
     * <tt>QueryExp</tt> {@link #getQuery query} filter will have been 
     * mounted in the target (cascading) <tt>MBeanServer</tt> under the
     * specified {@link #getTargetPath targetPath}.
     * <br>
     * After a successful invocation of <tt>start()</tt>, the 
     * <tt>CascadingAgent</tt> becomes active 
     * (see {@link CascadingAgentMBean#isActive isActive()}).
     * </p>
     * <p>
     * <tt>CascadingAgents</tt> may be started and stopped multiple times,
     * long as their underlying {@link MBeanServerConnectionFactory} is
     * able to return valid <tt>MBeanServerConnections</tt>.
     * </p>
     * <p>If <var>conflictAllowed</var> is false, and a name conflict is 
     * detected, this method will throw an {@link 
     * InstanceAlreadyExistsException}.
     * Otherwise, conflicting names are simply skipped - no proxy is
     * created for the names in conflict.
     * Using a wildcard pattern/query and setting this parameter to false
     * with no <var>targetPath</var> will always result in throwing an
     * <tt>InstanceAlreadyExistsException</tt>.</p>
     *
     * <p>For each cascaded MBean found in the source <tt>MBeanServer</tt>, 
     * this method will register a proxy MBean as returned by 
     * {@link #createProxy} in the target <tt>MBeanServer</tt>. 
     * The <tt>ProxyCascadingAgent</tt> is 
     * responsible for the life cycle of these proxies. The application should
     * not attempt to unregister MBean proxies which are under the 
     * responsibility of a <tt>ProxyCascadingAgent</tt>. Our implementation
     * does not enforce this rule, but unpredictable behavior may occur
     * if it is broken - that is: deleted proxies may suddenly reappear at
     * unpredictable time, when the <tt>ProxyCascadingAgent</tt> attempts
     * to update its view of the source <tt>MBeanServer</tt>.</p>
     *
     * <p>If this method raises an exception, then no MBeans will have
     * been cascaded as a result of this invocation.</p>
     *
     * <p>Proxy MBeans will be later deleted by the 
     * <tt>ProxyCascadingAgent</tt> on one of these three condition:
     * <ul><li>The {@link #stop} method is called, in which case all proxies
     *         managed by this cascading agent are unregistered from the
     *         target <tt>MBeanServer</tt>.</li>
     *     <li>An external event - e.g reception of a 
     *         {@link JMXConnectionNotification#NOTIFS_LOST} Notification -
     *         makes the <tt>ProxyCascadingAgent</tt> update its view of
     *         source <tt>MBeanServer</tt>. As a result proxies for which 
     *         cascaded MBeans no longer exists in the source 
     *         <tt>MBeanServer</tt> will be
     *         removed from the target <tt>MBeanServer</tt>.</li>
     *     <li>The <tt>ProxyCascadingAgent</tt> receives a      
     *        {@link MBeanServerNotification#UNREGISTRATION_NOTIFICATION 
     *        MBeanServerNotification.UNREGISTRATION_NOTIFICATION} 
     *        for one of the cascaded MBean. In which case the proxy 
     *        corresponding to that source MBean will be removed from
     *        the target <tt>MBeanServer</tt>.</li>
     * </ul>
     *
     * @param conflictAllowed if <code>true</code> the cascading agent will
     *        ignore name conflicts. if <code>false</code>, the cascading 
     *        agent will throw an <tt>InstanceAlreadyExistsException</tt> if
     *        it detects a name conflict while starting. 
     *        After the <tt>CascadingAgent</tt> has started, name conflicts
     *        are always ignored: MBeans from the source <tt>MBeanServer</tt>
     *        whose name would cause a conflict in the target 
     *        <tt>MBeanServer</tt> are simply not cascaded.
     *
     * @exception IOException if the connection with the source 
     *            <tt>MBeanServer</tt> fails.
     * @exception IllegalStateException if this cascading agent is not 
     *            stopped, or if the target <tt>MBeanServer</tt> can't 
     *            be obtained (e.g. the <tt>CascadingAgent</tt> MBean was 
     *            not registered).
     *
     * @see CascadingAgentMBean#start
     * @see #mustCascade
     * @see #handleMBeanServerNotification
     * @see #preRegister
     **/
    public synchronized void start(boolean conflictAllowed)
	throws IOException, InstanceAlreadyExistsException {
	if (!state.equals(State.STOPPED))
	    throw new IllegalStateException("Can't start when state is: " + 
					    state);

	final MBeanServer mbs = getTargetMBeanServer();
	if (mbs == null) 
	  throw new IllegalStateException("Can't start with no MBeanServer");

	state = State.STARTING;
	if (logger.fineOn()) logger.fine("start", state);
	try {
	    getConnectionFactory().getMBeanServerConnection().
		addNotificationListener(MBSDelegateObjectName,
					mbsNotifHandler,null,null);
	    enableConnectionNotifications();
	} catch (IOException io) {
	    state = State.STOPPED;
	    unexpectedException("start",MBSDelegateObjectName,io);
	    throw io;
	} catch (Exception x) {
	    unexpectedException("start",MBSDelegateObjectName,x);
	    final IOException io = new IOException("failed to start: " + x);
	    Utils.initCause(io,x);
	    state = State.STOPPED;
	    throw io;
	} catch (Error e) {
	    state = State.STOPPED;
	    if (logger.finerOn()) 
		logger.finer("start", "failed to start: " + e);
	    throw e;
	}

	Throwable failure = null;

	try {
	    
	    Set mbeans = getConnectionFactory().getMBeanServerConnection().
		queryNames(getPattern(),getQuery());
		
	    final ObjectName[] names = new ObjectName[mbeans.size()];
	    int   count=0;
	    for (Iterator it = mbeans.iterator(); it.hasNext();) {
		final ObjectName sourceName = (ObjectName)it.next();
		final ObjectName targetName = getTargetName(sourceName);
		if (mbs.isRegistered(targetName) && !conflictAllowed) {
		    nameConflictDetected("start",targetName);
		    throw new InstanceAlreadyExistsException(
						   String.valueOf(targetName));
		} else names[count++]=sourceName;
	    }

	    for (int i=0;i<count;i++)
		showMBean("start",names[i]);

	} catch (Throwable t) {
	    failure = t;
	}

	// Handle failure
	//
	if (failure != null) try {
	    state = State.SHUTTING_DOWN;
	    if (logger.fineOn()) 
		logger.fine("start", "Failed to start: " + state);
	    cleanup(false);
	    throw failure;
	} catch (IOException x) {
	    throw x;
	} catch (InstanceAlreadyExistsException x) {
	    throw x;
	} catch (Error e) {
	    throw e;
	} catch (Throwable x) {
	    final IOException io = new IOException("Failed to start: " + x);
	    Utils.initCause(io,x);
	    throw io;
	} finally {
	    state = State.STOPPED;
	    if (logger.fineOn()) 
		logger.fine("start", "Not started: " + state);
	}

	// Everything OK.
	state = State.STARTED;
	if (logger.fineOn()) logger.fine("start", state);
    }


    /**
     * Stops this <tt>CascadingAgent</tt>.
     * Does nothing if the <tt>CascadingAgent</tt> is already stopped.
     * Deregisters from the target <tt>MBeanServer</tt> all proxies 
     * corresponding to source MBeans.
     * @exception IOException not thrown in this default implementation.
     * @exception IllegalStateException if the <tt>CascadingAgent</tt> is not
     *            started.
     **/
    public void stop() throws IOException {
	stop(false);
    }

    // from CascadingAgentMBean
    //
    public int getCascadedMBeanCount() {
	return getLinkedCount();    
    }

    // from CascadingAgentMBean
    //
    public Set getCascadedMBeans() {
	final Set result = new HashSet();
	try {
	    final Set sprutstc = 
		getConnectionFactory().getMBeanServerConnection().
		queryMBeans(getPattern(),getQuery());
	    for (Iterator it = sprutstc.iterator(); it.hasNext();) {
		final ObjectInstance moi = (ObjectInstance)it.next();
		if (isLinked(moi.getObjectName())) result.add(moi);
	    }
	} catch (Exception x) {
	    unexpectedException("getCascadedMBeans",getPattern(),x);
	}
	return result;
    }

    // from CascadingAgentMBean
    //
    public String getDescription() {
	return description;
    }

    // from CascadingAgentMBean
    //
    public synchronized boolean isActive() {
	return state.equals(State.STARTED);
    }

    /**
     * Update the proxies managed by this <tt>CascadingAgent</tt> by comparing
     * the list of currently linked proxies in the target <tt>MBeanServer</tt>
     * with the list of source MBeans in the source <tt>MBeanServer</tt>. 
     * This makes it possible to recover from a
     * potential <tt>MBeanServerNotification</tt> loss.
     * @exception IOException if the connection with the cascaded 
     *            <tt>MBeanServer</tt> fails.
     **/
    public synchronized void update() throws IOException {
	if (!state.equals(State.STARTED)) {
	    if (logger.finerOn()) 
		logger.finer("update","CascadingAgent " + state);
	    return;
	}
	if (logger.finerOn()) 
	    logger.finer("update","CascadingAgent " + state);
	final ObjectName[] names = getLinkedSourceNames();
	final Set sprutstc = new HashSet( 
	    getConnectionFactory().getMBeanServerConnection().
	    queryNames(getPattern(),getQuery()));

	final int len = names.length;
	final MBeanServer mbs = getTargetMBeanServer();
	if (mbs == null) return;
	for (int i=0;i<len;i++) {
	    final ObjectName targetName = getTargetName(names[i]);
	    if (sprutstc.remove(names[i])) {
		// Name found in cascaded MBS. show it.
		showMBean("update",names[i]);
	    } else {
		// Name not found! hide it. 
		hideMBean("update",names[i]);
	    }
	}
	for (Iterator it = sprutstc.iterator(); it.hasNext(); ) {
	    // show remaining mbeans.
	    showMBean("update",(ObjectName)it.next());
	}
	if (logger.finerOn()) 
	    logger.finer("update","CascadingAgent updated");
    }

    /**
     * Allows the MBean to perform any operations it needs before
     * being unregistered by the MBean server.
     * This implementation throws an <tt>IllegalStateException</tt> if 
     * the <tt>ProxyCascadingAgent</tt> is active.
     *
     * @exception IllegalStateException if the <tt>ProxyCascadingAgent</tt>
     *            is not stopped.
     * @see CascadingAgent#preDeregister
     */
    public synchronized void preDeregister() throws java.lang.Exception {
	if (!state.equals(State.STOPPED)) {
	    throw new IllegalStateException("ProxyCascadingAgent "+
					    "is still active.");
	}
    }


    /**
     * Creates a new proxy for the specified source MBean.
     * This default implementation returns a new instance of 
     * {@link CascadingProxy}. Subclasses can redefine this method in order
     * to return other types of proxy. The returned object must simply be
     * a valid MBean that can be registered in the target 
     * <tt>MBeanServer</tt>.
     * @param  sourceName The name of the source MBean.
     * @param  cf The <tt>MBeanServerConnectionFactory</tt> used to obtain 
     *         connections with the source <tt>MBeanServer</tt>.
     * @return A new cascading proxy for the given source MBean.
     **/
    protected Object createProxy(ObjectName sourceName,
				 MBeanServerConnectionFactory cf) {
	return new CascadingProxy(sourceName,cf);
    }

    /**
     * This method is called internally when a
     * {@link JMXConnectionNotification} is received through the underlying
     * source {@link MBeanServerConnectionFactory}.
     * This method is called only after {@link #start()} was called.
     * This method is a callback that should never be called directly
     * by subclasses.
     * <p>It proceeds as follows:
     * <ul>{@link JMXConnectionNotification#OPENED} and 
     *     {@link JMXConnectionNotification#NOTIFS_LOST} trigger an
     *     {@link #update} operation.</ul>
     * <ul>{@link JMXConnectionNotification#CLOSED} makes the 
     *     <tt>CascadingAgent</tt> attempt to reopen the underlying 
     *     connection by calling <tt>getConnectionFactory().
     *     getMBeanServerConection().getDefaultDomain()</tt>. 
     *     <br>If this call fails, the <tt>CascadingAgent</tt> assumes that
     *     the underlying connection is broken and proceeds as if the
     *     {@link JMXConnectionNotification#FAILED} were received.</ul>
     * <ul>{@link JMXConnectionNotification#FAILED} makes the 
     *     <tt>CascadingAgent</tt> stop - proxies are removed and the 
     *     <tt>CascadingAgent</tt> becomes inactive.</ul>
     * </p>
     * In all cases, a similar notification is re-emitted to this 
     * object's listeners by calling {@link CascadingAgent#sendNotification}:
     * <ul><li>The <var>source</var> is changed to <var>this</var>,</li>
     *     <li>The <var>sequenceNumber</var> is reset to this object's
     *         sequenceNumber</var>,</li>
     *     <li>All other fields are imported from the original 
     *         notification.</li>  
     * </ul> 
     *
     **/
    protected void handleJMXConnectionNotification(Notification n,
						   Object handback) {
	final String nt = n.getType();
	try {
	    synchronized (this) {
		if (!state.equals(State.STARTED)) return;
		if (JMXConnectionNotification.OPENED.equals(nt) ||
		    JMXConnectionNotification.NOTIFS_LOST.equals(nt)) {
		    update();
		} else if (JMXConnectionNotification.CLOSED.equals(nt)) {
		    stopIfClosed();
		} else if (JMXConnectionNotification.FAILED.equals(nt)) {
		    stop(true);
		}
	    }
	} catch (Exception x) {
	    unexpectedException(nt,null,x);
	}
	final String connectionId = 
	    ((JMXConnectionNotification)n).getConnectionId();
	final JMXConnectionNotification newn = 
	    new JMXConnectionNotification(nt,this,connectionId,
					  newSequenceNumber(), n.getMessage(),
					  n.getUserData());
	sendNotification(newn);
    }


    /**
     * This method is called internally when a
     * {@link MBeanServerNotification} is received from the source (cascaded)
     * {@link MBeanServer}. 
     * This method is called only after {@link #start()} was called.
     * This method is a callback that should never be called directly
     * by subclasses.
     * @param notification The notification.
     * @param handback An opaque object which helps the listener to 
     *        associate information regarding the MBean emitter. 
     *        This object is passed to the MBean during the addListener 
     *        call and resent, without modification, to the listener. 
     *        In this implementation, the handback is <tt>null</tt>.
     * @see NotificationListener#handleNotification
     **/
    protected void handleMBeanServerNotification(Notification notification, 
					  Object handback) {
	if (notification instanceof MBeanServerNotification) {
	    final MBeanServerNotification n = 
		(MBeanServerNotification) notification;
	    final String nt = notification.getType();
	    final ObjectName sourceName = n.getMBeanName();
	    if (MBeanServerNotification.REGISTRATION_NOTIFICATION.
		equals(nt)) {
		if (mustCascade(sourceName)) { 
		    showMBean(nt,sourceName);
		}
	    } else if (MBeanServerNotification.UNREGISTRATION_NOTIFICATION.
		       equals(nt)) {
		if (isLinked(sourceName)) {
		    hideMBean(nt,sourceName);
		}
	    }
	}
    }

    /**
     * Returns true if the given source MBean name is the name of an MBean
     * that must be cascaded. 
     * <p>This method is called when a {@link 
     * MBeanServerNotification#REGISTRATION_NOTIFICATION} is received.
     * This default implementation checks that
     * the given source name satisfies the source <tt>ObjectName</tt> {@link 
     * CascadingAgent#getPattern() pattern} of this cascading agent, that 
     * the source MBean satisfies the {@link CascadingAgent#getQuery() 
     * query} filter of this cascading agent, and that this cascading 
     * agent has the permission to access the source MBean in the subagent.
     * @param sourceName The source MBean name.
     * @return true if the source MBean name is the name of an MBean that
     *         must be cascaded. 
     **/
    protected boolean mustCascade(ObjectName sourceName) {
	return isIncluded(sourceName,getPattern(),getQuery());
    }

    /**
     * Shows a source MBean by registering a <tt>CascadingProxy</tt> for 
     * that MBean in the target MBeanServer.
     * <p>This method does nothing if the <tt>CascadingAgent</tt> is not 
     * started, or not starting, or if a proxy is already registered 
     * for that source MBean.</p>
     *
     * <p>If no proxy is registered for that MBean, but a target MBean with 
     * the same name than the cascading proxy already exists in the target
     * <tt>MBeanServer</tt>, this method calls {@link #nameConflictDetected
     * nameConflictDetected(operation,targetName)} and returns.</p>
     *
     * <p>Otherwise, if an exception is raised while attempting to perform
     * this operation, this method calls {@link #unexpectedException
     * unexpectedException(operation,sourceName,exception)}.</p>
     *
     * @param operation The notification/method name from which this
     *                  operation was triggered. This could be one of:
     *        <ul>
     *        <li>{@link #start "start"}
     *        </li>
     *        <li>{@link #update "update"}
     *        </li>
     *        <li>or {@link MBeanServerNotification#REGISTRATION_NOTIFICATION}
     *        </li>
     *        </ul>
     * @param sourceName Name of the source MBean in the source MBeanServer.
     **/
    // * <p>This method calls {@link #getTargetName} in order to obtain
    // * the name with which the cascading proxy will be registered.</p>
    // *
    synchronized void showMBean(String operation,ObjectName sourceName) {
 	if (state.equals(State.STOPPED))      return;
	if (state.equals(State.SHUTTING_DOWN)) return;

	final MBeanServer srv = getTargetMBeanServer();
	if (srv == null) return;

	try {

	    final ObjectName targetName =  getTargetName(sourceName);

	    if (isLinked(sourceName) && srv.isRegistered(targetName))
		return ;
	    
	    final Object proxy = getProxy(sourceName,getConnectionFactory());
	
	    link(sourceName,proxy);
	    try {
		// Register the proxy locally
		srv.registerMBean(proxy,targetName);
		if (logger.finestOn()) 
		    logger.finest(operation,
				  "Registered proxy: " + targetName + 
				  " for: " + sourceName);
	    } catch (InstanceAlreadyExistsException x) {
		unlink(sourceName);
		nameConflictDetected(operation,targetName);
		return;
	    } catch (Exception x) {
		unlink(sourceName);
		// no good: log something.
		// possibly with a given Exception Handler?
		unexpectedException(operation,sourceName,x);
		return;
	    }
	} catch (Exception x) {
	    // no good: log something.
	    // possibly with a given Exception Handler?
	    unexpectedException(operation,sourceName,x);
	}
    }
    
    /**
     * Hides a source (cascaded) MBean by unregistering the
     * <tt>CascadingProxy</tt> for that MBean from the target MBeanServer. 
     * <p>This method does nothing if the <tt>CascadingAgent</tt> is not 
     * started, or not starting, or if no proxy is registered for that 
     * source MBean.</p>
     *
     * <p>If an exception is raised while attempting to perform
     * this operation, this method calls {@link #unexpectedException
     * unexpectedException(operation,sourceName,exception)}.
     *
     * @param operation The notification/method name from which this
     *                  operation was triggered. This could be one of:
     *        <ul>
     *        <li>{@link #update "update"}
     *        </li>
     *        <li>or {@link 
     *            MBeanServerNotification#UNREGISTRATION_NOTIFICATION}
     *        </li>
     *        </ul>
     * @param sourceName Name of the source MBean in the source MBeanServer.
     **/
    // * <p>This method calls {@link #getTargetName} in order to obtain
    // * the name with which the cascading proxy was registered.</p>
    // *
    synchronized void hideMBean(String operation,ObjectName sourceName) {
	final MBeanServer srv = getTargetMBeanServer();
	if (srv == null) return;
 	if (state.equals(State.STOPPED))      return;
	if (state.equals(State.SHUTTING_DOWN)) return;

	try {
	    if (isLinked(sourceName)) {
		final ObjectName targetName = getTargetName(sourceName);
		srv.unregisterMBean(targetName);
		if (logger.finestOn()) 
		    logger.finest(operation,
				  "Unregistered proxy: " + targetName + 
				  " for: " + sourceName);
		unlink(sourceName);
	    }
	} catch (InstanceNotFoundException x) {
	    // Already removed? that's strange, but hell, that's
	    // what we wanted anyway
	    // ==> should log something...
	    return;
	} catch (Exception x) {
	    // no good: log something.
	    // possibly with a given Exception Handler?
	    unexpectedException(operation,sourceName,x);
	    return;
	}
    }

    /**
     * Called when an unexpected exception is raised (<tt>IOException</tt> 
     * etc...) while accessing a cascaded MBean(s)
     * Subclasses should redefine this method if they need to implement
     * some special behavior (logging etc...).
     * @param operation The name of the operation from which this method
     *        was called (e.g. "start", "mustCascade", "getCascadedMBeans",
     *        "update", any <tt>JMXConnectionNotification</tt> 
     *        type, any <tt>MBeanServerNotification</tt> type, etc...)
     * @param name The <tt>ObjectName</tt> of the MBean for which the 
     *        exception was raised.
     *        If no particular MBean was involved in the operation that 
     *        caused the exception to be raised, then this parameter can
     *        be null.
     * @param x The exception.
     **/
    void unexpectedException(String operation, ObjectName name,
			     Exception x) {
	logger.fine(operation,"Unexpected exception while handling " + name +
		     ": " + x);
	logger.finest(operation,x);
    }

    /**
     * Called when an unexpected exception is raised (<tt>IOException</tt> 
     * etc...) while performing stopping the <tt>CascadingAgent</tt>
     * (<tt>stop()</tt> called, <tt>JMXConnectionNotification.FAILED</tt>
     * received).
     * Subclasses should redefine this method if they need to implement
     * some special behavior (logging etc...).
     * @param targetName The <tt>ObjectName</tt> of the cascading proxy for
     *        which the exception was raised. This name is interpreted
     *        in the context of the target MBeanServer.
     *        If no particular MBean was involved in the operation that 
     *        caused the exception to be raised, then this parameter can
     *        be null.
     * @param x The exception.
     **/
    void unexpectedCleanupException(ObjectName targetName,
				    Exception x) {
	logger.finer("cleanup","Unexpected exception while handling " + 
		     targetName + ": " + x);
	logger.finest("cleanup",x);
    }

    /**
     * Called when a name conflict is detected while trying to register
     * a cascading proxy.
     * Subclasses should redefine this method if they need to implement
     * some special behavior (logging etc...).
     * @param operation The name of the operation from which this method
     *        was called ("start", "update" or 
     *        {@link MBeanServerNotification#UNREGISTRATION_NOTIFICATION 
     *        MBeanServerNotification.UNREGISTRATION_NOTIFICATION})
     * @param targetName The <tt>ObjectName</tt> of the cascading proxy that 
     *        couldn't be created.
     **/
    void nameConflictDetected(String operation, ObjectName targetName) {
	logger.fine(operation,"Name conflict detected for " + targetName);
    }

    /**
     * Increments and returns this object's notification sequence number.
     **/
    protected final synchronized long newSequenceNumber() {
	return sequenceNumber++;
    }

    /**
     * Returns the <tt>ObjectName</tt> of the cascading proxy proxying the
     * source MBean identified by the given sourceName.
     * @param  sourceName The source MBean name.
     * @return The target MBean name.
     **/
    ObjectName getTargetName(ObjectName sourceName) {
	final String path = getTargetPath();
	if (path == null || path.length() == 0) 
	    return sourceName;
	try {
	    final String domain  = sourceName.getDomain();
	    final String list    = sourceName.getKeyPropertyListString();
	    final String targetName = path + "/" + domain + ":" +
		list;
	    return ObjectName.getInstance(targetName);
	} catch (MalformedObjectNameException x) {
	    // FIXME: must log something.
	    return sourceName;
	}
    }

    /**
     * Gets a target proxy for the specified source MBean.
     * If the given sourceName is already linked, simply return the proxy
     * returned by {@link #getLinked}. Otherwise, calls {@link #createProxy}
     * to create a new <tt>CascadingProxy</tt>.
     * @param  sourceName The name of the source MBean.
     * @param  cf The source <tt>MBeanServerConnectionFactory</tt> used to
     *         obtain connections with the source <tt>MBeanServer</tt>.
     * @return A cascading proxy for the given source name.
     **/
    private Object getProxy(ObjectName sourceName,
		    MBeanServerConnectionFactory cf) {
	final Object proxy = getLinked(sourceName);
	if (proxy != null) return proxy;
	if (logger.finestOn()) 
	    logger.finest("getProxy","create proxy for: " + sourceName);
	return createProxy(sourceName,cf);
    }

    // This is a hack: this method is called when 
    // JMXConnectionNotification.CLOSED is received. 
    // It calls getDefaultDomain() in order to check whether the connection
    // is permanently closed - if so, it calls stop(true);
    //
    private void stopIfClosed() throws IOException {
	if (!state.equals(State.STARTED)) return; 
	try {
	    getConnectionFactory().getMBeanServerConnection().
		getDefaultDomain();
	    return ;
	} catch (IOException x) {
	    // OK: really failed...
	} catch (Exception x) {
	    // OK: Handle this as failed...
	} 
	stop(true);
    }

    // If connectionDown is true - don't attempt to unregister listener
    // from remote MBeanServerDelegate: it would fail anyway.
    //
    private synchronized void stop(boolean connectionDown) 
	throws IOException {
	if (state.equals(State.STOPPED)) {
	    if (logger.fineOn()) logger.fine("stop","Already " + state);
	    return;
	}
	if (!state.equals(State.STARTED)) 
	    throw new IllegalStateException("Can't stop when state is: " + 
					    state);
	state = State.SHUTTING_DOWN;
	if (logger.fineOn()) logger.fine("stop",state);
	try {
	    cleanup(connectionDown);
	} finally {
	    state = State.STOPPED;
	    if (logger.fineOn()) logger.fine("stop",state);
	}
    }

    // If connectionDown is true - don't attempt to unregister listener
    // from remote MBeanServerDelegate: it would fail anyway.
    //
    private synchronized void cleanup(boolean connectionDown) {
	try {
	    try {
		if (! connectionDown)
		    getConnectionFactory().getMBeanServerConnection().
			removeNotificationListener(MBSDelegateObjectName,
						   mbsNotifHandler,null,null);
	    } catch (Exception x) {
		unexpectedCleanupException(MBSDelegateObjectName,x);
	    }
	    try {
		disableConnectionNotifications();
	    } catch (Exception x) {
		unexpectedCleanupException(null,x);
	    }
	    clearProxies();
	} catch (Exception x) {
	    unexpectedCleanupException(null,x);
	}
    }

    private synchronized void clearProxies() {
	try {
	    ObjectName[] names = clearLinks();
	    final MBeanServer mbs = getTargetMBeanServer();
	    if (mbs == null) return;
	    for (int i=0;i<names.length;i++) {
		try {
		    mbs.unregisterMBean(names[i]);
		    if (logger.finestOn()) 
			logger.finest("clearProxies",
				      "Unregistered target proxy: "+names[i]);
		} catch (Exception x) {
		    unexpectedCleanupException(names[i],x);
		}
	    }
	} catch (Exception x) {
	    unexpectedCleanupException(null,x);
	}
    }

    /**
     * Records the link being made between a source MBean name and a
     * Cascading Proxy.
     * This method is called internally and is provided as a hook for 
     * subclasses. You should never call this method directly.
     * @param sourceName  The name of the source MBean.
     * @param targetProxy The cascading proxy that will be registered for that
     *        source MBean in the target MBeanServer.
     **/
    private synchronized void link(ObjectName sourceName,Object targetProxy) {
	mbeanList.put(sourceName,targetProxy);
    }
    
    /**
     * Deletes the link between a source MBean name and a target
     * Cascading Proxy.
     * This method is called internally and is provided as a hook for 
     * subclasses. You should never call this method directly.
     * @param sourceName  The name of the source MBean.
     **/
    private synchronized void unlink(ObjectName sourceName) {
	mbeanList.remove(sourceName);
    }

    /**
     * Clears the list of proxies linked by that <tt>CascadingAgent</tt>.
     * Returns an array of cascading proxy <tt>ObjectName</tt>s that must be
     * deregistered from the cascading server.
     * This method is called internally and is provided as a hook for 
     * subclasses. You should never call this method directly.
     * @return The array of target proxy name that must be deleted from
     *         target MBeanServer.
     **/
    private synchronized ObjectName[] clearLinks() {
	final ObjectName[] keys = getLinkedSourceNames();
	for (int i=0;i<keys.length;i++) {
	    keys[i]=getTargetName(keys[i]);
	}
	mbeanList.clear();
	return keys;
    }

    /**
     * Tell whether the given source MBean is currently linked by
     * that cascading agent.
     * @param sourceName  The name of the source MBean.
     * @return true if that name is linked ({@link #link} was called.)
     **/
    private synchronized boolean isLinked(ObjectName sourceName) {
	return (mbeanList.get(sourceName) != null);
    }

    /**
     * Returns the target cascading proxy that is currently linked to the given
     * source name. Returns null if no target proxy is linked to that source
     * name.
     * @return The linked target proxy, if any.
     **/
    private synchronized Object getLinked(ObjectName sourceName) {
	return mbeanList.get(sourceName);
    }

    /**
     * Returns the number of source MBeans which are currently linked by this
     * <tt>CascadingAgent</tt>.
     * @return The number of source MBeans which are currently mounted in
     *         the target MBeanServer.
     **/
    private synchronized int getLinkedCount() {
	return mbeanList.size();
    }

    /**
     * Returns the <tt>ObjectName</tt>s of the source MBeans which are 
     * currently linked by this <tt>CascadingAgent</tt>.
     * @return The source MBeans names.
     **/
    private synchronized ObjectName[] getLinkedSourceNames() {
	final ObjectName[] keys = new ObjectName[mbeanList.size()];
	mbeanList.keySet().toArray(keys);
	return keys;	
    }

    /**
     * Apply the given <var>sourcePattern</var> and <var>sourceQuery</var> 
     * to the source MBean identified by  <var>sourceName</var>.
     * @return true if the source MBean matches the sourcePattern and 
     *         sourceQuery, false if it doesn't - or if the sourceQuery 
     *         couldn't be applied (<tt>IOException</tt> raised etc...).
     **/
    private boolean isIncluded(ObjectName sourceName, ObjectName sourcePattern,
		       QueryExp sourceQuery) {
	// match the sourcePattern
	//
	try {
	    if (sourcePattern != null && !(sourcePattern.apply(sourceName))) 
		return false;
 
	    // We can't simly do: if (sourceQuery == null) return true;
	    // we must verify that this CascadingAgent has the permissions
	    // to access the proxied MBean...
	    //
	    return (wrapper.queryNames(sourceName,sourceQuery).size() == 1);
	} catch (Exception x) {
	    unexpectedException("mustCascade",sourceName,x);
	}
	return false;
    }

    private final NotificationListener mbsNotifHandler;
    private final HashMap mbeanList;
    private final MBeanServerConnectionWrapper wrapper;
    private final String description;
    private String state;
    private long sequenceNumber;

    private final static ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_CASCADING,"ProxyCascadingAgent");
}
