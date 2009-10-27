/*
 * @(#)file      CascadingAgentMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.13
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
package com.sun.jdmk.remote.cascading;

// java import
import java.util.Set;
import java.io.IOException;

// jdmk import
import javax.management.QueryExp;
import javax.management.ObjectName;
import javax.management.ObjectInstance;
import javax.management.InstanceAlreadyExistsException;

/**
 * Describes the management interface of the cascading agent MBean.
 * <p>
 * A <tt>CascadingAgent</tt> is an MBean that is able to <i>mount</i> a partial
 * view of a <i>source MBeanServer</i> into a <i>target MBeanServer</i>.
 * The source <tt>MBeanServer</tt> is also sometimes called the 
 * <i>cascaded MBeanServer</i>, while the target <tt>MBeanServer</tt> is 
 * called the <i>cascading MBeanServer</i>.
 * <p>
 * See {@link CascadingAgent} for more details.
 *
 * @since Java DMK 5.1
 */
public interface CascadingAgentMBean  {

    /**
     * Returns the number of source MBeans cascaded by this
     * <CODE>CascadingAgent</CODE>. 
     * This is the number of source MBeans that have been mounted by
     * this cascading agent in the target MBeanServer.
     * @return the number of source MBeans cascaded by this
     * <CODE>CascadingAgent</CODE>.
     */
    public int getCascadedMBeanCount();

    /**
     * Returns a Set of {@link ObjectInstance} representing the source MBeans 
     * cascaded by this <CODE>CascadingAgent</CODE>. 
     * The <tt>ObjectInstance</tt> objects returned are to be interpreted
     * in the context of the source MBeanServer: the <tt>ObjectNames</tt> 
     * correspond to the <tt>ObjectNames</tt> of the source MBeans in the 
     * source MBeanServer.
     *
     * @return a {@link Set} containing all 
     *  {@link ObjectInstance ObjectInstances} representing the cascaded
     *  source MBeans.
     */
    public Set getCascadedMBeans();

    /**
     * Returns the source {@link ObjectName} pattern filter that the 
     * source MBean names must satisfy in order to be cascaded.
     * This pattern is to be evaluated in the context of the source
     * MBeanServer.
     * @return the source <tt>ObjectName</tt> pattern filter.
     */
    public ObjectName getPattern();

    /**
     * Returns the source {@link QueryExp} query filter that the 
     * source MBean names must satisfy in order to be cascaded.
     * This query is to be evaluated in the context of the source
     * MBeanServer.
     * @return the source <tt>QueryExp</tt> query filter.
     */
    public QueryExp getQuery();

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
     * <p>If this method raises an exception, then no MBeans will have
     * been cascaded as a result of this invocation.</p>
     *
     * @param conflictAllowed if <code>true</code> the cascading agent will
     *        ignore name conflicts. if <code>false</code>, the cascading 
     *        agent will throw an <tt>InstanceAlreadyExistsException</tt> if
     *        it detects a name conflict while starting. 
     *        After the <tt>CascadingAgent</tt> has started, name conflicts
     *        are always ignored: MBeans from the source MBeanServer whose
     *        name would cause a conflict in the target MBeanServer are
     *        simply not cascaded.
     *
     * @exception IOException if the connection with the source 
     *            <tt>MBeanServer</tt> fails.
     * @exception IllegalStateException if this cascading agent is not 
     *            stopped, or if the target <tt>MBeanServer</tt> can't 
     *            be obtained (e.g. the <tt>CascadingAgent</tt> MBean was 
     *            not registered).
     * @exception InstanceAlreadyExistsException if a name conflict is
     *            detected while starting.
     *
     * @see CascadingAgentMBean#start
     **/
    public void start(boolean conflictAllowed) 
	throws IOException, InstanceAlreadyExistsException;

    /**
     * Starts the cascading. 
     * <p>This is equivalent to calling <tt>start(true)</tt></p>
     * <p>
     * When this method successfully completes, the source MBeans from the 
     * source (cascaded) <tt>MBeanServer</tt> which satisfy the source 
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
     * <p>If this method raises an exception, then no MBeans will have
     * been cascaded as a result of this invocation.</p>
     * 
     * @exception IOException if cascading couldn't be established.
     * @exception IllegalStateException if this cascading agent is not in
     *            a state where it can be started (for instance, a stop
     *            operation is still in progress). The exact cases
     *            where <tt>IllegalStateException</tt> can be thrown 
     *            is implementation dependent.
     * @see #stop
     * @see #isActive
     */
    public  void start() throws IOException;
  
    /**
     * Stops the cascading.
     * <p>
     * When this method completes, the MBeans that were cascaded by this
     * <tt>CascadingAgent</tt> will no longer be mounted in the cascading
     * <tt>MBeanServer</tt>. 
     * After a successful invocation of <tt>stop()</tt>, the 
     * <tt>CascadingAgent</tt> becomes inactive 
     * (see {@link #isActive isActive()}).
     * </p>
     * @exception IOException if cascading couldn't be stopped.
     * @exception IllegalStateException if this cascading agent is not in
     *            a state where it can be stopped (for instance, a start
     *            operation is still in progress). The exact cases
     *            where <tt>IllegalStateException</tt> can be thrown 
     *            is implementation dependent.
     * @see #start
     * @see #isActive
     */
    public  void stop() throws IOException;

    /**
     * Tests if the <CODE>CascadingAgent</CODE> is active.
     * @return <code>true</code> if the cascading agent is active.
     */
    public boolean isActive();
  
    /**
     * A human readable string describing this cascading agent.
     * <p>Whenever possible, this description string should identify
     * the source <tt>MBeanServer</tt> which is cascaded by this 
     * <tt>CascadingAgent</tt>, 
     * and the semantics of this cascading agent. <br>
     * For instance, if the cascaded agent has a human readable 
     * <tt>JMXServiceURL</tt> (i.e. not the form containing the 
     * encoded stub), then the source agent could be identified by that 
     * URL, and that URL could be used in this description string.<br>
     * Alternatively, if the cascaded agent connector was retrieved from
     * a naming service, then the JMX Agent Name of the cascaded agent
     * could be used to identify it.</p>
     * <p>A valid description could be e.g:
     * <ul> 
     * <tt>"mount service:jmx:jmxmp://localhost:9876 java.lang:* null 
     *        server1/instance1"</tt>
     * </ul>
     * @return A human readable string describing this cascading agent.
     **/
    public String getDescription(); 
  
    /**
     * Gets the targetPath. 
     * This is the <i>domain path</i> under which the source
     * MBeans will be mounted in the target <tt>MBeanServer</tt>.
     * If this attribute is not <tt>null</tt>, all source MBean names
     * will be transformed in the target <tt>MBeanServer</tt> by prefixing
     * their domain name with the string 
     * <tt><i>targetPath</i>+"/"</tt>. 
     * <br>
     * An MBean whose name is
     *      <tt>"D:k1=v1,k2=v2"</tt> will thus be mounted as 
     *      <tt>"<i>targetPath</i>/D:k1=v1,k2=v2"</tt>.
     * <br>
     * A <tt>null</tt> <var>targetPath</var> means that MBeans are
     * mounted directly at the root of the hierarchy, that is, as if
     * they were local MBeans. <b>Using a null <i>targetPath</i> is
     * thus highly discouraged, as it could cause name conflicts
     * in the target <tt>MBeanServer</tt></b>. 
     * <br>
     * Similarly, MBeans from different sources should not be
     * mounted under the same <var>targetPath</var>. Moreover,
     * an application should not attempt to mount source MBeans under
     * a <var>targetPath</var> that already contain MBeans in the
     * target <tt>MBeanServer</tt>.
     * @return The value of the <i>targetPath</i> under which source MBeans
     *         are mounted in the target <tt>MBeanServer</tt>.
     * @see com.sun.jdmk.remote.cascading
     **/
    public String getTargetPath();

}
