/*
 * @(#)file      CascadingServiceMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.5
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

import javax.management.remote.JMXServiceURL;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectionNotification;
import javax.management.remote.JMXConnectorFactory;
import javax.management.*;
import java.util.Map;
import java.io.IOException;

/**
 * The <tt>CascadingServiceMBean</tt> is a high level service MBean that
 * makes it possible to remotely configure <tt>CascadingAgents</tt>.
 * This MBean makes it possible to <i>mount</i> a partial view of 
 * a <i>source MBeanServer</i> known by its <tt>JMXServiceURL</tt> into
 * the <i>target MBeanServer</i> of this <tt>CascadingServiceMBean</tt>.
 * <p>The target <tt>MBeanServer</tt> of a <tt>CascadingServiceMBean</tt> is
 * usually the <tt>MBeanServer</tt> in which that 
 * <tt>CascadingServiceMBean</tt> is registered.
 * It is recommended to instantiate at most one 
 * <tt>CascadingServiceMBean</tt> per target MBeanServer. If no 
 * <tt>ObjectName</tt> is specified when registering the 
 * <tt>CascadingServiceMBean</tt>, then
 * the <tt>CascadingServiceMBean</tt> will supply its own default name:
 * <tt>{@link #CASCADING_SERVICE_DEFAULT_NAME 
 * CASCADING_SERVICE_DEFAULT_NAME}=new
 * ObjectName("com.sun.jdmk:type=CascadingService")</tt>.
 * </p>
 * <p>
 * If the JMX Connection with a source MBeanServer fails, the 
 * <tt>CascadingServiceMBean</tt> will emit a 
 * {@link JMXConnectionNotification} with the following parameters: 
 * <ul>
 * <li>The <var>type</var> is <tt>{@link #CASCADING_FAILED_NOTIFICATION 
 *     "com.sun.jdmk.remote.cascading.failed"}</tt></li>
 * <li>The <var>source</var> is the <tt>CascadingServiceMBean</tt>.</tt></li>
 * <li>The <var>connectionId</var> is the <var>mountPointID</var> 
 *     that was returned by the the {@link #mount mount} method</li>
 * <li>The <var>userData</var> is the source 
 *     <tt>JMXConnectionNotification</tt></li>
 * </ul>
 * </p>
 * <p>
 * If the cascading is unmounted ({@link #unmount unmount} was called) then
 * the <tt>CascadingServiceMBean</tt> will emit a 
 * {@link JMXConnectionNotification} with the following parameters: 
 * <ul>
 * <li>The <var>type</var> is <tt>{@link #CASCADING_FAILED_NOTIFICATION 
 *     "com.sun.jdmk.remote.cascading.stopped"}</tt></li>
 * <li>The <var>source</var> is the <tt>CascadingServiceMBean</tt>.</tt></li>
 * <li>The <var>connectionId</var> is the <var>mountPointID</var> 
 *     that was returned by the the {@link #mount mount} method</li>
 * </ul>
 * </p>
 * <p><b>Note:</b> In this implementation, the {@link #mount mount} 
 *    operation does not perform any checks with regards to the coherency
 *    of the supplied <var>targetPath</var>. It is under the responsibility of
 *    the application to conform to the rules documented in {@link
 *    com.sun.jdmk.remote.cascading#The_File_System_Analogy 
 *    com.sun.jdmk.remote.cascading} package documentation, section 
 *    "The File System Analogy".
 * </p>
 *
 * @since Java DMK 5.1
 **/
public interface CascadingServiceMBean {

    /**
     * Default <tt>ObjectName</tt> of the <tt>CascadingServiceMBean</tt>
     * MBean. 
     * <pre>
     * CASCADING_SERVICE_DEFAULT_NAME = 
     *    new ObjectName("com.sun.jdmk:type=CascadingService");
     * </pre>
     **/
    public final static ObjectName CASCADING_SERVICE_DEFAULT_NAME = 
	new Object() {
		public final ObjectName getDefaultName() {
		    try {
			return new 
			    ObjectName("com.sun.jdmk:type=CascadingService");
		    } catch (MalformedObjectNameException x) {
			// should never happen...
			throw new Error(x);
		    }
		}
	    }.getDefaultName();

    /**
     * The type of the <tt>JMXConnectionNotification</tt> emitted when a 
     * {@link JMXConnectionNotification#FAILED 
     *  JMXConnectionNotification.FAILED} is emitted by the underlying
     * JMX Remote Connection.
     **/
    public final String CASCADING_FAILED_NOTIFICATION = 
	"com.sun.jdmk.remote.cascading.failed";

    /**
     * The type of the <tt>JMXConnectionNotification</tt> emitted when a
     * source <tt>MBeanServer</tt> is unmounted.
     * @see #unmount
     **/
    public final String CASCADING_STOPPED_NOTIFICATION = 
	"com.sun.jdmk.remote.cascading.stopped";

    /**
     * Mounts a partial view of the source <tt>MBeanServer</tt> identified 
     * by its <tt>JMXServiceURL</tt>.
     *
     * This method obtains a {@link JMXConnector} by calling 
     * <tt>JMXConnectorFactory.connect(sourceURL,sourceMap)</tt>.
     * Then it mounts the source <tt>MBeanServer</tt> thus connected under the 
     * provided <var>targetPath</var> into the target <tt>MBeanServer</tt> 
     * of this <tt>CascadingServiceMBean</tt>.
     * Note that only the source MBeans whose source <tt>ObjectName</tt> 
     * satisfy the provided <var>sourcePattern</var> will be mounted.
     * 
     * <p>Finally, it returns a <var>mountPointID</var> string identifying
     * this mount operation. The calling application is expected to later
     * call {@link #unmount unmount} with this  <var>mountPointID</var> 
     * as parameter.
     * </p>
     * 
     * @param sourceURL A <tt>JMXServiceURL</tt> from which a 
     *        <tt>JMXConnector</tt> to the source <tt>MBeanServer</tt> can 
     *        be obtained.
     *        <p>
     * @param sourceMap A Map object that will be passed to the 
     *        {@link JMXConnectorFactory#connect(JMXServiceURL,Map)}
     *        method, in order to connect to the source <tt>MBeanServer</tt>.
     *        This parameter can be null.
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
     * @param targetPath The <i>domain path</i> under which the source
     *        MBeans will be mounted in the target <tt>MBeanServer</tt>.
     *        <p>If this parameter is not <tt>null</tt>, all source MBean names
     *        will be transformed in the target <tt>MBeanServer</tt> by 
     *        prefixing their domain name with the string 
     *        <tt><i>targetPath</i>+"/"</tt>. An MBean whose name is
     *        <tt>"D:k1=v1,k2=v2"</tt> will thus be mounted as 
     *        <tt>"<i>targetPath</i>/D:k1=v1,k2=v2"</tt>.
     *        <p>
     *        A <tt>null</tt> <var>targetPath</var> means that MBeans are
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
     *        However, this implementation does not enforce these rules: 
     *        It is the responsibility of the application that uses the
     *        <tt>CascadingService</tt> to ensure the consistency of
     *        the mounting strategy - see {@link 
     *        com.sun.jdmk.remote.cascading#The_File_System_Analogy 
     *        The File System Analogy}.
     *        <p>
     *        <b>Note:</b> A zero-length <var>targetPath</var> is treated 
     *        as a null <var>targetPath</var>.
     *        <p>
     * @return A <var>mountPointID</var> identifying this mount operation.
     *         This mountPointID must be later used to call {@link #unmount}. 
     * @exception NullPointerException if sourceURL is <tt>null</tt>. See 
     *            {@link JMXConnectorFactory#connect 
     *            JMXConnectorFactory.connect}.
     * @exception IOException if the connector client or the connection to
     *            the source MBeanServer cannot be made, or if the underlying
     *            <tt>CascadingAgent</tt> cannot be started because of a 
     *            communication problem. See {@link 
     *            JMXConnectorFactory#connect JMXConnectorFactory.connect}
     *            and {@link CascadingAgentMBean#start(boolean) 
     *            CascadingAgentMBean.start}.
     * @exception SecurityException if the connection with the source
     *            MBeanServer cannot be made for security reasons. See 
     *            {@link JMXConnectorFactory#connect 
     *            JMXConnectorFactory.connect}.
     * @exception InstanceAlreadyExistsException if a name conflict is
     *            detected while performing the mount operation.
     *            See {@link CascadingAgentMBean#start(boolean) 
     *            CascadingAgentMBean.start}.
     **/
    public String mount(JMXServiceURL sourceURL, Map sourceMap,
			ObjectName sourcePattern,
			String targetPath) 
	throws IOException, InstanceAlreadyExistsException;


    /**
     * Undo the mount operation identified by <var>mountPointID</var>.
     * The specified <var>mountPointID</var> must be a mount point ID 
     * obtained from {@link #mount mount}. 
     * @param mountPointID A mount point ID previously obtained from
     *        {@link #mount mount}.
     * @return true if the given <var>mountPointID</var> was found and
     *         unmounted by this invocation. 
     * @exception IOException if the connection with the source
     *         <tt>MBeanServer</tt> cannot be closed cleanly, or if 
     *         the underlying <tt>CascadingAgent</tt> fails to stop.
     *         See {@link JMXConnector#close JMXConnector.close} and {@link 
     *         CascadingAgentMBean#stop CascadingAgentMBean.stop}.
     **/
    public boolean unmount(String mountPointID) throws IOException;

    /**
     * Tell whether the given ID identifies a currently mounted mountPoint.
     * @return true if the given <var>mountPointID</var> was found.
     **/
    public boolean isMounted(String mountPointID);


    /**
     * Returns an array of current <var>mountPointIDs</var>.
     * @return an array of <var>mountPointID</var>s currently mounted. 
     **/
    public String[] getMountPointIDs();
}
