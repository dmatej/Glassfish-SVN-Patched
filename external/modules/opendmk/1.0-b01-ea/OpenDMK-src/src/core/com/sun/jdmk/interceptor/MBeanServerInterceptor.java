/* 
 * @(#)file      MBeanServerInterceptor.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.14 
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

package com.sun.jdmk.interceptor;

import java.util.Set;

// RI import
import javax.management.DynamicMBean;
import javax.management.AttributeNotFoundException;
import javax.management.MBeanException; 
import javax.management.ReflectionException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.QueryExp;
import javax.management.NotificationListener;
import javax.management.NotificationFilter;
import javax.management.ListenerNotFoundException;
import javax.management.IntrospectionException;
import javax.management.OperationsException;
import javax.management.MBeanNotificationInfo;
import javax.management.JMRuntimeException;
import javax.management.InstanceNotFoundException;
import javax.management.NotCompliantMBeanException;
import javax.management.MBeanRegistrationException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InvalidAttributeValueException;
import javax.management.ObjectName;
import javax.management.ObjectInstance;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.RuntimeOperationsException;
import javax.management.MBeanServerConnection; 
import javax.management.MBeanServerDelegate; 
import javax.management.loading.ClassLoaderRepository;

import com.sun.jdmk.MBeanInterceptor;
import com.sun.jdmk.JdmkMBeanServer;

/**
 * <p>This interface specifies the behavior to be implemented by an
 * MBean Server Interceptor.  An MBean Server Interceptor has
 * essentially the same interface as an MBean Server.  An MBean Server
 * forwards received requests to its default interceptor, which may
 * handle them itself or forward them to other interceptors.  The
 * default interceptor may be changed via the {@link
 * JdmkMBeanServer#setMBeanServerInterceptor} method.</p>
 *
 * <p>The initial default interceptor provides the standard MBean
 * Server behavior.  It handles a collection of named MBeans, each
 * represented by a Java object.  A replacement default interceptor
 * may build on this behavior, for instance by adding logging or
 * security checks, before forwarding requests to the initial default
 * interceptor.  Or, it may route each request to one of a number of
 * sub-interceptors, for instance based on the {@link ObjectName} in
 * the request.</p>
 *
 * <p>An interceptor, default or not, need not implement MBeans as
 * Java objects, in the way that the initial default interceptor does.
 * It may instead implement <em>virtual MBeans</em>, which do not
 * exist as Java objects when they are not in use.  For example, these
 * MBeans could be implemented by forwarding requests to a database,
 * or to a remote MBean server, or by performing system calls to query
 * or modify system resources.</p>
 *
 * <p><b>Note:</b> This interface extends the MBeanInterceptor
 * interface for backward compatibility with the Java DMK 5.0.
 * In next releases, the methods defined in 
 * {@link com.sun.jdmk.MBeanInterceptor} may be moved to this interface
 * and the inheritance link broken.
 *
 * @since Java DMK 5.1
 */

public interface MBeanServerInterceptor extends MBeanInterceptor {
    /**
     * <p>Removes a listener from a registered MBean.</p>
     *
     * <p>The MBean must have a listener that exactly matches the
     * given <code>listener</code>, <code>filter</code>, and
     * <code>handback</code> parameters.  If there is more than one
     * such listener, only one is removed.</p>
     *
     * <p>The <code>filter</code> and <code>handback</code> parameters
     * may be null if and only if they are null in a listener to be
     * removed.</p>
     *
     * @param name The name of the MBean on which the listener should
     * be removed.
     * @param listener A listener that was previously added to this
     * MBean.
     * @param filter The filter that was specified when the listener
     * was added.
     * @param handback The handback that was specified when the
     * listener was added.
     *
     * @exception InstanceNotFoundException The MBean name provided
     * does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not
     * registered in the MBean, or it is not registered with the given
     * filter and handback.
     */
    public void removeNotificationListener(ObjectName name,
					   ObjectName listener,
					   NotificationFilter filter,
					   Object handback)
	    throws InstanceNotFoundException, ListenerNotFoundException;


    /**
     * <p>Removes a listener from a registered MBean.</p>
     *
     * <p>The MBean must have a listener that exactly matches the
     * given <code>listener</code>, <code>filter</code>, and
     * <code>handback</code> parameters.  If there is more than one
     * such listener, only one is removed.</p>
     *
     * <p>The <code>filter</code> and <code>handback</code> parameters
     * may be null if and only if they are null in a listener to be
     * removed.</p>
     *
     * @param name The name of the MBean on which the listener should
     * be removed.
     * @param listener A listener that was previously added to this
     * MBean.
     * @param filter The filter that was specified when the listener
     * was added.
     * @param handback The handback that was specified when the
     * listener was added.
     *
     * @exception InstanceNotFoundException The MBean name provided
     * does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not
     * registered in the MBean, or it is not registered with the given
     * filter and handback.
     */
    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener,
					   NotificationFilter filter,
					   Object handback)
	    throws InstanceNotFoundException, ListenerNotFoundException;

    /**
     * <p>Return the {@link java.lang.ClassLoader} that was used for
     * loading the class of the named MBean.
     * @param mbeanName The ObjectName of the MBean.
     * @return The ClassLoader used for that MBean.
     * @exception InstanceNotFoundException if the named MBean is not found.
     */
    public ClassLoader getClassLoaderFor(ObjectName mbeanName)
	throws InstanceNotFoundException;

    /**
     * <p>Return the named {@link java.lang.ClassLoader}.
     * @param loaderName The ObjectName of the ClassLoader.
     * @return The named ClassLoader.
     * @exception InstanceNotFoundException if the named ClassLoader is 
     *    not found.
     */
    public ClassLoader getClassLoader(ObjectName loaderName)
	throws InstanceNotFoundException;

    /**
     * Returns the list of domains in which any MBean is currently
     * registered.
     */
    public String[] getDomains();

    /**
     * <p>Return the {@link java.lang.ClassLoader} that was used for
     * loading the class of the named MBean.
     * @param mbeanName The ObjectName of the MBean.
     * @return The ClassLoader used for that MBean.
     * @exception InstanceNotFoundException if the named MBean is not found.
     * @deprecated Use      
     * {@link #getClassLoaderFor(javax.management.ObjectName) }
     * @since Java DMK 5.0 (deprecated in Java DMK 5.1)
     */
    public ClassLoader getMBeanClassLoader(ObjectName mbeanName) 
	throws InstanceNotFoundException;
}
