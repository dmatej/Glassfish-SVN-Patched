/*
 * @(#)file      MBeanInterceptor.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.19
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

package com.sun.jdmk;

import javax.management.*;
import java.util.Set;

/**
 * @deprecated Use {@link
 * com.sun.jdmk.interceptor.MBeanServerInterceptor} instead.
 *
 * @since Java DMK 5.0
 */
public interface MBeanInterceptor extends ProxyHandler {
    /**
     * Instantiates and registers an MBean in the MBean server.  The
     * MBean server will use its {@link
     * javax.management.loading.ClassLoaderRepository Default Loader
     * Repository} to load the class of the MBean.  An object name is
     * associated to the MBean.  If the object name given is null, the
     * MBean must provide its own name by implementing the {@link
     * javax.management.MBeanRegistration MBeanRegistration} interface
     * and returning the name from the {@link
     * MBeanRegistration#preRegister preRegister} method.
     *
     * @param className The class name of the MBean to be instantiated.
     * @param name The object name of the MBean. May be null.
     * @param params An array containing the parameters of the
     * constructor to be invoked.
     * @param signature An array containing the signature of the
     * constructor to be invoked.
     *
     * @return An <CODE>ObjectInstance</CODE>, containing the
     * <CODE>ObjectName</CODE> and the Java class name of the newly
     * instantiated MBean.
     *
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.ClassNotFoundException</CODE> or a
     * <CODE>java.lang.Exception</CODE> that occurred when trying to
     * invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already
     * under the control of the MBean server.
     * @exception MBeanRegistrationException The
     * <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The
     * MBean will not be registered.
     * @exception MBeanException The constructor of the MBean has
     * thrown an exception
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The className
     * passed in parameter is null, the <CODE>ObjectName</CODE> passed
     * in parameter contains a pattern or no <CODE>ObjectName</CODE>
     * is specified for the MBean.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      Object params[], String signature[])
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException;

    /**
     * Instantiates and registers an MBean in the MBean server.  The
     * class loader to be used is identified by its object name. An
     * object name is associated to the MBean. If the object name of
     * the loader is not specified, the ClassLoader that loaded the
     * MBean server will be used.  If the MBean object name given is
     * null, the MBean must provide its own name by implementing the
     * {@link javax.management.MBeanRegistration MBeanRegistration}
     * interface and returning the name from the {@link
     * MBeanRegistration#preRegister preRegister} method.
     *
     * @param className The class name of the MBean to be instantiated.
     * @param name The object name of the MBean. May be null.
     * @param params An array containing the parameters of the
     * constructor to be invoked.
     * @param signature An array containing the signature of the
     * constructor to be invoked.
     * @param loaderName The object name of the class loader to be used.
     *
     * @return An <CODE>ObjectInstance</CODE>, containing the
     * <CODE>ObjectName</CODE> and the Java class name of the newly
     * instantiated MBean.
     *
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.ClassNotFoundException</CODE> or a
     * <CODE>java.lang.Exception</CODE> that occurred when trying to
     * invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already
     * under the control of the MBean server.
     * @exception MBeanRegistrationException The
     * <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The
     * MBean will not be registered.
     * @exception MBeanException The constructor of the MBean has
     * thrown an exception
     * @exception InstanceNotFoundException The specified class loader
     * is not registered in the MBean server.
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The className
     * passed in parameter is null, the <CODE>ObjectName</CODE> passed
     * in parameter contains a pattern or no <CODE>ObjectName</CODE>
     * is specified for the MBean.
     *
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName, Object params[],
                                      String signature[])
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException, InstanceNotFoundException;

    /**
     * Registers a pre-existing object as an MBean with the MBean
     * server. If the object name given is null, the MBean must
     * provide its own name by implementing the {@link
     * javax.management.MBeanRegistration MBeanRegistration} interface
     * and returning the name from the {@link
     * MBeanRegistration#preRegister preRegister} method.
     *
     * @param object The  MBean to be registered as an MBean.
     * @param name The object name of the MBean. May be null.
     *
     * @return The <CODE>ObjectInstance</CODE> for the MBean that has
     * been registered.
     *
     * @exception InstanceAlreadyExistsException The MBean is already
     * under the control of the MBean server.
     * @exception MBeanRegistrationException The
     * <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The
     * MBean will not be registered.
     * @exception NotCompliantMBeanException This object is not a JMX
     * compliant MBean
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The object
     * passed in parameter is null or no object name is specified.
     */
    public ObjectInstance registerMBean(Object object, ObjectName name)
            throws InstanceAlreadyExistsException, MBeanRegistrationException,
                   NotCompliantMBeanException;

    /**
     * Unregisters an MBean from the MBean server. The MBean is
     * identified by its object name. Once the method has been
     * invoked, the MBean may no longer be accessed by its object
     * name.
     *
     * @param name The object name of the MBean to be unregistered.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     * @exception MBeanRegistrationException The preDeregister
     * ((<CODE>MBeanRegistration</CODE> interface) method of the MBean
     * has thrown an exception.
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The object
     * name in parameter is null or the MBean you are when trying to
     * unregister is the {@link javax.management.MBeanServerDelegate
     * MBeanServerDelegate} MBean.
     *
     */
    public void unregisterMBean(ObjectName name)
            throws InstanceNotFoundException, MBeanRegistrationException;

    /**
     * Gets the <CODE>ObjectInstance</CODE> for a given MBean
     * registered with the MBean server.
     *
     * @param name The object name of the MBean.
     *
     * @return The <CODE>ObjectInstance</CODE> associated to the MBean
     * specified by <VAR>name</VAR>.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     */
    public ObjectInstance getObjectInstance(ObjectName name)
            throws InstanceNotFoundException;

    /**
     * Gets MBeans controlled by the MBean server. This method allows
     * any of the following to be obtained: All MBeans, a set of
     * MBeans specified by pattern matching on the
     * <CODE>ObjectName</CODE> and/or a Query expression, a specific
     * MBean. When the object name is null or no domain and key
     * properties are specified, all objects are to be selected (and
     * filtered if a query is specified). It returns the set of
     * <CODE>ObjectInstance</CODE> objects (containing the
     * <CODE>ObjectName</CODE> and the Java Class name) for the
     * selected MBeans.
     *
     * @param name The object name pattern identifying the MBeans to
     * be retrieved. If null or no domain and key properties are
     * specified, all the MBeans registered will be retrieved.
     * @param query The query expression to be applied for selecting
     * MBeans. If null no query expression will be applied for
     * selecting MBeans.
     *
     * @return A set containing the <CODE>ObjectInstance</CODE>
     * objects for the selected MBeans.  If no MBean satisfies the
     * query an empty list is returned.
     */
    public Set queryMBeans(ObjectName name, QueryExp query);

    /**
     * Gets the names of MBeans controlled by the MBean server. This
     * method enables any of the following to be obtained: The names
     * of all MBeans, the names of a set of MBeans specified by
     * pattern matching on the <CODE>ObjectName</CODE> and/or a Query
     * expression, a specific MBean name (equivalent to testing
     * whether an MBean is registered). When the object name is null
     * or no domain and key properties are specified, all objects are
     * selected (and filtered if a query is specified). It returns the
     * set of ObjectNames for the MBeans selected.
     *
     * @param name The object name pattern identifying the MBean names
     * to be retrieved. If null or no domain and key properties are
     * specified, the name of all registered MBeans will be retrieved.
     * @param query The query expression to be applied for selecting
     * MBeans. If null no query expression will be applied for
     * selecting MBeans.
     *
     * @return A set containing the ObjectNames for the MBeans
     * selected.  If no MBean satisfies the query, an empty list is
     * returned.
     */
    public Set queryNames(ObjectName name, QueryExp query);

    /**
     * Checks whether an MBean, identified by its object name, is
     * already registered with the MBean server.
     *
     * @param name The object name of the MBean to be checked.
     *
     * @return True if the MBean is already registered in the MBean
     * server, false otherwise.
     *
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The object
     * name in parameter is null.
     */
    public boolean isRegistered(ObjectName name);

    /**
     * Returns the number of MBeans registered in the MBean server.
     */
    public Integer getMBeanCount();

    /**
     * Gets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The object name of the MBean from which the
     * attribute is to be retrieved.
     * @param attribute A String specifying the name of the attribute
     * to be retrieved.
     *
     * @return  The value of the retrieved attribute.
     *
     * @exception AttributeNotFoundException The attribute specified
     * is not accessible in the MBean.
     * @exception MBeanException Wraps an exception thrown by the
     * MBean's getter.
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.Exception</CODE> thrown when trying to invoke
     * the setter.
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The object
     * name in parameter is null or the attribute in parameter is
     * null.
     */
    public Object getAttribute(ObjectName name, String attribute)
            throws MBeanException, AttributeNotFoundException,
                   InstanceNotFoundException, ReflectionException;

    /**
     * Enables the values of several attributes of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The object name of the MBean from which the
     * attributes are retrieved.
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return The list of the retrieved attributes.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     * @exception ReflectionException An exception occurred when
     * trying to invoke the getAttributes method of a Dynamic MBean.
     * @exception RuntimeOperationsException Wrap a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The object
     * name in parameter is null or attributes in parameter is null.
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
            throws InstanceNotFoundException, ReflectionException;

    /**
     * Sets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The name of the MBean within which the attribute is
     * to be set.
     * @param attribute The identification of the attribute to be set
     * and the value it is to be set to.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     * @exception AttributeNotFoundException The attribute specified
     * is not accessible in the MBean.
     * @exception InvalidAttributeValueException The value specified
     * for the attribute is not valid.
     * @exception MBeanException Wraps an exception thrown by the
     * MBean's setter.
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.Exception</CODE> thrown when trying to invoke
     * the setter.
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The object
     * name in parameter is null or the attribute in parameter is
     * null.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
            throws InstanceNotFoundException, AttributeNotFoundException,
                   InvalidAttributeValueException, MBeanException,
                   ReflectionException;



    /**
     * Sets the values of several attributes of a named MBean. The MBean is
     * identified by its object name.
     *
     * @param name The object name of the MBean within which the
     * attributes are to be set.
     * @param attributes A list of attributes: The identification of
     * the attributes to be set and the values they are to be set to.
     *
     * @return The list of attributes that were set, with their new
     * values.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     * @exception ReflectionException An exception occurred when
     * trying to invoke the getAttributes method of a Dynamic MBean.
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: The object
     * name in parameter is null or attributes in parameter is null.
     */
    public AttributeList setAttributes(ObjectName name,
                                       AttributeList attributes)
        throws InstanceNotFoundException, ReflectionException;

    /**
     * Invokes an operation on an MBean.
     *
     * @param name The object name of the MBean on which the method is
     * to be invoked.
     * @param operationName The name of the operation to be invoked.
     * @param params An array containing the parameters to be set when
     * the operation is invoked
     * @param signature An array containing the signature of the
     * operation. The class objects will be loaded using the same
     * class loader as the one used for loading the MBean on which the
     * operation was invoked.
     *
     * @return The object returned by the operation, which represents
     * the result of invoking the operation on the MBean specified.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     * @exception MBeanException Wraps an exception thrown by the
     * MBean's invoked method.
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.Exception</CODE> thrown while trying to invoke
     * the method.
     */
    public Object invoke(ObjectName name, String operationName,
                         Object params[], String signature[])
            throws InstanceNotFoundException, MBeanException,
                   ReflectionException;

    /**
     * Returns the default domain used for naming the MBean.
     * The default domain name is used as the domain part in the ObjectName
     * of MBeans if no domain is specified by the user.
     */
    public String getDefaultDomain();

    /**
     * <p>Adds a listener to a registered MBean.</p>
     *
     * <P> A notification emitted by an MBean will be forwarded by the
     * MBeanServer to the listener.  If the source of the notification
     * is a reference to an MBean object, the MBean server will replace it
     * by that MBean's ObjectName.  Otherwise the source is unchanged.
     *
     * @param name The name of the MBean on which the listener should
     * be added.
     * @param listener The listener object which will handle the
     * notifications emitted by the registered MBean.
     * @param filter The filter object. If filter is null, no
     * filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a
     * notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name provided
     * does not match any of the registered MBeans.
     */
    public void addNotificationListener(ObjectName name,
                                        NotificationListener listener,
                                        NotificationFilter filter,
                                        Object handback)
            throws InstanceNotFoundException;


    /**
     * <p>Adds a listener to a registered MBean.</p>
     *
     * <p>A notification emitted by an MBean will be forwarded by the
     * MBeanServer to the listener.  If the source of the notification
     * is a reference to an MBean object, the MBean server will
     * replace it by that MBean's ObjectName.  Otherwise the source is
     * unchanged.</p>
     *
     * <p>The listener object that receives notifications is the one
     * that is registered with the given name at the time this method
     * is called.  Even if it is subsequently unregistered, it will
     * continue to receive notifications.</p>
     *
     * @param name The name of the MBean on which the listener should
     * be added.
     * @param listener The object name of the listener which will
     * handle the notifications emitted by the registered MBean.
     * @param filter The filter object. If filter is null, no
     * filtering will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a
     * notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name of the
     * notification listener or of the notification broadcaster does
     * not match any of the registered MBeans.
     * @exception RuntimeOperationsException Wraps an {@link
     * IllegalArgumentException}.  The MBean named by
     * <code>listener</code> exists but does not implement the {@link
     * NotificationListener} interface.
     * @exception IOException A communication problem occurred when
     * talking to the MBean server.
     */
    public void addNotificationListener(ObjectName name,
                                        ObjectName listener,
                                        NotificationFilter filter,
                                        Object handback)
            throws InstanceNotFoundException;

    /**
     * Removes a listener from a registered MBean.
     *
     * <P> If the listener is registered more than once, perhaps with
     * different filters or callbacks, this method will remove all
     * those registrations.
     *
     * @param name The name of the MBean on which the listener should
     * be removed.
     * @param listener The object name of the listener to be removed.
     *
     * @exception InstanceNotFoundException The MBean name provided
     * does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not
     * registered in the MBean.
     */
    public void removeNotificationListener(ObjectName name,
                                           ObjectName listener)
        throws InstanceNotFoundException, ListenerNotFoundException;

    /**
     * <p>Removes a listener from a registered MBean.</p>
     *
     * <P> If the listener is registered more than once, perhaps with
     * different filters or callbacks, this method will remove all
     * those registrations.
     *
     * @param name The name of the MBean on which the listener should
     * be removed.
     * @param listener The listener object which will handle the
     * notifications emitted by the registered MBean.
     *
     * @exception InstanceNotFoundException The MBean name provided
     * does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not
     * registered in the MBean.
     */
    public void removeNotificationListener(ObjectName name,
                                           NotificationListener listener)
            throws InstanceNotFoundException, ListenerNotFoundException;

    /**
     * This method discovers the attributes and operations that an
     * MBean exposes for management.
     *
     * @param name The name of the MBean to analyze
     *
     * @return An instance of <CODE>MBeanInfo</CODE> allowing the
     * retrieval of all attributes and operations of this MBean.
     *
     * @exception IntrospectionException An exception occurred during
     * introspection.
     * @exception InstanceNotFoundException The MBean specified was
     * not found.
     * @exception ReflectionException An exception occurred when
     * trying to invoke the getMBeanInfo of a Dynamic MBean.
     */
    public MBeanInfo getMBeanInfo(ObjectName name)
            throws InstanceNotFoundException, IntrospectionException,
                   ReflectionException;


    /**
     * Returns true if the MBean specified is an instance of the
     * specified class, false otherwise.
     *
     * @param name The <CODE>ObjectName</CODE> of the MBean.
     * @param className The name of the class.
     *
     * @return true if the MBean specified is an instance of the
     * specified class, false otherwise.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     * registered in the MBean server.
     */
    public boolean isInstanceOf(ObjectName name, String className)
            throws InstanceNotFoundException;

    /**
     * <p>Return the {@link java.lang.ClassLoader} that was used for
     * loading the class of the named MBean.
     * @param mbeanName The ObjectName of the MBean.
     * @return The ClassLoader used for that MBean.
     * @exception InstanceNotFoundException if the named MBean is not found.
     */
    public ClassLoader getMBeanClassLoader(ObjectName mbeanName)
        throws InstanceNotFoundException;
}
