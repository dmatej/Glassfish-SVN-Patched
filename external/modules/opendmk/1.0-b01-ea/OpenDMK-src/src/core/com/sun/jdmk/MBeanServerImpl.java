/*
 * @(#)file      MBeanServerImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.41
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

// java import
import java.util.Iterator;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.io.OptionalDataException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException ;

// RI import
import javax.management.*;
import javax.management.MBeanServerDelegate;
import javax.management.MBeanServerFactory;
import javax.management.loading.ClassLoaderRepository;

import com.sun.jdmk.defaults.Utils;
import com.sun.jdmk.interceptor.MBeanServerInterceptor;

/**
 * <p><b>Deprecated.</b> Use objects returned by
 * {@link JdmkMBeanServerBuilder}
 * instead.</p>
 * This is a JMX 1.1 implementation of the MBeanServer interface,
 * the base class for MBean manipulation on the agent side. It
 * contains the methods necessary for the creation, registration, and
 * deletion of MBeans as well as the access methods for registered MBeans.
 * This class is no longer used by the Java DMK runtime which now relies
 * on JMX 1.2 MBeanServer implementations obtained through the
 * {@link MBeanServerFactory}.
 * Instead of using this class, you can obtain a Java DMK 5.0 backward
 * compatible MBeanServer from the
 * {@link  MBeanServerFactory} by setting the system property
 * <tt>javax.management.builder.initial</tt> value to point to the
 * Java DMK MBeanServerBuilder {@link JdmkMBeanServerBuilder
 * com.sun.jdmk.JdmkMBeanServerBuilder}.
 * The MBeanServer instance returned by the <tt>JdmkMBeanServerBuilder</tt>
 * is a JMX 1.2 compliant MBeanServer that also provides backward
 * compatibility with Java DMK 5.0 APIs.
 *
 * @deprecated Use objects returned by {@link JdmkMBeanServerBuilder} instead.
 */
public class MBeanServerImpl implements MBeanServer, MBeanServerInt {

    /**
     * Fix security hole in ClassLoaderRepository. This class wraps
     * the actual ClassLoaderRepository implementation so that
     * only the methods from {@link
     * javax.management.loading.ClassLoaderRepository}
     * can be accessed (read-only).
     *
     * @since Java DMK 5.1
     */
    final static class SecureClassLoaderRepository
        implements ClassLoaderRepository {

        private final ClassLoaderRepository clr;

        /**
         * Creates a new secure ClassLoaderRepository wrapping an
         * unsecure implementation.
         * @param clr Unsecure {@link ClassLoaderRepository} implementation
         *            to wrap.
         **/
        public SecureClassLoaderRepository(ClassLoaderRepository clr) {
            this.clr=clr;
        }
        public final Class loadClass(String className)
            throws ClassNotFoundException {
            return clr.loadClass(className);
        }
        public final Class loadClassWithout(ClassLoader loader,
                                            String className)
            throws ClassNotFoundException {
            return clr.loadClassWithout(loader,className);
        }
        public final Class loadClassBefore(ClassLoader loader,
                                           String className)
            throws ClassNotFoundException {
            return clr.loadClassBefore(loader,className);
        }
    }

    /** The Interceptor object to which the MBean Server forwards
        all requests */
    private transient MBeanInterceptor interceptor = null;

    /** The name of this class to be used for tracing */
    private final String dbgTag = "MBeanServer";

    private MBeanInstantiator instantiator = null;

    private MetaData meta = null;

    /** The MBeanServerDelegate object representing the MBean Server */
    private transient MBeanServerDelegate MBeanServerDelegateObject = null;

    /** The MBeanServerDelegate object name */
    private transient ObjectName MBeanServerDelegateObjectName = null;

    private transient SecureClassLoaderRepository secureClr = null;

    /**
     * <b>Public internal:</b> Creates an MBeanServer with a standard default
     * domain name. This constructor is used internally by the
     * {@link javax.management.MBeanServerFactory}.
     * <p>The default domain name is used as the domain part in the ObjectName
     * of MBeans, if no domain is specified by the user.
     * <br>
     * The standard default domain name is defined in
     * {@link com.sun.jdmk.ServiceName#DOMAIN ServiceName.DOMAIN}
     * <ul><b>Note:</b>Using this constructor directly is strongly
     *     discouraged. You should use
     *     {@link javax.management.MBeanServerFactory#createMBeanServer()}
     *     or
     *     {@link javax.management.MBeanServerFactory#newMBeanServer()}
     *     instead.
     * </ul>
     * @param instantiator The MBeanInstantiator that will be used to
     *        instantiate MBeans and take care of class loading issues.
     *        This parameter may not be null.
     * @exception IllegalArgumentException if the instantiator is null.
     */
    public MBeanServerImpl(MBeanInstantiator instantiator) {
        this(instantiator,null);
    }

    /**
     * <b>Public internal:</b> Creates an MBeanServer with the
     * specified default domain name. This constructor is used internally
     * by the {@link javax.management.MBeanServerFactory}.
     * <p>The default domain name is used as the domain part in the ObjectName
     * of MBeans if no domain is specified by the user.
     * <ul><b>Note:</b>Using this constructor directly is strongly
     *     discouraged. You should use
     *     {@link javax.management.MBeanServerFactory#createMBeanServer(java.lang.String)}
     *     or
     *     {@link javax.management.MBeanServerFactory#newMBeanServer(java.lang.String)}
     *     instead.
     * </ul>
     * @param instantiator The MBeanInstantiator that will be used to
     *        instantiate MBeans and take care of class loading issues.
     *        This parameter may not be null.
     * @param domain The default domain name used by this MBeanServer
     * @exception IllegalArgumentException if the instantiator is null.
     */
    public MBeanServerImpl(MBeanInstantiator instantiator, String domain)  {
        this(instantiator, null, domain);
    }

    /**
     * <b>Public internal:</b> Creates an MBeanServer with the
     * specified default domain name.  This constructor is used internally
     * by the {@link javax.management.MBeanServerFactory}.
     * <p>The default domain name is used as the domain part in the ObjectName
     * of MBeans if no domain is specified by the user.
     * <ul><b>Note:</b>Using this constructor directly is strongly
     *     discouraged. You should use
     *     {@link javax.management.MBeanServerFactory#createMBeanServer(java.lang.String)}
     *     or
     *     {@link javax.management.MBeanServerFactory#newMBeanServer(java.lang.String)}
     *     instead.
     * </ul>
     * @param instantiator The MBeanInstantiator that will be used to
     *        instantiate MBeans and take care of class loading issues.
     *        This parameter may not be null.
     * @param metadata The MetaData object that will be used by the
     *        MBean server in order to invoke the MBean interface of
     *        the registered MBeans.
     * @param domain The default domain name used by this MBeanServer
     * @exception IllegalArgumentException if the instantiator is null.
     */
    public MBeanServerImpl(MBeanInstantiator instantiator,
                           MetaData metadata, String domain)  {
        initialize(instantiator, metadata, domain);
    }

    /**
     * Return the MBeanInstantiator associated to this MBeanServer.
     */
    public MBeanInstantiator getMBeanInstantiator() {

        return instantiator;
    }

    /**
     * Return the MetaData associated to this MBeanServer.
     */
    public MetaData getMetaData() {

        return meta;
    }

    /**
     * Return the MBeanInterceptor to which all requests from the
     * MBeanServer interface are forwarded.
     */
    public synchronized MBeanInterceptor getDefaultMBeanInterceptor() {
        return interceptor;
    }

    /**
     * <p>Change the MBeanInterceptor object to which all requests
     * from the MBeanServer interface are forwarded.  The old
     * object is lost unless it was accessible by other means.</p>
     *
     * @param mbi the new  object.
     * @exception IllegalArgumentException if <code>mbi</code> is null.
     */
    public synchronized void setDefaultMBeanInterceptor(MBeanInterceptor mbi)
            throws IllegalArgumentException {
        if (mbi == null)
            throw new IllegalArgumentException("Null MBeanInterceptor");
        this.interceptor = mbi;
    }


    /**
     * Instantiates and registers an MBean in the MBean server.
     * The MBean server will use its
     * {@link javax.management.loading.ClassLoaderRepository
     *  Default Loader Repository}
     * to load the class of the MBean.
     * An object name is associated to the MBean.
     * If the object name given is null, the MBean can automatically
     * provide its own name by implementing the
     * {@link javax.management.MBeanRegistration MBeanRegistration} interface.
     * The call returns an <CODE>ObjectInstance</CODE> object representing
     * the newly created MBean.
     *
     * @param className The class name of the MBean to be instantiated.
     * @param name The object name of the MBean. May be null.
     *
     * @return  An <CODE>ObjectInstance</CODE>, containing the
     *     <CODE>ObjectName</CODE> and the Java class name of the newly
     *     instantiated MBean.
     *
     * @exception ReflectionException Wraps an
     *     <CODE>{@link java.lang.ClassNotFoundException}</CODE> or an
     *     <CODE>{@link java.lang.Exception}</CODE> that occurred
     *     when trying to invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already
     *     under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister()</CODE>
     *     (<CODE>MBeanRegistration</CODE> interface) method of the MBean
     *     has thrown an exception. The MBean will not be registered.
     * @exception MBeanException The constructor of the MBean has thrown
     *     an exception.
     * @exception NotCompliantMBeanException This class is not a JMX
     *     compliant MBean.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>:
     *     The className passed in parameter is null, the
     *     <CODE>ObjectName</CODE> passed in parameter contains a pattern
     *     or no <CODE>ObjectName</CODE> is specified for the MBean.
     *
     */
    public ObjectInstance createMBean(String className, ObjectName name)
        throws ReflectionException, InstanceAlreadyExistsException,
               MBeanRegistrationException, MBeanException,
               NotCompliantMBeanException {

        return interceptor.createMBean(className, name, (Object[]) null,
                                       (String[]) null);
    }

    /**
     * Instantiates and registers an MBean in the MBean server.
     * The class loader to be used is identified by its object  name.
     * An object name is associated to the MBean.
     * If the object name  of the loader is null, the ClassLoader that
     * loaded the MBean server will be used.
     * If the MBean's object name given is null, the MBean can
     * automatically provide its own name by implementing the
     * {@link javax.management.MBeanRegistration MBeanRegistration} interface.
     * The call returns an <CODE>ObjectInstance</CODE> object representing
     * the newly created MBean.
     *
     * @param className The class name of the MBean to be instantiated.
     * @param name The object name of the MBean. May be null.
     * @param loaderName The object name of the class loader to be used.
     *
     * @return  An <CODE>ObjectInstance</CODE>, containing the
     *     <CODE>ObjectName</CODE> and the Java class name
     *     of the newly instantiated MBean.
     *
     * @exception ReflectionException  Wraps an
     *     <CODE>{@link java.lang.ClassNotFoundException}</CODE> or an
     *     <CODE>{@link java.lang.Exception}</CODE> that occurred when trying
     *     to invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already
     *     under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister()</CODE>
     *     (<CODE>MBeanRegistration</CODE>  interface) method of the MBean
     *     has thrown an exception. The MBean will not be registered.
     * @exception MBeanException The constructor of the MBean has thrown
     *     an exception
     * @exception NotCompliantMBeanException This class is not a JMX
     *     compliant MBean.
     * @exception InstanceNotFoundException The specified class loader
     *     is not registered in the MBean server.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *     className passed in parameter is null, the <CODE>ObjectName</CODE>
     *     passed in parameter contains a pattern or no
     *     <CODE>ObjectName</CODE> is specified for the MBean.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName)
        throws ReflectionException, InstanceAlreadyExistsException,
               MBeanRegistrationException, MBeanException,
               NotCompliantMBeanException, InstanceNotFoundException {

        return interceptor.createMBean(className, name, loaderName,
                                       (Object[]) null,(String[]) null);
    }


    /**
     * Instantiates and registers an MBean in the MBean server.
     * The MBean server will use its
     * {@link javax.management.loading.ClassLoaderRepository
     * Default Loader Repository}
     * to load the class of the MBean.
     * An object name is associated to the MBean.
     * If the object name given is null, the MBean can automatically
     * provide its own name by implementing the
     * {@link javax.management.MBeanRegistration MBeanRegistration} interface.
     * The call returns an <CODE>ObjectInstance</CODE> object representing
     * the newly created MBean.
     *
     * @param className The class name of the MBean to be instantiated.
     * @param name The object name of the MBean. May be null.
     * @param params An array containing the parameters of the constructor
     *     to be invoked.
     * @param signature An array containing the signature of the
     *     constructor to be invoked.
     *
     * @return  An <CODE>ObjectInstance</CODE>, containing the
     *     <CODE>ObjectName</CODE> and the Java class name
     *     of the newly instantiated MBean.
     *
     * @exception ReflectionException Wraps a
     *     <CODE>{@link java.lang.ClassNotFoundException}</CODE> or an
     *     <CODE>{@link java.lang.Exception}</CODE> that occurred
     *     when trying to invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already
     *     under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister()</CODE>
     *     (<CODE>MBeanRegistration</CODE>  interface) method of the MBean
     *     has thrown an exception. The MBean will not be registered.
     * @exception MBeanException The constructor of the MBean has
     *     thrown an exception.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *     className passed in parameter is null, the <CODE>ObjectName</CODE>
     *     passed in parameter contains a pattern or no
     *     <CODE>ObjectName</CODE> is specified for the MBean.
     *
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      Object params[], String signature[])
        throws ReflectionException, InstanceAlreadyExistsException,
               MBeanRegistrationException, MBeanException,
               NotCompliantMBeanException  {

        return interceptor.createMBean(className, name, params,signature);
    }

   /**
     * Instantiates and registers an MBean in the MBean server.
     * The class loader to be used is identified by its object name.
     * An object name is associated to the MBean. If the object name
     * of the loader is not specified, the ClassLoader that loaded the
     * MBean server will be used.
     * If  the MBean object name given is null, the MBean can automatically
     * provide its own name by implementing the
     * {@link javax.management.MBeanRegistration MBeanRegistration} interface.
     * The call returns an <CODE>ObjectInstance</CODE> object representing
     * the newly created MBean.
     *
     * @param className The class name of the MBean to be instantiated.
     * @param name The object name of the MBean. May be null.
     * @param params An array containing the parameters of the constructor
     *      to be invoked.
     * @param signature An array containing the signature of the
     *     constructor to be invoked.
     * @param loaderName The object name of the class loader to be used.
     *
     * @return  An <CODE>ObjectInstance</CODE>, containing the
     *     <CODE>ObjectName</CODE> and the Java class name of the newly
     *     instantiated MBean.
     *
     * @exception ReflectionException Wraps a
     *     <CODE>{@link java.lang.ClassNotFoundException}</CODE> or an
     *     <CODE>{@link java.lang.Exception}</CODE>
     *     that occurred when trying to invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already
     *     under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister()</CODE>
     *     (<CODE>MBeanRegistration</CODE>  interface) method of the MBean
     *     has thrown an exception. The MBean will not be registered.
     * @exception MBeanException The constructor of the MBean has
     *      thrown an exception
     * @exception InstanceNotFoundException The specified class loader is
     *      not registered in the MBean server.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *     className passed in parameter is null, the <CODE>ObjectName</CODE>
     *     passed in parameter contains a pattern or no
     *     <CODE>ObjectName</CODE> is specified for the MBean.
     *
     */
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName, Object params[],
                                      String signature[])
        throws ReflectionException, InstanceAlreadyExistsException,
               MBeanRegistrationException, MBeanException,
               NotCompliantMBeanException, InstanceNotFoundException {

        return interceptor.createMBean(className, name, loaderName,
                                       params,signature);
    }

    /**
     * Registers a pre-existing object as an MBean with the MBean server.
     * If the object name given is null, the MBean may automatically
     * provide its own name by implementing the
     * {@link javax.management.MBeanRegistration MBeanRegistration}  interface.
     * The call returns an <CODE>ObjectInstance</CODE> object representing
     * the registered MBean.
     *
     * @param object The  MBean to be registered as an MBean.
     * @param name The object name of the MBean. May be null.
     *
     * @return The <CODE>ObjectInstance</CODE> for the MBean that has been
     *      registered.
     *
     * @exception InstanceAlreadyExistsException The MBean is already
     *      under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister()</CODE>
     *      (<CODE>MBeanRegistration</CODE>  interface) method of the MBean
     *      has thrown an exception. The MBean will not be registered.
     * @exception NotCompliantMBeanException This object is not a JMX
     *      compliant MBean
     * @exception RuntimeOperationsException Wraps an
     *      <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *      object passed in parameter is null or no object name is specified.
     *
     */
    public ObjectInstance registerMBean(Object object, ObjectName name)
        throws InstanceAlreadyExistsException, MBeanRegistrationException,
               NotCompliantMBeanException  {

        return interceptor.registerMBean(object, name);
    }

    /**
     * De-registers an MBean from the MBean server. The MBean is identified by
     * its object name. Once the method has been invoked, the MBean may
     * no longer be accessed by its object name.
     *
     * @param name The object name of the MBean to be de-registered.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     *     registered in the MBean server.
     * @exception MBeanRegistrationException The <code>preDeregister()<code>
     *     (<CODE>MBeanRegistration</CODE>  interface) method of the MBean
     *     has thrown an exception.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *     object name in parameter is null or the MBean you are when
     *     trying to de-register is the
     *     {@link javax.management.MBeanServerDelegate MBeanServerDelegate}
     *     MBean.
     **/
    public void unregisterMBean(ObjectName name)
        throws InstanceNotFoundException, MBeanRegistrationException  {
        // Now handled by the delegate itself..
        // if (name.equals(MBeanServerDelegateObjectName)) {
        //    throw new RuntimeOperationsException(
        //          new IllegalArgumentException(
        //               "The MBeanDelegate MBean cannot be unregistered"));
        // }
        interceptor.unregisterMBean(name);
    }

    /**
     * Gets the <CODE>ObjectInstance</CODE> for a given MBean registered
     * with the MBean server.
     *
     * @param name The object name of the MBean.
     *
     * @return The <CODE>ObjectInstance</CODE> associated to the MBean
     *       specified by <VAR>name</VAR>.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     *       registered in the MBean server.
     */
    public ObjectInstance getObjectInstance(ObjectName name)
        throws InstanceNotFoundException {

        return interceptor.getObjectInstance(name);
    }

    /**
     * Gets MBeans controlled by the MBean server. This method allows any
     * of the following to be obtained: All MBeans, a set of MBeans specified
     * by pattern matching on the <CODE>ObjectName</CODE> and/or a Query
     * expression, a specific MBean. When the object name is null or no
     * domain and key properties are specified, all objects are to be
     * selected (and filtered if a query is specified). It returns the
     * set of <CODE>ObjectInstance</CODE> objects (containing the
     * <CODE>ObjectName</CODE> and the Java Class name) for
     * the selected MBeans.
     *
     * @param name The object name pattern identifying the MBeans to
     *      be retrieved. If null or or no domain and key properties
     *      are specified, all the MBeans registered will be retrieved.
     * @param query The query expression to be applied for selecting
     *      MBeans. If null no query expression will be applied for
     *      selecting MBeans.
     *
     * @return  A set containing the <CODE>ObjectInstance</CODE> objects
     *      for the selected MBeans.
     *      If no MBean satisfies the query an empty list is returned.
     *
     */
    public Set queryMBeans(ObjectName name, QueryExp query) {

        return interceptor.queryMBeans(name, query);
    }

    /**
     * Gets the names of MBeans controlled by the MBean server. This method
     * enables any of the following to be obtained: The names of all MBeans,
     * the names of a set of MBeans specified by pattern matching on the
     * <CODE>ObjectName</CODE> and/or a Query expression, a specific
     * MBean name (equivalent to testing whether an MBean is registered).
     * When the object name is null or or no domain and key properties are
     * specified, all objects are selected (and filtered if a query is
     * specified). It returns the set of ObjectNames for the MBeans
     * selected.
     *
     * @param name The object name pattern identifying the MBeans to be
     *     retrieved. If null or no domain and key properties are
     *     specified, all the MBeans registered will be retrieved.
     * @param query The query expression to be applied for selecting
     *     MBeans. If null no query expression will be applied for
     *     selecting MBeans.
     *
     * @return  A set containing the ObjectNames for the MBeans selected.
     *     If no MBean satisfies the query, an empty list is returned.
     *
     */
    public Set queryNames(ObjectName name, QueryExp query) {

        return interceptor.queryNames(name, query);
    }



    /**
     * Checks whether an MBean, identified by its object name, is already
     * registered with the MBean server.
     *
     * @param name The object name of the MBean to be checked.
     *
     * @return  True if the MBean is already registered in the MBean server,
     *     false otherwise.
     *
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The object
     *      name in parameter is null.
     *
     */
    public boolean isRegistered(ObjectName name)  {

        return interceptor.isRegistered(name);
    }



    /**
     * Returns the number of MBeans registered in the MBean server.
     */
    public Integer getMBeanCount()  {

        return interceptor.getMBeanCount();
    }

    /**
     * Gets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The object name of the MBean from which the attribute
     *     is to be retrieved.
     * @param attribute A String specifying the name of the attribute to be
     *     retrieved.
     *
     * @return  The value of the retrieved attribute.
     *
     * @exception AttributeNotFoundException The attribute specified
     *     is not accessible in the MBean.
     * @exception MBeanException  Wraps an exception thrown by the
     *     MBean's getter.
     * @exception InstanceNotFoundException The MBean specified is not
     *     registered in the MBean server.
     * @exception ReflectionException  Wraps an
     *     <CODE>{@link java.lang.Exception}</CODE> thrown when trying to
     *     invoke the setter.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>:
     *     The object name in parameter is null or the attribute in
     *     parameter is null.
     */
    public Object getAttribute(ObjectName name, String attribute)
        throws MBeanException, AttributeNotFoundException,
               InstanceNotFoundException, ReflectionException {

        return interceptor.getAttribute(name, attribute);
    }


    /**
     * Enables the values of several attributes of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The object name of the MBean from which the attributes are
     *     retrieved.
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return The list of the retrieved attributes.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     *     registered in the MBean server.
     * @exception ReflectionException An exception occurred when trying
     *     to invoke the getAttributes method of a Dynamic MBean.
     * @exception RuntimeOperationsException Wrap an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *     object name in parameter is null or attributes in parameter
     *     is null.
     *
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
        throws InstanceNotFoundException, ReflectionException  {

        return interceptor.getAttributes(name, attributes);

    }

    /**
     * Sets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The name of the MBean within which the attribute is
     *     to be set.
     * @param attribute The identification of the attribute to be set
     *     and the value it is to be set to.
     *
     * @exception InstanceNotFoundException The MBean specified is
     *     not registered in the MBean server.
     * @exception AttributeNotFoundException The attribute specified is
     *     not accessible in the MBean.
     * @exception InvalidAttributeValueException The value specified for
     *     the attribute is not valid.
     * @exception MBeanException Wraps an exception thrown by the
     *     MBean's setter.
     * @exception ReflectionException  Wraps an
     *     <CODE>{@link java.lang.Exception}</CODE> thrown when trying
     *     to invoke the setter.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *     object name in parameter is null or the attribute in parameter
     *     is null.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
        throws InstanceNotFoundException, AttributeNotFoundException,
               InvalidAttributeValueException, MBeanException,
               ReflectionException  {

        interceptor.setAttribute(name, attribute);
    }


    /**
     * Sets the values of several attributes of a named MBean. The MBean is
     * identified by its object name.
     *
     * @param name The object name of the MBean within which the
     *     attributes are to  be set.
     * @param attributes A list of attributes: The identification of the
     *     attributes to be set and  the values they are to be set to.
     *
     * @return  The list of attributes that were set, with their new values.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     *      registered in the MBean server.
     * @exception ReflectionException An exception occurred when trying
     *      to invoke the getAttributes method of a Dynamic MBean.
     * @exception RuntimeOperationsException Wraps an
     *      <CODE>{@link java.lang.IllegalArgumentException}</CODE>:
     *     The object name in parameter is null or  attributes in
     *     parameter is null.
     *
     */
    public AttributeList setAttributes(ObjectName name,
                                       AttributeList attributes)
        throws InstanceNotFoundException, ReflectionException  {

        return interceptor.setAttributes(name, attributes);
    }

    /**
     * Invokes an operation on an MBean.
     *
     * @param name The object name of the MBean on which the method is to be
     *     invoked.
     * @param operationName The name of the operation to be invoked.
     * @param params An array containing the parameters to be set when
     *     the operation is invoked
     * @param signature An array containing the signature of the operation.
     *     The class objects will be loaded using the same class loader as
     *     the one used for loading the MBean on which the operation was
     *     invoked.
     *
     * @return  The object returned by the operation, which represents the
     *      result of invoking the operation on the  MBean specified.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     *       registered in the MBean server.
     * @exception MBeanException  Wraps an exception thrown by the MBean's
     *       invoked method.
     * @exception ReflectionException  Wraps an
     *       <CODE>{@link java.lang.Exception}</CODE> thrown while trying
     *        to invoke the method.
     *
     */
    public Object invoke(ObjectName name, String operationName,
                         Object params[], String signature[])
        throws InstanceNotFoundException, MBeanException,
               ReflectionException {
        return interceptor.invoke(name, operationName, params, signature);
    }

    /**
     * Returns the default domain used for naming the MBean.
     * The default domain name is used as the domain part in the ObjectName
     * of MBeans if no domain is specified by the user.
     */
    public String getDefaultDomain()  {
        return interceptor.getDefaultDomain();
    }

    /**
     * Adds a listener to a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be added.
     * @param listener The listener object which will handle the
     *        notifications emitted by the registered MBean.
     * @param filter The filter object. If filter is null, no filtering
     *        will be performed before handling notifications.
     * @param handback The context to be sent to the listener when a
     *        notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name provided does
     *       not match any of the registered MBeans.
     */
    public void addNotificationListener(ObjectName name,
                                        NotificationListener listener,
                                        NotificationFilter filter,
                                        Object handback)
        throws InstanceNotFoundException {

        interceptor.addNotificationListener(name, listener, filter, handback);
    }


    /**
     * Adds a listener to a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be added.
     * @param listener The object name of the listener which will handle the
     *        notifications emitted by the registered MBean.
     * @param filter The filter object. If filter is null, no filtering will
     *        be performed before handling notifications.
     * @param handback The context to be sent to the listener when a
     *        notification is emitted.
     *
     * @exception InstanceNotFoundException The MBean name of the
     *       notification listener or of the notification broadcaster
     *       does not match any of the registered MBeans.
     */
    public void addNotificationListener(ObjectName name, ObjectName listener,
                                   NotificationFilter filter, Object handback)
        throws InstanceNotFoundException {

        interceptor.addNotificationListener(name, listener, filter, handback);
    }


    /**
     * Removes a listener from a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be removed.
     * @param listener The listener object which will handle the notifications emitted by the registered MBean.
     * This method will remove all the information related to this listener.
     *
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not registered in the MBean.
     */
    public void removeNotificationListener(ObjectName name, NotificationListener listener )
        throws InstanceNotFoundException, ListenerNotFoundException {

        interceptor.removeNotificationListener(name, listener) ;
    }

    /**
     * Removes a listener from a registered MBean.
     *
     * @param name The name of the MBean on which the listener should be removed.
     * @param listener The object name of the listener which will handle the notifications emitted by the registered MBean.
     * This method will remove all the information related to this listener.
     *
     * @exception InstanceNotFoundException The MBean name provided does not match any of the registered MBeans.
     * @exception ListenerNotFoundException The listener is not registered in the MBean.
     */
    public void removeNotificationListener(ObjectName name, ObjectName listener)
        throws InstanceNotFoundException, ListenerNotFoundException {

        interceptor.removeNotificationListener(name, listener);
    }

     /**
     * This method discovers the attributes and operations that an MBean exposes
     * for management.
     *
     * @param name The name of the MBean to analyze
     *
     * @return  An instance of <CODE>MBeanInfo</CODE> allowing the retrieval of all attributes and operations
     * of this MBean.
     *
     * @exception IntrospectionException An exception occurs during introspection.
     * @exception InstanceNotFoundException The MBean specified is not found.
     * @exception ReflectionException An exception occurred when trying to invoke the getMBeanInfo of a Dynamic MBean.
     */
    public MBeanInfo getMBeanInfo(ObjectName name) throws
    InstanceNotFoundException, IntrospectionException, ReflectionException {

        return interceptor.getMBeanInfo(name);
    }

    /**
     * Instantiates an object using the list of all class loaders registered
     * in the MBean server (using its
     * {@link javax.management.loading.ClassLoaderRepository
     * Default Loader Repository}).
     * The object's class should have a public constructor.
     * It returns a reference to the newly created object.
     * The newly created object is not registered in the MBean server.
     *
     * @param className The class name of the object to be instantiated.
     *
     * @return The newly instantiated object.
     *
     * @exception ReflectionException Wraps the
     *     <CODE>{@link java.lang.ClassNotFoundException}</CODE> or the
     *     <CODE>{@link java.lang.Exception}</CODE> that
     *     occurred when trying to invoke the object's constructor.
     * @exception MBeanException The constructor of the object has thrown
     *     an exception.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>:
     *     The className passed in parameter is null.
     *
     */
    public Object instantiate(String className)
        throws ReflectionException, MBeanException {
        return instantiator.instantiate(className);
    }


    /**
     * Instantiates an object using the class Loader specified by its
     * <CODE>ObjectName</CODE>.
     * If the loader name is null, the ClassLoader that loaded the
     * MBean Server will be used.
     * The object's class should have a public constructor.
     * It returns a reference to the newly created object.
     * The newly created object is not registered in the MBean server.
     *
     * @param className The class name of the MBean to be instantiated.
     * @param loaderName The object name of the class loader to be used.
     *
     * @return The newly instantiated object.
     *
     * @exception ReflectionException Wraps the
     *     <CODE>{@link java.lang.ClassNotFoundException}</CODE> or the
     *     <CODE>{@link java.lang.Exception}</CODE> that
     *     occurred when trying to invoke the object's constructor.
     * @exception MBeanException The constructor of the object has thrown
     *     an exception.
     * @exception InstanceNotFoundException The specified class loader
     *     is not registered in the MBaenServer.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>: The
     *     className passed in parameter is null.
     *
     */
    public Object instantiate(String className, ObjectName loaderName)
        throws ReflectionException, MBeanException,
               InstanceNotFoundException {

        return instantiator.instantiate(className, loaderName,
                                        this.getClass().getClassLoader());

    }

     /**
     * Instantiates an object using the list of all class loaders registered
     * in the MBean server (using its
     * {@link javax.management.loading.ClassLoaderRepository Default Loader Repository}).
     * The object's class should have a public constructor.
     * The call returns a reference to the newly created object.
     * The newly created object is not registered in the MBean server.
     *
     * @param className The class name of the object to be instantiated.
     * @param params An array containing the parameters of the constructor
     *     to be invoked.
     * @param signature An array containing the signature of the
     *     constructor to be invoked.
     *
     * @return The newly instantiated object.
     *
     * @exception ReflectionException Wraps the
     *     <CODE>{@link java.lang.ClassNotFoundException}</CODE> or the
     *     <CODE>{@link java.lang.Exception}</CODE> that
     *     occurred when trying to invoke the object's constructor.
     * @exception MBeanException The constructor of the object has thrown
     *     an exception.
     * @exception RuntimeOperationsException Wraps an
     *     <CODE>{@link java.lang.IllegalArgumentException}</CODE>:
     *     The className passed in parameter is null.
     *
     */
    public Object instantiate(String className, Object params[],
                              String signature[])
        throws ReflectionException, MBeanException {

        return instantiator.instantiate(className, params, signature,
                                        this.getClass().getClassLoader());

    }


    /**
     * Instantiates an object. The class loader to be used is identified
     * by its object name. If the object name of the loader is null,
     * the ClassLoader that loaded the MBean server will be used.
     * The object's class should have a public constructor.
     * The call returns a reference to the newly created object.
     * The newly created object is not registered in the MBean server.
     *
     * @param className The class name of the object to be instantiated.
     * @param params An array containing the parameters of the constructor
     *     to be invoked.
     * @param signature An array containing the signature of the constructor
     *     to be invoked.
     * @param loaderName The object name of the class loader to be used.
     *
     * @return The newly instantiated object.
     *
     * @exception ReflectionException Wraps the
     *    <CODE>{@link java.lang.ClassNotFoundException}</CODE> or the
     *    <CODE>{@link java.lang.Exception}</CODE> that
     *    occurred when trying to invoke the object's constructor.
     * @exception MBeanException The constructor of the object has thrown
     *    an exception.
     * @exception InstanceNotFoundException The specified class loader
     *    is not registered in the MBean server.
     * @exception RuntimeOperationsException Wraps an
     *    <CODE>{@link java.lang.IllegalArgumentException}</CODE>:
     *    The className passed in parameter is null.
     *
     */
    public Object instantiate(String className, ObjectName loaderName,
                              Object params[], String signature[])
        throws ReflectionException, MBeanException,
               InstanceNotFoundException {

        return instantiator.instantiate(className, loaderName, params,
                                signature, this.getClass().getClassLoader());
    }


    /**
     * Returns true if the MBean specified is an instance of the specified
     * class, false otherwise.
     *
     * @param name The <CODE>ObjectName</CODE> of the MBean.
     * @param className The name of the class.
     *
     * @return true if the MBean specified is an instance of the specified
     *     class, false otherwise.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     *     registered in the MBean server.
     */
    public boolean isInstanceOf(ObjectName name, String className)
        throws InstanceNotFoundException {

        return interceptor.isInstanceOf(name, className);
    }

    /**
     * De-serializes a byte array in the context of the class loader
     * of an MBean.
     *
     * @param name The name of the MBean whose class loader should
     *     be used for the de-serialization.
     * @param data The byte array to be de-sererialized.
     *
     * @return  The de-serialized object stream.
     *
     * @exception InstanceNotFoundException The MBean specified is not
     *     found.
     * @exception OperationsException Any of the usual Input/Output
     *     related exceptions.
     *
     */
    public ObjectInputStream deserialize(ObjectName name, byte[] data)
        throws InstanceNotFoundException, OperationsException {

        return instantiator.deserialize(name, data);
    }


    /**
     * De-serializes a byte array in the context of a given MBean class loader.
     * The class loader is the one that loaded the class with name "className".
     *
     * @param className The name of the class whose class loader should be
     *      used for the de-serialization.
     * @param data The byte array to be de-sererialized.
     *
     * @return  The de-serialized object stream.
     *
     * @exception OperationsException Any of the usual Input/Output
     *      related exceptions.
     * @exception ReflectionException The specified class could not be
     *      loaded by the default loader repository
     *
     */
    public ObjectInputStream deserialize(String className, byte[] data)
        throws OperationsException, ReflectionException {

        return instantiator.deserialize(className, data);
    }



    /**
     * De-serializes a byte array in the context of a given MBean class loader.
     * The class loader is the one that loaded the class with name "className".
     * The name of the class loader to be used for loading the specified
     * class is specified.
     * If null, the MBean Server's class loader will be used.
     *
     * @param className The name of the class whose class loader should be
     *     used for the de-serialization.
     * @param data The byte array to be de-sererialized.
     * @param loaderName The name of the class loader to be used for
     *     loading the specified class.
     *     If null, the MBean Server's class loader will be used.
     *
     * @return  The de-serialized object stream.
     *
     * @exception InstanceNotFoundException The specified class loader
     *     MBean is not found.
     * @exception OperationsException Any of the usual Input/Output
     *     related exceptions.
     * @exception ReflectionException The specified class could not
     *     be loaded by the specified class loader.
     *
     */
    public ObjectInputStream deserialize(String className,
                                         ObjectName loaderName, byte[] data)
        throws InstanceNotFoundException, OperationsException,
               ReflectionException {

        return instantiator.deserialize(className, loaderName, data,
                                        this.getClass().getClassLoader());
    }

    /**
     *  Return the ClassLoader of the MBean with the specified ObjectName.
     */
    public ClassLoader getMBeanClassLoader(ObjectName name)
        throws InstanceNotFoundException {

        return interceptor.getMBeanClassLoader(name);
    }


    private void initialize(MBeanInstantiator instantiator,
                            MetaData meta, String domain) {

        this.instantiator = instantiator;
        if (instantiator == null) throw new
            IllegalArgumentException("instantiator must not be null.");
        this.meta = (meta==null?new MetaDataImpl(instantiator):meta);
        this.secureClr = new
         SecureClassLoaderRepository(instantiator.getClassLoaderRepository());

        if (domain !=null) {
            interceptor = new DefaultMBeanInterceptor(this, domain);
        } else {
            interceptor = new DefaultMBeanInterceptor(this);
        }

        // Create the MBeanServer identification MBean
        try {
            MBeanServerDelegateObject     =
                new MBeanServerDelegateImpl(new MBeanServerDelegate()) ;
            MBeanServerDelegateObjectName =
                new  ObjectName(ServiceName.DELEGATE) ;

            interceptor.registerMBean(MBeanServerDelegateObject,
                                      MBeanServerDelegateObjectName ) ;
        } catch (JMException e) {
            // This should never happen!
            final RuntimeException r =
                new RuntimeException("Unexpected JMException: " + e);
            Utils.initCause(r,e);
            throw r;
        }
        ClassLoader myLoader = this.getClass().getClassLoader();
        final ModifiableClassLoaderRepository loaders =
            instantiator.getClassLoaderRepository();
        if (loaders != null && myLoader != null) {
            loaders.addClassLoader(myLoader);
        }
    }

    /**
     * <p>Return the ClassLoaderRepository for this MBeanServer.
     * @return The ClassLoaderRepository for this MBeanServer.
     *
     * @since Java DMK 5.1 (JMX 1.2)
     */
    public ClassLoaderRepository getClassLoaderRepository() {
        return secureClr;
    }


    /**
     * @since Java DMK 5.1 (JMX 1.2)
     */
    public ClassLoader getClassLoaderFor(ObjectName mbeanName)
        throws InstanceNotFoundException {

        return interceptor.getMBeanClassLoader(mbeanName);
    }


    /**
     * Implements {@link MBeanServer#getClassLoader(ObjectName)}.
     * Contrarily to JMX 1.2 specifications, this methods only works for
     * ClassLoaders that are registered in the ClassLoaderRepository.
     * If you need a fully JMX 1.2 compatible MBeanServer use
     * {@link JdmkMBeanServerBuilder} to create one.
     * @since Java DMK 5.1 (JMX 1.2)
     */
    public ClassLoader getClassLoader(ObjectName loaderName)
        throws InstanceNotFoundException {

        if (interceptor instanceof MBeanServerInterceptor) {
            return ((MBeanServerInterceptor)interceptor).
                getClassLoader(loaderName);
        }
        final ModifiableClassLoaderRepository loaders =
            instantiator.getClassLoaderRepository();
        final ClassLoader loader;
        synchronized (instantiator) {
            loader = loaders.getClassLoader(loaderName);
        }
        if (loader == null) {
            throw new InstanceNotFoundException("The loader named " +
                       loaderName + " is not registered in the MBeanServer");
        }
        return loader;
    }

    /**
     * Only supported if the underlying interceptor
     * is an instance of {@link MBeanServerInterceptor}
     * Otherwise throws UnsupportedOperationException.
     * @exception UnsupportedOperationException if the operation is not
     *            supported by the underlying interceptor.
     * @since Java DMK 1.5
     **/
    public void removeNotificationListener(ObjectName name,
                                           NotificationListener listener,
                                           NotificationFilter filter,
                                           Object handback)
            throws InstanceNotFoundException, ListenerNotFoundException {

        if (interceptor instanceof MBeanServerInterceptor) {
            ((MBeanServerInterceptor)interceptor).
                removeNotificationListener(name,listener,filter,handback);
            return;
        }
        final String unsupported =
            "Operation not supported in this implementation";
        throw new UnsupportedOperationException(unsupported);
    }



    /**
     * Only supported if the underlying interceptor
     * is an instance of {@link MBeanServerInterceptor}
     * Otherwise throws UnsupportedOperationException.
     * @exception UnsupportedOperationException if the operation is not
     *            supported by the underlying interceptor.
     * @since Java DMK 1.5
     **/
    public void removeNotificationListener(ObjectName name,
                                           ObjectName listenerName,
                                           NotificationFilter filter,
                                           Object handback)
            throws InstanceNotFoundException, ListenerNotFoundException {

        if (interceptor instanceof MBeanServerInterceptor) {
            ((MBeanServerInterceptor)interceptor).
                removeNotificationListener(name,listenerName,filter,handback);
            return;
        }
        final String unsupported =
            "Operation not supported in this implementation";
        throw new UnsupportedOperationException(unsupported);
    }

    /**
     * @since Java DMK 1.5
     */
    public String[] getDomains() {
        if (interceptor instanceof MBeanServerInterceptor) {
            return ((MBeanServerInterceptor)interceptor).getDomains();
        }
        final Set names = interceptor.queryNames(null,null);
        final Set tmpSet = new HashSet(1);
        for (final Iterator i = names.iterator() ; i.hasNext() ; ) {
            final ObjectName x = (ObjectName)i.next();
            final String domain = x.getDomain();
            if (tmpSet.contains(domain)) continue;
            tmpSet.add(domain);
        }
        final String[] result = new String[tmpSet.size()];
        return (String[]) tmpSet.toArray(result);
    }



    public javax.management.MBeanServerDelegate getMBeanServerDelegate() {

        return MBeanServerDelegateObject;
    }

}
