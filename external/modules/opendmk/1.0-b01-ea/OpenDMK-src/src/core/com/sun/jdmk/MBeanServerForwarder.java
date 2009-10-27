/*
 * @(#)file      MBeanServerForwarder.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.14
 * @(#)lastedit  07/03/08
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

import java.io.*;
import java.util.*;
import java.util.Set;
/*
 * We import Set explicitly so that the script that converts 1.2 java.util
 * classes into 1.1 equivalents in a special jar can work.
 */

import javax.management.*;
import javax.management.loading.ClassLoaderRepository;

/**
 * <p>An object of this class implements the MBeanServer interface and
 * wraps another object that also implements that interface.  Every
 * method in MBeanServer is forwarded to the wrapped object.</p>
 *
 * <p>This class is principally useful as the superclass for other wrapping
 * classes that intercept some of the methods in MBeanServer and perform
 * some action before forwarding to the wrapped object (or do not forward
 * at all).</p>
 *
 * <p>The wrapped object can be changed at any time.  A frequent operation is
 * to insert another MBeanServerForwarder between this one and the wrapped
 * object.  The idiom for that is:</p>
 *
 * <pre>
 * synchronized (mbeanServerForwarder) {
 *     MBeanServer oldmbs = mbeanServerForwarder.getMBeanServer();
 *     MBeanServer newmbs = new MyMBeanServerForwarder(oldmbs);
 *     mbeanServerForwarder.setMBeanServer(newmbs);
 * }
 * </pre>
 *
 * <p>The <code>synchronized</code> block ensures that information is not
 * lost if two threads try to insert MBeanServerForwarders at the same time.<p>
 *
 * @since Java DMK 5.1
 */
public class MBeanServerForwarder implements MBeanServer {

    /**
     * Make a new MBeanServerForwarder that forwards all requests from the
     * MBeanServer interface to <code>mbs</code>.
     * @param mbs the wrapped MBeanServer to forward to.
     */
    public MBeanServerForwarder(MBeanServer mbs) {
        setMBeanServer(mbs);
    }

    /**
     * Return the wrapped MBeanServer to which all requests from the
     * MBeanServer interface are forwarded.
     */
    public synchronized MBeanServer getMBeanServer() {
        return mbs;
    }

    /**
     * <p>Change the wrapped MBeanServer object to which all requests
     * from the MBeanServer interface are forwarded.  The old wrapped
     * object is lost unless it was accessible by other means.</p>
     * @param mbs the new wrapped object.
     * @exception IllegalArgumentException if <code>mbs</code> is null.
     */
    public synchronized void setMBeanServer(MBeanServer mbs)
            throws IllegalArgumentException {
        if (mbs == null)
            throw new IllegalArgumentException("Null MBeanServer");
        this.mbs = mbs;
    }

    // See javax.management.MBeanServer
    //
    public Object instantiate(String className)
            throws ReflectionException, MBeanException {
        return instantiate(className, (Object[]) null, (String[]) null);
    }

    // See javax.management.MBeanServer
    //
    public Object instantiate(String className, ObjectName loaderName) 
            throws ReflectionException, MBeanException,
                   InstanceNotFoundException {
        return instantiate(className, loaderName, (Object[]) null,
                           (String[]) null);
    }

    // See javax.management.MBeanServer
    //
    public Object instantiate(String className, Object params[],
                              String signature[]) 
            throws ReflectionException, MBeanException {
        return mbs.instantiate(className, params, signature);
    }

    // See javax.management.MBeanServer
    //
    public Object instantiate(String className, ObjectName loaderName,
                              Object params[], String signature[]) 
            throws ReflectionException, MBeanException,
                   InstanceNotFoundException {
        return mbs.instantiate(className, loaderName, params, signature);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInstance createMBean(String className, ObjectName name)
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException {
        return createMBean(className, name, (Object[]) null, (String[]) null);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName) 
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException, InstanceNotFoundException {
        return createMBean(className, name, loaderName, (Object[]) null,
                           (String[]) null);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInstance createMBean(String className, ObjectName name,
                                      Object params[], String signature[]) 
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException {
        return mbs.createMBean(className, name, params, signature);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName, Object params[],
                                      String signature[]) 
            throws ReflectionException, InstanceAlreadyExistsException,
                   MBeanRegistrationException, MBeanException,
                   NotCompliantMBeanException, InstanceNotFoundException {
        return mbs.createMBean(className, name, loaderName, params, signature);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInstance registerMBean(Object object, ObjectName name)
            throws InstanceAlreadyExistsException, MBeanRegistrationException,
                   NotCompliantMBeanException {
        return mbs.registerMBean(object, name);
    }

    // See javax.management.MBeanServer
    //
    public void unregisterMBean(ObjectName name)
            throws InstanceNotFoundException, MBeanRegistrationException {
        mbs.unregisterMBean(name);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInstance getObjectInstance(ObjectName name)
            throws InstanceNotFoundException {
        return mbs.getObjectInstance(name);
    }

    // See javax.management.MBeanServer
    //
    public Set queryMBeans(ObjectName name, QueryExp query) {
        return mbs.queryMBeans(name, query);
    }

    // See javax.management.MBeanServer
    //
    public Set queryNames(ObjectName name, QueryExp query) {
        return mbs.queryNames(name, query);
    }

    // See javax.management.MBeanServer
    //
    public boolean isRegistered(ObjectName name) {
        return mbs.isRegistered(name);
    }

    // See javax.management.MBeanServer
    //
    public Integer getMBeanCount() {
        return mbs.getMBeanCount();
    }

    // See javax.management.MBeanServer
    //
    public Object getAttribute(ObjectName name, String attribute)
            throws MBeanException, AttributeNotFoundException,
                   InstanceNotFoundException, ReflectionException {
        return mbs.getAttribute(name, attribute);
    }

    // See javax.management.MBeanServer
    //
    public AttributeList getAttributes(ObjectName name, String[] attributes)
            throws InstanceNotFoundException, ReflectionException {
        return mbs.getAttributes(name, attributes);
    }

    // See javax.management.MBeanServer
    //
    public void setAttribute(ObjectName name, Attribute attribute)
            throws InstanceNotFoundException, AttributeNotFoundException,
                   InvalidAttributeValueException, MBeanException, 
                   ReflectionException {
        mbs.setAttribute(name, attribute);
    }

    // See javax.management.MBeanServer
    //
    public AttributeList setAttributes(ObjectName name,
                                       AttributeList attributes)
            throws InstanceNotFoundException, ReflectionException {
        return mbs.setAttributes(name, attributes);
    }

    // See javax.management.MBeanServer
    //
    public Object invoke(ObjectName name, String operationName,
                         Object params[], String signature[])
            throws InstanceNotFoundException, MBeanException,
                   ReflectionException {
        return mbs.invoke(name, operationName, params, signature);
    }
 
    // See javax.management.MBeanServer
    //
    public String getDefaultDomain() {
        return mbs.getDefaultDomain();
    }

    // See javax.management.MBeanServer
    //
    //
    public String[] getDomains() {
        return mbs.getDomains();
    }

    // See javax.management.MBeanServer
    //
    public void addNotificationListener(ObjectName name,
                                        NotificationListener listener,
                                        NotificationFilter filter,
                                        Object handback)
            throws InstanceNotFoundException {
        mbs.addNotificationListener(name, listener, filter, handback);
    }

    // See javax.management.MBeanServer
    //
    public void addNotificationListener(ObjectName name, ObjectName listener,
                                        NotificationFilter filter,
                                        Object handback)
            throws InstanceNotFoundException {
        mbs.addNotificationListener(name, listener, filter, handback);
    }

    // See javax.management.MBeanServer
    //
    public void removeNotificationListener(ObjectName name,
                                           NotificationListener listener)
            throws InstanceNotFoundException, ListenerNotFoundException {
        mbs.removeNotificationListener(name, listener);
    }

    // See javax.management.MBeanServer
    //
    //
    public void removeNotificationListener(ObjectName name,
                                           NotificationListener listener,
                                           NotificationFilter filter,
                                           Object handback)
            throws InstanceNotFoundException, ListenerNotFoundException {
        mbs.removeNotificationListener(name, listener, filter, handback);
    }

    // See javax.management.MBeanServer
    //
    public void removeNotificationListener(ObjectName name,
                                           ObjectName listener) 
            throws InstanceNotFoundException, ListenerNotFoundException {
        mbs.removeNotificationListener(name, listener);
    }

    // See javax.management.MBeanServer
    //
    //
    public void removeNotificationListener(ObjectName name,
                                           ObjectName listener,
                                           NotificationFilter filter,
                                           Object handback) 
            throws InstanceNotFoundException, ListenerNotFoundException {
        mbs.removeNotificationListener(name, listener, filter, handback);
    }

    // See javax.management.MBeanServer
    //
    public MBeanInfo getMBeanInfo(ObjectName name)
            throws InstanceNotFoundException, IntrospectionException,
                   ReflectionException {
        return mbs.getMBeanInfo(name);
    }

    // See javax.management.MBeanServer
    //
    public boolean isInstanceOf(ObjectName name, String className)
            throws InstanceNotFoundException {
        return mbs.isInstanceOf(name, className);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInputStream deserialize(ObjectName name, byte[] data)
            throws InstanceNotFoundException, OperationsException {
        return mbs.deserialize(name, data);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInputStream deserialize(String className, byte[] data)
            throws OperationsException, ReflectionException {
        return mbs.deserialize(className, data);
    }

    // See javax.management.MBeanServer
    //
    public ObjectInputStream deserialize(String className,
                                         ObjectName loaderName, byte[] data)
            throws InstanceNotFoundException, OperationsException,
                   ReflectionException {
        return mbs.deserialize(className, loaderName, data);
    }

    // See javax.management.MBeanServer
    //
    //
    public ClassLoader getClassLoaderFor(ObjectName mbeanName) 
        throws InstanceNotFoundException {
        return mbs.getClassLoaderFor(mbeanName);
    }

    // See javax.management.MBeanServer
    //
    //
    public ClassLoader getClassLoader(ObjectName loaderName)
        throws InstanceNotFoundException {
        return mbs.getClassLoader(loaderName);
    }

    // See javax.management.MBeanServer
    //
    //
    public ClassLoaderRepository getClassLoaderRepository() {
        return mbs.getClassLoaderRepository();
    }

    private MBeanServer mbs;
}
