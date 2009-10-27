/*
 * @(#)file      MBeanServerChecker.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.13
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
 * <p>An object of this class implements the MBeanServer interface
 * and, for each of its methods, calls an appropriate checking method
 * and then forwards the request to a wrapped MBeanServer object.  The
 * checking method may throw a RuntimeException if the operation is
 * not allowed; in this case the request is not forwarded to the
 * wrapped object.</p>
 *
 * <p>A typical use of this class is to insert it between a connector server
 * such as the HTTP connector and the MBeanServer with which the connector
 * is associated.  Requests from the connector client can then be filtered
 * and those operations that are not allowed, or not allowed in a particular
 * context, can be rejected by throwing a <code>SecurityException</code>
 * in the corresponding <code>check*</code> method.</p>
 *
 * <p>A checking method can obtain the <code>OperationContext</code>
 * associated with the request being handled, if there is one, by
 * calling <code>getOperationContext()</code>.</p>
 *
 * <p>This is an abstract class, because in its implementation none of
 * the checking methods does anything.  To be useful, it must be
 * subclassed and at least one of the checking methods overridden to
 * do some checking.  Some or all of the MBeanServer methods may also
 * be overridden, for instance if the default checking behavior is
 * inappropriate.</p>
 *
 * <p>In effect, three levels of checking are possible:</p>
 *
 * <ul>
 * <li>The subclass can override <code>checkAny</code> so that it applies
 *     some restriction to every operation, or so that it consults the
 *     <code>methodName</code> parameter to apply restrictions to particular
 *     operations.</li>
 * <li>The subclass can override individual <code>check*</code> methods to
 *     apply restrictions to certain classes of methods.  Again, the
 *     <code>methodName</code> parameter is available to refine these
 *     restrictions by applying them only to particular operations.</li>
 * <li>The subclass can override the methods of the <code>MBeanServer</code>
 *     interface itself.  These methods are called with exactly the same
 *     parameters as they will have in the real MBeanServer, so decisions can
 *     be based on complete information.  If an operation is accepted, the
 *     overriding method should call <code>super.<i>methodName</i></code> to
 *     forward it to the real MBeanServer.</li>
 * </ul>
 *
 * <p>The documentation of the individual <code>check*</code> methods describes
 * what the value of the <code>methodName</code> parameter may be.  Future
 * versions of this class may extend these possible values to allow for new
 * methods in the <code>MBeanServer</code> interface.</p>
 */
public abstract class MBeanServerChecker extends MBeanServerForwarder {

    /**
     * <p>Make a new MBeanServerChecker that forwards each method from
     * the MBeanServer interface to the given object, after calling an
     * appropriate checking method.</p>
     *
     * @param mbs the object implementing the MBeanServer interface to which
     * methods should be forwarded.
     */
    protected MBeanServerChecker(MBeanServer mbs) {
	super(mbs);
    }

    /**
     * Call <code>checkAny("instantiate", null)</code>, then
     * <code>checkInstantiate("instantiate", className, null, params,
     * signature)</code>, then forward this method to the wrapped
     * object.
     */
    public Object instantiate(String className, Object params[],
			      String signature[]) 
	    throws ReflectionException, MBeanException {
	checkAny("instantiate", null);
	checkInstantiate("instantiate", className, null, params, signature);
	return super.instantiate(className, params, signature);
    }

    /**
     * Call <code>checkAny("instantiate", null)</code>, then
     * <code>checkInstantiate("instantiate", className, loaderName,
     * params, signature)</code>, then forward this method to the
     * wrapped object.
     */
    public Object instantiate(String className, ObjectName loaderName,
			      Object params[], String signature[]) 
	    throws ReflectionException, MBeanException,
		   InstanceNotFoundException {
	checkAny("instantiate", null);
	checkInstantiate("instantiate", className, loaderName, params,
			 signature);
	return super.instantiate(className, loaderName, params, signature);
    }

    /**
     * Call <code>checkAny("createMBean", name)</code>, then
     * <code>checkCreate("createMBean", className, name, null, params,
     * signature)</code>, then forward this method to the wrapped object.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
				      Object params[], String signature[]) 
	    throws ReflectionException, InstanceAlreadyExistsException,
		   MBeanRegistrationException, MBeanException,
		   NotCompliantMBeanException {
	checkAny("createMBean", name);
	checkCreate("createMBean", className, name, null, params, signature);
	return super.createMBean(className, name, params, signature);
    }

    /**
     * Call <code>checkAny("createMBean", name)</code>, then
     * <code>checkCreate("createMBean", className, name, loaderName, params,
     * signature)</code>, then forward this method to the wrapped object.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
				      ObjectName loaderName, Object params[],
				      String signature[]) 
	    throws ReflectionException, InstanceAlreadyExistsException,
		   MBeanRegistrationException, MBeanException,
		   NotCompliantMBeanException, InstanceNotFoundException {
	checkAny("createMBean", name);
	checkCreate("createMBean", className, name, loaderName, params,
		    signature);
	return super.createMBean(className, name, loaderName, params,
				 signature);
    }

    /**
     * Call <code>checkAny("registerMBean", name)</code>, then
     * <code>checkCreate("registerMBean", object.getClass().getName(), name,
     * null, null, null)</code>, then forward this method to the wrapped
     * object.
     */
    public ObjectInstance registerMBean(Object object, ObjectName name)
	    throws InstanceAlreadyExistsException, MBeanRegistrationException,
		   NotCompliantMBeanException {
	checkAny("registerMBean", name);
	checkCreate("registerMBean",
		    object == null ? null : object.getClass().getName(),
		    name, null, null, null);
	return super.registerMBean(object, name);
    }

    /**
     * Call <code>checkAny("unregisterMBean", name)</code>, then
     * <code>checkDelete("unregisterMBean", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void unregisterMBean(ObjectName name)
	    throws InstanceNotFoundException, MBeanRegistrationException {
	checkAny("unregisterMBean", name);
	checkDelete("unregisterMBean", name);
	super.unregisterMBean(name);
    }

    /**
     * Call <code>checkAny("getObjectInstance", name)</code>, then
     * <code>checkRead("getObjectInstance", name)</code>,
     * then forward this method to the wrapped object.
     */
    public ObjectInstance getObjectInstance(ObjectName name)
	    throws InstanceNotFoundException {
	checkAny("getObjectInstance", name);
	checkRead("getObjectInstance", name);
	return super.getObjectInstance(name);
    }

    /**
     * Call <code>checkAny("queryMBeans", name)</code>, then
     * <code>checkQuery("queryMBeans", name, query)</code>,
     * then forward this method to the wrapped object.
     */
    public Set queryMBeans(ObjectName name, QueryExp query) {
	checkAny("queryMBeans", name);
	checkQuery("queryMBeans", name, query);
	return super.queryMBeans(name, query);
    }

    /**
     * Call <code>checkAny("queryNames", name)</code>, then
     * <code>checkQuery("queryNames", name, query)</code>,
     * then forward this method to the wrapped object.
     */
    public Set queryNames(ObjectName name, QueryExp query) {
	checkAny("queryNames", name);
	checkQuery("queryNames", name, query);
	return super.queryNames(name, query);
    }

    /**
     * Call <code>checkAny("isRegistered", name)</code>, then
     * <code>checkRead("isRegistered", name)</code>,
     * then forward this method to the wrapped object.
     */
    public boolean isRegistered(ObjectName name) {
	checkAny("isRegistered", name);
	checkRead("isRegistered", name);
	return super.isRegistered(name);
    }

    /**
     * Call <code>checkAny("getMBeanCount", null)</code>, then
     * <code>checkRead("getMBeanCount", null)</code>,
     * then forward this method to the wrapped object.
     */
    public Integer getMBeanCount() {
	checkAny("getMBeanCount", null);
	checkRead("getMBeanCount", null);
	return super.getMBeanCount();
    }

    /**
     * Call <code>checkAny("getAttribute")</code>, then
     * <code>checkRead("getAttribute", name)</code>,
     * then forward this method to the wrapped object.
     */
    public Object getAttribute(ObjectName name, String attribute)
	    throws MBeanException, AttributeNotFoundException,
		   InstanceNotFoundException, ReflectionException {
	checkAny("getAttribute", name);
	checkRead("getAttribute", name);
	return super.getAttribute(name, attribute);
    }

    /**
     * Call <code>checkAny("getAttributes", name)</code>, then
     * <code>checkRead("getAttributes", name)</code>,
     * then forward this method to the wrapped object.
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
	    throws InstanceNotFoundException, ReflectionException {
	checkAny("getAttributes", name);
	checkRead("getAttributes", name);
	return super.getAttributes(name, attributes);
    }

    /**
     * Call <code>checkAny("setAttribute", name)</code>, then
     * <code>checkWrite("setAttribute", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
	    throws InstanceNotFoundException, AttributeNotFoundException,
		   InvalidAttributeValueException, MBeanException, 
		   ReflectionException {
	checkAny("setAttribute", name);
	checkWrite("setAttribute", name);
	super.setAttribute(name, attribute);
    }

    /**
     * Call <code>checkAny("setAttributes", name)</code>, then
     * <code>checkWrite("setAttributes", name)</code>,
     * then forward this method to the wrapped object.
     */
    public AttributeList setAttributes(ObjectName name,
				       AttributeList attributes)
	    throws InstanceNotFoundException, ReflectionException {
	checkAny("setAttributes", name);
	checkWrite("setAttributes", name);
	return super.setAttributes(name, attributes);
    }

    /**
     * Call <code>checkAny("invoke", name)</code>, then
     * <code>checkInvoke("invoke", name, operationName, params,
     * signature)</code>, then forward this method to the wrapped object.
     */
    public Object invoke(ObjectName name, String operationName,
			 Object params[], String signature[])
	    throws InstanceNotFoundException, MBeanException,
		   ReflectionException {
	checkAny("invoke", name);
	checkInvoke("invoke", name, operationName, params, signature);
	return super.invoke(name, operationName, params, signature);
    }
 
    /**
     * Call <code>checkAny("getDefaultDomain", null)</code>, then
     * <code>checkRead("getDefaultDomain", null)</code>,
     * then forward this method to the wrapped object.
     */
    public String getDefaultDomain() {
	checkAny("getDefaultDomain", null);
	checkRead("getDefaultDomain", null);
	return super.getDefaultDomain();
    }

    /**
     * Call <code>checkAny("getDomains", null)</code>, then
     * <code>checkRead("getDomains", null)</code>,
     * then forward this method to the wrapped object.
     */
    public String[] getDomains() {
	checkAny("getDomains", null);
	checkRead("getDomains", null);
	return super.getDomains();
    }

    /**
     * Call <code>checkAny("addNotificationListener", name)</code>, then
     * <code>checkNotification("addNotificationListener", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void addNotificationListener(ObjectName name,
					NotificationListener listener,
					NotificationFilter filter,
					Object handback)
	    throws InstanceNotFoundException {
	checkAny("addNotificationListener", name);
	checkNotification("addNotificationListener", name);
	super.addNotificationListener(name, listener, filter, handback);
    }

    /**
     * Call <code>checkAny("addNotificationListener", name)</code>, then
     * <code>checkNotification("addNotificationListener", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void addNotificationListener(ObjectName name, ObjectName listener,
					NotificationFilter filter,
					Object handback)
	    throws InstanceNotFoundException {
	checkAny("addNotificationListener", name);
	checkNotification("addNotificationListener", name);
	super.addNotificationListener(name, listener, filter, handback);
    }

    /**
     * Call <code>checkAny("removeNotificationListener", name)</code>, then
     * <code>checkNotification("removeNotificationListener", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener)
	    throws InstanceNotFoundException, ListenerNotFoundException {
	checkAny("removeNotificationListener", name);
	checkNotification("removeNotificationListener", name);
	super.removeNotificationListener(name, listener);
    }

    /**
     * Call <code>checkAny("removeNotificationListener", name)</code>, then
     * <code>checkNotification("removeNotificationListener", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
                                           NotificationListener listener,
                                           NotificationFilter filter,
                                           Object handback)
            throws InstanceNotFoundException, ListenerNotFoundException {
	checkAny("removeNotificationListener", name);
	checkNotification("removeNotificationListener", name);
	super.removeNotificationListener(name, listener, filter, handback);
    }

    /**
     * Call <code>checkAny("removeNotificationListener", name)</code>, then
     * <code>checkNotification("removeNotificationListener", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
					   ObjectName listener) 
	    throws InstanceNotFoundException, ListenerNotFoundException {
	checkAny("removeNotificationListener", name);
	checkNotification("removeNotificationListener", name);
	super.removeNotificationListener(name, listener);
    }

    /**
     * Call <code>checkAny("removeNotificationListener", name)</code>, then
     * <code>checkNotification("removeNotificationListener", name)</code>,
     * then forward this method to the wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
                                           ObjectName listener,
                                           NotificationFilter filter,
                                           Object handback) 
            throws InstanceNotFoundException, ListenerNotFoundException {
	checkAny("removeNotificationListener", name);
	checkNotification("removeNotificationListener", name);
	super.removeNotificationListener(name, listener, filter, handback);
    }

    /**
     * Call <code>checkAny("getMBeanInfo", name)</code>, then
     * <code>checkRead("getMBeanInfo", name)</code>,
     * then forward this method to the wrapped object.
     */
    public MBeanInfo getMBeanInfo(ObjectName name)
	    throws InstanceNotFoundException, IntrospectionException,
		   ReflectionException {
	checkAny("getMBeanInfo", name);
	checkRead("getMBeanInfo", name);
	return super.getMBeanInfo(name);
    }

    /**
     * Call <code>checkAny("isInstanceOf", name)</code>, then
     * <code>checkRead("isInstanceOf", name)</code>,
     * then forward this method to the wrapped object.
     */
    public boolean isInstanceOf(ObjectName name, String className)
	    throws InstanceNotFoundException {
	checkAny("isInstanceOf", name);
	checkRead("isInstanceOf", name);
	return super.isInstanceOf(name, className);
    }

    /**
     * Call <code>checkAny("deserialize", name)</code>, then
     * <code>checkDeserialize("deserialize", name)</code>,
     * then forward this method to the wrapped object.
     */
    public ObjectInputStream deserialize(ObjectName name, byte[] data)
	    throws InstanceNotFoundException, OperationsException {
	checkAny("deserialize", name);
	checkDeserialize("deserialize", name);
	return super.deserialize(name, data);
    }

    /**
     * Call <code>checkAny("deserialize", null)</code>, then
     * <code>checkDeserialize("deserialize", className)</code>,
     * then forward this method to the wrapped object.
     */
    public ObjectInputStream deserialize(String className, byte[] data)
	    throws OperationsException, ReflectionException {
	checkAny("deserialize", null);
	checkDeserialize("deserialize", className);
	return super.deserialize(className, data);
    }

    /**
     * Call <code>checkAny("deserialize", null)</code>, then
     * <code>checkDeserialize("deserialize", className)</code>,
     * then forward this method to the wrapped object.
     */
    public ObjectInputStream deserialize(String className,
					 ObjectName loaderName, byte[] data)
	    throws InstanceNotFoundException, OperationsException,
		   ReflectionException {
	checkAny("deserialize", null);
	checkDeserialize("deserialize", className);
	return super.deserialize(className, loaderName, data);
    }

    /**
     * Call <code>checkAny("getClassLoaderFor", mbeanName)</code>, then
     * <code>checkRead("getClassLoaderFor", mbeanName)</code>,
     * then forward this method to the wrapped object.
     */
    public ClassLoader getClassLoaderFor(ObjectName mbeanName) 
        throws InstanceNotFoundException {
	checkAny("getClassLoaderFor", mbeanName);
	checkRead("getClassLoaderFor", mbeanName);
        return super.getClassLoaderFor(mbeanName);
    }

    /**
     * Call <code>checkAny("getClassLoader", loaderName)</code>, then
     * <code>checkRead("getClassLoader", loaderName)</code>,
     * then forward this method to the wrapped object.
     */
    public ClassLoader getClassLoader(ObjectName loaderName)
        throws InstanceNotFoundException {
	checkAny("getClassLoader", loaderName);
	checkRead("getClassLoader", loaderName);
        return super.getClassLoader(loaderName);
    }

    /**
     * Call <code>checkAny("getClassLoaderRepository", null)</code>, then
     * <code>checkRead("getClassLoaderRepository", null)</code>,
     * then forward this method to the wrapped object.
     */
    public ClassLoaderRepository getClassLoaderRepository() {
	checkAny("getClassLoaderRepository", null);
	checkRead("getClassLoaderRepository", null);
        return super.getClassLoaderRepository();
    }

    /**
     * <p>Return the <code>OperationContext</code> associated with the
     * request being handled.</p>
     *
     * @return the <code>OperationContext</code> associated with the
     * request being handled, or null if there is none.
     */
    protected OperationContext getOperationContext()
	    throws ClassCastException {
	Object context = ThreadContext.get("OperationContext");
	if (context instanceof OperationContext)
	    return (OperationContext) context;
	return null;
    }

    /**
     * <p>Checking operation invoked by every method from the
     * <code>MBeanServer</code> interface.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name, e.g.,
     * "<code>createMBean</code>".
     * @param objectName the <code>ObjectName</code> of the MBean
     * referenced by the operation, or null if there is none.
     */
    protected void checkAny(String methodName, ObjectName objectName) {
    }

    /**
     * <p>Checking operation invoked by each of the overloaded forms of
     * <code>createMBean</code> and by <code>registerMBean</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name,
     * "<code>createMBean</code>" or "<code>registerMBean</code>".
     * @param className the <code>className</code> parameter of the
     * <code>createMBean</code> operation.
     * @param objectName the <code>name</code> parameter of the
     * <code>createMBean</code> operation.
     * @param loaderName the <code>loaderName</code> parameter of the
     * <code>createMBean</code> operation, or null for those forms of
     * the operation that do not have one.
     * @param params the <code>params</code> parameter of the
     * <code>createMBean</code> operation, or null for those forms of
     * the operation that do not have one.
     * @param signature the <code>signature</code> parameter of the
     * <code>createMBean</code> operation, or null for those forms of
     * the operation that do not have one.
     */
    protected void checkCreate(String methodName, String className,
			       ObjectName objectName, ObjectName loaderName,
			       Object[] params, String[] signature) {
    }

    /**
     * <p>Checking operation invoked by each of the overloaded forms of
     * <code>instantiate</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name,
     * "<code>instantiate</code>".
     * @param className the <code>className</code> parameter of the
     * <code>instantiate</code> operation.
     * @param loaderName the <code>loaderName</code> parameter of the
     * <code>instantiate</code> operation, or null for those forms of
     * the operation that do not have one.
     * @param params the <code>params</code> parameter of the
     * <code>instantiate</code> operation, or null for those forms of
     * the operation that do not have one.
     * @param signature the <code>signature</code> parameter of the
     * <code>instantiate</code> operation, or null for those forms of
     * the operation that do not have one.
     */
    protected void checkInstantiate(String methodName, String className,
				    ObjectName loaderName, Object[] params,
				    String[] signature) {
    }

    /**
     * <p>Checking operation invoked by <code>unregisterMBean</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name,
     * "<code>unregisterMBean</code>".
     * @param objectName the <code>name</code> parameter of the
     * <code>unregisterMBean</code> operation.
     */
    protected void checkDelete(String methodName, ObjectName objectName) {
    }

    /**
     * <p>Checking operation invoked by <code>getAttribute</code>,
     * <code>getAttributes</code>, <code>getObjectInstance</code>,
     * <code>isRegistered</code>, <code>getMBeanCount</code>,
     * <code>getDefaultDomain</code>, <code>getMBeanInfo</code>, and
     * <code>isInstanceOf</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name, e.g.,
     * "<code>getAttribute</code>".
     * @param objectName the ObjectName of the object being accessed, for those
     * operations where there is one, or null otherwise.
     */
    protected void checkRead(String methodName, ObjectName objectName) {
    }

    /**
     * <p>Checking operation invoked by <code>setAttribute</code> and
     * <code>setAttributes</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name, e.g.,
     * "<code>setAttribute</code>".
     * @param objectName the ObjectName of the object being accessed.
     */
    protected void checkWrite(String methodName, ObjectName objectName) {
    }

    /**
     * <p>Checking operation invoked by <code>queryMBeans</code> and
     * <code>queryNames</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name, e.g.,
     * "<code>queryMBeans</code>".
     * @param name the <code>name</code> parameter of the operation.
     * @param query the <code>query</code> parameter of the operation.
     */
    protected void checkQuery(String methodName, ObjectName name,
			      QueryExp query) {
    }

    /**
     * <p>Checking operation invoked by <code>invoke</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name,
     * "<code>invoke</code>".
     * @param objectName the <code>name</code> parameter of the
     * <code>invoke</code> operation.
     * @param operationName the <code>operationName</code> parameter of the
     * <code>invoke</code> operation.
     * @param params the <code>params</code> parameter of the
     * <code>invoke</code> operation.
     * @param signature the <code>signature</code> parameter of the
     * <code>invoke</code> operation.
     */
    protected void checkInvoke(String methodName, ObjectName objectName,
			       String operationName, Object[] params,
			       String[] signature) {
    }

    /**
     * <p>Checking operation invoked by each of the overloaded forms of
     * <code>addNotificationListener</code> and
     * <code>removeNotificationListener</code>.</p>
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name, e.g.,
     * "<code>addNotificationListener</code>".
     * @param objectName the <code>name</code> parameter of the operation.
     */
    protected void checkNotification(String methodName,
				     ObjectName objectName) {
    }

    /**
     * <p>Checking operation invoked by each of the overloaded forms of
     * <code>deserialize</code>.
     *
     * <p>The default implementation does nothing.  A subclass may
     * override this method to throw a <code>RuntimeException</code>
     * (or a subclass of it) if the operation is not to be
     * allowed.</p>
     *
     * @param methodName the calling method in the <code>MBeanServer</code>
     * interface.  This string is just the method name,
     * "<code>deserialize</code>".
     * @param objectNameOrClass a <code>String</code> for those forms of the
     * <code>deserialize</code> operation that have a <code>className</code>
     * parameter, or an <code>ObjectName</code> for those forms that do not.
     */
    protected void checkDeserialize(String methodName,
				    Object objectNameOrClass) {
    }

}
