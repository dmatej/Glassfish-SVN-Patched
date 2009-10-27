/* 
 * @(#)file      ForwardingMBeanServerInterceptor.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.11 
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
import java.util.IdentityHashMap;

import javax.management.*;
import com.sun.jdmk.JdmkMBeanServer;

/**
 * <p>An {@link MBeanServerInterceptor} that forwards every request
 * unchanged to another {@link MBeanServerInterceptor}.  This class is
 * useful as a superclass for interceptors that override some its
 * methods and perform some action before forwarding to the chained
 * interceptor.  Subclasses forward methods such as {@link
 * #getAttribute} by calling <code>super.getAttribute(...)</code>.
 * They may of course choose not to forward if appropriate.</p>
 *
 * <p>Any number of <code>ForwardingMBeanServerInterceptor</code>
 * objects may be chained together on the path from the MBean server
 * to its default {@link MBeanServerInterceptor}.  This class provides
 * convenience methods to manipulate such a chain.</p>
 *
 * <p>The same <code>ForwardingMBeanServerInterceptor</code> instance
 * should not be inserted into more than one MBean server at a
 * time.</p>
 */
public abstract class ForwardingMBeanServerInterceptor implements MBeanServerInterceptor {
    /**
     * <p>Create a new <code>ForwardingMBeanServerInterceptor</code>
     * that initially forwards its requests to nobody.  {@link
     * #setNextInterceptor} should be called before the interceptor is
     * inserted into an MBean server.  Otherwise, {@link
     * NullPointerException} will be produced for every operation on
     * that MBean server.</p>
     */
    protected ForwardingMBeanServerInterceptor() {
    }

    /**
     * <p>Set the {@link MBeanServerInterceptor} to which requests will be
     * forwarded.</p>
     *
     * @param next The {@link MBeanServerInterceptor} that will become next
     * in the chain.  If there was already a chained {@link
     * MBeanServerInterceptor}, it is forgotten.
     *
     * @exception NullPointerException if <code>next</code> is null.
     *
     * @exception InterceptorCycleException if <code>next</code> is
     * the first in a chain of
     * <code>ForwardingMBeanServerInterceptor</code> leading to
     * <code>this</code>.
     */
    public void setNextInterceptor(MBeanServerInterceptor next)
	    throws NullPointerException, InterceptorCycleException {
	if (next == null)
	    throw new NullPointerException("Null interceptor");

	if (next instanceof ForwardingMBeanServerInterceptor) {
	    IdentityHashMap seen = new IdentityHashMap();
	    MBeanServerInterceptor mbsi = next;
	    while (mbsi instanceof ForwardingMBeanServerInterceptor) {
		if (seen.containsKey(mbsi))
		    throw new InterceptorCycleException("Would make cycle");
		seen.put(mbsi, mbsi);
		ForwardingMBeanServerInterceptor f =
		     (ForwardingMBeanServerInterceptor) mbsi;
		mbsi = f.getNextInterceptor();
	    }
	}

	this.next = next;
    }

    /**
     * <p>Return the {@link MBeanServerInterceptor} to which requests will
     * be forwarded.</p>
     *
     * @return The next {@link MBeanServerInterceptor} in the chain.  This
     * is the value that was most recently given to the {@link
     * #setNextInterceptor} method, or null if that method has never
     * been called on this object.
     */
    public MBeanServerInterceptor getNextInterceptor() {
	return next;
    }

    /**
     * <p>Insert this <code>ForwardingMBeanServerInterceptor</code> at
     * the head of the chain of interceptors in a {@link
     * com.sun.jdmk.JdmkMBeanServer}.  Requests will be forwarded to 
     * the {@link MBeanServerInterceptor} that was previously the default
     * interceptor for <code>lmbs</code>.</p>
     *
     * @param lmbs the MBean server where this interceptor is to be
     * inserted.
     *
     * @exception NullPointerException if <code>lmbs</code> is null.
     *
     * @exception InterceptorCycleException if <code>this</code> is
     * already part of a chain of
     * <code>ForwardingMBeanServerInterceptor</code> objects starting
     * from the default interceptor for <code>lmbs</code>.  Attempting
     * to insert it at the head would then produce a cycle of
     * interceptors.
     */
    public void insertFirst(JdmkMBeanServer lmbs)
	    throws NullPointerException, InterceptorCycleException {
	synchronized (lmbs) {
	    MBeanServerInterceptor inter = lmbs.getMBeanServerInterceptor();
	    setNextInterceptor(inter);
	    lmbs.setMBeanServerInterceptor(this);
	}
    }

    /**
     * <p>Insert this <code>ForwardMBeanServerInterceptor</code> at
     * the tail of the chain of interceptors in a {@link
     * com.sun.jdmk.JdmkMBeanServer}.  
     * A chain of interceptors is defined here as
     * a list of {@link ForwardingMBeanServerInterceptor} objects,
     * starting with <code>lmbs.</code>{@link
     * com.sun.jdmk.JdmkMBeanServer#getMBeanServerInterceptor()
     * getMBeanServerInterceptor()}, where each one is linked
     * to the next.  The chain ends with a {@link
     * ForwardingMBeanServerInterceptor}, <em>f</em>, whose next
     * interceptor, <em>n</em> is not a {@link
     * ForwardingMBeanServerInterceptor}, including the case where
     * <em>n</em> is null.  This {@link
     * ForwardingMBeanServerInterceptor} is inserted between
     * <em>f</em> and <em>n</em>.  So on return from this method,<br>
     * <em>f</em>.{@link #getNextInterceptor()} <code>== this</code>,
     * and<br> <code>this</code>.{@link #getNextInterceptor()}
     * <code>==</code> <em>n</em>.</p>
     *
     * <p>The chain may be empty, in which case this method is
     * equivalent to {@link #insertFirst insertFirst(lmbs)}.</p>
     *
     * @param lmbs the MBean server where this interceptor is to be
     * inserted.
     *
     * @exception NullPointerException if <code>lmbs</code> is null.
     *
     * @exception InterceptorCycleException if <code>this</code> is
     * already part of a chain of
     * <code>ForwardingMBeanServerInterceptor</code> objects starting
     * from the default interceptor for <code>lmbs</code>.  Attempting
     * to insert it at the head would then produce a cycle of
     * interceptors.
     */
    public void insertLast(JdmkMBeanServer lmbs)
	    throws NullPointerException, InterceptorCycleException {
	synchronized (lmbs) {
	    MBeanServerInterceptor inter = lmbs.getMBeanServerInterceptor();

	    if (!(inter instanceof ForwardingMBeanServerInterceptor)) {
		insertFirst(lmbs);
		return;
	    }

	    ForwardingMBeanServerInterceptor finter =
		(ForwardingMBeanServerInterceptor) inter;

	    IdentityHashMap seen = new IdentityHashMap();
	    while (true) {
		inter = finter.getNextInterceptor();
		if (!(inter instanceof ForwardingMBeanServerInterceptor))
		    break;
		/* The check in setNextInterceptor should mean that we
		   can't have a cycle in the existing interceptors.  But
		   one could arise in unusual circumstances, for instance
		   after deleting an interceptor in the chain that was
		   not a ForwardingMBeanServerInterceptor.  The spec
		   doesn't say we have to check this, but not doing so
		   puts us in an infinite loop, which is not very helpful.  */
		if (seen.containsKey(inter))
		    throw new InterceptorCycleException("Existing cycle!");
		seen.put(inter, inter);
		finter = (ForwardingMBeanServerInterceptor) inter;
	    }

	    setNextInterceptor(inter);
	    finter.setNextInterceptor(this);
	}
    }

    /**
     * <p>Remove this <code>ForwardingMBeanServerInterceptor</code>
     * from the chain of interceptors in a 
     * {@link com.sun.jdmk.JdmkMBeanServer}.
     * A chain of interceptors is defined here as a list of {@link
     * ForwardingMBeanServerInterceptor} objects, starting with
     * <code>lmbs.</code>{@link
     * com.sun.jdmk.JdmkMBeanServer#getMBeanServerInterceptor()
     * getMBeanServerInterceptor()}, where each one is linked to the
     * next.  The chain ends with a {@link
     * ForwardingMBeanServerInterceptor}, <em>f</em>, whose next
     * interceptor, <em>n</em> is not a {@link
     * ForwardingMBeanServerInterceptor}, including the case where
     * <em>n</em> is null.</p>
     *
     * <p>If <code>lmbs.</code>{@link
     * com.sun.jdmk.JdmkMBeanServer#getMBeanServerInterceptor()
     * getMBeanServerInterceptor()} <code>== this</code>, then on
     * return from this method, <code>lmbs.</code>{@link
     * com.sun.jdmk.JdmkMBeanServer#getMBeanServerInterceptor()
     * getMBeanServerInterceptor()} <code>== this.{@link
     * #getNextInterceptor()}</code>.</p>
     *
     * <p>Otherwise, let <em>p</em> be the {@link
     * ForwardingMBeanServerInterceptor} in the chain where
     * <em>p</em>.{@link #getNextInterceptor()} <code>== this</code>.
     * (If there is no such interceptor, {@link
     * InterceptorNotPresentException} is thrown.  There cannot be
     * more than one such interceptor since that would imply a cycle,
     * which we forbid.)  On return from this method,
     * <em>p</em>.{@link #getNextInterceptor()} <code>==
     * this.</code>{@link #getNextInterceptor()}.</p>
     *
     * @param lmbs the MBean server where this interceptor is to be
     * removed.
     *
     * @exception InterceptorNotPresentException if this {@link
     * ForwardingMBeanServerInterceptor} is not in the chain of
     * interceptors for <code>lmbs</code>.
     *
     * @exception InterceptorCycleException if a cycle is detected
     * while looking for this interceptor in the chain.
     */
    public void remove(JdmkMBeanServer lmbs)
	    throws InterceptorNotPresentException, InterceptorCycleException {
	final String msg = "Interceptor not in JdmkMBeanServer chain";

	synchronized (lmbs) {
	    MBeanServerInterceptor inter = lmbs.getMBeanServerInterceptor();
	    if (!(inter instanceof ForwardingMBeanServerInterceptor))
		throw new InterceptorNotPresentException(msg);

	    ForwardingMBeanServerInterceptor finter =
		(ForwardingMBeanServerInterceptor) inter;
	    if (finter == this) {
		lmbs.setMBeanServerInterceptor(getNextInterceptor());
		return;
	    }

	    IdentityHashMap seen = new IdentityHashMap();
	    while (true) {
		inter = finter.getNextInterceptor();
		if (!(inter instanceof ForwardingMBeanServerInterceptor))
		    throw new InterceptorNotPresentException(msg);
		if (inter == this)
		    break;
		/* See insertLast for explanation of following check.  */
		if (seen.containsKey(inter))
		    throw new InterceptorCycleException("Existing cycle!");
		seen.put(inter, inter);
		finter = (ForwardingMBeanServerInterceptor) inter;
	    }

	    finter.setNextInterceptor(getNextInterceptor());
	}
    }

    public ObjectInstance createMBean(String className, ObjectName name,
				      Object params[], String signature[]) 
	    throws ReflectionException, InstanceAlreadyExistsException,
	    	   MBeanRegistrationException, MBeanException,
		   NotCompliantMBeanException {
	return next.createMBean(className, name, params, signature);
    }

    public ObjectInstance createMBean(String className, ObjectName name,
				      ObjectName loaderName, Object params[],
				      String signature[]) 
	    throws ReflectionException, InstanceAlreadyExistsException,
	    	   MBeanRegistrationException, MBeanException,
	    	   NotCompliantMBeanException, InstanceNotFoundException {
	return next.createMBean(className, name, loaderName, params,
				signature);
    }

    public ObjectInstance registerMBean(Object object, ObjectName name)
	    throws InstanceAlreadyExistsException, MBeanRegistrationException,
		   NotCompliantMBeanException {
	return next.registerMBean(object, name);
    }

    public void unregisterMBean(ObjectName name)
	    throws InstanceNotFoundException, MBeanRegistrationException {
	next.unregisterMBean(name);
    }

    public ObjectInstance getObjectInstance(ObjectName name)
	    throws InstanceNotFoundException {
	return next.getObjectInstance(name);
    }

    public Set queryMBeans(ObjectName name, QueryExp query) {
	return next.queryMBeans(name, query);
    }

    public Set queryNames(ObjectName name, QueryExp query) {
	return next.queryNames(name, query);
    }

    public boolean isRegistered(ObjectName name) {
	return next.isRegistered(name);
    }

    public Integer getMBeanCount() {
	return next.getMBeanCount();
    }

    public Object getAttribute(ObjectName name, String attribute)
	    throws MBeanException, AttributeNotFoundException,
	    	   InstanceNotFoundException, ReflectionException {
	return next.getAttribute(name, attribute);
    }

    public AttributeList getAttributes(ObjectName name, String[] attributes)
	    throws InstanceNotFoundException, ReflectionException {
	return next.getAttributes(name, attributes);
    }

    public void setAttribute(ObjectName name, Attribute attribute)
	    throws InstanceNotFoundException, AttributeNotFoundException,
		   InvalidAttributeValueException, MBeanException, 
		   ReflectionException {
	next.setAttribute(name, attribute);
    }

    public AttributeList setAttributes(ObjectName name,
				       AttributeList attributes)
	    throws InstanceNotFoundException, ReflectionException {
	return next.setAttributes(name, attributes);
    }

    public Object invoke(ObjectName name, String operationName,
			 Object params[], String signature[])
	    throws InstanceNotFoundException, MBeanException,
		   ReflectionException {
	return next.invoke(name, operationName, params, signature);
    }
 
    public String getDefaultDomain() {
	return next.getDefaultDomain();
    }

    public String[] getDomains() {
	return next.getDomains();
    }

    public void addNotificationListener(ObjectName name,
					NotificationListener listener,
					NotificationFilter filter,
					Object handback)
	    throws InstanceNotFoundException {
	next.addNotificationListener(name, listener, filter, handback);
    }

    public void addNotificationListener(ObjectName name,
					ObjectName listener,
					NotificationFilter filter,
					Object handback)
	    throws InstanceNotFoundException {
	next.addNotificationListener(name, listener, filter, handback);
    }

    public void removeNotificationListener(ObjectName name,
					   ObjectName listener) 
	    throws InstanceNotFoundException, ListenerNotFoundException {
	next.removeNotificationListener(name, listener);
    }

    public void removeNotificationListener(ObjectName name,
					   ObjectName listener,
					   NotificationFilter filter,
					   Object handback)
	    throws InstanceNotFoundException, ListenerNotFoundException {
	next.removeNotificationListener(name, listener, filter, handback);
    }

    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener)
	    throws InstanceNotFoundException, ListenerNotFoundException {
	next.removeNotificationListener(name, listener);
    }

    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener,
					   NotificationFilter filter,
					   Object handback)
	    throws InstanceNotFoundException, ListenerNotFoundException {
	next.removeNotificationListener(name, listener, filter, handback);
    }

    public MBeanInfo getMBeanInfo(ObjectName name)
	    throws InstanceNotFoundException, IntrospectionException,
	    	   ReflectionException {
	return next.getMBeanInfo(name);
    }

    public boolean isInstanceOf(ObjectName name, String className)
	    throws InstanceNotFoundException {
	return next.isInstanceOf(name, className);
    }

    public ClassLoader getClassLoaderFor(ObjectName mbeanName)
	throws InstanceNotFoundException {
	return next.getClassLoaderFor(mbeanName);
    }

    public ClassLoader getClassLoader(ObjectName loaderName)
	throws InstanceNotFoundException {
	return next.getClassLoader(loaderName);
    }

    public final ClassLoader getMBeanClassLoader(ObjectName name) 
	throws InstanceNotFoundException {
	return getClassLoaderFor(name);
    }

    private MBeanServerInterceptor next;
}
