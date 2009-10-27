/* 
 * @(#)file      CompatibleMBeanInterceptor.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.8 
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
import java.util.HashSet;
import java.util.Iterator;
import java.util.IdentityHashMap;

import javax.management.*;
import com.sun.jdmk.MBeanInterceptor;

/**
 * <p>An {@link MBeanServerInterceptor} that forwards every request
 * unchanged to an {@link MBeanInterceptor}.  This class is useful
 * to transform an MBeanInterceptor into an MBeanServerInterceptor.
 *
 * @since Java DMK 5.1
 */
public class CompatibleMBeanInterceptor 
    implements MBeanServerInterceptor {
    /**
     * <p>Create a new <code>CompatibleMBeanInterceptor</code>
     * that forwards its requests to a {@link MBeanInterceptor}.
     */
    public CompatibleMBeanInterceptor(MBeanInterceptor inter) {
	this.next = inter;
    }

    /**
     * <p>Return the {@link MBeanInterceptor} to which requests will
     * be forwarded.</p>
     *
     * @return The wrapped {@link MBeanInterceptor} in the chain. 
     */
    public MBeanInterceptor getNextInterceptor() {
	return next;
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
	if (next instanceof MBeanServerInterceptor) {
	    return ((MBeanServerInterceptor)next).getDomains();
	}
	final Set names = next.queryNames(null,null);
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
	if (next instanceof MBeanServerInterceptor) {
	    ((MBeanServerInterceptor)next).
		removeNotificationListener(name, listener, filter, handback);
	} else {
	    final String txt = "Operation not supported by interceptor.";
	    final RuntimeException rtx = 
		new UnsupportedOperationException(txt);
	    throw new RuntimeOperationsException(rtx,txt);
	}
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
	if (next instanceof MBeanServerInterceptor) {
	    ((MBeanServerInterceptor)next).
		removeNotificationListener(name, listener, filter, handback);
	} else {
	    final String txt = "Operation not supported by interceptor.";
	    final RuntimeException rtx = 
		new UnsupportedOperationException(txt);
	    throw new RuntimeOperationsException(rtx,txt);
	}
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
	if (next instanceof MBeanServerInterceptor) {
	    return ((MBeanServerInterceptor)next).getClassLoaderFor(mbeanName);
	} else {
	    return next.getMBeanClassLoader(mbeanName);
	}
    }

    public ClassLoader getClassLoader(ObjectName loaderName)
	throws InstanceNotFoundException {
	if (next instanceof MBeanServerInterceptor) {
	    return ((MBeanServerInterceptor)next).getClassLoader(loaderName);
	} else {
	    throw new InstanceNotFoundException("No such Class Loader");
	}
    }

    public final ClassLoader getMBeanClassLoader(ObjectName name) 
	throws InstanceNotFoundException {
	return getClassLoaderFor(name);
    }

    private MBeanInterceptor next;
}
