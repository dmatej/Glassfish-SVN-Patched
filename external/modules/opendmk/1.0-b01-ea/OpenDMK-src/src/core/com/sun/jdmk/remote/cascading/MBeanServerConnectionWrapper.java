/*
 * @(#)file      MBeanServerConnectionWrapper.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.8
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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Set;
import java.util.HashSet;
import java.lang.reflect.UndeclaredThrowableException;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceNotFoundException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.IntrospectionException;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.OperationsException;
import javax.management.QueryExp;
import javax.management.ReflectionException;
import javax.management.loading.ClassLoaderRepository;
import javax.management.remote.MBeanServerForwarder;

import com.sun.jdmk.defaults.Utils;

/**
 * <p>An object of this class implements the MBeanServer interface
 * and, for each of its methods forwards the request to a wrapped 
 * {@link MBeanServerConnection} object. 
 * Some methods of the {@link MBeanServer} interface do not have
 * any equivalent in {@link MBeanServerConnection}. In that case, an
 * {@link UnsupportedOperationException} will be thrown.
 *
 * <p>A typical use of this class is to apply a QueryExp object locally,
 * on an MBean that resides in a remote MBeanServer. Since an 
 * MBeanServerConnection is not an MBeanServer, it cannot be passed
 * to the <code>setMBeanServer()</code> method of the {@link QueryExp}
 * object. However, this object can.</p>
 *
 * <p>This is an abstract class, and although only 
 * {@link #getMBeanServerConnection()} is declared abstract, implementing
 * this single method will usually not be sufficient in order to
 * substitute an instance of this class to an
 * MBeanServer in the general case: for instance, the methods like
 * {@link #getClassLoader}, {@link #getClassLoaderFor} etc... which 
 * are not part of the {@link MBeanServerConnection} interface will
 * throw an {@link UnsupportedOperationException} in their
 * default implementation. Therefore,
 * it would not be appropriate to pass an instance of this class
 * to an object that expect these methods to be implemented, unless those
 * methods have been appropriately implemented in a subclass.</p>
 *
 * @since Java DMK 5.1
 */
public abstract class MBeanServerConnectionWrapper
	implements MBeanServer {

    /**
     * Returns an MBeanServerConnection. This method is called each time
     * an operation must be invoked on the underlying MBeanServerConnection.
     **/
    protected abstract MBeanServerConnection getMBeanServerConnection() 
	throws IOException;
    
    /**
     * This method is called each time an IOException is raised when 
     * trying to forward an operation to the underlying 
     * MBeanServerConnection, as a result of calling 
     * {@link #getMBeanServerConnection()} or as a result of invoking the
     * operation on the returned connection.
     * Subclasses may redefine this method if they need to perform any 
     * specific handling of IOException (logging etc...).
     * @param x The raised IOException.
     * @param method The name of the method in which the exception was
     *        raised. This is one of the methods of the MBeanServer
     *        interface.
     * @return A RuntimeException that should be thrown by the caller.
     *         In this default implementation, this is an
     *         {@link UndeclaredThrowableException} wrapping <var>x</var>.
     **/
    protected RuntimeException handleIOException(IOException x, 
						 String method) {
	final RuntimeException r = new UndeclaredThrowableException(x);
	return r;
    }
    
    // Take care of getMBeanServerConnection returning null.
    //
    private synchronized MBeanServerConnection connection() 
	throws IOException {
	final MBeanServerConnection c = getMBeanServerConnection();
	if (c == null) 
	    throw new IOException("MBeanServerConnection unavailable");
	return c;
    }
	
    //--------------------------------------------
    //--------------------------------------------
    //
    // Implementation of the MBeanServer interface
    //
    //--------------------------------------------
    //--------------------------------------------

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void addNotificationListener(ObjectName name,
					NotificationListener listener,
					NotificationFilter filter,
					Object handback)
	throws InstanceNotFoundException {
	try {
	    connection().addNotificationListener(name, listener,
						 filter, handback);
	} catch (IOException x) {
	    throw handleIOException(x,"addNotificationListener");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void addNotificationListener(ObjectName name,
					ObjectName listener,
					NotificationFilter filter,
					Object handback)
	throws InstanceNotFoundException {
	try {
	    connection().addNotificationListener(name, listener,
						 filter, handback);
	} catch (IOException x) {
	    throw handleIOException(x,"addNotificationListener");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public ObjectInstance createMBean(String className, ObjectName name)
	throws
	ReflectionException,
	InstanceAlreadyExistsException,
	MBeanRegistrationException,
	MBeanException,
	NotCompliantMBeanException {
	try {	
	    return connection().createMBean(className, name);
	} catch (IOException x) {
	    throw handleIOException(x,"createMBean");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public ObjectInstance createMBean(String className, ObjectName name,
				      Object params[], String signature[])
	throws
	ReflectionException,
	InstanceAlreadyExistsException,
	MBeanRegistrationException,
	MBeanException,
	NotCompliantMBeanException {
	try {
	    return connection().createMBean(className, name,
					    params, signature);
	} catch (IOException x) {
	    throw handleIOException(x,"createMBean");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public ObjectInstance createMBean(String className,
				      ObjectName name,
				      ObjectName loaderName)
	throws
	ReflectionException,
	InstanceAlreadyExistsException,
	MBeanRegistrationException,
	MBeanException,
	NotCompliantMBeanException,
	InstanceNotFoundException {
	try {
	    return connection().createMBean(className, name, loaderName);
	} catch (IOException x) {
	    throw handleIOException(x,"createMBean");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public ObjectInstance createMBean(String className,
				      ObjectName name,
				      ObjectName loaderName,
				      Object params[],
				      String signature[])
	throws
	ReflectionException,
	InstanceAlreadyExistsException,
	MBeanRegistrationException,
	MBeanException,
	NotCompliantMBeanException,
	InstanceNotFoundException {
	try {
	    return connection().createMBean(className, name, loaderName,
					    params, signature);
	} catch (IOException x) {
	    throw handleIOException(x,"createMBean");
	}
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public ObjectInputStream deserialize(ObjectName name, byte[] data)
	throws InstanceNotFoundException, OperationsException {
	throw new UnsupportedOperationException("deserialize");
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public ObjectInputStream deserialize(String className, byte[] data)
	throws OperationsException, ReflectionException {
	throw new UnsupportedOperationException("deserialize");
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public ObjectInputStream deserialize(String className,
					 ObjectName loaderName,
					 byte[] data)
	throws
	InstanceNotFoundException,
	OperationsException,
	ReflectionException {
	throw new UnsupportedOperationException("deserialize");
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public Object getAttribute(ObjectName name, String attribute)
	throws
	MBeanException,
	AttributeNotFoundException,
	InstanceNotFoundException,
	ReflectionException {
	try {
	    return connection().getAttribute(name, attribute);
	} catch (IOException x) {
	    throw handleIOException(x,"getAttribute");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
	throws InstanceNotFoundException, ReflectionException {
	try {
	    return connection().getAttributes(name, attributes);
	} catch (IOException x) {
	    throw handleIOException(x,"getAttributes");
	}
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public ClassLoader getClassLoader(ObjectName loaderName)
	throws InstanceNotFoundException {
	throw new UnsupportedOperationException("getClassLoader");
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public ClassLoader getClassLoaderFor(ObjectName mbeanName)
	throws InstanceNotFoundException {
	throw new UnsupportedOperationException("getClassLoaderFor");
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public ClassLoaderRepository getClassLoaderRepository() {
	throw new UnsupportedOperationException("getClassLoaderRepository");
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public String getDefaultDomain() {
	try {
	    return connection().getDefaultDomain();
	} catch (IOException x) {
	    throw handleIOException(x,"getDefaultDomain");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public String[] getDomains() {
	try {
	    return connection().getDomains();
	} catch (IOException x) {
	    throw handleIOException(x,"getDomains");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public Integer getMBeanCount() {
	try {
	    return connection().getMBeanCount();
	} catch (IOException x) {
	    throw handleIOException(x,"getMBeanCount");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public MBeanInfo getMBeanInfo(ObjectName name)
	throws
	InstanceNotFoundException,
	IntrospectionException,
	ReflectionException {
	try {
	    return connection().getMBeanInfo(name);
	} catch (IOException x) {
	    throw handleIOException(x,"getMBeanInfo");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public ObjectInstance getObjectInstance(ObjectName name)
	throws InstanceNotFoundException {
	try {
	    return connection().getObjectInstance(name);
	} catch (IOException x) {
	    throw handleIOException(x,"getObjectInstance");
	}
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public Object instantiate(String className)
	throws ReflectionException, MBeanException {
 	throw new UnsupportedOperationException("instantiate");
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public Object instantiate(String className,
			      Object params[],
			      String signature[]) 
	throws ReflectionException, MBeanException {
 	throw new UnsupportedOperationException("instantiate");
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public Object instantiate(String className, ObjectName loaderName)
	throws ReflectionException, MBeanException, 
	       InstanceNotFoundException {
 	throw new UnsupportedOperationException("instantiate");
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public Object instantiate(String className, ObjectName loaderName,
			      Object params[], String signature[])
	throws ReflectionException, MBeanException, 
	       InstanceNotFoundException {
 	throw new UnsupportedOperationException("instantiate");
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public Object invoke(ObjectName name, String operationName,
			 Object params[], String signature[])
	throws
	InstanceNotFoundException,
	MBeanException,
	ReflectionException {
	try {
	    return connection().invoke(name,operationName,params,signature);
	} catch (IOException x) {
	    throw handleIOException(x,"invoke");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public boolean isInstanceOf(ObjectName name, String className)
	throws InstanceNotFoundException {
	try {
	    return connection().isInstanceOf(name, className);
	} catch (IOException x) {
	    throw handleIOException(x,"isInstanceOf");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public boolean isRegistered(ObjectName name) {
	try {
	    return connection().isRegistered(name);
	} catch (IOException x) {
	    throw handleIOException(x,"isRegistered");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     * If an IOException is raised, returns an empty Set.
     */
    public Set queryMBeans(ObjectName name, QueryExp query) {
	try {
	    return connection().queryMBeans(name, query);
	} catch (IOException x) {
	    handleIOException(x,"queryMBeans");
	    return new HashSet();
	}
    }

    /**
     * Forward this method to the
     * wrapped object. 
     * If an IOException is raised, returns an empty Set.
     */
    public Set queryNames(ObjectName name, QueryExp query) {
	try {
	    return connection().queryNames(name, query);
	} catch (IOException x) {
	    handleIOException(x,"queryNames");
	    return new HashSet();
	}
    }

    /**
     * Throws an {@link UnsupportedOperationException}. This behavior can
     * be changed by subclasses.
     */
    public ObjectInstance registerMBean(Object object, ObjectName name)
	throws
	InstanceAlreadyExistsException,
	MBeanRegistrationException,
	NotCompliantMBeanException {
	throw new UnsupportedOperationException("registerMBean");
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener)
	throws InstanceNotFoundException, ListenerNotFoundException {
	try {
	    connection().removeNotificationListener(name, listener);
	} catch (IOException x) {
	    throw handleIOException(x,"removeNotificationListener");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener,
					   NotificationFilter filter,
					   Object handback)
	throws InstanceNotFoundException, ListenerNotFoundException {
	try {
	    connection().removeNotificationListener(name, listener,
						    filter, handback);
	} catch (IOException x) {
	    throw handleIOException(x,"removeNotificationListener");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
					   ObjectName listener)
	throws InstanceNotFoundException, ListenerNotFoundException {
	try {
	    connection().removeNotificationListener(name, listener);
	} catch (IOException x) {
	    throw handleIOException(x,"removeNotificationListener");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void removeNotificationListener(ObjectName name,
					   ObjectName listener,
					   NotificationFilter filter,
					   Object handback)
	throws InstanceNotFoundException, ListenerNotFoundException {
	try {
	    connection().removeNotificationListener(name, listener,
						    filter, handback);
	} catch (IOException x) {
	    throw handleIOException(x,"removeNotificationListener");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
	throws
	InstanceNotFoundException,
	AttributeNotFoundException,
	InvalidAttributeValueException,
	MBeanException,
	ReflectionException {
	try {
	    connection().setAttribute(name, attribute);
	} catch (IOException x) {
	    throw handleIOException(x,"setAttribute");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public AttributeList setAttributes(ObjectName name,
				       AttributeList attributes)
	throws InstanceNotFoundException, ReflectionException {
	try {
	    return connection().setAttributes(name, attributes);
	} catch (IOException x) {
	    throw handleIOException(x,"setAttributes");
	}
    }

    /**
     * Forward this method to the
     * wrapped object.
     */
    public void unregisterMBean(ObjectName name)
	throws InstanceNotFoundException, MBeanRegistrationException {
	try {
	    connection().unregisterMBean(name);
	} catch (IOException x) {
	    throw handleIOException(x,"unregisterMBean");
	}
    }

    //----------------
    // PRIVATE METHODS
    //----------------

}
