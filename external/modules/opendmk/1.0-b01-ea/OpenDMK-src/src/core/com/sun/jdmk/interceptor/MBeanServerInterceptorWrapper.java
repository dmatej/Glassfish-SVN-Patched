/*
 * @(#)file      MBeanServerInterceptorWrapper.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.6
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

// JMX import
import javax.management.*; 


/**
 * Implements a MBeanServerInterceptorWrapper that receives all requests from
 * the MBeanServer and forward them to a wrapped MBeanServerInterceptor.
 * <p>
 * If the MBeanServerInterceptorWrapper is instantiated with a non null
 * {@link com.sun.jdmk.interceptor.MBeanServerInterceptorWrapper.Controller} 
 * object, then <i>beginOperation</i> is called on the controller before any
 * operation is invoked on the wrapped interceptor, and <i>endOperation</i>
 * is called on the controller when the operation invoked on the wrapped
 * interceptor is completed.
 * <p>
 * The controller object can be used, for instance, to determine how
 * many operations are currently active on the wrapped interceptor.
 * @see com.sun.jdmk.interceptor.MBeanServerInterceptorWrapper.Controller
 *
 * @since Java DMK 5.0
 */

public class MBeanServerInterceptorWrapper implements MBeanServerInterceptor {

    /**
     * This interface is called before and after each operation is invoked
     * on the wrapped interceptor.
     * @see com.sun.jdmk.interceptor.MBeanServerInterceptorWrapper
     *
     * @since Java DMK 5.0
     **/
    public interface Controller {
	/**
	 * This constant indicates the type of operation.
	 * <ul>
	 * <li>CREATE for <i>createMBean</i>,</li>
	 * <li>REGISTER for <i>registerMBean</i>,</li>
	 * <li>UNREGISTER for <i>unregisterMBean</i>,</li>
	 * <li>GET for <i>getAttribute</i> and <i>getAttributes</i>,</li>
	 * <li>SET for <i>setAttribute</i> and <i>setAttributes</i>,</li>
	 * <li>INVOKE for <i>invoke</i>,</li>
	 * <li>QUERY for <i>queryNames</i> and <i>queryMBeans</i>,</li>
	 * <li>LISTEN for <i>addNotificationListener</i> and
	 *            <i>removeNotificationListener</i>,</li>
	 * <li>INFO for the remaining operations (<i>getMBeanInfo</i> etc...).
	 * </li></ul>
	 **/
	final static int CREATE = 0, REGISTER=1, UNREGISTER=2, 
	    GET=3, SET=4, INVOKE=5, QUERY=6, LISTEN=7, INFO=8;
	
	/**
	 * This method is called before invoking the operation on the
	 * wrapped interceptor. 
	 *
	 * @param operationType Type of the operation invoked.
	 * @param operationName Name of the operation invoked through the 
	 *        MBeanServerInterceptor interface. When the operation is 
	 *        <i>invoke</i>, <code>operationName</code> corresponds to 
	 *        the real operation name as passed to the <i>invoke</i> 
	 *        method.
	 * @param mbeanName Name of the MBean on which the operation is 
	 *        invoked. This name can be <code>null</code> for those
	 *        operations that do not apply to any specific MBean
	 *        (like <i>getMBeanCount</i>).
	 * @return A handle that will be passed to <i>endOperation</i>
	 *         when the operation invoked on the wrapped interceptor
	 *         is completed. This handle can be null if it is not 
	 *         needed.
	 *
	 * @exception JMRuntimeException if a runtime exception is thrown,
	 *         the operation is aborted and <i>endOperation</i> is not
	 *         called.<br>
	 *         This method is not expected to throw any RuntimeException
	 *         but JMRuntimeExceptions. However this rule is not enforced.
	 *         The results caused by throwing anything but 
	 *         JMRuntimeException are undefined and MBeanServer 
	 *         implementation specific.
	 **/
	public Object beginOperation(final int operationType, 
				     final String operationName,
				     final ObjectName mbeanName)
	    throws javax.management.JMRuntimeException;


	/**
	 * This method is called when the operation invoked on the
	 * wrapped interceptor is completed. 
	 * <p>
	 * This method is called whatever the result of the invoked
	 * operation was, even if an exception was thrown. The only case
	 * where <i>endOperation</i> is not called is when 
	 * <i>beginOperation</i> threw an exception.
	 * <p>
	 * This method is not expected to throw any RuntimeException:
	 * however this rule is not enforced. The results caused by 
         * throwing a RuntimeException are undefined and MBeanServer 
	 * implementation specific.
	 *
	 * @param handle The handle returned by <i>beginOperation</i>
	 * @param operationType Type of the operation invoked.
	 * @param operationName Name of the operation invoked through the 
	 *        MBeanServerInterceptor interface. When the operation is 
	 *        <i>invoke</i>, <code>operationName</code> corresponds to 
	 *        the real operation name as passed to the <i>invoke</i> 
	 *        method.
	 * @param mbeanName Name of the MBean on which the operation is 
	 *        invoked. This name can be <code>null</code> for those
	 *        operations that do not apply to any specific MBean
	 *        (like <i>getMBeanCount</i>).
	 *
	 **/
	public void endOperation(Object handle, final int operationType, 
				 final String operationName,
				 final ObjectName mbeanName);
    }

    final MBeanServerInterceptor  interceptor;
    final Controller              controller;

    /**
     * Construct a new MBeanServerInterceptorWrapper.
     * @param interceptor   The wrapped interceptor.
     * @param controller The (possibly null) controller object on which 
     *        <i>beginOperation</i> and <i>endOperation</i> should be
     *        called. If <code>controller==null</code>, then
     *        <i>beginOperation</i> and <i>endOperation</i> are not called.
     **/
    public MBeanServerInterceptorWrapper(final MBeanServerInterceptor  interceptor,
				   final Controller controller) {
	this.interceptor   = interceptor;
	this.controller = controller;
	checkInitialization();
    }
   
    /**
     * Check that this MBeanServerInterceptorWrapper is correctly initialized.
     * @exception IllegalArgumentException if the wrapped MBeanServerInterceptor
     *            is null.
     **/
    protected void checkInitialization() 
	throws IllegalArgumentException {
	if (interceptor == null) 
	    throw new IllegalArgumentException("Bad initialization: " +
			    "Wrapped MBeanServerInterceptor must not be null.");
    }

    public final ObjectInstance createMBean(final String className, 
					    final ObjectName name, 
					    final Object params[], 
					    final String signature[]) 
        throws ReflectionException, InstanceAlreadyExistsException, 
	MBeanRegistrationException, MBeanException, 
	NotCompliantMBeanException {
	
	final Object handle = beginOperation(Controller.CREATE,"createMBean",
					     name);
	try {
	    return interceptor.createMBean(className, name, 
					     params, signature);
	} finally {
	    endOperation(handle,Controller.CREATE,"createMBean",name);
	}
    }


    public final ObjectInstance createMBean(final String className, 
				      final ObjectName name, 
				      final ObjectName loaderName, 
				      final Object params[], 
				      final String signature[]) 
        throws ReflectionException, InstanceAlreadyExistsException,
	MBeanRegistrationException, MBeanException, 
	NotCompliantMBeanException, InstanceNotFoundException {

	final Object handle = beginOperation(Controller.CREATE,"createMBean",
					     name);
	try {
	    return interceptor.createMBean(className, name, loaderName, 
					params, signature);
	} finally {
	    endOperation(handle,Controller.CREATE,"createMBean",name);
	}
    }


 
    public final ObjectInstance getObjectInstance(final ObjectName name) 
	throws InstanceNotFoundException {

	final Object handle = beginOperation(Controller.INFO,
					     "getObjectInstance", name);
	try {
	    return interceptor.getObjectInstance(name);
	} finally {
	    endOperation(handle, Controller.INFO,
			 "getObjectInstance",name);
	}
    }

   
    public final Set queryMBeans(final ObjectName name, final QueryExp query) {

	final Object handle = beginOperation(Controller.QUERY,"queryMBeans",
					     name);
	try {
	    return interceptor.queryMBeans(name, query);
	} finally {
	    endOperation(handle,Controller.QUERY,"queryMBeans",name);
	}
    }


 
    public final Set queryNames(final ObjectName name, final QueryExp query) {

	final Object handle = beginOperation(Controller.QUERY,"queryNames",
					     name);
	try {
	    return interceptor.queryNames(name, query);
	} finally {
	    endOperation(handle,Controller.QUERY,"queryNames",name);
	}
    }


   
    public String[] getDomains() {

	final Object handle = 
	    beginOperation(Controller.INFO,"getDomains",null);
	try {
	    return interceptor.getDomains();
	} finally {
	    endOperation(handle,Controller.INFO,"getDomains",null);
	}
    }


    public final String getDefaultDomain()  {

	final Object handle = 
	    beginOperation(Controller.INFO,"getDefaultDomain",null);
	try {
	    return interceptor.getDefaultDomain();
	} finally {
	    endOperation(handle,Controller.INFO,"getDefaultDomain",null);
	}
    }

    public final Integer getMBeanCount()  {

	final Object handle = 
	    beginOperation(Controller.INFO,"getMBeanCount",null);
	try {
	    return interceptor.getMBeanCount();
	} finally {
	    endOperation(handle,Controller.INFO,"getMBeanCount",null);
	}
    }
    
 
    public final boolean isRegistered(final ObjectName name) {

	final Object handle = 
	    beginOperation(Controller.INFO,"isRegistered",name);
	try {
	    return interceptor.isRegistered(name);
	} finally {
	    endOperation(handle,Controller.INFO,"isRegistered",name);
	}
    }



    public final boolean isInstanceOf(final ObjectName name, 
				      final String className) 
	throws InstanceNotFoundException {

	final Object handle = 
	    beginOperation(Controller.INFO,"isInstanceOf",name);
	try {
	    return interceptor.isInstanceOf(name, className);
	} finally {
	    endOperation(handle,Controller.INFO,"isInstanceOf",name);
	}
    }

  
    public final ObjectInstance registerMBean(final Object object, 
					      final ObjectName name)
	throws InstanceAlreadyExistsException,
	       MBeanRegistrationException, NotCompliantMBeanException {

	final Object handle = 
	    beginOperation(Controller.REGISTER,"registerMBean",name);
	try {
	    return interceptor.registerMBean(object, name);
	} finally {
	    endOperation(handle,Controller.REGISTER,"registerMBean",name);
	}
    } 

  
    public final void addNotificationListener(final ObjectName name, 
					final NotificationListener listener, 
					final NotificationFilter filter, 
					final Object handback)
	throws InstanceNotFoundException {

	final Object handle = 
	    beginOperation(Controller.LISTEN,"addNotificationListener",name);
	try {
	    interceptor.addNotificationListener(name, listener, filter,
						  handback);
	} finally {
	    endOperation(handle,Controller.LISTEN,"addNotificationListener",
			 name);
	}
    }

    public final void addNotificationListener(final ObjectName name, 
					final ObjectName listener, 
					final NotificationFilter filter, 
					final Object handback)
        throws InstanceNotFoundException {

	final Object handle = 
	    beginOperation(Controller.LISTEN,"addNotificationListener",name);
	try {
	    interceptor.addNotificationListener(name, listener, filter, 
						  handback);
	} finally {
	    endOperation(handle,Controller.LISTEN,"addNotificationListener",
			 name);
	}
    }

 
    public final void removeNotificationListener(final ObjectName name, 
				     final NotificationListener listener) 
        throws InstanceNotFoundException, ListenerNotFoundException {

	final Object handle = 
	  beginOperation(Controller.LISTEN,"removeNotificationListener",name);
	try {
	    interceptor.removeNotificationListener(name, listener);
	} finally {
	    endOperation(handle,Controller.LISTEN,"removeNotificationListener",
			 name);
	}
    }


    public final void removeNotificationListener(final ObjectName name, 
					   final ObjectName listener) 
       throws InstanceNotFoundException, ListenerNotFoundException {

	final Object handle = 
	  beginOperation(Controller.LISTEN,"removeNotificationListener",name);
	try {
	    interceptor.removeNotificationListener(name, listener);
	} finally {
	    endOperation(handle,Controller.LISTEN,"removeNotificationListener",
			 name);
	}
    }

    public void removeNotificationListener(ObjectName name,
					   ObjectName listener,
					   NotificationFilter filter,
					   Object handback)
	throws InstanceNotFoundException, ListenerNotFoundException {
	final Object handle = 
	  beginOperation(Controller.LISTEN,"removeNotificationListener",name);
	try {
	    interceptor.removeNotificationListener(name,listener,filter,
						   handback);
	} finally {
	    endOperation(handle,Controller.LISTEN,"removeNotificationListener",
			 name);
	}
    }


    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener,
					   NotificationFilter filter,
					   Object handback)
	    throws InstanceNotFoundException, ListenerNotFoundException {
	final Object handle = 
	  beginOperation(Controller.LISTEN,"removeNotificationListener",name);
	try {
	    interceptor.removeNotificationListener(name,listener,filter,
						   handback);
	} finally {
	    endOperation(handle,Controller.LISTEN,"removeNotificationListener",
			 name);
	}
    }

    public final void unregisterMBean(final ObjectName name)
	throws InstanceNotFoundException, MBeanRegistrationException {

	final Object handle = 
	    beginOperation(Controller.UNREGISTER,"unregisterMBean",name);
	try {
	    interceptor.unregisterMBean(name);
	} finally {
	    endOperation(handle,Controller.UNREGISTER,"unregisterMBean",name);
	}
    }


    public final Object getAttribute(final ObjectName name, 
				     final String attribute)
	throws MBeanException, AttributeNotFoundException, 
	InstanceNotFoundException, ReflectionException {

	final Object handle = 
	    beginOperation(Controller.GET,"getAttribute",name);
	try {
	    return interceptor.getAttribute(name, attribute);
	} finally {
	    endOperation(handle,Controller.GET,"getAttribute",name);
	}
    }


    public final AttributeList getAttributes(final ObjectName name, 
					     final String[] attributes)
        throws InstanceNotFoundException, ReflectionException {
	
	final Object handle = 
	    beginOperation(Controller.GET,"getAttributes",name);
	try {
	    return interceptor.getAttributes(name, attributes);
	} finally {
	    endOperation(handle,Controller.GET,"getAttributes",name);
	}
    }

 
    public final void setAttribute(final ObjectName name, 
				   final Attribute attribute)
    	throws InstanceNotFoundException, AttributeNotFoundException,
	InvalidAttributeValueException, MBeanException, ReflectionException {

	final Object handle = 
	    beginOperation(Controller.SET,"setAttribute",name);
	try {
	    interceptor.setAttribute(name, attribute);
	} finally {
	    endOperation(handle,Controller.SET,"setAttribute",name);
	}
    }

    public final AttributeList setAttributes(final ObjectName name, 
					     final AttributeList attributes)
        throws InstanceNotFoundException, ReflectionException  {

	final Object handle = 
	    beginOperation(Controller.SET,"setAttributes",name);
	try {
	    return interceptor.setAttributes(name, attributes);
	} finally {
	    endOperation(handle,Controller.SET,"setAttributes",name);
	}
    }

    public final Object invoke(final ObjectName name, 
			       final String operationName, 
			       final Object params[], 
			       final String signature[])
	throws InstanceNotFoundException, MBeanException, 
	ReflectionException {

	final Object handle = 
	    beginOperation(Controller.INVOKE,operationName,name);
	try {
	    return interceptor.invoke(name, operationName, params, signature);
	} finally {
	    endOperation(handle,Controller.INVOKE,operationName,name);
	}
    }
 

    public final MBeanInfo getMBeanInfo(final ObjectName name)
	throws InstanceNotFoundException, IntrospectionException, 
	ReflectionException   {

	final Object handle = 
	    beginOperation(Controller.INFO,"getMBeanInfo",name);
	try {
	    return interceptor.getMBeanInfo(name);
	} finally {
	    endOperation(handle,Controller.INFO,"getMBeanInfo",name);
	}
    }

    public final ClassLoader getMBeanClassLoader(final ObjectName name) 
	throws InstanceNotFoundException {
	return getClassLoaderFor(name);
    }

    public final ClassLoader getClassLoaderFor(ObjectName mbeanName)
	throws InstanceNotFoundException {

	final Object handle = 
	    beginOperation(Controller.INFO,"getClassLoaderFor",mbeanName);
	try {
	    return interceptor.getClassLoaderFor(mbeanName);
	} finally {
	    endOperation(handle,Controller.INFO,"getClassLoaderFor",mbeanName);
	}
    }


    public final ClassLoader getClassLoader(ObjectName loaderName)
	throws InstanceNotFoundException {

	final Object handle = 
	    beginOperation(Controller.INFO,"getClassLoader",loaderName);
	try {
	    return interceptor.getClassLoader(loaderName);
	} finally {
	    endOperation(handle,Controller.INFO,"getClassLoader",loaderName);
	}
    }


    /**
     * This method is called each time a method from the MBeanServerInterceptor
     * interface is invoked, just before forwarding the call to the
     * request dispatcher.
     **/
    private final Object beginOperation(final int operationType,
					final String operationName,
					final ObjectName name) 
	throws javax.management.JMRuntimeException {
	if (controller == null) return null;
	return controller.beginOperation(operationType,operationName,
					 name);
    }

    /**
     * This method is called each time a method from the MBeanServerInterceptor
     * interface is invoked, just before returning.
     **/
     private final void endOperation(final Object handle, 
				     final int operationType,
				     final String operationName,
				     final ObjectName name) {
	 if (controller == null) return;
	 controller.endOperation(handle,operationType,operationName,
				 name);
     }

}
