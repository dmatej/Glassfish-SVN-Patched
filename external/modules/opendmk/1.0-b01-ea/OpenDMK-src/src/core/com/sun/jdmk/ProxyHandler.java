/*
 * @(#)file      ProxyHandler.java
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

package com.sun.jdmk;

import javax.management.*;

import com.sun.jdmk.NotificationRegistration;


/**
 * This interface groups all functions necessary to support a
 * proxy. If both a remote and a local MBean server implement this
 * interface, it will allow a proxy based application to be run at
 * remote or local MBean server side without modifying its code.
 *
 * @deprecated MBean proxies should be generated using {@link
 * javax.management.MBeanServerInvocationHandler#newProxyInstance
 * MBeanServerInvocationHandler.newProxyInstance}.  The interface
 * <code>ProxyHandler</code> may be removed in a
 * future version of Java DMK.
 * 
 */
public interface ProxyHandler extends NotificationRegistration { 

    /**
     * Unregisters an MBean from the MBean server. The MBean is identified by
     * its object name. Once the method has been invoked, the MBean may
     * no longer be accessed by its object name.
     *
     * @param name The object name of the MBean to be unregistered.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception MBeanRegistrationException The preDeregister ((<CODE>MBeanRegistration</CODE>  interface) method of the MBean
     * has thrown an exception.
     * @exception RuntimeOperationsException Wraps a <CODE>java.lang.IllegalArgumentException</CODE>: The object name in parameter is null or 
     * the MBean you are when trying to de-register is the {@link javax.management.MBeanServerDelegate MBeanServerDelegate} MBean.
     *
     */
    public void unregisterMBean(ObjectName name)
	throws InstanceNotFoundException, MBeanRegistrationException ; 

    /**
     * Gets the value of a specific attribute of an MBean
     *
     * @param name The object name of the MBean from which the attribute is to be retrieved.
     * @param attribute A String specifying the name of the attribute to be
     * retrieved.
     *
     * @return  The value of the retrieved attribute.
     *
     * @exception AttributeNotFoundException The attribute specified is not accessible in the MBean.
     * @exception MBeanException  Wraps an exception thrown by the MBean's getter.
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException  Wraps a <CODE>java.lang.Exception</CODE> thrown when trying to invoke the setter. 
     * @exception RuntimeOperationsException Wraps a <CODE>java.lang.IllegalArgumentException</CODE>: The object name in parameter is null or 
     * the attribute in parameter is null.
     */
    public Object getAttribute(ObjectName name, String attribute)
	throws MBeanException, AttributeNotFoundException, InstanceNotFoundException, ReflectionException  ;


    /**
     * Gets a set of attributes in an MBean.
     *
     * @param name The object name of the MBean from which the attributes are
     * retrieved.
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return The list of the retrieved attributes.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException An exception occurred when trying to invoke the getAttributes method of a Dynamic MBean.
     * @exception RuntimeOperationsException Wrap a <CODE>java.lang.IllegalArgumentException</CODE>: The object name in parameter is null or 
     * attributes in parameter is null.
     *
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
        throws InstanceNotFoundException, ReflectionException ;

    /**
     * Sets a specific attribute in an MBean.
     *
     * @param name The name of the MBean within which the attribute is to be set.
     * @param attribute The identification of the attribute to be set and the value it is to be set to.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception AttributeNotFoundException The attribute specified is not accessible in the MBean.
     * @exception InvalidAttributeValueException The value specified for the attribute is not valid.
     * @exception MBeanException Wraps an exception thrown by the MBean's setter.
     * @exception ReflectionException  Wraps a <CODE>java.lang.Exception</CODE> thrown when trying to invoke the setter. 
     * @exception RuntimeOperationsException Wraps a <CODE>java.lang.IllegalArgumentException</CODE>: The object name in parameter is null or 
     * the attribute in parameter is null.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
    	throws InstanceNotFoundException, AttributeNotFoundException, InvalidAttributeValueException, MBeanException, ReflectionException;


    /**
     * Sets a set of attributes in an MBean.
     *
     * @param name The object name of the MBean within which the attributes are to
     * be set.
     * @param attributes A list of attributes: The identification of the
     * attributes to be set and  the values they are to be set to.
     *
     * @return  The list of attributes that were set, with their new values.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException An exception occurred when trying to invoke the getAttributes method of a Dynamic MBean.
     * @exception RuntimeOperationsException Wraps a <CODE>java.lang.IllegalArgumentException</CODE>: The object name in parameter is null or 
     * attributes in parameter is null.
     *
     */
    public AttributeList setAttributes(ObjectName name, AttributeList attributes)
        throws InstanceNotFoundException, ReflectionException  ;

    /**
     * Invokes an operation on an MBean.
     *
     * @param name The object name of the MBean on which the method is to be invoked.
     * @param operationName The name of the operation to be invoked.
     * @param params An array containing the parameters to be set when the operation is
     * invoked
     * @param signature An array containing the signature of the operation. The class objects will
     * be loaded using the same class loader as the one used for loading the MBean on which the operation was invoked.
     *
     * @return  The object returned by the operation, which represents the result of invoking the operation on the 
     * MBean specified.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception MBeanException  Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke the method.
     *
     */
    public Object invoke(ObjectName name, String operationName, Object params[], String signature[])
	throws InstanceNotFoundException, MBeanException, ReflectionException ;
 

    /**
     * Discovers the attributes and operations that an MBean exposes
     * for management.
     *
     * @param name The object name of the MBean
     *
     * @return  A <CODE>MBeanInfo</CODE> instance which contains all attributes and operations
     * exposed by the MBean.
     *
     * @exception IntrospectionException An exception occurs during introspection.
     * @exception InstanceNotFoundException The MBean specified is not found.
     * @exception ReflectionException An exception occurred when trying to invoke the getMBeanInfo of a Dynamic MBean.
     */
    public MBeanInfo getMBeanInfo(ObjectName name)
	throws InstanceNotFoundException, IntrospectionException, ReflectionException ;
 


}
