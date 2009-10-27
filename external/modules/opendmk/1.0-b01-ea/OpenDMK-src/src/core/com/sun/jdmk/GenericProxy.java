/*
 * @(#)file      GenericProxy.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.29
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

// jmx import
//
import javax.management.*;

// jdmk import
//
import com.sun.jdmk.comm.RemoteMBeanServer;

/**
 * Implementation of the <CODE>GenericProxy</CODE> object associated
 * with a remote MBean.
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connectors.  This class may be removed in a future
 * version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector} and {@link
 * com.sun.jdmk.remote.cascading.proxy.CascadingProxy}.
 */
public class GenericProxy implements Proxy {

    /*
     * ------------------------------------------
     *  CONSTRUCTORS
     * ------------------------------------------
     */

    /**
     * Constructs a GenericProxy instance.
     *
     * @param instance The object instance identifying a MBean.
     */
    public GenericProxy(ObjectInstance instance) {
	setMBeanObjectInstance(instance);
    }

    /**
     * Constructs a GenericProxy instance.
     *  
     * @param instance The object instance identifying a MBean.
     * @param server The server to which this proxy is bound.
     */
    public GenericProxy(ObjectInstance instance, ProxyHandler server) {
	setMBeanObjectInstance(instance);

	this.server = server;
    }

    /*
     * ------------------------------------------
     *  PROXY IMPLEMENTATION
     * ------------------------------------------
     */

    public ObjectInstance getMBeanObjectInstance() {
	return objectInstance;
    }

    public ProxyHandler getServer() {
        return server;
    }

    public void setServer(ProxyHandler server) {
	this.server = server;
    }

    /*
     * ------------------------------------------
     *  PUBLIC METHODS
     * ------------------------------------------
     */

    /**
     * This method provides the exposed attributes and actions of the MBean.
     * It provides this information using an <CODE>MBeanInfo</CODE> object.
     *
     * @return  An instance of <CODE>MBeanInfo</CODE> allowing all attributes and actions of
     * this MBean to be retrieved.
     *
     * @exception ProxyMBeanInstantiationException The <CODE>ProxyMBean</CODE> Metadata Service could not be instantiated.
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke the introspect
     * method.
     * @exception IntrospectionException  An exception occurs during introspection.
     */
    public MBeanInfo getMBeanInfo()
	throws InstanceNotFoundException, ProxyMBeanInstantiationException, ReflectionException, IntrospectionException {

        return server.getMBeanInfo(myName);
    }

    /**
     * Gets the value of a specific attribute of an MBean.
     *
     * @param attribute The name of the attribute to be retrieved.
     *
     * @return The value of the retrieved attribute.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception AttributeNotFoundException The attribute specified is not accessible in the MBean.
     * @exception MBeanException  Wraps an exception thrown by the MBean's getter.
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException Wraps an exception thrown while trying to instantiate and apply the
     * operator specified in Modification.
     */
    public Object getAttribute(String attribute)
        throws AttributeNotFoundException, InstanceNotFoundException, MBeanException, ReflectionException {     

	return(server.getAttribute(myName, attribute));
    }

    /**
     * Gets the values of several attributes of an MBean.
     *
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return The values of the retrieved attributes.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException An exception occurred trying to invoke the getAttributes of a Dynamic MBean.
     */
    public AttributeList getAttributes(String[] attributes) throws InstanceNotFoundException,ReflectionException {

        return server.getAttributes(myName, attributes);
    }

    /**
     * Sets the value of a specific attribute of an MBean.
     *
     * @param attribute The modification to be performed: The identification of the
     * attribute to be set, the value it is to be set to, and the operator to apply.
     * The value of the attribute can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception AttributeNotFoundException The attribute specified is not accessible in the MBean.
     * @exception InvalidAttributeValueException  The value specified for the attribute is not valid.
     * @exception MBeanException Wraps an exception thrown by the MBean's setter.
     * @exception ReflectionException Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke the setter.
     */
    public void setAttribute(Attribute attribute)
	throws InstanceNotFoundException, AttributeNotFoundException, InvalidAttributeValueException,
	       MBeanException, ReflectionException {

	server.setAttribute(myName, attribute);
    }

    /**
     * Sets the values of several attributes of an MBean.
     *
     * @param attributes A list of attributes: The identification of the
     * attributes to be set and the values to which they are to be set.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @return  The list of attributes that were set, with their new values.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception ReflectionException An exception occurred trying to invoke the setAttributes of a Dynamic MBean.
     */
    public AttributeList setAttributes(AttributeList attributes) throws InstanceNotFoundException, ReflectionException {

	return(server.setAttributes(myName, attributes));
    }

    /**
     * Invokes a method of an MBean.
     *
     * @param methodName The name of the method to be invoked.
     * @param arguments An array containing the arguments to be set when the method is invoked.
     * An argument can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the method.
     *
     * @return The object returned by the invocation of the given method.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.
     * @exception MBeanException  Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke the method.
     */
    public Object invoke(String methodName, Object arguments[], String signature[])
	throws InstanceNotFoundException, MBeanException, ReflectionException {

	return(server.invoke(myName, methodName, arguments, signature));
    }

    /**
     * Sets an ObjectInstance identifying an MBean which will be represented
     * by this proxy.
     *
     * @param instance The object instance identifying a MBean.
     */
    public void setMBeanObjectInstance(ObjectInstance instance) {

	if (instance == null) {
            throw new IllegalArgumentException("Cannot set to a null ObjectInstance.");
        }

	this.objectInstance = instance;
	myName = objectInstance.getObjectName();
    }

    /*
     * ------------------------------------------
     *  PROTECTED VARIABLES
     * ------------------------------------------
     */

    private ObjectName myName = null;
    private ObjectInstance objectInstance = null;

    private ProxyHandler server = null;
}
