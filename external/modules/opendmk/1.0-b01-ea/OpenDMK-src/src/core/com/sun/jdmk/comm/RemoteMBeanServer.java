/*
 * @(#)file      RemoteMBeanServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.37
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

package com.sun.jdmk.comm;

//
// RI import
//
import javax.management.* ;

//
// jdmk import
//
import com.sun.jdmk.* ;

/**
 * Defines the methods necessary for remote creation, access and
 * deletion of MBeans. It creates local objects
 * (<CODE>ProxyMBeans</CODE> and <CODE>GenericProxies</CODE>) to be
 * used as "proxies" on the MBeans.
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This interface may be removed
 * in a future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 */
public interface RemoteMBeanServer extends ProxyHandler, ClientNotificationHandler {

    /**
     * ---------------------------------
     * Communication handling
     * ---------------------------------
     */

    /**
     * Initializes the communication with the remote <CODE>MBeanServer</CODE>. All the information
     * needed for identifying the <CODE>MBeanServer</CODE> to contact and the protocol to
     * be used is contained in the object of type <CODE>ConnectorAddress</CODE>, passed as
     * a parameter. If a communication problem occurs this method will throw
     * a <CODE>CommunicationException</CODE> (<CODE>JMRuntimeException</CODE>).
     * If the <CODE>RemoteMBeanServer</CODE> had already been connected and disconnected with an
     * <CODE>MBeanServer</CODE> identified by its <CODE>MBeanServerId</CODE>, and if the
     * <CODE>MBeanServer</CODE> reachable by the MBeanServerAddress parameter doesn't
     * have the same <CODE>MBeanServerId</CODE>, the <CODE>java.lang.IllegalAccessException</CODE> is thrown.
     *
     * @param MBeanServerAddress The exact <CODE>MBeanServer</CODE> address to contact
     * (<CODE>MBeanServer</CODE> identification, protocol specification).
     *
     * @exception IllegalArgumentException The <CODE>RemoteMBeanServer</CODE> has already been connected and disconnected
     * and the specified <CODE>ConnectorAddress</CODE> doesn't identify the same <CODE>MBeanServer</CODE>.
     *
     * @return A String identifying the <CODE>MBeanServer</CODE> with which the communication is established.
     *
     */
    public String connect(ConnectorAddress MBeanServerAddress);

    /**
     * Terminates the communication with the <CODE>MBeanServer</CODE>.
     */
    public void disconnect();

    /**
     * Checks whether communication with the <CODE>MBeanServer</CODE> is established.
     * 
     * @return True, if the communication is established, otherwise false.
     */
    public boolean isConnected();

    /**
     * Returns the exact address of the <CODE>MBeanServer</CODE> to which the ConnectorClient is
     * connected. The address is of type <CODE>ConnectorAddress</CODE>.
     *
     * @return The exact address of the remote MBeanServer, or null if the ConnectorClient is
     * not connected.
     */
    public ConnectorAddress getMBeanServerAddress();

    /**
     * <p>Set the OperationContext of this connection.  This context will be
     * sent along with each request and can be recovered by the server,
     * which can make it available to the operations it invokes.</p>
     *
     * <p>The saved OperationContext will be a clone of the object
     * <code>c</code> made using its <code>clone</code> method.</p>
     *
     * @param c the new OperationContext.  It may be null to indicate that
     * there is no context.  The previous OperationContext, if any, is lost.
     */
    public void setOperationContext(OperationContext c);

    /**
     * Get the OperationContext that was last given to setOperationContext,
     * or null if setOperationContext was never called.
     * @return the current OperationContext.
     */
    public OperationContext getOperationContext();

    /**
     * Returns a string which represents the <CODE>MBeanServer</CODE> identification.
     * This String comes from the <CODE>MBeanServerDelegate</CODE> Mbean.
     *
     * @return If the Connector Client has not been connected yet, it returns null. If the
     * connector Client has been connected and disconnected, <CODE>getMbeanServerId</CODE> still returns
     * the previous <CODE>MbeanServer</CODE> identification.
     */
    public String getMBeanServerId();

    /**
     * ---------------------------------------------------------
     * MBean creation and registration operations
     * ---------------------------------------------------------
     */

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It
     * returns an <CODE>ObjectInstance</CODE> representing the remote MBean created. 
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating the MBean.
     * @param name The name of the MBean to be created.
     *
     * @return An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the <CODE>java.lang.Exception</CODE> that occurred when trying
     * to invoke the MBean's constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server. 
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     */
    public ObjectInstance createMBean(String className, ObjectName name)
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	       MBeanException, NotCompliantMBeanException;

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns  an <CODE>ObjectInstance</CODE> representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating the MBean.
     * @param name The name of the MBean to be created.
     * @param loaderName The name of the class loader to be used by the <CODE>MBeanServer</CODE>.
     *
     * @return An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the <CODE>java.lang.Exception</CODE> that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     * @exception InstanceNotFoundException The specified loader is not registered in the <CODE>MBeanServer</CODE>
     */
    public ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName)
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	       MBeanException, NotCompliantMBeanException, InstanceNotFoundException;

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It
     * returns an <CODE>ObjectInstance</CODE> representing the remote MBean created. 
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating
     * the MBean.
     * @param name The name of the MBean to be created.
     * @param params An array containing the parameters of the constructor to be invoked.
     * A parameter can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     * @return An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the <CODE>java.lang.Exception</CODE> that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     */
    public ObjectInstance createMBean(String className, ObjectName name, Object params[], String signature[])
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	       MBeanException, NotCompliantMBeanException;

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns an <CODE>ObjectInstance</CODE> representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the <CODE>MBeanServer</CODE> for creating
     * the MBean.
     * @param name The name of the MBean to be created.
     * @param loaderName The name of the class loader to be used by the <CODE>MBeanServer</CODE>.
     * @param params An array containing the parameters of the constructor to be invoked.
     * A parameter can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     * @return An <CODE>ObjectInstance</CODE> representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the <CODE>java.lang.Exception</CODE> that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException The MBean is already under the control of the MBean server.
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> (<CODE>MBeanRegistration</CODE>
     * interface) method of the MBean has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not a JMX compliant MBean.
     * @exception InstanceNotFoundException The specified loader is not registered in the <CODE>MBeanServer</CODE>.
     */
    public ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName,  Object params[], String signature[])
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	       MBeanException, NotCompliantMBeanException, InstanceNotFoundException;

    /**
     * ---------------------------------------------------------
     * ProxyMBean/GenericProxy operations
     * ---------------------------------------------------------
     */

    /**
     * ---------------------------------------------------------
     * MBean unregistration operations
     * ---------------------------------------------------------
     */

    /**
     * Deletes an instance of an MBean in the remote MBean server.
     * It also removes its local proxy (<CODE>ProxyMBean</CODE> and/or <CODE>GenericProxy</CODE>) object from 
     * remote MBean server.
     *
     * @param name The name of the MBean to be deleted.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.  
     * @exception MBeanRegistrationException The <CODE>preDeregister</CODE> (<CODE>MBeanRegistration</CODE>
     *  interface) method of the MBean has thrown an exception.
     */
    public void unregisterMBean(ObjectName name) throws InstanceNotFoundException, MBeanRegistrationException;

    /**
     * ---------------------------------------------------------
     * Query operations
     * ---------------------------------------------------------
     */

    /**
     * Gets the names of MBeans controlled by the <CODE>MBeanServer</CODE>. This method
     * allows any of the following to be obtained: The names of all MBeans,
     * the names of a set of MBeans specified by pattern matching on the
     * <CODE>ObjectName</CODE> and/or a Query expression, a specific MBean name (equivalent
     * to testing whether an MBean is registered). When the object name is
     * null or empty, all the objects are to be selected (and filtered if
     * a query is specified). It returns the set of <CODE>ObjectName</CODE>s for the
     * MBeans selected.
     *
     * @param name The object name pattern identifying the MBean names to be retrieved. If
     * null or empty, the names of all the registered MBeans will be retrieved.
     *
     * @param query The query expression to be applied for selecting MBeans.
     *
     * @return A set containing the <CODE>ObjectNames</CODE> for the MBeans selected.
     */
    public java.util.Set queryNames(ObjectName name, QueryExp query);

    /**
     * Gets MBeans controlled by the <CODE>MBeanServer</CODE>. This method allows any
     * of the following to be obtained: All MBeans, a set of MBeans specified
     * by pattern matching on the ObjectName and/or a Query expression, a
     * specific MBean. When the object name is null or empty, all objects are
     * to be selected (and filtered if a query is specified). It returns the
     * set of <CODE>ObjectInstance</CODE>s for the selected MBeans.
     *
     * @param name The object name pattern identifying the MBeans to be retrieved. If
     * null or empty all the MBeans registered will be retrieved.
     * @param query The query expression to be applied for selecting MBeans.
     *
     * @return A set containing the <CODE>ObjectInstance</CODE>s for the MBeans selected.
     */
    public java.util.Set queryMBeans(ObjectName name, QueryExp query);

    /**
     * ---------------------------------------------------------
     * Management operations on MBean
     * ---------------------------------------------------------
     */


    /**
     * Checks whether an MBean, identified by its object name, is already registered
     * with the MBeanServer.
     *   
     * @param name The object name of the MBean to be checked.
     *   
     * @return  True if the MBean is already registered in the MBeanServer, false otherwise.
     *   
     * @exception RuntimeOperationsException Wraps an IllegalArgumentException: The object name in parameter is null.
     *   
     */
    public boolean isRegistered(ObjectName name) ;

    /**
     * Gets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The object name of the MBean from which the attribute is to be retrieved.
     *
     * @param attribute The name of the attribute to be retrieved.
     *
     * @return The value of the retrieved attribute.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception AttributeNotFoundException The specified attribute is not accessible in the MBean.
     * @exception MBeanException  Wraps an exception thrown by the MBean's getter.
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception ReflectionException Wraps an exception thrown while trying to instantiate and apply the
     * operator specified in Modification.
     */
    public Object getAttribute(ObjectName name, String attribute)
	throws MBeanException, AttributeNotFoundException, InstanceNotFoundException, ReflectionException;

    /**
     * Allows you to retrieve the values of several attributes of an MBean.
     *
     * @param name The object name of the MBean from within which the attributes are
     * to be retrieved.
     *
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return The values of the attributes retrieved.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception ReflectionException An exception occurred trying to invoke the getAttributes of a Dynamic MBean.
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
	throws InstanceNotFoundException, ReflectionException;

    /**
     * Sets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The name of the MBean within which the attribute is to be set.
     *
     * @param attribute The modification to be performed: The identification of the
     * attribute to be set, the value it is to be set to, and the operator to apply.
     * The value of the attribute can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception AttributeNotFoundException The specified attribute is not accessible in the MBean.
     * @exception InvalidAttributeValueException The specified value for the attribute is not valid.
     * @exception MBeanException Wraps an exception thrown by the MBean's setter.
     * @exception ReflectionException Wraps an exception thrown while trying to instantiate and apply the
     * operator specified in Modification.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
	throws InstanceNotFoundException, AttributeNotFoundException, InvalidAttributeValueException,
	       MBeanException, ReflectionException;

    /**
     * Allows you to modify the values of several attributes of an MBean.
     *
     * @param name The object name of the MBean from within which the attributes are
     * to be set.
     * @param attributes A list of the attributes to be set, their values and, optionally, the
     * operators to apply.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @return The values of the attributes that were set.
     * The value of the attributes can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception ReflectionException An exception occurred trying to invoke the setAttributes of a Dynamic MBean.
     */
    public AttributeList setAttributes(ObjectName name, AttributeList attributes)
	throws InstanceNotFoundException, ReflectionException;

    /**
     * Invokes a method of an MBean.
     *
     * @param name The name of the MBean on which the method is to be invoked.
     * @param operationName The name of the operation to be invoked.
     * @param params An array containing the parameters to be set when the operation is
     * invoked.
     * A parameter can be any Java object that is <CODE>serializable</CODE>.
     * @param signature An array containing the signature of the method.
     *
     * @return The object returned by the invocation of the given method.
     * The return value can be any Java object that is <CODE>serializable</CODE>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception MBeanException  Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke the method.
     */
    public Object invoke(ObjectName name, String operationName, Object params[], String signature[])
	throws InstanceNotFoundException, MBeanException, ReflectionException;

    /**
     * This method supplies the exposed attributes and actions of the MBean.
     * It provides this information using an <CODE>MBeanInfo</CODE> object.
     *
     * @param name The name of the MBean whose attributes and actions will be returned.
     *
     * @return An instance of <CODE>MBeanInfo</CODE> which allows all methods and actions of
     * this MBean to be retrieved.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBean server.
     * @exception IntrospectionException An exception occurs during introspection.
     * @exception ReflectionException Wraps a <CODE>java.lang.Exception</CODE> thrown while trying to invoke
     * the <CODE>getMBeanInfo</CODE> method.
     */   
    public MBeanInfo getMBeanInfo(ObjectName name)
	throws InstanceNotFoundException, IntrospectionException, ReflectionException;

    /**
     * Gets the ObjectInstance for a given MBean registered with the MBeanServer.
     *
     * @param name The object name of the MBean.
     *
     * @return The ObjectInstance associated to the MBean specified by <VAR>name</VAR>.
     *
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBeanServer.
     */
    public ObjectInstance getObjectInstance(ObjectName name) throws InstanceNotFoundException;

    /**
     * Returns the number of MBeans controlled by the <CODE>MBeanServer</CODE>.
     */
     public Integer getMBeanCount();

    /**
     * Returns the default domain used for the MBean naming.
     */
    public String getDefaultDomain();


    /** Returns true if the MBean specified is an instance of the specified class, false otherwise.
     * 
     * @param name The <CODE>ObjectName</CODE> of the MBean.
     * @param className The name of the class.
     *
     * @return true if the MBean specified is an instance of the specified class, false otherwise.
     *
     * @exception InstanceNotFoundException The MBean specified is not registered in the MBean server.          
     */
    public boolean isInstanceOf(ObjectName name, String className) throws InstanceNotFoundException;


    /**
     * ---------------------------------------------------------
     * Local operations
     * ---------------------------------------------------------
     */

    /**
     * Given the object name and the Java class name of the MBean(<CODE>ObjectInstance</CODE>), this
     * method returns the name of the Java class of the corresponding <CODE>ProxyMBean</CODE>.
     * The returned name can be null, if there is no Java class corresponding to
     * the <CODE>ProxyMBean</CODE> needed.
     *
     * @param instance The <CODE>ObjectInstance</CODE> of the MBean which is represented by the
     *  <CODE>ProxyMBean</CODE>.
     *
     * @return The name of the Java class of the  <CODE>ProxyMBean</CODE>.
     * @exception ProxyMBeanInstantiationException
     */
    public String getClassForProxyMBean(ObjectInstance instance)
	throws ProxyMBeanInstantiationException;

}
