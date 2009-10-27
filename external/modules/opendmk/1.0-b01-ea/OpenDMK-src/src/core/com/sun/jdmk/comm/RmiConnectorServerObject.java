/*
 * @(#)file      RmiConnectorServerObject.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.18
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
 *
 */

package com.sun.jdmk.comm;

// java import
//
import java.rmi.Remote;
import java.rmi.RemoteException;

// jmx import
//
import javax.management.*;

/**
 * The <CODE>RmiConnectorServerObject</CODE> interface defines the remote
 * method invocation (RMI) interface to be used by the RMI implementation of
 * {@link com.sun.jdmk.comm.RemoteMBeanServer} class.
 *
 */

interface RmiConnectorServerObject extends Remote {

    /**
     * ---------------------------------------------------------
     * MBean creation and registration operations
     * ---------------------------------------------------------
     */

    /**
     * Creates an registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It
     * returns an ObjectInstance representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating the MBean.
     *
     * @param name The name of the MBean to be created.
     *
     * @return  An ObjectInstance representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceAlreadyExistsException
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     */

    public ObjectInstance createMBean(String className, ObjectName name)
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	MBeanException, NotCompliantMBeanException, RemoteException ;

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns  an ObjectInstance representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating the MBean.
     *
     * @param name The name of the MBean to be created.
     *
     * @param loaderName The name of the class loader to be used by the MBeanServer.
     *
     * @return  An ObjectInstance representing the newly created MBean.
     *
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceAlreadyExistsException
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     * @exception InstanceNotFoundException The specified loader is not registered in the MBeanServer
     */
    public ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName)
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	MBeanException, NotCompliantMBeanException, InstanceNotFoundException, RemoteException ;

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. It   
     * returns an ObjectInstance representing the remote MBean created. 
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating
     * the MBean. 
     *
     * @param name The name of the MBean to be created.
     *
     * @param params An array containing the parameters of the constructor to be invoked.
     *
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     *
     * @return  An ObjectInstance representing the newly created MBean.
     *
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     */
 
    public ObjectInstance createMBean(String className, ObjectName name, Object params[], String signature[])
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	MBeanException, NotCompliantMBeanException, RemoteException ;

    /**
     * Creates and registers an instance of an MBean in the remote object server. When
     * calling the method, you have to provide the class name of the Java
     * implementation to be used for instantiating the new object. You can
     * optionally provide the name of the class loader to be used. It
     * returns an ObjectInstance representing the remote MBean created.
     *
     * @param className The name of the Java class to be used by the MBeanServer for creating
     * the MBean.
     *
     * @param name The name of the MBean to be created.
     *
     * @param loaderName The name of the class loader to be used by the MBeanServer.
     *
     * @param params An array containing the parameters of the constructor to be invoked.
     *
     * @param signature An array containing the signature of the constructor to be invoked.
     *
     *
     * @return  An ObjectInstance representing the newly created MBean.
     *
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception ReflectionException Wraps the java.lang.Exception that occurred trying to invoke the MBean's
     * constructor.
     * @exception InstanceAlreadyExistsException
     * @exception MBeanRegistrationException The preRegister (MBeanRegistration interface) method of the MBean
     * has thrown an exception. The MBean will not be registered.
     * @exception MBeanException  Wraps an exception thrown by the MBean's constructor.
     * @exception NotCompliantMBeanException This class is not an JMX compliant MBean
     * @exception InstanceNotFoundException The specified loader is not registered in the MBeanServer
     */
    public ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName, Object params[], String signature[])
	throws ReflectionException, InstanceAlreadyExistsException, MBeanRegistrationException,
	MBeanException, NotCompliantMBeanException, InstanceNotFoundException, RemoteException ;

    /**  
     * ---------------------------------------------------------
     * MBean unregistration operations
     * ---------------------------------------------------------
     */

    /**
     * Deletes an instance of an MBean in the remote MBean server.
     *
     * @param name The name of the MBean to be deleted.
     *
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceNotFoundException*
     * @exception MBeanRegistrationException The preDeregister (MBeanRegistration interface) method of the MBean
     * has thrown an exception.
     */
    public void unregisterMBean(ObjectName name) throws InstanceNotFoundException, MBeanRegistrationException, RemoteException ;

    /**
     * ---------------------------------------------------------
     * ProxyMBean/GenericProxy creation operations
     * ---------------------------------------------------------
     */

    /**
     * Gets the ObjectInstance for a given MBean registered with the MBeanServer.
     *   
     * @param name The object name of the MBean.
     *   
     * @return The ObjectInstance associated to the MBean specified by <VAR>name</VAR>.
     *   
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBeanServer.
     */  
    public ObjectInstance getObjectInstance(ObjectName name) throws InstanceNotFoundException, RemoteException ;

    /**
     * Gets MBeans controlled by the MBeanServer. This method allows any
     * of the following to be obtained: All MBeans, a set of MBeans specified
     * by pattern matching on the ObjectName and/or a Query expression, a
     * specific MBean. When the object name is null or empty, all objects are
     * to be selected (and filtered if a query is specified). It returns the 
     * set of ObjectInstance objects (containing the ObjectName and the Java Class name) for 
     * the selected MBeans. 
     *   
     * @param name The object name pattern identifying the MBeans to be retrieved. If 
     * null or empty all the MBeans registered will be retrieved. 
     * @param query The query expression to be applied for selecting MBeans. 
     *    
     * @return  A set containing the ObjectInstance objects for the selected MBeans. 
     * If no MBean satisfies the query an empty list is returned. 
     *    
     * @exception RemoteException See java.rmi.RemoteException.
     */ 
    public java.util.Set queryMBeans(ObjectName name, QueryExp query) throws RemoteException ;

    /**
     * ---------------------------------------------------------
     * Miscelleneous operations
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
     * @exception RemoteException See java.rmi.RemoteException.
     */
    public boolean isRegistered(ObjectName name) throws RemoteException ;

    /**
     * Gets the names of MBeans controlled by the MBeanServer. This method
     * allows any of the following to be obtained: The names of all MBeans,
     * the names of a set of MBeans specified by pattern matching on the
     * ObjectName and/or a Query expression, a specific MBean name (equivalent to
     * testing whether an MBean is registered). When the object name is
     * null or empty, all the objects are to be selected (and filtered if
     * a query is specified). It returns the set of ObjectNames for the
     * MBeans selected.
     * 
     * @param name The object name pattern identifying the MBean names to be retrieved. If
     * null or empty, the names of all the registered MBeans will be retrieved.
     * 
     *
     * @param query The query expression to be applied for selecting MBeans.
     * 
     * @return  A set containing the ObjectNames for the MBeans selected.
     * 
     * @exception RemoteException See java.rmi.RemoteException.
     */
    public java.util.Set queryNames(ObjectName name, QueryExp query) throws RemoteException ;

    /**   
     * Returns the number of MBeans controlled by the MBeanServer. 
     * 
     * @exception RemoteException See java.rmi.RemoteException.
     */   
    public Integer getMBeanCount() throws RemoteException ;

    /**
     * Returns the default domain used for the MBean naming.
     * 
     * @exception RemoteException See java.rmi.RemoteException.
     */
    public String getDefaultDomain()throws RemoteException;

    /**
     * ---------------------------------------------------------
     * Management operations on MBean
     * ---------------------------------------------------------
     */

    /**  
     * Gets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *   
     * @param name The object name of the MBean from which the attribute is to be retrieved.
     * @param attribute A String specifying the name of the attribute to be
     * retrieved.
     *   
     * @return  The value of the retrieved attribute.
     *   
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception AttributeNotFoundException The specified attribute is not accessible in the MBean.
     * @exception MBeanException  Wraps an exception thrown by the MBean's getter.
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBeanServer.
     * @exception ReflectionException  Wraps an java.lang.Exception thrown while trying to invoke the setter.
     */
    public Object getAttribute(ObjectName name, String attribute) throws
	MBeanException, AttributeNotFoundException, InstanceNotFoundException, ReflectionException,
	RemoteException ;

    /**
     * Allows to retrieve the values of several attributes of an MBean.
     * 
     * @param name The object name of the MBean from within which the attributes are
     * to be retrieved.
     *
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return  The values of the attributes retrieved.
     *   
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceNotFoundException
     * @exception ReflectionException An exception occurred trying to invoke the getAttributes method of a Dynamic MBean.
     */
    public AttributeList getAttributes(ObjectName name, String[] attributes)
	throws InstanceNotFoundException, ReflectionException, RemoteException ;

    /**
     * Sets the value of a specific attribute of a named MBean. The MBean
     * is identified by its object name.
     *
     * @param name The name of the MBean within which the attribute is to 
     *        be set.
     *
     * @param attribute The attribute to be set.
     *
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceNotFoundException
     * @exception AttributeNotFoundException
     * @exception InvalidAttributeValueException
     * @exception MBeanException Wraps an exception thrown by the MBean's 
     *            setter.
     *
     * @exception ReflectionException Wraps an exception thrown while trying 
     *            to set the attribute.
     */
    public void setAttribute(ObjectName name, Attribute attribute)
	throws InstanceNotFoundException, AttributeNotFoundException, 
	       InvalidAttributeValueException, MBeanException,
	       ReflectionException, RemoteException ;

    /**
     * Allows to modify the values of several attributes of an MBean.
     *
     * @param name The object name of the MBean from within which the 
     *        attributes are to be set.
     *
     * @param attributes A list of the attributes to be set.
     *
     * @return  The values of the attributes that were set.
     *
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceNotFoundException
     * @exception ReflectionException An exception occurred trying to 
     *            invoke the setAttributes method of a Dynamic MBean.
     */
    public AttributeList setAttributes(ObjectName name,
				       AttributeList attributes)
	throws InstanceNotFoundException, ReflectionException, 
	       RemoteException ;

    /**
     * Invokes an action on an MBean.
     * 
     * @param name The object name of the MBean on which the method is to be invoked.
     * @param actionName The name of the action to be invoked.
     * @param params An array containing the parameters to be set when the action is
     * invoked
     * @param signature An array containing the signature of the action. The class objects will
     * be loaded using the same class loader as the one used for loading the MBean on which the action was invoked.
     *
     * @return  The object returned by the action, which represents the result of invoking the action on the
     * specified MBean.
     *
     * @exception RemoteException See java.rmi.RemoteException.
     * @exception InstanceNotFoundException The specified MBean is not registered in the MBeanServer.
     * @exception MBeanException  Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps an java.lang.Exception thrown while trying to invoke the method.
     */
    public Object invoke(ObjectName name, String actionName, Object params[], String signature[])
	throws InstanceNotFoundException, MBeanException, ReflectionException, RemoteException ;

    /**
     * This method discovers the attributes and operations that an MBean exposes
     * for management. When flatten is false, inherited attributes are not returned.
     *   
     * @param name The name of the MBean to analyze
     *   
     * @return  An instance of MBeanInfo allowing to retrieve all attributes and operations
     * of this MBean.
     *   
     * @exception RemoteException See java.rmi.RemoteException. 
     * @exception MBeanException 
     * @exception IntrospectionException An exception occurs during introspection.
     * @exception InstanceNotFoundException The specified MBean is not found.
     * @exception ReflectionException An exception occurred trying to invoke the getMBeanInfo of a Dynamic MBean.
     */  
    public MBeanInfo getMBeanInfo(ObjectName name) throws
	InstanceNotFoundException, IntrospectionException, ReflectionException, RemoteException ;

    /**
     * Notifications
     */
    public Object[] remoteRequest(int opType, Object[] params) throws Exception, RemoteException ;

    /**
     * HeartBeat
     */
    public String pingHeartBeatServer(String sessionId, int period, int nretries, Long notifSessionId)
	throws RemoteException;
}
