/*
 * @(#)file      MetaData.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.45
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

import javax.management.* ; 
 

/**
 * The MetaData interface provides local to the metadata service in
 * an agent.
 *
 * @since Java DMK 5.0
 */
public interface MetaData {
    
    /** 
     * This methods tests if the MBean is JMX compliant
     */    
    public void testCompliance(Class c) throws NotCompliantMBeanException;

    /**
     * Invokes the  preRegister method  of an MBean that implements 
     * MBeanRegistration
     */
    public ObjectName preRegisterInvoker(Object moi, ObjectName name, 
					 MBeanServer mbs) 
	throws InstanceAlreadyExistsException, MBeanRegistrationException;

    /**
     * Invokes the  postRegister method  of an MBean that implements 
     * MBeanRegistration
     */
    public void postRegisterInvoker(Object moi, boolean registrationDone);

    
    /**
     * Invokes the  preDeregister method  of an MBean that implements 
     * MBeanRegistration
     */
    public void preDeregisterInvoker(Object moi) 
	throws MBeanRegistrationException;

    /**
     * Invokes the  postDeregister method  of an MBean that implements 
     * MBeanRegistration
     */
    public void postDeregisterInvoker(Object moi);


    /**
     * This method discovers the attributes and operations that an MBean 
     * exposes for management.
     *
     * @param instance The MBean whose class is to be analyzed.
     *
     * @return  An instance of MBeanInfo allowing to retrieve all methods 
     *          and operations of this MBean.
     *
     * @exception IntrospectionException if an exception occurs during 
     *             introspection.
     *
     */
    public MBeanInfo getMBeanInfo(Object instance) 
	throws IntrospectionException ;
       
    
    /**
     * This method returns the class name of an MBean.
     *
     * @param instance The MBean whose class is to be analyzed.
     *
     * @return The class name of the MBean, as registered in its MBeanInfo. 
     *
     * @exception IntrospectionException if an exception occurs during 
     *             introspection.
     *
     */
    public String getMBeanClassName(Object instance) 
	throws IntrospectionException, NotCompliantMBeanException ;
       
    
    /**
     * Gets the value of a specific attribute of an MBean. 
     *
     * @param instance The MBean from which the attribute is to be retrieved.
     * @param attribute An String specifying the name of the attribute to be
     * retrieved.
     *
     * @return  The value of the retrieved attribute.
     *
     * @exception AttributeNotFoundException The specified attribute is 
     *            not accessible in the MBean.
     * @exception MBeanException  Wraps an exception thrown by the MBean's 
     *            getter.
     * @exception ReflectionException  Wraps a java.lang.Exception thrown 
     *            while trying to invoke the getter.   
     */    
    public Object getAttribute(Object instance, String attribute)  
	throws MBeanException, AttributeNotFoundException, ReflectionException;


    /**
     * Enables the values of several attributes of an MBean.
     *
     * @param instance The MBean from which the attributes are to be retrieved.
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return The list of the retrieved attributes.
     *
     * @exception ReflectionException An exception occurred when trying to invoke the getAttributes method of a Dynamic MBean.
     *
     */
      public AttributeList getAttributes(Object instance, String[] attributes)
        throws ReflectionException ;


    /**
     * Sets the value of a specific attribute of an MBean. 
     *
     * @param instance The MBean within which the attribute is to be set.
     * @param attribute The identification of the attribute to be set and 
     *        the value it is to be set to.
     *
     * @return  The value of the attribute that has been set.
     *
     * @exception AttributeNotFoundException  The specified attribute is 
     *            not accessible in the MBean.
     * @exception InvalidAttributeValueException The specified value for 
     *            the attribute is not valid.
     * @exception MBeanException Wraps an exception thrown by the MBean's 
     *            setter.
     * @exception ReflectionException  Wraps a java.lang.Exception thrown 
     *            while trying to invoke the setter. 
     */
    public Object setAttribute(Object instance, Attribute attribute) 
	throws AttributeNotFoundException, InvalidAttributeValueException, 
	       MBeanException, ReflectionException;
  

    /**
     * Sets the values of several attributes of an MBean.
     *
     * @param instance The MBean within which the attributes are to be set.
     * @param attributes A list of attributes: The identification of the
     * attributes to be set and  the values they are to be set to.
     *
     * @return  The list of attributes that were set, with their new values.
     *
     * @exception ReflectionException An exception occurred when trying to 
     *            invoke the getAttributes method of a Dynamic MBean.
     *
     */
    public AttributeList setAttributes(Object instance, 
				       AttributeList attributes)
        throws ReflectionException;


    /**
     * Invokes an operation on an MBean.
     *
     * @param instance The MBean on which the method is to be invoked.
     * @param operationName The name of the operation to be invoked.
     * @param params An array containing the parameters to be set when the operation is
     * invoked
     * @param signature An array containing the signature of the operation. The class objects will
     * be loaded using the same class loader as the one used for loading the
     * MBean on which the operation was invoked.
     *
     * @return  The object returned by the operation, which represents the result of
     * invoking the operation on the MBean specified.
     *
     * @exception MBeanException  Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps a java.lang.Exception thrown while trying to invoke the method.
     */
    public Object invoke(Object instance, String operationName, 
			 Object params[],String signature[]) 
	throws  MBeanException, ReflectionException;         
 
    /**
     * Determine whether the given MBean is an instance of a given
     * class/interface.
     * 
     * @param instance The MBean concerned.
     * @param className The name of the class or interface.
     * @return <code>true</code> if the MBean is an instance of the
     *         given <code>class</code>, <code>false</code> otherwise.
     * @exception ReflectionException if 
     **/
    public boolean isInstanceOf(Object instance, String className)
	throws ReflectionException;

}
