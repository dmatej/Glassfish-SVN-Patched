/*
 * @(#)file      MBeanInstantiator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.14
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
import java.io.ObjectInputStream;


/**
 * Contains methods for instantiating objects, finding the class given
 * its name and using different class loaders, deserializing objects
 * in the context of a given class loader.
 *
 * @since Java DMK 5.0
 */

public interface MBeanInstantiator {

    /** 
     * This methods tests if the MBean class makes it possible to 
     * instantiate an MBean of this class in the MBeanServer.
     * e.g. it must have a public constructor, be a concrete class...
     */    
    public void testCreation(Class c) throws NotCompliantMBeanException;

    /**
     * Loads the class with the specified name using this object's 
     * Default Loader Repository. 
     **/
    public Class findClassWithDefaultLoaderRepository(String className) 
	throws ReflectionException;


    /**
     * Return the Default Loader Repository used by this instantiator object.
     **/
    public ModifiableClassLoaderRepository getClassLoaderRepository();

    /**
     * Gets the class for the specified class name using the MBean 
     * Interceptor's classloader
     */
    public Class findClass(String className, ClassLoader loader) 
	throws ReflectionException;

    /**
     * Gets the class for the specified class name using the specified 
     * class loader
     */
    public Class findClass(String className, ObjectName loaderName) 
        throws ReflectionException, InstanceNotFoundException ;

    /**
     * Return an array of Class corresponding to the given signature, using
     * the specified class loader.
     */
    public Class[] findSignatureClasses(String signature[],
					ClassLoader loader)
	throws ReflectionException;
    
    /**
     * Check whether the attribute values needs to be transferred in the
     * destination class loader. Returns an attribute list containing the
     * transferred values.<p>
     * The method checks if the context of the operation is ok, i.e if all the
     * attribute values are coming from the same class loader. If not, 
     * the method will replace the attributes that need to be transferred by 
     * a new attribute object transferred in the target class loader.
     * @param target the target class loader.
     * @param attributes the attribute list to be checked.
     * @return the attribute list, where values have been transferred as needed.
     **/
    public AttributeList checkTransferAttributeList(ClassLoader target, 
						    AttributeList attributes);

    /**
     * Check whether the attribute value needs to be transferred in the
     * destination class loader. Returns an attribute containing the
     * transferred value.<p>
     * The method checks if the context of the operation is ok, i.e if all the
     * attribute values are coming from the same class loader. If not, 
     * the method will replace the attributes that need to be transferred by 
     * a new attribute object transferred in the target class loader.
     * @param target the target class loader.
     * @param attribute attribute to be checked.
     * @return attribute, or a new attribute containing the transferred value.
     **/
    public Attribute checkTransferAttribute(ClassLoader target,
					    Attribute attribute);

    /**
     * Check whether the given parameters need to be transferred into the
     * target class loader.<p>
     * The method checks if the context of the operation is ok, i.e if all the
     * objects are coming from the same class loader. If not, the method will
     * replace the parameters that need to be transferred by a new object
     * transferred in the target class loader.
     * @param target the target class loader.
     * @param params parameters to be checked.
     * @return params.
     */
    public Object[] checkTransferParameters(ClassLoader target, 
					    Object params[]);

    /**
     * Check whether the given value need to be transferred into the
     * target class loader.<p>
     * The method checks if the context of the operation is ok, i.e if the
     * value is coming from the target class loader. If not, the method will
     * return a new object transferred in the target class loader.
     * @param targetLoader the target class loader.
     * @param value  object to be checked.
     * @return value, or the new transferred value.
     */
    public Object checkTransferObject(ClassLoader targetLoader, 
				      Object value);
    /**
     * De-serializes a byte array in the context of the class loader 
     * of an MBean.
     *
     * @param name The name of the MBean whose class loader should be used for
     * the de-serialization.
     * @param data The byte array to be de-sererialized.
     *
     * @return  The de-serialized object stream.
     *
     * @exception InstanceNotFoundException The MBean specified is not found.
     * @exception OperationsException Any of the usual Input/Output related
     * exceptions.
     */
    public ObjectInputStream deserialize(ObjectName name, byte[] data)
	throws InstanceNotFoundException,
	       OperationsException;

    /**
     * De-serializes a byte array in the context of a given MBean class loader.
     * The class loader is the one that loaded the class with name "className".
     *
     * @param className The name of the class whose class loader should 
     *        be used for the de-serialization.
     * @param data The byte array to be de-sererialized.
     *
     * @return  The de-serialized object stream.
     *
     * @exception OperationsException Any of the usual Input/Output related
     * exceptions.
     * @exception ReflectionException The specified class could not be loaded
     * by the default loader repository     
     */
    public ObjectInputStream deserialize(String className, byte[] data)
	throws OperationsException,
	       ReflectionException;
   
    /**
     * De-serializes a byte array in the context of a given MBean class loader.
     * <P>The class loader is the one that loaded the class with name
     * "className".
     * <P>The name of the class loader to be used for loading the specified
     * class is specified. If null, a default one has to be provided (for a
     * MBean Server, its own class loader will be used).
     *
     * @param className The name of the class whose class loader should be 
     *        used for the de-serialization.
     * @param loaderName The name of the class loader to be used for loading
     * the specified class. If null, a default one has to be provided (for a
     * MBean Server, its own class loader will be used).
     * @param data The byte array to be de-sererialized.
     * @param loader default class loader.
     *
     * @return  The de-serialized object stream.
     *
     * @exception InstanceNotFoundException The specified class loader MBean is
     * not found.          
     * @exception OperationsException Any of the usual Input/Output related
     * exceptions.
     * @exception ReflectionException The specified class could not be loaded
     * by the specified class loader.
     */
    public ObjectInputStream deserialize(String className,
					 ObjectName loaderName,
					 byte[] data,
					 ClassLoader loader)
	throws InstanceNotFoundException,
	       OperationsException,
	       ReflectionException;

    /**
     * Instantiates an object using the list of all class loaders registered
     * in the MBean Interceptor
     * (using its {@link javax.management.loading.ClassLoaderRepository}).
     * <P>The object's class should have a public constructor.
     * <P>It returns a reference to the newly created object.
     * <P>The newly created object is not registered in the MBean Interceptor.
     *
     * @param className The class name of the object to be instantiated.    
     *
     * @return The newly instantiated object.    
     *
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.ClassNotFoundException</CODE> or the
     * <CODE>java.lang.Exception</CODE> that occurred when trying to invoke the
     * object's constructor.
     * @exception MBeanException The constructor of the object has thrown an
     * exception
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: the className passed in
     * parameter is null.
     */
    public Object instantiate(String className)
	throws ReflectionException,
	       MBeanException;

    /**
     * Instantiates an object using the class Loader specified by its
     * <CODE>ObjectName</CODE>.
     * <P>If the loader name is null, a default one has to be provided (for a
     * MBean Server, the ClassLoader that loaded it will be used).
     * <P>The object's class should have a public constructor.
     * <P>It returns a reference to the newly created object.
     * <P>The newly created object is not registered in the MBean Interceptor.
     *
     * @param className The class name of the MBean to be instantiated.    
     * @param loaderName The object name of the class loader to be used.
     *
     * @return The newly instantiated object.    
     *
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.ClassNotFoundException</CODE> or the
     * <CODE>java.lang.Exception</CODE> that occurred when trying to invoke the
     * object's constructor.
     * @exception MBeanException The constructor of the object has thrown an
     * exception.
     * @exception InstanceNotFoundException The specified class loader is not
     * registered in the MBeanInterceptor.
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: the className passed in
     * parameter is null.
     */
    public Object instantiate(String className, ObjectName loaderName, ClassLoader loader) 
        throws ReflectionException,
	       MBeanException,
	       InstanceNotFoundException;

    /**
     * Instantiates an object using the list of all class loaders registered
     * in the MBean server
     * (using its {@link javax.management.loading.ClassLoaderRepository}).
     * <P>The object's class should have a public constructor.
     * <P>The call returns a reference to the newly created object.
     * <P>The newly created object is not registered in the MBean Interceptor.
     *
     * @param className The class name of the object to be instantiated.
     * @param params An array containing the parameters of the constructor to
     * be invoked.
     * @param signature An array containing the signature of the constructor to
     * be invoked.     
     *
     * @return The newly instantiated object.    
     *
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.ClassNotFoundException</CODE> or the
     * <CODE>java.lang.Exception</CODE> that occurred when trying to invoke the
     * object's constructor.  
     * @exception MBeanException The constructor of the object has thrown an
     * exception
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: the className passed in
     * parameter is null.
     */    
    public Object instantiate(String className,
			      Object params[],
			      String signature[],
			      ClassLoader loader) 
        throws ReflectionException,
	       MBeanException ; 


    /**
     * Instantiates an object. The class loader to be used is identified by its
     * object name.
     * <P>If the object name of the loader is null, a default has to be
     * provided (for example, for a MBean Server, the ClassLoader that loaded
     * it will be used).
     * <P>The object's class should have a public constructor.
     * <P>The call returns a reference to the newly created object.
     * <P>The newly created object is not registered in the MBean server.
     *
     * @param className The class name of the object to be instantiated.
     * @param params An array containing the parameters of the constructor to
     * be invoked.
     * @param signature An array containing the signature of the constructor to
     * be invoked.     
     * @param loaderName The object name of the class loader to be used.
     *
     * @return The newly instantiated object.    
     *
     * @exception ReflectionException Wraps a
     * <CODE>java.lang.ClassNotFoundException</CODE> or the
     * <CODE>java.lang.Exception</CODE> that occurred when trying to invoke the
     * object's constructor.  
     * @exception MBeanException The constructor of the object has thrown an
     * exception
     * @exception InstanceNotFoundException The specified class loader is not
     * registered in the MBean Interceptor. 
     * @exception RuntimeOperationsException Wraps a
     * <CODE>java.lang.IllegalArgumentException</CODE>: the className passed in
     * parameter is null.
     */    
    public Object instantiate(String className,
			      ObjectName loaderName,
			      Object params[],
			      String signature[],
			      ClassLoader loader) 
        throws ReflectionException,
	       MBeanException,
	       InstanceNotFoundException;


    /**
     * Instantiates an object given its class, using its empty constructor.
     * The call returns a reference to the newly created object.
     */
    public Object instantiate(Class theClass) throws ReflectionException, MBeanException;


   /**
     * Instantiates an object given its class, the parameters and signature of its constructor
     * The call returns a reference to the newly created object.
     */
    public Object instantiate(Class theClass, Object params[], String signature[], ClassLoader loader) throws ReflectionException, MBeanException;


    /**
     * Transfer a given object into the context of a different class loader
     */
    public Object transferObject(Object obj, ClassLoader loader);

}
