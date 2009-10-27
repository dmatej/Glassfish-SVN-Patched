/*
 * @(#)file      MetaDataImpl.java
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
 */

package com.sun.jdmk;

// java import

import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Hashtable;
import java.util.Iterator;
import java.io.PrintWriter;
import java.io.StringWriter;

// RI import
import javax.management.* ; 
import com.sun.jdmk.internal.ClassLogger;

 

/**
 * The MetaData class provides local access to the metadata service in
 * an agent.
 *
 * @deprecated This class is kept as a utility class, though it is no longer
 *      used. It may not be supported in future releases.
 * @since Java DMK 5.0
 */
public class MetaDataImpl extends DynamicMetaDataImpl {


    /** The name of this class to be used for tracing */
    private final static String dbgTag = "MetaDataImpl";
    
    /**
     * Cache of MBeanInfo objects.
     */
    private static  java.util.Hashtable mbeanInfoCache = 
        new java.util.Hashtable();
    
    /**
     * Cache of MBean Interface objects.
     */
    private static  java.util.Hashtable mbeanInterfaceCache = 
        new java.util.Hashtable();

    /**
     * objects maps from primitive classes to primitive object classes.
     */
    // private static Hashtable primitiveobjects = new Hashtable();
    // {
    //  primitiveobjects.put(Boolean.TYPE, getClass("java.lang.Boolean"));
    //  primitiveobjects.put(Character.TYPE, getClass("java.lang.Character"));
    //  primitiveobjects.put(Byte.TYPE, getClass("java.lang.Byte"));
    //  primitiveobjects.put(Short.TYPE, getClass("java.lang.Short"));
    //  primitiveobjects.put(Integer.TYPE, getClass("java.lang.Integer"));
    //  primitiveobjects.put(Long.TYPE, getClass("java.lang.Long"));
    //  primitiveobjects.put(Float.TYPE, getClass("java.lang.Float"));
    //  primitiveobjects.put(Double.TYPE, getClass("java.lang.Double"));
    // }
    private final static Hashtable primitiveClasses = new Hashtable(8);
    {
        primitiveClasses.put(Boolean.TYPE.toString(), Boolean.TYPE);
        primitiveClasses.put(Character.TYPE.toString(), Character.TYPE);
        primitiveClasses.put(Byte.TYPE.toString(), Byte.TYPE);
        primitiveClasses.put(Short.TYPE.toString(), Short.TYPE);
        primitiveClasses.put(Integer.TYPE.toString(), Integer.TYPE);
        primitiveClasses.put(Long.TYPE.toString(), Long.TYPE);
        primitiveClasses.put(Float.TYPE.toString(), Float.TYPE);
        primitiveClasses.put(Double.TYPE.toString(), Double.TYPE);
     }    



    /**
     * Creates a Metadata Service.
     * @param instantiator The MBeanInstantiator that will be used to
     *        take care of class loading issues.
     *        This parameter may not be null.
     * @exception IllegalArgumentException if the instantiator is null.
     */
    MetaDataImpl(MBeanInstantiator instantiator)  {
        super(instantiator);
    } 

    
    /** 
     * This methods tests if the MBean is JMX compliant
     */    
    public synchronized void testCompliance(Class c) 
        throws NotCompliantMBeanException {
        MBeanInfo bi = Introspector.testCompliance(c);
        if (bi != null) {
            if (mbeanInfoCache.get(c) == null) {
                mbeanInfoCache.put(c, bi);
            }
        }
        Class inter = Introspector.getMBeanInterface(c);
        if (inter != null) {
            if (mbeanInterfaceCache.get(c) == null) {
                mbeanInterfaceCache.put(c, inter);
            }
        }
    }

  
    /** 
     * This methods returns the MBean interface of an MBean
     */    
    public Class getMBeanInterfaceFromClass(Class c) {
        Class inter = (Class)mbeanInterfaceCache.get(c);
        return inter;
    }

    
    /**
     * This method discovers the attributes and operations that an MBean 
     * exposes for management.
     *
     * @param beanClass The class to be analyzed.
     *
     * @return  An instance of MBeanInfo allowing to retrieve all methods
     *          and operations of this class.
     *
     * @exception IntrospectionException if an exception occurs during
     *            introspection.
     * @exception NotCompliantMBeanException if the MBean class is not
     *            MBean compliant.
     *
     */
    public MBeanInfo getMBeanInfoFromClass(Class beanClass) 
        throws IntrospectionException, NotCompliantMBeanException {

        // Check the mbean information cache.        
        MBeanInfo bi = (MBeanInfo)mbeanInfoCache.get(beanClass);

        // Make an independent copy of the MBeanInfo.        
        if (bi != null) return (MBeanInfo) bi.clone() ;

        // We don't have have any MBeanInfo for that class yet.
        // => test compliance.
        testCompliance(beanClass);
        bi = (MBeanInfo)mbeanInfoCache.get(beanClass);

        // Make an independent copy of the MBeanInfo.
        if (bi != null) return (MBeanInfo) bi.clone() ;
        return bi;
    } 
       

    /**
     * Finds a specific method of an object.
     * Returns the method or null if not found
     */    
    public static Method findMethod(Class classObj, String name,
                             Class parameterTypes[]) {
        Method method=null;   
        try {
            method= classObj.getMethod(name, parameterTypes); 
        } catch(Exception e) {
        }
        
        return method;
    }
    
  

    /**
     * Finds a specific method of an object without knowing the parameter 
     * types.
     * Returns the method or null if not found
     */
    public static Method findMethod(Class classObj, String name) {
        Method method = null ;
        
        try {
            Method[] methods=classObj.getMethods();
            int i = 0;
            while ((i < methods.length) && 
                   !methods[i].getName().equals(name)) {
                i++;
            }
            if (i < methods.length) { 
                method = methods[i];
            }
        } catch(Exception e) {
        }
        return method;
    }

 
    /**
    * Finds a specific method of an object given the number of parameters.
    * Returns the method or null if not found
    */
    public static Method findMethod(Class classObj, String name, 
                                    int paramCount) {
 
        Method method = null;
        try {
            Method[] methods=classObj.getMethods();
            int i = 0;
            boolean found = false;
            while ((i < methods.length) && !found) {
                found = methods[i].getName().equals(name);
                if (found) { // Now check if the number of parameters
                    found = (methods[i].getParameterTypes().length == 
                             paramCount);
                }
                i++;
            }
            if (found) { 
                method = methods[i-1] ; // Note i-1 !
            }
        } catch(Exception e) {
        }
        return method;
    }
    
   
    /**
     * Finds the getter of a specific attribute in an object.
     * Returns the method for accessing the attributes, null otherwise
     */
    public static Method findGetter(Class classObj, String attribute)  {
        // A getter should not have any parameter       
        Method m = findMethod(classObj, "get" + attribute, null);
        if (m == null) {
            m = findMethod(classObj, "is" + attribute, null);
        }
        return m;
    }
    
   
    /**
     * Finds the setter of a specific attribute in an object.
     * Returns the method for accessing the attribute, null otherwise
     */
    public static Method findSetter(Class classObj, String attribute, 
                                    Class type)  {      

        Method mth= findMethod(classObj, "set" + attribute, 1);
        if (mth != null) {
            Class[] pars = mth.getParameterTypes();
            if (pars[0].isAssignableFrom(type)) {
                return mth;
            }
        }
        return null;
    }   

    /**
     * Finds the setter of a specific attribute without knowing its type.   
     * Returns the method for accessing the attribute, null otherwise
     */    
    public static Method findSetter(Class classObj, String attribute)  {
        return findMethod(classObj, "set" + attribute, 1) ;
    }
      
   /**
    * Finds a specific constructor of a class
    * Returns the requested constructor or null if not found
    */
    public static Constructor findConstructor(Class theClass,
                                              Class parameterTypes[]) {
        // Get the list of methods              
        Constructor mth = null;
        
        try {
            mth = theClass.getConstructor(parameterTypes);
        } catch(Exception e) {
            return null;
        }
        return mth;
    }
   
    /** 
     * Get the class of the constructed type 
     * corresponding to the given primitive type
     */    
    public static Class findClassForPrim(String primName) {
        return (Class) primitiveClasses.get(primName);
    }
      
    /**
     * Get the class of the primitive type 
     * corresponding to the given constructed object.
     */
    public static Class findPrimForClass(Object value) {
        if (value instanceof Boolean)
            return Boolean.TYPE;
        else if (value instanceof Character)
            return Character.TYPE;
        else if (value instanceof Byte)
            return Byte.TYPE;
        else if (value instanceof Short)
            return Short.TYPE;
        else if (value instanceof Integer)
            return Integer.TYPE;
        else if (value instanceof Long)
         return Long.TYPE;
        else if (value instanceof Float)
            return Float.TYPE;
        else if (value instanceof Double)
            return Double.TYPE;
        return null;
    }

    

    //---------------------------------------------------------------------
    //
    // From the MetaData interface
    //
    //---------------------------------------------------------------------

    public String getMBeanClassName(Object moi) 
        throws IntrospectionException, NotCompliantMBeanException {
        if (moi instanceof DynamicMBean)
            return super.getMBeanClassName(moi);
        else
            return moi.getClass().getName();
    }

    public MBeanInfo getMBeanInfo(Object moi) 
        throws IntrospectionException {

        if (moi instanceof DynamicMBean) 
            return super.getMBeanInfo(moi);

        try {
            final MBeanInfo mbi = getMBeanInfoFromClass(moi.getClass());
            return new MBeanInfo(mbi.getClassName(), mbi.getDescription(), 
                                 mbi.getAttributes(), 
                                 mbi.getConstructors(), 
                                 mbi.getOperations(), 
                                 findNotifications(moi));
        } catch (NotCompliantMBeanException x) {
            logger.finest("getMBeanInfo",x);
            throw new IntrospectionException("Can't build MBeanInfo for "+
                                             moi.getClass().getName());
        }
    }

    public Object getAttribute(Object instance, String attribute)  
        throws MBeanException, AttributeNotFoundException, 
               ReflectionException {

        // Treatment for Dynamic MBean
        if (instance instanceof DynamicMBean)
            return super.getAttribute(instance,attribute);

        Class mbeanClass = getMBeanInterfaceFromClass(instance.getClass());
        if (logger.finestOn()) {
            logger.finest("getAttribute","MBean Class is " + 
                         instance.getClass());
            logger.finest("getAttribute","MBean Interface is " + mbeanClass);
        }

        return getAttribute(instance, attribute, mbeanClass);
    }

 
    public AttributeList getAttributes(Object instance, String[] attributes) 
        throws ReflectionException {

        if (instance instanceof DynamicMBean) 
            return super.getAttributes(instance, attributes);
        
        final Class mbeanClass = 
            getMBeanInterfaceFromClass(instance.getClass());

        if (logger.finestOn()) {
            logger.finest("getAttributes","MBean Class is " + 
                         instance.getClass());
            logger.finest("getAttributes","MBean Interface is " + mbeanClass);
        }

        if (attributes == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Attributes cannot be null"), 
                "Exception occurred trying to invoke the getter on the MBean");
        }

        // Go through the list of attributes
        //
        final int maxLimit = attributes.length;
        final AttributeList result = new AttributeList(maxLimit);

        for (int i=0;i<maxLimit;i++) { 
            final String elmt = (String)attributes[i];
            try {        
                final Object value = 
                    getAttribute(instance, elmt, mbeanClass);
                result.add(new Attribute(elmt, value));
            } catch (Exception excep) {
                if (logger.finestOn()) {
                    logger.finest("getAttributes", "Object= " + instance + 
                          ", Attribute=" + elmt + " failed: " + excep);
                }          
            }
        }
        return result;
    }


    public AttributeList setAttributes(Object instance, 
                                       AttributeList attributes) 
        throws ReflectionException {
        
        if (instance instanceof DynamicMBean) 
            return super.setAttributes(instance,attributes);

        final Class objClass       = instance.getClass();
        final Class mbeanClass     = getMBeanInterfaceFromClass(objClass);
        final ClassLoader aLoader  = objClass.getClassLoader();
        
        if (logger.finestOn()) {
            logger.finest("setAttributes","MBean Class is " + 
                         instance.getClass());
            logger.finest("setAttributes","MBean Interface is " + mbeanClass);
        }

        if (attributes == null) return new AttributeList();

        final AttributeList result = new AttributeList(attributes.size());

        // Go through the list of attributes
        for (final Iterator i = attributes.iterator(); i.hasNext();) {
            final Attribute attr = (Attribute) i.next();
            final String id          = attr.getName();
            final Object value       = attr.getValue();          
            try {
                final Object newValue = 
                    setAttribute(instance, attr, mbeanClass);  
                if (logger.traceOn()) {
                    logger.trace("setAttributes", "Updating the list\n");
                }                                                
                result.add(new Attribute(id, newValue));
            } catch (Exception excep) {
                if (logger.finestOn()) {
                    logger.finest("setAttributes", 
                                 "Unexpected exception occurred: " +
                                 excep.getClass().getName());
                }
            }
        }
        return result;  

    }
    

    public Object setAttribute(Object instance, Attribute attribute) 
        throws AttributeNotFoundException, InvalidAttributeValueException, 
               MBeanException, ReflectionException {

        // Dynamic MBean
        if (instance instanceof DynamicMBean)
            return super.setAttribute(instance,attribute);

        // Standard MBean
        final Class mbeanClass = 
            getMBeanInterfaceFromClass(instance.getClass());

        if (logger.finestOn()) {
            logger.finest("setAttribute","MBean Class is " + 
                         instance.getClass());
            logger.finest("setAttribute","MBean Interface is " + mbeanClass);
        }

        return setAttribute(instance,attribute,mbeanClass);
    }

    public Object invoke(Object instance, String operationName, 
                         Object params[], String signature[]) 
        throws  MBeanException, ReflectionException {

        if (operationName == null) {
            final RuntimeException r = 
              new IllegalArgumentException("Operation name  cannot be null");
            throw new RuntimeOperationsException(r, 
              "Exception occurred trying to invoke the operation on the MBean");
        } 

        if (instance instanceof DynamicMBean) 
            return super.invoke(instance,operationName,params,signature);

        final Class objClass = instance.getClass();
        final Class mbeanClass = getMBeanInterfaceFromClass(objClass);
        final ClassLoader aLoader = objClass.getClassLoader();

        if (logger.finestOn()) {
            logger.finest("invoke","MBean Class is " + instance.getClass());
            logger.finest("invoke","MBean Interface is " + mbeanClass);
        }

        // Build the signature of the method
        //
        final Class[] tab = 
            ((signature == null)?null:
             instantiator.findSignatureClasses(signature,aLoader));
        
        // Query the metadata service to get the right method
        //    
        Method mth= findMethod(mbeanClass, operationName, tab);
        
        if (mth == null) {
            if (logger.traceOn()) {
                logger.trace("invoke", operationName + " not found in class " +
                             mbeanClass.getName());
            }
            throw new ReflectionException(
                          new NoSuchMethodException(operationName), 
                          "The operation with name " + operationName + 
                          " could not be found");
        }
        // invoke the method        
        if (logger.traceOn()) {
            logger.trace("invoke", "Invoking " + operationName);
        }           
        Object result=null;
        try {
            final Object[] args = 
                instantiator.checkTransferParameters(aLoader,params);
            result= mth.invoke(instance,args); 
        } catch (IllegalAccessException e) {
            logger.finest("invoke",e);
            throw new ReflectionException(e, "IllegalAccessException" + 
                   " occurred trying to invoke operation " + operationName);
        } catch (RuntimeException e) {
            logger.finest("invoke",e);
            throw new RuntimeOperationsException(e, "RuntimeException" + 
                   " occurred trying to invoke operation " + operationName);
        } catch (InvocationTargetException e) {
            // Wrap the exception.         
            Throwable t = e.getTargetException();
            logger.finest("invoke",t);
            if (t instanceof RuntimeException) {
                throw new RuntimeMBeanException((RuntimeException)t, 
                   "RuntimeException thrown in operation " + operationName);
            } else if (t instanceof Error) {
                throw new RuntimeErrorException((Error) t, 
                   "Error thrown in operation " + operationName);
            } else {
                throw new MBeanException((Exception) t, 
                   "Exception thrown in operation " + operationName);
            }
        }
        if (logger.traceOn()) {
            logger.trace("invoke", "Send the result");
        }      
        return (result);
    }

    public boolean isInstanceOf(Object instance, String className) 
        throws ReflectionException {
        
        if (instance instanceof DynamicMBean) {
            final boolean result = super.isInstanceOf(instance,className);
            if (result) return true;
        }

        final Class c = instantiator.findClass(className, 
                         instance.getClass().getClassLoader());

        return c.isInstance(instance);
    }


    /**
     * Invoke getAttribute through reflection on a standard MBean instance.
     **/
    protected Object getAttribute(Object instance, String attribute, 
                                  Class mbeanClass)  
        throws MBeanException, AttributeNotFoundException, 
               ReflectionException {

        if (attribute == null) {
            final RuntimeException r = 
                new IllegalArgumentException("Attribute name cannot be null");
            throw new RuntimeOperationsException(r, 
                "Exception occurred trying to invoke the getter on the MBean");
        }  
 
        // Standard MBean: need to reflect...
        Method meth = null;
        meth = findGetter(mbeanClass, attribute);
        if (meth == null) {
            if (logger.traceOn()) {
                logger.trace("getAttribute", "Cannot find getter for "+
                             attribute+
                             " in class " + mbeanClass.getName());
            }                 
            throw new AttributeNotFoundException(attribute + 
                                                 " not accessible");
        }

        // Invoke the getter     
        if (logger.traceOn()) {
            logger.trace("getAttribute", "Invoke callback");
        }                 
        Object result= null;
        try {
            result = meth.invoke(instance,(Object[])null);      
        } catch (InvocationTargetException e) {
            Throwable t = e.getTargetException();
            if (t instanceof RuntimeException) {
                logger.finest("getAttribute",t);
                throw new RuntimeMBeanException((RuntimeException)t, 
                  "RuntimeException thrown in the getter for the attribute " +
                  attribute); 
            } else if (t instanceof Error) {
                logger.finest("getAttribute",t);
                throw new RuntimeErrorException((Error) t ,
                  "Error thrown in the getter for the attribute " + 
                                                attribute);    
            } else {
                logger.finest("getAttribute",t);
                throw new MBeanException((Exception) t, 
                  "Exception thrown in the getter for the attribute " + 
                  attribute);    
            }
        } catch (RuntimeException e) {
            logger.finest("getAttribute",e);
            throw new RuntimeOperationsException(e, 
                  "RuntimeException thrown trying to invoke the getter" +
                  " for the attribute " + attribute);    
        } catch (IllegalAccessException e) {
            logger.finest("getAttribute",e);
            throw new ReflectionException(e, "Exception thrown trying to" +
                  " invoke the getter for the attribute " + attribute);
        } catch (Error e) {
            logger.finest("getAttribute",e);
            throw new RuntimeErrorException((Error)e, 
                  "Error thrown trying to invoke the getter " +
                  " for the attribute " + attribute);
        }

        if (logger.traceOn()) {
            logger.trace("getAttribute", attribute + "= " + result + "\n");
        }                 
        return result; 
    }

    /**
     * Invoke setAttribute through reflection on a standard MBean instance.
     **/
    protected Object setAttribute(Object instance, Attribute attribute, 
                                  Class mbeanClass)  
        throws AttributeNotFoundException, InvalidAttributeValueException, 
               MBeanException, ReflectionException {

        if (attribute == null) {
            final RuntimeException r = 
                new IllegalArgumentException("Attribute name cannot be null");
            throw new RuntimeOperationsException(r, 
                "Exception occurred trying to invoke the setter on the MBean");
        }  
 
        final Class objClass = instance.getClass();
        final ClassLoader aLoader = objClass.getClassLoader();

        Object result = null;
        final Attribute attr = 
            instantiator.checkTransferAttribute(aLoader, attribute);
        final Object value   = attr.getValue();
        final String attname = attr.getName();

        // Query the metadata service to get the appropriate setter
        // of the object.
        Method meth = null;

        if (value == null) {
            meth = findSetter(mbeanClass, attname);
        } else {
            meth = findSetter(mbeanClass, attname, value.getClass());
        }
        if (meth == null) {       
            // Check whether the type is a primitive one       
            Class primClass = findPrimForClass(value);     
       
            if (primClass != null) {       
                meth = findSetter(mbeanClass, attname, primClass);     
            }
        }     
        if (meth == null) {
            // Try to check if the attribute name does correspond to a 
            // valid property       
            meth= findSetter(mbeanClass, attname);
            if (meth == null) {
                if (logger.traceOn()) {
                    logger.trace("setAttribute", "Cannot find setter for "+
                                 attribute+
                                 " in class " + mbeanClass.getName());
                }                 
                throw new AttributeNotFoundException( attname + 
                                                      " not accessible");
            } else {
                final Object v = attribute.getValue();
                if (v == null) {
                    throw new InvalidAttributeValueException("attribute= " + 
                                attname + " value = null");
                } else {
                    throw new InvalidAttributeValueException("attribute= " + 
                                attname + " value = " + v);
                }
            }
        }     
        // Invoke the setter     
        if (logger.traceOn()) {
            logger.trace("setAttribute", "Invoking the set method for " + 
                  attname);
        }   
     
        final Object[] values = new Object[1];
        values[0] = value;

        try {
            result = meth.invoke(instance,values);
        } catch (IllegalAccessException e) {
            logger.finest("setAttribute",e);
            // Wrap the exception.            
            throw new ReflectionException(e, "IllegalAccessException" + 
                          " occurred trying to invoke the setter on the MBean");
        } catch (InvocationTargetException e) {
            Throwable t = e.getTargetException();
            logger.finest("setAttribute",t);
            if (t instanceof RuntimeException) {
                throw new RuntimeMBeanException((RuntimeException)t, 
                            "RuntimeException thrown in the MBean's setter");  
            } else if (t instanceof Error) {
                throw new RuntimeErrorException((Error) t, 
                           "Error thrown in the MBean's setter");   
            } else {
                throw new MBeanException((Exception) t, 
                           "Exception thrown in the MBean's setter");
            }
        }
        if (logger.traceOn()) {
            logger.trace("setAttribute", attname + "= " + value);
        }         
        return value;
    }


    /**
     * Converts the array of classes to an array of class signatures.
     */
    static String[] findSignatures(Class[] clz) {
        String signers[] = new String[clz.length];
        for (int i = 0; i < clz.length; i++) {
            signers[i] = findSignature(clz[i]);
        }
        return signers;
    }
    
    /**
     * Converts the class to a class signature.
     */
    static String findSignature(Class clz) {
        return clz.getName();
    }
    

    /**
     * Returns the MBeanNotificationInfo of the MBeans that implement
     * the NotificationBroadcaster interface.
     */
    private MBeanNotificationInfo[] findNotifications(Object moi) {
        
        if (moi instanceof javax.management.NotificationBroadcaster) {       
            MBeanNotificationInfo[] mbn = 
            ((NotificationBroadcaster)moi).getNotificationInfo();
            if (mbn == null) {
                return new MBeanNotificationInfo[0];
            }
            MBeanNotificationInfo[] result = 
            new MBeanNotificationInfo[mbn.length];
            for (int i = 0; i < mbn.length; i++) {
                result[i] = (MBeanNotificationInfo) mbn[i].clone();
            }
            return result;
        }
        return new MBeanNotificationInfo[0];
    }

    // TRACES & DEBUG
    //---------------
    private final ClassLogger logger = 
        new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
                        "MetaDataImpl");
    
 }
