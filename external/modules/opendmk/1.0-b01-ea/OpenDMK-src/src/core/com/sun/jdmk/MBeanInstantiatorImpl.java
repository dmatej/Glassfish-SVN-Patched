/*
 * @(#)file      MBeanInstantiatorImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.19
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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.io.*;

import javax.management.*; 
import javax.management.loading.ClassLoaderRepository;
import com.sun.jdmk.internal.ClassLogger;



/**
 * Implements the MBeanInstantiator interface. Provides methods for
 * instantiating objects, finding the class given its name and using
 * different class loaders, deserializing objects in the context of a
 * given class loader.
 *
 * @deprecated This class is kept as a utility class, though it is no longer
 *      used. It may not be supported in future releases.
 * @since Java DMK 5.0
 */

public class MBeanInstantiatorImpl implements MBeanInstantiator {

    private final ModifiableClassLoaderRepository clr; 
    //    private MetaData meta = null;

    /** The name of this class to be used for tracing */
    private final static String dbgTag = "MBeanInstantiatorImpl";

    public MBeanInstantiatorImpl(ModifiableClassLoaderRepository clr) {
        this.clr = clr;
    }

  
    public void testCreation(Class c) throws NotCompliantMBeanException {
        Introspector.testCreation(c);
    }

    public Class findClassWithDefaultLoaderRepository(String className) 
        throws ReflectionException {

        Class theClass;
        if (className == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("The class name cannot be null"), 
                             "Exception occurred during object instantiation");
        }

        try {
            if (clr == null) throw new ClassNotFoundException(className);
            theClass = clr.loadClass(className);
        }
        catch (ClassNotFoundException ee) {
            throw new ReflectionException(ee, 
       "The MBean class could not be loaded by the default loader repository");
        }
        
        return theClass;
    }


    public Class findClass(String className, ClassLoader loader) 
        throws ReflectionException {
   
        Class theClass = null;
        if (className == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("The class name cannot be null"), 
                              "Exception occurred during object instantiation");
        } 
        try {
            if (loader != null) {
                theClass = loader.loadClass(className);
            } else {
                theClass = Class.forName(className);
            }
        } catch (ClassNotFoundException e) {
            throw new ReflectionException(e, 
   "The MBean class could not be loaded by the MBean Interceptor's classloader");
        }
        return theClass;
    }

    public Class findClass(String className, ObjectName aLoader) 
        throws ReflectionException, InstanceNotFoundException  {
        Class theClass = null;

        if (aLoader == null)  
            throw new RuntimeOperationsException(new 
                IllegalArgumentException(), "Null loader passed in parameter");

        // Retrieve the class loader from the repository
        Object loader = null;
        synchronized(this) {
            if (clr!=null) 
                loader = clr.getClassLoader(aLoader);
        }
        if (loader == null) {
            throw new InstanceNotFoundException("The loader named " + 
                       aLoader + " is not registered in the MBeanServer");
        }     
        try {
            theClass = ((ClassLoader)loader).loadClass(className);
        } catch (ClassNotFoundException e) {
            throw new ReflectionException(e, 
           "The MBean class could not be loaded by the specified classloader");
        }
        return theClass;
    }


    public Class[] findSignatureClasses(String signature[],
                                        ClassLoader loader)
        throws  ReflectionException {

        if (signature == null) return null;
        final ClassLoader aLoader = (ClassLoader) loader;
        final int length= signature.length;
        final Class tab[]=new Class[length]; 

        if (length == 0) return tab;
        try {
            for (int i= 0; i < length; i++) {
                // Start handling primitive types (int. boolean and so 
                // forth)
                //
                
                final Class primCla = 
                    MetaDataImpl.findClassForPrim(signature[i]);
                if (primCla != null) {
                    tab[i] = primCla;
                    continue;
                }

                // Ok we do not have a primitive type ! We need to build 
                // the signature of the method
                //
                if (aLoader != null) {
                    // We need to load the class through the class 
                    // loader of the target object.
                    // 
                    final Class sclass = 
                        aLoader.loadClass(signature[i]);
                    if (sclass == null) continue;
                    else tab[i]=sclass;

                } else {
                    // Load through the default class loader
                    //
                    tab[i]= findClass(signature[i], 
                                      this.getClass().getClassLoader());
                }
            }
        } catch (ClassNotFoundException e) {
            logger.finest("findSignatureClasses",e);
            throw new ReflectionException(e, 
                      "The parameter class could not be found");
        } catch (RuntimeException e) {
            logger.finest("findSignatureClasses",e);
            throw e; 
        }
        return tab;
    }


    public Object instantiate(Class theClass) 
        throws ReflectionException, MBeanException {
        Object moi = null;


        Constructor cons = MetaDataImpl.findConstructor(theClass, null);
        if (cons == null) {
            throw new ReflectionException(new 
                NoSuchMethodException("No such constructor"));
        }
        // Instantiate the new object
        try {     
            moi= cons.newInstance((Object[])null);
        } catch (InvocationTargetException e) {
            // Wrap the exception.
            Throwable t = e.getTargetException();
            if (t instanceof RuntimeException) {
                throw new RuntimeMBeanException((RuntimeException)t, 
                   "RuntimeException thrown in the MBean's empty constructor");
            } else if (t instanceof Error) {
                throw new RuntimeErrorException((Error) t, 
                   "Error thrown in the MBean's empty constructor");
            } else {
                throw new MBeanException((Exception) t, 
                   "Exception thrown in the MBean's empty constructor");  
            }
        } catch (NoSuchMethodError error) {
            throw new ReflectionException(new 
                NoSuchMethodException("No constructor"), 
                                          "No such constructor");
        } catch (InstantiationException e) {
            throw new ReflectionException(e, 
            "Exception thrown trying to invoke the MBean's empty constructor");
        } catch (IllegalAccessException e) {
            throw new ReflectionException(e, 
            "Exception thrown trying to invoke the MBean's empty constructor");
        } catch (IllegalArgumentException e) {
            throw new ReflectionException(e, 
            "Exception thrown trying to invoke the MBean's empty constructor");
        }
        return moi;

    }

   

    public Object instantiate(Class theClass, Object params[], 
                              String signature[], ClassLoader loader)
        throws ReflectionException, MBeanException {
        // Instantiate the new object    

       final Class[] tab;
        Object moi= null;
        try {
            // Build the signature of the method
            //
            ClassLoader aLoader= (ClassLoader) theClass.getClassLoader();
            // Build the signature of the method
            //
            tab = 
                ((signature == null)?null:
                 findSignatureClasses(signature,aLoader));
            params = checkTransferParameters(aLoader,params);
        }
        // Exception IllegalArgumentException raised in Jdk1.1.8
        catch (IllegalArgumentException e) {
            throw new ReflectionException(e, 
                    "The constructor parameter classes could not be loaded");
        }
    
        // Query the metadata service to get the right constructor          
        Constructor cons = null;
        cons= MetaDataImpl.findConstructor(theClass, tab);
        
        if (cons == null) {
            throw new ReflectionException(new 
                NoSuchMethodException("No such constructor"));
        }
        try {
            moi = cons.newInstance(params);     
        } 
        catch (NoSuchMethodError error) {
            throw new ReflectionException(new 
                NoSuchMethodException("No such constructor found"), 
                                          "No such constructor" );
        }
        catch (InstantiationException e) {
            throw new ReflectionException(e, 
                "Exception thrown trying to invoke the MBean's constructor");
        }
        catch (IllegalAccessException e) {
            throw new ReflectionException(e, 
                "Exception thrown trying to invoke the MBean's constructor");
        }
        catch (InvocationTargetException e) {
            // Wrap the exception.         
            Throwable th = e.getTargetException();
            if (th instanceof RuntimeException) {
                throw new RuntimeMBeanException((RuntimeException)th, 
                      "RuntimeException thrown in the MBean's constructor");
            } else if (th instanceof Error) {
                throw new RuntimeErrorException((Error) th, 
                      "Error thrown in the MBean's constructor");   
            } else {
                throw new MBeanException((Exception) th, 
                      "Exception thrown in the MBean's constructor");
            }
        }       
        return moi;
    }

    public final AttributeList checkTransferAttributeList(
                    final ClassLoader dest, final AttributeList attributes) {
        final ClassLoader aLoader = dest;
        // No need to transfer...
        if (aLoader == null || attributes == null) return attributes;
        final int count = attributes.size();
        if (count == 0) return attributes;

        final Attribute[] attrs = (Attribute[]) 
            attributes.toArray( new Attribute[count]);

        for (int i = 0; i < count ; i++ ) {
            final Attribute attr = attrs[i];
            final Attribute newattr = checkTransferAttribute(aLoader,attr);
            if (attr == newattr) continue;
            attributes.set(i,newattr);
        }
        return attributes;
    }

    public Attribute checkTransferAttribute(final ClassLoader dest,
                                                   final Attribute attribute) {
        final ClassLoader aLoader = dest;
        // No need to transfer...
        if (aLoader == null || attribute == null) return attribute;
        
        final Object v = attribute.getValue();
        if (v == null) return attribute;

        // Check whether the value need to be transfered
        final Object newval = checkTransferObject(dest,v);
        if (newval == v) return attribute;

        // We need to replace the original value with the transfered value.
        return new Attribute(attribute.getName(),newval);
    }

    public Object[] checkTransferParameters(ClassLoader dest, 
                                            Object params[]) {
        final Object[] values=params;
        if (values == null) return params;
        final int len = values.length;
        if (len == 0) return params;
        for (int i=0;i<len;i++) {
            values[i]=checkTransferObject(dest,values[i]);
        }
        return values;
    }

    public Object checkTransferObject(ClassLoader targetLoader, 
                                      Object value) {
 
        // Simple check, we never know ...
        //
        if (value == null) return null;
        final ClassLoader t_loader = targetLoader;
        if (t_loader == null)
            return value;
        ClassLoader v_loader= value.getClass().getClassLoader();
        if (v_loader == null)
            // The value comes from the system class loader. It is ok.
            return value;
        if (t_loader.equals(v_loader))
            // The two classes come from the same class loader. It is ok.
            return value;
        else 
            // We need to "tranfer" the value within the context of the 
            // class loader of the object on which we have to apply the 
            // operation (target)
            //
            return transferObject(value, t_loader);
    }

    public Object transferObject(Object obj, ClassLoader aloader) {
        
        // Serialize the parameter        
        ByteArrayOutputStream arrayOut= new ByteArrayOutputStream();
    
        try {
            ObjectOutputStream out= new ObjectOutputStream(arrayOut);
            out.writeObject(obj);
            out.flush();
        } catch (Exception e) {
            return obj;
        } 
    
        // Start deserialization        
        try {
            ByteArrayInputStream  arrayIn= 
                new ByteArrayInputStream(arrayOut.toByteArray());
            ObjectInputStreamWithLoader in= 
                new ObjectInputStreamWithLoader (arrayIn, aloader);
            return in.readObject();
        } catch (Exception e) {
            return obj;
        }
    }  



    public ObjectInputStream deserialize(ObjectName name, byte[] data)
        throws InstanceNotFoundException,
        OperationsException {

        // Check parameter validity    
        if (data == null) {
            throw new  RuntimeOperationsException(new 
                IllegalArgumentException(), "Null data passed in parameter");
        }
        if (data.length == 0) {
            throw new  RuntimeOperationsException(new 
                IllegalArgumentException(), "Empty data passed in parameter");
        }
 
        // Get the MBean class loader
        ClassLoader loader = clr.getClassLoader(name);        
      
        // Object deserialization      
        ByteArrayInputStream bIn;
        ObjectInputStream    objIn;
        String               typeStr;

        bIn   = new ByteArrayInputStream(data);
        try {
            objIn = new ObjectInputStreamWithLoader(bIn,loader);
        } catch (IOException e) {
            throw new OperationsException(
                     "An IOException occurred trying to de-serialize the data");
        }
 
        return objIn;    
    }


    public ObjectInputStream deserialize(String className, byte[] data)
        throws OperationsException,
               ReflectionException  {

        // Check parameter validity
        if (data == null) {
            throw new  RuntimeOperationsException(new 
                IllegalArgumentException(), "Null data passed in parameter");
        }
        if (data.length == 0) {
            throw new  RuntimeOperationsException(new 
                IllegalArgumentException(), "Empty data passed in parameter");
        }
        if (className == null) {
            throw new  RuntimeOperationsException(
                              new IllegalArgumentException(), 
                              "Null className passed in parameter");
        }       

        // Load the class using the default loader repository    

        Class theClass = findClassWithDefaultLoaderRepository(className);
 
        // Object deserialization
        ByteArrayInputStream bIn;
        ObjectInputStream    objIn;
        String               typeStr;

        bIn   = new ByteArrayInputStream(data);
        try {
            objIn = 
                new ObjectInputStreamWithLoader(bIn,theClass.getClassLoader());
        } catch (IOException e) {
            throw new OperationsException(
                 "An IOException occurred trying to de-serialize the data");
        }
 
        return objIn;
    }
   

    public ObjectInputStream deserialize(String className,
                                         ObjectName loaderName,
                                         byte[] data,
                                         ClassLoader loader)
        throws InstanceNotFoundException,
               OperationsException,
               ReflectionException  {

        // Check parameter validity
        if (data == null) {
            throw new  RuntimeOperationsException(new 
                IllegalArgumentException(), "Null data passed in parameter");
        }
        if (data.length == 0) {
            throw new  RuntimeOperationsException(new 
                IllegalArgumentException(), "Empty data passed in parameter");
        }
        if (className == null) {
            throw new  RuntimeOperationsException(new 
             IllegalArgumentException(), "Null className passed in parameter");
        }       
        Class theClass = null;
        if (loaderName == null) {
            // Load the class using the agent class loader
            theClass = findClass(className, loader);
        
        } else {
            // Get the class loader MBean
            try {
                Object instance = null;
                
                if (clr!=null)  
                    instance = clr.getClassLoader(loaderName);
                if (instance == null) 
                    throw new ClassNotFoundException(className);
                theClass = ((ClassLoader)instance).loadClass(className);
            }
            catch (ClassNotFoundException e) {
                throw new ReflectionException(e, 
                               "The MBean class could not be loaded by the " + 
                               loaderName.toString() + " class loader");
            }
        }
 
        // Object deserialization
        ByteArrayInputStream bIn;
        ObjectInputStream    objIn;
        String               typeStr;
        
        bIn   = new ByteArrayInputStream(data);
        try {
            objIn = new ObjectInputStreamWithLoader(bIn,
                                           theClass.getClassLoader());
        } catch (IOException e) {
            throw new OperationsException(
                    "An IOException occurred trying to de-serialize the data");
        }
        
        return objIn;
    }


    public Object instantiate(String className)
        throws ReflectionException,
        MBeanException {

        return instantiate(className, (Object[]) null, (String[]) null, null);
    }


    public Object instantiate(String className, ObjectName loaderName, 
                              ClassLoader loader) 
        throws ReflectionException, MBeanException,
               InstanceNotFoundException {

        return instantiate(className, loaderName, (Object[]) null, 
                           (String[]) null, loader);
    }


    public Object instantiate(String className,
                              Object params[],
                              String signature[],
                              ClassLoader loader) 
        throws ReflectionException,
        MBeanException {

        Class theClass = findClassWithDefaultLoaderRepository(className);
        return instantiate(theClass, params, signature, loader);
    }



    public Object instantiate(String className,
                              ObjectName loaderName,
                              Object params[],
                              String signature[],
                              ClassLoader loader) 
        throws ReflectionException,
               MBeanException,
        InstanceNotFoundException {

        Class theClass;
        
        if (loaderName == null) {
            theClass = findClass(className, loader);
        } else {
            theClass = findClass(className, loaderName);        
        }       
        return instantiate(theClass, params, signature, loader);
    }


    public ModifiableClassLoaderRepository getClassLoaderRepository() {
        return clr;
    }

    private final ClassLogger logger = 
        new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
                        "MBeanInstantiatorImpl");
    
}
