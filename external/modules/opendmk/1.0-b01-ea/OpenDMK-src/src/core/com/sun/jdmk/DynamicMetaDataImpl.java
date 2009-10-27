/*
 * @(#)file      DynamicMetaDataImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.16
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
import java.util.Iterator;
import java.io.PrintWriter;
import java.io.StringWriter;

// RI import
import javax.management.* ; 
import com.sun.jdmk.internal.ClassLogger;



/**
 * The DynamicMetaDataImpl class provides local access to the metadata 
 * service in an agent. 
 * The DynamicMetaDataImpl only handles DynamicMBeans.
 *
 * @deprecated This class is kept as a utility class, though it is no longer
 *      used. It may not be supported in future releases.
 * @since Java DMK 5.0
 */
public class DynamicMetaDataImpl implements MetaData {

    /** The name of this class to be used for tracing */
    private final static String dbgTag = "DynamicMetaDataImpl";
    

    /**
     * The MBeanInstantiator associated to the MetaData
     */
    protected final MBeanInstantiator instantiator;
 
    /**
     * Creates a Metadata Service.
     * @param instantiator The MBeanInstantiator that will be used to
     *        take care of class loading issues.
     *        This parameter may not be null.
     * @exception IllegalArgumentException if the instantiator is null.
     */
    DynamicMetaDataImpl(MBeanInstantiator instantiator)  {

        this.instantiator = instantiator;
        if (instantiator == null) throw new 
            IllegalArgumentException("instantiator must not be null.");
    } 

    
    /** 
     * This methods tests if the MBean is JMX compliant
     */    
    public void testCompliance(Class c) 
        throws NotCompliantMBeanException {
        if (DynamicMBean.class.isAssignableFrom(c)) return;
        throw new NotCompliantMBeanException(
               "Only DynamicMBeans are supported by this implementation");
    }

  
    //---------------------------------------------------------------------
    //
    // From the MetaData interface
    //
    //---------------------------------------------------------------------

    public ObjectName preRegisterInvoker(Object moi, ObjectName name, 
                                         MBeanServer mbs) 
        throws InstanceAlreadyExistsException, MBeanRegistrationException {
   
        if (!(moi instanceof MBeanRegistration)) return name;

        ObjectName logicalName=null;
        ObjectName newName=null;
      
        try {
            newName=(ObjectName)
                ((MBeanRegistration)moi).preRegister(mbs, name);
        } catch (RuntimeException e) {
                throw new RuntimeMBeanException((RuntimeException)e, 
                           "RuntimeException thrown in preRegister method");       
        } catch (Error er) {      
                throw new RuntimeErrorException((Error) er, 
                           "Error thrown in preRegister method");
        } catch (MBeanRegistrationException r) {
            throw (MBeanRegistrationException)r;
        } catch (Exception ex) {
            throw new MBeanRegistrationException((Exception) ex, 
                          "Exception thrown in preRegister method");
        }      
        
        if (name!=null) logicalName=name;
        else logicalName=newName;       
        
        return logicalName;    
    }


 
    public void postRegisterInvoker(Object moi, boolean registrationDone) {
        if (!(moi instanceof MBeanRegistration)) return;

        try {
            ((MBeanRegistration)moi).
                postRegister(new Boolean(registrationDone));
        } catch (RuntimeException e) {
            throw new RuntimeMBeanException((RuntimeException)e,  
                      "RuntimeException thrown in postRegister method");   
        } catch (Error er) {
            throw new RuntimeErrorException((Error) er,  
                      "Error thrown in postRegister method");       
        }
    }
    
   

    public void preDeregisterInvoker(Object moi) 
        throws MBeanRegistrationException {
        if (!(moi instanceof MBeanRegistration)) return;

        try {
            ((MBeanRegistration)moi).preDeregister();
        } catch (RuntimeException e) {
            throw new RuntimeMBeanException((RuntimeException) e,  
                         "RuntimeException thrown in preDeregister method");
        } catch (Error er) {         
            throw new RuntimeErrorException((Error) er,  
                         "Error thrown in preDeregister method");     
        } catch (MBeanRegistrationException t) {
            throw (MBeanRegistrationException)t;   
        } catch (Exception ex) {
            throw new MBeanRegistrationException((Exception)ex,  
                         "Exception thrown in preDeregister method"); 
        }
    }

  
    public void postDeregisterInvoker(Object moi) {
        if (!(moi instanceof MBeanRegistration)) return;

        try {
            ((MBeanRegistration)moi).postDeregister();
        } catch (RuntimeException e) {
            throw new RuntimeMBeanException((RuntimeException)e, 
                         "RuntimeException thrown in postDeregister method"); 
        } catch (Error er) {
            throw new RuntimeErrorException((Error) er, 
                         "Error thrown in postDeregister method"); 
        } 
    }

    public String getMBeanClassName(Object moi) 
        throws IntrospectionException, NotCompliantMBeanException {

        // Ask the MBeanInfo for the class name
        final MBeanInfo mbi = getMBeanInfo(moi);
                
        if (mbi == null) {
            throw new 
                NotCompliantMBeanException("The MBeanInfo returned is null");
        }
                
        final String className = mbi.getClassName();
                
        if (className == null) {
            throw new 
                IntrospectionException("The class Name returned is null");
        }

        return className;
    }

    public MBeanInfo getMBeanInfo(Object moi) 
        throws IntrospectionException {

        try {
            return (MBeanInfo) 
                ((javax.management.DynamicMBean)moi).getMBeanInfo();
        } catch (RuntimeException r) {
            logger.finest("getMBeanInfo",r);
            throw new RuntimeMBeanException((RuntimeException)r, 
           "Runtime Exception thrown by getMBeanInfo method of Dynamic MBean");
        } catch (Error e ) {
            logger.finest("getMBeanInfo",e);
            throw new RuntimeErrorException((Error)e, 
                      "Error thrown by getMBeanInfo method of Dynamic MBean");
        }

    }

    public Object getAttribute(Object instance, String attribute)
        throws MBeanException, AttributeNotFoundException, 
               ReflectionException {
        if (attribute == null) {
            final RuntimeException r = 
                new IllegalArgumentException("Attribute name cannot be null");
            throw new RuntimeOperationsException(r, 
                "Exception occurred trying to invoke the getter on the MBean");
        }  
 
        try {
            return ((javax.management.DynamicMBean)instance).
                getAttribute(attribute);
        } catch (RuntimeException e) {
            logger.finest("getAttribute",e);
            throw new RuntimeOperationsException(e, "RuntimeException" +
                    " thrown by the getAttribute method of the DynamicMBean" +
                    " for the attribute " + attribute);
        } catch (Error e) {
            logger.finest("getAttribute",e);
            throw new RuntimeErrorException((Error)e, "Error" + 
                    " thrown by the getAttribute method of the DynamicMBean "+
                    " for the attribute " + attribute);                  
        }
    }

    public AttributeList getAttributes(Object instance, String[] attributes) 
        throws ReflectionException {

        if (attributes == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Attributes cannot be null"), 
                "Exception occurred trying to invoke the getter on the MBean");
        }

        try {
            return ((javax.management.DynamicMBean)instance).
                getAttributes(attributes);
        } catch (RuntimeException e) {
            logger.finest("getAttributes",e);
            throw new RuntimeOperationsException(e, "RuntimeException" +
                   " thrown by the getAttributes method of the DynamicMBean");
        } catch (Error e) {
            logger.finest("getAttributes",e);
            throw new RuntimeErrorException((Error)e, "Error" + 
                   " thrown by the getAttributes method of the DynamicMBean");
        }
    }


    public AttributeList setAttributes(Object instance, 
                                       AttributeList attributes) 
        throws ReflectionException {

        final Class objClass = instance.getClass();
        final ClassLoader aLoader = objClass.getClassLoader();
        
        try {
            final AttributeList attlist = 
                instantiator.checkTransferAttributeList(aLoader,attributes);
            return ((javax.management.DynamicMBean)instance).
                setAttributes(attlist);
        } catch (RuntimeException e) {
            logger.finest("setAttributes",e);
            throw new RuntimeOperationsException(e, 
                     "RuntimeException thrown by the setAttributes " + 
                     "method of the Dynamic MBean");
        } catch (Error e) {
            logger.finest("setAttributes",e);
            throw new RuntimeErrorException((Error)e, 
                      "Error thrown by the setAttributes " + 
                      "method of the Dynamic MBean");
        }
    }
    

    public Object setAttribute(Object instance, Attribute attribute)  
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

        try {
            final Attribute attr = 
                instantiator.checkTransferAttribute(aLoader, attribute);
            ((javax.management.DynamicMBean)instance).
                setAttribute(attr);
            return attr.getValue();
        } catch (RuntimeException e) {
            logger.finest("setAttribute",e);
            throw new RuntimeOperationsException(e, 
                      "RuntimeException thrown by the setAttribute " + 
                      attribute + "method of the Dynamic MBean");
        } catch (Error e) {
            logger.finest("setAttribute",e);
            throw new RuntimeErrorException((Error)e, 
                      "Error thrown by the setAttribute " + attribute + 
                      "method of the Dynamic MBean");
        }
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

        final Class objClass = instance.getClass();
        final ClassLoader aLoader = objClass.getClassLoader();

        try {
            final Object[] args = 
                instantiator.checkTransferParameters(aLoader,params);
            return (((javax.management.DynamicMBean)instance).
                    invoke(operationName, args, signature));
        } catch (RuntimeException e) {
            logger.finest("invoke",e);
            throw new RuntimeOperationsException(e, "RuntimeException" +
                      " thrown by the invoke method of the Dynamic MBean");
        } catch (Error e) {
            logger.finest("invoke",e);
            throw new RuntimeErrorException((Error)e, "Error" + 
                     " thrown by the invoke method of the Dynamic MBean");
        } catch (ReflectionException e) {
            logger.finest("invoke",e);
            throw e;
        } catch (MBeanException e) {
            logger.finest("invoke",e);
            throw e;
        }
    }

    public boolean isInstanceOf(Object instance, String className) 
        throws ReflectionException {

        try {
            final String cn = getMBeanClassName(instance);
            if (cn != null)
                return cn.equals(className);
            else throw new 
                IntrospectionException("Can't obtain MBean class name for "+
                                       instance.getClass().getName());
        } catch (IntrospectionException x) {
            logger.finest("isInstanceOf",x);
            throw new ReflectionException(x,x.getMessage());
        } catch (NotCompliantMBeanException x) {
            logger.finest("isInstanceOf",x);
            throw new ReflectionException(x,x.getMessage());
        }
    }

    private static final ClassLogger logger = 
        new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
                        "DynamicMetaDataImpl");

 }
