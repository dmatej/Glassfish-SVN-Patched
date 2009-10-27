/*
 * @(#)file      DefaultMBeanInterceptor.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.32
 * @(#)lastedit  07/03/08
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
import java.util.Set;
import java.util.HashSet;
import java.util.WeakHashMap;
import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException; 
import java.lang.reflect.Method; 
import java.lang.reflect.Constructor;
import java.io.OptionalDataException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.IOException;

// JMX import
import javax.management.MBeanServerDelegate;
import javax.management.*; 
import javax.management.loading.ClassLoaderRepository;

import com.sun.jdmk.interceptor.MBeanServerInterceptor;
import com.sun.jdmk.internal.ClassLogger;


/**
 * This is the default class for MBean manipulation on the agent side. It
 * contains the methods necessary for the creation, registration, and
 * deletion of MBeans as well as the access methods for registered MBeans.
 * This is the core component of the JMX infrastructure.
 * <P>
 * Every MBean which is added to the MBean server becomes manageable: 
 * its attributes and operations become remotely accessible through 
 * the connectors/adaptors connected to that MBean server. 
 * A Java object cannot be registered in the MBean server unless it is a 
 * JMX compliant MBean.
 * <P>
 * When an MBean is registered or unregistered in the MBean server an 
 * {@link javax.management.MBeanServerNotification MBeanServerNotification} 
 * Notification is emitted. To register an object as listener to MBeanServerNotifications
 * you should call the MBean server method {@link #addNotificationListener addNotificationListener} with <CODE>ObjectName</CODE>
 * the <CODE>ObjectName</CODE> of the {@link javax.management.MBeanServerDelegate MBeanServerDelegate}. 
 * This <CODE>ObjectName</CODE> is:
 * <BR>
 * <CODE>JMImplementation:type=MBeanServerDelegate</CODE>.
 *
 * @since Java DMK 5.0
 * @deprecated This class is kept as a utility class, though it is no longer
 *      used. It may not be supported in future releases. The 
 *      {@link com.sun.jdmk.JdmkMBeanServer} now uses 
 *      {@link com.sun.jdmk.interceptor.DefaultMBeanServerInterceptor}
 */
public class DefaultMBeanInterceptor implements MBeanServerInterceptor {

    /** MBeanServerDelegate ObjectName shared ref */
    private final static ObjectName _MBSDelegateObjectName;
    static {
        try {
            _MBSDelegateObjectName = new ObjectName(ServiceName.DELEGATE);
        } catch (MalformedObjectNameException e) {
            throw new UnsupportedOperationException(e.getMessage());
        }
    }

    /** The MBeanInstantiator object used by the DefaultMBeanInterceptor */
    private transient MBeanInstantiator instantiator = null;

    /** The MBean server object that associated to the 
        DefaultMBeanInterceptor */
    private transient MBeanServer server = null;

    /** The Metadata object used by the DefaultMBeanInterceptor */
    private transient MetaData meta = null;

    /** The Repository object used by the DefaultMBeanInterceptor */
    private transient Repository repository = null;

    /** Wrappers for client listeners.  */
    /* See the comment before addNotificationListener below.  */
    private transient WeakHashMap listenerWrappers = new WeakHashMap();

    /** The default domain of the object names */
    private String domain = null;

    /** True if the repository perform queries, false otherwise */
    private boolean queryByRepo;
    
    /** The sequence number identifyng the notifications sent */
    // Now sequence number is handled by MBeanServerDelegate.
    // private int sequenceNumber=0;

    /** The name of this class to be used for tracing */
    private final static String dbgTag = "DefaultMBeanInterceptor";
    private static final ClassLogger logger = 
        new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
                        dbgTag);
     
    
    /**
     * Creates a DefaultMBeanInterceptor with a standard default domain name.     
     * The default domain name is used as the domain part in the ObjectName
     * of MBeans, if no domain is specified by the user.
     * <p>
     * The standard default domain name is defined in {@link com.sun.jdmk.ServiceName#DOMAIN ServiceName.DOMAIN}
     */
    public DefaultMBeanInterceptor(MBeanServer server)  {
        initialize(server, ((MBeanServerInt)server).getMetaData(),
                   ((MBeanServerInt)server).getMBeanInstantiator(), 
                   (String)null, (Repository)null);
    } 

    /**
     * Creates a DefaultMBeanInterceptor with the specified default domain name.
     * The default domain name is used as the domain part in the ObjectName
     * of MBeans if no domain is specified by the user.
     */
    public DefaultMBeanInterceptor(MBeanServer server, String domain)  {
        initialize(server, ((MBeanServerInt)server).getMetaData(), 
                   ((MBeanServerInt)server).getMBeanInstantiator(), domain, 
                   (Repository)null);
    }


    /**
     * Creates a DefaultMBeanInterceptor with the specified default domain name.
     * and the specified object repository.
     * The default domain name is used as the domain part in the ObjectName
     * of MBeans if no domain is specified by the user.
     */
    public DefaultMBeanInterceptor(MBeanServer server, String domain,
                                Repository repository)  {
        initialize(server, ((MBeanServerInt)server).getMetaData(),
                   ((MBeanServerInt)server).getMBeanInstantiator(), 
                   domain, repository);
    }


    public ObjectInstance createMBean(String className, ObjectName name)
        throws ReflectionException, InstanceAlreadyExistsException, 
               MBeanRegistrationException, MBeanException, 
               NotCompliantMBeanException { 

        return createMBean(className, name, (Object[]) null,(String[]) null); 
        
    }

 
    public ObjectInstance createMBean(String className, ObjectName name,
                                      ObjectName loaderName) 
        throws ReflectionException, InstanceAlreadyExistsException, 
               MBeanRegistrationException, MBeanException, 
               NotCompliantMBeanException, InstanceNotFoundException {

        return createMBean(className, name, loaderName, (Object[]) null,
                           (String[]) null);
    }


    public ObjectInstance createMBean(String className, ObjectName name, 
                                      Object params[], String signature[]) 
        throws ReflectionException, InstanceAlreadyExistsException, 
               MBeanRegistrationException, MBeanException, 
               NotCompliantMBeanException  {

        ObjectName logicalName = name;
        Class theClass;


        if (className == null) {
            final RuntimeException wrapped =
                new IllegalArgumentException("The class name cannot be null");
            throw new RuntimeOperationsException(wrapped, 
                      "Exception occurred during MBean creation");
        }  
            
        // Test  object name   
        if ((name !=null) && (name.isPattern() == true)) {
            final RuntimeException wrapped =
                new IllegalArgumentException("Invalid name->" +
                                             name.toString());
            throw new RuntimeOperationsException(wrapped, 
                                 "Exception occurred during MBean creation");
        }   
    
        String nameMessage = "null";
        if (name!=null) {
            nameMessage = name.toString();
        }

        if (logger.finerOn()) {
            logger.finer("createMBean", "ClassName = " + className + 
                  ",ObjectName = " + nameMessage);
        }

        theClass = 
            instantiator.findClassWithDefaultLoaderRepository(className);      
            
        // Check that the MBean can be instantiated by the MBeanServer.
        instantiator.testCreation(theClass);

        // Check the JMX compliance of the class        
        meta.testCompliance(theClass); 
            
        Object moi= instantiator.instantiate(theClass, params,  signature, 
                                             this.getClass().getClassLoader());
            
        final ObjectName actualName = registerCreatedObject(moi, name);

        return makeObjectInstance(className, moi, actualName); 
    }


    public ObjectInstance createMBean(String className, ObjectName name, 
                                      ObjectName loaderName, Object params[], 
                                      String signature[]) 
        throws ReflectionException, InstanceAlreadyExistsException, 
               MBeanRegistrationException, MBeanException, 
               NotCompliantMBeanException, InstanceNotFoundException { 

        Class theClass;
            
        if (className == null) {
            final RuntimeException wrapped = 
                new IllegalArgumentException("The class name cannot be null");
            throw new RuntimeOperationsException(wrapped, 
                            "Exception occurred during MBean creation");
        }  
            
        // Test  object name   
        if ((name !=null) && (name.isPattern() == true)) {
            final RuntimeException wrapped =
                new IllegalArgumentException("Invalid name->" + 
                                             name.toString());
            throw new RuntimeOperationsException(wrapped, 
                   "Exception occurred during MBean registration");
        }   
    
        // The class loader service must be available               
        if (loaderName == null) {
            String nameMessage = "null";
            if (name!=null) {
                nameMessage = name.toString();
            }
            if (logger.finerOn()) {
                logger.finer("createMBean", "ClassName = " + className + 
                      ",ObjectName = " + nameMessage + " Loader name = null");
            }    

            theClass = instantiator.findClass(className, 
                                  this.getClass().getClassLoader());
        
        } else {
            String nameMessage = "null";
            if (name!=null) {
                nameMessage = name.toString();
            }
            if (logger.finerOn()) {
                logger.finer("createMBean", "ClassName = " + className + 
                      ",ObjectName = " + nameMessage + ",Loader name = "+ 
                      loaderName.toString());
            }      
  
            theClass = instantiator.findClass(className, loaderName);
        
        } 

        // Check that the MBean can be instantiated by the MBeanServer.
        instantiator.testCreation(theClass);

        // Check the JMX compliance of the class
        meta.testCompliance(theClass);

        Object moi = instantiator.instantiate(theClass, params,  signature, 
                                        this.getClass().getClassLoader());

        final ObjectName actualName = registerCreatedObject(moi, name);

        return makeObjectInstance(className, moi, actualName); 
    }


    public ObjectInstance registerMBean(Object object, ObjectName name) 
        throws InstanceAlreadyExistsException, MBeanRegistrationException,
        NotCompliantMBeanException  { 

        ObjectName logicalName = name;

        Class theClass   = object.getClass();   
        String className = theClass.getName();

        // Check the JMX compliance of the class        
        meta.testCompliance(theClass); 

        final ObjectName actualName = registerObject(object, name);

        return makeObjectInstance(className, object, actualName); 
    } 


    public void unregisterMBean(ObjectName name) 
        throws InstanceNotFoundException, MBeanRegistrationException  {    
        Object object;

        if (name == null) {
            final RuntimeException wrapped = 
                new IllegalArgumentException("Object name cannot be null");
            throw new RuntimeOperationsException(wrapped, 
                      "Exception occurred trying to unregister the MBean");
        }     
  
        synchronized(this) {
            object = repository.retrieve(name);
            if (object==null) {         
                if (logger.finerOn()) {
                    logger.finer("unregisterMBean", name+": Found no object");
                }           
                throw new InstanceNotFoundException(name.toString());          
            }   
            if (object instanceof MBeanRegistration) {
                meta.preDeregisterInvoker(object);      
            }
            // Let the repository do the work.   
            try {
                repository.remove(name);            
            }
            catch (InstanceNotFoundException e) {
                throw e;
            }   
        
            // ---------------------
            // Send deletion event
            // ---------------------
            if (logger.finerOn()) {
                logger.finer("unregisterMBean", "Send delete notification of object "
                      + name.getCanonicalName());
            }       
            sendNotification(MBeanServerNotification.
                             UNREGISTRATION_NOTIFICATION, name ) ;

            if (object instanceof MBeanRegistration) {
                meta.postDeregisterInvoker(object);      
            }
  
            /**
             * Checks if the unregistered MBean is a ClassLoader
             * If so, it removes the  MBean from the default loader repository.
             */

            if (object instanceof ClassLoader) {
                final ModifiableClassLoaderRepository clr = 
                    instantiator.getClassLoaderRepository();
                if (clr != null) clr.removeClassLoader(name);
            }
        }
    }
 

   

    public ObjectInstance getObjectInstance(ObjectName name) 
        throws InstanceNotFoundException {

        Object obj = getMBean(name);
        final String className;
        try {
            className = meta.getMBeanClassName(obj);
        } catch (IntrospectionException x) {
            logger.finest("getObjectInstance",x);
            throw new JMRuntimeException("Can't obtain class name for " +
                                         name + ": " + x);
        } catch (NotCompliantMBeanException x) {
            logger.finest("getObjectInstance",x);
            throw new JMRuntimeException("Can't obtain class name for " +
                                         name + ": " + x);
        }
        return new ObjectInstance(name, className);
    }

  
    public Set queryMBeans(ObjectName name, QueryExp query) {
        final Set list; 
        synchronized(this) {
            list =  repository.query(name, query);
        }
        // The repository performs the filtering
        if (queryByRepo) {
            return list;
        } else {
            // The filtering will be performed by the MBeanServer
            return(filterListOfObjects(list, query));
        }   
    } 

  
    public Set queryNames(ObjectName name, QueryExp query) {

        final HashSet result = new HashSet();    
        final Set nos = queryMBeans(name, query);
        for (final Iterator i  = nos.iterator(); i.hasNext(); ) {
            final ObjectInstance oi     = (ObjectInstance) i.next();
            result.add(oi.getObjectName());     
        }
        return (result);        
    } 

    public String[] getDomains() {
        
        final Set names;
        synchronized(this) {
            names = repository.query(null,null);
        }
        final Set tmpSet = new HashSet(1);
        for (final Iterator i = names.iterator() ; i.hasNext() ; ) {
            final ObjectInstance oi  = (ObjectInstance) i.next();
            final ObjectName x = oi.getObjectName();
            final String domain = x.getDomain();
            if (tmpSet.contains(domain)) continue;
            tmpSet.add(domain);
        }
        final String[] result = new String[tmpSet.size()];
        return (String[]) tmpSet.toArray(result);
    }

    public boolean isRegistered(ObjectName name)  { 
        if (name == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Object name cannot be null"), 
                "Object name cannot be null");
        }  

        synchronized(this) {
            return (repository.contains(name));
        }
    } 


    public Integer getMBeanCount()  { 
        return (repository.getCount());
    } 

 

    public Object getAttribute(ObjectName name, String attribute) 
        throws MBeanException, AttributeNotFoundException, 
               InstanceNotFoundException, ReflectionException { 

        if (name == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Object name cannot be null"), 
                "Exception occurred trying to invoke the getter on the MBean");
        }    
        if (attribute == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Attribute cannot be null"), 
                "Exception occurred trying to invoke the getter on the MBean");
        }    
        if (logger.finerOn()) {
            logger.finer("getAttribute", "Attribute= " + attribute + 
                  ", obj= " + name);
        }         
        final Object instance = getMBean(name);
        return meta.getAttribute(instance, attribute);
    } 

  
    public AttributeList getAttributes(ObjectName name, String[] attributes)
        throws InstanceNotFoundException, ReflectionException  {
  

        if (name == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("ObjectName name cannot be null"),
                "Exception occurred trying to invoke the getter on the MBean");
        } 

        if (attributes == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Attributes cannot be null"), 
                "Exception occurred trying to invoke the getter on the MBean");
        } 

        // Check if we really have to work ...
        //
        if (attributes.length == 0) return new AttributeList(0);
        if (logger.finerOn()) {
            logger.finer("getAttributes", "Object= " + name);
        }          
        final Object obj = getMBean(name);
        return meta.getAttributes(obj,attributes);
    } 


    public void setAttribute(ObjectName name, Attribute attribute) 
        throws InstanceNotFoundException, AttributeNotFoundException, 
               InvalidAttributeValueException, MBeanException, 
               ReflectionException  { 

        if (name == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("ObjectName name cannot be null"), 
                "Exception occurred trying to invoke the setter on the MBean");
        } 
   
        if (attribute == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Attribute cannot be null"), 
                "Exception occurred trying to invoke the setter on the MBean");
        } 
        if (logger.finerOn()) {
            logger.finer("setAttribute", "Object= " + name + ", attribute=" + 
                  attribute.getName());
        }          
        final Object instance = getMBean(name);
        final Object o = meta.setAttribute(instance, attribute);
    }

  
    public AttributeList setAttributes(ObjectName name, 
                                       AttributeList attributes)
        throws InstanceNotFoundException, ReflectionException  { 

        if (name == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("ObjectName name cannot be null"), 
                "Exception occurred trying to invoke the setter on the MBean");
        } 
        if (attributes == null) {
            throw new RuntimeOperationsException(new 
            IllegalArgumentException("AttributeList  cannot be null"), 
            "Exception occurred trying to invoke the setter on the MBean");
        } 

        if (attributes.isEmpty()) return attributes;

        final Object obj = getMBean(name);
        return meta.setAttributes(obj,attributes);           
    }

 

    public Object invoke(ObjectName name, String operationName, 
                         Object params[], String signature[]) 
        throws InstanceNotFoundException, MBeanException, 
               ReflectionException { 

        final Object obj = getMBean(name);
        return meta.invoke(obj, operationName, params, signature);
    }
 

    /**
     * Return the MetaData service object used by this interceptor.
     *
     **/
    protected MetaData meta() {
        return meta;
    }

    /**
     * Builds an ObjectInstance.
     * <ul>
     * <li> If the given <code>object</code> implements DynamicMBean,
     *      then ask its MBeanInfo for the class name.</li>
     * <li> Otherwise, uses the provided <code>className</code></li>
     * </ul>
     *
     * @return A new ObjectInstance for the given <code>object</code>.
     * @exception NotCompliantMBeanException if the <code>object</code>
     *            implements DynamicMBean but the class name can't be
     *            retrieved from its MBeanInfo.
     **/
    protected ObjectInstance makeObjectInstance(String className, 
                                                Object object, 
                                                ObjectName name) 
        throws NotCompliantMBeanException {

        // if the MBean is a dynamic MBean ask its MBeanInfo for the 
        // class name
        if (object instanceof DynamicMBean) {
            try {
                className = meta.getMBeanClassName(object);
            } catch (IntrospectionException x) {
                logger.finest("makeObjectInstance",x);
                throw new NotCompliantMBeanException(
                           "Can't obtain class name for " + name + ": " + x);
            } catch (JMRuntimeException x) {
                logger.finest("makeObjectInstance",x);
                throw new NotCompliantMBeanException(
                           "Can't obtain class name for " + name + ": " + x);
            }
        }
                
        if (className == null) {
            throw new NotCompliantMBeanException(
                             "The class Name returned is null");
        }

        return(new ObjectInstance(name, className));
    }

    /**
     * Register <code>object</code> in the repository, with the 
     * given <code>name</code>.
     * This method is called by the various createMBean() flavors
     * and by registerMBean() after all MBean compliance tests
     * have been performed.
     * <p>
     * This method does not performed any kind of test compliance,
     * and the caller should make sure that the given <code>object</object>
     * is MBean compliant.
     * <p>
     * This methods performed all the basic steps needed for object
     * registration:
     * <ul>
     * <li>If the <code>object</code> implements the MBeanRegistration
     *     interface, it invokes preRegister() on the object.</li>
     * <li>Then the object is added to the repository with the given
     *     <code>name</code>.</li>
     * <li>Finally, if the <code>object</code> implements the 
     *     MBeanRegistration interface, it invokes postRegister() 
     *     on the object.</li>
     * </ul>
     * @param object A reference to a MBean compliant object.
     * @param name   The ObjectName of the <code>object</code> MBean.
     * @return the actual ObjectName with which the object was registered.
     * @exception InstanceAlreadyExistsException if an object is already
     *            registered with that name.
     * @exception MBeanRegistrationException if an exception occurs during
     *            registration.
     **/
    protected ObjectName registerObject(Object object, ObjectName name) 
        throws InstanceAlreadyExistsException, MBeanRegistrationException {
        ObjectName logicalName = name;
        if (object == null) {    
            final RuntimeException wrapped = 
                new IllegalArgumentException("Cannot add null object");
            throw new RuntimeOperationsException(wrapped, 
                        "Exception occurred trying to register the MBean");
        }
        
        String nameMessage = "null";
        if (name!=null) {
            nameMessage = name.toString();
        }
        if (logger.finerOn()) {
            logger.finer("registerMBean", "ObjectName = " + nameMessage);
        }          
        
        ObjectName res = null;
        if (object instanceof MBeanRegistration) {
            logicalName=meta.preRegisterInvoker(object, name, server);
        }
   
 
        if (logicalName!=null) {
            internal_addObject(object, logicalName);
        } else {
            if (object instanceof MBeanRegistration ) {            
                meta.postRegisterInvoker(object, false);        
            }
            final RuntimeException wrapped = 
                new IllegalArgumentException("No object name specified");
            throw new RuntimeOperationsException(wrapped, 
                        "Exception occurred trying to register the MBean");
        }       
        if (object instanceof MBeanRegistration)     
            meta.postRegisterInvoker(object, true);

        /**
         * Checks if the newly registered MBean is a ClassLoader
         * If so, it adds the  MBean in the default loader repository.
         */     
        if (object instanceof ClassLoader) {
            final ModifiableClassLoaderRepository clr = 
                instantiator.getClassLoaderRepository();
            if (clr == null) {
                final RuntimeException wrapped = 
                    new IllegalArgumentException(
                     "Dynamic addition of class loaders is not supported");
                throw new RuntimeOperationsException(wrapped, 
           "Exception occurred trying to register the MBean as a class loader");
            }
            clr.addClassLoader(logicalName, (ClassLoader)object);
        }

        return logicalName;
    } 

    /**
     * Register an object from within createMBean().
     * This method wraps registerObject() and is only called from within
     * createMBean().
     * It calls directly registerObject(). Its only purpose is to provide
     * hooks for derived classes.
     **/
    protected ObjectName registerCreatedObject(Object object, ObjectName name) 
        throws InstanceAlreadyExistsException, MBeanRegistrationException {
        return registerObject(object,name);
    }


    /**
     * Gets a specific MBean controlled by the DefaultMBeanInterceptor.
     */
    private Object getMBean(ObjectName name)  
        throws InstanceNotFoundException {

        if (name == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Object name cannot be null"), 
                               "Exception occurred trying to get an MBean");
        } 
        if (name.getDomain().equals("")) {
            try {
                name = new ObjectName(domain + name.toString());
            } catch (MalformedObjectNameException e) {
                if (logger.finestOn()) {
                    logger.finest("getMBean", 
                          "Unexpected MalformedObjectNameException");
                }       
            }
        }
        Object obj = null;
        synchronized(this) {
            obj = repository.retrieve(name);    
            if (obj == null) {         
                if (logger.finerOn()) {
                    logger.finer("getMBean", name+": Found no object");
                }                       
                throw new InstanceNotFoundException(name.toString());          
            }  
        }
        return obj;
    }
   
 
    public String getDefaultDomain()  { 
        return domain;
    } 

    /*
     * Notification handling.
     *
     * This is not trivial, because the MBeanServer translates the
     * source of a received notification from a reference to an MBean
     * into the ObjectName of that MBean.  While that does make
     * notification sending easier for MBean writers, it comes at a
     * considerable cost.  We need to replace the source of a
     * notification, which is basically wrong if there are also
     * listeners registered directly with the MBean (without going
     * through the MBean server).  We also need to wrap the listener
     * supplied by the client of the MBeanServer with a listener that
     * performs the substitution before forwarding.  This is why we
     * strongly discourage people from putting MBean references in the
     * source of their notifications.  Instead they should arrange to
     * put the ObjectName there themselves.
     *
     * However, existing code relies on the substitution, so we are
     * stuck with it.
     *
     * Here's how we handle it.  When you add a listener, we make a
     * ListenerWrapper around it.  We look that up in the
     * listenerWrappers map, and if there was already a wrapper for
     * that listener with the given ObjectName, we reuse it.  This map
     * is a WeakHashMap, so a listener that is no longer registered
     * with any MBean can be garbage collected.
     *
     * We cannot use simpler solutions such as always creating a new
     * wrapper or always registering the same listener with the MBean
     * and using the handback to find the client's original listener.
     * The reason is that we need to support the removeListener
     * variant that removes all (listener,filter,handback) triples on
     * a broadcaster that have a given listener.  And we do not have
     * any way to inspect a broadcaster's internal list of triples.
     * So the same client listener must always map to the same
     * listener registered with the broadcaster.
     *
     * Another possible solution would be to map from ObjectName to
     * list of listener wrappers (or IdentityHashMap of listener
     * wrappers), making this list the first time a listener is added
     * on a given MBean, and removing it when the MBean is removed.
     * This is probably more costly in memory, but could be useful if
     * some day we don't want to rely on weak references.
     */

    public void addNotificationListener(ObjectName name, 
                                        NotificationListener listener, 
                                        NotificationFilter filter, 
                                        Object handback)
            throws InstanceNotFoundException {

        if (logger.finerOn()) {
            logger.finer("addNotificationListener", "obj= " + name);
        }          
        Object instance = getMBean(name);
        NotificationBroadcaster broadcaster;

        if (!(instance instanceof NotificationBroadcaster)) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException(name.getCanonicalName() ),  
                "The MBean " + name.getCanonicalName() +
                " does not implement the NotificationBroadcaster interface");
        }       
        broadcaster = (NotificationBroadcaster) instance;

        // ------------------
        // Check listener 
        // ------------------
        if (listener == null) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException("Null listener"),"Null listener");
        }

        NotificationListener listenerWrapper =
            getListenerWrapper(listener, name, instance, true);
        broadcaster.addNotificationListener(listenerWrapper, filter, handback);
    }
   
    public void addNotificationListener(ObjectName name,
                                        ObjectName listener, 
                                        NotificationFilter filter,
                                        Object handback)
            throws InstanceNotFoundException {
        // ----------------
        // Get listener object
        // ----------------
        Object instance = getMBean(listener);
        if (!(instance instanceof NotificationListener)) {
            throw new RuntimeOperationsException(new 
                IllegalArgumentException(listener.getCanonicalName()), 
                "The MBean " + listener.getCanonicalName() + 
                "does not implement the NotificationListener interface") ;
        }

        // ----------------
        // Add a listener on an MBean
        // ----------------
        if (logger.finerOn()) {
            logger.finer("addNotificationListener", "obj= " + name + " listener= " + 
                  listener);
        }           
        addNotificationListener(name,(NotificationListener) instance, 
                                filter, handback) ;
    }

    public void removeNotificationListener(ObjectName name,
                                           NotificationListener listener)
            throws InstanceNotFoundException, ListenerNotFoundException {
        removeNotificationListener(name, listener, null, null, true);
    }

    public void removeNotificationListener(ObjectName name,
                                           NotificationListener listener,
                                           NotificationFilter filter,
                                           Object handback)
            throws InstanceNotFoundException, ListenerNotFoundException {
        removeNotificationListener(name, listener, filter, handback, false);
    }

    private void removeNotificationListener(ObjectName name,
                                            NotificationListener listener,
                                            NotificationFilter filter,
                                            Object handback,
                                            boolean removeAll)
            throws InstanceNotFoundException, ListenerNotFoundException {

        if (logger.finerOn()) {
            logger.finer("removeNotificationListener", "obj= " + name);
        }
        Object instance = getMBean(name);

        /* We could simplify the code by assigning broadcaster after
           assigning listenerWrapper, but that would change the error
           behavior when both the broadcaster and the listener are
           erroneous.  */
        NotificationBroadcaster broadcaster = null;
        NotificationEmitter emitter = null;
        if (removeAll) {
            if (!(instance instanceof NotificationBroadcaster)) {
                final RuntimeException exc =
                    new IllegalArgumentException(name.getCanonicalName());
                final String msg =
                    "MBean " + name.getCanonicalName() + " does not " +
                    "implement " + NotificationBroadcaster.class.getName();
                throw new RuntimeOperationsException(exc, msg);
            }
            broadcaster = (NotificationBroadcaster) instance;
        } else {
            if (!(instance instanceof NotificationEmitter)) {
                final RuntimeException exc =
                    new IllegalArgumentException(name.getCanonicalName());
                final String msg =
                    "MBean " + name.getCanonicalName() + " does not " +
                    "implement " + NotificationEmitter.class.getName();
                throw new RuntimeOperationsException(exc, msg);
            }
            emitter = (NotificationEmitter) instance;
        }

        NotificationListener listenerWrapper =
            getListenerWrapper(listener, name, instance, false);

        if (listenerWrapper == null)
            throw new ListenerNotFoundException("Unknown listener");

        if (removeAll)
            broadcaster.removeNotificationListener(listenerWrapper);
        else {
            emitter.removeNotificationListener(listenerWrapper,
                                               filter,
                                               handback);
        }
    }


    public void removeNotificationListener(ObjectName name,
                                           ObjectName listener)
            throws InstanceNotFoundException, ListenerNotFoundException {
        removeNotificationListener(name, listener, null, null, true);
    }

    public void removeNotificationListener(ObjectName name,
                                           ObjectName listener,
                                           NotificationFilter filter,
                                           Object handback)
            throws InstanceNotFoundException, ListenerNotFoundException {
        removeNotificationListener(name, listener, filter, handback, false);
    }

    private void removeNotificationListener(ObjectName name,
                                            ObjectName listener,
                                            NotificationFilter filter,
                                            Object handback,
                                            boolean removeAll)
            throws InstanceNotFoundException, ListenerNotFoundException {

        // ----------------
        // Get listener object
        // ----------------
        Object instance;
        try {
            instance = getMBean(listener);
        } catch (InstanceNotFoundException e) {
            throw new ListenerNotFoundException(e.getMessage()) ;
        }

        if (!(instance instanceof NotificationListener)) {
            final RuntimeException exc =
                new IllegalArgumentException(listener.getCanonicalName());
            final String msg =
                "MBean " + name.getCanonicalName() + " does not " +
                "implement " + NotificationListener.class.getName();
            throw new RuntimeOperationsException(exc, msg);
        }
        if (logger.finerOn()) {
            logger.finer("removeNotificationListener", "obj= " + name +
                  " listener= " + listener);
        }
        removeNotificationListener(name,
                                   (NotificationListener) instance,
                                   filter,
                                   handback,
                                   removeAll);
    }


    public MBeanInfo getMBeanInfo(ObjectName name) 
        throws InstanceNotFoundException, IntrospectionException, 
               ReflectionException { 

        Object moi = getMBean(name);
        final MBeanInfo mbi = meta.getMBeanInfo(moi);
        if (mbi == null)
            throw new JMRuntimeException("MBean " + name + 
                                         "has no MBeanInfo");
        return mbi;
    }
 
 
    public boolean isInstanceOf(ObjectName name, String className) 
        throws InstanceNotFoundException {

        Object obj = getMBean(name);

        try {
            return meta.isInstanceOf(obj, className);
        } catch (ReflectionException e) {
            logger.finest("isInstanceOf",e);
            return false;
        }
    }

    public ClassLoader getClassLoader(ObjectName loaderName)
        throws InstanceNotFoundException {
        Object obj = getMBean(loaderName);
        if (obj == null) throw new 
            InstanceNotFoundException(loaderName.toString());
        if (!(obj instanceof  ClassLoader)) throw new
            InstanceNotFoundException(loaderName.toString() + 
                                      " is not a ClassLoader");
        return (ClassLoader)obj;
    }
     
    public final ClassLoader getMBeanClassLoader(ObjectName name) 
        throws InstanceNotFoundException {
        return getClassLoaderFor(name);
    }

    public ClassLoader getClassLoaderFor(ObjectName name) 
        throws InstanceNotFoundException {

        Object instance = getMBean(name);
        return instance.getClass().getClassLoader();
    }

    

     /**
     * Adds a MBean in the repository
     */
    private void internal_addObject(Object object, ObjectName logicalName) 
        throws InstanceAlreadyExistsException {

  
        // Let the repository do the work.
        
        synchronized(this) {
            try {
                repository.addMBean(object, logicalName);
            }
            catch (InstanceAlreadyExistsException e) {
                if (object instanceof MBeanRegistration ) {                     
                    meta.postRegisterInvoker(object,false);       
                }
                throw e;
            }          
        }
        // ---------------------
        // Send create event
        // ---------------------
        if (logger.finerOn()) {
            logger.finer("addObject", "Send create notification of object " + 
                  logicalName.getCanonicalName());
        }         
       
        sendNotification(MBeanServerNotification.REGISTRATION_NOTIFICATION, 
                         logicalName ) ;
    }
   

    /**
     * Sends an MBeanServerNotifications with the specified type for the 
     * MBean with the specified ObjectName
     */
    private void sendNotification(String NotifType, ObjectName name)
    {

        // ---------------------
        // Create notification
        // ---------------------
        MBeanServerNotification notif = null;
        synchronized(this) {
            // sequenceNumber = sequenceNumber + 1 ;
            
            if (logger.finerOn()) {
                // logger.finer("sendNotification", "Incr sequenceNumber = " + 
                //      sequenceNumber);
                logger.finer("sendNotrification", NotifType + " " + name);
            }        
            notif = new MBeanServerNotification(NotifType, 
                                                _MBSDelegateObjectName, 0, name);
        }
        final javax.management.MBeanServerDelegate delegate =
            ((MBeanServerInt)server).getMBeanServerDelegate();
        delegate.sendNotification(notif);
    }

    /**
     * Performs the necessary initializations for the MBeanServer.
     * Creates and registers the MetaData service and the MBeanServer 
     * identification MBean
     */
    private void initialize(MBeanServer mbs, MetaData meta, 
                            MBeanInstantiator inst, String domain, 
                            Repository repos) {

        this.server = mbs;
        this.meta = meta;
        this.instantiator = inst;
        this.repository   = repos;

        // If repository is null, uses new default repository
        if (repository == null) 
            repository = new RepositorySupport();

        if ((domain == null) || (domain.equals(""))) {    
            this.domain = repository.getDefaultDomain();
        }
        else {
            this.domain = domain;
        }
  
        // Set the domain name within the repository
        repository.setDefaultDomain(this.domain);
        if (logger.finerOn()) {
            logger.finer("new", "Initializing domain " + this.domain);
        }         
        // Check whether or not the repository is able to perform
        // the query service.
        //
        try {
            queryByRepo = repository.isFiltering();
        } catch (Exception e) {
            queryByRepo = false;
        }
    }

    /**
     * Applies the specified queries to the set of objects
     */
    private Set filterListOfObjects(Set list, QueryExp query) {
        Set result = new HashSet();

        // No query ...
        if (query == null ) {
            for (final Iterator i  = list.iterator(); i.hasNext(); ) {
                final NamedObject no = (NamedObject) i.next();
                final Object obj = no.getObject();
                String className = null;

                try {
                        className = meta.getMBeanClassName(obj);
                } catch (JMException x) {
                    if (logger.finestOn())
                        logger.finest("filterListOfObjects", 
                              "Can't obtain class name for " +
                              no.getName() + ": " + x);
                }

                result.add(new ObjectInstance(no.getName(), className));
            }
        } else {
    
            // Access the filter  
            for (final Iterator i  = list.iterator(); i.hasNext(); ) {
                final NamedObject no = (NamedObject) i.next();
                final Object obj = no.getObject();
                boolean res = false;
                query.setMBeanServer(server);
                try {
                    res = query.apply(no.getName());
                } catch (Exception e) {
                    res = false;
                }
                if (res) {

                    // if the MBean is a dynamic MBean ask its MBeanInfo 
                    // for the class name               
                    String className = null;
                    try {
                        className = meta.getMBeanClassName(obj);
                    } catch (JMException x) {
                        if (logger.finestOn())
                            logger.finest("filterListOfObjects", 
                                  "Can't obtain class name for " +
                                  no.getName() + ": " + x);
                    }

                    result.add(new ObjectInstance(no.getName(), className));
                } 
            }
        }
        return result;  
    }

    /*
     * Get the existing wrapper for this listener, name, and mbean, if
     * there is one.  Otherwise, if "create" is true, create and
     * return one.  Otherwise, return null.
     *
     * We use a WeakHashMap so that if the only reference to a user
     * listener is in listenerWrappers, it can be garbage collected.
     * This requires a certain amount of care, because only the key in
     * a WeakHashMap is weak; the value is strong.  We need to recover
     * the existing wrapper object (not just an object that is equal
     * to it), so we would like listenerWrappers to map any
     * ListenerWrapper to the canonical ListenerWrapper for that
     * (listener,name,mbean) set.  But we do not want this canonical
     * wrapper to be referenced strongly.  Therefore we put it inside
     * a WeakReference and that is the value in the WeakHashMap.
     */
    private NotificationListener getListenerWrapper(NotificationListener l,
                                                    ObjectName name,
                                                    Object mbean,
                                                    boolean create) {
        NotificationListener wrapper = new ListenerWrapper(l, name, mbean);
        synchronized (listenerWrappers) {
            WeakReference ref = (WeakReference) listenerWrappers.get(wrapper);
            if (ref != null) {
                NotificationListener existing =
                    (NotificationListener) ref.get();
                if (existing != null)
                    return existing;
            }
            if (create) {
                listenerWrappers.put(wrapper, new WeakReference(wrapper));
                return wrapper;
            } else
                return null;
        }
    }

    private static class ListenerWrapper implements NotificationListener {
        ListenerWrapper(NotificationListener l, ObjectName name,
                        Object mbean) {
            this.listener = l;
            this.name = name;
            this.mbean = mbean;
        }

        public void handleNotification(Notification notification,
                                       Object handback) {
            if (notification != null) {
                if (notification.getSource() == mbean)
                    notification.setSource(name);
            }

            /*
             * Listeners are not supposed to throw exceptions.  If
             * this one does, we could remove it from the MBean.  It
             * might indicate that a connector has stopped working,
             * for instance, and there is no point in sending future
             * notifications over that connection.  However, this
             * seems rather drastic, so instead we propagate the
             * exception and let the broadcaster handle it.
             */
            listener.handleNotification(notification, handback);
        }

        public boolean equals(Object o) {
            if (!(o instanceof ListenerWrapper))
                return false;
            ListenerWrapper w = (ListenerWrapper) o;
            return (w.listener == listener && w.mbean == mbean
                    && w.name.equals(name));
            /*
             * We compare all three, in case the same MBean object
             * gets unregistered and then reregistered under a
             * different name, or the same name gets assigned to two
             * different MBean objects at different times.  We do the
             * comparisons in this order to avoid the slow
             * ObjectName.equals when possible.
             */
        }

        public int hashCode() {
            return (System.identityHashCode(listener) ^
                    System.identityHashCode(mbean));
            /*
             * We do not include name.hashCode() in the hash because
             * computing it is slow and usually we will not have two
             * instances of ListenerWrapper with the same mbean but
             * different ObjectNames.  That can happen if the MBean is
             * unregistered from one name and reregistered with
             * another, and there is no garbage collection between; or
             * if the same object is registered under two names (which
             * is not recommended because MBeanRegistration will
             * break).  But even in these unusual cases the hash code
             * does not have to be unique.
             */
        }

        private NotificationListener listener;
        private ObjectName name;
        private Object mbean;
    }


}
