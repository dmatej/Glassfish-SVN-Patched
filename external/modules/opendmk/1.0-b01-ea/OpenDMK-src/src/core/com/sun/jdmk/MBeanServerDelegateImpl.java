/* 
 * @(#)file      MBeanServerDelegateImpl.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.13 
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

import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.MBeanRegistration;
import javax.management.DynamicMBean;
import javax.management.AttributeNotFoundException;
import javax.management.MBeanException; 
import javax.management.ReflectionException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanNotificationInfo;
import javax.management.Notification;
import javax.management.ListenerNotFoundException;
import javax.management.NotificationListener;
import javax.management.NotificationFilter;
import javax.management.JMRuntimeException;
import javax.management.InvalidAttributeValueException;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.RuntimeOperationsException;
import javax.management.MBeanServerDelegate;

import com.sun.jdmk.internal.ClassLogger;

/**
 * Wraps the default JMX MBeanServerDelegate.
 **/
final class MBeanServerDelegateImpl
    extends MBeanServerDelegate 
    implements DynamicMBean, MBeanRegistration {

    /** The name of this class to be used for tracing */
    private final static String dbgTag = "MBeanServerDelegateImpl";
     
    final private static String[] attributeNames = new String[] {
	"MBeanServerId",
	"SpecificationName",
	"SpecificationVersion",
	"SpecificationVendor",
	"ImplementationName",
	"ImplementationVersion",
	"ImplementationVendor"    
    };

    private static final MBeanAttributeInfo[] attributeInfos = 
	new MBeanAttributeInfo[] {
	    new MBeanAttributeInfo("MBeanServerId","java.lang.String",
				   "The MBean server agent identification",
				   true,false,false),
	    new MBeanAttributeInfo("SpecificationName","java.lang.String",
				   "The full name of the JMX specification "+
				   "implemented by this product.",
				   true,false,false),
	    new MBeanAttributeInfo("SpecificationVersion","java.lang.String",
				   "The version of the JMX specification "+
				   "implemented by this product.",
				   true,false,false),
	    new MBeanAttributeInfo("SpecificationVendor","java.lang.String",
				   "The vendor of the JMX specification "+
				   "implemented by this product.",
				   true,false,false),
	    new MBeanAttributeInfo("ImplementationName","java.lang.String",
				   "The JMX implementation name "+
				   "(the name of this product)",
				   true,false,false),
	    new MBeanAttributeInfo("ImplementationVersion","java.lang.String",
				   "The JMX implementation version "+
				   "(the version of this product).",
				   true,false,false),
	    new MBeanAttributeInfo("ImplementationVendor","java.lang.String",
				   "the JMX implementation vendor "+
				   "(the vendor of this product).",
				   true,false,false)
		};

    private final MBeanInfo delegateInfo;
    private final MBeanServerDelegate delegateImpl;

    public MBeanServerDelegateImpl (MBeanServerDelegate delegateImpl) {
	super();
	if (delegateImpl == null) delegateImpl = new MBeanServerDelegate();
	this.delegateImpl = delegateImpl;
	delegateInfo = 
	    new MBeanInfo("javax.management.MBeanServerDelegate",
			  "Represents  the MBean server from the management "+
			  "point of view.",
			  MBeanServerDelegateImpl.attributeInfos, null,
			  null,delegateImpl.getNotificationInfo());
    }

    final public ObjectName preRegister (MBeanServer server, ObjectName name) 
	throws java.lang.Exception {
	if (name == null) return new ObjectName(ServiceName.DELEGATE);
	else return name;
    }
    
    final public void postRegister (Boolean registrationDone) {
    }
       
    final public void preDeregister() 
	throws java.lang.Exception {
	throw new IllegalArgumentException(
         	 "The MBeanServerDelegate MBean cannot be unregistered");
    }

    final public void postDeregister() {
    }

    /**
     * Obtains the value of a specific attribute of the MBeanServerDelegate.
     *
     * @param attribute The name of the attribute to be retrieved
     *
     * @return  The value of the attribute retrieved.
     *
     * @exception AttributeNotFoundException
     * @exception MBeanException  
     *            Wraps a <CODE>java.lang.Exception</CODE> thrown by the 
     *            MBean's getter.
     */
    public Object getAttribute(String attribute) 
	throws AttributeNotFoundException,
	       MBeanException, ReflectionException {
	try {
	    // attribute must not be null
	    //
	    if (attribute == null) 
		throw new AttributeNotFoundException("null");

	    // Extract the requested attribute from file
	    //
	    if (attribute.equals("MBeanServerId")) 
		return delegateImpl.getMBeanServerId();
	    else if (attribute.equals("SpecificationName"))
		return delegateImpl.getSpecificationName();
	    else if (attribute.equals("SpecificationVersion"))
		return delegateImpl.getSpecificationVersion();
	    else if (attribute.equals("SpecificationVendor"))
		return delegateImpl.getSpecificationVendor();
	    else if (attribute.equals("ImplementationName"))
		return getImplementationName();
	    else if (attribute.equals("ImplementationVersion"))
		return getImplementationVersion();
	    else if (attribute.equals("ImplementationVendor"))
		return getImplementationVendor();

	    // Unknown attribute
	    //
	    else 
		throw new AttributeNotFoundException("null");

	} catch (AttributeNotFoundException x) {
	    throw x;
	} catch (JMRuntimeException j) {
	    throw j;
	} catch (Exception x) {
	    throw new MBeanException(x,"Failed to get " + attribute);
	}
    }
    
    /**
     * This method always fail since all MBeanServerDelegateMBean attributes
     * are read-only.
     *
     * @param attribute The identification of the attribute to
     * be set and  the value it is to be set to.
     *
     * @exception AttributeNotFoundException 
     */
    public void setAttribute(Attribute attribute) 
	throws AttributeNotFoundException, InvalidAttributeValueException, 
	       MBeanException, ReflectionException {
	
	// Now we will always fail:
	// Either because the attribute is null or because it is not
	// accessible (or does not exist).
	//
	final String attname = (attribute==null?null:attribute.getName());
        if (attname == null) {
	    final RuntimeException r = 
		new IllegalArgumentException("Attribute name cannot be null");
            throw new RuntimeOperationsException(r, 
                "Exception occurred trying to invoke the setter on the MBean");
        }  
	
	// This is a hack: we call getAttribute in order to generate an
	// AttributeNotFoundException if the attribute does not exist.
	//
	Object val = getAttribute(attname);

	// If we reach this point, we know that the requested attribute
	// exists. However, since all attributes are read-only, we throw
	// an AttributeNotFoundException.
	//
	throw new AttributeNotFoundException(attname + " not accessible");
    }
    
    /**
     * Makes it possible to get the values of several attributes of 
     * the MBeanServerDelegate.
     *
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return  The list of attributes retrieved.
     *
     */
    public AttributeList getAttributes(String[] attributes) {
	// If attributes is null, the get all attributes.
	//
	final String[] attn = (attributes==null?attributeNames:attributes);

	// Prepare the result list.
	//
	final int len = attn.length;
	final AttributeList list = new AttributeList(len);
	final boolean dbg = logger.finerOn();

	// Get each requested attribute.
	//
	for (int i=0;i<len;i++) {
	    try {
		final Attribute a = 
		    new Attribute(attn[i],getAttribute(attn[i]));
		list.add(a);
	    } catch (Exception x) {
		// Skip the attribute that couldn't be obtained.
		//
		if (dbg) {
		    logger.finer("getAttributes","Attribute " + attn[i] +
				 " not found: " + x);
		    logger.finest("getAttributes", x);
		}
	    }
	}

	// Finally return the result.
	//
	return list; 
    }

    /**
     * This method always return an empty list since all 
     * MBeanServerDelegateMBean attributes are read-only.
     *
     * @param attributes A list of attributes: The identification of the
     * attributes to be set and  the values they are to be set to.
     *
     * @return  The list of attributes that were set, with their new values.
     *          In fact, this method always return an empty list since all 
     *          MBeanServerDelegateMBean attributes are read-only.
     */
    public AttributeList setAttributes(AttributeList attributes) {
	return new AttributeList(0);
    }
    
    /**
     * Always fails since the MBeanServerDelegate MBean has no operation.
     *
     * @param actionName The name of the action to be invoked.
     * @param params An array containing the parameters to be set when the 
     *        action is invoked.
     * @param signature An array containing the signature of the action. 
     *
     * @return  The object returned by the action, which represents 
     *          the result of invoking the action on the MBean specified.
     *
     * @exception MBeanException  Wraps a <CODE>java.lang.Exception</CODE> 
     *         thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps a 
     *      <CODE>java.lang.Exception</CODE> thrown while trying to invoke 
     *      the method.
     */
    public Object invoke(String actionName, Object params[], 
			 String signature[])
	throws MBeanException, ReflectionException {
	// Check that operation name is not null.
	//
        if (actionName == null) {
	    final RuntimeException r = 
	      new IllegalArgumentException("Operation name  cannot be null");
            throw new RuntimeOperationsException(r, 
            "Exception occurred trying to invoke the operation on the MBean");
        } 

	throw new ReflectionException(
			  new NoSuchMethodException(actionName), 
                          "The operation with name " + actionName + 
			  " could not be found");
    }
    
    public final String getImplementationName() {
	return ServiceName.JMX_IMPL_NAME;
    }

    public final String getImplementationVersion() {
	return ServiceName.JMX_IMPL_VERSION;
    }

    public final String getImplementationVendor()  {
	return ServiceName.JMX_IMPL_VENDOR;
    }
    
    
    // From NotificationEmitter extends NotificationBroacaster
    //
    public final 
	void addNotificationListener(NotificationListener listener, 
				     NotificationFilter filter, 
				     Object handback) 
        throws IllegalArgumentException {
        delegateImpl.addNotificationListener(listener,filter,handback) ;
    }

    // From NotificationEmitter extends NotificationBroacaster
    //
    public final 
	void removeNotificationListener(NotificationListener listener)
        throws ListenerNotFoundException {
        delegateImpl.removeNotificationListener(listener) ;
    }

    // From NotificationEmitter extends NotificationBroacaster
    //
    public final 
	void removeNotificationListener(NotificationListener listener,
					NotificationFilter filter,
					Object handback)
	throws ListenerNotFoundException {
        delegateImpl.removeNotificationListener(listener, filter, handback);
    }

    public final 
	void sendNotification(Notification notification) {
        delegateImpl.sendNotification(notification);
    }

    /**
     * Provides the MBeanInfo describing the MBeanServerDelegate.
     *
     * @return  The MBeanInfo describing the MBeanServerDelegate.
     *
     */
    public MBeanInfo getMBeanInfo() {
	return delegateInfo;
    }

    private final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
			"MBeanServerDelegateImpl");

}
