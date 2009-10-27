/*
 * @(#)file      CascadingProxy.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.11
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
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
package com.sun.jdmk.remote.cascading.proxy;

// java import
import java.util.Set;
import java.util.List;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Collections;
import java.util.ArrayList;
import java.io.IOException;

import java.lang.reflect.UndeclaredThrowableException;

// jdmk import
import javax.management.QueryExp;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.MBeanRegistration;
import javax.management.MBeanRegistrationException;
import javax.management.RuntimeMBeanException;
import javax.management.DynamicMBean;
import javax.management.MBeanServerNotification;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.Notification;
import javax.management.MBeanNotificationInfo;
import javax.management.NotificationEmitter;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationListener;
import javax.management.NotificationFilter;
import javax.management.NotificationBroadcasterSupport;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.ListenerNotFoundException;
import javax.management.AttributeNotFoundException;
import javax.management.MBeanException;
import javax.management.ReflectionException;
import javax.management.Attribute;
import javax.management.InvalidAttributeValueException;
import javax.management.AttributeList;
import javax.management.MBeanInfo;
import javax.management.remote.JMXConnectionNotification;

import com.sun.jdmk.remote.cascading.CascadingAgent;
import com.sun.jdmk.remote.cascading.CascadingAgentMBean;
import com.sun.jdmk.remote.cascading.MBeanServerConnectionFactory;
import com.sun.jdmk.defaults.Utils;

/**
 * This class is used to proxy an MBean residing in a source 
 * <tt>MBeanServer</tt>. Connections to that source <tt>MBeanServer</tt>
 * are obtained through an {@link MBeanServerConnectionFactory}.
 * The <tt>CascadingProxy</tt> is a {@link DynamicMBean} that forwards all
 * the calls it receive to a source MBean, residing in a source
 * <tt>MBeanServer</tt>. The <tt>CascadingProxy</tt> also makes it possible
 * for  {@link NotificationListener Notification Listeners} to register
 * for notifications emitted by the source MBeans.
 *
 * @since Java DMK 5.1
 **/
public class CascadingProxy 
    implements DynamicMBean, MBeanRegistration, NotificationEmitter {

    /**
     * Creates a new <tt>CascadingProxy</tt>.
     * @param sourceMBeanName The <tt>ObjectName</tt> of the source MBean.
     * @param mbscf An <tt>MBeanServerConnectionFactory</tt> from which 
     *        connections with the <tt>MBeanServer</tt> containing the 
     *        source MBean can be obtained.
     *        The <tt>CascadingProxy</tt> will call {@link 
     *        MBeanServerConnectionFactory#getMBeanServerConnection() 
     *        mbscf.getMBeanServerConnection()} every time it needs to access
     *        the subagent.
     **/
    public CascadingProxy(ObjectName sourceMBeanName,
			  MBeanServerConnectionFactory mbscf) {
	connectionFactory = mbscf;
	source = sourceMBeanName;
	targetName = null;
	listenerList = Collections.EMPTY_LIST;
    }

    /**
     * Called by the <tt>CascadingProxy</tt> when an <tt>IOException</tt>
     * exception is caught while trying to talk with the source MBean.
     * Subclasses who need a more specific handling of such exceptions
     * (e.g. logging etc) can implement it by redefining this method.
     * The returned <tt>RuntimeException</tt> will be thrown by the calling
     * method.
     * @param x The <tt>IOException</tt> exception.
     * @param method The name of the method in which the exception 
     *        <var>x</var> was caught.
     * @return An {@link UndeclaredThrowableException} wrapping the
     *         undeclared checked exception <var>x</var>.
     **/
    RuntimeException handleIOException(IOException x, 
						 String method) {
	final RuntimeException r = new UndeclaredThrowableException(x);
	return r;
    }

    /**
     * Called by the <tt>CascadingProxy</tt> when an 
     * <tt>InstanceNotFoundException</tt> exception is caught while 
     * trying to talk with the source MBean.
     * Subclasses who need a more specific handling of such exceptions
     * (e.g. logging etc) can implement it by redefining this method.
     * The returned <tt>RuntimeException</tt> will be thrown by the calling
     * method.
     * @param x The <tt>InstanceNotFoundException</tt> exception.
     * @param method The name of the method in which the exception 
     *        <var>x</var> was caught.
     * @return An {@link UndeclaredThrowableException} wrapping the
     *         undeclared checked exception <var>x</var>.
     **/
    RuntimeException handleInstanceNotFoundException(
						InstanceNotFoundException x, 
						String method) {
	final RuntimeException r = new UndeclaredThrowableException(x);
	return r;
    }

    /**
     * Called by the <tt>CascadingProxy</tt> when an undeclared checked
     * exception is caught while trying to talk with the source MBean.
     * This method is not called for either <tt>IOException</tt> 
     * (see {@link #handleIOException handleIOException(x,method)})
     * or <tt>InstanceNotFoundException</tt> 
     * (see {@link #handleInstanceNotFoundException 
     * handleInstanceNotFoundException(x,method)}).
     * Subclasses who need a more specific handling of such exceptions
     * (e.g. logging etc) can implement it by redefining this method.
     * The returned <tt>RuntimeException</tt> will be thrown by the calling
     * method.
     * @param x The undeclared checked exception.
     * @param method The name of the method in which the exception 
     *        <var>x</var> was caught.
     * @return An {@link UndeclaredThrowableException} wrapping the
     *         undeclared checked exception <var>x</var>.
     **/
    RuntimeException handleCheckedException(Exception x, String method) {
	final RuntimeException r = new UndeclaredThrowableException(x);
	return r;
    }

    /**
     * Returns an <tt>MBeanServerConnection</tt> obtained from the underlying 
     * {@link MBeanServerConnectionFactory}. This method is called 
     * every time the cascading proxy needs to forward an operation
     * to the source MBean. Subclasses should not implement connection
     * caching by redefining this method. Connection caching, if needed,
     * should be delegated to the underlying 
     * {@link MBeanServerConnectionFactory}.
     * @exception IOException if no <tt>MBeanServerConnection</tt> can be 
     *            obtained.
     * @return An <tt>MBeanServerConnection</tt> to the <tt>MBeanServer</tt>
     *         in which the source MBean resides.
     **/
    private MBeanServerConnection connection() 
	throws IOException {
	final MBeanServerConnection c = 
	    connectionFactory.getMBeanServerConnection();
	if (c == null) 
	    throw new IOException("MBeanServerConnection unavailable");
	return c;
    }


    /**
     * Obtain the value of a specific attribute from the source MBean.
     *
     * @param attribute The name of the attribute to be retrieved
     *
     * @return  The value of the attribute retrieved.
     *
     * @exception AttributeNotFoundException
     * @exception MBeanException  
     *    Wraps a <tt>java.lang.Exception</tt> thrown by the MBean's getter.
     * @exception ReflectionException  Wraps a <tt>java.lang.Exception</tt> 
     *    thrown while trying to invoke the getter. 
     * @exception UndeclaredThrowableException if an 
     *         <tt>InstanceNotFoundException</tt> or an <tt>IOException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *            
     * @see #setAttribute
     * @see DynamicMBean#getAttribute
     */
    public Object getAttribute(String attribute) 
	throws AttributeNotFoundException,
	       MBeanException, ReflectionException {
	try {
	    return connection().getAttribute(source, attribute);
	} catch (IOException x) {
	    throw handleIOException(x,"getAttribute");
	} catch (InstanceNotFoundException x) {
	    throw handleInstanceNotFoundException(x,"getAttribute");
	}
    }
    
    /**
     * Set the value of a specific attribute on the source MBean.
     *
     * @param attribute The identification of the attribute to
     * be set and  the value it is to be set to.
     *
     * @exception AttributeNotFoundException 
     * @exception InvalidAttributeValueException 
     * @exception MBeanException Wraps a <tt>java.lang.Exception</tt> thrown 
     *            by the MBean's setter.
     * @exception ReflectionException Wraps a <tt>java.lang.Exception</tt> 
     *            thrown while trying to invoke the MBean's setter.
     * @exception UndeclaredThrowableException if an 
     *         <tt>InstanceNotFoundException</tt> or an <tt>IOException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *            
     * @see #getAttribute
     * @see DynamicMBean#setAttribute
     */
    public void setAttribute(Attribute attribute) 
	throws AttributeNotFoundException,
	       InvalidAttributeValueException, MBeanException, 
	       ReflectionException {
	try {
	    connection().setAttribute(source, attribute);
	} catch (IOException x) {
	    throw handleIOException(x,"setAttribute");
	} catch (InstanceNotFoundException x) {
	    throw handleInstanceNotFoundException(x,"setAttribute");
	}
    }
        
    /**
     * Get the values of several attributes from the source MBean.
     *
     * @param attributes A list of the attributes to be retrieved.
     *
     * @return  The list of attributes retrieved.
     *
     * @exception UndeclaredThrowableException if an 
     *        <tt>InstanceNotFoundException</tt>, an <tt>IOException</tt>,
     *        or a <tt>ReflectionException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *
     * @see #setAttributes
     * @see DynamicMBean#getAttributes
     */
    public AttributeList getAttributes(String[] attributes) {
	try {
	    return connection().getAttributes(source, attributes);
	} catch (IOException x) {
	    throw handleIOException(x,"getAttributes");
	} catch (InstanceNotFoundException x) {
	    throw handleInstanceNotFoundException(x,"getAttributes");
	} catch (RuntimeException r) {
	    throw r;
	} catch (Exception x) {
	    throw handleCheckedException(x,"getAttributes");
	}
    }
        

    /**
     * Sets the values of several attributes in the source MBean.
     *
     * @param attributes A list of attributes: The identification of the
     * attributes to be set and  the values they are to be set to.
     *
     * @return  The list of attributes that were set, with their new values.
     *
     * @exception UndeclaredThrowableException if an 
     *        <tt>InstanceNotFoundException</tt>, an <tt>IOException</tt>,
     *        or a <tt>ReflectionException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *
     * @see #getAttributes
     * @see DynamicMBean#setAttributes
     */
    public AttributeList setAttributes(AttributeList attributes) {
	try {
	    return connection().setAttributes(source, attributes);
	} catch (IOException x) {
	    throw handleIOException(x,"setAttributes");
	} catch (InstanceNotFoundException x) {
	    throw handleInstanceNotFoundException(x,"setAttributes");
	} catch (RuntimeException r) {
	    throw r;
	} catch (Exception x) {
	    throw handleCheckedException(x,"setAttributes");
	}
    }
    
    /**
     * Allows an action to be invoked on the source MBean.
     *
     * @param actionName The name of the action to be invoked.
     * @param params An array containing the parameters to be set when 
     *        the action is invoked.
     * @param signature An array containing the signature of the action. 
     *
     * @return The object returned by the action, which represents the 
     *         result of invoking the action on the MBean specified.
     *
     * @exception MBeanException  Wraps a <tt>java.lang.Exception</tt> 
     *            thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps a <tt>java.lang.Exception</tt> 
     *            thrown while trying to invoke the method
     * @exception UndeclaredThrowableException if an 
     *         <tt>InstanceNotFoundException</tt> or an <tt>IOException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *
     * @see DynamicMBean#invoke
     */
    public Object invoke(String actionName,Object params[],String signature[])
	throws MBeanException, ReflectionException {
	try {
	    return connection().invoke(source, actionName, params, 
				       signature);
	} catch (IOException x) {
	    throw handleIOException(x,"invoke");
	} catch (InstanceNotFoundException x) {
	    throw handleInstanceNotFoundException(x,"invoke");
	}
    }

    /**
     * Returns the <tt>MBeanInfo</tt> of the source MBean. 
     * This <tt>MBeanInfo</tt> is not cached by the proxy, which could 
     * be considered as a suboptimal implementation for those 
     * implementations in which it is known that the source 
     * <tt>MBeanInfo</tt> will not change over time. 
     * <p>Subclasses may therefore wish to redefine this method in order
     * to cache the returned <tt>MBeanInfo</tt>.
     * @return The source MBean <tt>MBeanInfo</tt>.
     * @exception UndeclaredThrowableException if an 
     *        <tt>InstanceNotFoundException</tt>, an <tt>IOException</tt>,
     *        a <tt>ReflectionException</tt>, or an 
     *        <tt>IntrospectionException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     **/
    public MBeanInfo getMBeanInfo() {
	try {
	    return connection().getMBeanInfo(source);
	} catch (IOException x) {
	    throw handleIOException(x,"getMBeanInfo");
	} catch (InstanceNotFoundException x) {
	    throw handleInstanceNotFoundException(x,"getMBeanInfo");
	} catch (RuntimeException r) {
	    throw r;
	} catch (Exception x) {
	    throw handleCheckedException(x,"getMBeanInfo");
	}
    }
    
    /**
     * Allows the MBean to perform any operations it needs before
     * being registered in the MBean server. This default implementation
     * simply checks that the given <var>name</var> is not null, and cache
     * it in the <tt>CascadingProxy</tt>. Once the <tt>CascadingProxy</tt>
     * is registered, its <tt>ObjectName</tt> can be later obtained by 
     * calling {@link #getTargetName()}.
     *
     * @param server The MBean server in which the MBean will be registered.
     *
     * @param name The object name of the MBean. If <var>name</var>
     *        this method throws an <tt>IllegalArgumentException</tt>.
     *
     * @return The given <var>name</var>.
     *
     * @exception IllegalArgumentException if the parameter <var>name</var>
     *         is <tt>null</tt>
     * @see MBeanRegistration#preRegister
     */
    public ObjectName preRegister(MBeanServer server,
				  ObjectName name) 
	throws java.lang.Exception {
	if (name == null) 
	    throw new IllegalArgumentException("Illegal ObjectName: null");

	synchronized (this) { targetName = name ; }
	return targetName;
    }

    /**
     * Allows the MBean to perform any operations needed after having been
     * registered in the MBean server or after the registration has failed.
     * This default implementation does nothing.
     *
     * @param registrationDone Indicates whether or not the MBean has
     * been successfully registered in the MBean server. The value
     * false means that the registration phase has failed.
     * @see MBeanRegistration#postRegister
     */
    public void postRegister(Boolean registrationDone) {
    }

    /**
     * Allows the MBean to perform any operations it needs before
     * being unregistered by the MBean server.
     * This default implementation does nothing.
     *
     * @exception java.lang.Exception This exception will be caught by
     * the MBean server and re-thrown as an {@link
     * MBeanRegistrationException} or a {@link RuntimeMBeanException}.
     * @see MBeanRegistration#preDeregister
     */
    public void preDeregister() throws java.lang.Exception {
    }

    /**
     * Allows the MBean to perform any operations needed after having been
     * unregistered in the MBean server.
     * This default implementation does nothing.
     */
    public void postDeregister() { }

    /**
     * The <tt>ObjectName</tt> of the source MBean, as passed to this 
     * Object's constructor.
     **/
    public final ObjectName getSourceMBeanName() {
	return source;
    }

    /**
     * The <tt>ObjectName</tt> of this cascading proxy. May be null if this
     * object has not been registered in an <tt>MBeanServer</tt> yet. 
     * Otherwise, it is the <tt>ObjectName</tt> obtained from the
     * {@link #preRegister} method.
     **/
    public synchronized ObjectName getTargetName() {
	return targetName;
    }


    /**
     * <p>Returns an array indicating, for each notification this
     * MBean may send, the name of the Java class of the notification
     * and the notification type.</p>
     *
     * <p>This method returns the notification information present
     *    in the source MBean's <tt>MBeanInfo</tt> - if any.
     *    Otherwise, it returns an empty array.</p>
     *
     * <p>It is not illegal for the MBean to send notifications not
     * described in this array.  However, some clients of the MBean
     * server may depend on the array being complete for their correct
     * functioning.</p>
     *
     * @return the array of possible notifications.
     * @see NotificationBroadcaster#getNotificationInfo
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
	try {
	    final MBeanNotificationInfo[] info = 
	    getMBeanInfo().getNotifications();
	    if (info == null) return new MBeanNotificationInfo[0];
	    else return info;
	} catch(Exception x) {
	    // OK.
	}
	return  new MBeanNotificationInfo[0];
    }
    
    /*
     * The following is a "close clone" of NotificationBroadcasterSupport,
     * that handles an internal list of ListenerWrapper.
     * The sole purpose of the listener wrapper is to modify the
     * source of the emitted notification.
     */
    

    /**
     * Adds a listener to the source MBean.
     *
     * @param listener The listener object which will handle the
     * notifications emitted by the broadcaster.
     * @param filter The filter object. If filter is null, no
     * filtering will be performed before handling notifications.
     * @param handback An opaque object to be sent back to the
     * listener when a notification is emitted. This object cannot be
     * used by the Notification broadcaster object. It should be
     * resent unchanged with the notification to the listener.
     *
     * @exception IllegalArgumentException Listener parameter is null.
     * @exception UndeclaredThrowableException if an 
     *         <tt>InstanceNotFoundException</tt> or an <tt>IOException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *
     * @see #removeNotificationListener
     * @see NotificationBroadcaster#addNotificationListener
     */
    public void addNotificationListener(NotificationListener listener,
					NotificationFilter filter,
					Object handback) {

        if (listener == null) {
            throw new IllegalArgumentException ("Listener can't be null") ;
        }

	/* Adding a new listener takes O(n) time where n is the number
	   of existing listeners.  If you have a very large number of
	   listeners performance could degrade.  That's a fairly
	   surprising configuration, and it is hard to avoid this
	   behavior while still retaining the property that the
	   listenerList is not synchronized while notifications are
	   being sent through it.  If this becomes a problem, a
	   possible solution would be a multiple-readers single-writer
	   setup, so any number of sendNotification() calls could run
	   concurrently but they would exclude an
	   add/removeNotificationListener.  A simpler but less
	   efficient solution would be to clone the listener list
	   every time a notification is sent.  */
	synchronized (this) {
	    ListenerWrapper w = 
		new ListenerWrapper(listener, filter, handback);
	    try {
		connection().addNotificationListener(source,w,filter,
						     handback);
		List newList = new ArrayList(listenerList.size() + 1);
		newList.addAll(listenerList);
		newList.add(w);
		listenerList = newList;
	    } catch (IOException x) {
		throw handleIOException(x,"addNotificationListener");
	    } catch (InstanceNotFoundException x) {
		throw handleInstanceNotFoundException(x,
                      "addNotificationListener");
	    }
	}
    }

    /**
     * Removes a listener from the source MBean.  If the listener
     * has been registered with different handback objects or
     * notification filters, all entries corresponding to the listener
     * will be removed.
     *
     * @param listener A listener that was previously added to this
     * MBean.
     *
     * @exception ListenerNotFoundException The listener is not
     * registered with the MBean.
     * @exception UndeclaredThrowableException if an 
     *         <tt>InstanceNotFoundException</tt> or an <tt>IOException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *
     * @see #addNotificationListener
     * @see NotificationEmitter#removeNotificationListener
     */
    public void removeNotificationListener(NotificationListener listener)
        throws ListenerNotFoundException {

	synchronized (this) {
	    List newList = new ArrayList(listenerList);
	    /* We scan the list of listeners in reverse order because
	       in forward order we would have to repeat the loop with
	       the same index after a remove.  */
	    for (int i=newList.size()-1; i>=0; i--) {
		ListenerWrapper li = (ListenerWrapper)newList.get(i);

		if (li.listener == listener) {
		    try {
			connection().removeNotificationListener(source,li);
			newList.remove(i);
		    } catch (IOException x) {
			throw handleIOException(x,
			      "removeNotificationListener");
		    } catch (InstanceNotFoundException x) {
			throw handleInstanceNotFoundException(x,
                              "removeNotificationListener");
		    }
		}
	    }
	    if (newList.size() == listenerList.size())
	       throw new ListenerNotFoundException("Listener not registered");
	    listenerList = newList;
	}
    }

    /**
     * <p>Removes a listener from the source MBean.  The MBean must have a
     * listener that exactly matches the given <code>listener</code>,
     * <code>filter</code>, and <code>handback</code> parameters.  If
     * there is more than one such listener, only one is removed.</p>
     *
     * <p>The <code>filter</code> and <code>handback</code> parameters
     * may be null if and only if they are null in a listener to be
     * removed.</p>
     *
     * @param listener A listener that was previously added to this
     * MBean.
     * @param filter The filter that was specified when the listener
     * was added.
     * @param handback The handback that was specified when the listener was
     * added.
     *
     * @exception ListenerNotFoundException The listener is not
     * registered with the MBean, or it is not registered with the
     * given filter and handback.
     * @exception UndeclaredThrowableException if an 
     *         <tt>InstanceNotFoundException</tt> or an <tt>IOException</tt>
     *        is raised while trying to forward the request to the source
     *        MBean.
     *
     * @see NotificationEmitter#removeNotificationListener
     */
    public void removeNotificationListener(NotificationListener listener,
					   NotificationFilter filter,
					   Object handback)
	    throws ListenerNotFoundException {

	boolean found = false;

	synchronized (this) {
	    List newList = new ArrayList(listenerList);
	    final int size = newList.size();
	    for (int i = 0; i < size; i++) {
		ListenerWrapper li = (ListenerWrapper) newList.get(i);

		if (li.listener == listener) {
		    found = true;
		    if (li.filter == filter
			&& li.handback == handback) {
			try {
			    connection().removeNotificationListener(source,
								    li,
								    filter,
								    handback);
			    newList.remove(i);
			} catch (IOException x) {
			    throw handleIOException(x,
				  "removeNotificationListener");
			} catch (InstanceNotFoundException x) {
			    throw handleInstanceNotFoundException(x,
                                  "removeNotificationListener");
			}
			newList.remove(i);
			listenerList = newList;
			return;
		    }
		}
	    }
	}
	
	if (found) {
	    /* We found this listener, but not with the given filter
	     * and handback.  A more informative exception message may
	     * make debugging easier.  */
	    throw new ListenerNotFoundException("Listener not registered " +
						"with this filter and " +
						"handback");
	} else {
	    throw new ListenerNotFoundException("Listener not registered");
	}
    }

    /**
     * Returns the <tt>MBeanServerConnectionFactory</tt>, as passed to this
     * object's constructor.
     **/
    public final MBeanServerConnectionFactory getConnectionFactory() {
	return connectionFactory;
    }

    /**
     * ListenerWrapper wraps a client NotificationListener - in order
     * to translate ObjectNames in the received notification before
     * forwarding it to the wrapped listener.
     **/
    private final class ListenerWrapper implements  NotificationListener {
	public final NotificationListener listener;
	final NotificationFilter filter;
	final Object handback;

	public ListenerWrapper(NotificationListener listener,
			    NotificationFilter   filter,
			    Object               handback) {
	    this.listener = listener;
	    this.filter = filter;
	    this.handback = handback;
	}
	public void handleNotification(Notification notif, 
				       Object handback) {

	    // Only change the source if it's the object name of the
	    // source object.
	    //
	    listener.handleNotification(translate(notif),handback);
	}
    }

    /**
     * Translate the source <tt>ObjectName</tt> into this 
     * <tt>CascadingProxy</tt>'s <tt>ObjectName</tt>.
     * This implementation simply substitute the notification source
     * by this <tt>CascadingProxy</tt>'s <tt>ObjectName</tt>, iff the
     * source is the <tt>ObjectName</tt> of the source MBean.
     * @return <var>notif</var>, after the source has been possibly
     *         substituted.
     **/
    protected Notification translate(Notification notif) {
	notif.setSource(makeSource(notif.getSource()));
	return notif;
    }

    private Object makeSource(Object source) {
	if (source != null && !source.equals(getSourceMBeanName()))
	    return source;
	final ObjectName name = getTargetName();
	if (name == null) return CascadingProxy.this;
	return name;
    }

    /**
     * Current list of listeners, a List of ListenerInfo.  The object
     * referenced by this field is never modified.  Instead, the field
     * is set to a new object when a listener is added or removed,
     * within a synchronized(this).  In this way, there is no need to
     * synchronize when traversing the list to send a notification to
     * the listeners in it.  That avoids potential deadlocks if the
     * listeners end up depending on other threads that are themselves
     * accessing this <tt>NotificationBroadcasterSupport</tt>.
     */
    private List listenerList;

    /**
     * The underlying <code>MBeanServerConnectionFactory</code>
     **/
    private final MBeanServerConnectionFactory connectionFactory;
 
    /**
     * The <tt>ObjectName</tt> of the source MBean.
     **/
    private final ObjectName source;

    /**
     * The <tt>ObjectName</tt> of the cascading proxy (this object's name).
     **/
    private         ObjectName targetName;
}
