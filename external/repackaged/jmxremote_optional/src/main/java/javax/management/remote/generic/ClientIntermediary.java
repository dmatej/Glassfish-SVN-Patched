/*
 * @(#)file      ClientIntermediary.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.45
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

package javax.management.remote.generic;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.Serializable;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;

import java.rmi.NoSuchObjectException;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerNotification;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationFilter;
import javax.management.NotificationFilterSupport;
import javax.management.NotificationListener;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.QueryExp;
import javax.management.ReflectionException;
import javax.management.MalformedObjectNameException;

import javax.management.remote.NotificationResult;
import javax.management.remote.TargetedNotification;
import javax.management.remote.JMXConnectionNotification;
import javax.management.remote.message.MBeanServerRequestMessage;
import javax.management.remote.message.MBeanServerResponseMessage;
import javax.management.remote.message.NotificationRequestMessage;
import javax.management.remote.message.NotificationResponseMessage;
import javax.security.auth.Subject;

import com.sun.jmx.remote.generic.ObjectWrappingImpl;
import com.sun.jmx.remote.generic.ClientSynchroMessageConnection;
import com.sun.jmx.remote.generic.DefaultConfig;
import com.sun.jmx.remote.opt.util.ClassLogger;
import com.sun.jmx.remote.opt.util.EnvHelp;
import com.sun.jmx.remote.opt.internal.ClientNotifForwarder;
import com.sun.jmx.remote.opt.internal.ClientCommunicatorAdmin;
import com.sun.jmx.remote.opt.internal.ClientListenerInfo;

class ClientIntermediary {

    public ClientIntermediary(ClientSynchroMessageConnection connection,
			      ObjectWrapping wrap,
			      GenericConnector client,
			      Map env) {
	logger.trace("constructor", "Create a ClientIntermediary object.");

	if (connection == null) {
	    throw new NullPointerException("Null connection.");
	}

	this.connection = connection;
	if (wrap == null) {
	    logger.trace("constructor",
			 "Use a default ObjectWrapping implementation.");

	    this.serialization = new ObjectWrappingImpl();
	} else {
	    this.serialization = wrap;
	}
	myloader = EnvHelp.resolveClientClassLoader(env);

	this.client = client;

	communicatorAdmin =
	    new GenericClientCommunicatorAdmin(
		  EnvHelp.getConnectionCheckPeriod(env));
	notifForwarder = new GenericClientNotifForwarder(env);

	requestTimeoutReconn = DefaultConfig.getTimeoutReconnection(env);
    }

    //-------------------------------------------------------------
    // Implementation of MBeanServerConnection + Delegation Subject
    //-------------------------------------------------------------

    public ObjectInstance createMBean(String className,
				      ObjectName name,
				      Subject delegationSubject)
	    throws ReflectionException,
		   InstanceAlreadyExistsException,
		   MBeanRegistrationException,
		   MBeanException,
		   NotCompliantMBeanException,
		   IOException {

	logger.trace("createMBean", "called");

	try {
	    return (ObjectInstance)
		mBeanServerRequest(MBeanServerRequestMessage.CREATE_MBEAN,
				   new Object[] {className, name},
				   delegationSubject);
	} catch (ReflectionException e) {
	    throw e;
	} catch (InstanceAlreadyExistsException e) {
	    throw e;
	} catch (MBeanRegistrationException e) {
	    throw e;
	} catch (MBeanException e) {
	    throw e;
	} catch (NotCompliantMBeanException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public ObjectInstance createMBean(String className,
				      ObjectName name,
				      Object params[],
				      String signature[],
				      Subject delegationSubject)
	    throws ReflectionException,
		   InstanceAlreadyExistsException,
		   MBeanRegistrationException,
		   MBeanException,
		   NotCompliantMBeanException,
		   IOException {

	logger.trace("createMBean", "called");

	try {
	    return (ObjectInstance)
	      mBeanServerRequest(MBeanServerRequestMessage.CREATE_MBEAN_PARAMS,
				   new Object[] {className,
						 name,
						 serialization.wrap(params),
						 signature},
				   delegationSubject);
	} catch (ReflectionException e) {
	    throw e;
	} catch (InstanceAlreadyExistsException e) {
	    throw e;
	} catch (MBeanRegistrationException e) {
	    throw e;
	} catch (MBeanException e) {
	    throw e;
	} catch (NotCompliantMBeanException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public ObjectInstance createMBean(String className,
				      ObjectName name,
				      ObjectName loaderName,
				      Subject delegationSubject)
	    throws ReflectionException,
		   InstanceAlreadyExistsException,
		   MBeanRegistrationException,
		   MBeanException,
		   NotCompliantMBeanException,
		   InstanceNotFoundException,
		   IOException {

	logger.trace("createMBean", "called");

	try {
	    return (ObjectInstance)
	      mBeanServerRequest(MBeanServerRequestMessage.CREATE_MBEAN_LOADER,
				   new Object[] {className,
						 name,
						 loaderName},
				   delegationSubject);
	} catch (ReflectionException e) {
	    throw e;
	} catch (InstanceAlreadyExistsException e) {
	    throw e;
	} catch (MBeanRegistrationException e) {
	    throw e;
	} catch (MBeanException e) {
	    throw e;
	} catch (NotCompliantMBeanException e) {
	    throw e;
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public ObjectInstance createMBean(String className,
				      ObjectName name,
				      ObjectName loaderName,
				      Object params[],
				      String signature[],
				      Subject delegationSubject)
	    throws ReflectionException,
		   InstanceAlreadyExistsException,
		   MBeanRegistrationException,
		   MBeanException,
		   NotCompliantMBeanException,
		   InstanceNotFoundException,
		   IOException {


	logger.trace("createMBean", "called");

	try {
	    int code = MBeanServerRequestMessage.CREATE_MBEAN_LOADER_PARAMS;
	    return (ObjectInstance)
		mBeanServerRequest(code,
				   new Object[] {className,
						 name,
						 loaderName,
						 serialization.wrap(params),
						 signature},
				   delegationSubject);
	} catch (ReflectionException e) {
	    throw e;
	} catch (InstanceAlreadyExistsException e) {
	    throw e;
	} catch (MBeanRegistrationException e) {
	    throw e;
	} catch (MBeanException e) {
	    throw e;
	} catch (NotCompliantMBeanException e) {
	    throw e;
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void unregisterMBean(ObjectName name,
				Subject delegationSubject)
	    throws InstanceNotFoundException,
		   MBeanRegistrationException,
		   IOException {

	logger.trace("unregisterMBean", "called");

	try {
	    mBeanServerRequest(MBeanServerRequestMessage.UNREGISTER_MBEAN,
			       new Object[] {name},
			       delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (MBeanRegistrationException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public ObjectInstance getObjectInstance(ObjectName name,
					    Subject delegationSubject)
	    throws InstanceNotFoundException,
		   IOException {

	logger.trace("getObjectInstance", "called");

	try {
	    return (ObjectInstance)
              mBeanServerRequest(MBeanServerRequestMessage.GET_OBJECT_INSTANCE,
				   new Object[] {name},
				   delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public Set queryMBeans(ObjectName name,
			   QueryExp query,
			   Subject delegationSubject)
	    throws IOException {

	logger.trace("queryMBeans", "called");

	try {
	    return (Set)
		mBeanServerRequest(MBeanServerRequestMessage.QUERY_MBEANS,
				   new Object[] {name,
						 serialization.wrap(query)},
				   delegationSubject);
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public Set queryNames(ObjectName name,
			  QueryExp query,
			  Subject delegationSubject)
	    throws IOException {

	logger.trace("queryNames", "called");

	try {
	    return (Set)
		mBeanServerRequest(MBeanServerRequestMessage.QUERY_NAMES,
				   new Object[] {name,
						 serialization.wrap(query)},
				   delegationSubject);
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public boolean isRegistered(ObjectName name, Subject delegationSubject)
	    throws IOException {

	logger.trace("isRegistered", "called");

	try {
	    Boolean is = (Boolean)
		mBeanServerRequest(MBeanServerRequestMessage.IS_REGISTERED,
				   new Object[] {name},
				   delegationSubject);

	    return is.booleanValue();
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public Integer getMBeanCount(Subject delegationSubject)
	    throws IOException {

	logger.trace("getMBeanCount", "called");

	try {
	    return (Integer)
		mBeanServerRequest(MBeanServerRequestMessage.GET_MBEAN_COUNT,
				   null,
				   delegationSubject);
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public Object getAttribute(ObjectName name,
			       String attribute,
			       Subject delegationSubject)
	    throws MBeanException,
		   AttributeNotFoundException,
		   InstanceNotFoundException,
		   ReflectionException,
		   IOException {

	logger.trace("getAttribute", "called");

	try {
	    return
		mBeanServerRequest(MBeanServerRequestMessage.GET_ATTRIBUTE,
				   new Object[] {name, attribute},
				   delegationSubject);
	} catch (MBeanException e) {
	    throw e;
	} catch (AttributeNotFoundException e) {
	    throw e;
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (ReflectionException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public AttributeList getAttributes(ObjectName name,
				       String[] attributes,
				       Subject delegationSubject)
	    throws InstanceNotFoundException,
		   ReflectionException,
		   IOException {

	logger.trace("getAttributes", "called");

	try {
	    return (AttributeList)
		mBeanServerRequest(MBeanServerRequestMessage.GET_ATTRIBUTES,
				   new Object[] {name, attributes},
				   delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (ReflectionException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void setAttribute(ObjectName name,
			     Attribute attribute,
			     Subject delegationSubject)
	    throws InstanceNotFoundException,
		   AttributeNotFoundException,
		   InvalidAttributeValueException,
		   MBeanException,
		   ReflectionException,
		   IOException {

	logger.trace("setAttribute", "called");

	try {
	    mBeanServerRequest(MBeanServerRequestMessage.SET_ATTRIBUTE,
			       new Object[] {name,
					     serialization.wrap(attribute)},
			       delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (AttributeNotFoundException e) {
	    throw e;
	} catch (InvalidAttributeValueException e) {
	    throw e;
	} catch (MBeanException e) {
	    throw e;
	} catch (ReflectionException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public AttributeList setAttributes(ObjectName name,
				       AttributeList attributes,
				       Subject delegationSubject)
	    throws InstanceNotFoundException,
		   ReflectionException,
		   IOException {

	logger.trace("setAttributes", "called");

	try {
	    Object wrappedAttrs = serialization.wrap(attributes);
	    return (AttributeList)
		mBeanServerRequest(MBeanServerRequestMessage.SET_ATTRIBUTES,
				   new Object[] {name, wrappedAttrs},
				   delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (ReflectionException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public Object invoke(ObjectName name,
			 String operationName,
			 Object params[],
			 String signature[],
			 Subject delegationSubject)
	    throws InstanceNotFoundException,
		   MBeanException,
		   ReflectionException,
		   IOException {

	logger.trace("invoke", "called");

	try {
	    return
		mBeanServerRequest(MBeanServerRequestMessage.INVOKE,
				   new Object[] {name,
						 operationName,
						 serialization.wrap(params),
						 signature},
				   delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (MBeanException e) {
	    throw e;
	} catch (ReflectionException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public String getDefaultDomain(Subject delegationSubject)
	    throws IOException {

	logger.trace("getDefaultDomain", "called");

	try {
	    int code = MBeanServerRequestMessage.GET_DEFAULT_DOMAIN;
	    return (String)
		mBeanServerRequest(code,
				   null,
				   delegationSubject);
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public String[] getDomains(Subject delegationSubject)
	    throws IOException {

      	logger.trace("getDomains", "called");

	try {
	    return (String[])
		mBeanServerRequest(MBeanServerRequestMessage.GET_DOMAINS,
				   null,
				   delegationSubject);
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public MBeanInfo getMBeanInfo(ObjectName name,
				  Subject delegationSubject)
	    throws InstanceNotFoundException,
		   IntrospectionException,
		   ReflectionException,
		   IOException {

	logger.trace("getMBeanInfo", "called");

	try {
	    return (MBeanInfo)
		mBeanServerRequest(MBeanServerRequestMessage.GET_MBEAN_INFO,
				   new Object[] {name},
				   delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (IntrospectionException e) {
	    throw e;
	} catch (ReflectionException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public boolean isInstanceOf(ObjectName name,
				String className,
				Subject delegationSubject)
	    throws InstanceNotFoundException,
		   IOException {

	logger.trace("isInstanceOf", "called");

	try {
	    Boolean is = (Boolean)
		mBeanServerRequest(MBeanServerRequestMessage.IS_INSTANCE_OF,
				   new Object[] {name,
						 className},
				   delegationSubject);
	    return is.booleanValue();
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void addNotificationListener(ObjectName name,
					NotificationListener listener,
					NotificationFilter filter,
					Object handback,
					Subject delegationSubject)
	    throws InstanceNotFoundException,
		   IOException {
	logger.trace("addNotificationListener", "name=" + name);

	final Integer listenerID = 
	    addListenerWithSubject(name,serialization.wrap(filter), 
				   delegationSubject, true);
	notifForwarder.addNotificationListener(listenerID,
		    name, listener, filter, handback, delegationSubject);
    }


    private Integer addListenerWithSubject(ObjectName name,
					   Object     wrappedFilter,
					   Subject    delegationSubject,
					   boolean    reconnect)
	throws InstanceNotFoundException, IOException {
	final boolean debug = logger.debugOn();
	if (debug)
	    logger.debug("addListenerWithSubject",
			 "(ObjectName,Object,Subject)");	

	final ObjectName[] names = {name};
	final Object[] filters = new Object[] {wrappedFilter};
	final Object[] params = new Object[] {names, filters};
	final int code =
	    MBeanServerRequestMessage.ADD_NOTIFICATION_LISTENERS;
	try {
	    Object o = mBeanServerRequest(code,params,delegationSubject,reconnect);
	    if (o instanceof Integer) { // compatible with RI1.0: bug 4948444
		return (Integer)o;
	    } else {
		return ((Integer[])o)[0];
	    }
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void addNotificationListener(ObjectName name,
					ObjectName listener,
					NotificationFilter filter,
					Object handback,
					Subject delegationSubject)
	    throws InstanceNotFoundException,
		   IOException {

	logger.trace("addNotificationListener", "called");

	try {
	    int code =
		MBeanServerRequestMessage.ADD_NOTIFICATION_LISTENER_OBJECTNAME;
	    mBeanServerRequest(code,
			       new Object[] {name,
					     listener,
					     serialization.wrap(filter),
					     serialization.wrap(handback)},
			       delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener,
					   Subject delegationSubject)
	    throws InstanceNotFoundException,
		   ListenerNotFoundException,
		   IOException {

	logger.trace("removeNotificationListener", "called");

	final Integer[] ids =
	    notifForwarder.removeNotificationListener(name, listener);

	try {
	    int code =
		MBeanServerRequestMessage.REMOVE_NOTIFICATION_LISTENER;
	    mBeanServerRequest(code,
			       new Object[] {name, ids},
			       delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (ListenerNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void removeNotificationListener(ObjectName name,
					   NotificationListener listener,
					   NotificationFilter filter,
					   Object handback,
					   Subject delegationSubject)
	    throws InstanceNotFoundException,
		   ListenerNotFoundException,
		   IOException {

	logger.trace("removeNotificationListener", "called");

	final Integer ids =
	    notifForwarder.removeNotificationListener(name, listener,
						      filter, handback);
	try {
	    int code = MBeanServerRequestMessage.
		REMOVE_NOTIFICATION_LISTENER_FILTER_HANDBACK;
	    mBeanServerRequest(code,
			       new Object[] {name, ids},
			       delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (ListenerNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void removeNotificationListener(ObjectName name,
					   ObjectName listener,
					   Subject delegationSubject)
	    throws InstanceNotFoundException,
		   ListenerNotFoundException,
		   IOException {

	logger.trace("removeNotificationListener", "called");

	try {
	    int code = MBeanServerRequestMessage.
		REMOVE_NOTIFICATION_LISTENER_OBJECTNAME;
	    mBeanServerRequest(code,
			       new Object[] {name,
					     listener},
			       delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (ListenerNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void removeNotificationListener(ObjectName name,
					   ObjectName listener,
					   NotificationFilter filter,
					   Object handback,
					   Subject delegationSubject)
	    throws InstanceNotFoundException,
		   ListenerNotFoundException,
		   IOException {

	logger.trace("removeNotificationListener", "called");

	try {
	    int code = MBeanServerRequestMessage.
		REMOVE_NOTIFICATION_LISTENER_OBJECTNAME_FILTER_HANDBACK;
	    mBeanServerRequest(code,
			       new Object[] {name,
					     listener,
					     serialization.wrap(filter),
					     serialization.wrap(handback)},
			       delegationSubject);
	} catch (InstanceNotFoundException e) {
	    throw e;
	} catch (ListenerNotFoundException e) {
	    throw e;
	} catch (Exception e) {
	    throw appropriateException(e);
	}
    }

    public void terminate() {
	logger.trace("terminate", "Terminated.");

	if (terminated) {
	    return;
	}

	terminated = true;

	communicatorAdmin.terminate();
	notifForwarder.terminate();
    }

//----------------------------------------------
// private classes
//----------------------------------------------
    //
    private class GenericClientCommunicatorAdmin
	    extends ClientCommunicatorAdmin {

	public GenericClientCommunicatorAdmin(long period) {
	    super(period);
	}

	protected void checkConnection() throws IOException {
	    try {
	       mBeanServerRequest(MBeanServerRequestMessage.GET_DEFAULT_DOMAIN,
				   null, null, false);
	    } catch (InterruptedIOException irie) {
		// see  6496038
		logger.trace("GenericClientCommunicatorAdmin-" +
			     "checkConnection",
			     "Timeout?", irie);

		if (requestTimeoutReconn) { // force the heartbeat to do reconnection
		    throw (IOException)EnvHelp.initCause(
							 new IOException(irie.getMessage()), 
							 irie);
		}

		// no exception.
		// not sure that the connection is lost, let the heartbeat to try again
		return;
	    } catch (Exception e) {
		throw appropriateException(e);
	    }
	}

	public void reconnectNotificationListeners(ClientListenerInfo[] old) 
	    throws IOException {

	    ClientListenerInfo[] clis = new ClientListenerInfo[old.length];
	    int j=0;

	    // reconnect listeners one by one...
	    //
	    for (int i=0; i<old.length; i++) {
		try {
		    Integer id = addListenerWithSubject(
					 old[i].getObjectName(),
					 serialization.wrap(old[i].getNotificationFilter()),
					 old[i].getDelegationSubject(),
					 false);

		    clis[j++] = new ClientListenerInfo(id,
						       old[i].getObjectName(),
						       old[i].getListener(),
						       old[i].getNotificationFilter(),
						       old[j].getHandback(),
						       old[i].getDelegationSubject());
		} catch (InstanceNotFoundException infe) {
		    logger.warning("reconnectNotificationListeners",
				   "Can't reconnect a listener for " +
				   old[i].getObjectName(), infe);
		}
	    }

	    // we should call postReconnection even j == 0, because we have to
	    // inform the notif forwarder of end of reconnection.
	    if (j != old.length) {
		ClientListenerInfo[] tmp = clis;
		clis = new ClientListenerInfo[j];
		System.arraycopy(tmp, 0, clis, 0, j);
	    }

	    notifForwarder.postReconnection(clis);
	}

	protected void doStart() throws IOException {
	    connection = client.reconnect();

	    // notif issues
	    final ClientListenerInfo[] old = notifForwarder.preReconnection();
	    reconnectNotificationListeners(old);
	}

	protected void doStop() {
	    try {
		client.close();
	    } catch (IOException ioe) {
		logger.info("close", ioe);
	    }
	}
    }

    //
    private class GenericClientNotifForwarder extends ClientNotifForwarder {
	public GenericClientNotifForwarder(Map env) {
	    super(env);
	}

	protected NotificationResult fetchNotifs(long clientSequenceNumber,
						 int maxNotifications,
						 long timeout)
		throws IOException, ClassNotFoundException {

	    logger.trace("GenericClientNotifForwarder-fetchNotifs",
			 "fetching notifs...");

	    final NotificationRequestMessage nreq =
		new NotificationRequestMessage(clientSequenceNumber,
					       maxNotifications, timeout);

	    final NotificationResponseMessage nresp =
		(NotificationResponseMessage) connection.sendWithReturn(nreq);

	    Object wrapped = nresp.getWrappedNotificationResult();
	    Object unwrapped = serialization.unwrap(wrapped, myloader);
	    if (!(unwrapped instanceof NotificationResult)) {
		// This is a protocol error, so we close the client.
		final String msg =
		    "Not a NotificationResult: " + unwrapped.getClass();
		logger.warning("Forwarder.fetchNotifs", msg);
		logger.warning("Forwarder.fetchNotifs", "closing connector");
		client.close();
		/* Cast below will generate a ClassCastException, but
		   anyway this thread is going to die. */
	    }
	    return (NotificationResult) unwrapped;
	}

	protected Integer addListenerForMBeanRemovedNotif()
		throws IOException, InstanceNotFoundException {

	    logger.trace("GenericClientNotifForwarder-" +
		   "addListenerForMBeanRemovedNotif",
		   "Add a listener to receive UNREGISTRATION_NOTIFICATION");

	    NotificationFilterSupport clientFilter =
		new NotificationFilterSupport();
	    clientFilter.enableType(
		 MBeanServerNotification.UNREGISTRATION_NOTIFICATION);

	    final ObjectName[] names = {delegateName};
	    final Object wrappedFilter = serialization.wrap(clientFilter);
	    final Object[] filters = {wrappedFilter};
	    final Object[] params = {names, filters};

	    try {
		int code =
		    MBeanServerRequestMessage.ADD_NOTIFICATION_LISTENERS;
		return (Integer) mBeanServerRequest(code, params, null);
	    } catch (InstanceNotFoundException n) {
		throw n;
	    } catch (Exception e) {
		throw appropriateException(e);
	    }
	}

	protected void removeListenerForMBeanRemovedNotif(Integer id)
		throws IOException {

	    logger.trace("GenericClientNotifForwarder-" +
			 "removeListenerForMBeanRemovedNotif",
			 "Remove the listener used to receive " +
			 "UNREGISTRATION_NOTIFICATION.");

	    try {
		int code = MBeanServerRequestMessage.
		    REMOVE_NOTIFICATION_LISTENER_FILTER_HANDBACK;
		mBeanServerRequest(code,
				   new Object[] {delegateName, id},
				   null, false);
	    } catch (Exception e) {
		throw appropriateException(e);
	    }
	}

	protected void lostNotifs(String message, long number) {
	    final String notifType = JMXConnectionNotification.NOTIFS_LOST;
	    final JMXConnectionNotification n =
		new JMXConnectionNotification(notifType,
					      ClientIntermediary.this,
					      connection.getConnectionId(),
					      lostNotifCounter++,
					      message,
					      new Long(number));
	    client.sendNotification(n);
	}

    }


    // Used by a GenericConnector
    public GenericClientCommunicatorAdmin getCommunicatorAdmin() {
	return communicatorAdmin;
    }

//----------------------------------------------
// private methods
//----------------------------------------------

    private Object mBeanServerRequest(int methodId,
				      Object[] params,
				      Subject delegationSubject)
	    throws Exception {
	return mBeanServerRequest(methodId, params, delegationSubject,
				  true);
    }

    private Object mBeanServerRequest(int methodId,
				      Object[] params,
				      Subject delegationSubject,
				      boolean reconnect)
	    throws Exception {

	MBeanServerRequestMessage req =
	    new MBeanServerRequestMessage(methodId,
					  params,
					  delegationSubject);

	MBeanServerResponseMessage resp;

	try {
	    resp = (MBeanServerResponseMessage)
		connection.sendWithReturn(req);
	} catch (IOException e) {
	    if (terminated || !reconnect ||
		e instanceof InterruptedIOException) throw e;
	    
	    communicatorAdmin.gotIOException(e);
	    
	    resp = (MBeanServerResponseMessage)
		connection.sendWithReturn(req);
	}

	Object wrappedResult = resp.getWrappedResult(); // may throw exception
	Object result;

	try {
	    result = serialization.unwrap(wrappedResult, myloader);
	} catch (ClassNotFoundException e) {
	    IOException ioe = new IOException(e.toString());
	    EnvHelp.initCause(ioe, e);
	    throw ioe;
	}
	
	if (resp.isException())
	    throw (Exception) result;
	
	return result;
    }

    /*
     * Throw an exception appropriate for e.  This method is called
     * from the final catch (Exception e) clause of methods that have
     * already caught all the exceptions they were expecting, except
     * IOException and RuntimeException.  If the exception is one of
     * those it is thrown.  Otherwise, it is wrapped in an IOException
     * and that is thrown.  This method is declared to return an
     * exception but never does.  This simply allows us to write
     * "throw appropriateException(e)" without getting errors about
     * variables not initialized or missing return statements.
     */
    private static IOException appropriateException(Exception e)
	    throws IOException {
	if (e instanceof IOException)
	    throw (IOException) e;
	if (e instanceof RuntimeException)
	    throw (RuntimeException) e;
        IOException ioe = new IOException("Unexpected exception: " + e);
	EnvHelp.initCause(ioe, e);
	throw ioe;
    }

//----------------------------------------------
// private variables
//----------------------------------------------

    private ClientSynchroMessageConnection connection;
    private GenericConnector client;

    private ObjectWrapping serialization;
    private ClassLoader myloader;

    private GenericClientNotifForwarder notifForwarder;

    private GenericClientCommunicatorAdmin communicatorAdmin;

    private long lostNotifCounter = 0;

    private boolean terminated;

    private final boolean requestTimeoutReconn;

    private static final ObjectName delegateName;
    static {
	try {
	    delegateName =
		new ObjectName("JMImplementation:type=MBeanServerDelegate");
	} catch (MalformedObjectNameException e) {
	    throw new RuntimeException(e.toString());
	}
    }

    private static final ClassLogger logger =
	new ClassLogger("javax.management.remote.generic",
			"ClientIntermediary");
}
