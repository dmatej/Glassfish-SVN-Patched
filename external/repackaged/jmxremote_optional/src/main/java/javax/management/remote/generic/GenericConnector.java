/*
 * @(#)file      GenericConnector.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.85
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

import java.io.*;
import java.util.*;

import javax.management.*;
import javax.management.remote.NotificationResult;

import javax.management.remote.message.Message;
import javax.management.remote.message.MBeanServerRequestMessage;
import javax.management.remote.message.MBeanServerResponseMessage;
import javax.management.remote.message.NotificationRequestMessage;
import javax.management.remote.message.NotificationResponseMessage;
import javax.management.remote.NotificationResult;
import javax.management.remote.message.CloseMessage;
import javax.management.remote.JMXConnectionNotification;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXConnectorProvider; // javadoc
import javax.management.remote.JMXServiceURL; // javadoc

import javax.security.auth.Subject;

import com.sun.jmx.remote.generic.ObjectWrappingImpl;
import com.sun.jmx.remote.generic.DefaultConfig;
import com.sun.jmx.remote.generic.ClientSynchroMessageConnection;
import com.sun.jmx.remote.generic.ClientSynchroMessageConnectionImpl;
import com.sun.jmx.remote.generic.SynchroCallback;

import com.sun.jmx.remote.opt.util.ThreadService;

import com.sun.jmx.remote.opt.util.ClassLoaderWithRepository;
import com.sun.jmx.remote.opt.util.ClassLogger;
import com.sun.jmx.remote.opt.util.OrderClassLoaders;
import com.sun.jmx.remote.opt.util.EnvHelp;

/**
 * <p>A client connection to a remote JMX API server.  This class can
 * use a {@link MessageConnection} object to specify the transport for
 * communicating with the server.</p>
 *
 * <p>User code does not usually instantiate this class.  Instead, a
 * {@link JMXConnectorProvider} should be added to the {@link
 * JMXConnectorFactory} so that users can implicitly instantiate the
 * GenericConnector (or a subclass of it) through the {@link
 * JMXServiceURL} provided when connecting.</p>
 *
 * <p>The specific connector protocol to be used by an instance of
 * this class is specified by attributes in the <code>Map</code>
 * passed to the constructor or the {@link #connect(Map) connect}
 * method.  The attribute {@link #MESSAGE_CONNECTION} is the standard
 * way to define the transport.  An implementation can recognize other
 * attributes to define the transport differently.</p>
 */
public class GenericConnector implements JMXConnector {
    /**
     * <p>Name of the attribute that specifies the object wrapping for
     * parameters whose deserialization requires special treatment.
     * The value associated with this attribute, if any, must be an
     * object that implements the interface {@link ObjectWrapping}.</p>
     */
    public static final String OBJECT_WRAPPING =
	GenericConnectorServer.OBJECT_WRAPPING;

    /**
     * <p>Name of the attribute that specifies how this connector
     * sends messages to its connector server.  The value associated
     * with this attribute, if any, must be an object that implements
     * the interface {@link MessageConnection}.</p>
     */
    public static final String MESSAGE_CONNECTION =
	"jmx.remote.message.connection";

    // constructors

    /**
     * <p>Default no-arg constructor.</p>
     **/
    public GenericConnector() {
	// ------------------------------------------------------------
	//  WARNING - WARNING - WARNING - WARNING - WARNING - WARNING 
	// ------------------------------------------------------------
	// This constructor is needed in order for subclasses to be
	// serializable.
	//
	this((Map)null);
    }

    /**
     * <p>Constructor specifying connection attributes.</p>
     *
     * @param env the attributes of the connection.
     **/
    public GenericConnector(Map env) {
	// ------------------------------------------------------------
	//  WARNING - WARNING - WARNING - WARNING - WARNING - WARNING 
	// ------------------------------------------------------------
	// Initialize transient variables. All transient variables
	// that need a specific initialization must be initialized here.
	//
	rmbscMap = new WeakHashMap();
	lock     = new int[0];
	state    = CREATED;
	if (env == null) {
	    this.env = Collections.EMPTY_MAP;
	} else {
	    EnvHelp.checkAttributes(env);
            this.env = Collections.unmodifiableMap(env);
	}
	connectionBroadcaster = new NotificationBroadcasterSupport();
    }

    public void connect() throws IOException {
	connect(null);
    }

    public void connect(Map env) throws IOException {
	final boolean tracing = logger.traceOn();
	final String  idstr   = (tracing?"["+this.toString()+"]":null);

	synchronized(lock) {
	    switch (state) {
	    case CREATED: break;
	    case CONNECTED: 
		if (tracing)
		    logger.trace("connect",idstr + " already connected.");
		return;
	    case CLOSED: 
		if (tracing) logger.trace("connect",idstr + " already closed.");
		throw new IOException("Connector already closed.");
	    default:
		// should never happen
		if (tracing)
		    logger.trace("connect",idstr+" unknown state: "+state);
		throw new IOException("Invalid state ("+state+")");
	    }

            final Map tmpEnv =
		new HashMap((this.env==null)?Collections.EMPTY_MAP:this.env);

	    if (env != null) {
		EnvHelp.checkAttributes(env);
		tmpEnv.putAll(env);
	    }

	    MessageConnection conn =
		(MessageConnection) tmpEnv.get(MESSAGE_CONNECTION);
	    if (conn == null) {
		connection = DefaultConfig.getClientSynchroMessageConnection(tmpEnv);
		if (connection == null) {
		    if (tracing) 
			logger.trace("connect",idstr + " No MessageConnection");
		    throw new
			IllegalArgumentException("No MessageConnection");
		}
		if (tracing)
		    logger.trace("connect", "The connection uses a user "
				 +"specific Synchronous message connection.");
	    } else {
		requestHandler = new RequestHandler();

		connection = new ClientSynchroMessageConnectionImpl(conn, requestHandler, tmpEnv);
		if (tracing)
		    logger.trace("connect", "The connection uses a user "+
				 "specific Asynchronous message connection.");
	    }
	    connection.connect(tmpEnv);
	    connectionId = connection.getConnectionId();

	    objectWrapping = (ObjectWrapping) tmpEnv.get(OBJECT_WRAPPING);
	    if (objectWrapping == null)
		objectWrapping = new ObjectWrappingImpl();
	    	   
	    clientMBeanServer = new ClientIntermediary(connection,
				objectWrapping, this, tmpEnv);

	    this.env = tmpEnv;

	    state = CONNECTED;
	    if (tracing) logger.trace("connect",idstr+" "+connectionId+" Connected.");
	}

	notifThread = new ThreadService(0, 1);

	sendNotification(new JMXConnectionNotification(
		   JMXConnectionNotification.OPENED, 
		   this,
		   connectionId,
		   clientNotifID++,
		   null,
		   null));
    }

    public String getConnectionId() throws IOException {
	checkState();
	return connection.getConnectionId();
    }

    // implements client interface here
    public MBeanServerConnection getMBeanServerConnection() 
	throws IOException {
	return getMBeanServerConnection(null);
    }

    public MBeanServerConnection getMBeanServerConnection(
					       Subject delegationSubject)
	throws IOException {
	checkState();
	if (rmbscMap.containsKey(delegationSubject)) {
	    return (MBeanServerConnection) rmbscMap.get(delegationSubject);
	} else {
	    RemoteMBeanServerConnection rmbsc =
		new RemoteMBeanServerConnection(clientMBeanServer,
						delegationSubject);
	    rmbscMap.put(delegationSubject, rmbsc);
	    return rmbsc;
	}
    }

    public void close() throws IOException {
	close(false, "The connection is closed by a user.");
    }

    private void close(boolean local, String msg) throws IOException {
	final boolean tracing = logger.traceOn();
	final boolean debug   = logger.debugOn();
	final String  idstr   = (tracing?"["+this.toString()+"]":null);
	Exception         closeException;

	boolean createdState;
	synchronized(lock) {
	    if (state == CLOSED) {
		if (tracing) logger.trace("close",idstr + " already closed.");

		return;
	    }

	    createdState = (state == CREATED);

	    state = CLOSED;
	    closeException = null;

	    if (tracing) logger.trace("close",idstr + " closing.");

	    if (!createdState) {
		// inform the remote side of termination
		if (!local) {
		    try {
			synchronized(connection) {
			    connection.sendOneWay(new CloseMessage(msg));
			    Thread.sleep(100);
			}
		    } catch (InterruptedException ire) {
			// OK		    
		    } catch (Exception e1) {
			// error trace
			closeException = e1;
			if (tracing) logger.trace("close",idstr+
				      " failed to send close message: " + e1);
			if (debug) logger.debug("close",e1);
		    }
		}

		// close the transport protocol.
		try {
		    connection.close();
		} catch (Exception e1) {
		    closeException = e1;
		    if (tracing) logger.trace("close",idstr+
				  " failed to close MessageConnection: " + e1);
		    if (debug) logger.debug("close",e1);
		}
	    }

	    if (clientMBeanServer != null) {
		clientMBeanServer.terminate();
	    }

	    // Clean up MBeanServerConnection table
	    //
	    rmbscMap.clear();
	}

	// if not connected, no need to send closed notif
	if (!createdState) {
	    sendNotification(new JMXConnectionNotification(
			    JMXConnectionNotification.CLOSED,
			    this,
			    connectionId,
			    clientNotifID++,
			    "The client has been closed.", null));
	}

	if (closeException != null) {
	    if (closeException instanceof RuntimeException)
		throw (RuntimeException) closeException;
	    if (closeException instanceof IOException) 
		throw (IOException) closeException;
	    final IOException x = new IOException("Failed to close: " + 
						  closeException);
	    throw (IOException)EnvHelp.initCause(x,closeException);
	   
	}
	if (tracing) logger.trace("close",idstr + " closed.");
    }

    public void
	addConnectionNotificationListener(NotificationListener listener,
					  NotificationFilter filter,
					  Object handback) {
	if (listener == null) 
	    throw new NullPointerException("listener");
	connectionBroadcaster.addNotificationListener(listener, filter,
						      handback);
    }

    public void
	removeConnectionNotificationListener(NotificationListener listener)
	    throws ListenerNotFoundException {
	if (listener == null) 
	    throw new NullPointerException("listener");
	connectionBroadcaster.removeNotificationListener(listener);
    }

    public void
	removeConnectionNotificationListener(NotificationListener listener,
					     NotificationFilter filter,
					     Object handback)
	    throws ListenerNotFoundException {
	if (listener == null) 
	    throw new NullPointerException("listener");
	connectionBroadcaster.removeNotificationListener(listener, filter,
							 handback);
    }

    /**
     * <p>Send a notification to the connection listeners.  The
     * notification will be sent to every listener added with {@link
     * #addConnectionNotificationListener
     * addConnectionNotificationListener} that was not subsequently
     * removed by a <code>removeConnectionNotificationListener</code>,
     * provided the corresponding {@link NotificationFilter}
     * matches.</p>
     *
     * @param n the notification to send.  This will usually be a
     * {@link JMXConnectionNotification}, but an implementation can
     * send other notifications as well.
     */
    protected void sendNotification(final Notification n) {
	Runnable job = new Runnable() {
		public void run() {
		    try {			
			connectionBroadcaster.sendNotification(n);
		    } catch (Exception e) {
			// OK.
			// should never
		    }
		}
	    };

	    notifThread.handoff(job);
    }

//----------------------------------------------
// private classes
//----------------------------------------------

    private static class RemoteMBeanServerConnection
	    implements MBeanServerConnection {

        public RemoteMBeanServerConnection(ClientIntermediary ci) {
	    this(ci, null);
        }

        public RemoteMBeanServerConnection(ClientIntermediary ci, Subject ds) {
            this.ci = ci;
            this.ds = ds;
        }

        //----------------------------------------------
        // Implementation of MBeanServerConnection
        //----------------------------------------------

        public ObjectInstance createMBean(String className, ObjectName name)
            throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException,
            IOException {
            return ci.createMBean(className, name, ds);
        }

        public ObjectInstance createMBean(String className,
                                          ObjectName name,
                                          ObjectName loaderName)
            throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException,
            InstanceNotFoundException,
            IOException {
            return ci.createMBean(className, name, loaderName, ds);
        }

        public ObjectInstance createMBean(String className,
                                          ObjectName name,
                                          Object params[],
                                          String signature[])
            throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException,
            IOException {
            return ci.createMBean(className, name, params, signature, ds);
        }

        public ObjectInstance createMBean(String className,
                                          ObjectName name,
                                          ObjectName loaderName,
                                          Object params[],
                                          String signature[])
            throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException,
            InstanceNotFoundException,
            IOException {
            return ci.createMBean(className, name, loaderName,
                                  params, signature, ds);
        }

        public void unregisterMBean(ObjectName name)
            throws
            InstanceNotFoundException,
            MBeanRegistrationException,
            IOException {
            ci.unregisterMBean(name, ds);
        }

        public ObjectInstance getObjectInstance(ObjectName name)
            throws InstanceNotFoundException, IOException {
            return ci.getObjectInstance(name, ds);
        }

        public Set queryMBeans(ObjectName name, QueryExp query)
            throws IOException {
            return ci.queryMBeans(name, query, ds);
        }

        public Set queryNames(ObjectName name, QueryExp query)
            throws IOException {
            return ci.queryNames(name, query, ds);
        }

        public boolean isRegistered(ObjectName name)
            throws IOException {
            return ci.isRegistered(name, ds);
        }

        public Integer getMBeanCount()
            throws IOException {
            return ci.getMBeanCount(ds);
        }

        public Object getAttribute(ObjectName name, String attribute)
            throws
            MBeanException,
            AttributeNotFoundException,
            InstanceNotFoundException,
            ReflectionException,
            IOException {
            return ci.getAttribute(name, attribute, ds);
        }

        public AttributeList getAttributes(ObjectName name, String[] attributes)
            throws InstanceNotFoundException, ReflectionException, IOException {
            return ci.getAttributes(name, attributes, ds);
        }

        public void setAttribute(ObjectName name, Attribute attribute)
            throws
            InstanceNotFoundException,
            AttributeNotFoundException,
            InvalidAttributeValueException,
            MBeanException,
            ReflectionException,
            IOException {
            ci.setAttribute(name, attribute, ds);
        }

        public AttributeList setAttributes(ObjectName name,
                                           AttributeList attributes)
            throws InstanceNotFoundException, ReflectionException, IOException {
            return ci.setAttributes(name, attributes, ds);
        }

        public Object invoke(ObjectName name, String operationName,
                             Object params[], String signature[])
            throws
            InstanceNotFoundException,
            MBeanException,
            ReflectionException,
            IOException {
            return ci.invoke(name, operationName, params, signature, ds);
        }

        public String getDefaultDomain()
            throws IOException {
            return ci.getDefaultDomain(ds);
        }

        public String[] getDomains()
            throws IOException {
            return ci.getDomains(ds);
        }

        public void addNotificationListener(ObjectName name,
                                            NotificationListener listener,
                                            NotificationFilter filter,
                                            Object handback)
            throws InstanceNotFoundException, IOException {
            ci.addNotificationListener(name, listener, filter, handback, ds);
        }

        public void addNotificationListener(ObjectName name,
                                            ObjectName listener,
                                            NotificationFilter filter,
                                            Object handback)
            throws InstanceNotFoundException, IOException {
            ci.addNotificationListener(name, listener, filter, handback, ds);
        }

        public void removeNotificationListener(ObjectName name,
                                               ObjectName listener)
            throws
            InstanceNotFoundException,
            ListenerNotFoundException,
            IOException {
            ci.removeNotificationListener(name, listener, ds);
        }

        public void removeNotificationListener(ObjectName name,
                                               ObjectName listener,
                                               NotificationFilter filter,
                                               Object handback)
            throws
            InstanceNotFoundException,
            ListenerNotFoundException,
            IOException {
            ci.removeNotificationListener(name, listener, filter, handback, ds);
        }

        public void removeNotificationListener(ObjectName name,
                                               NotificationListener listener)
            throws
            InstanceNotFoundException,
            ListenerNotFoundException,
            IOException {
            ci.removeNotificationListener(name, listener, ds);
        }

        public void removeNotificationListener(ObjectName name,
                                               NotificationListener listener,
                                               NotificationFilter filter,
                                               Object handback)
            throws
            InstanceNotFoundException,
            ListenerNotFoundException,
            IOException {
            ci.removeNotificationListener(name, listener, filter, handback, ds);
        }

        public MBeanInfo getMBeanInfo(ObjectName name)
            throws
            InstanceNotFoundException,
            IntrospectionException,
            ReflectionException,
            IOException {
            return ci.getMBeanInfo(name, ds);
        }

        public boolean isInstanceOf(ObjectName name, String className)
            throws InstanceNotFoundException, IOException {
            return ci.isInstanceOf(name, className, ds);
        }

        private ClientIntermediary ci;
        private Subject ds;
    }

    private class RequestHandler implements SynchroCallback {

	public Message execute(Message msg) {	    
	    if (msg instanceof CloseMessage) {
		if (logger.traceOn())
		    logger.trace("RequestHandler-execute",
				      "got Message REMOTE_TERMINATION");


		// try to re-connect anyway
		try {
		    com.sun.jmx.remote.opt.internal.ClientCommunicatorAdmin admin
			= clientMBeanServer.getCommunicatorAdmin();
		    admin.gotIOException(new IOException(""));

		    return null;
		} catch (IOException ioe) {
		    // OK
		    // the server has been closed.
		}

		try {
		    GenericConnector.this.close(true, null);
		} catch (IOException ie) {
		    // OK never
		}
	    } else {
		final String errstr = 
		    ((msg==null)?"null":msg.getClass().getName())
		    +": Bad message type.";
		
		logger.warning("RequestHandler-execute",errstr);
		
		try {
		    logger.warning("RequestHandler-execute",
				   "Closing connector");
		    GenericConnector.this.close(false, null);
		} catch (IOException ie) {
		    logger.info("RequestHandler-execute", ie);
		}
	    }

	    return null;
	}

	public void connectionException(Exception e) {
	    synchronized(lock) {
		if (state != CONNECTED) {
		    return;
		}
	    }

	    logger.warning("RequestHandler-connectionException", e);

	    if (e instanceof IOException) {	    
		try {
		    com.sun.jmx.remote.opt.internal.ClientCommunicatorAdmin admin
			= clientMBeanServer.getCommunicatorAdmin();
		    admin.gotIOException((IOException)e);
		    
		    return;
		} catch (IOException ioe) {
		    // OK.
		    // closing at the following steps
		}
	    }

	    synchronized(lock) {
		if (state == CONNECTED) {
		    logger.warning("RequestHandler-connectionException", "Got connection exception: "+e.toString());
		    logger.debug("RequestHandler-connectionException", "Got connection exception: "+e.toString(), e);
		    
		    try {
			GenericConnector.this.close(true, null);
		    } catch (IOException ie) {
			logger.info("RequestHandler-execute", ie);
		    }
		}
	    }
	}
    }

    //
    private static class ResponseMsgWrapper {
	public boolean got = false;
	public Message msg = null;

	public ResponseMsgWrapper() {}

	public void setMsg(Message msg) {
	    got = true;
	    this.msg = msg;
	}
    }

// -------------------------------------------------
// package methods
// -------------------------------------------------
    /**
     * Called by a ClientIntermediary to reconnect the transport because
     * the server has been closed after its timeout.
     */
    ClientSynchroMessageConnection reconnect() throws IOException {
	synchronized(lock) {
	    if (state != CONNECTED) {
		throw new IOException(
		   "The connector is not at the connection state.");
	    }
	}

	sendNotification(new JMXConnectionNotification(
			 JMXConnectionNotification.FAILED,
			 this,
			 connectionId,
			 clientNotifID++,
			 "The client has got connection exception.", null));

	connection.connect(env);
	connectionId = connection.getConnectionId();

	sendNotification(new JMXConnectionNotification(
		      JMXConnectionNotification.OPENED,
		      this,
		      connectionId,
		      clientNotifID++,
		      "The client has succesfully reconnected to the server.", null));

	return connection;
    }

// -------------------------------------------------
// private methods
// -------------------------------------------------
    private void checkState() throws IOException {
	synchronized(lock) {
	    if (state == CREATED) {
		throw new IOException("The client has not been connected.");
	    } else if (state == CLOSED) {
		throw new IOException("The client has been closed.");
	    }
	}
    }

    // private variables

    // -------------------------------------------------------------------
    // WARNING - WARNING - WARNING - WARNING - WARNING - WARNING - WARNING
    // -------------------------------------------------------------------
    // 
    // SERIALIZATION ISSUES
    // --------------------
    //
    // All private variables must be defined transient. 
    //
    // Do not put any initialization here. If a specific initialization
    // is needed, put it in the empty default constructor.
    //
    // -------------------------------------------------------------------
    private transient ClientSynchroMessageConnection connection;
    private transient ObjectWrapping    objectWrapping;
    private transient Map env;

    private transient ClientIntermediary clientMBeanServer;
    private transient WeakHashMap rmbscMap;
    private transient String connectionId;

    private transient RequestHandler requestHandler;

    private transient final NotificationBroadcasterSupport 
	connectionBroadcaster;

    private transient ThreadService notifThread;

    // state
    private static final int CREATED = 1;
    private static final int CONNECTED = 2;
    private static final int CLOSED = 3;

    // default value is 0.
    private transient int state;
    private transient int[] lock;

    private transient long clientNotifID = 0;

    private static final ClassLogger logger =
	new ClassLogger("javax.management.remote.generic", "GenericConenctor");
}
