/*
 * @(#)file      RmiConnectorServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.48
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
 *
 */


package com.sun.jdmk.comm;



// jmx import
//
import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 * Provides an implementation of the <CODE>RmiConnectorServerMBean</CODE> interface.
 * <p>
 * When creating the <CODE>RmiConnectorServer</CODE> , it is possible to specify the port number and
 * service name to be used. To do so, you can use the appropriate constructor or use to corresponding setter.
 *
 * The default service name is specified by the constant com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER.
 * If you don't specify an object name when you register the <CODE>RmiConnectorServer</CODE>
 * within the <CODE>MBeanServer</CODE>, the default one is:
 * com.sun.jdmk.ServiceName.DOMAIN + ":" + com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  The legacy RMI connector,
 * including this class, may be removed in a future version of Java
 * DMK.  See {@link javax.management.remote.rmi} and {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 */

public class RmiConnectorServer extends CommunicatorServer
    implements MBeanRegistration, RmiConnectorServerMBean {

    // ===================================================================
    //
    // CONSTRUCTORS
    //
    // ===================================================================

    /**
     * Default constructor for <CODE>RmiConnectorServer</CODE>.
     * com.sun.jdmk.ServiceName.RMI_CONNECTOR_PORT is used as default port number and the local host as host name.
     * com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER is used as default port service name.
     */
    public RmiConnectorServer() {
        this(defaultPort,defaultServiceName);
    }

    /**
     * Constructor for <CODE>RmiConnectorServer</CODE>.
     * com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER is used as default port service name.
     */
    public RmiConnectorServer(int port) {
        this(port,defaultServiceName);
    }

    /**
     * Constructor for <CODE>RmiConnectorServer</CODE>.
     * com.sun.jdmk.ServiceName.RMI_CONNECTOR_PORT is used as default
     * port number and the local host as host name.
     */
    public RmiConnectorServer(String serviceName) {
        this(defaultPort,serviceName);
    }

    /**
     * Constructor for <CODE>RmiConnectorServer</CODE>.
     */
    public RmiConnectorServer(int port, String serviceName) {


        // ----------------------------
        // Call ancestor initialization
        // ----------------------------
        super(CommunicatorServer.RMI_TYPE);

        // ---------------------------
        // port initialization
        // ---------------------------
        this.port = port;
        this.serviceName = serviceName;

        // ---------------------------
        // Init private host var
        // ---------------------------
        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        //try {
        //    localHost= java.net.InetAddress.getLocalHost().getHostName();
        //} catch (Exception e) {
        //    if (logger.finerOn()) {
        //      logger.finer("RmiConnectorServer constructor","Set host to the default value (" + localHost + ")");
        //    }
        //    localHost= "localhost";
        //}
        // end NPCTE bugId 4770217
    }

    static String serviceNameForVersion(String serviceName, int version) {
        switch (version) {
        case 1: return serviceName;
        case 2: return serviceName + "V2";
        default:
            throw new IllegalArgumentException("serviceNameForVersion" +
                                               version);
        }
    }

    //
    // ===================================================================
    //
    // ServerNotificationHandlerInternal interface implementation
    //   
    // ===================================================================
    //   
        /** 
         * used to start the mode "push". A server connector should create a connector which 
         * allows communication from the server to client, by using the address provided as a 
         * parameter. 
         * 
         * @param addr a client connector address which can be used by the server connector 
         * to establish a connection.
         */ 
        NotificationBackConnector startPush(ConnectorAddress addr) {
                if ( ! (addr instanceof RmiConnectorAddress) )
                {
                        throw new java.lang.IllegalArgumentException ("Expected RmiConnectorAddress class, got " +  addr.getClass().getName() );
                }
                try
                {
                        RmiNotificationForwarder rmiNotificationForwarder = new RmiNotificationForwarder( (RmiConnectorAddress)addr ,this );
                        return rmiNotificationForwarder;
                }
                catch (java.lang.IllegalAccessException e)
                {
                        throw new CommunicationException(e);
                }
                catch (java.rmi.RemoteException e)
                {
                        throw new CommunicationException(e);
                }
        }
 
        /** 
         * used to tell a server connector to stop the mode "push", and to change to the mode "pull". 
         * 
         * @param connector the connector which is used for "push" mode. 
         */ 
        void stopPush(NotificationBackConnector connector) {
            if ( connector instanceof RmiNotificationForwarder) {
                ((RmiNotificationForwarder)connector).disconnect();
            }
        }
 

    //
    // ===================================================================
    // 
    // MBeanRegistration interface implementation
    //
    // ===================================================================
    //

    /**
     * Allows the MBean to perform any operations it needs before being
     * registered in the MBean server. 
     * If the name of the RMI connector server MBean is not specified, 
     * it is initialized with the default value:
     * {@link com.sun.jdmk.ServiceName#DOMAIN com.sun.jdmk.ServiceName.DOMAIN}:
     * {@link com.sun.jdmk.ServiceName#RMI_CONNECTOR_SERVER com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER}.
     * If any exception is raised, the RMI connector server MBean will not be registered in the MBean server.
     *
     * @param server The MBeanServer in which the MBean will be registered.
     * @param name The object name of the MBean.
     *
     * @return The name of the registered MBean.
     *
     * @exception Exception This exception should be caught by the MBeanServer
     * and re-thrown as an MBeanRegistrationException.
     */

    public ObjectName preRegister(MBeanServer server, ObjectName name) throws java.lang.Exception {
        // ------------------------------------------------
        // Identify the port and service name from the name
        // ------------------------------------------------
        String  propertyValue = null;
        if ( name == null )
            {
                name = new ObjectName (server.getDefaultDomain() + ":" + com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER);
            }

        // ------------------------------------------------
        // Call super 
        // ------------------------------------------------
        super.preRegister(server, name);

        // ------------------------------------------------
        // Return part
        // ------------------------------------------------
        return (name);
    }

    /**
     * Allows the MBean to perform any operations needed after having been
     * registered in the <CODE>MBeanServer</CODE> or after the registration has failed.
     *
     *@param registrationDone Indicates whether or not the MBean has been successfully registered in
     * the <CODE>MBeanServer</CODE>. The value false means that the registration phase has failed.
     */
    public void postRegister(Boolean registrationDone) {
        super.postRegister(registrationDone);
        NotificationHandlerInternal handler =
            new NotificationHandlerInternal(this);
        serverNotificationDispatcher =
            new ServerNotificationDispatcher(handler, topMBS);
        heartbeatServerHandler =
            new HeartBeatServerHandler(topMBS, serverNotificationDispatcher);
    }

    /**
     * Allows the MBean to perform any operations needed before being de-registered
     * by the <CODE>MBeanServer</CODE>.
     *
     *@exception java.lang.Exception  This exception should be caught by the <CODE>MBeanServer</CODE> and re-thrown
     *as an <CODE>MBeanRegistrationException</CODE>.
     */
    public void preDeregister() throws java.lang.Exception {
        super.preDeregister();
    }

    /**
     * Allows the MBean to perform any operations needed after having been
     * de-registered in the <CODE>MBeanServer</CODE>.
     */
    public void postDeregister() {
        super.postDeregister();
    }

    //
    // ===================================================================
    // 
    // RmiConnectorServerMBean interface implementation
    //
    // ===================================================================
    //

    /**
     * Returns the service name of this RMI object.
     *
     * @return The service name of this RMI connector.
     */
    public String getServiceName() {
        return serviceName;
    }

    /**
     * Set the service name of this RMI object.
     *
     * @param serviceName The service name of this RMI connector.
     *
     * @exception java.lang.IllegalStateException This method has been invoked while the
     * RMI connector was ONLINE or STARTING.
     */
    public void setServiceName(String serviceName) throws java.lang.IllegalStateException {
        if ((state == ONLINE) || (state == STARTING)) {
            throw new IllegalStateException("Stop server before carrying out this operation");
        }
        this.serviceName = serviceName;
        dbgTag = makeDebugTag();
    }

    /**
     * Returns the name of the protocol (rmi).
     */
    public String getProtocol() {
        return "rmi";
    }

    /**
     * Stops this connector server.
     * <p>
     * Has no effect if this RMI connector server is
     * <CODE>OFFLINE</CODE> or <CODE>STOPPING</CODE>.
     */
    public void stop() {
        if ((state == ONLINE) || (state == STARTING)) {
            super.stop();
            heartbeatServerHandler.cleanupClientResources();
        }
    }

    public synchronized void setMBeanServer(MBeanServer newMBS)
        throws IllegalArgumentException, IllegalStateException {
        super.setMBeanServer(newMBS);
        serverNotificationDispatcher.setMBeanServer(newMBS);
    }

    //
    // ===================================================================
    // 
    // superclass abstract method implementation
    //
    // ===================================================================
    //

    /**
     */
    protected void doError(Exception e) throws CommunicationException {
        return;
    }

    /**
     * Binds the adaptor server.
     * @exception CommunicationException if the adaptor can't bind to the specified port.
     * @exception InterruptedException when the adaptor is stopped.
     */
    protected synchronized void doBind()
            throws CommunicationException, InterruptedException {
        try {
            // ------------------------------
            // Create AdaptorServerRmi object
            // ------------------------------
            if (rmiConnectorV1 == null) {    
                if (logger.finerOn())
                    logger.finer("doBind","Create the RMI object server");
                if (logger.finestOn())
                    logger.finest("doBind","serviceName="+serviceName+" ; port="+port);
                String v1name = serviceNameForVersion(serviceName, 1);
                String v2name = serviceNameForVersion(serviceName, 2);
                rmiConnectorV2 =
                    new RmiConnectorServerObjectImplV2(this,
                                                       v2name, port,
                                                       serverNotificationDispatcher,
                                                       heartbeatServerHandler);
                rmiConnectorV1 =
                    new RmiConnectorServerObjectImpl(rmiConnectorV2,
                                                     v1name, port,
                                                     serverNotificationDispatcher,
                                                     heartbeatServerHandler);
            }

            // ---------------------------
            // Add adaptor in the registry
            // ---------------------------
            try {
                rmiConnectorV2.bind();
            } finally {
                /* Whether or not binding the V2 connector works, we want to
                   bind the V1 one.  If the V1 bind gets an exception, the
                   try...finally will propagate the same exception.  If the V1
                   bind succeeds but the V2 one got an exception, the
                   try...finally will propagate that exception.  */
                rmiConnectorV1.bind();
            }
            if (logger.finerOn())
                logger.finer("doBind","Bind RMI object server");
        } catch (java.rmi.RemoteException e) {
            if (logger.finestOn())
                logger.finest("doBind","Got Exception: " + e.getMessage());
            throw new CommunicationException(e);
        }
    }

    /**
     * Collects incoming requests.
     * @exception CommunicationException if the adaptor is not bound to the specified port.
     * @exception InterruptedException when the adaptor is stopped.
     */
    protected void doReceive() throws CommunicationException, InterruptedException {
        // -------------------------------------------------------------------
        // wait for invocations from clients
        // java IDL seams to create threads in daemon mode
        // We should create a thread in user mode to keep alive the adaptor
        // -------------------------------------------------------------------
        //  while true is required : an m-bean can send a 'notify-all'
        //  The current thread (keeping alive adaptor) will then be closed ...
        // -------------------------------------------------------------------
        while (true) {
            try {
                java.lang.Object sync = new java.lang.Object();
                synchronized (sync) {
                    sync.wait();
                }
            } catch (InterruptedException e) {
                throw e;
            } catch (Exception e) {
            }
        }
    }

    /**
     * Returns immediately.
     */
    protected void doProcess() throws CommunicationException, InterruptedException {
        return;
    }

    /**
     * Unbinds the adaptor.
     */
    protected void doUnbind() throws CommunicationException, InterruptedException {
        // -------------------------------
        // remove rmi object from registry
        // -------------------------------
        if (logger.finerOn())
            logger.finer("doUnbind","Unbind RMI object server");
        try {
            rmiConnectorV1.unbind();
        } finally {
            rmiConnectorV2.unbind();
        }
    }

    // ===================================================================
    //
    // Logging stuff
    //
    // ===================================================================

    protected String makeDebugTag() {
        return "RmiConnectorServer["+ getProtocol() + ":" + getPort() +
            ":" + getServiceName() + "]";
    }

    // ===================================================================
    //
    // Private class
    //
    // ===================================================================

    private class NotificationHandlerInternal implements ServerNotificationHandlerInternal {

        public NotificationHandlerInternal(RmiConnectorServer connector) {
            this.connector = connector;
        }

        /**
         * used to start the mode "push". A server connector should create a connector which
         * allows communication from the server to client, by using the address provided as a
         * parameter.
         *
         * @param addr a client connector address which can be used by the server connector
         * to establish a connection.
         */
        public NotificationBackConnector startPush(ConnectorAddress addr) {
            return connector.startPush(addr);
        }

        /**
         * used to tell a server connector to stop the mode "push", and to change to the mode "pull".
         *
         * @param connector the connector which is used for "push" mode.
         */
        public void stopPush(NotificationBackConnector connector) {
            this.connector.stopPush(connector);
        }

        private RmiConnectorServer connector;
    }

    // ===================================================================
    //
    // PRIVATE VARIABLES
    //
    // ===================================================================

    /**
     * The default port number.
     * @serial The default port number.
     */
    private static  int defaultPort = com.sun.jdmk.ServiceName.RMI_CONNECTOR_PORT;

    /**
     * The local host name.
     * @serial The local host name 
     */
    // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
    //private String localHost = "localhost";
    // end NPCTE bugId 4770217

    /**
     * The default service name.
     * @serial The default service name.
     */
    private static final String defaultServiceName =
        com.sun.jdmk.ServiceName.RMI_CONNECTOR_SERVER;

    /**
     * The service name.
     * @serial The service name.
     */
    private String serviceName = null;

    private transient RmiConnectorServerObjectImpl rmiConnectorV1;
    private transient RmiConnectorServerObjectImplV2 rmiConnectorV2;

    private ServerNotificationDispatcher serverNotificationDispatcher  = null;
    private RmiNotificationForwarder rmiNotificationForwarder = null;
    private HeartBeatServerHandler heartbeatServerHandler = null;

}
