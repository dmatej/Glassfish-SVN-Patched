/*
 * @(#)file      RmiConnectorServerObjectCommon.java
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
 *
 */

package com.sun.jdmk.comm;

//
// RMI import
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

//
// JMX import
import com.sun.jdmk.OperationContext;
import com.sun.jdmk.ThreadContext;
import javax.management.*;

import com.sun.jdmk.internal.ClassLogger;


/**
 * The <CODE>RmiConnectorServerObjectImpl</CODE> class provides an implementation of the
 * RmiConnectorServerObject RMI interface.<p>
 *
 */

abstract class RmiConnectorServerObjectCommon extends UnicastRemoteObject {


// --------------------------------
// Constructor
// --------------------------------
    public RmiConnectorServerObjectCommon(String serviceName, int port,
                                          ServerNotificationDispatcher
                                              serverNotificationDispatcher,
                                          HeartBeatServerHandler
                                              heartbeatServerHandler)
            throws RemoteException {

        logger = new ClassLogger(ClassLogger.LOGGER_LEGACY_RMI,
                                 ClassLogger.getClassName(this.getClass()));

        // NPCTE fix for bugId 4770217, esc 541597, MR, Oct 2002
        if (System.getProperty("jdmk.hostname")!=null)
            host = System.getProperty("jdmk.hostname");
        else {
            try {
                host = java.net.InetAddress.getLocalHost().getHostName();
            } catch (Exception e) {
                host = "localhost";
            }
        }
        if (logger.finerOn())
            logger.finer("RmiConnectorServerObjectCommon", "host="+host);
        // end of NPCTE fix for bugId 4770217

        // -------------------
        // Register the service within the Common Management Framework
        // Identify the port and service name from the name
        // -------------------
        this.serviceName = serviceName;
        this.port = port;
        this.serviceName = makeManagedObjFactoryName(host, port, serviceName);
        this.serverNotificationDispatcher = serverNotificationDispatcher;
        this.heartbeatServerHandler = heartbeatServerHandler;
    }

    /**
     * Method for making the name of the object factory.
     * This method is also used by the factory itself for constructing
     * the name and binding it to the registry.
     *
     * @param  host The host name of the registry (and factory) host.
     * @param  port The port number where the registry listens.
     * @param  name The name used to bind the object.
     *
     * @return A remote object name.
     */
    static String makeManagedObjFactoryName(String host, int port,
                                            String name) {

        // the RmiConnectorServer is registered to listen on all local 
        // interfaces,
        // so no need to specify a local host
        //return "rmi://" + host + ":" + port + "/" + name;
        return "rmi://:"+ port + "/" + name;
    }

// --------------------------------
// Connection / disconnection
// --------------------------------

    void bind() throws CommunicationException, RemoteException {
        // -------------------
        // Now declare the service to the RMI registry. In order to find
        // the configuration information required, we are going to query
        // a specific object containing host and port.
        // That's what we will do later ...
        // -------------------
        java.rmi.registry.Registry r = null;

        try {
            if (logger.finerOn())
                logger.finer("bind",
                      "Register object in RmiRegistry ; name="+serviceName);
            java.rmi.Naming.bind(serviceName, this);
            if (logger.finerOn())
                logger.finer("bind","Done");
        } catch (java.rmi.AlreadyBoundException e) {
            throw new CommunicationException(e, "Failed to register " +
                                             serviceName);
        } catch (java.net.MalformedURLException e) {
            throw new CommunicationException(e, "Failed to register " +
                                             serviceName);
        } catch (java.rmi.AccessException e) {
            throw new CommunicationException(e, "Failed to register " +
                                             serviceName);
        } catch (RemoteException e) {

            if (logger.finestOn()) {
                logger.finest("bind", "Failed to bind:"+e);
            }

            if (logger.finerOn()) {
                logger.finer("bind","Create Rmi registry port=" + port);
            }

            r = java.rmi.registry.LocateRegistry.createRegistry(port);
            if (logger.finerOn())
                logger.finer("bind","Creation done");
            try {
                if (logger.finerOn())
                    logger.finer("bind",
                          "Register object in RmiRegistry ; name="+
                          serviceName);
                java.rmi.Naming.bind(serviceName, this);
                if (logger.finerOn())
                    logger.finer("bind","Done");
            } catch (Exception x) {
                throw new CommunicationException(x, "Failed to register " +
                                                 serviceName);
            }
        }
        isActive = true;
     }

     void unbind() {
        // ------------------------
        // remove object in registry
        // ------------------------
        try {
            // ------------------------
            // unbind the Object Reference in Naming
            // ------------------------
            if (logger.finestOn())
                logger.finest("unbind",
                      "Unregister object in RmiRegistry ; name="+serviceName);
            java.rmi.Naming.unbind(serviceName);
            if (logger.finestOn())
                logger.finest("unbind","Done");
        } catch (Exception e) {
            // ------------------------
            // Don't throw exception
            // ------------------------
        }
        isActive = false;
    }

    void stopIfNotActive(String methodName) throws RemoteException {
        if (!isActive) {
            if (logger.finerOn())
                logger.finer(methodName, "Connector is OFFLINE.") ;
            throw new RemoteException("Connector is OFFLINE.") ;
        }
    }
 
    final ClassLogger logger;

    /**
     * Host name.
     */
    private String host;

    /**
     * Service Name
     */
    private String serviceName = null;

    /**
     * Port number.
     */
    private int port;

    private boolean isActive = false;

    /**
     * Notifications
     */
    ServerNotificationDispatcher serverNotificationDispatcher = null;

    /**
     * HeartBeat
     */
    HeartBeatServerHandler heartbeatServerHandler = null;
}
