/*
 * @(#)file      GenericHttpRequestHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.40
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

// java import
//
import java.io.*;
import java.net.*;
import java.util.Set;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.Hashtable;

// jmx import
//
import javax.management.QueryExp;
import javax.management.MBeanInfo;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.ObjectInstance;
import javax.management.MBeanException;
import javax.management.OperationsException;
import javax.management.ReflectionException;
import javax.management.IntrospectionException;
import javax.management.InstanceNotFoundException;
import javax.management.AttributeNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.MalformedObjectNameException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InvalidAttributeValueException;
import javax.management.JMException;

// jdmk import
//
import com.sun.jdmk.ThreadContext;
import com.sun.jdmk.OperationContext;

class GenericHttpRequestHandler extends GenericHttpClientHandler {

    // CONSTRUCTOR
    //------------

    public GenericHttpRequestHandler(GenericHttpConnectorServer server,
                                     int id, GenericHttpSocket s,
                                     MBeanServer mbs, ObjectName name) {
        super(server, id, s, mbs, name);        
        if (mbs == null)
            throw new CommunicationException("No MBeanServer provided");
    }

    // PROTECTED METHODS
    // -----------------

    /**
     * Authenticates the request and returns null if it fails.  If the
     * authentication is disabled, this method returns an AuthInfo
     * object with login and password both null.  If authentication is
     * enabled and passes, this method returns the AuthInfo that was
     * used to authenticate successfully.
     */
    protected AuthInfo authenticateRequest(HttpRequest request) throws IOException {
        GenericHttpConnectorServer server =
            (GenericHttpConnectorServer) adaptorServer;
        if (!server.isAuthenticationOn())
            return makeNullAuthInfo();

        AuthInfo authInfo = null;
        final String authorization =
            request.getHeader(request.AUTHORIZATION_HEADER);
        if (authorization != null
            && authorization.startsWith("CRAM-MD5 ")) {
            String encoding = authorization.substring("CRAM-MD5 ".length());
            byte decoding[] = new BASE64Decoder().decodeBuffer(encoding);
            String response = new String(decoding);
            authInfo = server.checkChallengeResponse(response);
        }
        if (logger.finestOn()) {
            if (authInfo != null) {
                logger.finest("authenticateRequest", "Authentication succeeded");
            } else {
                logger.finest("authenticateRequest", "Authentication failed");
            }
        }
        return authInfo;
    }

    /**
     * Process an incoming post request and return the response.
     */
    protected HttpResponse processPostRequest(HttpRequest request)
            throws IOException {

        if (logger.finerOn()) {
            logger.finer("processPostRequest",
                  "Process a POST request = " + request.getURIPath());
        }

        //
        // Check that content is not null
        //
        if (request.getContentLength() == 0) {
            //
            // Send a Bad Request
            //
            final CommunicationException ce =
                new CommunicationException("Content is null");
            return makeExceptionResponse(ce);
        }

        //
        // Get remote operation to be performed
        //
        String remoteOp = null;
        Object result;
        String resultType;
        ByteArrayInputStream bIn;
        ObjectInputStream objIn;
        ThreadContext oldThreadContext = null;

        try {
            bIn = new ByteArrayInputStream(request.getContentBytes());
            objIn = new ObjectInputStream(bIn);

            /* An old (Java DMK 4.1) client, or a new (Java DMK 4.2) client that has not
               set an OperationContext, sends the remote operation string as
               the first item.  A new Java DMK 4.2 client sends the
               OperationContext first, then the same thing as a client without
               an OperationContext.  This is slightly more awkward than if we
               sent the OperationContext at the end, but has the advantage
               that we already have the context in place for the cases where
               we need to call mbs.deserialize.  */
            Object contextOrRemoteOp = objIn.readObject();
            if (contextOrRemoteOp instanceof OperationContext) {
                oldThreadContext =
                    ThreadContext.push("OperationContext", contextOrRemoteOp);
                contextOrRemoteOp = objIn.readObject();
            }
            remoteOp = (String) contextOrRemoteOp;

            if (logger.finerOn())
                logger.finer("doRun", "Remote operation: " + remoteOp);

            final ParsedMethod pm = (ParsedMethod) methodTable.get(remoteOp);

            if (pm == null) {
                if (logger.finerOn())
                    logger.finer("doRun", "Unknown remote operation: " + remoteOp);
                return makeErrorResponse(HttpDef.HTTP_ERROR_BAD_REQUEST_ID,
                                         HttpDef.HTTP_ERROR_BAD_REQUEST);
            }

            resultType = pm.resultType;

            if (pm.magicParam > 0) {
                final Object[] args = readObjects(objIn, pm.magicParam);
                switch (pm.methodNumber) {
                case CREATE_MBEAN_PARAMS:
                    objIn = mbs.deserialize((String) args[0],
                                            request.getContentBytes());
                    break;
                case CREATE_MBEAN_LOADER_PARAMS:
                    objIn = mbs.deserialize((String) args[0],
                                            (ObjectName) args[2],
                                            request.getContentBytes());
                    break;
                case INVOKE:
                case SET_ATTRIBUTE:
                case SET_ATTRIBUTES:
                    objIn = mbs.deserialize((ObjectName) args[0],
                                            request.getContentBytes());
                    break;
                default:
                    throw new Error("method marked as magic but not in " +
                                    "switch: " + remoteOp);
                }
                contextOrRemoteOp = objIn.readObject();
                if (contextOrRemoteOp instanceof OperationContext)
                    contextOrRemoteOp = objIn.readObject();
                if (!((String) contextOrRemoteOp).equals(remoteOp))
                    throw new Error("remoteOp changed after deserialize");
            }

            final Object[] args = readObjects(objIn, pm.nParams);
            result = doOperation(pm.methodNumber, args);

        } catch (Exception e) {
            if (logger.finerOn())
                logger.finer("doRun", remoteOp + ":" + e.getMessage());
            return makeExceptionResponse(e);
        } finally {
            if (oldThreadContext != null)
                ThreadContext.restore(oldThreadContext);
        }

        //
        // Serialize the result and send OK
        //
        final ByteArrayOutputStream bOut = serialize(resultType, result);
        return makeOkResponse(bOut.toByteArray());
    }

    Object doOperation(int methodNumber, Object[] args) throws Exception {
        switch (methodNumber) {
        case CREATE_MBEAN:
            return createMBean((String) args[0], (ObjectName) args[1]);
        case CREATE_MBEAN_PARAMS:
            return createMBean((String) args[0], (ObjectName) args[1],
                               (Object[]) args[2], (String[]) args[3]);
        case CREATE_MBEAN_LOADER:
            return createMBean((String) args[0], (ObjectName) args[1],
                               (ObjectName) args[2]);
        case CREATE_MBEAN_LOADER_PARAMS:
            return createMBean((String) args[0], (ObjectName) args[1],
                               (ObjectName) args[2], (Object[]) args[3],
                               (String[]) args[4]);
        case GET_ATTRIBUTE:
            return getAttribute((ObjectName) args[0], (String) args[1]);
        case GET_ATTRIBUTES:
            return getAttributes((ObjectName) args[0], (String[]) args[1]);
        case GET_DEFAULT_DOMAIN:
            return getDefaultDomain();
        case IS_INSTANCE_OF:
            return new Boolean(isInstanceOf((ObjectName) args[0],
                                            (String) args[1]));
        case GET_OBJECT_INSTANCE:
            return getObjectInstance((ObjectName) args[0]);
        case GET_MBEAN_COUNT:
            return getMBeanCount();
        case GET_MBEAN_SERVER_ID:
            return getMBeanServerId();
        case GET_MBEAN_INFO:
            return getMBeanInfo((ObjectName) args[0]);
        case INVOKE:
            return invoke((ObjectName) args[0], (String) args[1],
                          (Object[]) args[2], (String[]) args[3]);
        case IS_REGISTERED:
            return new Boolean(isRegistered((ObjectName) args[0]));
        case QUERY_NAMES:
            return queryNames((ObjectName) args[0], (QueryExp) args[1]);
        case QUERY_MBEANS:
            return queryMBeans((ObjectName) args[0], (QueryExp) args[1]);
        case SET_ATTRIBUTE:
            setAttribute((ObjectName) args[0], (Attribute) args[1]);
            return null;
        case SET_ATTRIBUTES:
            return setAttributes((ObjectName) args[0],
                                 (AttributeList) args[1]);
        case UNREGISTER_MBEAN:
            unregisterMBean((ObjectName) args[0]);
            return null;
        case REMOTE_REQUEST:
            final int opType = ((Integer) args[0]).intValue();
            return remoteRequest(opType, (Object[]) args[1]);
        case PING_HEART_BEAT_SERVER:
            return pingHeartBeatServer((String) args[0],
                                       ((Integer) args[1]).intValue(),
                                       ((Integer) args[2]).intValue(),
                                       (Long) args[3]);
        case SUPPORTS:
            return new Boolean(supports((String) args[0]));
        default:
            throw new Error("internal error: bad method number " +
                            methodNumber);
        }
    }

    private Object[] readObjects(ObjectInputStream s, int nObjects)
            throws IOException, ClassNotFoundException {
        Object[] objects = new Object[nObjects];
        for (int i = 0; i < nObjects; i++)
            objects[i] = s.readObject();
        return objects;
    }

    // HTTP SPECIFIC METHODS
    //----------------------

    /**
     * Generate a challenge if authentication is required.
     */
    protected String getChallenge() {
        GenericHttpConnectorServer server = (GenericHttpConnectorServer) adaptorServer;
        if (server.isAuthenticationOn()) {
            //
            // Generate CRAM-MD5 challenge and store expected response
            //
            String challenge = server.generateChallengeResponse();
            // The maximum number of bytes encoded at a time by the BASE64Encoder is 57.
            // This results in encoded lines being no more than 76 characters long.
            final int maxBytesPerLine = 57;
            String encoded_challenge = "";
            String chunk = null;
            int quotient = challenge.length() / maxBytesPerLine;
            int modulus  = challenge.length() % maxBytesPerLine;
            for (int i = 0; i < quotient; i++) {
                chunk = challenge.substring((i*maxBytesPerLine),(i+1)*maxBytesPerLine);
                encoded_challenge += new BASE64Encoder().encode(chunk.getBytes());
            }
            if (modulus > 0) {
                chunk = challenge.substring(quotient*maxBytesPerLine);
                encoded_challenge += new BASE64Encoder().encode(chunk.getBytes());
            }
            return "CRAM-MD5 " + encoded_challenge;
        } else {
            return null;
        }
    }

    // MAPPING OF CLIENT REQUESTS TO MBEANSERVER METHODS
    //--------------------------------------------------

    /**
     * ObjectInstance
     * createMBean(String className, ObjectName name) 
     * Creates an registers an instance of an MBean in the remote object server.
     */
    private ObjectInstance createMBean(String className, ObjectName name)
        throws InstanceAlreadyExistsException, MBeanException, MBeanRegistrationException,
               NotCompliantMBeanException, ReflectionException {
        if (logger.finerOn())
            logger.finer("createMBean", "createMBean");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.createMBean(className, name);
    }

    /**
     * ObjectInstance
     * createMBean(String className, ObjectName name, Object[] params, String[] signature) 
     * Creates and registers an instance of an MBean in the remote object server.
     */
    private ObjectInstance createMBean(String className, ObjectName name, Object[] params, String[] signature)
        throws InstanceAlreadyExistsException, MBeanException, MBeanRegistrationException,
               NotCompliantMBeanException, ReflectionException {
        if (logger.finerOn())
            logger.finer("createMBean", "createMBean");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.createMBean(className, name, params, signature);
    }

    /**
     * ObjectInstance
     * createMBean(String className, ObjectName name, ObjectName loaderName) 
     * Creates and registers an instance of an MBean in the remote object server.
     */
    private ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName)
        throws InstanceAlreadyExistsException, MBeanException, MBeanRegistrationException,
               NotCompliantMBeanException, ReflectionException, InstanceNotFoundException {
        if (logger.finerOn())
            logger.finer("createMBean", "createMBean");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.createMBean(className, name, loaderName);
    }

    /**
     * ObjectInstance
     * createMBean(String className, ObjectName name, ObjectName loaderName, Object[] params, String[] signature) 
     * Creates and registers an instance of an MBean in the remote object server.
     */
    private ObjectInstance createMBean(String className, ObjectName name, ObjectName loaderName, Object[] params, String[] signature)
        throws InstanceAlreadyExistsException, MBeanException, MBeanRegistrationException,
               NotCompliantMBeanException, ReflectionException, InstanceNotFoundException {
        if (logger.finerOn())
            logger.finer("createMBean", "createMBean");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.createMBean(className, name, loaderName, params, signature);
    }

    /**
     * Object
     * getAttribute(ObjectName name, String attribute) 
     * Gets the value of a specific attribute of a named MBean.
     */
    private Object getAttribute(ObjectName name, String attribute)
        throws AttributeNotFoundException, InstanceNotFoundException, MBeanException, ReflectionException {
        if (logger.finerOn())
            logger.finer("getAttribute", "getAttribute");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.getAttribute(name, attribute);
    }

    /**
     * AttributeList
     * getAttributes(ObjectName name, String[] attributes) 
     * Allows to retrieve the values of several attributes of an MBean.
     */
    private AttributeList getAttributes(ObjectName name, String[] attributes)
        throws InstanceNotFoundException, ReflectionException {
        if (logger.finerOn())
            logger.finer("getAttributes", "getAttributes");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.getAttributes(name, attributes);
    }

    /**
     * String
     * getDefaultDomain()
     * Returns the default domain used for the MBean naming.
     */
    private String getDefaultDomain() {
        if (logger.finerOn())
            logger.finer("getDefaultDomain", "getDefaultDomain");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.getDefaultDomain();
    }

    /**
     * boolean
     * isInstanceOf()
     * Returns true if the MBean is of an instance of class name
     */
    private boolean isInstanceOf(ObjectName name, String className) throws InstanceNotFoundException {
        if (logger.finerOn())
            logger.finer("isInstanceOf", "isInstanceOf");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.isInstanceOf(name, className);
    }


    /**
     * ObjectInstance
     * getObjectInstance(ObjectName name) 
     */
    private ObjectInstance getObjectInstance(ObjectName name) throws InstanceNotFoundException {
        if (logger.finerOn())
            logger.finer("getObjectInstance", "getObjectInstance");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.getObjectInstance(name);
    }

    /**
     * Integer
     * getMBeanCount() 
     * Returns the number of MBeans controlled by the MBeanServer.
     */
    private Integer getMBeanCount() {
        if (logger.finerOn())
            logger.finer("getMBeanCount", "getMBeanCount");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.getMBeanCount();
    }

    /**
     * MBeanInfo
     * getMBeanInfo(ObjectName name) 
     * This method supplies the exposed attributes and actions of the MBean.
     */
    private MBeanInfo getMBeanInfo(ObjectName name)
        throws InstanceNotFoundException, IntrospectionException, ReflectionException {
        if (logger.finerOn())
            logger.finer("getMBeanInfo", "getMBeanInfo");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.getMBeanInfo(name);
    }

    /**
     * String
     * getMBeanServerId() 
     * Return a string which represents the agent identification.
     */
    private String getMBeanServerId() {
        if (logger.finerOn())
            logger.finer("getMBeanServerId", "getMBeanServerId");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        // Get value from MBeanServerDelegate MBean
        try {
            return (String) mbs.getAttribute(new ObjectName("JMImplementation:type=MBeanServerDelegate") , "MBeanServerId");
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Object
     * invoke(ObjectName name, String methodName, Object[] params, String[] signature) 
     * Invokes a method of an MBean.
     */
    private Object invoke(ObjectName name, String methodName, Object[] params, String[] signature)
        throws InstanceNotFoundException, MBeanException, ReflectionException {
        if (logger.finerOn())
            logger.finer("invoke", "invoke");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.invoke(name, methodName, params, signature);
    }

    /**
     * boolean
     * isRegistered(ObjectName name)
     * Checks if the given MBean is registered with the MBeanServer.
     */
    private boolean isRegistered(ObjectName name) {
        if (logger.finerOn())
            logger.finer("isRegistered", "isRegistered");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.isRegistered(name);
    }

    /**
     * Set
     * queryNames(ObjectName name, QueryExp query) 
     * Gets the names of MBeans controlled by the MBeanServer.
     */
    private Set queryNames(ObjectName name, QueryExp query) {
        if (logger.finerOn())
            logger.finer("queryNames", "queryNames");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.queryNames(name, query);
    }

    /**
     * Set
     * queryMBeans(ObjectName name, QueryExp query) 
     * Gets the MBeans controlled by the MBeanServer.
     */
    private Set queryMBeans(ObjectName name, QueryExp query) {
        if (logger.finerOn())
            logger.finer("queryMBeans", "queryMBeans");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.queryMBeans(name, query);
    }

    /**
     * void
     * setAttribute(ObjectName name, Attribute attribute) 
     * Sets the value of a specific attribute of a named MBean.
     */
    private void setAttribute(ObjectName name, Attribute attribute)
        throws AttributeNotFoundException, InstanceNotFoundException,
               InvalidAttributeValueException, MBeanException, ReflectionException {
        if (logger.finerOn())
            logger.finer("setAttribute", "setAttribute");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        mbs.setAttribute(name, attribute);
    }

    /**
     * AttributeList
     * setAttributes(ObjectName name, AttributeList attributes) 
     * Allows to modify the values of several attributes of an MBean.
     */
    private AttributeList setAttributes(ObjectName name, AttributeList attributes)
        throws InstanceNotFoundException, ReflectionException {
        if (logger.finerOn())
            logger.finer("setAttributes", "setAttributes");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        return mbs.setAttributes(name, attributes);
    }

    /**
     * void
     * unregisterMBean(ObjectName name) 
     * Deletes an instance of an MBean in the remote MBean server.
     */
    private void unregisterMBean(ObjectName name) throws InstanceNotFoundException, MBeanRegistrationException {
        if (logger.finerOn())
            logger.finer("unregisterMBean", "unregisterMBean");
        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }
        mbs.unregisterMBean(name);
    }

    /**
     * Object[]
     * remoteRequest(int opType, Object[] params)
     * Transfers a notification request from the client to the agent.
     */
    private Object[] remoteRequest(int opType, Object[] params) throws Exception {
        if (logger.finerOn())
            logger.finer("remoteRequest", "remoteRequest");

        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }

        // Invoke remoteRequest method in server notification dispatcher.
        //
        GenericHttpConnectorServer server = (GenericHttpConnectorServer) adaptorServer;
        return server.serverNotificationDispatcher.remoteRequest(opType,params);
    }

    /**
     * String
     * pingHeartBeatServer(String sessionId, int period, int nretries, Long notifSessionId)
     * Transfers a ping request from the client to the agent.
     */
    private String pingHeartBeatServer(String sessionId, int period, int nretries, Long notifSessionId) {
        if (logger.finerOn())
            logger.finer("pingHeartBeatServer", "pingHeartBeatServer");

        if (!adaptorServer.isActive()) {
            throw new CommunicationException("Connector Server is OFFLINE");
        }

        // Invoke pingHeartBeatServer method in heartbeat server handler.
        //
        GenericHttpConnectorServer server = (GenericHttpConnectorServer) adaptorServer;
        return server.heartbeatServerHandler.pingHeartBeatServer(sessionId, period, nretries, notifSessionId);
    }

    private static final String[] supportedFeatures = {
        "OperationContext",
    };

    private boolean supports(String feature) {
        if (logger.finerOn())
            logger.finer("supports", feature);
        for (int i = 0; i < supportedFeatures.length; i++) {
            if (supportedFeatures[i].equals(feature))
                return true;
        }
        return false;
    }

    // TRACE METHODS
    //--------------

    /**
     * Returns the string used in debug traces.
     */
    protected String makeDebugTag() {
        return "GenericHttpRequestHandler[" + adaptorServer.getProtocol() + ":" + adaptorServer.getPort() + "][" + requestId + "]";
    }

    // PRIVATE VARIABLES
    //------------------

    /* This table drives the conversion of received requests into method calls.
       Each entry has a corresponding constant in the list immediately after
       the table, and if you change one you must change the other.  The
       constants are for use in a switch statement.

       Each entry is a string with three or four separated words.

       The first word is the request code that will be received from the
       remote client.

       The second word is the number of Object parameters to be read from
       the remote client for that request.

       The third word is the return type string for that request.  This is
       included for compatibility with previous versions of the protocol,
       but it is not actually used for anything, except to distinguish
       exception returns from normal returns whose value happens to be an
       object of type exception.

       The fourth word, if present, indicates that that many parameters should
       be read from the remote client in a first pass.  This is used when
       a non-default class loader specified by the these parameters must be
       used when reading the remaining parameters.
       
       IF YOU CHANGE THIS LIST YOU MUST CHANGE THE CONSTANTS IMMEDIATELY
       FOLLOWING IT.  */
    private static final String[] methodList = {
        "createMBean 2 ObjectInstance",
        "createMBeanParams 4 ObjectInstance 2",
        "createMBeanLoader 3 ObjectInstance",
        "createMBeanLoaderParams 5 ObjectInstance 3",
        "getAttribute 2 Object",
        "getAttributes 2 AttributeList",
        "getDefaultDomain 0 String",
        "isInstanceOf 2 Boolean",
        "getObjectInstance 1 ObjectInstance",
        "getMBeanCount 0 Integer",
        "getMBeanServerId 0 String",
        "getMBeanInfo 1 MBeanInfo",
        "invoke 4 Object 1",
        "isRegistered 1 Boolean",
        "queryNames 2 Set",
        "queryMBeans 2 Set",
        "setAttribute 2 Object 1",
        "setAttributes 2 AttributeList 1",
        "unregisterMBean 1 Object",
        "remoteRequest 2 Object[]",
        "pingHeartBeatServer 4 String",
        "supports 1 Boolean",
    };
    private static final int
        CREATE_MBEAN = 0,
        CREATE_MBEAN_PARAMS = 1,
        CREATE_MBEAN_LOADER = 2,
        CREATE_MBEAN_LOADER_PARAMS = 3,
        GET_ATTRIBUTE = 4,
        GET_ATTRIBUTES = 5,
        GET_DEFAULT_DOMAIN = 6,
        IS_INSTANCE_OF = 7,
        GET_OBJECT_INSTANCE = 8,
        GET_MBEAN_COUNT = 9,
        GET_MBEAN_SERVER_ID = 10,
        GET_MBEAN_INFO = 11,
        INVOKE = 12,
        IS_REGISTERED = 13,
        QUERY_NAMES = 14,
        QUERY_MBEANS = 15,
        SET_ATTRIBUTE = 16,
        SET_ATTRIBUTES = 17,
        UNREGISTER_MBEAN = 18,
        REMOTE_REQUEST = 19,
        PING_HEART_BEAT_SERVER = 20,
        SUPPORTS = 21;

    private static final class ParsedMethod {
        int methodNumber;
        int nParams;
        String resultType;
        int magicParam;
    }

    private static final Hashtable methodTable = new Hashtable();

    static {
        for (int i = 0; i < methodList.length; i++) {
            final String entry = methodList[i];
            final StringTokenizer tok = new StringTokenizer(entry);
            final ParsedMethod pm = new ParsedMethod();
            final String methodName = tok.nextToken();
            pm.methodNumber = i;
            pm.nParams = Integer.parseInt(tok.nextToken());
            pm.resultType = tok.nextToken();
            if (tok.hasMoreTokens())
                pm.magicParam = Integer.parseInt(tok.nextToken());
            else
                pm.magicParam = -1;
            if (tok.hasMoreTokens())
                throw new Error("invalid methodList entry: " + entry);
            methodTable.put(methodName, pm);
        }
    }
}
