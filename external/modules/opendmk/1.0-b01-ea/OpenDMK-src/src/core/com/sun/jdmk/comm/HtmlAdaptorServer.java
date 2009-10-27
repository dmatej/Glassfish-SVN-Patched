/*
 * @(#)file      HtmlAdaptorServer.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.65
 * @(#)lastedit      07/03/08
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


// @(#)HtmlAdaptorServer.java 1.65 07/03/08 

// java import
//
import java.io.IOException;
import java.util.Iterator;
import java.lang.reflect.Constructor;
import java.io.InterruptedIOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.InetAddress;
import java.lang.InterruptedException;
import java.util.Enumeration;
import java.util.Vector;
import java.util.StringTokenizer;
import java.util.NoSuchElementException;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.MalformedObjectNameException;
import javax.management.MBeanRegistration;
import javax.management.ReflectionException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.ServiceNotFoundException;
import javax.management.DynamicMBean;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanInfo;
import javax.management.AttributeNotFoundException;
import javax.management.RuntimeOperationsException;
import javax.management.InvalidAttributeValueException;
import javax.management.AttributeList;
import javax.management.Attribute;
import javax.management.ObjectInstance;
import javax.management.MBeanServerFactory;
import javax.management.loading.ClassLoaderRepository;


/**
 * Acts as an HTML server which allows an HTML browser to manage all MBeans
 * in the agent. The HTML protocol adaptor is implemented as a dynamic MBean.
 * <P>
 * To connect a browser to the agent open this page in a web browser:
 * <UL>
 * </CODE>http://host:port</CODE>
 * </UL>
 * where:
 * <UL>
 * <LI><I>host</I> is the host name of the machine on which the agent is running.
 * <LI><I>port</I> is the port number used by the HTML server in the agent. (default: 8082)
 * </UL>
 * The HTML protocol adaptor provides the following main HTML pages for managing MBeans in an agent:
 * <UL>
 * <LI><I>Agent View</I>: Provides a list of object names of all the MBeans registered in the agent.
 * <LI><I>Agent Administration</I>: Registers and unregisters MBeans in the agent.
 * <LI><I>MBean View</I>: Reads and writes MBean attributes and perform operations on MBeans in the agent.
 * </UL>
 * <P>
 * When the HTML protocol adaptor is started it creates a TCP/IP socket and listens for
 * client connections.
 * <P>
 * The default port number of the socket is 8082. This default value can be changed by specifying a port number:
 * <UL>
 * <LI>in the object constructor
 * <LI>using the {@link com.sun.jdmk.comm.CommunicatorServer#setPort setPort} method before starting the adaptor
 * </UL>
 * The default object name is define by {@link com.sun.jdmk.ServiceName#DOMAIN com.sun.jdmk.ServiceName.DOMAIN} 
 * and {@link com.sun.jdmk.ServiceName#HTML_ADAPTOR_SERVER com.sun.jdmk.ServiceName.HTML_ADAPTOR_SERVER}.
 * <P>
 * If a client tries to connect, the  <CODE>HtmlAdaptorServer</CODE> creates a thread 
 * which receives and processes all subsequent requests from this client. The number of
 * clients is limited by the <CODE>maxActiveClientCount</CODE> property. The default value
 * of the <CODE>maxActiveClientCount</CODE> is 10.
 * <P>
 * When an <CODE>HtmlAdaptorServer</CODE> is stopped, all current HTTP connections
 * are interrupted (some requests may be terminated abruptly), and the TCP/IP
 * socket is closed.
 * <P>
 * The <CODE>HtmlAdaptorServer</CODE> can perform user authentication. The add/remove
 * user authentication info methods can be used to manage users and their corresponding
 * authentication information. The HTML server uses the 'Basic Authentication Scheme'
 * (as defined in RFC 1945, section 11.1) to authenticate clients connecting to the server.
 * <P>
 * Limitations of the HTML protocol adaptor:
 * <UL>
 * <LI>The minimum value for the reload period is 5 seconds. (0 defaults to no reloading)
 * <LI>Array of class are always displayed in read only mode.
 * <LI>Arrays of dimension 2 and higher are not fully expanded.
 * <LI>List of supported attribute types (for reading and writing):
 * <UL>
 * <LI><CODE>boolean boolean[] Boolean Boolean[]</CODE>
 * <LI><CODE>byte Byte Byte[]</CODE>
 * <LI><CODE>char char[] Character Character[]</CODE>
 * <LI><CODE>Date Date[]</CODE> e.g. September 30, 1999 8:49:04 PM CEST
 * <LI><CODE>double double[] Double Double[]</CODE>
 * <LI><CODE>float float[] Float Float[]</CODE>
 * <LI><CODE>int int[] Integer Integer[]</CODE>
 * <LI><CODE>long Long Long[]</CODE>
 * <LI><CODE>Number</CODE>
 * <LI><CODE>javax.management.ObjectName javax.management.ObjectName[]</CODE>
 * <LI><CODE>short Short Short[]</CODE>
 * <LI><CODE>String String[]</CODE>
 * <LI><CODE>com.sun.jdmk.Enumerated</CODE>: Supported for readable attributes. As <CODE>com.sun.jdmk.Enumerated</CODE> is an abstract class, 
 *     only write-only attributes whose actual subclass is declared in the signature of its setter, can be set through the Html adaptor.
 * <LI>For unsupported readable attribute types, if not null, the <CODE>toString()</CODE> method is called.
 * <LI> If the getter of a readable attribute throw an exception, the thrown exception name and message are displayed, and this attribute cannot be set through the Html Adaptor even though it were a read-write attribute.
 * </UL>
 * <LI>List of supported operation and constructor parameter types:
 * <UL>
 * <LI><CODE>boolean Boolean</CODE>
 * <LI><CODE>byte Byte</CODE>
 * <LI><CODE>char Character</CODE>
 * <LI><CODE>Date</CODE> e.g. September 30, 1999 8:49:04 PM CEST
 * <LI><CODE>double Double </CODE>
 * <LI><CODE>float Float</CODE>
 * <LI><CODE>int Integer</CODE>
 * <LI><CODE>long Long</CODE>
 * <LI><CODE>Number</CODE>
 * <LI><CODE>javax.management.ObjectName</CODE>
 * <LI><CODE>short Short</CODE>
 * <LI><CODE>String</CODE>
 * </UL>
 * <P><I>Note 1</I>: When reading a value of type <CODE>Number</CODE> the server tries to convert it
 * first to an Integer, then a Long, then a Float and finally a Double, stopping at the first
 * which succeeds.
 * <P><I>Note 2</I>: Use the "Reload" button displayed in the HTML page of an MBean view rather than the reload button of the web-browser, otherwise you may invoke again the setters of all attributes if this was your last action. 
 * </UL>
 */
public class HtmlAdaptorServer
    extends CommunicatorServer
    implements MBeanRegistration, DynamicMBean {


    // --------------------------------------------------------
    // CONSTRUCTORS
    // --------------------------------------------------------

    /**
     * Constructs an <CODE>HtmlAdaptorServer</CODE> that will use the default port (8082).
     * <P>
     * The default port is defined in com.sun.jdmk.ServiceName.HTML_ADAPTOR_PORT.
     */
    public HtmlAdaptorServer () {
        this(com.sun.jdmk.ServiceName.HTML_ADAPTOR_PORT);
    }

    /**
     * Constructs the <CODE>HtmlAdaptorServer</CODE> that will use the specified port.
     *
     * @param port An integer representing a valid port number.
     */
    public HtmlAdaptorServer(int port) {
        super(CommunicatorServer.HTML_TYPE);
        this.port = port;
        maxActiveClientCount = 10;
        dbgTag = makeDebugTag();
        buildInfo();
    }

    /**
     * Constructs the <CODE>HtmlAdaptorServer</CODE> that will use the 
     * specified port and user authentication information list.
     *
     * @param port An integer representing a valid port number.
     * @param authInfoList The user authentication information list.
     */
    public HtmlAdaptorServer(int port, AuthInfo[] authInfoList) {
        this(port);
	if (authInfoList != null) {
	    for (int i = 0; i < authInfoList.length; i++) {
		addUserAuthenticationInfo(authInfoList[i]);
	    }
	}
    }

    // --------------------------------------------------------
    // PUBLIC METHODS
    // --------------------------------------------------------

    /**
     * Sets the MBean that will be used to parse the incoming HTML requests
     * or to built the outgoing HTML pages. This MBean must implement
     * {@link com.sun.jdmk.comm.HtmlParser HtmlParser} and must already
     * be registered in the agent's MBean server.
     *
     * @param parser the <CODE>ObjectName</CODE> of the HTML parser MBean.
     *
     * @exception InstanceNotFoundException
     *      The HTML parser does not exist in the repository.
     * @exception ServiceNotFoundException
     *      The requested service is not supported.
     */
    public void setParser(ObjectName parser)
        throws InstanceNotFoundException, ServiceNotFoundException {
        
	// if parser is null, just reset it and return.
	if (parser == null) {
	    resetParser();
	    return;
	}
	
	boolean isParser = 
	    topMBS.isInstanceOf(parser,
				com.sun.jdmk.comm.HtmlParser.class.getName());
	if (!isParser)
	    throw new ServiceNotFoundException("The HtmlParser interface " + 
					       "is not implemented by the " + 
					       "MBean = " + parser);
	userParser = parser;
    }
    
    /**
     * Returns the ObjectName of the currently designated HTML parser MBean.
     *
     * @return The <CODE>ObjectName</CODE> that is the current value of the Parser attribute.
     */
    public ObjectName getParser() {
        return userParser;
    }

    /**
     *  Sets the ObjectName of the Parser attribute to null
     */
    public void resetParser() {
        userParser = null;
    }

    /**
     * Creates and registers in the <CODE>MBeanServer</CODE> the HTML parser 
     * MBean used by the <CODE>HtmlAdaptorServer</CODE> to parse incoming 
     * requests or build outgoing HTML pages.
     *
     * @param className the class that contains the parsing procedures
     * @param parserName the String used as an object name to register 
     *        the parser into the <CODE>MBeanServer</CODE>
     * @param loaderName the loader user to obtain the class of the
     *        parser
     *
     * @exception MalformedObjectNameException The string passed in the 
     *            parameters does not have the right format.
     * @exception ReflectionException Wraps a {@link ClassNotFoundException}
     *            or a {@link java.lang.Exception} that occurred when trying 
     *            to invoke the MBean's constructor.
     * @exception  InstanceAlreadyExistsException The MBean is already under 
     *             the control of the <CODE>MBeanServer</CODE>.
     * @exception MBeanRegistrationException The <CODE>preRegister</CODE> 
     *            (<CODE>MBeanRegistration</CODE> interface) method of the 
     *            MBean has thrown an exception. The MBean will not be 
     *            registered.
     * @exception MBeanException The constructor of the MBean has 
     *            thrown an exception.
     * @exception InstanceNotFoundException The specified class loader is 
     *            not registered in the <CODE>MBeanServer</CODE>.
     */
    
    public void createParser(String className, String parserName, 
			     String loaderName) 
        throws MalformedObjectNameException,
               ReflectionException,
               InstanceAlreadyExistsException,
               MBeanRegistrationException,
               MBeanException,
               NotCompliantMBeanException,
               InstanceNotFoundException {

        // Instanciate the parser.
        //
        Object theParser = null;
        if (loaderName != null && loaderName.length() > 0) {
            ObjectName loaderObN = new ObjectName(loaderName);
            theParser = topMBS.instantiate(className, loaderObN);
        } else {
            theParser = topMBS.instantiate(className);
        }

        // Validate the parser.
        //
	if (!com.sun.jdmk.comm.HtmlParser.class.isAssignableFrom(theParser.getClass())) {
	    theParser = null;
	    throw new MBeanException(new ServiceNotFoundException("The HtmlParser interface is not implemented by the MBbean = "+className));
	}

        // Register the parser.
        //
        ObjectName parserObN = new ObjectName(parserName);
        topMBS.registerMBean(theParser, parserObN);

        // Set the parser.
        //
        try {
            setParser(parserObN);
        } catch (ServiceNotFoundException snfe) {
            if (logger.finestOn()) {
                logger.finest("setParser"," Service not found. [Exception="+snfe+"]");
            }
            throw new MBeanException(snfe);
        }
    }

    /**
     * Gets the IP address of the last connected client.
     *
     * @return The IP address of the last connected client or "unknown" if none.
     *
     * @see java.net.InetAddress
     */
    public String getLastConnectedClient() {
        if (addrLastClient == null) {
            return "unknown";
        }
        return addrLastClient.getHostAddress();
    }

    /**
     * Returns the protocol of this <CODE>HtmlAdaptorServer</CODE>.
     *
     * @return  The string "html".
     */
    public String getProtocol() {
        return "html";
    }
  
    /**
     * Gets the number of clients that have been processed by this <CODE>HtmlAdaptorServer</CODE> 
     * since its creation.
     *
     * @return The number of clients handled by this <CODE>HtmlAdaptorServer</CODE>
     *         since its creation. This counter is not reset by the <CODE>stop</CODE> method.
     */
    public int getServedClientCount() {
        return super.getServedClientCount();
    }

    /**
     * Gets the number of clients currently being processed by this 
     * <CODE>HtmlAdaptorServer</CODE>.
     *
     * @return The number of clients currently being processed by this 
     *         <CODE>HtmlAdaptorServer</CODE>.
     */
    public int getActiveClientCount() {
        return super.getActiveClientCount();
    }

    /**
     * Gets the maximum number of clients that this 
     * <CODE>HtmlAdaptorServer</CODE> can process concurrently.
     *
     * @return The maximum number of clients that this 
     *    <CODE>HtmlAdaptorServer</CODE> can process concurrently.
     */
    public int getMaxActiveClientCount() {
        return super.getMaxActiveClientCount();
    }

    /**
     * Sets the maximum number of clients this 
     * <CODE>HtmlAdaptorServer</CODE> can process concurrently.
     *
     * @param c The number of clients.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the communicator was ONLINE or STARTING.
     */
    public void setMaxActiveClientCount(int c) 
	throws java.lang.IllegalStateException {
	super.setMaxActiveClientCount(c);
    }

    /**
     * Adds the authentication information of the user to be authenticated by this server.
     * In order to populate the list of users supported by this server, invoke this method
     * for each user you want to add. If the user already exists, then update his authentication
     * information.
     *
     * @param authinfo the user authentication information.
     */
    public synchronized void addUserAuthenticationInfo(AuthInfo authinfo) {
        if (authinfo != null) {
            // Check if user already exists. If true, update his password.
            //
            String username = authinfo.getLogin();
            for (Enumeration e = authInfo.elements(); e.hasMoreElements(); ) {
                AuthInfo ai = (AuthInfo) e.nextElement();
                if (ai.getLogin().equals(username)) {
                    authInfo.removeElement(ai);
                    break;
                }
            }
            authInfo.addElement(authinfo);
        }
    }

    /**
     * Removes the authentication information of the given user from the
     * list of users authenticated by this server.
     *
     * @param authinfo the user authentication information.
     */
    public synchronized void removeUserAuthenticationInfo(AuthInfo authinfo) {
        if (authinfo != null) {
            // Check if user exists.
            //
            String username = authinfo.getLogin();
            for (Enumeration e = authInfo.elements(); e.hasMoreElements(); ) {
                AuthInfo ai = (AuthInfo) e.nextElement();
                if (ai.getLogin().equals(username)) {
                    authInfo.removeElement(ai);
                    break;
                }
            }
        }
    }

    /**
     * Returns true if the list of users supported by this server is not empty.
     *
     * @return True, if the list of users supported by this server is not empty. False, if
     * the list of supported users is empty so that no authentication is performed by this server.
     */
    public boolean isAuthenticationOn() {
        return (!authInfo.isEmpty());
    }
    
    /**
     * Stops the HTML protocol adaptor.
     * <p> 
     * Has no effect if this SNMP protocol adaptor is <CODE>OFFLINE</CODE> or 
     * <CODE>STOPPING</CODE>.
     */
    public void stop() {
        if ((state == ONLINE) || (state == STARTING)) {
            super.stop();
            try {
                Socket s = new Socket(java.net.InetAddress.getLocalHost(),port);
                s.close();
            } catch (IOException ioe) {
                if (logger.finestOn()) {
                    logger.finest("stop","I/O exception. [Exception="+ioe+"]");
                }
            }
        }
    }

    /**
     * Creates a TCP/IP connection to listen for client connections.
     * <P> 
     * If no object name is provided, the server will use a default object name
     * (defaultDomain:name=HtmlAdaptorServer).
     *
     *@param server The MBeanServer in which the HTML protocol adaptor will be registered.
     *
     *@param name The object name of the HTML protocol adaptor or null to specify the default object name
     * (defaultDomain:name=HtmlAdaptorServer).
     *
     *@return  The name of the HTML protocol adaptor registered.
     *
     *@exception java.langException This exception should be caught by the <CODE>MBeanServer</CODE> and re-thrown
     *as an <CODE>MBeanRegistrationException</CODE>.
     */
    public ObjectName preRegister(MBeanServer server, ObjectName name) 
        throws java.lang.Exception {

        if (name == null) {
            name = new ObjectName(server.getDefaultDomain() +":"+ com.sun.jdmk.ServiceName.HTML_ADAPTOR_SERVER);
        }
        return (super.preRegister(server, name));
    }

    /**
     * Does nothing, needed for <CODE>MBeanRegistration</CODE> interface.
     */
    public void postRegister(Boolean registrationDone) {
        super.postRegister(registrationDone);
    } 
    
    /**
     * Interrupts all the current HTTP connections and close the TCP/IP socket.
     *
     *@exception java.lang.Exception  This exception should be caught by the MBeanServer and re-thrown
     * as an MBeanRegistrationException.
     */
    public void preDeregister()
        throws java.lang.Exception {
        super.preDeregister();
    }
    
    /**
     * Does nothing, needed for <CODE>MBeanRegistration</CODE> interface.
     */
    public void postDeregister() {
        super.postDeregister();
    }
    
    /**
     * Gets the exposed attributes and actions of the HTML protocol adaptor.
     * 
     * @return An instance of MBeanInfo containing all attributes and actions exposed by the HTML protocol adaptor.
     */
    public MBeanInfo getMBeanInfo() {
        return dmbeaninfo;
    }

    /**
     * Gets the value of a specific attribute of the HTML protocol adaptor.
     * <P>
     * Gets the values for the following attributes of the HTML protocol adaptor:
     * <P>
     * <TABLE>
     * <TR><TH>Name</TH><TH>Type</TH><TH>Description</TH></TR>
     * <TR><TD ALIGN="right">Active</TD><TD ALIGN="center">boolean</TD><TD>True if the HtmlAdaptorServer is in the ONLINE state.</TD></TR>
     * <TR><TD ALIGN="right">ActiveClientCount</TD><TD ALIGN="center">int</TD><TD>The number of clients being processed currently by the HtmlAdaptorServer.</TD></TR>
     * <TR><TD ALIGN="right">AuthenticationOn</TD><TD ALIGN="center">boolean</TD><TD>True if the HtmlAdaptorServer requests authentication.</TD></TR>
     * <TR><TD ALIGN="right">Host</TD><TD ALIGN="center">java.lang.String</TD><TD>Hostname.</TD></TR>
     * <TR><TD ALIGN="right">LastConnectedClient</TD><TD ALIGN="center">java.lang.String</TD><TD>The IP address of the last connected client.</TD></TR>
     * <TR><TD ALIGN="right">MaxActiveClientCount</TD><TD ALIGN="center">int</TD><TD>The maximum number of clients the HtmlAdaptorServer can process concurrently.</TD></TR>
     * <TR><TD ALIGN="right">Parser</TD><TD ALIGN="center">{@link javax.management.ObjectName ObjectName}</TD><TD>ObjectName of the MBean used to customized HTML pages generated by the HtmlAdaptorServer.</TD></TR>
     * <TR><TD ALIGN="right">Port</TD><TD ALIGN="center">int</TD><TD>Port number used.</TD></TR>
     * <TR><TD ALIGN="right">Protocol</TD><TD ALIGN="center">java.lang.String</TD><TD>The communication protocol supported. (html)</TD></TR>
     * <TR><TD ALIGN="right">ServedClientCount</TD><TD ALIGN="center">int</TD><TD>The number of clients that have been processed by the HtmlAdaptorServer since its creation.</TD></TR>
     * <TR><TD ALIGN="right">State</TD><TD ALIGN="center">int</TD><TD>State of the HtmlAdaptorServer.</TD></TR>
     * <TR><TD ALIGN="right">StateString</TD><TD ALIGN="center">java.lang.String</TD><TD>String representation of the HtmlAdaptorServer state.</TD></TR>
     * </TABLE>
     * <P>
     *
     * @param attribute The name of the attribute to be retrieved
     *
     * @return The value of the specified attribute
     *
     * @exception AttributeNotFoundException The specified attribute does not exist or cannot be retrieved.
     * @exception MBeanException  Wraps an exception thrown by the MBean's getter.
     * @exception ReflectionException  Wraps an java.lang.Exception thrown while trying to invoke the getter. 
     */
    public java.lang.Object getAttribute(java.lang.String attribute)
        throws AttributeNotFoundException,
               MBeanException,
               ReflectionException {
        
        // Validate the attribute.
        //
        if (attribute == null || attribute.trim().equals("")) {
            throw new RuntimeOperationsException
		(new IllegalArgumentException("Attribute name cannot be null or empty"), 
		 "The getAttribute method of HtmlAdaptorServer was called with a null or empty attribute name string.");
        }
        
        // Find and return the value of the specified attribute.
        //
        if (attribute.equals("Active"))
            return new Boolean(isActive());
        if (attribute.equals("ActiveClientCount"))
            return new Integer(getActiveClientCount());
        if (attribute.equals("AuthenticationOn"))
            return new Boolean(isAuthenticationOn());
        if (attribute.equals("Host"))
            return getHost();
        if (attribute.equals("LastConnectedClient"))
            return getLastConnectedClient();
        if (attribute.equals("MaxActiveClientCount"))
            return new Integer(getMaxActiveClientCount());
        if (attribute.equals("Parser"))
            return getParser();
        if (attribute.equals("Port"))
            return new Integer(getPort());
        if (attribute.equals("Protocol"))
            return getProtocol();
        if (attribute.equals("ServedClientCount"))
            return new Integer(getServedClientCount());
        if (attribute.equals("State"))
            return new Integer(getState());
        if (attribute.equals("StateString"))
            return getStateString();

        throw new AttributeNotFoundException(attribute+" is unknown in HtmlAdaptorServer");
    }

    /**
     * Gets the values of several attributes of the HTML server.
     * <P>
     * For a list of possible attributes look at the method {@link com.sun.jdmk.comm.HtmlAdaptorServer#getAttribute getAttribute}.
     * 
     * @param attributes Enables the values of several attributes of the Dynamic MBean.
     *
     * @return  The list of retrieved attributes.
     */
    public AttributeList getAttributes(java.lang.String[] attributes) {
        AttributeList result = new AttributeList();
        String attribute = null;
        
        // Validate the list of get attributes.
        //
        if (attributes == null) {
            throw new RuntimeOperationsException(new IllegalArgumentException("Attributes cannot be null"), "Exception occurred trying to invoke the getter on the HtmlAdaptorServer");
        }
        
        // If there is no attributes to get, return an empty attribute list.
        //
        if (attributes.length == 0) {
            return result;
        }
        
        // Get the values of the specified attributes and populate the result list.
        //
        for (int i =0 ; i<attributes.length ; i++) {
            attribute = attributes[i];
            String elmt = (String)attributes[i];
            try {        
                Object value = getAttribute(elmt);     
                result.add(new Attribute(elmt, value));
            }  catch (Exception e) {
                if (logger.finestOn()) {
                    logger.finest("getAttributes","Unexpected exception [Exception="+e+"]");
                }
            }
        }
        return result;
    }

    /**
     * Invokes a specific operation on the HTML protocol adaptor.
     *
     * <P>
     * Invokes the following operations of the HTML protocol adaptor:
     * <P>
     * <TABLE>
     * <TR><TH>Name</TH><TH>Signature</TH><TH>Description</TH></TR>
     * <TR><TD ALIGN="right">resetParser</TD><TD ALIGN="center">()</TD><TD>Remove the customization from HtmlAdaptorServer by resetting the Parser property to null.</TD></TR>
     * <TR><TD ALIGN="right">createParser</TD><TD ALIGN="center">(java.lang.String, java.lang.String, java.lang.String) <TABLE><TR><TD ALIGN="right">class name</TD><TD ALIGN="center">java.lang.String</TD><TD>The Java class of HTML parser MBean.</TD></TR><TR><TD ALIGN="right">object name</TD><TD ALIGN="center">java.lang.String</TD><TD>The string used as an object name to register the parser into the MBeanServer.</TD></TR><TR><TD ALIGN="right">class loader name</TD><TD ALIGN="center">java.lang.String</TD><TD>The ObjectName of the MBean to use as class loader, if it is empty, the default agent class loader will be used by the MBeanServer.</TD></TR></TABLE></TD><TD>Create and Register the HTML parser MBean in the MBeanServer and set the Parser attribute with the parser.</TD></TR>
     * <TR><TD ALIGN="right">waitState</TD><TD ALIGN="center">(int, long) <TABLE><TR><TD ALIGN="right">state</TD><TD ALIGN="center">int</TD><TD>The state to wait for.</TD></TR><TR><TD ALIGN="right">timeout</TD><TD ALIGN="center">long</TD><TD>The maximum time to wait in milliseconds.</TD></TR></TABLE></TD><TD>Waits to be notified of a specific state change in the HtmlAdaptorServer.</TD></TR>
     * <TR><TD ALIGN="right">stop</TD><TD ALIGN="center">()</TD><TD>Stop the HtmlAdaptorServer.</TD></TR>
     * <TR><TD ALIGN="right">start</TD><TD ALIGN="center">()</TD><TD>Start the HtmlAdaptorServer.</TD></TR>
     * </TABLE>
     * <P>
     *
     * @param actionName The name of the action to be invoked.
     * @param params An array containing the parameters to be set when the action is
     * invoked.
     * @param signature An array containing the signature of the action. The class objects will
     * be loaded through the same class loader as the one used for loading the
     * HTML protocol adaptor.
     *
     * @return  The result returned by the specific action.
     *
     * @exception MBeanException  Wraps an exception thrown by the MBean's invoked method.
     * @exception ReflectionException  Wraps an java.lang.Exception thrown while trying to invoke the method
     */
    public Object invoke(String actionName, Object[] params,  String[] signature) 
        throws MBeanException, ReflectionException {
        
        if (actionName == null || actionName.trim().equals("")) {
		    throw new RuntimeOperationsException
			(new IllegalArgumentException("String parameter 'actionName' of invoke method of HtmlAdaptorServer"+
						      " cannot be null or empty"), 
			 "String parameter 'actionName' of invoke method of HtmlAdaptorServer cannot be null or empty");
	} 

        /* First, handle operations 
	 * (ie all methods defined as operations in the MBeanInfo object,
	 * and which therefore are not attribute getters or setters) 
	 */
        
	// public void start()
        if (actionName.equals("start")) {
            start();
            return null;
        } 
	// public void stop()
	else if (actionName.equals("stop")) {
            stop();
            return null;
        }
	// public boolean waitState(int state, long timeOut)
	else if (actionName.equals("waitState")) {
            try {
		if ( params.length != 2 ||
		     ! (params[0] instanceof Integer) ||
		     ! (params[1] instanceof Long) ) {
		    throw new RuntimeOperationsException
			(new IllegalArgumentException("invoke waitState: "+
						      "expecting params[0] instanceof Integer and params[1] instanceof Long"),
			 "Wrong content for array Object[] params to invoke waitState method of HtmlAdaptorServer");
		}
		if ( signature.length != 2 ||
		     ! signature[0].equals("int") ||
		     ! signature[1].equals("long") ) {
		    throw new RuntimeOperationsException
			(new IllegalArgumentException("invoke waitState: "+
						      "expecting signature[0].equals(\"int\") and signature[1].equals(\"long\")"),
			 "Wrong content for array String[] signature to invoke waitState method of HtmlAdaptorServer");
		}
                return new Boolean(waitState(((Integer)params[0]).intValue(), ((Long)params[1]).longValue()));
            } catch (Exception e) {
                throw new MBeanException(e, "invoke waitState: "+ e.getClass().getName() +
					 "caught ["+ e.getMessage() +"]");
            }
        }
	// public void createParser(java.lang.String className, java.lang.String parserName, java.lang.String loaderName)
	else if (actionName.equals("createParser")) {
            try {
		if ( params.length != 3 ||
		     ! (params[0] instanceof String) ||
		     ! (params[1] instanceof String) ||
		     ! (params[2] instanceof String) ) {
		    throw new RuntimeOperationsException
			(new IllegalArgumentException("invoke createParser: "+
						      "expecting params[0] instanceof String and "+
						      "params[1] instanceof String and params[2] instanceof String"),
			 "Wrong content for array Object[] params to invoke createParser method of HtmlAdaptorServer");
		}
		if ( signature.length != 3 ||
		     ! signature[0].equals("String") ||
		     ! signature[1].equals("String") ||
		     ! signature[2].equals("String") ) {
		    throw new RuntimeOperationsException
			(new IllegalArgumentException("invoke createParser: "+
						      "expecting signature[0].equals(\"String\") and "+
						      "signature[1].equals(\"String\") and signature[2].equals(\"String\")"),
			 "Wrong content for array String[] signature to invoke createParser method of HtmlAdaptorServer");
		}
                createParser((String)params[0], (String)params[1], (String)params[2]);
            } catch (Exception e) {
                throw new MBeanException(e, "invoke createParser: "+ e.getClass().getName() +
					 "caught ["+ e.getMessage() +"]");
            }
            return null;
        }
	// public void resetParser()
	else if (actionName.equals("resetParser")) {
            resetParser();
            return null;
        }

	/* Second, handle getters: this is to fix 'bug' (rather an RFE really) 4337393 
	 * MUST BE DONE AFTER OPERATIONS, in case an operation starts with "get" 
	 */
	// getToto getters
	else if (actionName.startsWith("get") && actionName.length()>3) {
	    String attrName = actionName.substring(3);
	    try {
		return this.getAttribute(attrName);
            } catch (AttributeNotFoundException e) {
                throw new ReflectionException(new NoSuchMethodException(actionName), 
					      "The action with name " + actionName + " could not be found in HtmlAdaptorServer");
	    }
	}
	// isToto getters
	else if (actionName.startsWith("is") && actionName.length()>2) {
	    String attrName = actionName.substring(2);
	    try {
		return this.getAttribute(attrName);
            } catch (AttributeNotFoundException e) {
                throw new ReflectionException(new NoSuchMethodException(actionName), 
					      "The action with name " + actionName + " could not be found in HtmlAdaptorServer");
	    }
	}

	/* Third, handle setters: this is still to fix 'bug' 4337393
	 * MUST BE DONE AFTER OPERATIONS, in case an operation starts with "set" 
	 */
	else if (actionName.startsWith("set") && actionName.length()>3) {
	    String attrName = actionName.substring(3);
	    if ( params.length != 1 ) {
		throw new RuntimeOperationsException
		    (new IllegalArgumentException("invoke "+ actionName +": expecting params.length == 1"),
		     "Array Object[] params to invoke createParser method of HtmlAdaptorServer should be of length 1");
	    }
	    try {
	        this.setAttribute(new Attribute(attrName, params[0]));
            } catch (AttributeNotFoundException e) {
                throw new ReflectionException(new NoSuchMethodException(actionName), 
					      "The action with name " + actionName + " could not be found in HtmlAdaptorServer");
	    } catch (InvalidAttributeValueException e) {
		throw new MBeanException(e, "InvalidAttributeValueException thrown when invoking setAttribute for attribute with "+
					 "[name="+ attrName +"] and [value="+ params[0] +"]");
	    }
	    return null;
	}

	// not found 
	else { 
            throw new ReflectionException(new NoSuchMethodException(actionName), 
					  "The action with name " + actionName + " could not be found in HtmlAdaptorServer");
        }
    }

    /**
     * Sets the value of a specific attribute of the HTML protocol adaptor.
     * <P>
     * Sets the following attributes of the HTML protocol adaptor:
     * <P>
     * <TABLE>
     * <TR><TH>Name</TH><TH>Type</TH><TH>Description</TH></TR>
     * <TR><TD ALIGN="right">Port</TD><TD ALIGN="center">int</TD><TD>Port number used. (NOTE: This can only be changed when the adaptor is OFFLINE)</TD></TR>
     * <TR><TD ALIGN="right">MaxActiveClientCount</TD><TD ALIGN="center">int</TD><TD>The maximum number of clients the HtmlAdaptorServer can process concurrently. (NOTE: This can only be changed when the adaptor is OFFLINE)</TD></TR>
     * <TR><TD ALIGN="right">Parser</TD><TD ALIGN="center">{@link javax.management.ObjectName ObjectName}</TD><TD>ObjectName of the HTML parser MBean used to customize the HTML pages generated by the HtmlAdaptorServer.</TD></TR>
     * </TABLE>
     * <P>
     * @param attribute The identification of the attribute to
     * be set and the value it is to be set to.
     *
     * @exception AttributeNotFoundException The specified attribute does not exist or cannot be retrieved.
     * @exception InvalidAttributeValueException The specified value is not a valid value for the attribute.
     * @exception MBeanException Wraps an exception thrown by the MBean's setter.
     * @exception ReflectionException Wraps an exception thrown while trying to invoke the MBean's setter.
     */
    public void setAttribute(Attribute attribute) 
        throws AttributeNotFoundException,
               InvalidAttributeValueException,
               MBeanException,
               ReflectionException {

        // Validate the attribute.
        //
        if (attribute == null) {
            throw new RuntimeOperationsException(new IllegalArgumentException("setAttribute: attribute parameter cannot be null"), 
						 "Cannot invoke setAttribute method of " + dclassName + " with null attribute parameter");
        }
        String name = attribute.getName();
        Object value = attribute.getValue();
        if (name == null || name.trim().equals("") ) {
            throw new RuntimeOperationsException(new IllegalArgumentException("setAttribute: name field of attribute parameter "+
									      "cannot be null or empty"), 
						 "Cannot invoke setAttribute method of " + dclassName + 
						 " with null or empty attribute parameter name field");
        }


        /* Find and set the specified attribute.
	 */

	// Port
        if (name.equals("Port")) {
            try {
                if (java.lang.Integer.class.isAssignableFrom
		   ((value!=null)?value.getClass():java.lang.Integer.class)) {
                    setPort(((Integer)attribute.getValue()).intValue());
                } else {
                    throw(new InvalidAttributeValueException
			  ("Cannot set attribute "+ name +" to a " + 
			   ((value!=null) ? value.getClass().getName() : value) + " object, java.lang.Integer expected"));
                 }
	    } catch (Exception e) {
                if (logger.finestOn()) {
                    logger.finest("setAttribute","setAttribute Port: caught [Exception="+e+"]");
                }
                throw new MBeanException(e, "setAttribute Port: "+ e.getClass().getName() +" caught ["+ e.getMessage() +"]");
            }
        }
	// MaxActiveClientCount
	else if (name.equals("MaxActiveClientCount")) {
            try {
                if (java.lang.Integer.class.isAssignableFrom
		   ((value!=null)?value.getClass():java.lang.Integer.class)) {
                    setMaxActiveClientCount(((Integer)attribute.getValue()).intValue());
                } else {
                    throw(new InvalidAttributeValueException
			  ("Cannot set attribute "+ name +" to a " + 
			   ((value!=null) ? value.getClass().getName() : value) + " object, java.lang.Integer expected"));
                }
	    } catch (Exception e) {
                if (logger.finestOn()) {
                    logger.finest("setAttribute","setAttribute MaxActiveClientCount: caught [Exception="+e+"]");
                }
                throw new MBeanException(e, "setAttribute MaxActiveClientCount: "+ e.getClass().getName() +" caught ["+ e.getMessage() +"]");
            }
        }
	// Parser
	else if (name.equals("Parser")) {
            try {
                if (javax.management.ObjectName.class.isAssignableFrom
		    ((value!=null)?value.getClass():
		     javax.management.ObjectName.class)) {
                    setParser((ObjectName)attribute.getValue());
                } else {
                    throw(new InvalidAttributeValueException
			  ("Cannot set attribute "+ name +" to a " + 
			   ((value!=null) ? value.getClass().getName() : value) + " object, javax.management.ObjectName expected"));
                }
	    } catch (Exception e) {
                if (logger.finestOn()) {
                    logger.finest("setAttribute","setAttribute Parser: caught [Exception="+e+"]");
                }
                throw new MBeanException(e, "setAttribute Parser: "+ e.getClass().getName() +" caught ["+ e.getMessage() +"]");
            }
        } else {
            throw new AttributeNotFoundException("Attribute "+ name +" is unknown in HtmlAdaptorServer, or is not writable");
        }
    }

    /**
     * Sets the values of several attributes of the HTML protocol adaptor.
     * <P>
     * For a list of possible attributes look at the method {@link com.sun.jdmk.comm.HtmlAdaptorServer#setAttribute setAttribute}.
     *
     * @param attributes A list of attributes: The identification of the
     * attributes to be set and  the values they are to be set to.
     *
     * @return  The list of attributes that were set, with their new values.
     */
    public AttributeList setAttributes(AttributeList attributes) {
        // Validate the list of attributes.
        //
        if (attributes == null) {
            throw new RuntimeOperationsException(new IllegalArgumentException("AttributeList  cannot be null"), "Exception occurred trying to invoke the setter on the MBean");
        } 
        
        // If there is no attributes to set, return an empty attribute list.
        //
        AttributeList result = new AttributeList();
        if (attributes.isEmpty()) {
            return result;
        }
        
        // Set the attributes and populate the result list.
        //
        for (Iterator i = attributes.iterator(); i.hasNext();) {
            Attribute attr = (Attribute) i.next();
            String id          = attr.getName();
            Object value       = attr.getValue();          
            try{
                Object newValue = null; 
                setAttribute(attr);
                result.add(new Attribute(id, getAttribute(id)));
            } catch(Exception e) {
                if (logger.finestOn()) {
                    logger.finest("setAttributes","Exception when setting "+id+". [Exception="+e+"]");
                }
            }
        }
        return result;
    }

    // --------------------------------------------------------
    // PROTECTED METHODS
    // --------------------------------------------------------

    /**
     */
    protected void doError(Exception e) throws CommunicationException {
	return;
    }

    /**
     * Binds the HTML protocol adaptor.
     * <P>
     * Create a socket listener.
     *
     * @exception CommunicationException Communication problem while creating listener socket.
     * @exception InterruptedException Creating listener socket was interrupted.
     */
    protected void doBind() 
        throws CommunicationException, InterruptedException {

        if (logger.finerOn()) {
            logger.finer("doBind","Bind the socket listener to [Port="+port+", MaxActiveClientCount="+maxActiveClientCount+"]");
        }

        try {
            sockListen = new ServerSocket(port, 2 * maxActiveClientCount);
            if (logger.finerOn()) {
                logger.finer("doBind","Bound to [Address="+sockListen.getInetAddress()+", Port="+sockListen.getLocalPort()+"]");
            }
        } catch (SocketException e) {
            if (e.getMessage().equals(InterruptSysCallMsg))
                throw new InterruptedException(e.toString()) ;
            else
                throw new CommunicationException(e) ;
        } catch (InterruptedIOException e) {
            throw new InterruptedException(e.toString()) ;
        } catch (IOException e) {
            throw new CommunicationException(e) ;
        }
    }
    
    /**
     * Unbinds the HTML protocol adaptor.
     * <P>
     * Close the socket listener.
     *
     * @exception CommunicationException Communication problem while closing listener socket.
     * @exception InterruptedException Closing listener socket was interrupted.
     */
    protected void doUnbind() 
        throws CommunicationException, InterruptedException {

        if (logger.finerOn()) {
            logger.finer("doUnbind","Finally close the socket [Listener="+sockListen+"]");
        }
        try {
            sockListen.close();
        } catch (SocketException e) {
            if (e.getMessage().equals(InterruptSysCallMsg))
                throw new InterruptedException(e.toString()) ;
            else
                throw new CommunicationException(e) ;
        } catch (InterruptedIOException e) {
            throw new InterruptedException(e.toString()) ;
        } catch (IOException e) {
            throw new CommunicationException(e) ;
        }
    }

    /**
     * Collects incoming requests.
     *
     * @exception CommunicationException Communication problem while receiving request.
     * @exception InterruptedException Receiving request was interrupted.
     */
    protected void doReceive()
        throws CommunicationException, InterruptedException {

        if (logger.finerOn()) {
            logger.finer("doReceive","Listens for a connection on [Listener="+sockListen+"]");
        }
        try {
            sock = sockListen.accept();
            if (logger.finerOn()) {
                logger.finer("doReceive","Accepted a connection on [Socket="+sock+"]");
            }
        } catch (SocketException e) {
            if (e.getMessage().equals(InterruptSysCallMsg))
                throw new InterruptedException(e.toString()) ;
            else
                throw new CommunicationException(e) ;
        } catch (InterruptedIOException e) {
            throw new InterruptedException(e.toString()) ;
        } catch (IOException e) {
            throw new CommunicationException(e) ;
        }
    }
    
    /**
     * Handles incoming requests.
     *
     * @exception CommunicationException Communication problem while processing request.
     * @exception InterruptedException Request processing was interrupted.
     */
    protected void doProcess()
        throws CommunicationException, InterruptedException {

        if (logger.finerOn()) {
            logger.finer("doProcess","Process a request received on [Socket="+sock+"]");
        }
        addrLastClient = sock.getInetAddress();
        HtmlRequestHandler server =
	    new HtmlRequestHandler(sock, this, topMBS, objectName,
				   servedClientCount);
        sock = null ;
    }


    // --------------------------------------------------------
    // PACKAGE PRIVATE METHODS
    // --------------------------------------------------------

    /**
     * Checks whether the response sent by the client matches any of the login/password pairs stored in the server.
     * If the match is successful, the client has been authenticated and the method returns true, otherwise false.
     */
    synchronized boolean checkChallengeResponse(String response) {
        if (logger.finerOn()) {
            logger.finer("checkChallengeResponse"," Validate  request");
        }

        // Check we've got a response.
        //
        if (response == null) {
            return false;
        }
    
        // Extract the username and the password from response.
        //
        String username = null;
        String password = null;
        StringTokenizer parser = new StringTokenizer(response, ":");
        try {
            if (parser.hasMoreTokens()) {
                username = parser.nextToken();
                password = parser.nextToken();
            }
        } catch (NoSuchElementException nsee) { 
            if (logger.finestOn()) {
                logger.finest("checkChallengeResponse","No such element. [Exception="+nsee+"]");
            }
            return false;
        }

        if (logger.finerOn()) {
            logger.finer("checkChallengeResponse"," Validate the request for [Login="+username+", Password="+password+"]");
        }

        // Look for given username and password on the server store.
        //
        for (Enumeration e = authInfo.elements(); e.hasMoreElements(); ) {
            AuthInfo ai = (AuthInfo) e.nextElement();
            if (ai.getLogin().equals(username)) {
                if (ai.getPassword().equals(password)) {
                    return true;
                }
            }
        }
    
        return false;
    }


    // --------------------------------------------------------
    // PRIVATE METHODS
    // --------------------------------------------------------

    private void buildInfo() {

        dattributes[0]  = new MBeanAttributeInfo("Active","boolean","Active: True if the HtmlAdaptorServer is in the ONLINE state.",true,false,true);
        dattributes[1]  = new MBeanAttributeInfo("ActiveClientCount","int","ActiveClientCount: The number of clients being processed currently by the HtmlAdaptorServer.",true,false,false);
        dattributes[2]  = new MBeanAttributeInfo("AuthenticationOn","boolean","AuthenticationOn: True if the HtmlAdaptorServer requests authentication.",true,false,true);
        dattributes[3]  = new MBeanAttributeInfo("Host","java.lang.String","Host: Hostname.",true,false,false);
        dattributes[4]  = new MBeanAttributeInfo("LastConnectedClient","java.lang.String","LastConnectedClient: The IP address of the last connected client.",true,false,false);
        dattributes[5]  = new MBeanAttributeInfo("MaxActiveClientCount","int","MaxActiveClientCount: The maximum number of clients the HtmlAdaptorServer can process concurrently.",true,true,false);
        dattributes[6]  = new MBeanAttributeInfo("Parser","javax.management.ObjectName","Parser: ObjectName of the MBean used to customized HTML pages generated by the HtmlAdaptorServer.",true,true,false);
        dattributes[7]  = new MBeanAttributeInfo("Port","int","Port: Port number used.",true,true,false);
        dattributes[8]  = new MBeanAttributeInfo("Protocol","java.lang.String","Protocol: html.",true,false,false);
        dattributes[9]  = new MBeanAttributeInfo("ServedClientCount","int","ServedClientCount: The number of clients that have been processed by the HtmlAdaptorServer since its creation.",true,false,false);
        dattributes[10] = new MBeanAttributeInfo("State","int","State: State of the HtmlAdaptorServer.",true,false,false);
        dattributes[11] = new MBeanAttributeInfo("StateString","java.lang.String","StateString: State of the HtmlAdaptorServer.",true,false,false);

        Constructor[] ctor = this.getClass().getConstructors();
	for (int i = 0; i < ctor.length; i++) {
	    if (ctor[i].getParameterTypes().length == 0) {
		dconstructors[0] = new MBeanConstructorInfo("Instantiate HtmlAdaptorServer with default port number equal to " + com.sun.jdmk.ServiceName.HTML_ADAPTOR_PORT + ".",ctor[i]);
	    } else if (ctor[i].getParameterTypes().length == 1) {
		dconstructors[1] = new MBeanConstructorInfo("Instantiate HtmlAdaptorServer with the specified port number.",ctor[i]);
	    } else {
		dconstructors[2] = new MBeanConstructorInfo("Instantiate HtmlAdaptorServer with the specified port number and user authentication information list.",ctor[i]);
	    }
	}

	// public void start()
        doperations[0] = new MBeanOperationInfo("start","start: Start the HtmlAdaptorServer.",null,"void",MBeanOperationInfo.ACTION);

	// public void stop()
        doperations[1] = new MBeanOperationInfo("stop","stop: Stop the HtmlAdaptorServer.",null,"void",MBeanOperationInfo.ACTION);

	// public boolean waitState(int state, long timeOut)
        MBeanParameterInfo[] params1 = new MBeanParameterInfo[2];
        params1[0] = new MBeanParameterInfo("state","int","state: The state to wait for.");
        params1[1] = new MBeanParameterInfo("timeout","long","timeout: The maximum time to wait in milliseconds.");
        doperations[2] = new MBeanOperationInfo("waitState","waitState: Waits to be notified of a specific state change in the HtmlAdaptorServer.",params1,"boolean",MBeanOperationInfo.ACTION);

	// public void createParser(java.lang.String className, java.lang.String parserName, java.lang.String loaderName)
        MBeanParameterInfo[] params2 = new MBeanParameterInfo[3];
        params2[0] = new MBeanParameterInfo("className","java.lang.String","class name: The name of the class used to parse HTML page.");
        params2[1] = new MBeanParameterInfo("objectName","java.lang.String","object name: The name of the MBean to register in the MBeanServer.");
        params2[2] = new MBeanParameterInfo("classLoaderName","java.lang.String","class loader name: The ObjectName of the MBean to use as class loader, if it is empty, the default agent class loader will be used by the MBeanServer.");
        doperations[3] = new MBeanOperationInfo("createParser","createParser: Create, Register to the MBeanServer and set Parser attribute with the HTML parser MBean",params2,"void",MBeanOperationInfo.ACTION);

	// public void resetParser()
        doperations[4] = new MBeanOperationInfo("resetParser","resetParser: Remove the customization from HtmlAdaptorServer by reseting the Parser property to null.",null,"void",MBeanOperationInfo.ACTION);


	// At last, here is the MBeanInfo object
	//
        dmbeaninfo = new MBeanInfo(dclassName,
                                   ddescription,
                                   dattributes,
                                   dconstructors,
                                   doperations,
                                   new MBeanNotificationInfo[0]);
    }


    /**
     * Returns the string used in debug traces.
     */
    String makeDebugTag() {
        return "HtmlAdaptorServer["+ getProtocol() + ":" + getPort() + "]" ;
    }

    // --------------------------------------------------------
    // PRIVATE VARIABLES
    // --------------------------------------------------------

    /** @serial The Java class name of the HTML protocol adaptor. */
    private String dclassName = this.getClass().getName();

    /** @serial The description of the HTML protocol adaptor. */
    private String ddescription = "HtmlAdaptorServer class: Provides a management interface of an agent to Web browser clients.";

    /** @serial The list of exposed attributes for the HTML protocol adaptor. */
    private MBeanAttributeInfo[] dattributes = new MBeanAttributeInfo[12];

    /** @serial The list of public constructors for the HTML protocol adaptor. */
    private MBeanConstructorInfo[] dconstructors = new MBeanConstructorInfo[3];

    /** @serial The list of operations for the HTML protocol adaptor. */
    private MBeanOperationInfo[] doperations = new MBeanOperationInfo[5];

    /** @serial The management interface exposed by the HTML protocol adaptor. */
    private MBeanInfo dmbeaninfo = null;

    /** @serial The IP address of the last connected client. */
    private InetAddress addrLastClient = null;

    /** @serial The list of authentication information for the HTML protocol adaptor. */
    private Vector authInfo = new Vector();

    /** @serial The object name of the HTML parser MBean used by the HTML protocol adaptor. */
    private ObjectName userParser = null;

    private transient ServerSocket sockListen = null;
    private transient Socket sock = null;
    private static final String InterruptSysCallMsg = "Interrupted system call";
}
