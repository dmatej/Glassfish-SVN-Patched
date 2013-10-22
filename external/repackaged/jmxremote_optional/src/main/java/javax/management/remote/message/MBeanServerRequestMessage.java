/*
 * @(#)file      MBeanServerRequestMessage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.20
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

package javax.management.remote.message;

import java.io.Serializable;
import java.util.Set;

import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.remote.generic.ObjectWrapping;
import javax.security.auth.Subject;

/**
 * <p>An {@link MBeanServerConnection} method call, encoded as an
 * object. Objects of this type are sent from the client end to the
 * server end of a JMX API connection. The result of the method is sent
 * as an {@link MBeanServerResponseMessage}.</p>
 *
 * <p>Instances of this class are immutable.</p>
 *
 * <p>The method to be called is specified by an integer constant;
 * these constants are defined in this class. Overloaded methods such
 * as <code>createMBean</code> define a different constant for each
 * overloaded version.</p>
 *
 * <p>The parameters to the method are provided as an <code>Object[]</code>
 * when the <code>MBeanServerRequestMessage</code> is constructed. Except
 * where specified, the number and type of these parameters are the same as
 * for the <code>MBeanServerConnection</code> method to be called. Similarly,
 * except where specified, the type of the value returned in the corresponding
 * <code>MBeanServerResponseMessage</code> is the return type of the
 * method to be called, or null if this type is <code>void</code>.
 *
 * <p>Because an MBean can use a class loader other than the default
 * one, some parameters need to be wrapped before being encoded in an
 * <code>MBeanServerRequestMessage</code>.  See {@link ObjectWrapping}
 * and the <em>JMX Remote API</em> specification.</p>
 *
 * <p>Subject delegation might be used by supplying the appropriate delegation
 * subject on each request. The delegation subject is the subject on which the
 * authorization checks are performed for this request. If <code>null</code> the
 * authorization checks are performed on the authentication subject instead.</p>
 */
public class MBeanServerRequestMessage implements Message {

    private static final long serialVersionUID = -4181036756525151109L;

    /**
     * <p>Identifier for the method {@link
     * MBeanServerConnection#addNotificationListener(ObjectName,
     * NotificationListener, NotificationFilter, Object)}.  This
     * message allows several listeners to be added at the same
     * time.</p>
     *
     * <p>The meaning of this message is : register for notifications
     * from the given MBeans that match the given filters.  The remote
     * client can subsequently retrieve the notifications using {@link
     * NotificationRequestMessage}.</p>
     *
     * <p>For each listener, the original
     * <code>NotificationListener</code> and <code>handback</code> are
     * kept on the client side; in order for the client to be able to
     * identify them, the server generates and returns a unique
     * <code>listenerID</code>.  This <code>listenerID</code> is
     * forwarded with the <code>Notifications</code> to the remote
     * client.</p>
     *
     * <p>The parameters contained in the
     * <code>MBeanServerRequestMessage</code> for this method are an
     * array of <code>ObjectName</code> and an array of
     * <code>Object</code>.  Both arrays have the same size.  Each
     * element of the array of <code>Object</code> contains null or a
     * <code>NotificationFilter</code> object wrapped using {@link
     * ObjectWrapping}.</p>
     *
     * <p>The corresponding {@link MBeanServerResponseMessage} will
     * contain an <code>Integer[]</code> that identifies the listeners
     * that were registered.  Subsequent notifications sent from
     * server to client will include this identifier to indicate which
     * listener is to receive the notification.</p>
     */
    public final static int ADD_NOTIFICATION_LISTENERS = 1;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#addNotificationListener(ObjectName,
     * ObjectName, NotificationFilter, Object)}.</p>
     *
     * <p>The parameters contained in the <code>MBeanServerRequestMessage</code>
     * for this method are the four parameters to the method. The
     * <code>NotificationFilter</code> and <code>Object</code> parameters are
     * wrapped using {@link ObjectWrapping}.</p>
     */
    public final static int ADD_NOTIFICATION_LISTENER_OBJECTNAME = 2;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#createMBean(String, ObjectName)}.</p>
     */
    public final static int CREATE_MBEAN = 3;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#createMBean(String, ObjectName, Object[],
     * String[])}.</p>
     *
     * <p>The <code>Object[]</code> parameter is wrapped using
     * {@link ObjectWrapping}.</p>
     */
    public final static int CREATE_MBEAN_PARAMS = 4;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#createMBean(String, ObjectName,
     * ObjectName)}.</p>
     */
    public final static int CREATE_MBEAN_LOADER = 5;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#createMBean(String, ObjectName, ObjectName,
     * Object[], String[])}.</p>
     *
     * <p>The <code>Object[]</code> parameter is wrapped using
     * {@link ObjectWrapping}.</p>
     */
    public final static int CREATE_MBEAN_LOADER_PARAMS = 6;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#getAttribute(ObjectName, String)}.</p>
     */
    public final static int GET_ATTRIBUTE = 7;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#getAttributes(ObjectName, String[])}.</p>
     */
    public final static int GET_ATTRIBUTES = 8;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#getDefaultDomain()}.</p>
     */
    public final static int GET_DEFAULT_DOMAIN = 9;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#getDomains()}.</p>
     */
    public final static int GET_DOMAINS = 10;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#getMBeanCount()}.</p>
     */
    public final static int GET_MBEAN_COUNT = 11;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#getMBeanInfo(ObjectName)}.</p>
     */
    public final static int GET_MBEAN_INFO = 12;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#getObjectInstance(ObjectName)}.</p>
     */
    public final static int GET_OBJECT_INSTANCE = 13;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#invoke(ObjectName, String, Object[],
     * String[])}.</p>
     *
     * <p>The <code>Object[]</code> parameter is wrapped using
     * {@link ObjectWrapping}.</p>
     */
    public final static int INVOKE = 14;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#isInstanceOf(ObjectName, String)}.</p>
     *
     * <p>A successful {@link MBeanServerResponseMessage} response will contain
     * a return value of type <code>Boolean</code>.</p>
     */
    public final static int IS_INSTANCE_OF = 15;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#isRegistered(ObjectName)}.</p>
     *
     * <p>A successful {@link MBeanServerResponseMessage} response will contain
     * a return value of type <code>Boolean</code>.</p>
     */
    public final static int IS_REGISTERED = 16;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#queryMBeans(ObjectName, QueryExp)}.</p>
     */
    public final static int QUERY_MBEANS = 17;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#queryNames(ObjectName, QueryExp)}.</p>
     */
    public final static int QUERY_NAMES = 18;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#removeNotificationListener(ObjectName,
     * NotificationListener)}.</p>
     *
     * <p>The parameters contained in the <code>MBeanServerRequestMessage</code>
     * for this method are the <code>ObjectName</code> and an
     * <code>Integer[]</code>.</p>
     *
     * <p>The <code>Integer[]</code> contains the identifiers that
     * the server returned for every {@link #ADD_NOTIFICATION_LISTENERS}
     * message on this connection that specified the given
     * <code>ObjectName</code> and <code>NotificationListener</code> and that
     * was not cancelled by a subsequent {@link #REMOVE_NOTIFICATION_LISTENER}
     * or {@link #REMOVE_NOTIFICATION_LISTENER_FILTER_HANDBACK} message.</p>
     */
    public final static int REMOVE_NOTIFICATION_LISTENER = 19;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#removeNotificationListener(ObjectName,
     * NotificationListener, NotificationFilter, Object)}.</p>
     *
     * <p>The parameters contained in the <code>MBeanServerRequestMessage</code>
     * for this method are the <code>ObjectName</code> and an
     * <code>Integer</code>.</p>
     *
     * <p>The <code>Integer</code> contains the identifier that the server
     * returned for the {@link #ADD_NOTIFICATION_LISTENERS} message on this
     * connection that specified the given <code>ObjectName</code>,
     * <code>NotificationListener</code>, <code>NotificationFilter</code>,
     * and <code>Object</code> handback.</p>
     */
    public final static int REMOVE_NOTIFICATION_LISTENER_FILTER_HANDBACK = 20;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#removeNotificationListener(ObjectName,
     * ObjectName)}.</p>
     *
     * <p>The parameters contained in the <code>MBeanServerRequestMessage</code>
     * for this method are the two <code>ObjectName</code> parameters.</p>
     */
    public final static int REMOVE_NOTIFICATION_LISTENER_OBJECTNAME = 21;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#removeNotificationListener(ObjectName,
     * ObjectName, NotificationFilter, Object)}.</p>
     *
     * <p>The parameters contained in the <code>MBeanServerRequestMessage</code>
     * for this method are the four parameters to the method. The
     * <code>NotificationFilter</code> and <code>Object</code> parameters are
     * wrapped using {@link ObjectWrapping}.</p>
     */
    public final static int REMOVE_NOTIFICATION_LISTENER_OBJECTNAME_FILTER_HANDBACK = 22;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#setAttribute(ObjectName, Attribute)}.</p>
     *
     * <p>The <code>Attribute</code> parameter is wrapped using
     * {@link ObjectWrapping}.</p>
     */
    public final static int SET_ATTRIBUTE = 23;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#setAttributes(ObjectName,
     * AttributeList)}.</p>
     *
     * <p>The <code>AttributeList</code> is wrapped using
     * {@link ObjectWrapping}.</p>
     */
    public final static int SET_ATTRIBUTES = 24;

    /**
     * <p>Identifier for the method
     * {@link MBeanServerConnection#unregisterMBean(ObjectName)}.</p>
     */
    public final static int UNREGISTER_MBEAN = 25;

    /**
     * <p>Constructs a message to invoke the method with the given
     * identifier and parameters. Each constructed object gets a
     * unique message ID, as returned by {@link #getMessageId()}.</p>
     *
     * @param methodId the identifier of an <code>MBeanServerConnection</code>
     * method. This should be one of the integer constants defined in this
     * class.  The behavior is not specified if it is not.
     *
     * @param params parameters to the method. The number and types
     * of the parameters depend on the method, as specified by the
     * definition of the corresponding integer constant in this class.
     *
     * @param delegationSubject the subject on which the authorization checks
     * are performed for this request. If <code>null</code> the authorization
     * checks are performed on the authentication subject instead.
     */
    public MBeanServerRequestMessage(int methodId,
				     Object[] params,
				     Subject delegationSubject) {
        messageId = newId();
        this.methodId = methodId;
        this.params = (params == null) ? NO_PARAMS : params;
	this.delegationSubject = delegationSubject;
    }

    /**
     * <p>Returns the method identifier of this message.</p>
     *
     * @return the <code>MBeanServerConnection</code> method that this
     * <code>MBeanServerRequestMessage</code> corresponds to. This
     * will be one of the integer constants defined in this class.
     */
    public int getMethodId() {
        return methodId;
    }

    /**
     * <p>Returns the method parameters.</p>
     *
     * @return the method parameters. The number and types of the
     * parameters depend on the method, as specified by the definition
     * of the corresponding integer constant in this class. The
     * returned array must not be modified by the caller.
     */
    public Object[] getParams() {
        return params;
    }

    /**
     * <p>Returns the delegation subject.</p>
     *
     * @return the delegation subject on which the authorization checks
     * are performed for this request.
     */
    public Subject getDelegationSubject() {
        return delegationSubject;
    }

    /**
     * <p>Returns this message's unique identifier. Every instance of
     * this class has a different identifier.</p>
     *
     * @return the unique identifier of this message.
     */
    public long getMessageId() {
        return messageId;
    }

    private static long newId() {
        synchronized(counterLock) {
            return count++;
        }
    }

    /**
     * @serial This message's unique identifier.
     * @see #getMessageId()
     */
    private final long messageId;

    /**
     * @serial The method identifier of this message.
     * @see #getMethodId()
     */
    private final int methodId;

    /**
     * @serial The method parameters.
     * @see #getParams()
     */
    private final Object[] params;

    /**
     * @serial The delegation subject.
     * @see #getDelegationSubject()
     */
    private final Subject delegationSubject;

    private static long count = 0;
    private static final int[] counterLock = new int[0];
    private static final Object[] NO_PARAMS = new Object[0];
}
