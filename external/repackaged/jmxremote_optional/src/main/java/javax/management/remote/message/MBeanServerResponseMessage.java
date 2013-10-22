/*
 * @(#)file      MBeanServerResponseMessage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.12
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

import javax.management.MBeanServerConnection;
import javax.management.remote.generic.ObjectWrapping;

/**
 * <p>The result of an {@link MBeanServerConnection} method call,
 * encoded as an object.  Objects of this type are sent from the
 * server end to the client end of a JMX API connection in response to a
 * previous {@link MBeanServerRequestMessage}.</p>
 *
 * <p>Instances of this class are immutable.</p>
 *
 * <p>The type of the returned value for a successful method call is
 * the type of the <code>MBeanServerConnection</code> method that was
 * called, except where otherwise specified in
 * <code>MBeanServerRequestMessage</code>.  The value is wrapped using
 * the {@link ObjectWrapping} for the connection using this message.
 */
public class MBeanServerResponseMessage implements Message {

    private static final long serialVersionUID = 7964312628478315537L;

    /**
     * <p>Constructs a message representing the response to a previous
     * {@link MBeanServerRequestMessage}.</p>
     *
     * @param id the identifier of this message.  This must be the
     * same as the identifier of the {@link
     * MBeanServerRequestMessage}.
     *
     * @param wrappedResult the result of the
     * <code>MBeanServerConnection</code> method call, wrapped using
     * the {@link ObjectWrapping} for this connection.  If the method
     * returned normally, the wrapped object is the value returned, or
     * null if the method is <code>void</code>.  If the method
     * produced an exception, the wrapped object is the exception.
     *
     * @param isException true if the
     * <code>MBeanServerConnection</code> method call produced an
     * exception.  The <code>wrappedResponse</code> parameter then
     * contains the exception.  The <code>isException</code> parameter
     * serves to distinguish the case where a method generates an
     * exception from the case where the method returns an object of
     * type <code>Exception</code>.
     */
    public MBeanServerResponseMessage(long id, Object wrappedResult,
				      boolean isException) {
	this.id = id;
	this.wrappedResult = wrappedResult;
	this.isException = isException;
    }

    /**
     * <p>Returns the wrapped result of the method invocation.</p>
     *
     * @return the wrapped result of a successful method invocation,
     * or the wrapped exception of an unsuccessful one.
     */
    public Object getWrappedResult() {
	return wrappedResult;
    }

    /**
     * <p>Returns the unique identifier of this message.  This is the
     * same as the identifier of the corresponding {@link
     * MBeanServerRequestMessage}.</p>
     *
     * @return the unique identifier of this message.
     */
    public long getMessageId() {
	return id;
    }

    /**
     * <p>Indicates whether this message corresponds to an
     * exception.</p>
     *
     * @return true if this message represents an exception generated
     * by the {@link MBeanServerConnection} method that was called.
     */
    public boolean isException() {
	return isException;
    }

    /**
     * @serial The unique message identifier.
     * @see #getMessageId()
     **/
    private final long id;
    
    /**
     * @serial The wrapped result of the
     * <code>MBeanServerConnection</code> method call.  If the method
     * returned normally, this is the value returned, or null if the
     * method is <code>void</code>.  If the method produced an
     * exception, this is the exception.
     * @see #MBeanServerResponseMessage(long,Object,boolean)
     **/
    private final Object wrappedResult;

    /**
     * @serial True if this message represents an exception.
     * @see #isException()
     **/
    private final boolean isException;
}
