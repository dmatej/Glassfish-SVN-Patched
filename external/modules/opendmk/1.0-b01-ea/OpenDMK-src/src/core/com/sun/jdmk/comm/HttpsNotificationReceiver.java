/*
 * @(#)file      HttpsNotificationReceiver.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.10
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

/**
 * This class implements the notification receiver of the HTTP/SSL connector.
 * This class inherits most of its behavior. It performs only TCP-specific tasks:
 * <UL>
 * <LI>it creates, reads, writes and closes the SSL sockets.</LI>
 * </UL>
 *
 */

class HttpsNotificationReceiver extends GenericHttpNotificationReceiver {
  
    /**
     * Constructs an <CODE>HttpsNotificationReceiver</CODE>.
     */
    public HttpsNotificationReceiver(GenericHttpConnectorClient connector,
				     ClientNotificationDispatcher dispatcher,
				     boolean needClientAuth) {

        super(CommunicatorServer.HTTPS_TYPE, 0, connector, dispatcher);
	this.needClientAuth = needClientAuth;
    }

    /**
     * Creates a socket to be used for a HTTPS-based adaptor using TCP.
     * <P>
     * For internal use only.
     *
     * @return A HTTP/SSL adopter socket
     *
     */
    GenericHttpSocket createSocket() {
	HttpsSocket https_socket = new HttpsSocket(0, null, needClientAuth);
	https_socket.setTimeout(getTimeout());
        return https_socket;
    }

    /**
     * Returns the name of the protocol used.
     *
     * @return The string "https".
     */
    public String getProtocol() {
        return "https";
    }

    /**
     * Returns setting for Timeout.
     * <P>
     * 0 returns implies that the option is disabled
     * (i.e. timeout of infinity).
     * <P>
     * The default value for timeout is 60000 milliseconds.
     *
     * @return The current value of the "Timeout" property.
     */
    public int getTimeout() {
        if ( sockListen != null ) {
            return sockListen.getTimeout();
        } else {
            return timeout;
        }
    }

    /**  
     * Enables/disables Timeout with the specified timeout, in milliseconds.
     * <P>
     * With this option set to a non-zero timeout, a read() call on the 
     * InputStream associated with this Socket will block for only this 
     * amount of time. If the timeout expires, a {@link 
     * java.io.InterruptedIOException} is raised, though the Socket
     * is still valid. The option must be enabled prior to entering the 
     * blocking operation to have effect. The timeout must be > 0. 
     * A timeout of zero is interpreted as an infinite timeout.
     * <P>
     * The default value for timeout is 60000 milliseconds.
     *
     * @param value The new value of the property.
     *
     * @exception java.lang.IllegalStateException This method has been invoked
     * while the connector was ONLINE or STARTING.
     */
    public void setTimeout(int value) throws java.lang.IllegalStateException {
        if ((state == ONLINE) || (state == STARTING)) {
            throw new IllegalStateException(
                  "Stop server before carrying out this operation");
        }
	timeout = value;
        if ( sockListen != null ) {
            sockListen.setTimeout(value);
        }
    }

    // TRACE STUFF
    //------------

    /**
     * Returns the string used in debug traces.
     */
    protected String makeDebugTag() {
        return "HttpsNotificationReceiver[" + getProtocol() + ":" + getPort() + "]";
    }

    // PRIVATE VARIABLES
    //------------------

    /**
     * The timeout
     */
    private int timeout = 60000;

    /**
     * The SSL need client authentication flag
     */
    private boolean needClientAuth = true;
}
