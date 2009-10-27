/*
 * @(#)file      GenericHttpConnectorAddress.java
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



/**
 * This interface defines the Authentication Information required by
 * the connectors to carry out login/password authentication.
 *
 * @deprecated The JMX Remote API should be used in preference to the
 * legacy Java DMK connector classes.  This interface may be removed
 * in a future version of Java DMK.  See {@link
 * com.sun.jdmk.comm.JdmkLegacyConnector}.
 *
 */

public interface GenericHttpConnectorAddress extends ConnectorAddress, Cloneable {

    /**
     * Returns the value of the server host property.
     *
     * @return the value of the server host property.
     */
    public String getHost();

    /**
     * Sets the value of the server host property.
     *
     * @param host the new value of the server host property.
     */
    public void setHost(String host);

    /**
     * Returns the value of the server port property.
     *
     * @return the value of the server port property.
     */
    public int getPort();

    /**
     * Sets the value of the server port property.
     *
     * @param port the new value of the server port property.
     */
    public void setPort(int port);

    /**
     * Returns the value of the authentication info property.
     *
     * @return the value of the authentication info property.
     */
    public AuthInfo getAuthInfo();

    /**
     * Sets the value of the authentication info property.
     *
     * @param authInfo the new value of the authentication info property.
     */
    public void setAuthInfo(AuthInfo authInfo);

    /**
     * Clones this object.
     *
     * @see Object#clone
     */
    public Object clone();
}
