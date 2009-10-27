/*
 * @(#)file      AuthInfo.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.17
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



// java import
//
import java.io.*;

/**
 * <p>Defines the Authentication Information required by
 * the adaptors for carrying out login/password authentication.</p>
 *
 * <p>This class contains a cleartext password, so it should be used
 * with care.  In particular, it is not Serializable.  If you subclass
 * it to make it Serializable, be aware that this could result in
 * cleartext passwords being saved to files or sent over the
 * network.</p>
 *
 */

public class AuthInfo implements Cloneable {

    // CONSTRUCTORS
    //-------------

    /**
     * Creates an authentication info object with a null login/password.
     */
    public AuthInfo() {
    }

    /**
     * Creates an authentication info object with a given login/password.
     */
    public AuthInfo(String login, String password) {
	this.login = login;
	this.password = password;
    }

    // GETTERS/SETTERS
    //----------------
  
    /**
     * Returns the value of the login property.
     *
     * @return the value of the login property.
     */
    public String getLogin() {
	return login;
    }

    /**
     * Sets the value of the login property.
     *
     * @param login The value to which the login property will be set.
     */
    public void setLogin(String login) {
	this.login = login;
    }

    /**
     * Returns the value of the password property.
     *
     * @return the value of the password property.
     */
    public String getPassword() {
	return password;
    }

    /**
     * Sets the value of the password property.
     *
     * @param password The value to which the password property will be set.
     */
    public void setPassword(String password) {
	this.password = password;
    }

    // CLONEABLE INTERFACE
    //--------------------

    /**
     * Clone this <CODE>AuthInfo</CODE>.
     * The cloning is done using Object.clone.
     *
     * @see java.lang.Object#clone
     */
    public Object clone() {
	Object newAuthInfo = null;
	try {
	    newAuthInfo = super.clone();
	} catch(CloneNotSupportedException e) {
	}
	return newAuthInfo;
    }

    // PRIVATE VARIABLES
    //------------------

    /**
     * @serial The login value
     */
    private String login = null;

    /**
     * @serial The password value
     */
    private String password = null;
}
