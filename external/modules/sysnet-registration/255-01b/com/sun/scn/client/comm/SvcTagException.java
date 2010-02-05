/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

package com.sun.scn.client.comm;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


public class SvcTagException extends Exception {
    public static final int GENERAL_ERROR = 0;
    public static final int AGENT_ALREADY_EXISTS = 1;
    public static final int SVCTAG_ALREADY_EXISTS = 2;
    public static final int INVALID_AGENT_REF = 3;
    public static final int INVALID_PRODUCT_REF = 4;
    public static final int XML_VALIDATION_FAILED = 5;
    public static final int RESOURCE_NOT_FOUND = 6;
    public static final int INVALID_EMAIL_FORMAT = 7;
    public static final int INVALID_COUNTRY = 8;
    public static final int USERNAME_ALREADY_EXISTS = 9;
    public static final int SCREENNAME_ALREADY_EXISTS = 10;
    public static final int PASSWORD_TOO_SHORT = 11;
    public static final int ERROR_CREATING_ACCOUNT = 12;
    public static final int INVALID_USERNAME_PASSWORD = 13;
    public static final int CONNECTION_ERROR = 14;
    public static final int HTTP_ERROR = 15;
    public static final int IO_ERROR = 16;
    public static final int UNKNOWN_HOST = 17;
    public static final int MALFORMED_URL = 18;
    private static final String[] MESSAGES = {
            "General Error", "Agent Already Exists", "SvcTag Already Exists",
            "Invalid Agent Reference", "Invalid Product Reference",
            "XML Validation Failed", "Resource Not Found", "Invalid Email Format",
            "Invalid Country", "Username Already Exists", "Screen Name Already Exists",
            "Password Too Short", "Error Creating Account",
            "Invalid Sun Online Account UserName/Pasword", 
            "Cannot connect to host", "Error HTTP response", "I/O Error",
            "Unknown Host", "Malformed URL"
        };

    public static final int ERROR_CODE_START = 13;
    private int code;
    private String msg;
    private int minorCode;
    private int majorCode;

    /**
     * Default contructor not available. Must supply code.
     */
    private SvcTagException() {
        super(MESSAGES[GENERAL_ERROR]);
        this.code = GENERAL_ERROR;
        this.msg = MESSAGES[code];
    }

    /**
     * Constructor.
     * @param code Error code.
     */
    public SvcTagException(int code) {
        super(MESSAGES[code]);
        this.code = code;
        this.msg = MESSAGES[code];
    }

    public SvcTagException(String message) {
        super(message);
    }

    /**
     * Constructor.
     * @param doc Error response document.
     */
    public SvcTagException(Document doc) {
        this(GENERAL_ERROR, doc);
    }

    /**
     * Constructor.
     * @param code Error code
     * @param doc Error response document.
     */
    public SvcTagException(int code, Document doc) {
        super(MESSAGES[code]);

        this.code = code;
        
        if (doc == null) {
            return;
        }

        try {
            // Format should be:
            /*
            <HTML>
             <HEAD>
              <TITLE>
               <response>
                <major-code>409 </major-code>
                <minor-code>1 </minor-code>
                <message>Attempt to create an Agent which already exists with URN: urn:st:9dfc7092-0142-cbf0-aac5-c1744bb1e4ef </message>
               </response>
              </TITLE>
             </HEAD>
             <BODY>
              <response>
               <major-code>409 </major-code>
               <minor-code>1 </minor-code>
               <message>Attempt to create an Agent which already exists with URN: urn:st:9dfc7092-0142-cbf0-aac5-c1744bb1e4ef </message>
              </response>
             </BODY>
            </HTML>
            */

            // pull off the major code
            NodeList list = doc.getElementsByTagName("major-code");

            if (list.getLength() > 0) {
                Node n = list.item(0);
                String s = n.getTextContent();

                if (s != null) {
                    try {
                        majorCode = Integer.parseInt(s.trim());
                    } catch (NumberFormatException nfe) {
                    }
                }
            }

            // pull off the minor code
            list = doc.getElementsByTagName("minor-code");

            if (list.getLength() > 0) {
                Node n = list.item(0);
                String s = n.getTextContent();

                if (s != null) {
                    try {
                        minorCode = Integer.parseInt(s.trim());
                    } catch (NumberFormatException nfe) {
                    }
                }
            }

            // pull off the message
            list = doc.getElementsByTagName("message");

            if (list.getLength() > 0) {
                Node n = list.item(0);
                msg = n.getTextContent();
            }

            // TODO: fix
        } catch (Exception e) {
            this.msg = MESSAGES[code];
        }
    }

    /**
     * Constructor.
     * @param code Error code.
     * @param message The error msg string for this exception.
     */
    public SvcTagException(int code, String message) {
        super(message);
        this.code = code;
        this.msg = message;
    }

    /**
     * Constructor.
     * @param code Error code.
     * @param message The error msg string for this exception.
     * @param cause The exception that caused this exception.
     */
    public SvcTagException(int code, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
        this.msg = message;
    }

    /**
     * @return Error code.
     */
    public int getCode() {
        return code;
    }

    public String getDetailMessage() {
        return msg;
    }

    public int getMajorCode() {
        return majorCode;
    }

    public int getMinorCode() {
        return minorCode;
    }

    /*
    public String toString() {
        return "Code= " + code + ",msg=" + msg + ",majorCode=" + majorCode +
        ",minorCode=" + minorCode;
    }
    */
}
