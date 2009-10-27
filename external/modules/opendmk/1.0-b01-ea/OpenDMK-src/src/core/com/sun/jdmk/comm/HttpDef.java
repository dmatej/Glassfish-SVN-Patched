/*
 * @(#)file      HttpDef.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.21
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



class HttpDef {

    // JDMK version
    //
    public static final String jdmkVersion = com.sun.jdmk.ServiceName.JMX_IMPL_NAME + com.sun.jdmk.ServiceName.JMX_IMPL_VERSION;

    // CR + LF
    //
    public static final String CRLF = "\r\n";

    // HTTP Status Code Definitions
    //
    public static final String HTTP_REPLY_OK                          = "OK";
    public static final int    HTTP_REPLY_OK_ID                       = 200;
    public static final String HTTP_ERROR_BAD_REQUEST                 = "Bad Request";
    public static final int    HTTP_ERROR_BAD_REQUEST_ID              = 400;
    public static final String HTTP_ERROR_UNAUTHORIZED_REQUEST        = "Unauthorized";
    public static final int    HTTP_ERROR_UNAUTHORIZED_REQUEST_ID     = 401;
    public static final String HTTP_ERROR_NOT_FOUND                   = "Not Found";
    public static final int    HTTP_ERROR_NOT_FOUND_ID                = 404;
    public static final String HTTP_ERROR_DOMAIN_NOT_FOUND            = "Domain Not Found";
    public static final int    HTTP_ERROR_DOMAIN_NOT_FOUND_ID         = 455;
    public static final String HTTP_ERROR_INSTANTIATION               = "Instantiation Error";
    public static final int    HTTP_ERROR_INSTANTIATION_ID            = 456;
    public static final String HTTP_ERROR_ILLEGAL_ACCESS              = "Illegal Access";
    public static final int    HTTP_ERROR_ILLEGAL_ACCESS_ID           = 457;
    public static final String HTTP_ERROR_SERVICE_NOT_FOUND           = "Service Not Found";
    public static final int    HTTP_ERROR_SERVICE_NOT_FOUND_ID        = 458;
    public static final String HTTP_ERROR_INSTANCE_NOT_FOUND          = "Instance Not Found";
    public static final int    HTTP_ERROR_INSTANCE_NOT_FOUND_ID       = 459;
    public static final String HTTP_ERROR_CLASS_NOT_FOUND             = "Class Not Found";
    public static final int    HTTP_ERROR_CLASS_NOT_FOUND_ID          = 460;
    public static final String HTTP_ERROR_PROPERTY_NOT_FOUND          = "Property Not Found";
    public static final int    HTTP_ERROR_PROPERTY_NOT_FOUND_ID       = 462;
    public static final String HTTP_ERROR_INVOCATION_TARGET           = "Invocation Target";
    public static final int    HTTP_ERROR_INVOCATION_TARGET_ID        = 463;
    public static final String HTTP_ERROR_INVALID_PROP_VALUE          = "Invalid Property Value";
    public static final int    HTTP_ERROR_INVALID_PROP_VALUE_ID       = 464;
    public static final String HTTP_ERROR_INSTANCE_ALREADY_EXISTS     = "Instance Already Exists";
    public static final int    HTTP_ERROR_INSTANCE_ALREADY_EXISTS_ID  = 465;
    public static final String HTTP_ERROR_NO_SUCH_METHOD              = "No Such Method";
    public static final int    HTTP_ERROR_NO_SUCH_METHOD_ID           = 466;
    public static final String HTTP_ERROR_PERMISSION_DENIED           = "Permission Denied";
    public static final int    HTTP_ERROR_PERMISSION_DENIED_ID        = 469;
    public static final String HTTP_ERROR_REFLECTION                  = "Reflection";
    public static final int    HTTP_ERROR_REFLECTION_ID               = 470;
    public static final String HTTP_ERROR_MBEAN_REGISTRATION          = "MBean Registration Failure";
    public static final int    HTTP_ERROR_MBEAN_REGISTRATION_ID       = 471;
    public static final String HTTP_ERROR_MBEAN                       = "MBean Failure";
    public static final int    HTTP_ERROR_MBEAN_ID                    = 472;
    public static final String HTTP_ERROR_NOT_COMPLIANT_MBEAN         = "Not Compliant MBean";
    public static final int    HTTP_ERROR_NOT_COMPLIANT_MBEAN_ID      = 473;
    public static final String HTTP_ERROR_MALFORMED_OBJECTNAME        = "Malformed ObjectName";
    public static final int    HTTP_ERROR_MALFORMED_OBJECTNAME_ID     = 474;
    public static final String HTTP_ERROR_ATTRIBUTE_NOT_FOUND         = "Attribute Not Found";
    public static final int    HTTP_ERROR_ATTRIBUTE_NOT_FOUND_ID      = 475;
    public static final String HTTP_ERROR_INTROSPECTION               = "Introspection";
    public static final int    HTTP_ERROR_INTROSPECTION_ID            = 476;
    public static final String HTTP_ERROR_SET_ATTRIBUTES              = "All Attributes Not Set";
    public static final int    HTTP_ERROR_SET_ATTRIBUTES_ID           = 477;

    // Internal codes for HTTP requests
    //
    public static final int HTTP_BAD_REQUEST  = -1;
    public static final int HTTP_GET_REQUEST  =  1;
    public static final int HTTP_POST_REQUEST =  2;
    public static final int HTTP_HEAD_REQUEST =  3;
}
