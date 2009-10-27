/*
 * @(#)file      HtmlDef.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.19
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

class HtmlDef extends HttpDef {

    //
    // Defines
    //
    public static final String NULL             = "JDMK_NULL_VALUE";
    public static final String jdmk             = "Java Dynamic Management";
    public static final String topPageTitle     = jdmk + " View of ";
    public static final String masterPageTitle  = "Agent View";
    public static final String adminPageTitle   = "Agent Administration";
    public static final String objectPageTitle  = "MBean View";
    public static final String arrayPageTitle   = "Array View";
    public static final String docType          = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">";
    public static final String HTML_UNAVAILABLE = "<FONT color=red>UNAVAILABLE</FONT>";
    public static final String HTML_WO_DEFAULT  = "---"; // default displayed string for select bow of write-only Enumerated attributes 

    //
    // Supported types
    //
    public static final String goodType = "String String[] Integer Integer[] int int[] Double Double[] double double[] Float Float[] float float[] Short Short[] short Character Character[] char char[] Boolean Boolean[] boolean boolean[] javax.management.ObjectName javax.management.ObjectName[] Date Date[] Byte byte Long long Byte[] Long[] Number";

    //
    // Admin page
    //
    public static final String actionAdd          = "Create";
    public static final String actionDelete       = "Unregister";
    public static final String actionConstructors = "Constructors";
    public static final String sendReqBut         = "Send Request";
    public static final String resetBut           = "Reset";

    //
    // Common pages
    //
    public static final String setChangesBut = "Apply";

    //
    // Protocol
    //
    public static final String MAIN          = "/";
    public static final String FILTER        ="/Filter";
    public static final String ADMIN         = "/Admin";
    public static final String ADMIN_MAIN    = "/Main";
    public static final String ADMIN_OBJECT  = "/Objects";
    public static final String ADMIN_QUEST   = "/?";
    public static final String ADMIN_QUEST2  = "%%";
    public static final String SETFORM       = "/SetForm";
    public static final String VIEWOBJECTRES = "/ViewObjectRes";
    public static final String VIEWPROPERTY  = "/ViewProperty";
    public static final String INVOKEACTION  = "/InvokeAction";
    public static final String AUTOREFRESH   = "/AutoRefresh";

    //
    // HTML tags
    //
    public static final String endPage     = "</BODY></HTML>";
    public static final String PF          = "\r\n";
    public static final String LISTOFMBEAN = "<A HREF=\""+HtmlDef.MAIN+"\">Back to "+masterPageTitle+"</A>"; 
}
