/*
 * @(#)file      ServiceName.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.123
 * @(#)lastedit  07/03/08
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

package com.sun.jdmk;

/**
 * Used for storing default values used by JDMK services.
 */
public class ServiceName {

    // private constructor defined to "hide" the default public constructor
    private ServiceName() {
    }

    /**
     * The object name of the MBeanServer delegate object
     * <BR>
     * The value is <CODE>JMImplementation:type=MBeanServerDelegate</CODE>.
     */
    public static final String DELEGATE = "JMImplementation:type=MBeanServerDelegate" ;

    /**
     * The default key properties for registering the class loader of the MLet service.
     * <BR>
     * The value is <CODE>type=MLet</CODE>.
     */
    public static final String MLET = "type=MLet";

    /**
     * The default domain.
     * <BR>
     * The value is <CODE>DefaultDomain</CODE>.
     */
    public static final String DOMAIN = "DefaultDomain";

    /**
     * The default port for the RMI connector.
     * <BR>
     * The value is <CODE>1099</CODE>.
     */
    public static final int RMI_CONNECTOR_PORT = 1099 ;

    /**
     * The default key properties for the RMI connector.
     * <BR>
     * The value is <CODE>name=RmiConnectorServer</CODE>.
     */
    public static final String RMI_CONNECTOR_SERVER = "name=RmiConnectorServer" ;

    /**
     * The default port for the SNMP adaptor.
     * <BR>
     * The value is <CODE>161</CODE>.
     */
    public static final int SNMP_ADAPTOR_PORT = 161 ;

    /**
     * The default key properties for the SNMP protocol adaptor.
     * <BR>
     * The value is <CODE>name=SnmpAdaptorServer</CODE>.
     */
    public static final String SNMP_ADAPTOR_SERVER = "name=SnmpAdaptorServer" ;

    /**
     * The default port for the HTTP connector.
     * <BR>
     * The value is <CODE>8081</CODE>.
     */
    public static final int HTTP_CONNECTOR_PORT = 8081 ;

    /**
     * The default key properties for the HTTP connector.
     * <BR>
     * The value is <CODE>name=HttpConnectorServer</CODE>.
     */
    public static final String HTTP_CONNECTOR_SERVER = "name=HttpConnectorServer" ;

    /**
     * The default port for the HTTPS connector.
     * <BR>
     * The value is <CODE>8084</CODE>.
     */
    public static final int HTTPS_CONNECTOR_PORT = 8084 ;

    /**
     * The default key properties for the HTTPS connector.
     * <BR>
     * The value is <CODE>name=HttpsConnectorServer</CODE>.
     */
    public static final String HTTPS_CONNECTOR_SERVER = "name=HttpsConnectorServer" ;

    /**
     * The default port for the HTML adaptor.
     * <BR>
     * The value is <CODE>8082</CODE>.
     */
    public static final int HTML_ADAPTOR_PORT = 8082 ;

    /**
     * The default key properties for the HTML protocol adaptor.
     * <BR>
     * The value is <CODE>name=HtmlAdaptorServer</CODE>.
     */
    public static final String HTML_ADAPTOR_SERVER = "name=HtmlAdaptorServer" ;

    /**
     * The name of the JMX specification implemented by this product.    
     * <BR>
     * The value is <CODE>Java Management Extensions</CODE>.
     */
    public static final String JMX_SPEC_NAME = "Java Management Extensions";

    /**
     * The version of the JMX specification implemented by this product.
     * <BR>
     * The value is <CODE>1.2 Maintenance Release</CODE>.
     */
    public static final String JMX_SPEC_VERSION = "1.2 Maintenance Release";

    /**
     * The vendor of the JMX specification implemented by this product.     
     * <BR>
     * The value is <CODE>Sun Microsystems</CODE>.
     */
    public static final String JMX_SPEC_VENDOR = "Sun Microsystems";

    /**
     * The name of this product implementing the  JMX specification.
     * <BR>
     * The value is <CODE>Project OpenDMK</CODE>.
     */
    public static final String JMX_IMPL_NAME = "Project OpenDMK";

    /**
     * The name of the vendor of this product implementing the  JMX specification.  
     * <BR>
     * The value is <CODE>Sun Microsystems</CODE>.
     */
    public static final String JMX_IMPL_VENDOR = "Sun Microsystems";

    /**
      * The build number of the current product version, of the form 
      * <CODE>bXX</CODE>.
      */
    public static final String BUILD_NUMBER = "b01-ea";

    /**
     * The version of this product implementing the  JMX specification.  
     * <BR>
     * The value is <CODE>opendmk-1.0-bXX</CODE>, where <CODE>bXX</CODE> is the 
     * <CODE>BUILD_NUMBER</CODE> .
     */
    public static final String JMX_IMPL_VERSION = "opendmk-1.0-" + BUILD_NUMBER;

}
