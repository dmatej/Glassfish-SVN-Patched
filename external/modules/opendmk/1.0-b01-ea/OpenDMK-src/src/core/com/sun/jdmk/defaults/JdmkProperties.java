/*
 * @(#)file      JdmkProperties.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.9
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
 */

package com.sun.jdmk.defaults;

// java import
//
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.Enumeration;

/**
 * This class reads a file containing the property list defined for Java DMK
 * and adds all the read properties to the list of system properties.
 *
 * @since Java DMK 5.0
 */
public class JdmkProperties {

    // private constructor defined to "hide" the default public constructor
    private JdmkProperties() {
    }

    // PUBLIC STATIC METHODS
    //----------------------

    /**
     * Reads the Java DMK property list from a file and
     * adds the read properties as system properties.
     */
    public static void load(String file) throws IOException {
        Properties props = new Properties();
        InputStream is = new FileInputStream(file);
        props.load(is);
        is.close();
	for (final Enumeration e = props.keys(); e.hasMoreElements() ; ) {
	    final String key = (String) e.nextElement();
	    System.setProperty(key,props.getProperty(key));
	}
    }

    // PUBLIC STATIC VARIABLES
    //------------------------

    /**
     * References the property that specifies the directory where
     * the native libraries will be stored before the MLet Service
     * loads them into memory.
     * <p>
     * Property Name: <B>jmx.mlet.library.dir</B>
     */
    public static final String MLET_LIB_DIR = "jmx.mlet.library.dir";

    /**
     * References the property that specifies the ACL file
     * used by the SNMP protocol adaptor.
     * <p>
     * Property Name: <B>jdmk.acl.file</B>
     */
    public static final String ACL_FILE = "jdmk.acl.file";

    /**
     * References the property that specifies the Security file
     * used by the SNMP protocol adaptor.
     * <p>
     * Property Name: <B>jdmk.security.file</B>
     */
    public static final String SECURITY_FILE = "jdmk.security.file";

    /**
     * References the property that specifies the User ACL file
     * used by the SNMP protocol adaptor.
     * <p>
     * Property Name: <B>jdmk.uacl.file</B>
     */
    public static final String UACL_FILE = "jdmk.uacl.file";

    /**
     * References the property that specifies the default mib_core file
     * used by the mibgen compiler.
     * <p>
     * Property Name: <B>mibcore.file</B>
     */
    public static final String MIB_CORE_FILE = "mibcore.file";

    /**
     * References the property that specifies the full name of the JMX
     * specification implemented by this product.
     * <p>
     * Property Name: <B>jmx.specification.name</B>
     */
     public static final String JMX_SPEC_NAME = "jmx.specification.name";
    
    /**
     * References the property that specifies the version of the JMX
     * specification implemented by this product.
     * <p>
     * Property Name: <B>jmx.specification.version</B>
     */
     public static final String JMX_SPEC_VERSION = "jmx.specification.version";
    
    /**
     * References the property that specifies the vendor of the JMX
     * specification implemented by this product.
     * <p>
     * Property Name: <B>jmx.specification.vendor</B>
     */
     public static final String JMX_SPEC_VENDOR = "jmx.specification.vendor";
    
    /**
     * References the property that specifies the full name of this product
     * implementing the  JMX specification.
     * <p>
     * Property Name: <B>jmx.implementation.name</B>
     */
    public static final String JMX_IMPL_NAME = "jmx.implementation.name";
    
    /**
     * References the property that specifies the name of the vendor of this product
     * implementing the  JMX specification.
     * <p>
     * Property Name: <B>jmx.implementation.vendor</B>
     */
    public static final String JMX_IMPL_VENDOR = "jmx.implementation.vendor";
    
    /**
     * References the property that specifies the version of this product
     * implementing the  JMX specification.
     * <p>
     * Property Name: <B>jmx.implementation.version</B>
     */
    public static final String JMX_IMPL_VERSION = "jmx.implementation.version";

    /**
     * References the property that specifies the SSL cipher suites to
     * be enabled by the HTTP/SSL connector.
     * <p>
     * Property Name: <B>jdmk.ssl.cipher.suite.</B>
     * <p>
     * The list of SSL cipher suites is specified in the format:
     * <p>
     * <DD><B>jdmk.ssl.cipher.suite.</B>&lt;n&gt;<B>=</B>&lt;cipher suite name&gt;</DD>
     * <p>
     * For example:
     * <p>
     * <DD>jdmk.ssl.cipher.suite.1=SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA</DD>
     * <DD>jdmk.ssl.cipher.suite.2=SSL_RSA_EXPORT_WITH_RC4_40_MD5</DD>
     * <DD>. . .</DD>
     */
    public static final String SSL_CIPHER_SUITE = "jdmk.ssl.cipher.suite.";
}
