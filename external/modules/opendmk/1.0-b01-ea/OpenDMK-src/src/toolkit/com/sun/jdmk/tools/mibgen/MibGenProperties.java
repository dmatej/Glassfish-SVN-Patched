/* 
 * @(#)file      MibGenProperties.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.13 
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
 */ 
package com.sun.jdmk.tools.mibgen;

import java.util.Properties;
import java.util.Enumeration;
import java.io.IOException;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import	java.io.*;

public class MibGenProperties {

    private static String MIBGEN_RESOURCE_NAME=
	"com/sun/jdmk/tools/mibgen/mibgen.properties";

    public static String MIBGEN_RESOURCE_NAME_JDMK50=
	"com/sun/jdmk/tools/mibgen/mibgen.properties.jdmk50";
    
    public static final String CONFIG_RESOURCE=
	"com.sun.jdmk.tools.mibgen.config.resource";
    
    public static final String CONFIG_FILE = 
	"com.sun.jdmk.tools.mibgen.config.file";
    
    // Control whether Table accessor should be generated on Group MBeans
    //
    public static final String OPTION_USE_DISPLAY_HINT = 
	"com.sun.jdmk.tools.mibgen.options.use.display.hint";

    // Control whether Table accessor should be generated on Group MBeans
    //
    public static final String OPTION_MBEAN_TABLE_ACCESSOR = 
	"com.sun.jdmk.tools.mibgen.options.mbean.table.accessor";

    // Control whether getter should be generated for data which is
    // accessible-for-notify.
    //
    public static final String OPTION_MBEAN_AFN_GETTER = 
	"com.sun.jdmk.tools.mibgen.options.mbean.notification.data.getter";

    // Control whether MBean factory methods should be declared abstract
    // in the MIB.
    //
    public static final String OPTION_MIB_FACTORY_ABSTRACT = 
	"com.sun.jdmk.tools.mibgen.options.mib.factory.abstract";

    // This is a temporary compatibility hack - because Tiger Runtime 
    // has the skipVariable() defined, and therefore can call 
    // super.skipVariable(...) whereas JDMK does not yet have it, 
    // so super.skipVariable(...) would not compile.
    //
    // If this property is set to "true", the generated code will call
    //    `super.skipVariable(...)' - which is better because it makes
    //          possible to implement non default behavior in the
    //          runtime (today default is to return always false, but
    //          this could change).
    // Otherwise, the generated code will simply return 
    //    `false' in the default case, which is in fact what 
    //          super.skipVariable(...) would have returned.
    // 
    // Setting this property to "false" will also ensure compatibility of
    // the generated code with previous versions of JDMK (the generated
    // skipVariable(...) will never be called but will compile with
    // older versions of the runtime, whereas the code generated with
    // this property set to "true" will only compile with newer versions).
    //
    public static final String GEN_METABEAN_SKIPVAR_SUP = 
	"com.sun.jdmk.tools.mibgen.metabeangenerator.skipvar.super";

    public static final String PKG_SNMP_DEFINITIONS=
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpDefinitions";
    public static final String PKG_SNMP_COUNTER=
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpCounter";
    public static final String PKG_SNMP_COUNTER64=
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpCounter64";
    public static final String PKG_SNMP_GAUGE=
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpGauge";
    public static final String PKG_SNMP_INT= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpInt";
    public static final String PKG_SNMP_UINT= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpUnsignedInt";
    public static final String PKG_SNMP_IP_ADDR= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpIpAddress";
    public static final String PKG_SNMP_TIME_TICKS= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpTimeticks";
    public static final String PKG_SNMP_OPAQUE= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpOpaque";
    public static final String PKG_SNMP_STRING= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpString";
    public static final String PKG_SNMP_STRING_FIXED= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpStringFixed";
    public static final String PKG_SNMP_OID= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpOid";
    public static final String PKG_SNMP_NULL= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpNull";
    public static final String PKG_SNMP_VALUE= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpValue";
    public static final String PKG_SNMP_VARBIND= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpVarBind";
    public static final String PKG_SNMP_OID_RECORD= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpOidRecord";
    public static final String PKG_SNMP_STATUS_EXCEPTION= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpStatusException";
    public static final String PKG_ENUMERATED= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.Enumerated";
    public static final String PKG_SNMP_SUBREQ= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpMibSubRequest";
    public static final String PKG_SNMP_INDEX= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpIndex";
    public static final String PKG_SNMP_MIB= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpMib";
    public static final String PKG_SNMP_MIB_NODE= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpMibNode";
    public static final String PKG_SNMP_MIB_GROUP= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpMibGroup";
    public static final String PKG_SNMP_MIB_ENTRY= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpMibEntry";
    public static final String PKG_SNMP_MIB_TABLE= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpMibTable";
    public static final String PKG_SNMP_OID_TABLE_SUPPORT= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpOidTableSupport";
    public static final String PKG_SNMP_TABLE= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpMibTable";
    public static final String PKG_SNMP_TABLE_SUPPORT= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpTableSupport";
    public static final String PKG_SNMP_ENTRY_FACTORY= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpTableEntryFactory";
    public static final String PKG_SNMP_TABLE_CB= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpTableCallbackHandler";
    public static final String PKG_SNMP_STANDARD_MSRV= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpStandardMetaServer";
    public static final String PKG_SNMP_STANDARD_OSRV= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpStandardObjectServer";
    public static final String PKG_SNMP_GENERIC_MSRV= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpGenericMetaServer";
    public static final String PKG_SNMP_GENERIC_OSRV= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.SnmpGenericObjectServer";
    public static final String PKG_SNMP_ROWSTATUS= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.EnumRowStatus";

    public static String PKG_UNSIGNEDLONG= 
	"com.sun.jdmk.tools.mibgen.fullclassnames.UnsignedLong";

    private static void setDefault(Properties p, String name, String value) {
	p.put(name,System.getProperty(name,value));
    }

    private static Properties makedefaults() {
	final Properties p = new Properties();
	setDefault(p,OPTION_MIB_FACTORY_ABSTRACT,"false");
	setDefault(p,OPTION_MBEAN_TABLE_ACCESSOR,"true");
	setDefault(p,OPTION_USE_DISPLAY_HINT,"false");
	setDefault(p,GEN_METABEAN_SKIPVAR_SUP,"false");
	setDefault(p,PKG_SNMP_DEFINITIONS,
		   "com.sun.management.snmp.SnmpDefinitions");
	setDefault(p,PKG_SNMP_COUNTER,
		   "com.sun.management.snmp.SnmpCounter");
	setDefault(p,PKG_SNMP_COUNTER64,
		    "com.sun.management.snmp.SnmpCounter64");
	setDefault(p,PKG_SNMP_GAUGE,
		    "com.sun.management.snmp.SnmpGauge");
	setDefault(p,PKG_SNMP_INT,
		    "com.sun.management.snmp.SnmpInt");
	setDefault(p,PKG_SNMP_UINT,
		    "com.sun.management.snmp.SnmpUnsignedInt");
	setDefault(p,PKG_SNMP_IP_ADDR,
		    "com.sun.management.snmp.SnmpIpAddress");
	setDefault(p,PKG_SNMP_TIME_TICKS,
		    "com.sun.management.snmp.SnmpTimeticks");
	setDefault(p,PKG_SNMP_OPAQUE,
		    "com.sun.management.snmp.SnmpOpaque");
	setDefault(p,PKG_SNMP_STRING,
		    "com.sun.management.snmp.SnmpString");
	setDefault(p,PKG_SNMP_STRING_FIXED,
		    "com.sun.management.snmp.SnmpStringFixed");
	setDefault(p,PKG_SNMP_OID,
		    "com.sun.management.snmp.SnmpOid");
	setDefault(p,PKG_SNMP_NULL,
		    "com.sun.management.snmp.SnmpNull");
	setDefault(p,PKG_SNMP_VALUE,
		    "com.sun.management.snmp.SnmpValue");
	setDefault(p,PKG_SNMP_VARBIND,
		    "com.sun.management.snmp.SnmpVarBind");
	setDefault(p,PKG_SNMP_OID_RECORD,
		    "com.sun.management.snmp.SnmpOidRecord");
	setDefault(p,PKG_SNMP_STATUS_EXCEPTION,
		    "com.sun.management.snmp.SnmpStatusException");
	setDefault(p,PKG_ENUMERATED,
		    "com.sun.jdmk.Enumerated");
	setDefault(p,PKG_SNMP_SUBREQ,
		    "com.sun.management.snmp.agent.SnmpMibSubRequest");
	setDefault(p,PKG_SNMP_INDEX,
		    "com.sun.management.snmp.agent.SnmpIndex");
	setDefault(p,PKG_SNMP_MIB,
		    "com.sun.management.snmp.agent.SnmpMib");
	setDefault(p,PKG_SNMP_MIB_NODE,
		    "com.sun.management.snmp.agent.SnmpMibNode");
	setDefault(p,PKG_SNMP_MIB_GROUP,
		    "com.sun.management.snmp.agent.SnmpMibGroup");
	setDefault(p,PKG_SNMP_MIB_ENTRY,
		    "com.sun.management.snmp.agent.SnmpMibEntry");
	setDefault(p,PKG_SNMP_MIB_TABLE,
		    "com.sun.management.snmp.agent.SnmpMibTable");
	setDefault(p,PKG_SNMP_OID_TABLE_SUPPORT,
		    "com.sun.management.snmp.SnmpOidTableSupport");
	setDefault(p,PKG_SNMP_TABLE,
		    "com.sun.management.snmp.agent.SnmpMibTable");
	setDefault(p,PKG_SNMP_TABLE_SUPPORT,
		    "com.sun.management.snmp.agent.SnmpTableSupport");
	setDefault(p,PKG_SNMP_ENTRY_FACTORY,
		    "com.sun.management.snmp.agent.SnmpTableEntryFactory");
	setDefault(p,PKG_SNMP_TABLE_CB,
		    "com.sun.management.snmp.agent.SnmpTableCallbackHandler");
	setDefault(p,PKG_SNMP_STANDARD_MSRV,
		    "com.sun.management.snmp.agent.SnmpStandardMetaServer");
	setDefault(p,PKG_SNMP_STANDARD_OSRV,
		    "com.sun.management.snmp.agent.SnmpStandardObjectServer");
	setDefault(p,PKG_SNMP_GENERIC_MSRV,
		    "com.sun.management.snmp.agent.SnmpGenericMetaServer");
	setDefault(p,PKG_SNMP_GENERIC_OSRV,
		    "com.sun.management.snmp.agent.SnmpGenericObjectServer");
	setDefault(p,PKG_SNMP_ROWSTATUS,
		    "com.sun.management.snmp.EnumRowStatus");
	setDefault(p, PKG_UNSIGNEDLONG,
		    "com.sun.jdmk.UnsignedLong");
	return p;
    }

    // Load the default properties.
    //      
    private static Properties load() {
	// Create set of default properties.
	//
	final Properties p = makedefaults();

	// Resulting properties have default properties as parent.
	//
	final Properties result   = new Properties(p);

	// We load the resource file in a temporary Properties object.
	//
	final Properties resource = new Properties();
	try {
	    final String filename = System.getProperty(CONFIG_FILE);
	    final InputStream in;

	    if (filename != null) {
		// If a filename was specified for the resource file,
		// use this.
		//
		in = new FileInputStream(filename);
	    } else {
		String resourceName = 
		    System.getProperty(CONFIG_RESOURCE);
		
		if(resourceName == null)
		    resourceName = MIBGEN_RESOURCE_NAME;
		
		final ClassLoader mycl   = 
		    MibGenProperties.class.getClassLoader();
		final ClassLoader loader = (mycl==null)?
		    ClassLoader.getSystemClassLoader() : mycl;
		in = loader.getResourceAsStream(resourceName);
	    }

	    // Nothing to load? well, returns result.
	    //
	    if(in == null) return result;
	    resource.load(in);

	    // Foreach property specified in the resource, if there is
	    // a corresponding System property, take the System property.
	    // Otherwise, take the resource property (see setDefault())
	    // (Fix for 4937346 System property should take precedence 
	    //                  over resource-specified properties)
	    // 
	    for (Enumeration e=resource.keys();e.hasMoreElements();) {
		final String name  = (String) e.nextElement();
		final String value = resource.getProperty(name);

		// System property gets precedence.
		//
		setDefault(result,name,value);
	    }

	} catch (IOException x) {
	    // XXX Revisit: Must log an error!
	    x.printStackTrace();
	}

	return result;
    }

    /**
     * If the package name is not the "com.sun.management" one, it means that
     * generation is done for legacy code.
     */
    public static boolean isDeprecatedEnabled() {
	return !"com.sun.management.snmp.EnumRowStatus".
	    equals(MibGenProperties.getProperty(MibGenProperties.
						PKG_SNMP_ROWSTATUS));
    }
    
    public static boolean useDisplayHint() {
	return getBooleanProperty(OPTION_USE_DISPLAY_HINT,"false");
    }

    private static synchronized Properties properties() {
	if (properties == null) {
	    properties = load();
	}
	return properties;
    }

    public static synchronized void define(String property) 
	throws IllegalArgumentException {
	try {
	    final InputStream is = 
		new ByteArrayInputStream(property.getBytes());
	    properties().load(is);
	} catch (Exception x) {
	    throw new IllegalArgumentException(property);
	}
    }

    public static synchronized void setProperty(String name, String value) {
	properties().setProperty(name,value);
    }

    public static Properties getProperties() {
	return new Properties(properties());
    }

    public static String getProperty(String name) {
	return properties().getProperty(name);
    }

    public static String getProperty(String name, String defval) {
	// System.out.println("*** " + name + "=" + 
	//    properties.getProperty(name,defval));
	return properties().getProperty(name,defval);
    }

    public static boolean getBooleanProperty(String name, boolean defval) {
	// System.out.println("*** " + name + "=" + 
	//    properties.getProperty(name,defval));
	final String val = properties().getProperty(name);
	if (val == null) return defval;
	return booleanProperty(val);
    }

    public static boolean getBooleanProperty(String name, String defval) {
	// System.out.println("*** " + name + "=" + 
	//    properties.getProperty(name,defval));
	final String val = properties().getProperty(name,defval);
	return booleanProperty(val);
    }

    public static boolean booleanProperty(String val) {
	return Boolean.valueOf(val).booleanValue();
    }

    public static void store(String filename) throws IOException {
	store(getProperties(),filename);
    }

    public static void store(Properties p, String filename) 
	throws IOException {
	
	final Properties store = new Properties();
	if (p!=null) { 
	    for (Enumeration e=p.propertyNames();e.hasMoreElements();) {
		final String name=(String)e.nextElement();
		store.setProperty(name,p.getProperty(name));
	    }
	}
	store.store(new FileOutputStream(filename),
		    "mibgen.properties");
    }


    static Properties properties = null;
}
