/*
 * @(#)file      Def.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.41
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


package com.sun.jdmk.tools.mibgen;



import	java.io.*;
import	java.util.*;

/**
 * Default strings. 
 */
public  class Def implements Serializable {
  
    /**
     * Key words 
     */
    public static String ABSTRACT = "abstract ";
    public static String BOOLEAN = "boolean ";
    public static String CLASS = "class ";
    public static String INTERFACE = "interface ";
    public static String EXTEND= " extends ";
    public static String JAVA = ".java";
    public static String IMPLEMENT= " implements ";
    public static String IMPORT= "import ";
    public static String N_LBRACE= "{\n";
    public static String LBRACE= " {\n";
    public static String NEW= "new ";
    public static String PACKAGE= "package ";
    public static String FINAL = "final ";
    public static String PROTECTED = "protected ";
    public static String PRIVATE = "private ";
    public static String PUBLIC = "public "; 
    public static String RBRACE= "}\n";
    public static String N_RBRACE= "}";
    public static String RETURN= "return ";
    public static String SEMICOLON= ";\n";
    public static String SERIALIZABLE= "Serializable";
    public static String STATIC= "static ";
    public static String SYNCHRONIZE= "synchronized ";
    public static String THROWS= "throws ";
    public static String THROW_NEW= "throw new ";
    public static String VOID= "void ";
  
    /**
     * Some prefixes/suffixes used in the generated code
     */
    public static String ACCESS= "access";
    public static String BUILD= "build";
    public static String INIT= "init";
    public static String CHECK= "check";
    public static String FIXED= "Fixed";
    public static String GET= "get";
    public static String GET_ENTRY= "getEntry";
    public static String GET_ENTRY_OID= "getEntryOid";
    public static String GET_OBJNAME= "getObjectName";
    public static String GET_ENTRYNAME= "getEntryName";
    public static String GET_ENTRIES= "getEntries";
    public static String GET_NEXT= "getNext";
    public static String GET_NEXTVAR= "getNextVarEntry";
    public static String SET= "set";
    public static String SET_MOI= "setInstance";
    public static String SET_OBJSRV= "setObjectServer";
    public static String SET_OBJNAME= "setObjectName";
    public static String TABLEPREFIX= "Table";
    public static String TABLEMETA= "Meta";
    public static String ENUMPREFIX= "Enum";
    public static String METAPREFIX= "Meta";
    public static String GENERICPREFIX= "Generic";
    public static String OIDTABLESUFFIX= "OidTable";
    public static String MBEANSUFFIX= "MBean";
    public static String MBEANSERVER= "MBeanServer";
  
    /**
     * Some packages
     */
    
    // java imports
    //
    public static String PKG_SERIALIZABLE= "java.io.Serializable";
    public static String PKG_HASHTABLE= "java.util.Hashtable";
    public static String PKG_VECTOR= "java.util.Vector";
    
    // jmx imports
    //
    public static String PKG_MBEAN_SERVER= "javax.management.MBeanServer";
    public static String PKG_OBJECT_NAME= "javax.management.ObjectName";
    public static String PKG_INSTANCE_ALREADY_EXISTS_EXCEPTION= 
	"javax.management.InstanceAlreadyExistsException";
    
    public static String PKG_SNMP_DEFINITIONS= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_DEFINITIONS,
				     "com.sun.management.snmp.SnmpDefinitions");
    public static String PKG_SNMP_COUNTER= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_COUNTER,
				     "com.sun.management.snmp.SnmpCounter");
    public static String PKG_SNMP_COUNTER64= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_COUNTER64,
				     "com.sun.management.snmp.SnmpCounter64");
    public static String PKG_SNMP_GAUGE= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_GAUGE,
				     "com.sun.management.snmp.SnmpGauge");
    public static String PKG_SNMP_INT= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_INT,
				     "com.sun.management.snmp.SnmpInt");
    public static String PKG_SNMP_UINT= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_UINT,
				     "com.sun.management.snmp.SnmpUnsignedInt");
    public static String PKG_SNMP_IP_ADDR= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_IP_ADDR,
				     "com.sun.management.snmp.SnmpIpAddress");
    public static String PKG_SNMP_TIME_TICKS= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_TIME_TICKS,
				     "com.sun.management.snmp.SnmpTimeticks");
    public static String PKG_SNMP_OPAQUE= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_OPAQUE,
				     "com.sun.management.snmp.SnmpOpaque");
    public static String PKG_SNMP_STRING= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_STRING,
				     "com.sun.management.snmp.SnmpString");
    public static String PKG_SNMP_STRING_FIXED= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_STRING_FIXED,
				     "com.sun.management.snmp.SnmpStringFixed");
    public static String PKG_SNMP_OID= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_OID,
				     "com.sun.management.snmp.SnmpOid");
    public static String PKG_SNMP_NULL= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_NULL,
				     "com.sun.management.snmp.SnmpNull");
    public static String PKG_SNMP_VALUE= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_VALUE,
				     "com.sun.management.snmp.SnmpValue");
    public static String PKG_SNMP_VARBIND= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_VARBIND,
				     "com.sun.management.snmp.SnmpVarBind");
    public static String PKG_SNMP_OID_RECORD= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_OID_RECORD,
				     "com.sun.management.snmp.SnmpOidRecord");
    public static String PKG_SNMP_STATUS_EXCEPTION= 
	MibGenProperties.getProperty(
				MibGenProperties.PKG_SNMP_STATUS_EXCEPTION,
				"com.sun.management.snmp.SnmpStatusException");
    
    // RI imports
    //
    public static String PKG_ENUMERATED= 
	MibGenProperties.getProperty(MibGenProperties.PKG_ENUMERATED,
				     "com.sun.jdmk.Enumerated");
    
    // jdmk imports
    //
    public static String PKG_SNMP_SUBREQ= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_SUBREQ,
			 "com.sun.management.snmp.agent.SnmpMibSubRequest");
    public static String PKG_SNMP_INDEX= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_INDEX,
				     "com.sun.management.snmp.agent.SnmpIndex");
    public static String PKG_SNMP_MIB= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_MIB,
				     "com.sun.management.snmp.agent.SnmpMib");
    public static String PKG_SNMP_MIB_NODE= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_MIB_NODE,
				     "com.sun.management.snmp.agent.SnmpMibNode");
    public static String PKG_SNMP_MIB_GROUP= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_MIB_GROUP,
				     "com.sun.management.snmp.agent.SnmpMibGroup");
    public static String PKG_SNMP_MIB_ENTRY= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_MIB_ENTRY,
				     "com.sun.management.snmp.agent.SnmpMibEntry");
    public static String PKG_SNMP_MIB_TABLE= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_MIB_TABLE,
				     "com.sun.management.snmp.agent.SnmpMibTable");
    public static String PKG_SNMP_OID_TABLE_SUPPORT= 
	MibGenProperties.getProperty(
			 MibGenProperties.PKG_SNMP_OID_TABLE_SUPPORT,
			 "com.sun.management.snmp.SnmpOidTableSupport");
    public static String PKG_SNMP_TABLE= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_TABLE,
				     "com.sun.management.snmp.agent.SnmpMibTable");
    public static String PKG_SNMP_TABLE_SUPPORT= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_TABLE_SUPPORT,
			 "com.sun.management.snmp.agent.SnmpTableSupport");
    public static String PKG_SNMP_ENTRY_FACTORY= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_ENTRY_FACTORY,
			 "com.sun.management.snmp.agent.SnmpTableEntryFactory");
    public static String PKG_SNMP_TABLE_CB= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_TABLE_CB,
			 "com.sun.management.snmp.agent.SnmpTableCallbackHandler");
    public static String PKG_SNMP_STANDARD_MSRV= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_STANDARD_MSRV,
			 "com.sun.management.snmp.agent.SnmpStandardMetaServer");
    public static String PKG_SNMP_STANDARD_OSRV= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_STANDARD_OSRV,
			 "com.sun.management.snmp.agent.SnmpStandardObjectServer");
    public static String PKG_SNMP_GENERIC_MSRV= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_GENERIC_MSRV,
			 "com.sun.management.snmp.agent.SnmpGenericMetaServer");
    public static String PKG_SNMP_GENERIC_OSRV= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_GENERIC_OSRV,
			 "com.sun.management.snmp.agent.SnmpGenericObjectServer");
    public static String PKG_SNMP_ROWSTATUS= 
	MibGenProperties.getProperty(MibGenProperties.PKG_SNMP_ROWSTATUS,
				     "com.sun.management.snmp.EnumRowStatus");
        
    // NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
    public static String PKG_UNSIGNEDLONG= 
	MibGenProperties.getProperty(MibGenProperties.PKG_UNSIGNEDLONG,
				     "com.sun.jdmk.UnsignedLong");
    // end of NPCTE fix for bugId 4692891
    /**
     * Some symbols defined in the SNMP package.
     */
    public static String EXCP_SNMP= "SnmpStatusException";
    public static String EXCP_ILLEGAL= "IllegalAccessException";
    public static String EXCP_ARGUMENT= "IllegalArgumentException";
    public static String EXCP_RT= "RuntimeException";
    
    public static String ENUM_CLASS= "Enumerated";
    public static String ENUM_INTTAB= "intTable";
    public static String ENUM_STRINGTAB= "stringTable";
  
    public static String METH_TOOID= "toOid";
    public static String METH_INSERTINOID= "insertInOid"; // for SnmpVarBind
    public static String METH_APPENDINOID= "appendInOid"; // for SnmpVarBind
    public static String METH_FROMINDEX= "toOid";
    public static String METH_NEXTID= "getNextIdentifier";
    public static String METH_GETNEXTINDDEX= "getNextIndex"; // in SnmpMibTable
    public static String METH_NEXTINDDEX= "nextOid";
    public static String METH_T_ADD= "addEntry";
    public static String METH_T_REM= "removeEntry";
    public static String METH_T_REMENTRYCB= "removeEntryCb";
    public static String METH_T_INIT= "init";
    public static String METH_T_CREATE= "createNewEntry";
    public static String METH_T_BASIC= "getBasicEntries"; // in SnmpMibTable
    public static String METH_T_GETNEXTTABLE= "getNextVarEntry"; 
                         // in SnmpMibTable
    public static String METH_T_BUILDNAME= "buildNameFromIndex";
    public static String METH_T_BUILDOID= "buildOidFromIndex"; 
                         // in SnmpMibTable
    public static String METH_T_BUILDOIDVAL= "buildOidFromIndexVal"; 
                         // in SnmpMibTable
    public static String METH_T_GETVALENTRY= "getValueOfEntry"; 
                         // in SnmpMibTable
    public static String METH_T_SETVALENTRY= "setValueOfEntry"; 
                         // in SnmpMibTable
    public static String METH_T_CHECKVALENTRY= "checkValueOfEntry"; 
                         // in SnmpMibTable

    // public static String METH_T_GETREQ= "getEntryValues";
    // public static String METH_T_SETREQ= "setEntryValues";
    // public static String METH_T_CHECKREQ= "checkEntryValues";
    public static String METH_T_GETREQ       = "get";
    public static String METH_T_SETREQ       = "set";
    public static String METH_T_CHECKREQ     = "check";
    public static String METH_T_VALIDATE     = "validateVarEntryId";
    public static String METH_T_READABLE     = "isReadableEntryId";
    public static String METH_T_GETNEXTVARID = "getNextVarEntryId";
    public static String METH_T_SKIPVARIABLE = "skipEntryVariable";
    public static String METH_T_GETRSID      = "getRowStatusId";
    public static String METH_T_GETRSNAME    = "getRowStatusName";
    public static String METH_T_MAPRSVALUE   = "mapRowStatus";
    public static String METH_T_ISRS         = "isRowStatus";
    public static String METH_T_HASRS        = "hasRowStatus";
    public static String METH_T_SETRS        = "setRowStatus";
    public static String METH_T_GETRS        = "getRowStatus";
    public static String METH_T_ISROWREADY   = "isRowReady";


    public static String METH_APPENDTOOID="appendToOid"; // in SnmpValue ...
    public static String METH_REGISTER_VAR="registerObject";
    public static String METH_IS_SUBARC="isNestedArc";
    public static String METH_IS_VARIABLE="isVariable";
    public static String METH_IS_READABLE="isReadable";
    public static String METH_SKIP_VARIABLE="skipVariable";
    public static String METH_IS_TABLE="isTable";
    public static String METH_GET_TABLE="getTable";
    public static String METH_GET_REQ="get";
    public static String METH_SET_REQ="set";
    public static String METH_CHECK_REQ="check";
    public static String METH_GET_VAR="get";
    public static String METH_SET_VAR="set";
    public static String METH_CHECK_VAR="check";
    public static String METH_BUILDVAL="buildSnmpValue";
    public static String METH_BUILDATT="buildAttributeValue";
    public static String METH_GET_ATTNAME="getAttributeName";
    public static String METH_CHECK_SET="checkSetAccess";
    public static String METH_CHECK_GET="checkGetAccess";
    public static String METH_T_GETOID= "getGroupOid";
    public static String METH_T_GETOBJNAME= "getGroupObjectName";
    public static String METH_T_POPULATE= "populate";
    public static String METH_T_REGGROUP="registerGroupNode";
    public static String METH_T_REGTABLES="registerTableNodes";
    public static String METH_T_REGENTRY="registerEntryNode";
    public static String METH_T_REGTABLEMETA="registerTableMeta";
    public static String METH_T_GETTABLEMETA="getRegisteredTableMeta";
    public static String METH_T_SETOBJSRV="setObjectServer";
    public static String METH_T_GETOBJSRV="getObjectServer";
    public static String METH_T_GETSTDOBJSRV="getStandardObjectServer";
    public static String METH_T_ISNAMEREQ="isRegistrationRequired";


    public static String SNMP_TABLE= "SnmpMibTable";
    public static String SNMP_TABLE_SUPPORT= "SnmpTableSupport";
    public static String SNMP_NODE= "SnmpMibNode";
    public static String SNMP_GROUP= "SnmpMibGroup";
    public static String SNMP_ENTRY= "SnmpMibEntry";
    public static String SNMP_NODEOID= "SnmpMibOid";
    public static String SNMP_MIB= "SnmpMib";
    public static String SNMP_OID= "SnmpOid";
    public static String SNMP_VALUE= "SnmpValue";
    public static String SNMP_INT= "SnmpInt";
    public static String SNMP_ROWSTATUS= "EnumRowStatus";
    public static String SNMP_USERDATA= "Object";
    public static String SNMP_VARBIND= "SnmpVarBind";
    public static String SNMP_INDEX= "SnmpIndex";
    public static String SNMP_SUBREQ= "SnmpMibSubRequest";
    public static String SNMP_VARLIST= "varList";
    public static String SNMP_SORT= "sort";
    public static String SNMP_OID_TABLE= "SnmpOidTableSupport";
    public static String SNMP_STANDARD_MSRV= "SnmpStandardMetaServer";
    public static String SNMP_STANDARD_OSRV= "SnmpStandardObjectServer";
    public static String SNMP_GENERIC_MSRV= "SnmpGenericMetaServer";
    public static String SNMP_GENERIC_OSRV= "SnmpGenericObjectServer";
    public static String OBJECT_NAME = "ObjectName";
    
    /**
     * Some variables defined in SnmpStatusException
     */
    public static String V_READONLY= "snmpRspReadOnly";
    public static String V_NOSUCHNAME= "snmpRspNoSuchName"; 
    public static String V_NOACCESS= "snmpRspNoAccess";
    public static String V_NOSUCHINSTANCE= "noSuchInstance";
    public static String V_NOSUCHOBJECT= "noSuchObject";
    public static String V_BADVALUE= "snmpRspBadValue";
    public static String V_WRONGVALUE= "snmpRspWrongValue";
    public static String V_WRONGTYPE= "snmpRspWrongType";
    public static String V_NOTWRITABLE= "snmpRspNotWritable";
    public static String V_INCONSISTENTNAME= "snmpRspInconsistentName";
    public static String V_INCONSISTENTVALUE= "snmpRspInconsistentValue";
    public static String V_NOCREATION= "snmpRspNoCreation";
    

    /**
     * Some constants
     **/
    public static String C_SNMP_VERSION1= "SnmpDefinitions.snmpVersionOne";
    public static String C_SNMP_VERSION2= "SnmpDefinitions.snmpVersionTwo";
    public static String C_SNMP_VERSION3= "SnmpDefinitions.snmpVersionThree";

    /**
     * Tabulation rules
     */         
    public static String TAB = "    ";
    public static String TAB2 = new  String ( TAB + TAB);
    public static String TAB3 = new  String ( TAB + TAB + TAB);
    public static String TAB4 = new  String ( TAB + TAB + TAB + TAB);
    public static String TAB5 = new  String ( TAB + TAB + TAB + TAB + TAB);
    public static String TAB6 = new  String ( TAB + TAB + TAB + TAB + TAB + 
					      TAB);
    public static String LINE = 
	"// ------------------------------------------------------------";
}
