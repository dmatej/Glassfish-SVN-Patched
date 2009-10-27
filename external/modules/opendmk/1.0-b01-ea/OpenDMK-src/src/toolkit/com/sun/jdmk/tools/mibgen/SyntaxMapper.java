/*
 * @(#)file      SyntaxMapper.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.27
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



import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * The class contains rules used for mapping syntaxes.
 *
 */
public class SyntaxMapper implements Serializable {
    
    // NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
    static public boolean useUnsigned = false ;
    // end of NPCTE fix for bugId 4692891
    
    static public String MibStoreSequenceSymbol= "TA";
    static public String MibStoreEntrySymbol= "EN";
    static public String MibStoreIdentitySymbol= "ID";
    static public String MibStoreNotificationSymbol= "NT";
    static public String MibStoreNotificationGroupSymbol= "NTG";
    static public String MibStoreObjectGroupSymbol= "OBG";

    static Hashtable smiMappingRules= new Hashtable();
    static Hashtable asnMappingRules= new Hashtable();
    static Hashtable smiSnmpMappingRules= new Hashtable();
    static Hashtable asnSnmpMappingRules= new Hashtable();
    static Hashtable initRules= new Hashtable();
    static Hashtable intSyntaxMappingRules= new Hashtable();
    static Hashtable mibStoreMappingRules= new Hashtable();
    static Hashtable snmpValue2MbeanValue = new Hashtable();
    static {
        smiMappingRules.put("Counter", "Long ");
        smiMappingRules.put("Counter32", "Long ");
        smiMappingRules.put("Counter64", "Long ");
        smiMappingRules.put("Gauge", "Long ");
        smiMappingRules.put("Gauge32","Long ");
        smiMappingRules.put("Integer32", "Integer ");
        smiMappingRules.put("Unsigned32", "Long ");
        smiMappingRules.put("Integer64", "Long ");
        smiMappingRules.put("IpAddress", "String ");
        smiMappingRules.put("TimeTicks", "Long ");
        smiMappingRules.put("Opaque", "Byte[] ");
        smiMappingRules.put("DisplayString", "String ");
        smiMappingRules.put("TruthValue", "Integer ");
        smiMappingRules.put("NetworkAddress", "String ");
    
        mibStoreMappingRules.put("SnmpCounter", "C");
        mibStoreMappingRules.put("SnmpCounter64", "C64");
        mibStoreMappingRules.put("SnmpGauge", "G");
        mibStoreMappingRules.put("SnmpInt", "I");
        mibStoreMappingRules.put("SnmpUnsignedInt", "U");
        mibStoreMappingRules.put("SnmpIpAddress", "IP");
        mibStoreMappingRules.put("SnmpTimeticks", "T");
        mibStoreMappingRules.put("SnmpOpaque", "O");
        mibStoreMappingRules.put("SnmpString", "S");
        mibStoreMappingRules.put("SnmpOid", "OI");
        mibStoreMappingRules.put("SnmpNull", "NU");
        mibStoreMappingRules.put("SnmpSequence", "TA");
   
    
        asnMappingRules.put(new Integer(ParserConstants.INTEGER), "Integer ");
        asnMappingRules.put(new Integer(ParserConstants.OCTET), "Byte[] ");
        asnMappingRules.put(new Integer(ParserConstants.OBJECT), "String ");
        asnMappingRules.put(new Integer(ParserConstants.NULL), "String ");
        asnMappingRules.put(new Integer(ParserConstants.BIT), "Byte[] ");
    
        smiSnmpMappingRules.put("Counter", "SnmpCounter.toLong");
        smiSnmpMappingRules.put("Counter32", "SnmpCounter.toLong");
        smiSnmpMappingRules.put("Counter64", "SnmpCounter64.toLong");
        smiSnmpMappingRules.put("Gauge", "SnmpGauge.toLong");
        smiSnmpMappingRules.put("Gauge32","SnmpGauge.toLong");
        smiSnmpMappingRules.put("Integer32", "SnmpInt.toInteger");
        smiSnmpMappingRules.put("Unsigned32", "SnmpGauge.toLong");
        smiSnmpMappingRules.put("Integer64", "SnmpInt.toLong");
        smiSnmpMappingRules.put("IpAddress", "SnmpIpAddress.toString");
        smiSnmpMappingRules.put("TimeTicks", "SnmpTimeticks.toLong");
        smiSnmpMappingRules.put("Opaque", "SnmpOpaque.toByte");
        smiSnmpMappingRules.put("DisplayString", "SnmpString.toString");
        smiSnmpMappingRules.put("TruthValue", "SnmpInt.toInteger");
        smiSnmpMappingRules.put("NetworkAddress", "SnmpIpAddress.toString");
    
        asnSnmpMappingRules.put(new Integer(ParserConstants.INTEGER), "SnmpInt.toInteger");
        asnSnmpMappingRules.put(new Integer(ParserConstants.OCTET), "SnmpString.toByte");
        asnSnmpMappingRules.put(new Integer(ParserConstants.OBJECT), "SnmpOid.toString");
        asnSnmpMappingRules.put(new Integer(ParserConstants.NULL), "SnmpNull.toString");
        asnSnmpMappingRules.put(new Integer(ParserConstants.BIT), "SnmpString.toByte");
    
        asnSnmpMappingRules.put(new Integer(ParserConstants.SEQUENCE), "SnmpSequence.toInteger");
    
        initRules.put("SnmpCounter.toLong", " = new Long(1)");    
        initRules.put("SnmpCounter64.toLong", " = new Long(1)"); 
        initRules.put("SnmpGauge.toLong", " = new Long(1)");  
        initRules.put("SnmpInt.toInteger", " = new Integer(1)");
        initRules.put("SnmpInt.toLong", " = new Long(1)");
        initRules.put("SnmpUnsignedInt.toLong", " = new Long(2)");
        initRules.put("SnmpIpAddress.toString", " = new String(\"192.9.9.100\")");
        initRules.put("SnmpTimeticks.toLong", " = new Long(1)");
        initRules.put("SnmpOpaque.toByte", " = { new Byte(\"74\"), new Byte(\"68\"), new Byte(\"77\"), new Byte(\"75\")}");
        initRules.put("SnmpString.toString", " = new String(\"JDMK 5.1\")");
        initRules.put("SnmpInt.toBoolean", " = new Boolean(true)");    
        initRules.put("SnmpString.toByte", " = { new Byte(\"74\"), new Byte(\"68\"), new Byte(\"77\"), new Byte(\"75\")}");
        initRules.put("SnmpOid.toString", " = new String(\"1.3.6.1.4.1.42\")");
        initRules.put("SnmpNull.toString", " = new String(\"null\")"); 
        
        // This mapping is used to initialize the default value.
        //
        intSyntaxMappingRules.put("SnmpCounter.toLong", new Integer(SyntaxMapper.COUNTER32));
        intSyntaxMappingRules.put("SnmpCounter64.toLong", new Integer(SyntaxMapper.COUNTER64));
        intSyntaxMappingRules.put("SnmpGauge.toLong", new Integer(SyntaxMapper.GAUGE));
        intSyntaxMappingRules.put("SnmpInt.toInteger", new Integer(SyntaxMapper.INTEGER32));    
        intSyntaxMappingRules.put("SnmpInt.toLong", new Integer(SyntaxMapper.INTEGER64));
        intSyntaxMappingRules.put("SnmpIpAddress.toString", new Integer(SyntaxMapper.IP_ADDRESS));
        intSyntaxMappingRules.put("SnmpTimeticks.toLong", new Integer(SyntaxMapper.TIME_TICKS));
        intSyntaxMappingRules.put("SnmpOpaque.toByte", new Integer(SyntaxMapper.OPAQUE));    
        intSyntaxMappingRules.put("SnmpString.toString", new Integer(SyntaxMapper.STRING));
        intSyntaxMappingRules.put("SnmpString.toByte", new Integer(SyntaxMapper.BYTE));
        intSyntaxMappingRules.put("SnmpOid.toString", new Integer(SyntaxMapper.OBJECT_ID));
        intSyntaxMappingRules.put("SnmpNull.toString", new Integer(SyntaxMapper.NULL));    

	// This mapping is used to construct a SnmpValue from
	// an attribute value.
	//
        snmpValue2MbeanValue.put("SnmpCounter.toLong","Long");
        snmpValue2MbeanValue.put("SnmpCounter64.toLong","Long");
        snmpValue2MbeanValue.put("SnmpGauge.toLong","Long");
        snmpValue2MbeanValue.put("SnmpInt.toInteger","Integer");
        snmpValue2MbeanValue.put("SnmpInt.toLong", "Long");
        snmpValue2MbeanValue.put("SnmpIpAddress.toString","String");
        snmpValue2MbeanValue.put("SnmpTimeticks.toLong","Long");
        snmpValue2MbeanValue.put("SnmpOpaque.toByte","Byte[]");
        snmpValue2MbeanValue.put("SnmpString.toString","String");
        snmpValue2MbeanValue.put("SnmpString.toByte","Byte[]");
        snmpValue2MbeanValue.put("SnmpOid.toString","String");
        snmpValue2MbeanValue.put("SnmpNull.toString","String");
    }
    
    
    // CONSTANTS USED TO PARSE THE DEFAULT VALUE.
    //
    final static int COUNTER32  = 1;
    final static int COUNTER64  = 2;
    final static int GAUGE      = 3;
    final static int INTEGER32  = 4;
    final static int INTEGER64  = 5;
    final static int IP_ADDRESS = 6;
    final static int TIME_TICKS = 7;
    final static int OPAQUE     = 8;
    final static int STRING     = 9;
    final static int BYTE       = 10;
    final static int OBJECT_ID  = 11;
    final static int NULL       = 12;
    
    // NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
    static public void fillTable(boolean useUnsgd) {
        useUnsigned = useUnsgd; 
        if (useUnsigned) {
            smiMappingRules.put("Counter64", "UnsignedLong ");     
            smiSnmpMappingRules.put("Counter64", "SnmpCounter64.toUnsignedLong");  
            initRules.put("SnmpCounter64.toUnsignedLong", " = UnsignedLong.make(1)");      
            intSyntaxMappingRules.put("SnmpCounter64.toUnsignedLong", new Integer(SyntaxMapper.COUNTER64));
            snmpValue2MbeanValue.put("SnmpCounter64.toUnsignedLong","UnsignedLong");
                                               
        }
        else {
            smiMappingRules.put("Counter64", "Long ");
            smiSnmpMappingRules.put("Counter64", "SnmpCounter64.toLong");
            initRules.put("SnmpCounter64.toLong", " = new Long(1)");
            intSyntaxMappingRules.put("SnmpCounter64.toLong", new Integer(SyntaxMapper.COUNTER64));
            snmpValue2MbeanValue.put("SnmpCounter64.toLong","Long");
        }
    }
    // end of NPCTE fix for bugId 4692891

    static public int getIntSnmpSyntax(String snmpSyntax) {
        Integer i = (Integer)intSyntaxMappingRules.get(snmpSyntax.trim());
        if (i == null)
            return -1;
        return i.intValue();
    }
    
    /**
     * use the information contained in the mapping table to give the Java syntax 
     * associated to a well defined ASN syntax. Return null if the syntax is not known
     */
    static public String getMbeanSyntax(String syntaxName) {
        return (String) smiMappingRules.get(syntaxName);
    }
  
    /**
     * Return the Java representation associated to a basic ASN.1 type
     */
    static public String getMbeanSyntax(int parserNodeId) {
        return (String) asnMappingRules.get(new Integer(parserNodeId));
    }

    /**
     * Returns the Java type corresponding to the given SnmpValue name.
     * e.g. SnmpInt.toLong => Long
     *      SnmpInt.toInteger => Integer
     **/
    static public String getJavaSyntax(String snmpValueName) {
        return (String) snmpValue2MbeanValue.get(snmpValueName);
    }
  
    /**
     * Return the SNMP class to use for representing such a type
     */
    static public String getSnmpSyntax(String syntaxName) {
        return (String) smiSnmpMappingRules.get(syntaxName);
    }

    /**
     * Return the SNMP class to use for representing such a basic
     * ASN.1 type.
     */
    static public String getSnmpSyntax(int parserNodeId) {
        return (String) asnSnmpMappingRules.get(new Integer(parserNodeId));
    }
  
    /**
     * Return the initializer to use in a Mbean
     */
    static public String getInitializer(String key) {
        return (String) initRules.get(key);
    }
  
    /**
     * The method splits the content of asnSnmpMappingRules or smiSnmpMappingRules
     * and returns the method name required for getting the getter for a specific
     * primitive type.
     * The method makes the assumption that the data structure is:
     * <SNMP Value Object>.<name of method>.
     */
    static public String getCastMethod(String entry) {
        int pos= entry.indexOf(".");
        return entry.substring(pos+ 1);
    }
  
    /**
     * The method splits the content of asnSnmpMappingRules or smiSnmpMappingRules
     * and returns the method name required for getting the getter for a specific
     * primitive type.
     * The method makes the assumption that the data structure is:
     * <SNMP Value Object>.<name of method>.
     */
    static public String getIndexCastMethod(String entry) {
        int pos= entry.indexOf(".");
	
	// This is an ugly patch but I can't find another way to do this.
	//
	// 
	if (entry.startsWith("SnmpString") && entry.endsWith("toString"))
	    return "toOctetString";
        return entry.substring(pos+ 1);
    }
  
    static public String getTypeName(String entry) {
        int pos= entry.indexOf(".");
        return entry.substring(0, pos);
    }
    
    static public String getMibStoreSyntax(String syntax) {
        String result= (String) mibStoreMappingRules.get(syntax);
        if (result == null)
            return "NA";
        else 
            return result;
    }

    // Returns true if the entry corresponds to a V2 only syntax
    // i.e, if it is SnmpCounter64.<something>
    //
    static public boolean isV2Syntax(String entry) {
	return getTypeName(entry).endsWith("64");
    }
}
  
