/*
 * @(#)file      MetaTableGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.25
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
 *
 */
package com.sun.jdmk.tools.mibgen;

import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * The class generates code required for representing a group as a m-bean
 *
 */
public class MetaTableGenerator extends BeanGenerator 
    implements Serializable {
  
    public static String buildMetaName(Context ctxt, String pre, 
				       String name) {
 	if (ctxt.gentype == MetaBeanGenerator.GENERIC_META)
	    return pre + name + ctxt.genericPrefix + Def.TABLEMETA;
 	if (ctxt.gentype == MetaBeanGenerator.STANDARD_META)
	    return pre + name + ctxt.standardPrefix + Def.TABLEMETA;
        return pre + name + Def.METAPREFIX;
    }

    public MetaTableGenerator(ResourceManager mgr, MibNode table, 
			      Context ctxt) 
	throws IOException {
        
        super(mgr, table, ctxt);
     
	gentype = ctxt.gentype;

        // Get the symbol name associated to the table
        //
        varName     = node.getSymbolName();
        realName    = node.getRealSymbolName();
        oid         = node.getOid();
        symboleName = buildMetaName(ctxt,ctxt.prefix,varName);
    }

    public void generateCode() throws IOException  {

	// Generates Entry code
	//
	processEntry();

	// Generates table code
	//
	processTable();
    }

    protected void processEntry() throws IOException {

        // Get the node containing the entry ...
        //
        Hashtable children= node.getChildren();
        if (children.size() != 1) {
            // something wrong somewhere ...
            //
            Trace.error(MessageHandler.getMessage(
               "generate.error.table.entry", realName));
            throw new IOException();
        }
    
        // well it seems to be the only way to go through this hashtable ...
        //
        Enumeration a= children.elements();
        entryNode= (MibNode) a.nextElement();
	entryName = MbeanGenerator.getNodeSymbolName(context, entryNode);

        // Start code generation. Create a Bean generator for handling code
        // generation for the m-bean.
        //
        entryGen= new MetaEntryGenerator(manager, entryNode, context);
    
        // Process each single element contain in the group
        //
        for(Enumeration e= (entryNode.getChildren()).elements(); 
	    e.hasMoreElements(); ) {
            MibNode var= (MibNode) e.nextElement();
            entryGen.handleNode(var);
        }
        entryGen.endOfGroup();
    }

    protected void processTable() throws IOException  {
        // Open the file which will represent the M-bean.
        //
        out= openFile(symboleName+ Def.JAVA);
     
	try {
	    // Write generic header ...
	    //
	    writeHeader();
     
	    // write our own header ...
	    //
	    writeClassDeclaration();
	    
	    // Generates code for methods
	    //
	    buildMethods();

	    // Write the buffers in the file
	    //
	    writeBuffers();

	} catch (RuntimeException x) {
	    closeIO();
	    throw x;
	} catch (IOException x) {
	    closeIO();
	    throw x;
	} finally {
	    closeIO();
	}
    }

    protected void buildRowStatus() throws IOException {
	if (entryGen == null) 
	    throw new NullPointerException("Can't find entry generator");
	if (entryGen.implementsRowStatus()) {
	    final String rsid   = entryGen.getRowStatusId();
	    final String rsname = entryGen.getRowStatusName();
	    generateRowStatus(rsid,rsname,table_impl);
	} else {
	    generateRowStatus();
	}
    }

    protected void genMethodCall(String tab, String call, String res,
				 StringBuffer result)
	throws IOException {
        // Now that we have the entry, update the node in the metadata 
	// instance ...
        //
	String meth    = null;

	if (isStandard()) {
	    result.append(tab + entryName + Def.MBEANSUFFIX + 
			  " entry = (" + entryName + Def.MBEANSUFFIX + 
			  ") " + Def.GET_ENTRY + "(rowOid)"+ 
			  Def.SEMICOLON);
	    meth = Def.SET_MOI;
	}

	if (isGeneric()) {
	    result.append(tab + "ObjectName entry = " +
			  Def.GET_ENTRYNAME + "(rowOid)"+ 
			  Def.SEMICOLON);
	    meth = Def.SET_OBJNAME;
	}
			   
	result.append(tab + Def.SYNCHRONIZE + "(this)" + 
		      Def.LBRACE);
	
	if (meth != null) {
	    result.append(tab + Def.TAB + NODE + "." + meth + "(entry)" +
			       Def.SEMICOLON) ;
	}
	result.append(tab + Def.TAB + res + NODE + "." + call +  
		      Def.SEMICOLON + tab + Def.RBRACE);
    }

    protected void genCatchedMethodCall(String tab, String call, String res,
					StringBuffer result)
	throws IOException {
        // Now that we have the entry, update the node in the metadata 
	// instance ...
        //
	String meth    = null;

	if (isStandard()) {
	    result.append(tab + entryName + Def.MBEANSUFFIX + 
			  " entry = null" + Def.SEMICOLON); 
	    result.append(tab + "try " + Def.LBRACE);
	    result.append(tab + Def.TAB + "entry = (" + entryName + 
			  Def.MBEANSUFFIX + ") " + Def.GET_ENTRY + 
			  "(rowOid)" + Def.SEMICOLON);
	    result.append(tab + Def.N_RBRACE + " catch (" + Def.EXCP_SNMP
			  + " x) " + Def.LBRACE);
	    result.append(tab + Def.TAB + "entry = null" + Def.SEMICOLON); 
	    result.append(tab + Def.RBRACE);
	    meth = Def.SET_MOI;
	}

	if (isGeneric()) {
	    result.append(tab + "ObjectName entry = null" + Def.SEMICOLON); 
	    result.append(tab + "try " + Def.LBRACE);
	    result.append(tab + Def.TAB + "entry = " + Def.GET_ENTRYNAME + 
			  "(rowOid)" + Def.SEMICOLON);
	    result.append(tab + Def.N_RBRACE + " catch (" + Def.EXCP_SNMP
			  + " x) " + Def.LBRACE);
	    result.append(tab + Def.TAB + "entry = null" + Def.SEMICOLON); 
	    result.append(tab + Def.RBRACE);
	    meth = Def.SET_OBJNAME;
	}
			   
	result.append(tab + Def.SYNCHRONIZE + "(this)" + 
		      Def.LBRACE);
	
	if (meth != null) {
	    result.append(tab + Def.TAB + NODE + "." + meth + "(entry)" +
			       Def.SEMICOLON) ;
	}
	result.append(tab + Def.TAB + res + NODE + "." + call +  
		      Def.SEMICOLON + tab + Def.RBRACE);
    }

    protected void generateRowStatus() throws IOException {
    }

    protected void generateRowStatus(String rowStatusId, 
				     String rowStatusName, 
				     StringBuffer result)
	throws IOException {
	// isRowStatus()
	//
	genAbstractMethodComment(Def.METH_T_ISRS, Def.SNMP_TABLE, result);
	result.append("\n" + Def.TAB + Def.PUBLIC + " boolean " + 
		      Def.METH_T_ISRS + "(" + Def.SNMP_OID + " rowOid, " +
		      "long var, " + Def.SNMP_USERDATA + " userData) " + 
		      Def.LBRACE);
	result.append(Def.TAB2 + Def.RETURN + "(var == " + rowStatusId + 
		      ")" + Def.SEMICOLON);
	result.append(Def.TAB + Def.RBRACE + "\n");
	
	// hasRowStatus()
	//
	genAbstractMethodComment(Def.METH_T_HASRS, Def.SNMP_TABLE, result);
	result.append("\n" + Def.TAB + Def.PUBLIC + " boolean " + 
		      Def.METH_T_HASRS + "() " + 
		      Def.LBRACE);
	result.append(Def.TAB2 + Def.RETURN + "true" + Def.SEMICOLON);
	result.append(Def.TAB + Def.RBRACE + "\n");
	
	// setRowStatus()
	//
	genAbstractMethodComment(Def.METH_T_SETRS, Def.SNMP_TABLE, result);
	result.append("\n" + Def.TAB + Def.PUBLIC + " SnmpValue " + 
		      Def.METH_T_SETRS + "(" + Def.SNMP_OID + " rowOid, " +
		      "int status, " + Def.SNMP_USERDATA + " userData)\n" + 
		      Def.TAB3 + Def.THROWS + Def.EXCP_SNMP +
		      Def.LBRACE);
	genMethodCall(Def.TAB2,Def.METH_T_SETRS + 
		      "(status, userData)", "return ", result);
	result.append(Def.TAB + Def.RBRACE + "\n");
	
	// isRowReady()
	//
	genAbstractMethodComment(Def.METH_T_ISROWREADY, Def.SNMP_TABLE, 
				 result);
	result.append("\n" + Def.TAB + Def.PUBLIC + " boolean " + 
		      Def.METH_T_ISROWREADY + "(" + Def.SNMP_OID + 
		      " rowOid, " + Def.SNMP_USERDATA + " userData)\n" + 
		      Def.TAB3 + Def.THROWS + Def.EXCP_SNMP +
		      Def.LBRACE);
	genMethodCall(Def.TAB2,Def.METH_T_ISROWREADY + 
		      "(userData)", "return ", result);
	result.append(Def.TAB + Def.RBRACE + "\n");
	
	// mapRowStatus()
	//
	genAbstractMethodComment(Def.METH_T_MAPRSVALUE, Def.SNMP_TABLE, 
				 result);
	result.append("\n" + Def.TAB + Def.PUBLIC + " int " + 
		      Def.METH_T_MAPRSVALUE + "(" + Def.SNMP_OID + 
		      " rowOid, " + Def.SNMP_VARBIND + " vbstatus, " + 
		      Def.SNMP_USERDATA + " userData)\n" + 
		      Def.TAB3 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);
	genCatchedMethodCall(Def.TAB2,Def.METH_T_MAPRSVALUE + 
		      "(vbstatus, userData)", "return ", result);
	result.append(Def.TAB + Def.RBRACE + "\n");
	
	// getRowStatus()
	//
	result.append("\n" + Def.TAB + Def.PUBLIC + " int " + 
		      Def.METH_T_GETRS + "(" + Def.SNMP_OID + 
		      " rowOid, " + 
		      Def.SNMP_USERDATA + " userData)\n" + 
		      Def.TAB3 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);
	genMethodCall(Def.TAB2,Def.METH_T_GETRS + 
		      "(userData)", "return ", result);
	result.append(Def.TAB + Def.RBRACE + "\n");
	
    }

    protected void buildMethods() throws IOException {
        // Build create stuff ...
        //
        buildCreateReqHeader();

        // Index management
        //
        processIndex();
          
        // Table management
        //
        buildTable();
     
	// RowStatus
	//
	buildRowStatus();

        // Take into account external indexes that the processing of the
        // table might have discovered
        //
        // updateEntryWithExternalIndex();
     
        // Constructor
        //
        buildConstructorHeader();

        // Build the getter ...
        //
        buildGetReqHeader();
     
        // Build the setter ...
        //
	buildSetReqHeader();
     
        // Build the get next stuff ...
        //
        buildCheckReqHeader();

	buildValidateVarHeader();
	buildIsReadableVarHeader();
	buildGetNextVarIdHeader();
	buildSkipEntryVarHeader();
	generateMetaFactory(factory_impl);
    }

    protected void writeBuffers() throws IOException {
        // Write all the generated stuff ...
        //

        write(constructor.toString());
	write(factory_impl.toString());
        write(create_req_impl.toString());
        write(table_impl.toString());
    
        write(getReq_impl.toString());
        write(setReq_impl.toString());
        write(checkReq_impl.toString());

        write(validateVar_impl.toString());
        write(readableVar_impl.toString());
        write(getNextVarId_impl.toString());
        write(skipEntryVar_impl.toString());

        write(var_list.toString());
     
        write("\n" + Def.RBRACE);
    }

    public void setContext(Context ctxt) {
	super.setContext(ctxt);
	gentype = ctxt.gentype;
    }


    boolean isStandard() {
	return ((gentype & MetaBeanGenerator.STANDARD_META) != 0);
    }

    boolean isGeneric() {
	return ((gentype & MetaBeanGenerator.GENERIC_META) != 0);
    }

    public String getTableClassName() {
        return symboleName + " ";
    }
  
    public String getSymbolName() {
        return varName;
    }
  
    public String getEntryName() {
        return entryName;
    }
  
    protected void writeClassDeclaration() throws IOException {
        // Add some comments
        //
        write("/**\n" +
              " * " + MessageHandler.getMessage(
	              "generate.mbean.comment.desc", varName) + "\n" +
              " * " + MessageHandler.getMessage(
                      "generate.mbean.comment.oid", oid) + "\n" +
              " */\n");
        write(Def.PUBLIC + Def.CLASS + symboleName + Def.EXTEND + 
	      Def.SNMP_TABLE + Def.IMPLEMENT + Def.SERIALIZABLE + 
	      Def.LBRACE + "\n");
    }
    
    protected  void writeHeader() throws IOException {
        writePkg();
     
        // import the java.io package as everything needs to be serializable
        // import the java.util package
        //
        write("// java imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SERIALIZABLE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_VECTOR + Def.SEMICOLON);
     
        // import the JMX SNMP package                        
        //
        write("\n// jmx imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_MBEAN_SERVER + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_OBJECT_NAME + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_COUNTER + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_COUNTER64 + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_GAUGE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_INT + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_UINT + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_IP_ADDR + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_TIME_TICKS + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_OPAQUE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_STRING + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_STRING_FIXED + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_OID + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_NULL + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_VALUE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_VARBIND + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_STATUS_EXCEPTION + Def.SEMICOLON);
        // NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        if (SyntaxMapper.useUnsigned)
            write(Def.IMPORT + Def.PKG_UNSIGNEDLONG + Def.SEMICOLON);
        // end of NPCTE fix for bugId 4692891
        // import the Java DMK SNMP package                        
        //
        write("\n// jdmk imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SNMP_INDEX + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_MIB + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_MIB_TABLE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_SUBREQ + Def.SEMICOLON);	
	String[] intf = getInterfacePkgs();
	if (intf != null) {
	    for (int i=0; i < intf.length ; i++) {
		write(Def.IMPORT + intf[i] + Def.SEMICOLON);
	    }
	}

        write("\n");
    }
    
    protected String getObjectServerClass() {
	if (isStandard())
	    return Def.SNMP_STANDARD_OSRV;
	if (isGeneric())
	    return Def.SNMP_GENERIC_OSRV;
	return "Object";
    }

    protected String[] getInterfacePkgs() {
	int count = 0;
	if (isStandard()) count += 1;
	if (isGeneric()) count += 1;
	if (count == 0) return null;
	String[] result =  new String[count];
	if (isGeneric()) {
	    result[--count] = Def.PKG_SNMP_GENERIC_OSRV;
	}
	if (isStandard()) {
	    result[--count] = Def.PKG_SNMP_STANDARD_OSRV;
	}
	return result;
    }

    // PRIVATE METHODS
    //----------------
  
    private void buildTable() throws IOException {
        // Generate code for returning isObjectNameRequired
	// ------------------------------------------------
	table_impl.append("\n");
	genAbstractMethodComment(Def.METH_T_ISNAMEREQ, Def.SNMP_TABLE,
				 table_impl);
        table_impl.append(Def.TAB + Def.PUBLIC + 
			  Def.BOOLEAN + Def.METH_T_ISNAMEREQ + "() " +
			  Def.LBRACE);
	if (isGeneric()) {
	    table_impl.append(Def.TAB2 + Def.RETURN + "true" + Def.SEMICOLON); 
	} else {
	    table_impl.append(Def.TAB2 + Def.RETURN + "false" + 
			      Def.SEMICOLON); 
	}
        table_impl.append(Def.TAB + Def.RBRACE + "\n\n");
	
	// public void registerEntryNode(SnmpMib mib, MBeanServer server)
	//
        // Create meta data for entry.
        //
	String nodeType =  
	    MetaBeanGenerator.buildMetaName(context, "", entryName);
	table_impl.append("\n");
        table_impl.append(Def.TAB + Def.PUBLIC + 
			  Def.VOID + Def.METH_T_REGENTRY + "(" + 
			  Def.SNMP_MIB + " mib, " + Def.MBEANSERVER + 
			  " server) " +
			  Def.LBRACE);
	table_impl.append(Def.TAB2 + NODE + " = " + 
			  getMetaFactoryName(nodeType) + "(" +
			  "\"" + entryName + "\", \"" + varName + 
			  "\", mib, server)" +
			  Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n");


	genAbstractMethodComment(Def.METH_T_ADD, Def.SNMP_TABLE,
				 table_impl);
        // Generate code for adding elements in the table
	// ----------------------------------------------
	//
        // public synchronized void addEntry(SnmpOid rowOid, 
	//              ObjectName objname, Object entry) 
	//     throws SnmpStatusException { ... }
        table_impl.append(Def.TAB + Def.PUBLIC + Def.SYNCHRONIZE + 
			  Def.VOID + Def.METH_T_ADD +
                          "(" + Def.SNMP_OID + " rowOid, " +
			  Def.OBJECT_NAME + " objname,\n" + 
			  Def.TAB4 + " Object entry)\n" +
                          Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);

        // Now we have to add the element in the table
        // { ...
	//    addEntry(rowOid, objname, entry, true);
	// }
	if (isGeneric()) {

	    table_impl.append(Def.TAB2 + Def.METH_T_ADD + 
			      "(rowOid, objname, entry, true)" + 
			      Def.SEMICOLON);
	} else {
	    String stdbeanclass = 
		TableBeanGenerator.getEntryInterfaceName(context, entryName);
	    table_impl.append(Def.TAB2 + "if (! (entry instanceof " + 
			      stdbeanclass + ") )\n");
	    table_impl.append(Def.TAB3 + "throw new ClassCastException(" +
			      "\"Entries for Table \\\"\" + \n");
	    table_impl.append(Def.TAB3 + "               " + "\"" + varName + 
			      "\" + \"\\\" must implement the \\\"\" + \n");
	    table_impl.append(Def.TAB3 + "               " + "\"" + 
			      stdbeanclass + "\" + \"\\\" interface.\")" 
			      + Def.SEMICOLON); 
	    table_impl.append(Def.TAB2 + "super." + Def.METH_T_ADD + 
			      "(rowOid, objname, entry)" + 
			      Def.SEMICOLON);
	}

        table_impl.append(Def.TAB + Def.RBRACE + "\n");

	if (isGeneric()) {

	    // Generate code for adding elements in the table
	    // ----------------------------------------------
	    //
	    // public synchronized void addEntry(SnmpOid rowOid, 
	    //              ObjectName objname, Object entry, 
	    //              boolean registerFlag) 
	    //     throws SnmpStatusException { ... }
	    //	
	    table_impl.append(Def.TAB + Def.PUBLIC + Def.SYNCHRONIZE + 
			      Def.VOID + Def.METH_T_ADD +
			      "(" + Def.SNMP_OID + " rowOid," + 
			      Def.OBJECT_NAME + 
			      " objname," + " Object entry," + 
			      " boolean registerFlag)\n" +
			      Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
			      Def.LBRACE);
    
	    //    if (objname == null)
	    //        throw new SnmpStatusException(<inconsistentName>);
	    //
	    table_impl.append(Def.TAB2 + "if (" + "objname == null" + ")\n");
	    table_impl.append(Def.TAB3 + Def.THROW_NEW + Def.EXCP_SNMP + 
			      "(" + Def.EXCP_SNMP + "." + 
			      Def.V_INCONSISTENTNAME + ")" + 
			      Def.SEMICOLON);

	    // Now we register the entry
	    //
	    // try { 
	    //    if (entry!= null && registerFlag)
	    //        objectserver.registerTableEntry(this,rowOid,
	    //                     objname, entry);
	    // } ...
	    //
	    table_impl.append(Def.TAB2 + "if (" + "entry != null" + " && " +
			      "registerFlag" + ")" + 
			      Def.LBRACE);
	    table_impl.append(Def.TAB3 + OBJSRV + ".registerTableEntry" + 
			      "(this, rowOid, objname, entry)" + 
			      Def.SEMICOLON + Def.TAB3 + Def.RBRACE); 

	    // Now we have to add the element in the table
	    //   super.addEntry(rowOid, objname, entry);
	    // }
	    table_impl.append(Def.TAB2 + "super." + Def.METH_T_ADD + 
			      "(rowOid, objname, entry)" + 
			      Def.SEMICOLON);   
	    table_impl.append(Def.TAB + Def.RBRACE + "\n");
	    
	}

//      // Add the  code for removing an entry ...
//      //
//      table_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.METH_T_REM + 
//                        "(Object entry, " + Def.OBJECT_NAME + " objname)" +
// 			  Def.LBRACE); 
// 	if (isGeneric()) {
// 	    table_impl.append(Def.TAB2 + Def.METH_T_REMNAME + 
// 			      "( objname, entry ) " +  Def.SEMICOLON);
// 	}
// 	if (isStandard()) {
// 	    table_impl.append(Def.TAB2 + Def.METH_T_REM + 
// 			      "( (Object) entry) " +  Def.SEMICOLON);
// 	}
// 	table_impl.append(Def.TAB + Def.RBRACE + "\n");
    }
    
    private void processIndex() throws IOException {
  
        // First generate the method for getting the index from an
	// entry object
        //
        getIndexFromEntry(null);
   
    }
  
    // The method generates the code for extracting the index from an entry 
    // object
    //
    private void getIndexFromEntry(MibNode ref) throws IOException {
        // Access the node containing the list of indexes ...
        //
        ASTObjectTypeDefinition definition;
        if (ref != null)
            definition= ref.getObjectType();
        else
            definition= entryNode.getObjectType();
    
        if (definition.getDefinition() instanceof ASTObjectTypeDefinitionV1) {
            // we are dealing with V1 table definition
            //
            getV1IndexFromEntry((ASTObjectTypeDefinitionV1)
				definition.getDefinition(), ref);
        } else {
            // we are dealing with V2 table definition
            //
            getV2IndexFromEntry((ASTObjectTypeDefinitionV2)
				definition.getDefinition(), ref);
            return;
        }
    }
    private void getV1IndexFromEntry(ASTObjectTypeDefinitionV1 definition, 
				     MibNode ref) 
	throws IOException {
        processIndexes(definition.getIndex(), ref);
    }
 
    // zzzz XXXXX ZZZZZ xxxxxx
    // => need to check removeEntry()
    // => add a method to validate that an OID can be built into an
    //    index?
    // suppress  SnmpIndex wherever it appears.
    // 
    private void processIndexes(Node indexes, MibNode ref) 
	throws IOException {
    
        // It is possible to get a mib where there is no index defined for a
	// Table ...
        //
        if (indexes == null) {
            Trace.error(MessageHandler.getMessage(
               "generate.error.table.noIndex", realName));
            throw new IOException();
        }
  
        // Get the list of index ...
        //
        int nbIndexes= indexes.jjtGetNumChildren();
  
        
        // Create and update a vector of String objects containing the 
	// index names.
        // This names are used here after to generate a default ObjectName 
	// for the entry.
        //
        String[] indexObjectNames = new String[nbIndexes];
		      
        for (int i=0; i <nbIndexes; i++) {
            ASTIdentifier id= (ASTIdentifier) indexes.jjtGetChild(i);
            String key = id.getName();
      
            // Retrieve the node ...
            //
            MibNode indexNode;
            if (ref == null)
                indexNode= entryNode.getChildWithName(key);
            else 
                indexNode= ref.getChildWithName(key);
      
            if (indexNode == null) {
                // What shall we do ? The index is not part of entry ...
                // Try to retrieve the index.
                //
                indexNode= (mib.getModuleHandler()).findNodeWithName(key);
                if (indexNode == null) {
                    Trace.error(MessageHandler.getMessage(
		       "generate.error.table.index", key, realName));
                    throw new IOException();
                }
                node.addExternalIndex(indexNode);  
            }
            if (ref != null)
                // register any way ...
                //
                node.addExternalIndex(indexNode);
      
            ASTObjectTypeDefinition def= indexNode.getObjectType();
            String keyName= indexNode.getSymbolName();
            ASTNamedType syntaxObject=def.getSyntax();
            String baseSyntaxEntry= syntaxObject.getSnmpSyntax();
            String snmpSyntax= SyntaxMapper.getTypeName(baseSyntaxEntry);
            
            // Update the index ObjectNames array to generate the entry 
	    // ObjectName.
            //
            indexObjectNames[i] = keyName;
            
        }

        // Create the index ...
        //
    
        // The default ObjectName for this entry is:
        //      TableName:class=EntryName,index1Name=index1Value,...
        //
        String dot;
        if (packageName.length() != 0) 
            dot= ".";
        else
            dot= "";
        String entryObjectName = "\"" + symboleName + ":name=" + 
	    packageName + dot + entryName + "\"";
	String buildObjectName = "\"" + symboleName + ":name=" + 
	    packageName + dot + entryName + "\"";
        for (int j=0; j < nbIndexes; j++) {
            entryObjectName = entryObjectName + " + \"," + 
		indexObjectNames[j] + "=\" + " + "entry." + 
		indexObjectNames[j];
            buildObjectName = buildObjectName + " + \"," + 
		indexObjectNames[j] + "=\" + " + "_key" + indexObjectNames[j];
        }

    }
    
    private void getV2IndexFromEntry(ASTObjectTypeDefinitionV2 definition, 
				     MibNode ref) 
	throws IOException {
    
        // Get the list of index ...
        //
        ASTIndexParts indexParts= (ASTIndexParts) definition.getIndex();
        if (indexParts.type == ParserConstants.AUGMENTS) {
            //Trace.info("AUGMENT V2 tables are not supported yet");
            //Trace.info("Need to augment with " + 
	    //           indexParts.getAugmentIdentifier());
            String reference  = indexParts.getAugmentIdentifier();
            MibNode indexNode = 
		(mib.getModuleHandler()).findNodeWithName(reference);
            if (indexNode == null) {
                Trace.error(MessageHandler.getMessage(
		   "generate.error.table.index", reference, realName));
                throw new IOException();
            }

            getIndexFromEntry(indexNode);
	
            //indexParts.dump("------>");
            return;
        }
        ASTIndexTypesV2 v2Index= (ASTIndexTypesV2) indexParts.jjtGetChild(0);
        processIndexes((Node)v2Index, ref);
    }
  
      
    public void generateFactoryComments(String entryName, String className,
					String varName, String type, 
					StringBuffer result) {
	result.append("\n" + Def.TAB + "/**");
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.entry.abstract." + type, entryName));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.text1." + type));
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.text2." + type));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.entry.param.name", entryName));
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.entry.param.tablename",varName));
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.entry.param.mib"));
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.entry.param.server"));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage(
           "generate.mib.comment.factory.return1."+type));
	result.append("\n" + Def.TAB + " * " +  MessageHandler.getMessage(
	   "generate.mib.comment.factory.entry.return2."+type, entryName, 
	   className));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " **/" + "\n");
    }

    protected String getMetaFactoryName(String metaName) {
	return new String("create" + metaName + "Node");
    }

    protected void createMeta(String metaName, StringBuffer result) {
	result.append( Def.TAB2 +
		      Def.RETURN + "new " + metaName + 
		      "(mib, " + OBJSRV + ")" + 
		      Def.SEMICOLON);
    }

    protected void generateMetaFactory(StringBuffer result) {
	String metaName = 
	    MetaBeanGenerator.buildMetaName(context, "", entryName);
	generateFactoryComments(entryName,metaName,varName,"meta",result);
	String metafactory = getMetaFactoryName(metaName);
	result.append(Def.TAB + Def.PROTECTED + metaName + " " + 
		      metafactory + "(String snmpEntryName, " + 
		      "String tableName, " + Def.SNMP_MIB + 
		      " mib, MBeanServer server) " 
		      + Def.LBRACE);
	createMeta(metaName,result);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }  

    protected void buildConstructorHeader() throws IOException {
        constructor.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
          MessageHandler.getMessage("generate.meta.comment.table.constr", 
				     symboleName) + "\n" + Def.TAB + " * " + 
	  MessageHandler.getMessage("generate.meta.comment.table.registration")
			   + "\n" + Def.TAB + " */\n");
        constructor.append(Def.TAB + Def.PUBLIC + symboleName + 
			    "(SnmpMib myMib, " + getObjectServerClass() + 
			    " objserv)" + 
			    Def.LBRACE );
    
        // Create meta data for entry.
        //
	String nodeType =  
	    MetaBeanGenerator.buildMetaName(context, "", entryName);
        constructor.append(Def.TAB2 + "super(myMib)" + Def.SEMICOLON + 
			    Def.TAB2 + OBJSRV + " = objserv" +
			    Def.SEMICOLON);
	constructor.append(Def.TAB + Def.RBRACE + "\n");

    
        // Add definition of variable
        //
        var_list.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
           MessageHandler.getMessage("generate.meta.comment.table.var") + 
			"\n" + Def.TAB + " */\n" +
                        Def.TAB + Def.PRIVATE + nodeType + " " + NODE + 
			Def.SEMICOLON + "\n");
        var_list.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
           MessageHandler.getMessage("generate.meta.comment.table.objserver")+
			"\n" + Def.TAB + " */\n" +
                        Def.TAB + Def.PROTECTED + 
			getObjectServerClass() + 
			" " + OBJSRV + Def.SEMICOLON);
    }
    
    protected void buildValidateVarHeader() throws IOException {
        validateVar_impl.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
	   MessageHandler.getMessage("generate.meta.comment.validatevarid",
				     "var") + "\n" + Def.TAB + " */\n");
        validateVar_impl.append(Def.TAB + Def.PUBLIC + Def.VOID +
				Def.METH_T_VALIDATE + "( " + Def.SNMP_OID +
				" rowOid, long var, " + 
				Def.SNMP_USERDATA + " data )\n" +
				Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
				Def.LBRACE);
    
        // Now that we have the entry, update the node in the metadata 
	// instance ...
        //
	validateVar_impl.append(Def.TAB2 + NODE + ".validateVarId(var, data)" +
				Def.SEMICOLON + Def.TAB + Def.RBRACE);
    }

    protected void buildIsReadableVarHeader() throws IOException {
        readableVar_impl.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
	   MessageHandler.getMessage("generate.meta.comment.isreadable",
				     "var") + "\n" + Def.TAB + " */\n");
        readableVar_impl.append(Def.TAB + Def.PUBLIC + Def.BOOLEAN +
				Def.METH_T_READABLE + "( " + Def.SNMP_OID +
				" rowOid, long var, " + 
				Def.SNMP_USERDATA + " data )\n" +
				Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
				Def.LBRACE);
    
        // Now that we have the entry, update the node in the metadata 
	// instance ...
        //
	readableVar_impl.append(Def.TAB2 + Def.RETURN + NODE + "." +
				Def.METH_IS_READABLE + "(var)" +
				Def.SEMICOLON + Def.TAB + Def.RBRACE);
    }

    protected void buildGetNextVarIdHeader() throws IOException {
        getNextVarId_impl.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
	   MessageHandler.getMessage("generate.meta.comment.getnextvarid",
				     "var") + "\n" + Def.TAB + " */\n");
        getNextVarId_impl.append(Def.TAB + Def.PUBLIC + "long " +
				 Def.METH_T_GETNEXTVARID + "( " +
				 Def.SNMP_OID +	" rowOid, long var, " + 
				 Def.SNMP_USERDATA + " data )\n" +
				 Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
				 Def.LBRACE);
    
	getNextVarId_impl.append(Def.TAB2 + "long nextvar = "  +
				 NODE + ".getNextVarId(var, data)" +
				 Def.SEMICOLON);
	getNextVarId_impl.append(Def.TAB2 + "while (!" + Def.METH_T_READABLE + 
				"(rowOid, nextvar, data))\n");
	getNextVarId_impl.append(Def.TAB3 + "nextvar = "  + NODE + 
				 ".getNextVarId(nextvar, data)" + 
				 Def.SEMICOLON);
	getNextVarId_impl.append(Def.TAB2 + Def.RETURN + "nextvar" +
				 Def.SEMICOLON + Def.TAB + Def.RBRACE);
    }

    protected void buildSkipEntryVarHeader() throws IOException {
	genAbstractMethodComment(Def.METH_T_SKIPVARIABLE, Def.SNMP_TABLE,
				 skipEntryVar_impl);
        skipEntryVar_impl.append(Def.TAB + Def.PUBLIC + "boolean " +
				 Def.METH_T_SKIPVARIABLE + "( " +
				 Def.SNMP_OID +	" rowOid, long var, " + 
				 Def.SNMP_USERDATA + 
				 " data, int pduVersion)" + Def.LBRACE);

	// try {
	//   set node
	//   call return node.skipVariable(var, data, pduVersion);
	// } catch (SnmpStatusException x) {
	//   return false;
	// }
	skipEntryVar_impl.append(Def.TAB2 + "try" + Def.LBRACE);
	genMethodCall(Def.TAB3,Def.METH_SKIP_VARIABLE + 
		      "(var, data, pduVersion)", "return ", skipEntryVar_impl);
	skipEntryVar_impl.append(Def.TAB2 + Def.N_RBRACE + " catch (" + 
				 Def.EXCP_SNMP + " x)" + Def.LBRACE + 
				 Def.TAB3 + "return false" +
				 Def.SEMICOLON + Def.TAB2 + Def.RBRACE);
	skipEntryVar_impl.append(Def.TAB + Def.RBRACE + "\n");
    }

    protected void genAbstractMethodComment(String methname,
					    String classname,  
					    StringBuffer result) 
	throws IOException {
	result.append("\n" + Def.TAB + Def.LINE);
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + "// " +
	   MessageHandler.getMessage("generate.mib.comment.implements", 
				     methname, classname));
	result.append("\n" + Def.TAB + "// " + 
	   MessageHandler.getMessage("generate.mib.comment.seedoc", 
				     classname));
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + Def.LINE + "\n\n");
    }

    protected void buildSetReqHeader() throws IOException {
	genAbstractMethodComment(Def.METH_T_SETREQ, Def.SNMP_TABLE,
				 getReq_impl);
        setReq_impl.append(Def.TAB + Def.PUBLIC + Def.VOID +
			   Def.METH_T_SETREQ + "(" + Def.SNMP_SUBREQ + 
			   " req, " + Def.SNMP_OID + " rowOid," + 
			   " int depth" + ")\n" +
			   Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
			   Def.LBRACE);
    
        // Now that we have the entry, update the node in the metadata 
	// instance ...
        //
	setReq_impl.append(Def.TAB2 + "if (req.getSize() == 0) return;\n\n");
	genMethodCall(Def.TAB2,"set(req,depth)","",setReq_impl);
	setReq_impl.append(Def.TAB + Def.RBRACE);
    }

    protected void buildGetReqHeader() throws IOException {
	genAbstractMethodComment(Def.METH_T_GETREQ, Def.SNMP_TABLE,
				 getReq_impl);
        getReq_impl.append(Def.TAB + Def.PUBLIC + Def.VOID +
                        Def.METH_T_GETREQ + "(" + Def.SNMP_SUBREQ + 
			" req, " + Def.SNMP_OID + " rowOid," + " int depth" +
			")\n" + Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
			Def.LBRACE);
    
	genMethodCall(Def.TAB2,"get(req,depth)","",getReq_impl);
	getReq_impl.append(Def.TAB + Def.RBRACE);
    }
  
    protected void buildCheckReqHeader() throws IOException {
	genAbstractMethodComment(Def.METH_T_CHECKREQ, Def.SNMP_TABLE,
				 checkReq_impl);
        checkReq_impl.append(Def.TAB + Def.PUBLIC + Def.VOID +
			     Def.METH_T_CHECKREQ + "(" + Def.SNMP_SUBREQ +
			     " req, " + Def.SNMP_OID + " rowOid," + 
			     " int depth" + ")\n" + Def.TAB2 + Def.THROWS + 
			     Def.EXCP_SNMP + Def.LBRACE);
    
	checkReq_impl.append(Def.TAB2 + "if (req.getSize() == 0) return;\n\n");
	genMethodCall(Def.TAB2,"check(req,depth)","",checkReq_impl);
	checkReq_impl.append(Def.TAB + Def.RBRACE);
    }  
  
    protected void buildCreateReqHeader() throws IOException {
	genAbstractMethodComment(Def.METH_T_CREATE, Def.SNMP_TABLE,
				 create_req_impl);
     
        create_req_impl.append(Def.TAB + Def.PUBLIC + Def.VOID +
			       Def.METH_T_CREATE + "(" + Def.SNMP_SUBREQ + 
			       " req, " + Def.SNMP_OID  + 
			       " rowOid, int depth)\n" +
			       Def.TAB2 + Def.THROWS + Def.EXCP_SNMP 
			       + Def.LBRACE);
	create_req_impl.append(Def.TAB2 + "if (factory != null)\n"); 
	create_req_impl.append(Def.TAB3 + 
			       "factory." + Def.METH_T_CREATE +
			       "(req, rowOid, depth, this)" + 
			       Def.SEMICOLON);
	create_req_impl.append(Def.TAB2 + "else\n");
	create_req_impl.append(Def.TAB3 + Def.THROW_NEW + 
			       Def.EXCP_SNMP + "(\n");
	create_req_impl.append(Def.TAB4 + Def.EXCP_SNMP + "." + 
			       Def.V_NOACCESS + ")" + Def.SEMICOLON);
	create_req_impl.append(Def.TAB + Def.RBRACE + "\n");
    }
  

    // ---------------------------------------------------------------------
    // VARIABLES
    // ---------------------------------------------------------------------
 
    // MetaData type
    //
    protected int gentype = 0;

    /**
     * Variable list
     */
    protected StringBuffer var_list          = new StringBuffer();
    
    /** 
     * Compute index
     */
    protected StringBuffer  table_impl       = new StringBuffer();
  
    /**
     * Getter
     */
    protected StringBuffer getReq_impl       =  new StringBuffer();
  
    /**
     * Get Next stuff
     */
    protected StringBuffer validateVar_impl  =  new StringBuffer();
    protected StringBuffer readableVar_impl  =  new StringBuffer();
    protected StringBuffer getNextVarId_impl =  new StringBuffer();
    protected StringBuffer skipEntryVar_impl =  new StringBuffer();
  
    /*
    ** Setter
    */
    protected StringBuffer setReq_impl       =  new StringBuffer();
  
    /**
     * Checker
     */
    protected StringBuffer checkReq_impl     =  new StringBuffer();
  
    /**
     * Constructor
     */
    protected StringBuffer constructor= new StringBuffer();    
              // Constructor with reference on the MBeanServer
  
    /**
     * Create entry in the table ...
     */
    protected StringBuffer create_req_impl = new StringBuffer();
    protected StringBuffer factory_impl    = new StringBuffer();

    private String entryName= "";
    private String realName= "";
    private MibNode entryNode;
  
    private MetaEntryGenerator entryGen = null;

    public static final String OBJSRV = "objectserver";
    public static final String NODE   = "node";

}

