/*
 * @(#)file      TableBeanGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.44
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
 * The class generates code required for representing a group as a m-bean
 *
 */
public class TableBeanGenerator extends BeanGenerator implements Serializable {
  
    public TableBeanGenerator(ResourceManager mgr, MibNode table, 
			      Context ctxt) throws IOException {
        
        super(mgr, table, ctxt);
     
	gentype = ctxt.gentype;

        // Get the symbol name associated to the table
        //
        varName= node.getSymbolName();
        realName= node.getRealSymbolName();
        oid= node.getOid();
        symboleName= prefix + Def.TABLEPREFIX + varName;
     
        processEntry();
     
        // Open the file which will represent the M-bean.
        //
        out= openFile(symboleName+ Def.JAVA);
     
        // Write generic header ...
        //
        writeHeader();
     
        // write our own header ...
        //
        writeClassDeclaration();
     
        // Build create stuff ...
        //
        // buildCreateReqHeader();
        buildCreateHeader();
	buildBuildNameHeader();

        // Index management
        //
        processIndex(ctxt);
          
        // Table management
        //
        processTable();
     
        // Take into account external indexes that the processing of the
        // table might have discovered
        //
        updateEntryWithExternalIndex();
     
        // Constructor
        //
        buildConstructorHeader();

	// Set object server
	//
        // buildSetObjSrv();
          
        // Write all the generated stuff ...
        //
        write(constructor1.toString());
        write(constructor2.toString());
        write(create_impl.toString());
	//        write(create_req_impl.toString());
        write(table_impl.toString());
        write(buildname_impl.toString());
        write(build_impl.toString());
        write(oid_impl.toString());
	write(oidval_impl.toString());
        write(index_impl.toString());
	write(factory_impl.toString());

	// write(setobj_impl.toString());
        write(var_list.toString());
     
        write("\n" + Def.RBRACE);
        closeIO();
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
              " * " + MessageHandler.getMessage("generate.mbean.comment.desc", varName) + "\n" +
              " * " + MessageHandler.getMessage("generate.mbean.comment.oid", oid) + "\n" +
              " */\n");
        write(Def.PUBLIC + Def.CLASS + symboleName + Def.EXTEND +
	      Def.SNMP_TABLE_SUPPORT + Def.IMPLEMENT + Def.SERIALIZABLE +
	      Def.LBRACE + "\n");
    }
    
    protected void writeVersion() throws IOException {
	String msgid = "generate.version";
	
	write("\n//\n// " + 
	      MessageHandler.getMessage(msgid, mib.getModuleName()) +
              "\n//\n\n");
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
    
    protected void genAbstractMethodComment(String methname,
					    String classname,  
					    StringBuffer result) 
	throws IOException {
	result.append("\n" + Def.TAB + Def.LINE);
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + "// " + MessageHandler.getMessage("generate.mib.comment.implements", methname, classname));
	result.append("\n" + Def.TAB + "// " + MessageHandler.getMessage("generate.mib.comment.seedoc", classname));
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + Def.LINE + "\n\n");
    }

    // Sets the object server and instantiate the entry meta-node.
    //
//     protected void buildSetObjSrv() {
//         setobj_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
// 			   MessageHandler.getMessage("generate.meta.comment.table.setobjsrv") + "\n" +
// 			   Def.TAB + " */\n");
//         setobj_impl.append(Def.TAB + Def.PUBLIC + "void " +
// 			   Def.METH_T_SETOBJSRV +
// 			   "( " + getObjectServerClass() + " objsrv )" +
// 			   Def.LBRACE );
//         String nodeType=  MetaBeanGenerator.buildMetaName(prefix, entryName);
// 	setobj_impl.append(Def.TAB2 + "objectserver = objsrv;" + 
// 			   Def.SEMICOLON + Def.TAB2 + 
// 			   "node = new " +nodeType + "(theMib,objsrv)" + 
// 			   Def.SEMICOLON);
// 	setobj_impl.append(Def.TAB + Def.RBRACE + "\n");
//     }

    protected String[] getInterfacePkgs() {
	int count = 3;
	String[] result =  new String[count];
	result[--count] = Def.PKG_SNMP_TABLE_SUPPORT;
	result[--count] = Def.PKG_SNMP_TABLE_CB;
	result[--count] = Def.PKG_SNMP_ENTRY_FACTORY;
	return result;
    }

    // PRIVATE METHODS
    //----------------
    
    private void cline(String comment, StringBuffer result) 
	throws IOException {
	result.append("\n" + Def.TAB + " * " + comment);
    }

    private void processTable() throws IOException {
	// Generate code for initializing the meta.
	// ----------------------------------------
	//
	table_impl.append("\n");
	genAbstractMethodComment(Def.METH_T_GETTABLEMETA, 
				 Def.SNMP_TABLE_SUPPORT, table_impl);
        table_impl.append(Def.TAB + Def.PROTECTED + 
			  Def.SNMP_TABLE + " " + Def.METH_T_GETTABLEMETA +
                          "(" + Def.SNMP_MIB + " mib) " + Def.LBRACE);
        table_impl.append(Def.TAB2 + Def.RETURN + "mib." + 
			  Def.METH_T_GETTABLEMETA +
			  "(\"" + varName + "\")" + Def.SEMICOLON);
        table_impl.append(Def.TAB + Def.RBRACE + "\n");
	

	genAbstractMethodComment(Def.METH_T_REMENTRYCB, 
				 Def.SNMP_TABLE_SUPPORT, table_impl);
        table_impl.append(Def.TAB + Def.PUBLIC + "void " + 
			  Def.METH_T_REMENTRYCB + "(int pos, " + 
			  Def.SNMP_OID + " row, " + Def.OBJECT_NAME +
			  " name,\n" + Def.TAB4 + "Object entry, " + 
			  Def.SNMP_TABLE +  " meta)\n" + Def.TAB3 +
			  Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);
	table_impl.append(Def.TAB2 + "try " + Def.LBRACE);
	table_impl.append(Def.TAB3 + "super." + Def.METH_T_REMENTRYCB +
			  "(pos,row,name,entry,meta)" + Def.SEMICOLON);
	table_impl.append(Def.TAB3 +"if (server != null && name != null)\n"); 
	table_impl.append(Def.TAB4 + "server.unregisterMBean(name)" +
			  Def.SEMICOLON);
	table_impl.append(Def.TAB2 + Def.N_RBRACE + " catch (Exception x)" +
			  " { }\n");
	table_impl.append(Def.TAB + Def.RBRACE + "\n");
			  

        // Generate code for adding elements in the table.
	// -----------------------------------------------
        //
	table_impl.append("\n" + Def.TAB + "/**");
	cline(MessageHandler.getMessage("generate.table.comment.add1"),
	      table_impl);
	cline("",table_impl);
	cline(MessageHandler.getMessage("generate.table.comment.add2"),
	      table_impl);
	cline(MessageHandler.getMessage("generate.table.comment.add3",
					Def.METH_T_BUILDNAME),table_impl);
	cline("",table_impl);
	cline(MessageHandler.getMessage("generate.table.comment.calls",
					Def.METH_T_ADD, 
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline(MessageHandler.getMessage("generate.mib.comment.seedoc",
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline("",table_impl);
	table_impl.append("\n" + Def.TAB + " **/\n\n");

        table_impl.append(Def.TAB + Def.PUBLIC + Def.SYNCHRONIZE + 
			  Def.VOID + Def.METH_T_ADD +
                          "(" + entryName + Def.MBEANSUFFIX + " entry)\n" +
                          Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);
    
        // the first thing to do is to build the SMNP index ...
        //
        table_impl.append(Def.TAB2 + Def.SNMP_INDEX + " index = " + 
			  Def.BUILD +  Def.SNMP_INDEX +
                          "(entry)" + Def.SEMICOLON);

        // Now we have to add the element in the table
        //
        table_impl.append(Def.TAB2 + "super." + Def.METH_T_ADD + 
			  "(index, (Object) entry)" + 
			  Def.SEMICOLON);
        table_impl.append(Def.TAB + Def.RBRACE + "\n");
    

        // Generate code for adding elements in the table
	// ----------------------------------------
        //
	table_impl.append("\n" + Def.TAB + "/**");
	cline(MessageHandler.getMessage("generate.table.comment.add1"),
	      table_impl);
	cline("",table_impl);
	cline(MessageHandler.getMessage("generate.table.comment.calls",
					Def.METH_T_ADD, 
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline(MessageHandler.getMessage("generate.mib.comment.seedoc",
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline("",table_impl);
	table_impl.append("\n" + Def.TAB + " **/\n\n");

        table_impl.append(Def.TAB + Def.PUBLIC + Def.SYNCHRONIZE + 
			  Def.VOID + Def.METH_T_ADD +
                          "(" + entryName + Def.MBEANSUFFIX + 
			  " entry, ObjectName name)\n" +
                          Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);
    
        // the first thing to do is to build the SMNP index ...
        //
        table_impl.append(Def.TAB2 + Def.SNMP_INDEX + " index = " + 
			  Def.BUILD +  Def.SNMP_INDEX +
                          "(entry)" + Def.SEMICOLON);

        // Now we have to add the element in the table
        //
        table_impl.append(Def.TAB2 + "super." + Def.METH_T_ADD + 
			  "(index, name, (Object) entry)" + 
			  Def.SEMICOLON);   
        table_impl.append(Def.TAB + Def.RBRACE + "\n");
    

        // Add code for getting all the entries ...
	// ----------------------------------------
        //
	table_impl.append("\n" + Def.TAB + "/**");
	cline(MessageHandler.getMessage("generate.table.comment.getentries"),
	      table_impl);
	cline("",table_impl);
	cline(MessageHandler.getMessage("generate.table.comment.calls",
					Def.METH_T_BASIC, 
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline(MessageHandler.getMessage("generate.mib.comment.seedoc",
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline("",table_impl);
	table_impl.append("\n" + Def.TAB + " **/\n\n");


        table_impl.append(Def.TAB + Def.PUBLIC + Def.SYNCHRONIZE + entryName 
			  + Def.MBEANSUFFIX + "[] " + Def.GET_ENTRIES + 
                          "()" + Def.LBRACE + Def.TAB2 +
                          "Object[] array = " + Def.METH_T_BASIC + "()" + 
			  Def.SEMICOLON + Def.TAB2 +
                          entryName + Def.MBEANSUFFIX + "[] result = new " + 
			  entryName + Def.MBEANSUFFIX + "[array.length]" + 
                          Def.SEMICOLON + Def.TAB2 +
                          "java.lang.System.arraycopy(array,0, result," +
			  "0, array.length)" + Def.SEMICOLON + Def.TAB2 +
                          Def.RETURN + "result" + Def.SEMICOLON + Def.TAB  + 
			  Def.RBRACE + "\n");
    
        // Add the  code for removing an entry ...
	// ----------------------------------------
        //
	table_impl.append("\n" + Def.TAB + "/**");
	cline(MessageHandler.getMessage("generate.table.comment.remove"),
	      table_impl);
	cline("",table_impl);
	cline(MessageHandler.getMessage("generate.table.comment.calls",
					Def.METH_T_REM, 
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline(MessageHandler.getMessage("generate.mib.comment.seedoc",
					Def.SNMP_TABLE_SUPPORT),table_impl);
	cline("",table_impl);
	table_impl.append("\n" + Def.TAB + " **/\n\n");

        table_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.METH_T_REM + 
                          "(" + entryName + Def.MBEANSUFFIX + " entry)\n" +
			  Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);
        table_impl.append(Def.TAB2 + Def.SNMP_INDEX + " index = " + 
			  Def.BUILD +  Def.SNMP_INDEX +
                          "(entry)" + Def.SEMICOLON);
	table_impl.append(Def.TAB2 + "super." + Def.METH_T_REM + 
			  "(index, entry) " +  Def.SEMICOLON + Def.TAB  + 
			  Def.RBRACE + "\n");
    
    }
    
    private void processIndex(Context ctxt) throws IOException {
  
        // Header for building index ...
        //
        buildBuildIndexHeader();
        buildBuildOidIndexHeader();
        buildBuildIndexOidHeader();
    
        // First generate the method for getting the index from an entry object
        //
        getIndexFromEntry(ctxt, null);
   
    }
  
    // The method generates the code for extracting the index from an 
    // entry object
    //
    private void getIndexFromEntry(Context ctxt, MibNode ref) 
	throws IOException {
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
            getV1IndexFromEntry(ctxt, (ASTObjectTypeDefinitionV1)definition.getDefinition(), ref);
        } else {
            // we are dealing with V2 table definition
            //
            getV2IndexFromEntry(ctxt, (ASTObjectTypeDefinitionV2)definition.getDefinition(), ref);
            return;
        }
    }

    private void getV1IndexFromEntry(Context ctxt, ASTObjectTypeDefinitionV1 definition, MibNode ref) throws IOException {
        processIndexes(ctxt, definition.getIndex(), ref);
    }
  
    private void processIndexes(Context ctxt, Node indexes, MibNode ref) throws IOException {
    
        // It is possible to get a mib where there is no index defined for a Table ...
        //
        if (indexes == null) {
            Trace.error(MessageHandler.getMessage("generate.error.table.noIndex", realName));
            throw new IOException();
        }
  
        // Get the list of index ...
        //
        int nbIndexes= indexes.jjtGetNumChildren();

        // Allocate array of SNMPoid for storing index ...
        //
        build_impl.append(Def.TAB2 + Def.SNMP_OID + "[] oids = new " + 
                          Def.SNMP_OID + "[" + String.valueOf(nbIndexes) + "]" + Def.SEMICOLON +
                          Def.TAB2 + Def.SNMP_VALUE + " val = null"+ Def.SEMICOLON);
    
        index_impl.append(Def.TAB2 + Def.SNMP_OID + "[] oids = new " + 
                          Def.SNMP_OID + "[" + String.valueOf(nbIndexes) + "]" + Def.SEMICOLON +
                          Def.TAB2 +"int pos = start" + Def.SEMICOLON);
	
        // check the number of element in the index
        //
        oid_impl.append(Def.TAB2 + "if (index.getNbComponents() != " + String.valueOf(nbIndexes) + ")\n" + 
                        Def.TAB3 + Def.THROW_NEW + Def.EXCP_SNMP + "(" + Def.EXCP_SNMP +
                        "." + Def.V_NOSUCHINSTANCE + ")" + Def.SEMICOLON +
                        Def.TAB2 + "try" + Def.LBRACE);
    
        // Get the different component of the vector ...
        //
        oid_impl.append(Def.TAB3 + "Vector v = index.getComponents()" + Def.SEMICOLON);
        

	// Make the index translation in a try catch block.
	//
	oidval_impl.append(Def.TAB2 + "try" + Def.LBRACE);

        // Create and update a vector of String objects containing the index names.
        // This names are used here after to generate a default ObjectName for the entry.
        //
        String[] indexObjectNames = new String[nbIndexes];
	
	StringBuffer fcall        = new StringBuffer();
	String       stdbeanclass = getEntryInterfaceName(ctxt,entryName);
        String       beanclass    = getEntryFactoryResult(ctxt,entryName);

	if (isGeneric()) {
	    fcall.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.gen"));
	    fcall.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.gen",stdbeanclass));
	    fcall.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.gen"));
	    fcall.append("\n" + Def.TAB3 + "//\n");
	} else {
	    fcall.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.std"));
	    fcall.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.std",stdbeanclass));
	    fcall.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.std"));
	    fcall.append("\n" + Def.TAB3 + "//\n");
 	    // fcall.append(Def.TAB3 + "final " + 
	    //           stdbeanclass + 
	    //		 " entry = (" + stdbeanclass + ") \n" + Def.TAB4 + 
	    //		 getEntryFactoryName(ctxt,entryName) + 
	    //		 "(req, rowOid, depth, objname, meta");
	}

	fcall.append(Def.TAB3 + "final " + 
		     beanclass + 
		     " entry =\n " + Def.TAB4 + 
		     getEntryFactoryName(ctxt,entryName) + 
		     "(req, rowOid, depth, objname, meta");

        for (int i=0; i <nbIndexes; i++) {
            ASTIdentifier id= (ASTIdentifier) indexes.jjtGetChild(i);
            String key = id.getName();

            // Retrieve the node ...
            //
            MibNode indexNode;
            if (ref == null) {
		//debug("processIndex[" + i + "] (no ref): " + key);
                indexNode= entryNode.getChildWithName(key);
            } else { 
		//debug("processIndex[" + i + "] (ref): " + key);
                indexNode= ref.getChildWithName(key);
	    }

            if (indexNode == null) {
                // What shall we do ? The index is not part of entry ...
                // Try to retrieve the index.
                //
		ctxt.addExternalSymbol(key);
                indexNode= (mib.getModuleHandler()).findNodeWithName(key);
                if (indexNode == null) {
                    Trace.error(MessageHandler.getMessage("generate.error.table.index", key, realName));
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

	    if (ctxt.getJavaSyntax(keyName) == null) 
		ctxt.setIndexSyntax(keyName, syntaxObject);

            String baseSyntaxEntry= syntaxObject.getSnmpSyntax();
            String snmpSyntax= SyntaxMapper.getTypeName(baseSyntaxEntry);
            
            // Update the index ObjectNames array to generate the entry 
	    // ObjectName.
            //
            indexObjectNames[i] = keyName;

            // This is not impacted by fixed length ...
            //
            create_impl.append(Def.TAB3+ Def.FINAL + Def.SNMP_OID + " oid" + 
                   i + " = (" + Def.SNMP_OID + ") v.elementAt(" + 
                   String.valueOf(i) + ")" + Def.SEMICOLON);      
	    buildname_impl.append(Def.TAB3+ "oid = (" + Def.SNMP_OID + 
                   ") v.elementAt(" + String.valueOf(i) + ")" + Def.SEMICOLON);

            if (indexNode.isEnumeratedType() == false) {
                fcall.append(",\n" + Def.TAB5 + "oid" + i + "." + 
                     SyntaxMapper.getIndexCastMethod(baseSyntaxEntry) + "()");
                buildname_impl.append(Def.TAB3 +"String _key" + keyName + 
		           " = oid." + 
			   SyntaxMapper.getIndexCastMethod(baseSyntaxEntry) +
			   "().toString()" +  Def.SEMICOLON);
            } else {
                String enumeratiom= indexNode.getEnumeratedType();
                fcall.append(",\n" + Def.TAB5 +"new " +
			 enumeratiom + "(oid" + i + "." + 
			 SyntaxMapper.getIndexCastMethod(baseSyntaxEntry) + 
			 "())");
                buildname_impl.append(Def.TAB3 +"String _key" + keyName + 
			 " = new " + enumeratiom + "(oid." + 
                         SyntaxMapper.getIndexCastMethod(baseSyntaxEntry) + 
			 "()).toString()"+ Def.SEMICOLON);
            }
            
	    // NPCTE fix for bug 4406639, SD, 17 Aug 01
	    // we want to process the 'fixed length' case only for strings
            // was: if (indexNode.getFixedLength() == -1) {
	    if ((indexNode.getFixedLength() == -1) || 
		((indexNode.getFixedLength() != -1) && 
	    	(snmpSyntax.compareTo("SnmpString") != 0))) {
		// End NPCTE fix for bug 4406639
                build_impl.append(Def.TAB2 + "val = new " + snmpSyntax + "(");
      
                // if (indexNode.isEnumeratedType()) {
                // Add a casting ...
                //
                //build_impl.append("(" + Def.ENUM_CLASS + ") ");
                //}   
                index_impl.append(Def.TAB2 + "oids[" + String.valueOf(i) + 
				  "] = " + snmpSyntax + "." + 
                                  Def.METH_FROMINDEX + "(index, pos)" + 
				  Def.SEMICOLON);
                if (i < (nbIndexes -1))
                    index_impl.append(Def.TAB2 + "pos = " +snmpSyntax + "." + 
				      Def.METH_NEXTINDDEX +
                                      "(index, pos)" + Def.SEMICOLON );
                oid_impl.append(Def.TAB3 + snmpSyntax + "." + 
				Def.METH_APPENDTOOID + "((" + Def.SNMP_OID +
                                ")v.elementAt(" + String.valueOf(i) + 
				"), oid)" + Def.SEMICOLON);
		oidval_impl.append(Def.TAB3 + snmpSyntax + "." + 
				   Def.METH_APPENDTOOID + "(new " + snmpSyntax + "(a" + keyName + ")." + Def.METH_TOOID +"(), oid)" + Def.SEMICOLON);
		
            } else {
                // We are dealing with a fixed length index ...
                //
                String length= String.valueOf(indexNode.getFixedLength());
                build_impl.append(Def.TAB2 + "val = new " + snmpSyntax + 
				  Def.FIXED + "(" + length + " ," );
	
                index_impl.append(Def.TAB2 + "oids[" + String.valueOf(i) + 
				  "] = " + snmpSyntax + Def.FIXED + "." + 
                                  Def.METH_FROMINDEX + "(" + length + 
				  ", index, pos)" + Def.SEMICOLON);
                if (i < (nbIndexes -1))
                    index_impl.append(Def.TAB2 + "pos = " +snmpSyntax + 
				      Def.FIXED + "." + Def.METH_NEXTINDDEX +
                                      "(" + length + ", index, pos)" + 
				      Def.SEMICOLON );
	
                oid_impl.append(Def.TAB3 + snmpSyntax + Def.FIXED + "." + 
				Def.METH_APPENDTOOID + "(" + length +
                                ", (" + Def.SNMP_OID +
                                ")v.elementAt(" + String.valueOf(i) +
				"), oid)" + Def.SEMICOLON);

		oidval_impl.append(Def.TAB3 + snmpSyntax + Def.FIXED + "." + 
				   Def.METH_APPENDTOOID + "(" + length + ", new " + snmpSyntax + "(a" + keyName + ")." + Def.METH_TOOID +"(), oid)" + Def.SEMICOLON);

            }
	    
            build_impl.append("entry." + Def.GET + keyName + "())" + 
			      Def.SEMICOLON);
            build_impl.append(Def.TAB2 + "oids[" + String.valueOf(i) + 
			      "] = val." + Def.METH_TOOID + 
                              "()" + Def.SEMICOLON);
        }
        // catch vector expcetion ...
        //
        oid_impl.append(Def.TAB2 + Def.N_RBRACE + 
			" catch(ArrayIndexOutOfBoundsException e)" + 
			Def.LBRACE +
                        Def.TAB3 +  Def.THROW_NEW + Def.EXCP_SNMP + 
			"(" + Def.EXCP_SNMP +
                        "." + Def.V_NOSUCHINSTANCE + ")" + Def.SEMICOLON +
			Def.TAB2 + Def.RBRACE + Def.TAB2 +
                        "return oid" + Def.SEMICOLON + Def.TAB + 
			Def.RBRACE + "\n");
	
	oidval_impl.append(Def.TAB2 + Def.N_RBRACE + 
			   " catch(ArrayIndexOutOfBoundsException e)" + 
			   Def.LBRACE +
			   Def.TAB3 +  Def.THROW_NEW + Def.EXCP_SNMP + 
			   "(" + Def.EXCP_SNMP +
			   "." + Def.V_NOSUCHINSTANCE + ")" + Def.SEMICOLON +
			   Def.TAB2 + Def.RBRACE + Def.TAB2 +
			   "return oid" + Def.SEMICOLON + Def.TAB + 
			   Def.RBRACE + "\n");
        // Create the index ...
        //
        build_impl.append(Def.TAB2 + Def.RETURN + Def.NEW + Def.SNMP_INDEX + 
			  "(oids)" + Def.SEMICOLON +
                          Def.TAB + Def.RBRACE + "\n");
        index_impl.append(Def.TAB2 + Def.RETURN + Def.NEW + Def.SNMP_INDEX + 
			  "(oids)" + Def.SEMICOLON +
                          Def.TAB + Def.RBRACE + "\n");
    
        // The default ObjectName for this entry is:
        //      TableName:class=EntryName,index1Name=index1Value,...
        //
        String dot;
        if (packageName.length() != 0) 
            dot= ".";
        else
            dot= "";
	//        String entryObjectName = "\"" + symboleName + ":name=" + packageName + dot + entryName + "\"";
	String buildObjectName = "\"" + symboleName + ":name=" + packageName + dot + entryName + "\"";
         for (int j=0; j < nbIndexes; j++) {
	    //            entryObjectName = entryObjectName + " + \"," + indexObjectNames[j] + "=\" + " + "entry." + indexObjectNames[j];
            buildObjectName = buildObjectName + " + \"," + indexObjectNames[j] + "=\" + " + "_key" + indexObjectNames[j];
	 }
	 fcall.append(")" + Def.SEMICOLON);
	 // fcall.append(Def.TAB3 + "if ((!meta." + Def.METH_T_ISNAMEREQ + 
	 //              "()) &&\n");
	 // fcall.append(Def.TAB3 + "    (! entry instanceof " + 
         //              stdbeanclass + "))\n");
	 // fcall.append(Def.TAB4 + "throw new ClassCastException(" +
	 //	      "\"Entries for Table \\\"" + varName + 
	 //	      "\\\" must be instances of \\\"" + stdbeanclass + 
	 //	      "\\\".\")" + Def.SEMICOLON); 

	     
	 create_impl.append(Def.TAB3 + "ObjectName objname = null" + 
			    Def.SEMICOLON);
	 create_impl.append(Def.TAB3 + "if (server != null)\n");
	 create_impl.append(Def.TAB4 + "objname = " + Def.METH_T_BUILDNAME + 
			    "( index )" + Def.SEMICOLON);
			   
	 create_impl.append(fcall.toString());

	 // create_impl.append(Def.TAB4 + "objname = new ObjectName(" + 
	 //		   entryObjectName + ")" + Def.SEMICOLON);
	 buildname_impl.append(Def.TAB3 + "return new ObjectName("+ 
			       buildObjectName + ")" + Def.SEMICOLON);
	 
	 // Register the entry into the MBeanServer
	 create_impl.append(Def.TAB3 + "if (server != null)" + Def.LBRACE);
	 create_impl.append(Def.TAB4 + "server.registerMBean(entry, objname)" 
			    + Def.SEMICOLON + Def.TAB3 + Def.RBRACE); 
        
	 // Add the new created entry to the table.
	 //
	 create_impl.append(Def.TAB3 + "meta." + Def.METH_T_ADD + 
			    "(rowOid,objname,entry)" + Def.SEMICOLON);
	 
	 create_impl.append(Def.TAB2 + Def.N_RBRACE + " catch(" + Def.EXCP_SNMP + " e)" + Def.LBRACE +
			    Def.TAB3 +  "throw e" + Def.SEMICOLON);
	 create_impl.append(Def.TAB2 + Def.N_RBRACE + " catch(ArrayIndexOutOfBoundsException e)" + Def.LBRACE +
			    Def.TAB3 +  Def.THROW_NEW + Def.EXCP_SNMP + "(" + Def.EXCP_SNMP + "." + 
			    Def.V_WRONGVALUE +  ")" + Def.SEMICOLON);
	 create_impl.append(Def.TAB2 + Def.N_RBRACE + " catch(Exception e)" + Def.LBRACE +
			    Def.TAB3 +  Def.THROW_NEW + Def.EXCP_SNMP + "(e.getMessage())" + Def.SEMICOLON + Def.TAB2 + Def.RBRACE);
	 create_impl.append(Def.TAB +  Def.RBRACE + "\n");
	 

	 buildname_impl.append(Def.TAB2 + Def.N_RBRACE + " catch(ArrayIndexOutOfBoundsException e)" + Def.LBRACE +
			       Def.TAB3 +  Def.THROW_NEW + Def.EXCP_SNMP + "(" + Def.EXCP_SNMP + "." + 
			       Def.V_WRONGVALUE +  ")" + Def.SEMICOLON);
	 buildname_impl.append(Def.TAB2 + Def.N_RBRACE + " catch(Exception e)" + Def.LBRACE +
			       Def.TAB3 +  Def.THROW_NEW + Def.EXCP_SNMP + "(e.getMessage())" + Def.SEMICOLON + Def.TAB2 + Def.RBRACE);
	 buildname_impl.append(Def.TAB + Def.RBRACE + "\n");

	 // Handle oid from index values signature
	// FIX 
	buildBuildOidIndexValHeader(ctxt, indexObjectNames, nbIndexes);
	
	
	generateEntryFactory(ctxt,indexObjectNames,nbIndexes,entryName,
			     factory_impl);
    }
    
    public static String getEntryFactoryName(Context ctxt, String entryName) {
	return "create" + entryName + Def.MBEANSUFFIX;
    }

    public static String getEntryFactoryResult(Context ctxt, 
					       String entryName) {
	return "Object";
    }

    public static String getEntryInterfaceName(Context ctxt, 
					       String entryName) {
	return entryName + Def.MBEANSUFFIX;
    }

    private void generateFactoryComments(Context ctxt,String[] indexNames,
					 int indexCount, String entryName,
					 String implName,
					 StringBuffer result) {
	result.append("\n" + Def.TAB + "/**");
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.entry.abstract.bean", entryName));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.text1.bean"));
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.text2.bean"));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.return1.bean"));
	result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.entry.return2.bean", entryName));
	result.append("\n" + Def.TAB + " * ");
	if (isStandard()) {
	    result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.std"));
	    result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.std",implName));
	    result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.std"));
	} else {
	    result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.gen"));
	    result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.gen",implName));
	    result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.gen"));
	}

	result.append("\n" + Def.TAB + " */\n");
    }


    private void generateEntryFactory(Context ctxt, String[] indexNames,
				      int indexCount, String entryName,
				      StringBuffer result) {
	String stdbeanclass =  getEntryInterfaceName(ctxt,entryName);
	generateFactoryComments(ctxt,indexNames,indexCount,entryName,
				stdbeanclass,result);
	result.append("\n" + Def.TAB + Def.PUBLIC + 
		      getEntryFactoryResult(ctxt,entryName) + 
		      " " + getEntryFactoryName(ctxt,entryName) + "(" + 
		      Def.SNMP_SUBREQ + " req,\n");
	result.append(Def.TAB4 + Def.SNMP_OID + " rowOid, "
		      + "int depth, ObjectName entryObjName,\n"); 
	result.append(Def.TAB4 + Def.SNMP_TABLE + " meta");
	int j=1;
	for (int i=0; i<indexCount; i++) {
	    String typeName = ctxt.getJavaSyntax(indexNames[i]);
	    if (j % 3 == 0) result.append(",\n" + Def.TAB4);
	    else result.append(", ");
	    result.append(typeName + " a" + indexNames[i]);
	}
	result.append(")\n" + Def.TAB3 + Def.THROWS + Def.EXCP_SNMP + " " + 
		      Def.LBRACE);

	if (isStandard()) {
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.std"));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.std",stdbeanclass));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.std"));
	    result.append("\n" + Def.TAB2 + "//\n");
	} else {
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.gen"));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.gen",stdbeanclass));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.gen"));
	    result.append("\n" + Def.TAB2 + "//\n");
	}
	result.append(Def.TAB2 + entryName + " entry = new " + entryName +
		      "(theMib)" + Def.SEMICOLON);
	for (int i=0; i<indexCount; i++) {
	    result.append(Def.TAB2 + "entry." + indexNames[i] + " = a" +
			  indexNames[i] + Def.SEMICOLON);
	}
	result.append(Def.TAB2 + Def.RETURN + "entry" + Def.SEMICOLON);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }
 
    private void getV2IndexFromEntry(Context ctxt, ASTObjectTypeDefinitionV2 definition, MibNode ref) throws IOException {

        // Get the list of index ...
        //
        ASTIndexParts indexParts= (ASTIndexParts) definition.getIndex();
        if (indexParts.type == ParserConstants.AUGMENTS) {
            //Trace.info("AUGMENT V2 tables are not supported yet");
            //Trace.info("Need to augment with " + indexParts.getAugmentIdentifier());
	    //debug("getV2IndexFromEntry: AUGMENTS");
            String reference= indexParts.getAugmentIdentifier();
            MibNode indexNode= (mib.getModuleHandler()).findNodeWithName(reference);
            if (indexNode == null) {
                Trace.error(MessageHandler.getMessage("generate.error.table.index", reference, realName));
                throw new IOException();
            }
	    
            getIndexFromEntry(ctxt, indexNode);
	
            //indexParts.dump("------>");
            return;
        }
        ASTIndexTypesV2 v2Index= (ASTIndexTypesV2) indexParts.jjtGetChild(0);
        processIndexes(ctxt, (Node)v2Index, ref);
    }
  
    private void processEntry() throws IOException {
        
        // Get the node containing the entry ...
        //
        Hashtable children= node.getChildren();
        if (children.size() != 1) {
            // something wrong somewhere ...
            //
            Trace.error(MessageHandler.getMessage("generate.error.table.entry", realName));
            throw new IOException();
        }
    
        // well it seems to be the only way to go through this hashtable ...
        //
        Enumeration a= children.elements();
        entryNode= (MibNode) a.nextElement();
    
        // Here we could create a new Group generator for taking care of the entry.
        // Instead, we going to process the table from here through a M-bean generator.
        //
    
        // Start code generation. Create a Bean generator for handling code
        // generation for the m-bean.
        //
        beanGen= new EntryGenerator(manager, entryNode, context);
        beanIfGen= new EntryIfGenerator(manager, entryNode, context);
    
        // Process each single element contain in the group
        //
        for(Enumeration e= (entryNode.getChildren()).elements(); e.hasMoreElements(); ) {
            MibNode aNode= (MibNode) e.nextElement();
            beanGen.handleNode(aNode);
            beanIfGen.handleNode(aNode);
        }
        //beanGen.endOfGroup();
      
        entryName= beanGen.getSnmpClassName();
    }
      
    protected void updateEntryWithExternalIndex() throws IOException {
    
        for(Enumeration e= (node.getExternalIndex()).elements(); e.hasMoreElements();) {
            MibNode aNode= (MibNode) e.nextElement();
            beanGen.handleNode(aNode);
            beanIfGen.handleNode(aNode);
        }
        beanGen.endOfGroup();
        beanIfGen.endOfGroup();
    }
  
    protected void buildBuildIndexHeader() throws IOException {
        build_impl.append( Def.TAB + "/**\n" + Def.TAB + " * " +
                           MessageHandler.getMessage("generate.meta.comment.table.index", entryName) + 
                           "\n" + Def.TAB + " */\n" +
                           Def.TAB + Def.PUBLIC + Def.SNMP_INDEX + " " +
                           Def.BUILD +  Def.SNMP_INDEX + "(" + entryName + Def.MBEANSUFFIX + " entry)\n" + 
                           Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);
    }
    
    protected void buildBuildOidIndexValHeader(Context ctxt, 
					       String[] indexNames,
					       int indexCount) {
	// First create the list of parameters
	//
	int j=1;
	StringBuffer result = new StringBuffer();
	for (int i=0; i<indexCount; i++) {   
	    String typeName = ctxt.getJavaSyntax(indexNames[i]);
	    result.append(typeName + "a" + indexNames[i]);
	    if(i != (indexCount -1))
		result.append(", ");
	}
	
	//Insert at the beginning of the code the signature.
	//
	oidval_impl.insert(0, Def.TAB + "/**\n" + Def.TAB + " * " +
			   MessageHandler.getMessage("generate.meta.comment.table.index", entryName) + 
			   "\n" + Def.TAB + " */\n" +
			   Def.TAB + Def.PUBLIC + Def.SNMP_OID + " " + Def.METH_T_BUILDOIDVAL +
			   "("+result.toString()+")\n" + Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
			   " " + 
			   Def.LBRACE + Def.TAB2 + Def.SNMP_OID + " oid = new " + Def.SNMP_OID + "()" + Def.SEMICOLON);
		       
    }

    protected void buildBuildOidIndexHeader() throws IOException {
        index_impl.append( Def.TAB + "/**\n" + Def.TAB + " * " +
                           MessageHandler.getMessage("generate.meta.comment.table.index", entryName) + 
                           "\n" + Def.TAB + " */\n" +
                           Def.TAB + Def.PUBLIC + Def.SNMP_INDEX + " " +
                           Def.BUILD +  Def.SNMP_INDEX + "(long[] index, int start)\n" + 
                           Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE);		       
    }
    
    protected void buildBuildIndexOidHeader() throws IOException {
        oid_impl.append( Def.TAB + "/**\n" + Def.TAB + " * " +
                         MessageHandler.getMessage("generate.meta.comment.table.index", entryName) + 
                         "\n" + Def.TAB + " */\n" +
                         Def.TAB + Def.PUBLIC + Def.SNMP_OID + " " + Def.METH_T_BUILDOID +
                         "(" + Def.SNMP_INDEX + " index)\n" + 
                         Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE + Def.TAB2 +
                         Def.SNMP_OID + " oid = new " + Def.SNMP_OID + "()" + Def.SEMICOLON);		       
    }
  
    protected void buildConstructorHeader() throws IOException {
        constructor1.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
                            MessageHandler.getMessage("generate.meta.comment.table.constr", symboleName) + "\n" + Def.TAB +
                            " * " + MessageHandler.getMessage("generate.meta.comment.table.noRegistration") + "\n" +
                            Def.TAB + " */\n");
        constructor1.append(Def.TAB + Def.PUBLIC + symboleName + "(SnmpMib myMib)" + Def.LBRACE );
        
        constructor2.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
                            MessageHandler.getMessage("generate.meta.comment.table.constr", symboleName) + "\n" + Def.TAB +
                            " * " + MessageHandler.getMessage("generate.meta.comment.table.registration") + "\n" +
                            Def.TAB + " */\n");
        constructor2.append(Def.TAB + Def.PUBLIC + symboleName + 
			    "(SnmpMib myMib, MBeanServer server)" + 
			    Def.LBRACE );
    
        constructor1.append(Def.TAB2 + "super(myMib)" + Def.SEMICOLON + 
			    Def.TAB + Def.RBRACE + "\n");
        constructor2.append(Def.TAB2 + "this(myMib)" + 
			    Def.SEMICOLON + Def.TAB2 +
                            "this.server = server" + Def.SEMICOLON + 
			    Def.TAB + Def.RBRACE + "\n");
    
        // Add definition of variable
        //
        var_list.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
                        MessageHandler.getMessage("generate.meta.comment.table.server") + "\n" +
                        Def.TAB + " */\n" +
                        Def.TAB + Def.PROTECTED + "MBeanServer server" + Def.SEMICOLON);
    }
    
    protected void buildCreateHeader() throws IOException {
    
	// Generates comments
	//
 	genAbstractMethodComment(Def.METH_T_CREATE, Def.SNMP_TABLE_SUPPORT,
 				 create_impl);

	// public void createNewEntry(SnmpMibSubRequest req, int depth, 
	//                            SnmpMibTable meta)
	//
        create_impl.append(Def.TAB + Def.PUBLIC + Def.VOID +
                           Def.METH_T_CREATE + "(" + Def.SNMP_SUBREQ + 
			   " req, " + Def.SNMP_OID  + 
			   " rowOid,\n" + Def.TAB4 + " int depth, " +
			   Def.SNMP_TABLE + " meta)\n");

	//        throws SnmpStatusException {
	//
	create_impl.append(Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
			   Def.LBRACE);

	//   final SnmpOid rowOid = req.getEntryOid();
	//
	// create_impl.append(Def.TAB2 + Def.FINAL + Def.SNMP_OID + 
	//		   " rowOid = req." + Def.GET_ENTRY_OID + "()" + 
	//		   Def.SEMICOLON);

	//   final SnmpIndex index = buildSnmpIndex(rowOid);
	//
	create_impl.append(Def.TAB2 + Def.FINAL + Def.SNMP_INDEX + 
			   " index = " + Def.BUILD + Def.SNMP_INDEX +
			   "(rowOid)" + Def.SEMICOLON);

	//   final <EntryName> entry = new <EntryName>(theMib);
	//
	// create_impl.append(Def.TAB2 + Def.FINAL + entryName + 
	// Def.MBEANSUFFIX +
	//		   " entry = null" + Def.SEMICOLON);
 	create_impl.append(Def.TAB2 + Def.FINAL + 
			   "Vector v = index.getComponents()" + 
 			   Def.SEMICOLON);
 	create_impl.append(Def.TAB2 + Def.SNMP_OID + " oid" + Def.SEMICOLON);

	//    try {
	//
	create_impl.append(Def.TAB2 + "try " + Def.LBRACE );
    }
  
    protected void buildBuildNameHeader() throws IOException {
    
	genAbstractMethodComment(Def.METH_T_BUILDNAME, 
				 Def.SNMP_TABLE_SUPPORT, buildname_impl);

        buildname_impl.append("\n" + Def.TAB + Def.PUBLIC + "ObjectName " +
			      Def.METH_T_BUILDNAME + "(" + Def.SNMP_INDEX +
			      " index)\n");
	buildname_impl.append(Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
			      Def.LBRACE);
	buildname_impl.append(Def.TAB2 + "Vector v = index.getComponents()" + 
			      Def.SEMICOLON);
	buildname_impl.append(Def.TAB2 + Def.SNMP_OID + " oid" + 
			      Def.SEMICOLON);
	buildname_impl.append(Def.TAB2 + "try " + Def.LBRACE );
    }
  
//     protected void buildCreateReqHeader() throws IOException {
// 	genAbstractMethodComment(Def.METH_T_CREATE, Def.SNMP_TABLE_SUPPORT,
// 				 create_req_impl);
//         create_req_impl.append(Def.TAB + Def.PUBLIC + "Object " +
// 			       Def.METH_T_CREATE + "(" + Def.SNMP_SUBREQ + 
// 			       " req, " + Def.SNMP_OID + 
// 			       " rowOid, int depth, " +
// 			       Def.SNMP_TABLE + " meta)\n" +
// 			       Def.TAB2 + Def.THROWS + Def.EXCP_SNMP 
// 			       + Def.LBRACE + Def.TAB2 + "return " +
// 			       Def.METH_T_CREATE + "(index)" + 
// 			       Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n");
//     }
  
    // VARIABLES
    //----------
 
    // MetaData type
    //
    protected int gentype = 0;

    /**
     * Variable list
     */
    protected StringBuffer var_list= new StringBuffer();
  
    /**
     * Compute index from entry
     */
    protected StringBuffer  build_impl= new StringBuffer();
  
    /**
     * Compute oid from index
     */
    protected StringBuffer  oid_impl= new StringBuffer();
  
    /**
     * Compute oid from index values
     */
    protected StringBuffer  oidval_impl= new StringBuffer();
    
    /**
     * Compute index from oid
     */
    protected StringBuffer index_impl= new StringBuffer();
  
    /** 
     * Compute index
     */
    protected StringBuffer  table_impl= new StringBuffer();
  
    /**
     * Getter
     */
    protected StringBuffer get_impl =  new StringBuffer();
    protected StringBuffer getReq_impl =  new StringBuffer();
  
    /**
     * Get Next stuff
     */
    protected StringBuffer getNext_impl =  new StringBuffer();
    protected StringBuffer validateVar_impl =  new StringBuffer();
    protected StringBuffer getNextVarId_impl =  new StringBuffer();
  
    /*
    ** Setter
    */
    protected StringBuffer set_impl =  new StringBuffer();
    protected StringBuffer setReq_impl =  new StringBuffer();
  
    /**
     * Checker
     */
    protected StringBuffer check_impl =  new StringBuffer();
    protected StringBuffer checkReq_impl =  new StringBuffer();
  
    /**
     * Constructor
     */
    protected StringBuffer constructor1= new StringBuffer();    // Constructor without reference on the MBeanServer
    protected StringBuffer constructor2= new StringBuffer();    // Constructor with reference on the MBeanServer
  
    /**
     * Create entry in the table ...
     */
    protected StringBuffer create_impl     = new StringBuffer();
    //    protected StringBuffer create_req_impl = new StringBuffer();
    protected StringBuffer buildname_impl  = new StringBuffer();

    protected StringBuffer setobj_impl = new StringBuffer();

    protected StringBuffer factory_impl = new StringBuffer();

    private String entryName= "";
    private String realName= "";
    private MibNode entryNode;
  
    private MbeanGenerator beanGen;
    private MbeanIfGenerator beanIfGen;
}

