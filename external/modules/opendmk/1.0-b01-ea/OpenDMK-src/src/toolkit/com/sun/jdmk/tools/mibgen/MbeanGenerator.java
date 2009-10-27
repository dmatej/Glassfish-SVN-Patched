/*
 * @(#)file      MbeanGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.29
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
public class MbeanGenerator extends BeanGenerator implements Serializable {
  
    public static String getNodeSymbolName(Context ctxt, MibNode node) {
        String vName= node.getSymbolName();
        if (vName == null)
            vName= Generator.getClassName(ctxt, node.getComputedOid());
        String sName= ctxt.prefix + vName;
	return sName;
    }

    String getNodeSymbolName(MibNode node) {
	 return getNodeSymbolName(context,node);
    }

    public MbeanGenerator(ResourceManager mgr, MibNode aGroup, Context ctxt) 
	throws IOException {
        super(mgr, aGroup, ctxt);
    
	gentype = ctxt.gentype;

        // Specify oid of the current bean ...
        //
        oid= node.getComputedOid();
    
        // Try to find a symbol to associate to the group
        //
        varName= node.getSymbolName();
        if (varName == null)
            varName= getClassName(node.getComputedOid());
        symboleName= getNodeSymbolName(node);
     
        Trace.info(MessageHandler.getMessage("generate.info.var", varName));
     
        // Open the file which will represent the M-bean.
        //
        out= openFile(symboleName+ Def.JAVA);
     
        // Write generic header ...
        //
        writeHeader();
     
        // write our own header ...
        //
        writeClassDeclaration();
     
        // write the beginning of the constructor
        //
        buildConstructorHeader();
     
    }
      
    public void setContext(Context ctxt) {
	super.setContext(ctxt);
	gentype = ctxt.gentype;
    }

    protected void writeClassDeclaration() throws IOException {
        // Add some comments
        //
        write("/**\n" +
              " * " + MessageHandler.getMessage("generate.mbean.comment.desc", varName) + "\n" +
              " * " + MessageHandler.getMessage("generate.mbean.comment.oid", oid) + "\n" +
              " */\n");
        write(Def.PUBLIC + Def.CLASS + symboleName + Def.IMPLEMENT + symboleName + Def.MBEANSUFFIX + ", " + 
              Def.SERIALIZABLE + Def.LBRACE + "\n");
    }
    
    // Specify the version of mibgen used for generating the code
    //
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
     
        // import the JMX SNMP package                        
        //
        write("\n// jmx imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_MBEAN_SERVER + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_STRING + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_STATUS_EXCEPTION + Def.SEMICOLON);
	// NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        if (SyntaxMapper.useUnsigned)
            write(Def.IMPORT + Def.PKG_UNSIGNEDLONG + Def.SEMICOLON);
        // end of NPCTE fix for bugId 4692891
        // import the Java DMK SNMP package                        
        //
        write("\n// jdmk imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SNMP_MIB + Def.SEMICOLON);
	
        write("\n");
    }
    
    public String getSnmpClassName() {
        return symboleName;
    }
    
    public void endOfGroup() throws IOException {
        closeConstructor();
        write(var_list.toString());
        write(constructor1.toString());
        write(constructor2.toString());
        write(accessors.toString());
        write(Def.RBRACE);
        closeIO();
    }
  
    public void handleNode(MibNode aNode) throws IOException {
    
        // Check if the node is a nested group or not. A nested group
	// has some children but is not a associated.
	if (aNode.isGroup() || aNode.hasNestedGroups()) {
	    // The node is a nested group !
	    //
	    handleNestedGroups(aNode);
	    return;
	}
        // Check if the node is a table or not. A table has some children
        // but is not a group.
        if (aNode.isTable()) {            
            // The node is a table !
            //
            handleTable(aNode);
            return;
        }
    
        // Name of the symbol
        //
        String varName=  aNode.getSymbolName();
    
        // Get the object definition associated to the node
        //
        ASTObjectTypeDefinition definition= aNode.getObjectType();
        if (definition == null)
            return;
        // get the syntax ...
        //
        ASTNamedType syntax=definition.getSyntax();
        String strSyntax= "";
    
        // Get the default MIB variable value
        //
        ASTValue defValue=definition.getDefValue();
        
        String init = "";
        if (syntax.isEnumeratedType()) {
            // get the access mode of the node
            //
            EnumGenerator enumgen = new EnumGenerator(manager, varName, syntax.getEnumeratedDef(), context);
            strSyntax= enumgen.getTypeName();
            
            // Now get the SNMP syntax in order to get a valid init value ...
            //  - if a default value is defined, just take it
            //  - otherwise, get a valid init value defined in the syntax mapper
            //

            // There is a default value for this variable.
            //
            if (defValue != null) {
                init = defValue.getDefValInitializer(strSyntax, syntax, varName);
            }
            // There is no default value for this variable or the default value cannot be resolved.
            //
            if ((defValue == null) || (init.equals(""))) {  
                init = " = new " + strSyntax.trim() + "()";
            }
            
            aNode.setEnumerated(true);
            aNode.setEnumeratedType(strSyntax);
        } else {  
            // Get the real syntax to use for the node
            //  
            strSyntax= syntax.getMbeanSyntax();
      
            // Now get the SNMP syntax in order to get a valid init value ...
            //  - if a default value is defined, just take it
            //  - otherwise, get a valid init value defined in the syntax mapper
            //
            
            // There is a default value for this variable.
            //
            if (defValue != null) {  
                init = defValue.getDefValInitializer(strSyntax, syntax, varName);
            }
            // There is no default value for this variable or the default value cannot be resolved.
            //
            if ((defValue == null) || (init.equals(""))) {  
                init = SyntaxMapper.getInitializer(syntax.getSnmpSyntax());
            }
        }
    
        // Add a Java variable for representing the snmp variable ...
        //
        long length= syntax.getFixedLength();
        aNode.setFixedLength(length);
        addCacheVar(aNode, strSyntax, length, init, varName);
    
        // Generate getter and setter
        //
        addAccessors(aNode, strSyntax, varName);
    }
  
    /**
     * Process nested groups.
     */
    protected void handleNestedGroups(MibNode node) throws IOException {
	// java.lang.System.out.println(node.getComputedOid()+
	//			     ": is or has NESTED Groups!!!");
    }

    /**
     * Process table.
     */
    protected void handleTable(MibNode node) throws IOException {
        // create a table generator
        //
        TableBeanGenerator table= 
	    new TableBeanGenerator(manager, node, context);

	// The meta table generator is now created by the 
	// MetaBeanGenerator, which is more logic.
	//
        // // MetaTableGenerator metatable= 
	// //   new MetaTableGenerator(manager, node, context);
    
        // create a table generator interface
        //
        //TableBeanIfGenerator tableIf= new TableBeanIfGenerator(manager, packageName, prefix, targetDir, node, mib);
    
        // Get the entry name
        //
        String entry= table.getEntryName();
    
        // Get the table name
        //
        String tableName= table.getTableClassName();
    
        // Name of the symbol
        //
        String variable=  table.getSymbolName();
    
        // Add cache variable for storing table
        //
        addCacheVar(node, tableName,  (long) -1, null, variable);
    
        // Initialize the table in the constructor
        //
        constructor1.append(Def.TAB2 + variable + " = " + Def.NEW + tableName + "(myMib)" + Def.SEMICOLON);
        constructor2.append(Def.TAB2 + variable + " = " + Def.NEW + tableName + "(myMib, server)" + Def.SEMICOLON);
    
        // Add access method on the table
        //
        accessors.append(Def.TAB + "/**\n" + Def.TAB + " * " +
                         MessageHandler.getMessage("generate.mbean.comment.table.access", variable) + 
                         "\n" + Def.TAB + " */\n" );
        accessors.append(Def.TAB + Def.PUBLIC + tableName + Def.ACCESS + variable + "() " + accessThrows);
        accessors.append(Def.TAB2 + Def.RETURN + variable + Def.SEMICOLON +
                         Def.TAB + Def.RBRACE + "\n");
    
        // Show the table as an indexed property...
        //
        accessors.append(Def.TAB + "/**\n" + Def.TAB + " * " +
                         MessageHandler.getMessage("generate.mbean.comment.table.entry", variable) + 
                         "\n" + Def.TAB + " */\n" );
        accessors.append(Def.TAB + Def.PUBLIC + entry + Def.MBEANSUFFIX + "[] " + Def.GET + variable + "() " + accessThrows);
        accessors.append(Def.TAB2 + Def.RETURN + variable + ".getEntries()" + Def.SEMICOLON +
                         Def.TAB + Def.RBRACE + "\n");
    }

    /**
     * Generates the code for the getter.
     **/
    protected void addGetter(Context ctxt, MibNode node, String description, 
			     String syntax, String var, StringBuffer buff) {
	
        buff.append(Def.TAB + "/**\n" + Def.TAB + " * " +
	   MessageHandler.getMessage("generate.mbean.comment.getter", var) + 
		    "\n" + Def.TAB + " */\n" );
        buff.append(Def.TAB + Def.PUBLIC + syntax + Def.GET + var + "() " + 
		    accessThrows);
        buff.append(Def.TAB2 + Def.RETURN + var + Def.SEMICOLON);
        buff.append(Def.TAB + Def.RBRACE + "\n");
    }
  
    /**
     * Generates the code for the checker.
     **/
    protected void addChecker(Context ctxt, MibNode node, 
			      String description, String syntax, 
			      String var, StringBuffer buff) {
	buff.append(Def.TAB + "/**\n" + Def.TAB + " * " +
	  MessageHandler.getMessage("generate.mbean.comment.checker",var) + 
		    "\n" + Def.TAB + " */\n" );
	buff.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.CHECK + var + "(" 
		    + syntax + "x) " + accessThrows);
        buff.append(Def.TAB2 + "//\n" + Def.TAB2 + "// " + 
	  MessageHandler.getMessage("generate.mbean.comment.checker.policy")
		    + "\n" + Def.TAB2 + "//\n");
	buff.append(Def.TAB + Def.RBRACE + "\n");
    }
  
    /**
     * Generates the code for the checker.
     **/
    protected void addSetter(Context ctxt, MibNode node, 
			     String description, String syntax, 
			     String var, StringBuffer buff) {
	buff.append(Def.TAB + "/**\n" + Def.TAB + " * " +
	   MessageHandler.getMessage("generate.mbean.comment.setter", var) + 
		    "\n" + Def.TAB + " */\n" );
	buff.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.SET + var + 
		    "(" + syntax + "x) " + accessThrows);
	buff.append(Def.TAB2 + var + " = x" + Def.SEMICOLON);
	buff.append(Def.TAB + Def.RBRACE + "\n");
    }

    /**
     * Build getter and setter for a variable ...
     */
    protected void addAccessors(MibNode node, String syntax,  String var) 
	throws IOException {
   
        // Get the object definition associated to the node
        //
        ASTObjectTypeDefinition definition= node.getObjectType();
        String description= definition.getDefinition().getDescription();

	
	addGetter(this.context, node, description, syntax, var, accessors);

        // If the variable is read-write, add a setter and a checker ...
        //
        int access= definition.getDefinition().getAccess();
        if ((access == ParserConstants.RW)|| 
            (access == ParserConstants.WO)||
            (access == ParserConstants.RC)){
	    addSetter(this.context, node, description, syntax, var, 
		      accessors);
	    addChecker(this.context, node, description, syntax, var, 
		      accessors);
        }
    }
  
    protected void addCacheVar(MibNode node, String syntax, long fixed, String init,String var) throws IOException {    
        // Put some comments ...
        //
        var_list.append(Def.TAB + "/**\n" + Def.TAB + " * " +
                        MessageHandler.getMessage("generate.mbean.comment.varUse", var) + "\n" + Def.TAB + " * " +
                        MessageHandler.getMessage("generate.mbean.comment.varOid", node.getOid()) + "\n" + Def.TAB);
        if (fixed!= -1) {
            var_list.append(" * " + MessageHandler.getMessage("generate.mbean.comment.varFix", String.valueOf(fixed)) +
                            "\n" +Def.TAB );
        }

        // Shall we put the description in the generated code ?
        // if yes call the formatDescription method ...
        // The answer is yes if requested !
        if (mib.isDescriptionOn()) {
            // Get the object definition associated to the node
            //
            ASTObjectTypeDefinition definition= node.getObjectType();
            String description= definition.getDefinition().getDescription();
            var_list.append(formatDescription(description));
        }
        var_list.append( " */\n" );
    
        if (init== null)
            init= "";
    
        // Declare the variable
        //
        var_list.append(Def.TAB + Def.PROTECTED + syntax + var + init + Def.SEMICOLON + "\n");
    }
    
    // PRIVATE METHODS
    //----------------
  
    protected void buildConstructorHeader() throws IOException {
        constructor1.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
                            MessageHandler.getMessage("generate.mbean.comment.constr", symboleName) + "\n" + Def.TAB +
                            " * " + MessageHandler.getMessage("generate.mbean.comment.noRegistration") + "\n" +
                            Def.TAB + " */\n");
        constructor1.append(Def.TAB + Def.PUBLIC + symboleName + "(SnmpMib myMib)" + Def.LBRACE );
        
        constructor2.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
                            MessageHandler.getMessage("generate.mbean.comment.constr", symboleName) + "\n" + Def.TAB +
                            " * " + MessageHandler.getMessage("generate.mbean.comment.registration") + "\n" +
                            Def.TAB + " */\n");
        constructor2.append(Def.TAB + Def.PUBLIC + symboleName + "(SnmpMib myMib, MBeanServer server)" + Def.LBRACE );
    }
  
    protected void closeConstructor() throws IOException {
        constructor1.append( Def.TAB + Def.RBRACE + "\n");    
        constructor2.append( Def.TAB + Def.RBRACE + "\n");    
    }
  
    // VARIABLES
    //----------

    // MetaData type
    //
    protected int gentype = 0;

    /**
     * Variable list
     */
    protected StringBuffer var_list= new StringBuffer();
  
    /*
    ** Getters and setters
    */
    protected StringBuffer accessors= new StringBuffer();
  
    /*
    ** Constructor
    */
    protected StringBuffer constructor1= new StringBuffer();    // Constructor without reference on the MBeanServer
    protected StringBuffer constructor2= new StringBuffer();    // Constructor with reference on the MBeanServer
}

