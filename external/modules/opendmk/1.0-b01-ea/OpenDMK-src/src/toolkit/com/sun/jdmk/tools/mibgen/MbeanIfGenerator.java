/*
 * @(#)file      MbeanIfGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.15
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
 * The class generates interface required for representing a standard m-bean
 *
 */

public class MbeanIfGenerator extends BeanIfGenerator implements Serializable {
  
    public static String buildMBeanIfName(String pre, String name) {
        return pre + name + Def.MBEANSUFFIX;
    }
    
    public MbeanIfGenerator(ResourceManager mgr, MibNode aGroup, 
			    Context ctxt)
	throws IOException {
        
        super(mgr, aGroup, ctxt);
    
        // Try to find a symbol to associate to the group
        //
        varName = node.getSymbolName();
        if (varName == null)
            varName = getClassName(node.getComputedOid());
        symboleName = buildMBeanIfName(prefix, varName);
     
        Trace.info(MessageHandler.getMessage("generate.info.if", varName));
     
        // Open the file which will represent the M-bean.
        //
        out = openFile(symboleName + Def.JAVA);
     
        // Write generic header ...
        //
        writeHeader();
     
        // Write our own header ...
        //
        writeClassDeclaration();     
    }
  
    protected void writeClassDeclaration() throws IOException {
        // Add some comments
        //
        write("/**\n" +
              " * " + MessageHandler.getMessage("generate.mbeanif.comment.desc", varName) + "\n" +
              " */\n");
        write(Def.PUBLIC + Def.INTERFACE + symboleName + Def.LBRACE + "\n");
    }
    
    public void endOfGroup() throws IOException {
        write(accessors.toString());
        write(Def.RBRACE);
        closeIO();
    }
  
    public void handleNode(MibNode aNode) throws IOException {
	handleNode(aNode,this.context);
    }

    public void handleNode(MibNode aNode, Context ctxt) throws IOException {
    
        // Check if the node is a nested group or not. A nested group
	// has some children but is not a associated.
	if (aNode.isGroup() || aNode.hasNestedGroups()) {
	    // The node is a nested group !
	    //
	    handleNestedGroups(aNode,ctxt);
	    return;
	}
        // Check if the node is a table or not. A table has some children
        // but is not a group.
        if (aNode.isTable()) {            
            handleTable(aNode,ctxt);
            return;
        }
    
        // Name of the symbol
        //
        String varName = aNode.getSymbolName();
    
        // Get the object definition associated to the node
        //
        ASTObjectTypeDefinition definition = aNode.getObjectType();
        if (definition == null)
            return;
        
        // get the syntax ...
        //
        ASTNamedType syntax = definition.getSyntax();
        String strSyntax = "";
		 
        if (syntax.isEnumeratedType()) {
            String enumTypeName = syntax.getEnumeratedDef().getSymbol();
            if (enumTypeName.length() == 0) {
                // This is a in-line definition. Generate the code. 
                // Use the variable name to name the enumeratiom.
                //
                enumTypeName = varName;
            }
            strSyntax = prefix + Def.ENUMPREFIX + enumTypeName + " ";
            aNode.setEnumerated(true);
            aNode.setEnumeratedType(strSyntax);
        } else {  
            // Get the real syntax to use for the node
            //  
            strSyntax= syntax.getMbeanSyntax();
        }
        
        // Generate getter and setter
        //
        addAccessors(aNode, strSyntax, varName, ctxt);
    }
  
    /**
     * Process nested groups.
     */
    protected void handleNestedGroups(MibNode node, Context ctxt) 
	throws IOException {
	// java.lang.System.out.println(node.getComputedOid()+
	//			     ": is or has NESTED Groups!!!");
    }

    /**
     * Process table.
     */
    protected void handleTable(MibNode aNode, Context ctxt) 
	throws IOException {

	// Check whether accessTableXxxx() methods must be generated.
	//
	if (ctxt.genItfTableAccess == false) return;

        // Name of the symbol
        //
        String variable = aNode.getSymbolName();
        
        // Get the table name
        //
        String tableName = prefix + Def.TABLEPREFIX + variable;
        
        // Get the entry name
        //
        String entryName = getTableEntryName(aNode);
    
        // Add access method on the table
        //
        accessors.append(Def.TAB + "/**\n" + Def.TAB + " * " +
                         MessageHandler.getMessage("generate.mbean.comment.table.access", variable) + 
                         "\n" + Def.TAB + " */\n" );
        accessors.append(Def.TAB + Def.PUBLIC + tableName + " " + Def.ACCESS + variable + "() " + accessThrows + "\n");
    }
  
    private String getTableEntryName(MibNode aNode) throws IOException {
        
        // Get the node containing the entry ...
        //
        Hashtable children= aNode.getChildren();
        if (children.size() != 1) {
            // something wrong somewhere ...
            //
            Trace.error(MessageHandler.getMessage("generate.error.table.entry", aNode.getRealSymbolName()));
            throw new IOException();
        }
    
        // well it seems to be the only way to go through this hashtable ...
        //
        Enumeration a= children.elements();
        MibNode entryNode= (MibNode)a.nextElement();
                
        // Try to find a symbol to associate to the group
        //
        String varName = entryNode.getSymbolName();
        if (varName == null)
            varName = getClassName(entryNode.getComputedOid());
        
        String symboleName = prefix + varName;
        
        return symboleName;
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
		    accessThrows + "\n");
    }
  
    /**
     * Generates the code for the checker.
     **/
    protected void addChecker(Context ctxt, MibNode node, 
			      String description, String syntax, 
			      String var, StringBuffer buff) {
	buff.append(Def.TAB + "/**\n" + Def.TAB + " * " +
	  MessageHandler.getMessage("generate.mbean.comment.checker", var) + 
		    "\n" + Def.TAB + " */\n" );
	buff.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.CHECK + var +
		    "(" + syntax + "x) " + accessThrows + "\n");
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
		    "(" + syntax + "x) " + accessThrows + "\n");       
    }

    /**
     * Build getter and setter for a variable ...
     */
    protected void addAccessors(MibNode node, String syntax, String var,
				Context ctxt) 
	throws IOException {
   
        // Get the object definition associated to the node
        //
        ASTObjectTypeDefinition definition= node.getObjectType();
        String description= definition.getDefinition().getDescription();
    
        int access= definition.getDefinition().getAccess();
	
	// Do not add getter if the variable is accessible-for-notication
	// 
	if ((access != ParserConstants.AFN) || ctxt.genAFNGetter)
	    addGetter(ctxt, node, description, syntax, var, accessors);
    
        // If the variable is read-write, add a setter and a checker ...
        //
        if ((access == ParserConstants.RW)|| 
            (access == ParserConstants.WO)||
            (access == ParserConstants.RC)){
	    addSetter(ctxt, node, description, syntax, var, 
		      accessors);
	    addChecker(ctxt, node, description, syntax, var, 
		      accessors);
        }
    }
      
    // VARIABLES
    //----------
  
    /*
    ** Getters and setters
    */
    protected StringBuffer accessors = new StringBuffer();
}

