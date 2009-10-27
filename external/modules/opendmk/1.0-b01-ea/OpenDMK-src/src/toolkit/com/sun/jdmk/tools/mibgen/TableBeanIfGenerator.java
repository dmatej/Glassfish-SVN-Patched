/*
 * @(#)file      TableBeanIfGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.11
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

public class TableBeanIfGenerator extends BeanIfGenerator implements Serializable {
  
    public static String buildTableBeanIfName(String pre, String name) {
        return pre + Def.TABLEPREFIX + name + Def.MBEANSUFFIX;
    }
    
    public TableBeanIfGenerator(ResourceManager mgr, String pkgName, String prefix, String dir, MibNode table, ASTMib aMib, Context ctxt) throws IOException {
        
        // super(mgr, pkgName, prefix, dir, table, aMib);
        super(mgr, table, ctxt);
	
        this.node= table;
    
        // Get the symbol name associated to the table
        //
        varName = node.getSymbolName();
        symboleName = buildTableBeanIfName(prefix, varName);
     
        Trace.info(MessageHandler.getMessage("generate.info.if", varName));
             
        // Open the file which will represent the M-bean.
        //
        out= openFile(symboleName + Def.JAVA);
     
        // Write generic header ...
        //
        writeHeader();
     
        // Write our own header ...
        //
        writeClassDeclaration();
     
        // Table management
        //
        processTable();
          
        // Write all the generated stuff ...
        //
        write(table_impl.toString());
        write("\n" + Def.RBRACE);
        closeIO();
    }
  
    protected void writeClassDeclaration() throws IOException {
        // Add some comments
        //
        write("/**\n" +
              " * " + MessageHandler.getMessage("generate.mbeanif.comment.desc", Def.TABLEPREFIX + varName) + "\n" +
              " */\n");
        write(Def.PUBLIC + Def.INTERFACE + symboleName + Def.EXTEND + Def.SNMP_TABLE + Def.MBEANSUFFIX + Def.LBRACE + "\n");
    }
            
    // PRIVATE METHODS
    //----------------
  
    private void processTable() throws IOException {
        
        // Get the entry name
        //
        String entryName = getTableEntryName();
        
        // Generate code for adding elements in the table
        //
        table_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.METH_T_ADD +
                          "(" + entryName + " entry)\n" +
                          Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + Def.SEMICOLON + "\n");
        
        // Add the  code for removing an entry ...
        //
        table_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.METH_T_REM + 
                          "(" + entryName + " x)" + Def.SEMICOLON + "\n");
        
        // Add code for getting all the entries ...
        //
        table_impl.append(Def.TAB + Def.PUBLIC + entryName + "[] " + 
                          Def.GET_ENTRIES + "()" + Def.SEMICOLON + "\n");
    }
    
    private String getTableEntryName() throws IOException {
        
        // Get the node containing the entry ...
        //
        Hashtable children= node.getChildren();
        if (children.size() != 1) {
            // something wrong somewhere ...
            //
            Trace.error(MessageHandler.getMessage("generate.error.table.entry", node.getRealSymbolName()));
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
               
    // VARIABLES
    //----------
 
    /**
     * Node representing the group
     */
    protected MibNode node;
    
    /** 
     * Compute index
     */
    protected StringBuffer  table_impl= new StringBuffer();
}

