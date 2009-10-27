/*
 * @(#)file      MibStoreGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.20
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
 * The class generates code required for representing a whole MIB in a 
 * SnmpOidTable.
 *
 */
public class MibStoreGenerator extends Generator implements Serializable {
  
    public MibStoreGenerator(ResourceManager mgr, ASTMib aMib, Context ctxt) 
	throws IOException {
        
        super(mgr, ctxt);
    
        // No entry has been defined so far
        //
        entrySet= false;
     
        mib= aMib;
	context.mib = aMib;

        // Get the name of the MIB
        //
        module= aMib.getModuleName();
     
        // Translate the module name in order to get an acceptable Java class name
        //
        fileName= prefix + translateMibName() + Def.OIDTABLESUFFIX;
     
        // Open the file which will represent the M-bean.
        //
        out= openFile(fileName + Def.JAVA);
     
        // Write generic header ...
        //
        writeHeader();
     
        // Write our own header ...
        //
        writeClassDeclaration();
     
        writeConstructor();
    }
  
    public void setContext(Context ctxt) {
	super.setContext(ctxt);
	mib = ctxt.mib;
    }

    /**
     * Generates code for OBJECT-IDENTITY and NOTIFICATION-TYPE macros
     *
     * @param mib The mib we generate code for...
     *
     */
    
    private void generateSymbolCode(ASTMib mib) throws IOException  {
	if (mib.registrationTable == null) return;
	Enumeration keys = mib.registrationTable.keys();
	for (; keys.hasMoreElements() ;) {
	    String key = (String) keys.nextElement();
	    RegisteredObject obj = (RegisteredObject) 
		mib.registrationTable.get(key);
	    switch (obj.getNodeType()) {
	    case RegisteredObject.OBJECT_IDENTITY:
		addMibEntry(key, mib.resolveOidSymbol(key), 
			    SyntaxMapper.MibStoreIdentitySymbol);
		break;
	    case RegisteredObject.OBJECT_GROUP:
		addMibEntry(key, mib.resolveOidSymbol(key), 
			    SyntaxMapper.MibStoreObjectGroupSymbol);
		break;
	    case RegisteredObject.NOTIFICATION_GROUP:
		addMibEntry(key, mib.resolveOidSymbol(key), 
			    SyntaxMapper.MibStoreNotificationGroupSymbol);
		break;
	    case RegisteredObject.NOTIFICATION_TYPE:
		addMibEntry(key, mib.resolveOidSymbol(key), 
			    SyntaxMapper.MibStoreNotificationSymbol);
		break;
	    default:
		break;
	    }
	}
    }

    public void generateCode() throws IOException {
    
        initMeta();
    
        // Get the root of the tree
        //
        MibNode node= mib.getMibTree().getRoot();
      
        // Go through the tree and generate code
        //
        generateCode(node);
    
	// Generates code for additional symbols
	generateSymbolCode(mib);

        // That's it ...
        //
        endCodeGeneration();
    }   

    private void endCodeGeneration() throws IOException {
    
        // Check if the mib was containing some valid definitions ...
        //
        if (entrySet == true) {
            closeMeta();
        } else {
            closeMeta();
        }
    
        write(metadata.toString());
        write(Def.RBRACE);
	closeIO();
    }
    
    private void generateCode(MibNode node) throws IOException {
   
        // Check if the node has a symbol associated to it ...     
        //
        if (node.isAssociated() == true) {
            // We need to get the oid and syntax ...
            //
            String oid = node.getComputedOid();
            String syntax= SyntaxMapper.getMibStoreSyntax(SyntaxMapper.getTypeName(accessNodeSyntax(node)));
            if (syntax.equals(SyntaxMapper.MibStoreSequenceSymbol)) {
                // How do we make the difference between a table and an entry ?
                //
                Hashtable children= node.getChildren();
                if (children.isEmpty() == false) {
                    for(Enumeration e= children.elements(); e.hasMoreElements();) {
                        MibNode child= (MibNode) e.nextElement();
                        if (child.getChildren().isEmpty()) {
                            syntax= SyntaxMapper.MibStoreEntrySymbol;
                        }
                        break;
                    }
                }
            }
            addMibEntry(node.getRealSymbolName(), oid, syntax);
        }

        // keep going in the tree ...
        //
        Hashtable children= node.getChildren();
        for(Enumeration e= children.elements(); e.hasMoreElements();) {
            generateCode((MibNode) e.nextElement());
        }
    }

    protected void addMibEntry(String name, String oid, String syntax) throws IOException {
        if (entrySet == true) {
            metadata.append(",\n");
        }
        metadata.append(Def.TAB2 + "new SnmpOidRecord(\"" + name + "\", \"" + oid + "\", \"" + syntax + "\")");
        entrySet= true;
    }
 
    protected void closeMeta() throws IOException {
        metadata.append(Def.TAB + "};\n");
    }
  
    protected void initMeta() throws IOException {
        metadata.append(Def.TAB + "static SnmpOidRecord " + varName + " [] = {\n");
    }
 
    protected void writeClassDeclaration() throws IOException {
        // Add some comments
        //
        write("/**\n" +
              " * " + MessageHandler.getMessage("generate.miboidtable.comment.header", module) + "\n" +
              " * " + MessageHandler.getMessage("generate.miboidtable.comment.header2", fileName) + "\n" +
              " */\n");
    
        write(Def.PUBLIC + Def.CLASS + fileName + Def.EXTEND + Def.SNMP_OID_TABLE +
              Def.IMPLEMENT + Def.SERIALIZABLE + Def.LBRACE + "\n"); 
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
      
        // import the JMX SNMP package                        
        //
        write("\n// jmx imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SNMP_OID_RECORD + Def.SEMICOLON);
	// NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        if (SyntaxMapper.useUnsigned)
            write(Def.IMPORT + Def.PKG_UNSIGNEDLONG + Def.SEMICOLON);
        // end of NPCTE fix for bugId 4692891
        // import the Java DMK package                        
        //
	
        write("\n// jdmk imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SNMP_OID_TABLE_SUPPORT + Def.SEMICOLON);
        write("\n");
    }

    protected void writeConstructor() throws IOException {
        write(Def.TAB + "/**\n" +
              Def.TAB  + " * " + MessageHandler.getMessage("generate.mib.comment.const") + "\n" +
              Def.TAB +" */\n");
        write(Def.TAB + Def.PUBLIC + fileName + "()" + Def.LBRACE + Def.TAB2 +
              "super(\"" + translateMibName() + "\")" + Def.SEMICOLON  + Def.TAB2 +
              "loadMib(" + varName + ")" + Def.SEMICOLON + 
              Def.TAB + Def.RBRACE + "\n");
    }
    
    // PRIVATE METHODS
    //----------------
  
    private String translateMibName() throws IOException {
        String result= module.trim();
        result= result.replace('-', '_');
        result= result.replace('.', '_');
        result= result.replace(',', '_');
        result= result.replace(',', '_');
        result= result.replace(' ', '_');
        return result;
    }
  
    private String accessNodeSyntax(MibNode node) {
        ASTObjectTypeDefinition definition= node.getObjectType();
    
        ASTNamedType syntaxObject=definition.getSyntax();
        return  syntaxObject.getSnmpSyntax();
    }
  
    // PRIVATE VARIABLES
    //------------------
  
    /**
     * Name of the module
     */
    protected String module= "";
  
    /**
     * Name of the file
     */
    protected String fileName= "";
    
    /**
     * Variables used by the MIB.
     */
    protected StringBuffer metadata= new StringBuffer();

    /**
     * Indicates if an entry was added.
     */
    protected boolean entrySet= false;
  
    /**
     * var name containing all the definitions.
     */
    private String varName= "varList";
}

