/*
 * @(#)file      CodeGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.12
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
 * The class corresponds to the code generator (back-end)
 *
 */
public  class CodeGenerator extends Generator implements Serializable {
  
    public CodeGenerator(ResourceManager mgr, Context ctxt) {
	super(mgr, ctxt);
	groupGenerator= new GroupGenerator(mgr, ctxt.duplicate());
    }
    
    public void addExternalSymbol(Context ctxt, String symbolName) {
	groupGenerator.addExternalSymbol(ctxt,symbolName);
    }

    public void generateCode(ASTMib aMib, int type) throws IOException {
	
	Context ctxt = context.duplicate();
	ctxt.mib     = aMib;
	ctxt.gentype = type;
	ctxt.codegen = this;
	
	// Create a mib generator
	//
	MibGenerator gen= new MibGenerator(manager, aMib, ctxt);
	gen.setDomainSeparator(domainSeparator);
	ctxt.mibgen = gen;

	// Build oid translation table
	//
	// aMib.buildOidTable();
	
	// Get the mib tree
	//
	MibTree tree= aMib.getMibTree();

	// Get the root of the tree
	//
	MibNode node= tree.getRoot();
    
    
	// Configure the different code generators
	//
	groupGenerator.setContext(ctxt);
	setContext(ctxt);

	// Go through the tree and build symbol tables in the context.
	// 
	buildSymbolTable(ctxt,gen,node);

	// java.lang.System.out.println("\nJava Table:\n" + 
	//			     ctxt.getJavaSyntaxTable().toString()
	//			     + "\n");

	//
	// Go through the tree and generate code
	//
	generateCode(ctxt,gen,node);
	gen.endMib();
    }
  
    private void generateCode(Context ctxt, MibGenerator gen, MibNode node) 
	throws IOException {
	generateNode(ctxt,gen,node,null,null);
    }

    // Build the symbol table associated with the context
    //
    private void buildSymbolTable(Context ctxt, MibGenerator gen,MibNode node) 
	throws IOException {
	buildSymbolTable(ctxt,gen,node,null,null);
    }

    private void buildSymbolTable(Context ctxt, MibGenerator gen, 
				  MibNode node, 
				  MibNode group, String domain) 
	throws IOException {
    

	// If the node does not contain an object definition, it could be 
	// a group. It is a group if one of the children contains an Objet 
	// definition.
	//
	if (node.isGroup()) {
	    try {
		node.isValidGroup();
	    } catch (IOException e) {
		String oid= e.getMessage();
		String nodeOid = getClassName(node.getComputedOid());
		String msg =
		    MessageHandler.getMessage("generate.error.mib",
					      nodeOid,
					      getClassName(oid));
		//gen.closeCode();
		gen.endMib();
		throw e;
	    }
        
	    groupGenerator.buildSymbolTable(node,ctxt);

	    if (!node.hasNestedGroups()) 
		return;

	    // java.lang.System.out.print("Nesting:");
	    String symbol = getSymbolName(ctxt,node);
	    if (symbol != null) {
		// java.lang.System.out.println(" symbol=" + symbol);
		domain = appendName(domain,symbol);
		// java.lang.System.out.println("Nesting: domain=" + domain);
	    } else {
		// java.lang.System.out.println(" symbol=null");
	    }
	    group = node;
	}
    
	// keep going in the tree ...
	//
	Hashtable children= node.getChildren();
	for(Enumeration e= children.elements(); e.hasMoreElements();) {
	    buildSymbolTable(ctxt, gen, (MibNode) e.nextElement(), 
			     group, domain);
	}
    }
  
    private void generateNode(Context ctxt, MibGenerator gen, MibNode node, 
			      MibNode group, String domain) 
	throws IOException {
    

	// If the node does not contain an object definition, it could be 
	// a group. It is a group if one of the children contains an Objet 
	// definition.
	//
	if (node.isGroup()) {
	    try {
		node.isValidGroup();
	    } catch (IOException e) {
		String oid= e.getMessage();
		String nodeOid = getClassName(node.getComputedOid());
		String msg =
		    MessageHandler.getMessage("generate.error.mib",
					      nodeOid,
					      getClassName(oid));
		//gen.closeCode();
		gen.endMib();
		throw e;
	    }
        
	    // try {
	    groupGenerator.generateCode(node,ctxt);
	    gen.registerNode(node,ctxt,domain);
	    //}catch(Exception e) {
	    //e.printStackTrace();
	    //}
	    if (!node.hasNestedGroups()) 
		return;
	    // java.lang.System.out.print("Nesting:");
	    String symbol = getSymbolName(ctxt,node);
	    if (symbol != null) {
		// java.lang.System.out.println(" symbol=" + symbol);
		domain = appendName(domain,symbol);
		// java.lang.System.out.println("Nesting: domain=" + domain);
	    } else {
		// java.lang.System.out.println(" symbol=null");
	    }
	    group = node;
	}
    
	// keep going in the tree ...
	//
	Hashtable children= node.getChildren();
	for(Enumeration e= children.elements(); e.hasMoreElements();) {
	    generateNode(ctxt, gen, (MibNode) e.nextElement(), group, domain);
	}
    }
  
    public String appendName(String domain, String name) {
	if (domain == null) return name;
	if (name == null)   return domain;
	return new String(domain+domainSeparator+name);
    }

    public static String getSymbolName(Context ctxt, MibNode node) {
	if (node == null) return null;
	if (node.getSymbolName() != null) 
	    return node.getSymbolName();
        String symbol = ctxt.mib.getAssociatedSymbol(node.getComputedOid());
	if (symbol == null) return null;

        // Trimming operation ...
        //
        String result= symbol.trim();
        result= result.replace('-', '_');
        result= result.replace('.', '_');
        result= result.replace(',', '_');
        result= result.replace(',', '_');
        result= result.replace(' ', '_');
        return Character.toUpperCase(result.charAt(0)) + 
	    result.substring(1, result.length());
    }

    public String setDomainSeparator(String s) {
	if (s!= null) domainSeparator = s;
	return domainSeparator;
    }

    protected GroupGenerator groupGenerator = null;
    protected String domainSeparator = "_";
    
}
