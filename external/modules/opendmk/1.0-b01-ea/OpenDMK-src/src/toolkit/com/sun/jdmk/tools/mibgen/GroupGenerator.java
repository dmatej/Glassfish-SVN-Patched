/*
 * @(#)file      GroupGenerator.java
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
 * The class corresponds to the code generator (back-end)
 *
 */
public class GroupGenerator extends Generator implements Serializable {
  
    public GroupGenerator(ResourceManager mgr, Context ctxt) {
        super(mgr, ctxt);
    }
  
    public void generateCode(MibNode aGroup, Context ctxt) 
	throws IOException {
    
        // Start code generation. Create a Bean generator for handling code
        // generation for the m-bean.
        //
        MbeanGenerator beanGen= new MbeanGenerator(manager, aGroup, ctxt);
    
        // Create a BeanIf generator for handling code generation for the interface m-bean.
        //
        MbeanIfGenerator beanIfGen= new MbeanIfGenerator(manager, aGroup, ctxt);
    
        // Create a meta data generator
        //
        MetaBeanGenerator metaGen= 
	    mkMetaBeanGenerator(manager, aGroup, ctxt);
						     
        // Process each single element contain in the group
        //
        for(Enumeration e= (aGroup.getChildren()).elements(); e. hasMoreElements(); ) {
            MibNode aNode= (MibNode) e.nextElement();
            beanGen.handleNode(aNode);
            beanIfGen.handleNode(aNode);
            metaGen.handleNode(aNode);
        }
        beanGen.endOfGroup();
        beanIfGen.endOfGroup();
        metaGen.endOfGroup();
    }

    public void buildSymbolTable(MibNode aGroup, Context ctxt) 
	throws IOException {
    
        // Process each single element contain in the group
        //
        for(Enumeration e= (aGroup.getChildren()).elements(); 
	    e. hasMoreElements(); ) {
            MibNode aNode= (MibNode) e.nextElement();
	    if (aNode.isAssociated()) {
		if (aNode.isTable()) {
		    buildTableSymbolTable(aNode,ctxt);
		} else {
		    buildVariableSymbolTable(aNode,ctxt);
		}
	    }
	}
    }


    protected void buildTableSymbolTable(MibNode aTable, Context ctxt) 
	throws IOException {
    
        // Process each single element contain in the group
        //
        for(Enumeration e= (aTable.getChildren()).elements(); 
	    e. hasMoreElements(); ) {
            MibNode aNode= (MibNode) e.nextElement();
	    if (aNode.isAssociated()) {
		// We have the Entry node, and we handle it like a
		// group.
		buildSymbolTable(aNode,ctxt);
	    } 
	}
    }

    public void addExternalSymbol(Context ctxt, String symbol) {
	if (ctxt == null) return;
	if (ctxt.modules == null) return;

	final MibNode aNode = ctxt.modules.findNodeWithName(symbol);
	if (aNode == null) return;
	if (aNode.isVariable())  
	    buildVariableSymbolTable(aNode, ctxt);
    }

    protected void buildVariableSymbolTable(MibNode aNode, Context ctxt) {
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
        String strSyntax= null;
    
        // Get the default MIB variable value
        //
        ASTValue defValue=definition.getDefValue();
        
        if (syntax.isEnumeratedType()) {
            // get the access mode of the node
            //
            strSyntax= getEnumClassName(context,varName,
					syntax.getEnumeratedDef());
            aNode.setEnumerated(true);
            aNode.setEnumeratedType(strSyntax);
        } else {  
            // Get the real syntax to use for the node
            //  
            strSyntax= syntax.getMbeanSyntax();
        }

	String typeRef = syntax.getTypeReferenceName();
	if (typeRef != null) 
	    ctxt.setTypeRef(varName,typeRef);
    
	ctxt.setJavaSyntax(varName,strSyntax);
     }

    public String getEnumClassName(Context ctxt, String var,
					 Enumerated enumeration) {
	// See also Context.setIndexSyntax()
	return EnumGenerator.getEnumClassName(ctxt,var,enumeration);
    }

    public MetaBeanGenerator 
	mkMetaBeanGenerator(ResourceManager mgr, MibNode aGroup, Context ctxt) 
	throws IOException {
	// If we need to optimize we can replace this test with
	// aGroup.hasNestedGroups()
	if (aGroup.isGroup()) {
	    return new MetaGroupGenerator(mgr, aGroup, ctxt);
	} 
	return new MetaBeanGenerator(mgr, aGroup, ctxt);
    }

}
