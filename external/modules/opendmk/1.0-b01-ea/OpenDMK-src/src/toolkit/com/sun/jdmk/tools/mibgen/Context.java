/* 
 * @(#)file      Context.java 
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
 */ 


package com.sun.jdmk.tools.mibgen;



import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * This class is intended to carry contextual information about
 * a mib generation.
 * Things like:
 * <li>prefixes</li>
 * <li>the mib being generated</li>
 * <li>contextual translation tables</li>
 * <li>etc...</li>
 * should eventually be stored in this object.
 *
 * Ideally, all the contextual information necessary to the generation
 * of a particular MIB should be placed in there, rather than in the
 * set of generators instantiated for generating this mib.
 * In other words, the generators should contain the code (how to generate)
 * and the context should contain the data (the set of parameters that help
 * the generators decide what to generate).
 *
 * The context should eventually be passed as parameters to all the
 * generator's methods (rather than being stored in the generators).
 * This would allow to reuse the generators over several generations.
 *
 **/
public class Context {

    public Context() {
    }

    public int     gentype            = 0;

    // if true => 
    //    will generate accessTableXxxx() method in group MBean interface.
    //
    public boolean genItfTableAccess  = true;

    // if true => 
    //    will generate getXxxx() method in group MBean interface for
    //    accessible-for-notification objects.
    //
    public boolean genAFNGetter  = true;

    public String  prefix             = "";
    public String  genericPrefix      = "";
    public String  standardPrefix     = "";
    public String  defaultOidPrefix   = "oid_";
    public String  dir                = null;
    public ASTMib  mib                = null;
    public String  packageName        = null;
    public MibGenerator   mibgen     = null;
    public CodeGenerator  codegen    = null;
    public ModulesHandler modules    = null;
    public Hashtable javaSyntaxTable = null;
    public Hashtable typeRefTable    = null;

    /**
     * Creates a new context containing the same information
     * This is mainly used to "inherit" data, simulating a
     * context stack.
     **/
    public Context duplicate() {
	Context result         = new Context();
	result.gentype         = gentype;
	result.genItfTableAccess = genItfTableAccess;
	result.genAFNGetter    = genAFNGetter;
	result.prefix          = prefix;
	result.mib             = mib;
	result.packageName     = packageName;
	result.mibgen          = mibgen;
	result.codegen         = codegen;
	result.modules         = modules;
	result.dir             = dir;
	result.genericPrefix   = genericPrefix;
	result.standardPrefix  = standardPrefix;
	result.javaSyntaxTable = getJavaSyntaxTable();
	result.typeRefTable    = getTypeRefTable();
	return result;
    }

    /**
     * The javaSyntaxTable is built during generation and contains
     * a mapping <variable-name> => <associated Java syntax>
     *
     * This is a tricky thing because the syntax we're looking for
     * at a given point might be defined in a MIB which has not
     * been generated yet. 
     *
     **/
    public Hashtable getJavaSyntaxTable() {
	if (javaSyntaxTable == null) {
	    javaSyntaxTable = new Hashtable();
	}
	return javaSyntaxTable;
    }

    /**
     * The typeRefTable is built during generation and contains
     * a mapping <variable-name> => <associated type reference>
     *
     **/
    public Hashtable getTypeRefTable() {
	if (typeRefTable == null) {
	    typeRefTable = new Hashtable();
	}
	return typeRefTable;
    }

    /**
     * Sets the Java syntax associated to a given varName in
     * the javaSyntaxTable.
     *
     **/
    public void setJavaSyntax(String varName, String strSyntax) {
	Hashtable t = getJavaSyntaxTable();
	t.put(varName, strSyntax);
    }

    /**
     * Sets the Java syntax associated to a given varName in
     * the javaSyntaxTable.
     *
     **/
    public void setIndexSyntax(String varName, ASTNamedType syntax) {
	final String strSyntax;
        if (syntax.isEnumeratedType()) {
	    strSyntax = EnumGenerator.getEnumClassName(this,varName,
					syntax.getEnumeratedDef());
        } else {  
            strSyntax= syntax.getMbeanSyntax();
        }
	setJavaSyntax(varName, strSyntax);
    }

    /**
     * Sets the  type reference associated to a given varName in
     * the typeRefTable.
     *
     **/
    public void setTypeRef(String varName, String typeRef) {
	Hashtable t = getTypeRefTable();
	t.put(varName, typeRef);
    }

    /**
     * Look up the Java syntax associated to a given varName in
     * the javaSyntaxTable.
     *
     **/
    public String getJavaSyntax(String varName) {
	Hashtable t = getJavaSyntaxTable();
	return (String) t.get(varName);
    }

    /**
     * Look up the type reference associated to a given varName in
     * the javaSyntaxTable.
     *
     **/
    public String getTypeRef(String varName) {
	Hashtable t = getTypeRefTable();
	return (String) t.get(varName);
    }

    /**
     * The javaSyntaxTable is built during generation and contains
     * a mapping <variable-name> => <associated Java syntax>
     *
     * This is a tricky thing because the syntax we're looking for
     * at a given point might be defined in a MIB which has not
     * been generated yet.
     *
     * This function first look up the symbol in the javaSyntaxTable,
     * and if it is not found, asks the CodeGenerator to add it.
     *
     **/
    public void addExternalSymbol(String symbolName) {

	// If the symbol is already there, no need to go further
	//
	if (getJavaSyntax(symbolName) != null) return;

	// We don't have the symbol yet... Ask the code generator
	// to add it - this may happen if the MIB containing this
	// symbol has not been generated yet. 
	//
	if (codegen == null) return;
	codegen.addExternalSymbol(this,symbolName);
    }

}
