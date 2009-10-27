/*
 * @(#)file      MetaBeanGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.43
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
 * The class generates code required for providing a SNMP view of  a m-bean
 *
 */
public class MetaBeanGenerator extends BeanGenerator implements Serializable {
  
    public static String buildMetaName(Context ctxt, String pre, 
				       String name) {
 	if (ctxt.gentype == MetaBeanGenerator.GENERIC_META)
	    return pre + name + ctxt.genericPrefix + Def.METAPREFIX;
 	if (ctxt.gentype == MetaBeanGenerator.STANDARD_META)
	    return pre + name + ctxt.standardPrefix + Def.METAPREFIX;
        return pre + name + Def.METAPREFIX;
    }

    public static String getNodeSymbolName(Context ctxt, MibNode node) {
        String vName= node.getSymbolName();
        if (vName == null)
            vName= Generator.getClassName(ctxt, node.getComputedOid());
        String sName = buildMetaName(ctxt, ctxt.prefix, vName);
	return sName;
    }

    String getNodeSymbolName(MibNode node) {
	 return getNodeSymbolName(context,node);
    }

    public MetaBeanGenerator(ResourceManager mgr, MibNode aGroup, 
			     Context ctxt) throws IOException {
        super(mgr, aGroup, ctxt);
    
	// Meta Generation Type
	//
	gentype = ctxt.gentype;

        // Specify oid of the current bean ...
        //
        oid= node.getComputedOid();
    
        // Try to find a symbol to associate to the group
        //
        varName= node.getSymbolName();
        if (varName == null)
            varName = getClassName(node.getComputedOid());
        symboleName = getNodeSymbolName(node);
     
        Trace.info(MessageHandler.getMessage("generate.info.meta", varName));
	
        // Open the file which will represent the M-bean.
        //
        out= openFile(symboleName+ Def.JAVA);

	// Initialize the string buffers
	//
	initBuffers();

        // Write generic header ...
        //
        writeHeader();
     
        // write our own header ...
        //
        writeClassDeclaration();
     
        // Build list of variables
        //
        buildVarList();
     
        // write the beginning of the constructor
        //
        buildConstructorHeader();

	// build method headers
        //
	buildMethodHeaders();

    }

    public void setContext(Context ctxt) {
	super.setContext(ctxt);
	gentype = ctxt.gentype;
    }

    boolean isStandard() {
	return ((gentype & STANDARD_META) != 0);
    }

    boolean isGeneric() {
	return ((gentype & GENERIC_META) != 0);
    }

    protected void initBuffers() {
	isVariable_impl      =  new StringBuffer();
	isVariableBody_impl  =  new StringBuffer();
	isReadable_impl      =  new StringBuffer();
	isReadableBody_impl  =  new StringBuffer();
	skipVarCount         =  0;
	totalSkipVarCount    =  0;
	skipVar_impl         =  new StringBuffer();
	skipVarBody_impl     =  new StringBuffer();
	// OBSOLETE
	// --------
	// getNext_impl  =  new StringBuffer();
	// get_impl      =  new StringBuffer();
	// set_impl      =  new StringBuffer();
	// check_impl    =  new StringBuffer();

	if (isStandard()) {
	    getVar_impl   =  new StringBuffer();
	    setVar_impl   =  new StringBuffer();
	    checkVar_impl =  new StringBuffer();
	    moi_impl      =  new StringBuffer();
	}

	getReq_impl   =  new StringBuffer();
	setReq_impl   =  new StringBuffer();
	checkReq_impl =  new StringBuffer();

	init_impl     =  new StringBuffer();
	var_def       =  new StringBuffer();
	objsrv_impl   =  new StringBuffer();
	constructor   =  new StringBuffer();
	accessors     =  new StringBuffer();

	if (isGeneric()) {
	    buildval_impl = new StringBuffer();
	    buildatt_impl = new StringBuffer();
	    checkset_impl = new StringBuffer();
	    checkget_impl = new StringBuffer();
	    objname_impl  = new StringBuffer();
	}

	attname_impl  = new StringBuffer();
    }

    // Can be extended by subclasses if new methods need to be
    // added.
    protected void buildMethodHeaders() throws IOException {
        // write assignment of m-bean
        //
	if (isStandard()) 
	    buildSetInstance();
 
	if (isGeneric()) 
	    buildSetObjectName();

	// setObjectServer()
	// 
	// buildSetObjectServer();

        // build the common part of the get/set/check method
        // =================================================


	// GetVarHeader()   is get(id,userData)
	// SetVarHeader()   is set(value,id,userData)
	// CheckVarHeader() is check(value,id,userData)
	//
	// => it is part of the SnmpStandardMetaServer interface
	//
	if (isStandard()) {
	    buildGetVarHeader();
	    buildSetVarHeader();
	    buildCheckVarHeader();
	}

        buildGetAttNameHeader();

	if (isGeneric()) {
	    buildBuildValHeader();
	    buildBuildAttHeader();
	    buildCheckSetAccessHeader();
	    buildCheckGetAccessHeader();
	}

	// Get is the implementation of the abstract   get(req,depth)
	// Set is the implementation of the abstract   set(req,depth)
	// Check is the implementation of the abstract check(req,depth)
	//
	// => it is the implementation of the abstract part of 
	//    SnmpMibNode
	//
        buildGet();
        buildSet();
	buildCheck();


	// build the header of the isVariable() method
        //
	buildIsVariableHeader();

	// build the header of the isReadable() method
        //
	buildIsReadableHeader();

	// build the header of the skipVariable() method
        //
	buildSkipVarHeader();

    }


    protected  void closeMethods() throws IOException {
        // Close the get stuff (switch included)
        //
        // closeGetterMeth();
        // closeSetterMeth();
        // closeCheckerMeth();
        // closeGetNext();
	if (isStandard()) {
	    closeGetVarMeth();
	    closeSetVarMeth();
	    closeCheckVarMeth();
	}
        closeGetAttNameMeth();
	if (isGeneric()) {
	    closeBuildValMeth();
	    closeBuildAttMeth();
	    closeCheckSetAccessMeth();
	    closeCheckGetAccessMeth();
	}
	closeIsVariableMeth();
	closeIsReadableMeth();
	closeSkipVarMeth();
    }

    // Write methods - can be extended by subclasses
    protected  void writeMethods() throws IOException {
        // write(get_impl.toString());
        // write(getNext_impl.toString());
        // write(set_impl.toString());
        // write(check_impl.toString());
	if (isStandard()) {
	    write(getVar_impl.toString());
	    write(setVar_impl.toString());
	    write(checkVar_impl.toString());
	    write(moi_impl.toString());
	}

        write(getReq_impl.toString());
        write(setReq_impl.toString());
        write(checkReq_impl.toString());
        write(isVariable_impl.toString());
        write(isReadable_impl.toString());
	write(skipVar_impl.toString());
        write(objsrv_impl.toString());
        write(attname_impl.toString());

	if (isGeneric()) {
	    write(buildval_impl.toString());
	    write(buildatt_impl.toString());
	    write(checkset_impl.toString());
	    write(checkget_impl.toString());
	    write(objname_impl.toString());
	}
    }

    public void endOfGroup() throws IOException {

	closeMethods();
        closeConstructor();
        write(constructor.toString());
	writeMethods();
        write(var_def.toString());
        write(Def.RBRACE);
        closeIO();
    }
  
    public void handleNode(MibNode aNode) throws IOException {
    
        String varName=  aNode.getSymbolName();
        String id= aNode.getNodeId();
    
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
            Trace.info(MessageHandler.getMessage("generate.info.var",  
						 aNode.getSymbolName()));
            handleTable(aNode);
      
            // Add the id of the table in the list of indexes.
            //
            handleInit(id, varName,  ParserConstants.RO);
            return;
        }
    
        // Get node info
        //
        ASTObjectTypeDefinition definition= aNode.getObjectType();
        int access= definition.getDefinition().getAccess();
        ASTNamedType syntaxObject=definition.getSyntax();
        String syntax= syntaxObject.getSnmpSyntax();
        aNode.setSnmpSyntax(syntax);

	String enumtype = null;

        if (aNode.isEnumeratedType()) {
	    enumtype = aNode.getEnumeratedType();
        }
    
    
        // Take care of getNext stuff
        //
        handleMinMax(id);
    
	//Take care of the isVariable() stuff
	//
        handleVariableId(id, varName, syntax, enumtype, access); 

	String refName = context.getTypeRef(varName);
	// if (refName != null) {
	// java.lang.System.out.println(varName + ": syntax is " +
	//        refName);
	//    if (refName.equals("RowStatus")) {
	//	 java.lang.System.out.println("\n****" + varName + 
	//				     ": syntax is " +
	//				     refName + "\n" );
	//    }
	// }

        //Take care of the get
        //
        // handleGet(id, varName, syntax, access); 
	if (isStandard())
	    handleGetVar(id, varName, syntax, access); 
        handleGetAttName(id, varName, syntax, access); 
    
	// Take care of the check
	//
	// handleCheck(id, varName, syntax, enumtype, access);    
	if (isStandard()) {
	    if(!isRowStatus(context, varName) || 
	       MibGenProperties.isDeprecatedEnabled())
		handleCheckVar(id, varName, syntax, enumtype, access);
	}

	// Take care of the set ...
	//
	// handleSet(id, varName, syntax, enumtype, access);
	if (isStandard())
	    handleSetVar(id, varName, syntax, enumtype, access);

	if (isGeneric()) {
	    handleBuildVal(id, varName, syntax, enumtype, access);
	    handleBuildAtt(id, varName, syntax, enumtype, access);
	    handleCheckSetAccess(id, varName, syntax, enumtype, access);    
	    handleCheckGetAccess(id, varName, syntax, enumtype, access);    
	}

        // Take care of initializing index list
        //
        handleInit(id, varName, access);
	varCount++;
    }
  
  
    /**
     * Process nested groups.
     */
    protected void handleNestedGroups(MibNode node) throws IOException {
	// java.lang.System.out.println(node.getComputedOid()+
	//			     ": is or has NESTED Groups!!!");
	//
	// Do nothing... Nested groups are handled at the upper level
    }

    // ----------------------------------------------------------------
    // Generates code for tables in all the methods that needs
    // to.
    // ----------------------------------------------------------------

    protected void handleTableId(MibNode aNode,String name,String id) 
	throws IOException {
	// These are all obsolete
	//
	// handleGetterTable(aNode,name,id);
	// handleGetNextTable(aNode,name,id);
	// handleSetterTable(aNode,name,id);
	// handleCheckerTable(aNode,name,id);
	if (isStandard()) {
	    handleGetVarTable(aNode,name,id);
	    handleSetVarTable(aNode,name,id);
	    handleCheckVarTable(aNode,name,id);
	}
	handleGetAttNameTable(aNode,name,id);
	if (isGeneric()) {
	    handleBuildValTable(aNode,name,id);
	    handleBuildAttTable(aNode,name,id);
	    handleCheckSetAccessTable(aNode,name,id);
	    handleCheckGetAccessTable(aNode,name,id);
	}
    }

    // ----------------------------------------------------------------
    // Generates code for tables in get(long var, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleGetVarTable(MibNode aNode, String varName, 
				     String id) throws IOException {
	// case <id> : {
	//    throw new SnmpStatusException(SnmpStatusException.noAccess);
	//    }
        getVar_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        getVar_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			   "(" + Def.EXCP_SNMP + "." + Def.V_NOSUCHINSTANCE +
			   ")" + Def.SEMICOLON + Def.TAB4 + Def.RBRACE 
			   + "\n" );
    }

    // ----------------------------------------------------------------
    // Generates code for tables in get(long var, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleGetAttNameTable(MibNode aNode, String varName, 
					 String id) throws IOException {
	// case <id> : {
	//    throw new SnmpStatusException(SnmpStatusException.noAccess);
	//    }
        attname_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        attname_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			    "(" + Def.EXCP_SNMP + "." + Def.V_NOSUCHINSTANCE +
			    ")" + Def.SEMICOLON + Def.TAB4 + Def.RBRACE +
			    "\n" );
    }

    // ----------------------------------------------------------------
    // Generates code for tables in get(SnmpValue x,long var, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleSetVarTable(MibNode aNode, String varName, 
				     String id) throws IOException {
	// case <id> : {
	//    throw new SnmpStatusException(SnmpStatusException.noAccess);
	//    }
        setVar_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        setVar_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			   "(" + Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" 
			   + Def.SEMICOLON + Def.TAB4 + Def.RBRACE + "\n" );
    }

    // ----------------------------------------------------------------
    // Generates code for tables in buildSnmpValue(long var, Object att)
    // ----------------------------------------------------------------
    //
    protected void handleBuildValTable(MibNode aNode, String varName, 
				     String id) throws IOException {
	// case <id> : {
	//    throw new SnmpStatusException(SnmpStatusException.noAccess);
	//    }
        buildval_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        buildval_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			     "(" + Def.EXCP_SNMP + "." + 
			     Def.V_NOSUCHINSTANCE + ")" +
			     Def.SEMICOLON + Def.TAB4 + Def.RBRACE + "\n" );
    }
    
    // ----------------------------------------------------------------
    // Generates code for tables in buildAttributeValue(long var, SnmpValue x)
    // ----------------------------------------------------------------
    //
    protected void handleBuildAttTable(MibNode aNode, String varName, 
				     String id) throws IOException {
	// case <id> : {
	//    throw new SnmpStatusException(SnmpStatusException.noAccess);
	//    }
        buildatt_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        buildatt_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			     "(" + Def.EXCP_SNMP + "." + 
			     Def.V_NOSUCHINSTANCE + ")" + Def.SEMICOLON + 
			     Def.TAB4 + Def.RBRACE + "\n" );
    }
    
    // ----------------------------------------------------------------
    // Generates code for tables in check(SnmpValue x,long id, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleCheckVarTable(MibNode aNode, String varName, 
				     String id) throws IOException {
	// case <id> : {
	//    throw new SnmpStatusException(SnmpStatusException.noAccess);
	//    }
        checkVar_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        checkVar_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			     "(" + Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + 
			     ")" + Def.SEMICOLON + Def.TAB4 + Def.RBRACE +
			     "\n" );
    
    }

    // ----------------------------------------------------------------
    // Generates code for tables in 
    //    checkSetAccess(SnmpValue x,long id, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleCheckSetAccessTable(MibNode aNode, String varName, 
				     String id) throws IOException {
	// case <id> : {
	//    throw new 
	//        SnmpStatusException(SnmpStatusException.snmpRspotWritable);
	//    }
        checkset_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        checkset_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			     "(" + Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + 
			     ")" + Def.SEMICOLON + Def.TAB4 + Def.RBRACE +
			     "\n" );
    
    }

    // ----------------------------------------------------------------
    // Generates code for tables in 
    //    checkGetAccess(SnmpValue x,long id, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleCheckGetAccessTable(MibNode aNode, String varName, 
					     String id) throws IOException {
	// case <id> : {
	//    throw new 
	//       SnmpStatusException(SnmpStatusException.noSuchInstance);
	//    }
        checkget_impl.append(Def.TAB3 + "case " + id + ":" + Def.LBRACE);
        checkget_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + 
			     "(" + Def.EXCP_SNMP + "." + 
			     Def.V_NOSUCHINSTANCE + ")" + Def.SEMICOLON + 
			     Def.TAB4 + Def.RBRACE + "\n" );
    
    }

    /**
     * Process table.
     */
    protected void handleTable(MibNode aNode) throws IOException {
        // The node is a table. So first we need to access the table through 
        // the reference object.
        //
        String varName=  aNode.getSymbolName();
        String id= aNode.getNodeId();
    
	// Generates code for table in accessors
	//
	handleTableId(aNode,varName,id);

	// Now generate code for the table itself
	//
        MetaTableGenerator metatable= 
	    new MetaTableGenerator(manager, aNode, context);

	metatable.generateCode();

    }
  
    /**
     * Build array containing list of readable indexes ...
     */
    protected void handleInit(String id, String var, int acc) 
	throws IOException {
        if ((acc == ParserConstants.RO)||
            (acc == ParserConstants.RC)||
            (acc == ParserConstants.RW)) {
            init_impl.append(Def.TAB2 + Def.SNMP_VARLIST + "[" + 
			     String.valueOf(counter) + "] = " +
                             id + Def.SEMICOLON);
            counter ++;
        }
    }
  
    // ----------------------------------------------------------------
    // Returns true if the variable is of type RowStatus
    // ----------------------------------------------------------------
    // 4520460 => moved to super class.
    // protected boolean isRowStatus(Context ctxt, String var) {
    //	String typeRef = ctxt.getTypeRef(var);
    //	if (typeRef == null) return false;
    //	return typeRef.equals("RowStatus");
    // }

    // ----------------------------------------------------------------
    // Generates code for variables in methods that needs to
    // ----------------------------------------------------------------
    //
    protected void handleVariableId(String id, String var, String entry, 
				    String enumtype, int acc) 
	throws IOException {
        handleIsVariable(id,var,entry,acc);
        handleIsReadable(id,var,entry,acc);
        handleSkipVar(id,var,entry,acc);
    }

    
    // ----------------------------------------------------------------
    // Generates code for variables in boolean isVariable(long id)
    // ----------------------------------------------------------------
    //
    protected void handleIsVariable(String id, String var, String entry, 
				    int acc)
	throws IOException {
	// case <id> :
	//
        isVariableBody_impl.append(Def.TAB3 + "case " + id + ":\n");
    }

    // ----------------------------------------------------------------
    // Generates code for variables in boolean isVariable(long id)
    // ----------------------------------------------------------------
    //
    protected void handleIsReadable(String id, String var, String entry, 
				    int acc)
	throws IOException {
        if ((acc == ParserConstants.WO)||
            (acc == ParserConstants.NA) ||
            (acc == ParserConstants.AFN)) return;
	readCount++;
	// case <id> :
	//
        isReadableBody_impl.append(Def.TAB3 + "case " + id + ":\n");
    }

    // ----------------------------------------------------------------
    // Generates code for variables in get(long id, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleGetVar(String id, String var, String entry, int acc) 
	throws IOException {

	// case <id> :
	//
        getVar_impl.append(Def.TAB3 + "case " + id + ":\n" +  Def.TAB4);
        if ((acc == ParserConstants.WO)||
            (acc == ParserConstants.NA) ||
            (acc == ParserConstants.AFN)) {

            // we need to throw an exception ...
            //
	    // throw new 
	    //     SnmpStatusException(SnmpStatusException.noSuchInstance);
	    //
            getVar_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." + Def.V_NOSUCHINSTANCE +
			       ")" + Def.SEMICOLON);
	    // Bug Fix: 4306630 - due to write-only object
	    // getVar_impl.append(Def.TAB4 + "break" + Def.SEMICOLON + "\n");
            return;
        }

        // What SNMPValue shall we use ?
        //
        String syntax= SyntaxMapper.getTypeName(entry);

        // get the value ...
        //
	// return new <SnmpSyntax>(node.get<VarName>());
	//
        getVar_impl.append("return new "+ syntax + "(" +
			   MBEAN_NAME + "." + Def.GET + var + "())" + 
			   Def.SEMICOLON + "\n");
    }
  
    protected void genSkipVarCode(StringBuffer result) 
	throws IOException {
	result.append(Def.TAB4 + "if (pduVersion==" + 
		      Def.C_SNMP_VERSION1 +") return "+ 
		      "true" + Def.SEMICOLON);
	result.append(Def.TAB4 + "break" +  Def.SEMICOLON);
    }

    // ----------------------------------------------------------------
    // Generates code for variables in 
    //       skipVariable(long id, Object data, int pduVersion)
    // ----------------------------------------------------------------
    //
    protected void handleSkipVar(String id, String var, String entry, int acc) 
	throws IOException {
	// System.out.println("handleSkipVar: id=" + id+ ", var="+var+
	//		   ", entry="+entry+", acc=" + acc);

        if ((acc == ParserConstants.WO)||
            (acc == ParserConstants.NA) ||
            (acc == ParserConstants.AFN)) {

	    if (skipVarCount > 0) {
		genSkipVarCode(skipVarBody_impl);
		skipVarCount = 0;
	    }
	    // case <id> :
	    //    return true;
	    skipVarBody_impl.append(Def.TAB3+"case " + id + ":\n"+ Def.TAB4);
            skipVarBody_impl.append("return true"+ Def.SEMICOLON);
	    totalSkipVarCount++;
            return;
        }

        // If we have a V2 syntax type (i.e. SnmpCounter64) and the
        // version is V1, then we skip the variable
	// case <id> :
	// case <id> : 
	//    .....
	//    if (pduVersion==SnmpDefinitions.snmpVersionOne) return true;
	//    break;
        if (SyntaxMapper.isV2Syntax(entry)) {
	    skipVarBody_impl.append(Def.TAB3 + "case " + id + ":\n");
	    skipVarCount++;
	    totalSkipVarCount++;
	}

	 
	// otherwise, use default behavior, which is return false
	// or call super.skipVariable();
	// This is done will be handled by the default: case - so we
	// don't generate anything here.
	// 

    }
  
    // ----------------------------------------------------------------
    // Generates code for variables in getAttributeName(long id)
    // ----------------------------------------------------------------
    //
    protected void handleGetAttName(String id, String var, String entry, 
				    int acc) 
	throws IOException {

	// [Revisit: XXX]
	// should use an array or a hashtable 

	// case <id> :
	//
        attname_impl.append(Def.TAB3 + "case " + id + ":\n");

	// return new <SnmpSyntax>(node.get<VarName>());
	//
        attname_impl.append(Def.TAB4 + "return \"" + var + "\"" +
			    Def.SEMICOLON + "\n");
    }
  
    // ----------------------------------------------------------------
    // Generates code for variables in set(SnmpValue x,long id, Object data)
    // ----------------------------------------------------------------
    //
    protected void handleSetVar(String id, String var, String entry,
				String enumeratiom, int acc) 
                                throws IOException {
        setVar_impl.append(Def.TAB3 + "case " + id + ":\n" +  Def.TAB4);
        if (acc == ParserConstants.RO) {
            // Send a read-only exception ....
            //
	    // throw new SnmpStatusException(SnmpStatusException.readOnly);
	    //
            setVar_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" + 
			       Def.SEMICOLON + "\n");
            return;
        }
    
        if ((acc == ParserConstants.NA)||
            (acc == ParserConstants.AFN)) {
            // we need to throw a specific exception ...
            //
	    // throw new SnmpStatusException(SnmpStatusException.noAccess);
	    //
            setVar_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" +
			       Def.SEMICOLON + "\n");
            return;
        }
    
        // Now we are left with a RW or WO variable ...
        //
        String type= SyntaxMapper.getTypeName(entry);
        String meth= SyntaxMapper.getCastMethod(entry);
    
        // Check the type of the value ...
        //
	// if (x instanceof <SnmpType>) {
	// ....}
	//
        setVar_impl.append("if (x instanceof " + type +")" + Def.LBRACE + 
			Def.TAB5);
		    
        // First set the value ...
        //
        if (enumeratiom == null)
	    // {....
	    //   <mbean>.set<VarName>(((<SnmpType>)x).to<JavaType>());
	    //   return new <SnmpType>(<mbean>.get<VarName>());
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
            setVar_impl.append(MBEAN_NAME + "." + Def.SET + var + 
			       "(((" + type + ")x)." + meth + "())" + 
			       Def.SEMICOLON + Def.TAB5 + 
			       "return new " + type + "(" + MBEAN_NAME + "." 
			       + Def.GET + var + "())" + 
			       Def.SEMICOLON + Def.TAB4 +
			       "} else {\n" + Def.TAB5 +
			       Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
			       Def.SEMICOLON + Def.TAB4 + Def.RBRACE +"\n");
        else 
	    // {....
	    //    try {
	    //       <mbean>.set<VarName>( 
	    //           new <EnumType>(((<SnmpType>)x).to<JavaType>());
	    //    } catch (IllegalArgumentException e) {
	    //       throw new SnmpStatusException(
	    //           SnmpStatusException.badValue);
	    //    }
	    //    return new <SnmpType>(<mbean>.get<VarName>());
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
            setVar_impl.append("try " + Def.LBRACE + Def.TAB6 +
			       MBEAN_NAME + "." + Def.SET + var + "( new " + 
			       enumeratiom + "(((" + type + ")x)." + meth +"()))" +
			       Def.SEMICOLON + Def.TAB5 + Def.N_RBRACE + 
			       " catch(" + Def.EXCP_ARGUMENT + " e) " + 
			       Def.LBRACE + Def.TAB6 + 
			       Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." +Def.V_WRONGVALUE + ")" + 
			       Def.SEMICOLON + Def.TAB5 + Def.RBRACE + 
			       Def.TAB5 +
			       "return new " + type + "(" + MBEAN_NAME + "." 
			       + Def.GET + var + "())" + 
			       Def.SEMICOLON + Def.TAB4 +
			       "} else {\n" + Def.TAB5 +
			       Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
			       Def.SEMICOLON + Def.TAB4 + Def.RBRACE +"\n");
    }
  
    // ----------------------------------------------------------------
    // Generates code for variables in buildSnmpValue(long id, Object att)
    // ----------------------------------------------------------------
    //
    protected void handleBuildVal(String id, String var, String entry,
				String enumeratiom, int acc) throws IOException {
        buildval_impl.append(Def.TAB3 + "case " + id + ":\n" +  Def.TAB4);

        String type=  SyntaxMapper.getTypeName(entry);
        String meth=  SyntaxMapper.getCastMethod(entry);
	String jtype= SyntaxMapper.getJavaSyntax(entry);
	// java.lang.System.out.println("Value: " + var + "(" + entry + ") : "
	//			     + type + ", " + meth + ", " + jtype);
		    
        // First set the value ...
        //
        if (enumeratiom == null) {
	    // Check the type of the value ...
	    //
	    // if (x instanceof <JavaType>) {
	    // ....} 
	    //
	    buildval_impl.append("if (x instanceof " + jtype +")" + 
			     Def.LBRACE + Def.TAB5);

	    // {....
	    //   return new <SnmpType>((<jtype>)x);
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
            buildval_impl.append("return new " + type + 
			       "((" + jtype + ")x)" +
			       Def.SEMICOLON + Def.TAB4 +
			       "} else {\n" + Def.TAB5 +
			       Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
			       Def.SEMICOLON + Def.TAB4 + Def.RBRACE +"\n");
        } else { 
	    // Check the type of the value ...
	    //
	    // if (x instanceof <JavaType>) {
	    // ....} 
	    //
	    buildval_impl.append("if (x instanceof " + enumeratiom +")" + 
			     Def.LBRACE + Def.TAB5);

	    // {....
	    //    try {
	    //       return new <SnmpType>((<EnumType>)x);
	    //    } catch (IllegalArgumentException e) {
	    //       throw new SnmpStatusException(
	    //           SnmpStatusException.badValue);
	    //    }
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
            buildval_impl.append("try " + Def.LBRACE + Def.TAB6 +
			       "return new " + type + "((" + enumeratiom + ")x)" +   
			       Def.SEMICOLON + Def.TAB5 + Def.N_RBRACE + 
			       " catch(" + Def.EXCP_ARGUMENT + " e) " + 
			       Def.LBRACE + Def.TAB6 + 
			       Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." +Def.V_WRONGVALUE + ")" + 
			       Def.SEMICOLON + Def.TAB5 + Def.RBRACE +
			       Def.TAB4 + "} else {\n" + Def.TAB5 +
			       Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			       Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
			       Def.SEMICOLON + Def.TAB4 + Def.RBRACE +"\n");
	}
    }
  
    // ----------------------------------------------------------------
    // Generates code for variables in 
    //    Object buildAttributeValue(long id, SnmpValue x)
    // ----------------------------------------------------------------
    //
    protected void handleBuildAtt(String id, String var, String entry,
				  String enumeratiom, int acc) 
                                  throws IOException {
        buildatt_impl.append(Def.TAB3 + "case " + id + ":\n" +  Def.TAB4);

        String type=  SyntaxMapper.getTypeName(entry);
        String meth=  SyntaxMapper.getCastMethod(entry);
	String jtype= SyntaxMapper.getJavaSyntax(entry);
	// java.lang.System.out.println("Value: " + var + "(" + entry + ") : "
	//			     + type + ", " + meth + ", " + jtype);
		    
        // First set the value ...
        //
        if (enumeratiom == null) {
	    // Check the type of the value ...
	    //
	    // if (x instanceof <SnmpValue>) {
	    // ....} 
	    //
	    buildatt_impl.append("if (x instanceof " + type +")" + 
			     Def.LBRACE + Def.TAB5);

	    // {....
	    //   return ((<SnmpValue>)x).to<JavaType>();
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
            buildatt_impl.append("return " + 
				 "((" + type + ")x)." + meth + "()" +
				 Def.SEMICOLON + Def.TAB4 +
				 "} else {\n" + Def.TAB5 +
				 Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
				 Def.SEMICOLON + Def.TAB4 + Def.RBRACE +"\n");
        } else { 
	    // Check the type of the value ...
	    //
	    // if (x instanceof <SnmpValue>) {
	    // ....} 
	    //
	    buildatt_impl.append("if (x instanceof " + type +")" + 
				 Def.LBRACE + Def.TAB5);

	    // {....
	    //    try {
	    //       return new <EnumType>(((<SnmpType>)x).toInteger());
	    //    } catch (IllegalArgumentException e) {
	    //       throw new SnmpStatusException(
	    //           SnmpStatusException.badValue);
	    //    }
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
            buildatt_impl.append("try " + Def.LBRACE + Def.TAB6 +
				 "return new " + enumeratiom + "(((" + 
                                type + ")x)." + meth + "())" +
				 Def.SEMICOLON + Def.TAB5 + Def.N_RBRACE + 
				 " catch(" + Def.EXCP_ARGUMENT + " e) " + 
				 Def.LBRACE + Def.TAB6 + 
				 Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." +Def.V_WRONGVALUE + ")" + 
				 Def.SEMICOLON + Def.TAB5 + Def.RBRACE +
				 Def.TAB4 + "} else {\n" + Def.TAB5 +
				 Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
				 Def.SEMICOLON + Def.TAB4 + Def.RBRACE +"\n");
	}
    }
  
    protected void handleCheckVar(String id, String var, String entry, 
				  String enumeratiom, int acc) 
                                  throws IOException {
        checkVar_impl.append(Def.TAB3 + "case " + id + ":\n" +  Def.TAB4);
        if (acc == ParserConstants.RO) {
            // Send a read-only exception ....
            //
	    // throw new SnmpStatusException(SnmpStatusException.readOnly);
	    //
            checkVar_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" 
				 + Def.SEMICOLON +  "\n");
            return;
        }
    
        if ((acc == ParserConstants.NA)||
            (acc == ParserConstants.AFN)) {
            // we need to throw a specific exception ...
            //
	    // throw new SnmpStatusException(SnmpStatusException.noAccess);
	    //
            checkVar_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" 
				 + Def.SEMICOLON +   "\n");
            return;
        }
    
        // Now we are left with a RW or WO variable ...
        //
        String type= SyntaxMapper.getTypeName(entry);
        String meth= SyntaxMapper.getCastMethod(entry);
    
        // Check the type of the value ...
        //
	// if (x instanceof <SnmpType>) {
	// ....}
	//
        checkVar_impl.append("if (x instanceof " + type +")" + Def.LBRACE + 
			  Def.TAB5);
		    
        // First set the value ...
        //
        if (enumeratiom == null) 
	    // {....
	    //   <mbean>.check<VarName>(((<SnmpType>)x).to<JavaType>());
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
	    // break;
	    //
            checkVar_impl.append(MBEAN_NAME + "." + Def.CHECK + var + 
				 "(((" + type + ")x)." + meth + "())" + 
				 Def.SEMICOLON + Def.TAB4 +
				 "} else {\n" + Def.TAB5 +
				 Def.THROW_NEW + Def.EXCP_SNMP + "(" +
				 Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" 
				 + Def.SEMICOLON + Def.TAB4 + Def.RBRACE +
				 Def.TAB4 + "break" + Def.SEMICOLON + "\n");
        else 
	    // {....
	    //    try {
	    //       <mbean>.check<VarName>( 
	    //           new <EnumType>(((<SnmpType>)x).to<JavaType>());
	    //    } catch (IllegalArgumentException e) {
	    //       throw new SnmpStatusException(
	    //           SnmpStatusException.badValue);
	    //    }
	    // } else {
	    //   throw new SnmpStatusException(SnmpStatusException.badValue);
	    // }
	    // break;
	    //
            checkVar_impl.append("try " + Def.LBRACE + Def.TAB6 +
			      MBEAN_NAME + "." + Def.CHECK + var + "( new " 
			      + enumeratiom + "(((" + type + ")x)." + 
                              meth +"()))" +
                              Def.SEMICOLON + Def.TAB5 + Def.N_RBRACE + 
			      " catch(" + Def.EXCP_ARGUMENT + " e) " +  
			      Def.LBRACE + Def.TAB6 +
			      Def.THROW_NEW + Def.EXCP_SNMP + "("  + 
			      Def.EXCP_SNMP + "." +Def.V_WRONGVALUE + ")" +
                              Def.SEMICOLON + Def.TAB5 + Def.RBRACE +
			      Def.TAB4 + "} else {\n" + Def.TAB5 +
                              Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			      Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
			      Def.SEMICOLON + Def.TAB4 + Def.RBRACE +
                              Def.TAB4 + "break" + Def.SEMICOLON + "\n");
    }

    protected void handleCheckSetAccess(String id, String var, String entry, 
					String enumeratiom, int acc) 
	throws IOException {
        checkset_impl.append(Def.TAB3 + "case " + id + ":\n" +  Def.TAB4);
        if (acc == ParserConstants.RO) {
            // Send a read-only exception ....
            //
	    // throw new SnmpStatusException(SnmpStatusException.readOnly);
	    //
            checkset_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" 
				 + Def.SEMICOLON +  "\n");
            return;
        }
    
        if ((acc == ParserConstants.NA)||
            (acc == ParserConstants.AFN)) {
            // we need to throw a specific exception ...
            //
	    // throw new SnmpStatusException(SnmpStatusException.noAccess);
	    //
            checkset_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" 
				 + Def.SEMICOLON +   "\n");
            return;
        }
    
        // Now we are left with a RW or WO variable ...
        //
        String type= SyntaxMapper.getTypeName(entry);
    
        // Check the type of the value ...
        //
	// if (x instanceof <SnmpType>) {
	// ....}
	//
        checkset_impl.append("if (!(x instanceof " + type +"))" + 
			     Def.LBRACE +  Def.TAB5);
		    
	// {....
	//   throw new SnmpStatusException(SnmpStatusException.badValue);
	// }
	checkset_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" +
			     Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" +
			     Def.SEMICOLON + Def.TAB4 + Def.RBRACE +
			     Def.TAB4 + "break" + Def.SEMICOLON +
			     "\n");
    }

    protected void handleCheckGetAccess(String id, String var, String entry, 
					String enumeratiom, int acc) 
	throws IOException {
        checkget_impl.append(Def.TAB3 + "case " + id + ":\n" +  Def.TAB4);
        if ((acc == ParserConstants.NA)   ||
	    (acc == ParserConstants.WO)   ||
            (acc == ParserConstants.AFN)) {
            // we need to throw a specific exception ...
            //
	    // throw new SnmpStatusException(SnmpStatusException.noAccess);
	    //
            checkget_impl.append(Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
				 Def.EXCP_SNMP + "." + Def.V_NOSUCHINSTANCE +
				 ")" + Def.SEMICOLON +   "\n");
            return;
        }
	checkget_impl.append("break" + Def.SEMICOLON);
    }

    /**
     * Returns the full qualified name of the class from which 
     * the generated class will derive: 
     * (com.sun.management.snmp.agent.SnmpMibNode or 
     *  com.sun.management.snmp.agent.SnmpMibGroup)
     * This function is called when generating the Java DMK "imports"
     * clauses in the file header (see writeJdmkImports())
     * Can be overridden in subclasses. Should be kept consistent
     * with the implementation of getParentClass().
     */
    protected String getParentClassPkg() {
	return Def.PKG_SNMP_MIB_NODE;
    }

    // Returns the name of the class from which the generated class
    // should inherit (default is SnmpMibNode).
    // Used to change the class inheritence (called by 
    // writeClassDeclaration()).
    // Can be overriden in subclasses to change the class inheritence.
    protected String getParentClass() {
	return Def.SNMP_NODE;
    }

    protected String getObjectServerClass() {
	if (isStandard()) 
	    return Def.SNMP_STANDARD_OSRV;
	if (isGeneric())
	    return Def.SNMP_GENERIC_OSRV;
	return "Object";
    }

    protected String[] getInterfacePkgs() {
	int count = 1;
	if (isStandard()) count += 2;
	if (isGeneric()) count += 2;
	String[] result = new String[count];
	result[--count] = Def.PKG_SNMP_SUBREQ;
	if (isGeneric()) {
	    result[--count] = Def.PKG_SNMP_GENERIC_MSRV;
	    result[--count] = Def.PKG_SNMP_GENERIC_OSRV;
	}
	if (isStandard()) {
	    result[--count] = Def.PKG_SNMP_STANDARD_MSRV;
	    result[--count] = Def.PKG_SNMP_STANDARD_OSRV;
	}
	return result;
    }

    protected String getInterfaces() {
	StringBuffer itf = new StringBuffer();
	if (isStandard()) itf.append(", " + Def.SNMP_STANDARD_MSRV);
	if (isGeneric())  itf.append(", " + Def.SNMP_GENERIC_MSRV);
	return itf.toString();
    }

    protected void writeClassDeclaration() throws IOException {
        // Add some comments
        //
        write("/**\n" +
              " * " + MessageHandler.getMessage("generate.meta.comment.desc", varName) + "\n" +
              " * " + MessageHandler.getMessage("generate.mbean.comment.oid", oid) + "\n" +
              " */\n");
        write(Def.PUBLIC + Def.CLASS + symboleName + 
	      Def.EXTEND + getParentClass() + "\n");
        write(Def.TAB + Def.IMPLEMENT + Def.SERIALIZABLE + getInterfaces() + 
	      Def.LBRACE + "\n");
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
        writeJmxImports();

        // import the Java DMK SNMP package                        
        //
        write("\n// jdmk imports" + "\n//\n");
	// Write Java DMK import clauses
	writeJdmkImports();
        write("\n");
    }


    // Write JMX import clauses. can be overriden in subclasses.
    protected void writeJmxImports()  throws IOException {
        write(Def.IMPORT + Def.PKG_MBEAN_SERVER + Def.SEMICOLON);
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
	if (isGeneric()) {
	    write(Def.IMPORT + Def.PKG_OBJECT_NAME + Def.SEMICOLON);
	}
    }

    // Write Java DMK import clauses. can be overriden in subclasses.
    protected void writeJdmkImports()  throws IOException {
        write(Def.IMPORT + Def.PKG_SNMP_MIB + Def.SEMICOLON);
        write(Def.IMPORT + getParentClassPkg() + Def.SEMICOLON);
	String[] intf = getInterfacePkgs();
	if (intf != null) {
	    for (int i=0; i < intf.length ; i++) {
		write(Def.IMPORT + intf[i] + Def.SEMICOLON);
	    }
	}
        write(Def.IMPORT + Def.PKG_SNMP_MIB_TABLE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_ROWSTATUS + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_DEFINITIONS + Def.SEMICOLON);

	// NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        if (SyntaxMapper.useUnsigned)
            write(Def.IMPORT + Def.PKG_UNSIGNEDLONG + Def.SEMICOLON);
        // end of NPCTE fix for bugId 4692891
    }

    protected void handleMinMax(String id) {
    }
  
    // PRIVATE METHODS
    //----------------
  
    protected void buildConstructorHeader() throws IOException {
        constructor.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
                           MessageHandler.getMessage("generate.meta.comment.constr", varName) + "\n" +
                           Def.TAB + " */\n");
        constructor.append(Def.TAB + Def.PUBLIC + symboleName + "(SnmpMib myMib, " + getObjectServerClass() + " objserv)" + Def.LBRACE );
	constructor.append(Def.TAB2 + OBJSRV + " = objserv" +
			   Def.SEMICOLON);
    
    }
  
    protected void buildIsVariableHeader() throws IOException {
        isVariable_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			       MessageHandler.getMessage("generate.meta.comment.isvariable", "arc") + "\n" +
			       Def.TAB + " */\n" + Def.TAB + Def.PUBLIC + 
			       "boolean" + " " +
			       Def.METH_IS_VARIABLE + "(long arc)" + 
			       Def.LBRACE + "\n" );
    
    }
  
    protected void buildIsReadableHeader() throws IOException {
        isReadable_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			       MessageHandler.getMessage("generate.meta.comment.isreadable","arc") + "\n" +
			       Def.TAB + " */\n" + Def.TAB + Def.PUBLIC + 
			       "boolean" + " " +
			       Def.METH_IS_READABLE + "(long arc)" + 
			       Def.LBRACE + "\n" );
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

    protected void buildGetVarHeader() throws IOException {
        getVar_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			   MessageHandler.getMessage("generate.meta.comment.getvar") + "\n" +
			   Def.TAB + " */\n");
	getVar_impl.append(Def.TAB + Def.PUBLIC + 
			   Def.SNMP_VALUE + " " + Def.METH_GET_VAR + 
			   "(long var, " + Def.SNMP_USERDATA + " data)\n" + 
			   Def.TAB2 + Def.THROWS + 
			   Def.EXCP_SNMP + Def.LBRACE);
	getVar_impl.append( Def.TAB2 + "switch((int)var)" + Def.LBRACE);
    }
  
    protected void buildSkipVarHeader() throws IOException {
	genAbstractMethodComment(Def.METH_SKIP_VARIABLE, getParentClass(),
				 skipVar_impl);
	skipVar_impl.append(Def.TAB + Def.PUBLIC + 
			    Def.BOOLEAN + " " + Def.METH_SKIP_VARIABLE + 
			   "(long var, " + Def.SNMP_USERDATA + " data, " +
			    "int pduVersion)" + Def.LBRACE);
	skipVarBody_impl.append( Def.TAB2 + "switch((int)var)" + Def.LBRACE);
    }

    protected void buildBuildValHeader() throws IOException {
        buildval_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			   MessageHandler.getMessage("generate.meta.comment.buildval") + "\n" +
			   Def.TAB + " */\n" + Def.TAB + Def.PUBLIC + 
			   Def.SNMP_VALUE + " " + Def.METH_BUILDVAL + 
			   "(long id, Object x)\n" + 
			   Def.TAB2 + Def.THROWS + 
			   Def.EXCP_SNMP + Def.LBRACE);
	buildval_impl.append( Def.TAB2 + "switch((int)id)" + Def.LBRACE);
    }
  
    protected void buildBuildAttHeader() throws IOException {
        buildatt_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			   MessageHandler.getMessage("generate.meta.comment.buildatt") + "\n" +
			   Def.TAB + " */\n" + Def.TAB + Def.PUBLIC + 
			   "Object " + Def.METH_BUILDATT + 
			   "(long id, " + Def.SNMP_VALUE + " x)\n" + 
			   Def.TAB2 + Def.THROWS + 
			   Def.EXCP_SNMP + Def.LBRACE);
	buildatt_impl.append( Def.TAB2 + "switch((int)id)" + Def.LBRACE);
    }
  
    protected void buildGetAttNameHeader() throws IOException {
        attname_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			    MessageHandler.getMessage("generate.meta.comment.getattname","id") + "\n" +
			    Def.TAB + " */\n" + Def.TAB + Def.PUBLIC + 
			    "String " + Def.METH_GET_ATTNAME + 
			    "(long id)\n" + 
			    Def.TAB2 + Def.THROWS + 
			    Def.EXCP_SNMP + Def.LBRACE);
	attname_impl.append( Def.TAB2 + "switch((int)id)" + Def.LBRACE);
    }
  
    protected void buildGet() throws IOException {
	genAbstractMethodComment(Def.METH_GET_REQ, getParentClass(),
				 getReq_impl);
        getReq_impl.append(Def.TAB + Def.PUBLIC + 
			   Def.VOID + Def.METH_GET_REQ + 
			   "(" + Def.SNMP_SUBREQ + " req, " + 
			   "int depth)\n" + 
			   Def.TAB2 + Def.THROWS + 
			   Def.EXCP_SNMP + Def.LBRACE);
	if (isStandard()) {
	    getReq_impl.append( Def.TAB2 + OBJSRV + ".get(this,req,depth)" 
				+ Def.SEMICOLON);
	} else if (isGeneric()) {
	    getReq_impl.append( Def.TAB2 + 
				OBJSRV + ".get(this,objname,req,depth)" 
				+ Def.SEMICOLON);
	}
	getReq_impl.append(Def.TAB + Def.RBRACE + "\n");
    }
  
    protected void buildSetVarHeader() throws IOException {
        setVar_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			   MessageHandler.getMessage("generate.meta.comment.setvar") + "\n" +
			   Def.TAB + " */\n" + Def.TAB + Def.PUBLIC + 
			   Def.SNMP_VALUE + " " + Def.METH_SET_VAR + 
			   "(" + Def.SNMP_VALUE + 
			   " x, long var, " + Def.SNMP_USERDATA + 
			   " data)\n" +
			   Def.TAB2 + Def.THROWS + 
			   Def.EXCP_SNMP + Def.LBRACE);
	setVar_impl.append( Def.TAB2 + "switch((int)var)" + Def.LBRACE);
    }
  
    protected void buildSet() throws IOException {
	genAbstractMethodComment(Def.METH_SET_REQ, getParentClass(),
				 setReq_impl);
        setReq_impl.append(Def.TAB + Def.PUBLIC + 
			   Def.VOID + Def.METH_SET_REQ + 
			   "(" + Def.SNMP_SUBREQ + " req, " + 
			   "int depth)\n" + 
			   Def.TAB2 + Def.THROWS + 
			   Def.EXCP_SNMP + Def.LBRACE);
	if (isStandard()) {
	    setReq_impl.append( Def.TAB2 + OBJSRV + ".set(this,req,depth)" 
				+ Def.SEMICOLON);
	} else if (isGeneric()) {
	    setReq_impl.append( Def.TAB2 + 
				OBJSRV + ".set(this,objname,req,depth)" 
				+ Def.SEMICOLON);
	}
	setReq_impl.append(Def.TAB + Def.RBRACE + "\n");
    }
  
    protected void buildCheckVarHeader() throws IOException {
        checkVar_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
                          MessageHandler.getMessage("generate.meta.comment.checkvar") + "\n" +
			     Def.TAB + " */\n");
        checkVar_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + 
			     Def.METH_CHECK_VAR + 
			     "(" + Def.SNMP_VALUE + " x, long var, " 
			     + Def.SNMP_USERDATA + " data)\n" +
			     Def.TAB2 + Def.THROWS + Def.EXCP_SNMP + 
			     Def.LBRACE);
    
	checkVar_impl.append(Def.TAB2 + "switch((int) var)" + Def.LBRACE);
    }
	
  
    protected void buildCheckSetAccessHeader() throws IOException {
        checkset_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			     MessageHandler.getMessage("generate.meta.comment.checkset") + "\n" +
			     Def.TAB + " */\n");

        checkset_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + 
			     Def.METH_CHECK_SET + "(" + Def.SNMP_VALUE + 
			     " x, long id, " + Def.SNMP_USERDATA + 
			     " data)\n" + Def.TAB2 + Def.THROWS + 
			     Def.EXCP_SNMP + Def.LBRACE);
    
	checkset_impl.append(Def.TAB2 + "switch((int) id)" + Def.LBRACE);
    }
	
  
    protected void buildCheckGetAccessHeader() throws IOException {
        checkget_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			     MessageHandler.getMessage("generate.meta.comment.checkget") + "\n" +
			     Def.TAB + " */\n");

        checkget_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + 
			     Def.METH_CHECK_GET + "(long id, " + 
			     Def.SNMP_USERDATA + 
			     " data)\n" + Def.TAB2 + Def.THROWS + 
			     Def.EXCP_SNMP + Def.LBRACE);
    
	checkget_impl.append(Def.TAB2 + "switch((int) id)" + Def.LBRACE);
    }
	
  
    protected void buildCheck() throws IOException {
	genAbstractMethodComment(Def.METH_CHECK_REQ, getParentClass(),
				 checkReq_impl);
        checkReq_impl.append(Def.TAB + Def.PUBLIC + 
			   Def.VOID + Def.METH_CHECK_REQ + 
			   "(" + Def.SNMP_SUBREQ + " req, " + 
			   "int depth)\n" + 
			   Def.TAB2 + Def.THROWS + 
			   Def.EXCP_SNMP + Def.LBRACE);
	if (isStandard()) {
	    checkReq_impl.append( Def.TAB2 + 
				  OBJSRV  + ".check(this,req,depth)" 
				  + Def.SEMICOLON);
	} else if (isGeneric()) {
	    checkReq_impl.append( Def.TAB2 + 
				  OBJSRV + ".check(this,objname,req,depth)" 
				  + Def.SEMICOLON);
	}
	checkReq_impl.append(Def.TAB + Def.RBRACE + "\n");
    }
  
    /**
     * Writing end of stuff for constructor
     */
    protected void closeConstructor() throws IOException {
        constructor.append(Def.TAB2 + Def.SNMP_VARLIST + " = new int[" + 
			   String.valueOf(counter) +
                           "]" + Def.SEMICOLON + init_impl.toString());
    
        // Sort the index list
        //
        constructor.append(Def.TAB2 + Def.SNMP_NODE + "." + Def.SNMP_SORT + 
			   "(" + Def.SNMP_VARLIST + ")" + Def.SEMICOLON +
                           Def.TAB + Def.RBRACE + "\n");

    }
  
    protected void defaultGetVarMeth() throws IOException {
	getVar_impl.append(Def.TAB4 + "break" + Def.SEMICOLON);
    }

    protected void defaultSkipVarMeth(StringBuffer result) 
	throws IOException {
	result.append(Def.TAB4 + "break" + Def.SEMICOLON);
    }

    protected void defaultBuildValMeth() throws IOException {
	buildval_impl.append(Def.TAB4 + "break" + Def.SEMICOLON);
    }

    protected void endBuildValMeth() throws IOException {
        buildval_impl.append(Def.TAB2 + Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			Def.EXCP_SNMP + "." + Def.V_NOSUCHOBJECT + ")" + 
			Def.SEMICOLON); 
    }

    protected void defaultBuildAttMeth() throws IOException {
	buildatt_impl.append(Def.TAB4 + "break" + Def.SEMICOLON);
    }

    protected void endBuildAttMeth() throws IOException {
        buildatt_impl.append(Def.TAB2 + Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			     Def.EXCP_SNMP + "." + Def.V_NOSUCHOBJECT + ")" + 
			     Def.SEMICOLON); 
    }

    protected boolean skipVarUseSuper() {
	return MibGenProperties.
	    getBooleanProperty(MibGenProperties.GEN_METABEAN_SKIPVAR_SUP,
			       false);
    }

    protected void endSkipVarMeth(StringBuffer result) 
	throws IOException {
	if (skipVarUseSuper()) {
	    result.append(Def.TAB2 + "return super." +
			  Def.METH_SKIP_VARIABLE+"(var,data,pduVersion)"+
			  Def.SEMICOLON); 
	} else {
	    result.append(Def.TAB2 + "return false" + Def.SEMICOLON); 
	}
    }

    protected void endGetVarMeth() throws IOException {
        getVar_impl.append(Def.TAB2 + Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			Def.EXCP_SNMP + "." + Def.V_NOSUCHOBJECT + ")" + 
			Def.SEMICOLON); 
    }

    protected void defaultGetAttNameMeth() throws IOException {
	attname_impl.append(Def.TAB4 + "break" + Def.SEMICOLON);
    }

    protected void endGetAttNameMeth() throws IOException {
        attname_impl.append(Def.TAB2 + Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			   Def.EXCP_SNMP + "." + Def.V_NOSUCHOBJECT + ")" + 
			   Def.SEMICOLON); 
    }

    /**
     * Writing ending stuff for the get...
     */
    protected void closeGetVarMeth() throws IOException {
    
        getVar_impl.append(Def.TAB3 + "default:\n");
	defaultGetVarMeth();
        getVar_impl.append(Def.TAB2 + Def.RBRACE);
	endGetVarMeth();
        getVar_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }

    protected void closeSkipVarMeth() throws IOException {
    
	// avoid empty switch() block.
	//
	if (totalSkipVarCount > 0) {
	    // Some case: have been generated.
	    //
	    if (skipVarCount > 0) {
		// There are empty pending case: cases. 
		// Generate the skip code:
		//    if (pduVersion== ...) return true; 
		//    break;
		// 
		genSkipVarCode(skipVarBody_impl);
		skipVarCount = 0;
	    }
	    
	    // Generates the default: clause
	    //
	    skipVarBody_impl.append(Def.TAB3 + "default:\n");
	    defaultSkipVarMeth(skipVarBody_impl);
	    skipVarBody_impl.append(Def.TAB2 + Def.RBRACE);

	    // Now appends the whole switch() block to the method code.
	    // 
	    skipVar_impl.append(skipVarBody_impl.toString());
	}

	// Generates the default return statement.
	//
	endSkipVarMeth(skipVar_impl);
        skipVar_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }

    protected void closeBuildValMeth() throws IOException {
    
        buildval_impl.append(Def.TAB3 + "default:\n");
	defaultBuildValMeth();
        buildval_impl.append(Def.TAB2 + Def.RBRACE);
	endBuildValMeth();
        buildval_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }

    protected void closeBuildAttMeth() throws IOException {
    
        buildatt_impl.append(Def.TAB3 + "default:\n");
	defaultBuildAttMeth();
        buildatt_impl.append(Def.TAB2 + Def.RBRACE);
	endBuildAttMeth();
        buildatt_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }

    protected void closeGetAttNameMeth() throws IOException {
    
        attname_impl.append(Def.TAB3 + "default:\n");
	defaultGetAttNameMeth();
        attname_impl.append(Def.TAB2 + Def.RBRACE);
	endGetAttNameMeth();
        attname_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }
  
    /**
     * Writing ending stuff for the isVariable()...
     */
    protected void closeIsVariableMeth() throws IOException {
	if (varCount > 0) {
	    isVariable_impl.append(Def.TAB2 + "switch((int)arc)" + 
				   Def.LBRACE);
	    isVariable_impl.append((Object) isVariableBody_impl);
	    /* (Object) cast is because J2SE 1.4 added a method
	       StringBuffer.add(StringBuffer), which otherwise
	       we end up calling here; therefore if you compile
	       with 1.4 but run on an earlier version it fails.  */
	    isVariable_impl.append(Def.TAB4 + "return true" + Def.SEMICOLON);
	    isVariable_impl.append(Def.TAB3 + "default:\n");
	    isVariable_impl.append(Def.TAB4 + "break" + Def.SEMICOLON);
	    isVariable_impl.append(Def.TAB2 + Def.RBRACE);
	}
	isVariable_impl.append(Def.TAB2 + "return false"+Def.SEMICOLON);
        isVariable_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }
  
    /**
     * Writing ending stuff for the isReadableVariable()...
     */
    protected void closeIsReadableMeth() throws IOException {
	if (readCount > 0) {
	    isReadable_impl.append(Def.TAB2 + "switch((int)arc)" + 
				   Def.LBRACE);
	    isReadable_impl.append((Object) isReadableBody_impl);
	    /* (Object) cast is because J2SE 1.4 added a method
	       StringBuffer.add(StringBuffer), which otherwise
	       we end up calling here; therefore if you compile
	       with 1.4 but run on an earlier version it fails.  */
	    isReadable_impl.append(Def.TAB4 + "return true" + Def.SEMICOLON);
	    isReadable_impl.append(Def.TAB3 + "default:\n");
	    isReadable_impl.append(Def.TAB4 + "break" + Def.SEMICOLON);
	    isReadable_impl.append(Def.TAB2 + Def.RBRACE);
	}
	isReadable_impl.append(Def.TAB2 + "return false"+Def.SEMICOLON);
        isReadable_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }
  
    /**
     * Writing ending stuff for the get...
     */
    protected void defaultSetVarMeth() throws IOException {
	setVar_impl.append(Def.TAB4 + "break" + Def.SEMICOLON);
    }

    protected void endSetVarMeth() throws IOException {
	setVar_impl.append(Def.TAB2 + Def.THROW_NEW + Def.EXCP_SNMP + "(" + 
			   Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" + 
			   Def.SEMICOLON);
    }

    protected void closeSetVarMeth() throws IOException {
        setVar_impl.append(Def.TAB3 + "default:\n");
	defaultSetVarMeth();
	setVar_impl.append(Def.TAB2 +  Def.RBRACE);
	endSetVarMeth();
	setVar_impl.append(Def.TAB +  Def.RBRACE + "\n");
    }
  
    protected void defaultCheckVarMeth() throws IOException {
	checkVar_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + "(" 
			     + Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" 
			     +  Def.SEMICOLON);
    }

    protected void closeCheckVarMeth() throws IOException {
        checkVar_impl.append(Def.TAB3 + "default:\n");
	defaultCheckVarMeth();
	checkVar_impl.append(Def.TAB2 + Def.RBRACE +
			     Def.TAB +  Def.RBRACE + "\n");    
    }
  
    protected void defaultCheckSetAccessMeth() throws IOException {
	checkset_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + "(" 
			     + Def.EXCP_SNMP + "." + Def.V_NOTWRITABLE + ")" 
			     +  Def.SEMICOLON);
    }

    protected void defaultCheckGetAccessMeth() throws IOException {
	checkget_impl.append(Def.TAB4 + Def.THROW_NEW + Def.EXCP_SNMP + "(" 
			     + Def.EXCP_SNMP + "." + Def.V_NOSUCHOBJECT + ")" 
			     +  Def.SEMICOLON);
    }

    protected void closeCheckSetAccessMeth() throws IOException {
        checkset_impl.append(Def.TAB3 + "default:\n");
	defaultCheckSetAccessMeth();
	checkset_impl.append(Def.TAB2 + Def.RBRACE +
			     Def.TAB +  Def.RBRACE + "\n");    
    }
  
    protected void closeCheckGetAccessMeth() throws IOException {
        checkget_impl.append(Def.TAB3 + "default:\n");
	defaultCheckGetAccessMeth();
	checkget_impl.append(Def.TAB2 + Def.RBRACE +
			     Def.TAB +  Def.RBRACE + "\n");    
    }
  
    protected void buildVarList() throws IOException {
	if (isStandard()) {
	    var_def.append(Def.TAB + Def.PROTECTED + prefix + varName +
			   Def.MBEANSUFFIX + " node" + Def.SEMICOLON);
	}
	if (isGeneric()) {
	    var_def.append(Def.TAB + Def.PRIVATE + "ObjectName" +
			   " objname = null" + Def.SEMICOLON);
	}
        var_def.append(Def.TAB + Def.PROTECTED + getObjectServerClass() + 
		       " " + OBJSRV + " = null" + Def.SEMICOLON);
    }
  
    protected void buildSetInstance() throws IOException {
        moi_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
                        MessageHandler.getMessage("generate.meta.comment.setMoi") + "\n" +
                        Def.TAB + " */\n" + Def.TAB + 
                        Def.PROTECTED + Def.VOID + Def.SET_MOI + "(" + prefix + varName + Def.MBEANSUFFIX + " var)" + Def.LBRACE +
                        Def.TAB2 + MBEAN_NAME + " = var" + Def.SEMICOLON + Def.TAB + Def.RBRACE +"\n" );
    }
    
    protected void buildSetObjectName() throws IOException {
        objname_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			    MessageHandler.getMessage("generate.meta.comment.setobjname") + "\n" +
			    Def.TAB + " */\n" + Def.TAB + 
			    Def.PROTECTED + Def.VOID + Def.SET_OBJNAME + 
			    "( ObjectName name )" + Def.LBRACE +
			    Def.TAB2 + "objname = name" + Def.SEMICOLON + 
			    Def.TAB + Def.RBRACE +"\n" );
    }
    
    protected void buildSetObjectServer() throws IOException {
        objsrv_impl.append(Def.TAB + "/**\n"+ Def.TAB + " * " +
			   MessageHandler.getMessage("generate.meta.comment.table.setobjsrv") + "\n" +
			   Def.TAB + " */\n" + Def.TAB + 
			   Def.PROTECTED + Def.VOID + Def.SET_OBJSRV + 
			   "(" + getObjectServerClass() + " objserver)" 
			   + Def.LBRACE + Def.TAB2 +
			   OBJSRV + " = objserver" + Def.SEMICOLON + 
			   Def.TAB + Def.RBRACE +"\n" );
    }
    

    // ----------------------------------------------------------------------
    // VARIABLES
    // ----------------------------------------------------------------------

    public final static int STANDARD_META = 1;
    public final static int GENERIC_META = 2;

    
    int gentype = 0;
  
    /*
    ** isVariable()
    */
    protected StringBuffer isVariable_impl;
    protected StringBuffer isVariableBody_impl;
  
    /*
    ** isReadable()
    */
    protected int readCount = 0;
    protected StringBuffer isReadable_impl;
    protected StringBuffer isReadableBody_impl;
  
    /*
    ** Get next
    */
    protected StringBuffer getNext_impl;
    protected StringBuffer skipVar_impl;
    protected StringBuffer skipVarBody_impl;
    protected int skipVarCount;
    protected int totalSkipVarCount;
  
    /*
    ** Getter
    */
    protected StringBuffer get_impl;
    protected StringBuffer getVar_impl;
    protected StringBuffer getReq_impl;

    /*
    ** Setter
    */
    protected StringBuffer set_impl;
    protected StringBuffer setVar_impl;
    protected StringBuffer setReq_impl;
  
    /*
    ** Checker
    */
    protected StringBuffer check_impl;
    protected StringBuffer checkVar_impl;
    protected StringBuffer checkReq_impl;
  
    /*
    ** Init function
    */
    protected StringBuffer init_impl;
  
    /*
    ** var list
    */
    protected StringBuffer var_def;
  
    /*
    ** ste instance stuff
    */
    protected StringBuffer moi_impl;

    /*
     * set ObjectServer
     */
    protected StringBuffer objsrv_impl;

     protected StringBuffer buildval_impl;
    protected StringBuffer buildatt_impl;
    protected StringBuffer attname_impl;
    protected StringBuffer checkset_impl;
    protected StringBuffer checkget_impl;
    protected StringBuffer objname_impl;
  
    /*
    ** Constructor
    */
    protected StringBuffer constructor;
  
    /*
    ** Getters and setters
    */
    protected StringBuffer accessors;

    protected static String MBEAN_NAME= "node";
    protected static String OBJSRV    = "objectserver";
  
    protected int counter= 0;  // # of scalar objects + # of tables
    protected int varCount= 0; // # of scalar objects
}

