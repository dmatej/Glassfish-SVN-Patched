/*
 * @(#)file      MetaEntryGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.18
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
public class MetaEntryGenerator extends MetaBeanGenerator implements Serializable {
  
  
    public MetaEntryGenerator(ResourceManager mgr, MibNode aGroup, 
			      Context ctxt)
	throws IOException {
	super(mgr, aGroup, ctxt);
    }
  
    // Used to set the parent class to SnmpMibGroup (rather than to
    // SnmpMibNode)
    protected String getParentClass() {
	return Def.SNMP_ENTRY;
    }

    // Used to import com.sun.management.snmp.agent.SnmpMibGroup (rather than
    // com.sun.management.snmp.agent.SnmpMibNode)
    protected String getParentClassPkg() {
	return Def.PKG_SNMP_MIB_ENTRY;
    }

    // Write Java DMK import clauses. can be overriden in subclasses.
    protected void writeJdmkImports()  throws IOException {
        write(Def.IMPORT + Def.PKG_SNMP_MIB_NODE + Def.SEMICOLON);
	super.writeJdmkImports();
    }


    protected void handleMinMax(String id) {
    }
  
    protected void handleVariableId(String id, String var, String entry, 
				    String enumtype, int acc) 
	throws IOException {
	super.handleVariableId(id,var,entry,enumtype,acc);
	if (isRowStatus(context,var)) {
	    rowStatusId     = id;
	    rowStatusName   = var;
	    rowStatusAcc    = acc;
	    rowStatusEnum   = enumtype;
	    rowStatusSyntax = entry;
	}
    }

    public boolean implementsRowStatus() {
	return (rowStatusId != null);
    }

    public String getRowStatusId() {
	return rowStatusId;
    }

    public String getRowStatusName() {
	return rowStatusName;
    }

    public void generateRowStatusCode(Context ctxt, StringBuffer result) {
	if (! implementsRowStatus()) return;

	// getRowStatusId()
	//
	genGetRowStatusId(result);

	// getRowStatusName()
	//
	genGetRowStatusName(result);

	// mapRowStatus()
	//
	genMapRowStatus(result);

	// getRowStatus()
	//
        genGetRowStatus(result);

	// setRowStatus()
	//
	genSetRowStatus(result);

	// isRowReady() 
	//
	genIsRowReady(result);
    }

    protected  void genIsRowReady(StringBuffer result) {
	result.append("\n" + Def.TAB + Def.PUBLIC + " boolean " + 
		      Def.METH_T_ISROWREADY + "(" + Def.SNMP_USERDATA + 
		      " userData)\n" + Def.TAB3 + Def.THROWS + 
		      Def.EXCP_SNMP + Def.LBRACE );
	result.append(Def.TAB2 + "\n");
    result.append(Def.TAB2 + "// " + MessageHandler.getMessage("generate.meta.comment.impl.proposed1") + "\n");
	result.append(Def.TAB2 + "// " + Def.RETURN + "\t(" + 
		      Def.METH_T_GETRS + "(userData)" + 
		      " != EnumRowStatus.notReady)" + Def.SEMICOLON);
	result.append(Def.TAB2 + "\n");
	result.append(Def.TAB2 + Def.RETURN + "true" + Def.SEMICOLON);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    protected  void genGetRowStatusId(StringBuffer result) {
	result.append("\n" + Def.TAB + Def.PUBLIC + " long " + 
		      Def.METH_T_GETRSID + "() " + Def.LBRACE);
	result.append(Def.TAB2 + Def.RETURN + rowStatusId + Def.SEMICOLON);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    protected  void genGetRowStatusName(StringBuffer result) {
	result.append("\n" + Def.TAB + Def.PUBLIC + " String " + 
		      Def.METH_T_GETRSNAME + "() " + Def.LBRACE);
	result.append(Def.TAB2 + Def.RETURN + "new String (\"" + 
		      rowStatusName + "\")" + Def.SEMICOLON);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    protected  void genMapRowStatus(StringBuffer result) {
	result.append("\n" + Def.TAB + Def.PROTECTED + " int " + 
		      Def.METH_T_MAPRSVALUE + "(" + Def.SNMP_VARBIND + 
		      " var, " + Def.SNMP_USERDATA + " userData)\n" + 
		      Def.TAB3 + Def.THROWS + Def.EXCP_SNMP + Def.LBRACE );
	result.append(Def.TAB2 + Def.FINAL + Def.SNMP_VALUE + 
		      " value = var.getSnmpValue()" + 
		      Def.SEMICOLON);
	result.append(Def.TAB2 + "if (value == null)\n" + Def.TAB3 +
		      Def.THROW_NEW + Def.EXCP_SNMP + "(\n" + Def.TAB4 + 
		      Def.EXCP_SNMP + "." + Def.V_INCONSISTENTVALUE +
		      ")" + Def.SEMICOLON);
	result.append(Def.TAB2 + "if (!(value instanceof " + Def.SNMP_INT +
		      "))\n" + Def.TAB3 + 
		      Def.THROW_NEW + Def.EXCP_SNMP + "(\n" + Def.TAB4 + 
		      Def.EXCP_SNMP + "." + Def.V_WRONGTYPE + ")" + 
		      Def.SEMICOLON);
	result.append(Def.TAB2 + Def.FINAL + "int" + 
		      " code = ((" + Def.SNMP_INT + ")value).intValue()" + 
		      Def.SEMICOLON);
	result.append(Def.TAB2 + "if (!(" + Def.SNMP_ROWSTATUS + 
		      ".isValidValue(code)))\n" + Def.TAB3 +
		      Def.THROW_NEW + Def.EXCP_SNMP + "(\n" + Def.TAB4 + 
		      Def.EXCP_SNMP +"." + Def.V_INCONSISTENTVALUE + 
		      ")" + Def.SEMICOLON);
	result.append(Def.TAB2 + Def.RETURN + " code" + Def.SEMICOLON);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    protected  void genSetRowStatus(StringBuffer result) {

	result.append("\n" + Def.TAB + Def.PUBLIC + " SnmpValue " + 
		      Def.METH_T_SETRS + "(int status, " + 
		      Def.SNMP_USERDATA + " userData)\n" + 
		      Def.TAB3 + Def.THROWS + Def.EXCP_SNMP + 
		      Def.LBRACE);
	
	result.append(Def.TAB2 + Def.FINAL + " " + Def.SNMP_INT +
		      " x = new " + Def.SNMP_INT + "(status)" + 
		      Def.SEMICOLON);

	if (isStandard()) {
	    result.append(Def.TAB2 + Def.RETURN + " " + Def.METH_SET_VAR + 
			  "(x, " + rowStatusId + ", userData)" + 
			  Def.SEMICOLON);
	} 
	if (isGeneric()) {
	    result.append( Def.TAB2 + Def.RETURN + " " +
				OBJSRV + ".set(this, objname, x, " + 
				rowStatusId + ", userData)" + 
				Def.SEMICOLON);
	}
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    protected  void genGetRowStatus(StringBuffer result) {

	result.append("\n" + Def.TAB + Def.PUBLIC + " int " + 
		      Def.METH_T_GETRS + "(" + Def.SNMP_USERDATA + 
		      " userData)\n" + 
		      Def.TAB3 + Def.THROWS + Def.EXCP_SNMP + 
		      Def.LBRACE);
	
	if (isStandard()) {
	    result.append(Def.TAB2 + Def.FINAL + " " + Def.SNMP_VALUE +
			  " x = " + Def.METH_GET_VAR + "(" + rowStatusId +
			  ", userData)" + Def.SEMICOLON);
	}
	if (isGeneric()) {
	    result.append(Def.TAB2 + Def.FINAL + " " + Def.SNMP_VALUE +
			  " x = " + OBJSRV + ".get(this, objname, " + 
			   rowStatusId + ", userData)" + 
			   Def.SEMICOLON);
	}
	result.append(Def.TAB2 + Def.FINAL + " " + Def.SNMP_VARBIND + 
		      " vb = new " + Def.SNMP_VARBIND + "()" + Def.SEMICOLON);
	result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.meta.comment.impl.proposed2") + "\n");
	result.append(Def.TAB2 + "//\n");
	result.append(Def.TAB2 + "// vb.oid = new " + Def.SNMP_OID + 
		      "(\"" + oid + "." + rowStatusId + "\")" + 
		      Def.SEMICOLON + "\n"); 
	result.append(Def.TAB2 + "vb.setSnmpValue(x)" + Def.SEMICOLON); 
	result.append(Def.TAB2 + Def.RETURN + Def.METH_T_MAPRSVALUE +
		      "(vb, userData)" + Def.SEMICOLON); 
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    protected  void closeMethods() throws IOException {
	generateRowStatusCode(context, rs_impl);
	super.closeMethods();
    }

    protected  void writeMethods() throws IOException {
	super.writeMethods();
	write(rs_impl.toString());
    }

    protected void initBuffers() {
	super.initBuffers();
	rs_impl = new  StringBuffer();
    }

    // PRIVATE METHODS
    //----------------
  
 
    // VARIABLES
    //----------

    protected String rowStatusId     = null;
    protected String rowStatusName   = null;
    protected int    rowStatusAcc    = 0;
    protected String rowStatusEnum   = null;
    protected String rowStatusSyntax = null;
    protected StringBuffer rs_impl;

}

