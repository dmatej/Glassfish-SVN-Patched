/*
 * @(#)file      EntryGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.18
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
 * The class generates code required for representing an entry
 * With jdmk 3.x, we used the MBeanGenerator for both group and entry code generation.
 * With jdmk 4.0, this is no more possible as groups are always standard MBeans (ie. extends MBean interface)
 * and entries are standard MBeans only with the new mibgen option -mb.
 *
 */
public class EntryGenerator extends MbeanGenerator implements Serializable {
  
    public EntryGenerator(ResourceManager mgr, MibNode aGroup, Context ctxt)
	throws IOException {
        super(mgr, aGroup, ctxt);
    }
  
    protected void buildConstructorHeader() throws IOException {
        constructor1.append("\n"+ Def.TAB + "/**\n"+ Def.TAB + " * " +
                            MessageHandler.getMessage("generate.mbean.comment.constr", symboleName) + "\n" +
                            Def.TAB + " */\n");
        constructor1.append(Def.TAB + Def.PUBLIC + symboleName + "(SnmpMib myMib)" + Def.LBRACE );
    }
   
    protected void closeConstructor() throws IOException {
        constructor1.append( Def.TAB + Def.RBRACE + "\n");    
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

    /**
     * Generates the code for the checker.
     **/
    protected void addChecker(Context ctxt, MibNode node, 
			      String description, String syntax, 
			      String var, StringBuffer buff) {
	if (!isRowStatus(ctxt,var))
	    super.addChecker(ctxt,node,description,syntax,var,buff);
	else
	    if(MibGenProperties.isDeprecatedEnabled())
		// Generate deprecated checker for RowStatus.
		// Only for JDMK 5.0 compatible MIBs.
		addRowStatusChecker(ctxt,node,description,syntax,var,buff);
    }
    
    /**
     * Generates the code for the setter.
     **/
    protected void addSetter(Context ctxt, MibNode node, 
			     String description, String syntax, 
			     String var, StringBuffer buff) {
	if (!isRowStatus(ctxt,var))
	    super.addSetter(ctxt,node,description,syntax,var,buff);
	else
	    if(MibGenProperties.isDeprecatedEnabled())
		super.addSetter(ctxt,node,description,syntax,var,buff);
	    else
		//We add a comment to say that no checker is generated 
		// for row status
		addRowStatusSetter(ctxt,node,description,syntax,var,buff);
    }

    private void addRowStatusSetter(Context ctxt, MibNode node, 
				    String description, String syntax, 
				    String var, StringBuffer buff) {
	buff.append(Def.TAB + "/**\n" + Def.TAB + " * " +
	  MessageHandler.getMessage("generate.mbean.comment.setter", var) + 
		    "\n" + Def.TAB + " * " + 
		    MessageHandler.getMessage("generate.mbean.comment."+
					      "setter.rs.nochecker") + 
		    "\n" + Def.TAB + " * " + 
		    MessageHandler.getMessage("generate.mbean.comment."+
					      "checker.rs.override") + 
		    "\n" + Def.TAB + " */\n" );
	buff.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.SET + var + 
		    "(" + syntax + "x) " + accessThrows);
	buff.append(Def.TAB2 + var + " = x" + Def.SEMICOLON);
	buff.append(Def.TAB + Def.RBRACE + "\n");
    }
    
    private void addRowStatusChecker(Context ctxt, MibNode node, 
				     String description, String syntax, 
				     String var, StringBuffer buff) {
	buff.append(Def.TAB + "/**\n" + Def.TAB + " * " +
		    MessageHandler.getMessage("generate.mbean.comment.checker",
					      var)
		    +"\n"+   Def.TAB + " * " +
		    MessageHandler.getMessage(
		    "generate.mbean.comment.checker.rs.deprecated") + 
		    "\n" + Def.TAB + " * " +
	  MessageHandler.getMessage(
		    "generate.mbean.comment.checker.rs.override") + 
		    "\n" + Def.TAB + " */\n" );
	buff.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.CHECK + var + "(" 
		    + syntax + "x) " + BeanGenerator.accessThrows);
        buff.append(Def.TAB2 + "//\n" + Def.TAB2 + "// " + 
	  MessageHandler.getMessage(
                    "generate.mbean.comment.checker.rs.policy")
		    + "\n" + Def.TAB2 + "//\n");
	buff.append(Def.TAB + Def.RBRACE + "\n");
    }
    
}

