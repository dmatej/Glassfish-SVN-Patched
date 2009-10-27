/*
 * @(#)file      BeanGenerator.java
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
 * The class generates code required for representing a group as a m-bean
 *
 */
public abstract class BeanGenerator extends Generator implements Serializable {
  
    public BeanGenerator(ResourceManager mgr, MibNode node, Context ctxt) 
	throws IOException {
        super(mgr, ctxt);
        this.node= node;
        mib = ctxt.mib;
    }
  
 
    // PRIVATE METHODS
    //----------------
  
    protected String formatDescription(String description) {
    
	if (description == null) description="";
        int old= 0;
        int length= description.length();
        int pos= 0;
        //StringBuffer result= new StringBuffer(Def.TAB + "/*\n" + Def.TAB + " * ");
        StringBuffer result= new StringBuffer(  " *\n" + Def.TAB + " * ");
        while (old < length) {
            pos= description.indexOf('\n', old);
            if (pos == -1) {
                result.append(description.substring(old ).trim());
                break;
            }
            result.append(description.substring(old, pos).trim());
            // add tabulation
            //
            result.append("\n" + Def.TAB + " * ");
            old= pos + 1;
        }
        //result.append("\n" + Def.TAB + " */\n");
        result.append("\n" + Def.TAB + " *\n" + Def.TAB);
        return result.toString();
    }
  
    protected abstract void writeClassDeclaration() throws IOException;
  
    // VARIABLES
    //----------
  
    /**
     * Symbol name associated to the group
     */
    protected String symboleName= "";
  
    /**
     * Variable name
     */
    protected String varName= "";
  
    /**
     * OID associated to the generated bean
     */
    protected String oid;
  
    /**
     * Node representing the group
     */
    protected MibNode node;
  
    protected static String accessThrows= Def.THROWS + Def.EXCP_SNMP + Def.LBRACE;
  
}

