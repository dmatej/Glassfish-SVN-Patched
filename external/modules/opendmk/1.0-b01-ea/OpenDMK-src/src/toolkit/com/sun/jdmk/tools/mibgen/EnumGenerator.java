/*
 * @(#)file      EnumGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.17
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
public class EnumGenerator extends Generator implements Serializable {
  
    public EnumGenerator(ResourceManager mgr, String var,Enumerated enumeration, 
			 Context ctxt) 
	throws IOException {
        super(mgr, ctxt);
        // typeName = enumeration.getSymbol();
        typeName = getRealEnumName(ctxt,var,enumeration);
        this.enumeration= enumeration;
        this.mib= ctxt.mib;
	EnumGenerator gen = (EnumGenerator) enumList.get(typeName);
	if (gen == null) {
	    generateEnum();
	    enumList.put(typeName, this);
	}
    }

    static public String getRealEnumName(Context ctxt, String var,
					 Enumerated enumeration) {
	String typeName= enumeration.getSymbol();

	// This is a in-line definition. Generate the code. 
	// Use the variable name to name the enumeration. Use prefix Def.ENUMPREFIX.
	//
	if (typeName.length() == 0) {
	    return var;
	}

	// See if we already have something for this...
	//
	EnumGenerator gen = (EnumGenerator) enumList.get(typeName);
	if (gen != null) {
	    return gen.getRealName();
	} 
	return typeName;
    }

    static public String getEnumClassName(Context ctxt, String var, 
					  Enumerated enumeration) {
	String typeName = getRealEnumName(ctxt,var,enumeration);
	return ctxt.prefix + Def.ENUMPREFIX + typeName + " ";
    }

    public void setContext(Context ctxt) {
	super.setContext(ctxt);
	mib = ctxt.mib;
    }

    protected String getRealName() {
        return  typeName;
    }
  
    
    public String getTypeName() {
        return prefix + Def.ENUMPREFIX + typeName+ " ";
    }
  
    protected void writeVersion() throws IOException {
	String msgid = "generate.version";
	
	write("\n//\n// " + 
	      MessageHandler.getMessage(msgid, mib.getModuleName()) +
              "\n//\n\n");
    }

    private void generateEnum() throws IOException {
    
        String className= prefix + Def.ENUMPREFIX + typeName;
    
        Trace.info(MessageHandler.getMessage("generate.info.enum", typeName));
	
        out= openFile(className + Def.JAVA);   
        writePkg();
    
        // Add package declaration
        //
        write("// java imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SERIALIZABLE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_HASHTABLE + Def.SEMICOLON);
        write("\n// RI imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_ENUMERATED + Def.SEMICOLON + "\n");
    
        write("/**\n" +
              " * " + MessageHandler.getMessage("generate.enum.comment.desc", typeName) + "\n" +
              " */\n");
        write(Def.PUBLIC + Def.CLASS + className + Def.EXTEND + Def.ENUM_CLASS);
        write(Def.IMPLEMENT + Def.SERIALIZABLE + Def.LBRACE + "\n" +	  
              Def.TAB + "protected static Hashtable intTable = new Hashtable();\n" +
              Def.TAB + "protected static Hashtable stringTable = new Hashtable();\n");
        write(Def.TAB + Def.STATIC + Def.LBRACE);
        Hashtable list= enumeration.getEnum();
    
        StringBuffer buff1= new StringBuffer();
        StringBuffer buff2= new StringBuffer();
        for(Enumeration e= list.keys(); e.hasMoreElements(); ) {
            String key= (String) e.nextElement();
            String val= (String) list.get(key);
            buff1.append(Def.TAB2 + Def.ENUM_INTTAB+".put(new Integer(" + val + "), \"" +
                         key  + "\");\n");
            buff2.append(Def.TAB2 + Def.ENUM_STRINGTAB + ".put(\"" + key +
                         "\", new Integer(" + val + "));\n" ); 
        }
        write(buff1.toString());
        write(buff2. toString());
    
        write(Def.TAB + Def.RBRACE + "\n");
    
        // Add the constructor
        //
        write( Def.TAB + Def.PUBLIC + className + "(int valueIndex) " + Def.THROWS + Def.EXCP_ARGUMENT + 
               Def.LBRACE + Def.TAB2 + "super(valueIndex)" + Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n" +
               Def.TAB + Def.PUBLIC + className + "(Integer valueIndex) " + Def.THROWS + Def.EXCP_ARGUMENT + 
               Def.LBRACE + Def.TAB2 + "super(valueIndex)" + Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n"  +
               Def.TAB + Def.PUBLIC + className + "() " + Def.THROWS + Def.EXCP_ARGUMENT + 
               Def.LBRACE + Def.TAB2 + "super()" + Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n" +
               Def.TAB + Def.PUBLIC + className + "(String x) " + Def.THROWS + Def.EXCP_ARGUMENT + 
               Def.LBRACE + Def.TAB2 + "super(x)" + Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n" 
               );
    
        write(Def.TAB + "protected Hashtable getIntTable() {\n" +
              Def.TAB2 + "return intTable ;\n" +
              Def.TAB + Def.RBRACE + "\n" +
              Def.TAB + "protected Hashtable getStringTable() {\n" + 
              Def.TAB2 + "return stringTable ;\n" +
              Def.TAB + Def.RBRACE + "\n" +
              Def.RBRACE);
    
        closeIO();
    }
  
  
    // VARIABLES
    //----------
  
    /**
     * Variable list
     */
    protected StringBuffer var_list= new StringBuffer();
  
    /*
    ** Getters and setters
    */
    protected StringBuffer accessors= new StringBuffer();
  
    /*
    ** Constructor
    */
    protected StringBuffer constructor= new StringBuffer();
  
    /**
     * Enum description
     */
    protected Enumerated enumeration;
  
    /**
     * Name of the enumeration. 
     */
    private String typeName;
  
    /**
     * List of Enum in order to regenerate twice the same enumeration ...
     * Only applicable for type definition, not for the enumeration defined as 
     * ASN.1 in-line definition (SYNTAX close in the OBJECT-TYPE macro).
     */
    private static Hashtable enumList= new Hashtable();
}

