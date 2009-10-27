/*
 * @(#)file      MibGenerator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.29
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
 * The class generates code required for representing a whole MIB.
 *
 */
public class MibGenerator extends Generator implements Serializable {
  
    public MibGenerator(ResourceManager mgr, ASTMib aMib, Context ctxt) 
	throws IOException {
        super(mgr, ctxt);

        mib= aMib;

	// gentype indicates whether we should generate code for
	// standard or generic meta.
	//
	gentype = ctxt.gentype;
	
	context.mib    = aMib;
	context.mibgen = this;

        // Get the name of the MIB
        //
        module= aMib.getModuleName();
     
        // Translate the mib name so it can be used in a Java class name
        // or in a Java DMK objectName.
        //
        translatedMibName= translateMibName();

        // Translate the module name in order to get an acceptable Java 
	// class name
        //
        fileName= getMibClassName(prefix,aMib);
     
        // Open the file which will represent the M-bean.
        //
        out= openFile(fileName + Def.JAVA);
     
        // Write generic header ...
        //
        writeHeader();
     
        // write our own header ...
        //
        writeClassDeclaration();
     
	// write the MIB constructors
	//
        writeConstructor();
     
        // Build the headers for the different init
        //
        buildInitHeader();
        buildPopulateHeader();
        buildInitPreRegHeader();
	buildRegisterTableMeta(regmeta_impl);     
	buildGetRegisteredMeta(getmeta_impl);

	if (isStandard()) {
	    buildGetStandardObjectServer(objsrv_impl);
	}
    }
  
    public void setContext(Context ctxt) {
	super.setContext(ctxt);
	mib = ctxt.mib;
	gentype = ctxt.gentype;
	ctxt.mibgen = this;
    }

    /**
     * Tell whether MBean factory methods should be generated as abstract
     * methods.
     **/
    protected boolean isAbstractFactory() {
	return MibGenProperties.getBooleanProperty(
	       MibGenProperties.OPTION_MIB_FACTORY_ABSTRACT, false);
    }

    // Build the name of the MetaData factory method from the
    // name of the MetaData class.
    //
    protected String getMetaFactoryName(String metaName) {
	return "create" + metaName + "Node";
    }
    
    // Build the name of the MBean factory method from the
    // name of the MBean class.
    //
    protected String getBeanFactoryName(String beanName) {
	return "create" + beanName + Def.MBEANSUFFIX;
    }

    // Build the return type of the MBean factory method from the
    // name of the class
    //
    protected String getBeanFactoryResult(Context ctxt, String varName, 
					  String beanName) {
	if (isGeneric(ctxt, varName)) {
	    // return beanName + Def.MBEANSUFFIX;
	    return "Object";
	} else {
	    // return beanName + Def.MBEANSUFFIX;
	    return "Object";
	}
    }
    
    protected String getBeanInterfaceName(Context ctxt, String varName, 
					  String beanName) {
	return beanName + Def.MBEANSUFFIX;
    }

    // Generate comments for the factory method
    // 
    // @param varName Name of the group to which this method apply
    // @param className Name of the corresponding (Bean or Meta) class
    // @param type Either "bean" or "meta"
    // @param result The stringbuffer to which the comments will be
    //        added.
    //
    public void generateFactoryComments(String varName, String className,
					String actualClass,
					String type, StringBuffer result) {
	result.append("\n" + Def.TAB + "/**");
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.abstract." + type, varName));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.text1." + type));
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.text2." + type));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.param.name", varName));
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.param.oid"));
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.param.objname"));
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.param.server"));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.return1."+type));
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.return2."+type, varName, className));
	result.append("\n" + Def.TAB + " * ");
	if (actualClass != null) {
	    if (isStandard()) {
		result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.std"));
		result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.std",actualClass));
		result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.std"));
	    } else {
		result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.gen"));
		result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.gen",actualClass));
		result.append("\n" + Def.TAB + " * " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.gen"));
	    }
	}
	result.append("\n" + Def.TAB + " **/" + "\n");
    }

    // Write the line of code that instantiate the Meta Class
    //
    protected void createMeta(String metaName, StringBuffer result) {
	result.append( Def.TAB2 +
		      Def.RETURN + "new " + metaName + 
		      "(this, " + OBJSRV + ")" + 
		      Def.SEMICOLON);
    }

    // Generate the MetaData factory method.
    //
    protected void generateMetaFactory(Context ctxt,
				       String varName, String oid, 
				       String metaName, String implName, 
				       String objname, StringBuffer result) {
	generateFactoryComments(varName,metaName,null,"meta",result);
	String metafactory = getMetaFactoryName(metaName);
	result.append(Def.TAB + Def.PROTECTED + metaName + " " + 
		      metafactory + 
		      "(String groupName,\n" +Def.TAB4 + "String groupOid," +
		      " ObjectName groupObjname, MBeanServer server) " 
		      + Def.LBRACE);
	createMeta(metaName,result);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    // Generate the MBean Factory method
    //
    protected void generateBeanFactory(Context ctxt,
				       String varName, String oid, 
				       String metaName, String implName, 
				       String objname, StringBuffer result) {
	String beanfactory  = getBeanFactoryName(implName);
	String stdbeanclass = getBeanInterfaceName(ctxt,varName,implName);
	String beanclass    = getBeanFactoryResult(ctxt,varName,implName);
	final String abstr  = (isAbstractFactory()?Def.ABSTRACT:"");
	generateFactoryComments(varName,implName,stdbeanclass,"bean",result);
	result.append(Def.TAB + Def.PROTECTED + abstr + beanclass +
		      " " + beanfactory + 
		      "(String groupName,\n" +Def.TAB4 + "String groupOid, "+
		      " ObjectName groupObjname, MBeanServer server)");
	if (isAbstractFactory()) {
	    result.append(Def.SEMICOLON + "\n");
	    return;
	}

	result.append(" "+ Def.LBRACE); 
	if (isStandard()) {
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.std"));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.std",stdbeanclass));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.std"));
	    result.append("\n" + Def.TAB2 + "//\n");
	} else {
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.gen"));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.gen",stdbeanclass));
	    result.append("\n" + Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.gen"));
	    result.append("\n" + Def.TAB2 + "//\n");
	}
	
	result.append( Def.TAB2 +
		       "if (server != null) \n" + Def.TAB3 +
		       Def.RETURN + "new " + implName + "(this,server)" + 
		       Def.SEMICOLON + Def.TAB2 +
		       "else \n" + Def.TAB3 +
		       Def.RETURN + "new " + implName + "(this)" + 
		       Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n");
    }

    public String getInitMethodName(Context ctxt, String varName) {
 	if (ctxt.gentype == MetaBeanGenerator.GENERIC_META)
	    return Def.INIT + ctxt.genericPrefix + varName;
 	if (ctxt.gentype == MetaBeanGenerator.STANDARD_META)
	    return Def.INIT + ctxt.standardPrefix + varName;
	return Def.INIT + varName;
    }

    public void generateInitNodeMethod(Context ctxt,
				       String varName, String oid, 
				       String metaName, 
				       String implName, String objname,
				       StringBuffer result) {
	result.append("\n" + Def.TAB + "/**");
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.init.group", varName));
	result.append("\n" + Def.TAB + " * "); 
	result.append("\n" + Def.TAB + " * " +  
		      MessageHandler.getMessage("generate.mib.comment.init.support1"));
	result.append("\n" + Def.TAB + " * " +  
		      MessageHandler.getMessage("generate.mib.comment.init.support2",getMetaFactoryName(metaName)));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " * " + 
		      MessageHandler.getMessage("generate.mib.comment.factory.param.server"));
	result.append("\n" + Def.TAB + " * ");
	result.append("\n" + Def.TAB + " **/\n");
	result.append(Def.TAB + Def.PROTECTED + Def.VOID + 
		      getInitMethodName(ctxt, varName) + "(" +
		      Def.MBEANSERVER + 
		      " server) " + "\n" + Def.TAB2 +
		      Def.THROWS + "Exception" + Def.LBRACE);
	populateMib(ctxt,varName,oid,metaName,implName,objname,result);
	result.append(Def.TAB + Def.RBRACE + "\n");
    }

    // Generate the code that initialize the Meta & MBean for a given
    // group.
    //
    public void populateMib(Context ctxt,
			    String varName, String oid, String metaName, 
			    String implName, String objname, 
			    StringBuffer result) {
	
	String objnamevar = "objname";
	String server = "server";
	String metafactory  = getMetaFactoryName(metaName);
	String beanfactory  = getBeanFactoryName(implName);
	String stdbeanclass = getBeanInterfaceName(ctxt,varName,implName);
	String beanclass    = getBeanFactoryResult(ctxt,varName,implName);

	//   final String oid = getGroupOid("MyGroup","1.2.3.4");
	//
	result.append(Def.TAB2 + "final String oid = " + Def.METH_T_GETOID + 
		      "(\"" + varName + "\", \"" + oid + "\")" + 
		      Def.SEMICOLON);

	//   ObjectName objname = null;
	//
	result.append(Def.TAB2 + "ObjectName " + objnamevar + " = null" + 
		      Def.SEMICOLON);

	//   if (server != null) {
	//
	result.append(Def.TAB2 + "if (" + server + " != null) " + 
		      Def.N_LBRACE);

	//      objname = getGroupObjectName("MyGroup",oid,"<default>");
	//
	result.append(Def.TAB3 + objnamevar + " = " + 
		      Def.METH_T_GETOBJNAME + "(\"" + varName + "\", oid, " +
		      objname + ")" + Def.SEMICOLON);

	//   }
	//
	result.append(Def.TAB2 + Def.RBRACE);

	//   final MyGroupMeta meta = createMyGroupMetaNode("MyGroup", oid,
	//            objname, server);
	//
	result.append(Def.TAB2 + "final " + metaName + " meta = " + 
		      metafactory + "(\"" + varName + "\", oid, " + 
		      objnamevar + ", " + server + ")" + Def.SEMICOLON);

	//   if (meta != null) {
	//      meta.registerTables( this, objectserver, server );
	//
	result.append(Def.TAB2 + "if (meta != null) " + Def.N_LBRACE);
	result.append(Def.TAB3 + "meta." + Def.METH_T_REGTABLES + 
		      "( this, server )" + Def.SEMICOLON);

	//      final MyGroupMBean group = createMyGroupMBean("MyGroup",
	//                oid, objname, server);
	//
	if (isGeneric()) {
	    result.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.gen"));
	    result.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.gen",stdbeanclass));
	    result.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.gen"));
	    result.append("\n" + Def.TAB3 + "//\n");
	    result.append(Def.TAB3 + "final " + beanclass + 
			  " group = " + beanfactory + "(\"" + varName + 
			  "\", oid, " + objnamevar + ", " + server + ")" + 
			  Def.SEMICOLON);
	} else {
	    result.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return3.std"));
	    result.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return4.std",stdbeanclass));
	    result.append("\n" + Def.TAB3 + "// " + MessageHandler.getMessage("generate.mib.comment.factory.note.return5.std"));
	    result.append("\n" + Def.TAB3 + "//\n");
	    result.append(Def.TAB3 + "final " + stdbeanclass + 
			  " group = (" + stdbeanclass + ") " + 
			  beanfactory + "(\"" + varName + 
			  "\", oid, " + objnamevar + ", " + server + ")" + 
			  Def.SEMICOLON);
	}

	if (isStandard(ctxt,varName)) {

	    //      meta.setInstance( group );
	    //
	    result.append(Def.TAB3 + "meta." + Def.SET_MOI + "( group )" +
			  Def.SEMICOLON);
	}

	if (isGeneric(ctxt,varName)) {

	    //      meta.setObjectName( objname );
	    //
	    result.append(Def.TAB3 + "meta." + Def.SET_OBJNAME + "( " +
			  objnamevar + " )" +
			  Def.SEMICOLON);
	}
	    
	//      registerGroup("MyGroup",oid,objname,meta,group,server);
	//
	result.append(Def.TAB3 + Def.METH_T_REGGROUP + "(\"" + varName +
		      "\", oid, " + objnamevar + ", meta, group, " + server 
		      + ")" + Def.SEMICOLON);

	//   }
	//
	result.append(Def.TAB2 + Def.RBRACE);
    }

    public void registerNode(MibNode node, Context ctxt, String domain) 
	throws IOException {
    
        // Get the name of the node
        //
        String varName= node.getSymbolName();
        if (varName == null)
            varName= getClassName(node.getComputedOid());
    
        // Get the oid of the node
        //
        String oid= node.getComputedOid();
        String metaName= 
	    MetaBeanGenerator.buildMetaName(context,context.prefix,varName);
        String implName= prefix + varName;
    
        String dot;
        if (packageName.length() != 0) 
            dot= ".";
        else
            dot= "";

	if (domain == null) domain="";
	else domain = domainSeparator + domain;

	String objname = new String("mibName + \""+domain+":name=" + 
				    packageName + dot + implName +"\"");
	
        populate_impl.append(Def.TAB2 +  "// " +  
			     MessageHandler.getMessage("generate.mib.comment.init.group", varName) + "\n" +
			     Def.TAB2 + "// " +
			     MessageHandler.getMessage("generate.mib.comment.init.support1") + "\n" +
			     Def.TAB2 + "// " +  
			     MessageHandler.getMessage("generate.mib.comment.init.support2",getMetaFactoryName(metaName)) + "\n" +
			     Def.TAB2 + "//\n");
	populate_impl.append(Def.TAB2 + getInitMethodName(ctxt, varName) + 
			     "(server)" + Def.SEMICOLON);
	populate_impl.append("\n");

	factory_impl.append("\n" + Def.TAB + Def.LINE);
	factory_impl.append("\n" + Def.TAB + "// ");
	factory_impl.append("\n" + Def.TAB + "// " + MessageHandler.getMessage("generate.mib.comment.init.group", varName));
	factory_impl.append("\n" + Def.TAB + "// ");
	factory_impl.append("\n" + Def.TAB + Def.LINE + "\n\n");

	generateInitNodeMethod(ctxt,varName,oid,metaName,implName,objname,
			       factory_impl);
	generateMetaFactory(ctxt,varName,oid,metaName,implName,objname,
			    factory_impl);
	generateBeanFactory(ctxt,varName,oid,metaName,implName,objname,
			    factory_impl);

    }
    
    public void endMib() throws IOException {
    
        closeInit();
        closeInitPreReg();
        closePopulate();
        write(init_impl.toString());
        write(initprereg_impl.toString());
        write(populate_impl.toString());
        write(factory_impl.toString());
	write(regmeta_impl.toString());
	write(getmeta_impl.toString());
	if (isStandard()) write(objsrv_impl.toString());

        // Add variables
        //
        writeVariableDefinition();
        write(Def.RBRACE);
	closeIO();
    }
  

//     protected void closeCode()  {
//         try {
//             write(Def.RBRACE); 
//         } catch( IOException e) {}
//     }

    protected void writeClassDeclaration() throws IOException {
        // Add some comments
        //
        write("/**\n" +
              " * " + MessageHandler.getMessage("generate.mib.comment.header", module) + "\n" +
              " * " + MessageHandler.getMessage("generate.mib.comment.modif") + "\n" +
              " */\n");
    
        write(Def.PUBLIC);
	if (isAbstractFactory()) 
	    write(Def.ABSTRACT);
	write(Def.CLASS + fileName + Def.EXTEND + Def.SNMP_MIB +
              Def.IMPLEMENT + Def.SERIALIZABLE + Def.LBRACE + "\n"); 
    }
  
    protected  void writeHeader() throws IOException {
        writePkg();
     
        // import the java.io package as everything needs to be serializable
        // import the java.util package
        //
        write("// java imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SERIALIZABLE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_HASHTABLE + Def.SEMICOLON);
     
        // import the JMX SNMP package                        
        //
        write("\n// jmx imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_MBEAN_SERVER + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_OBJECT_NAME + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_INSTANCE_ALREADY_EXISTS_EXCEPTION + 
	      Def.SEMICOLON);
	// NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        if (SyntaxMapper.useUnsigned)
            write(Def.IMPORT + Def.PKG_UNSIGNEDLONG + Def.SEMICOLON);
        // end of NPCTE fix for bugId 4692891
	
        // import the Java DMK SNMP package                        
        //
        write("\n// jdmk imports" + "\n//\n");
        write(Def.IMPORT + Def.PKG_SNMP_MIB + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_MIB_NODE + Def.SEMICOLON);
        write(Def.IMPORT + Def.PKG_SNMP_TABLE + Def.SEMICOLON);
	String[] intf = getInterfacePkgs();
	if (intf != null) {
	    for (int i=0; i < intf.length ; i++) {
		write(Def.IMPORT + intf[i] + Def.SEMICOLON);
	    }
	}
        write("\n");
    }
    
    protected void writeConstructor() throws IOException {
        write(Def.TAB + "/**\n" +
              Def.TAB  + " * " + MessageHandler.getMessage("generate.mib.comment.const") + "\n" +
              Def.TAB +" */\n");
        write(Def.TAB + Def.PUBLIC + fileName + "()" + Def.LBRACE + Def.TAB2 +
              "mibName = \"" + translatedMibName + "\"" + Def.SEMICOLON + 
              Def.TAB + Def.RBRACE + "\n");
    }
  
    protected void buildInitHeader() throws IOException {

	// header comments
	//
        init_impl.append(Def.TAB +"/**\n");
	init_impl.append(Def.TAB  + " * " + MessageHandler.getMessage("generate.mib.comment.init") + "\n");
	init_impl.append(Def.TAB +" */\n");

	// public void init() throws Exception {
	//
	init_impl.append(Def.TAB + Def.PUBLIC + Def.VOID + Def.METH_T_INIT + 
			 "() " + Def.THROWS + Def.EXCP_ILLEGAL + Def.LBRACE);
	
	//    // [... comments ...]
	//    //
	init_impl.append(Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.oneCall") + "\n");
	init_impl.append(Def.TAB2 + "//\n");

	//    if (isInitialized == true) {
	//        return;
	//    }
	//
	init_impl.append(Def.TAB2 + "if (" + ISINIT + " == true)" + 
			 Def.LBRACE);
	init_impl.append(Def.TAB3 + Def.RETURN + Def.SEMICOLON);
	init_impl.append(Def.TAB2 + Def.RBRACE + "\n");		   
	 
	//     try {
	//         populate(null, null);
	//
	init_impl.append(Def.TAB2 + "try " + Def.LBRACE);
	init_impl.append(Def.TAB3 + Def.METH_T_POPULATE + "(null, null)" + 
			 Def.SEMICOLON);

	//     } catch (IllegalArgumentException x) {
	//          throw x;
	//
	init_impl.append(Def.TAB2 + Def.N_RBRACE + 
			 " catch(" + Def.EXCP_ILLEGAL + " x) " + Def.LBRACE);
	init_impl.append(Def.TAB3 + "throw x" + Def.SEMICOLON);

	//     } catch (RuntimeException x) {
	//          throw x;
	//
	init_impl.append(Def.TAB2 + Def.N_RBRACE + 
			 " catch(RuntimeException x) " + Def.LBRACE);
	init_impl.append(Def.TAB3 + "throw x" + Def.SEMICOLON);

	//     } catch (Exception x) {
	//          throw new Error(x.getMessage());
	//
	init_impl.append(Def.TAB2 + Def.N_RBRACE + " catch(Exception x) " + 
			 Def.LBRACE);
	init_impl.append(Def.TAB3 + Def.THROW_NEW + "Error(x.getMessage())" + 
			 Def.SEMICOLON);

	//     }
	//
	init_impl.append(Def.TAB2 + Def.RBRACE + "\n");
    }

    static boolean isStandard(Context ctxt, String groupName) {
	return ((ctxt.gentype & MetaBeanGenerator.STANDARD_META) != 0);
    }

    static boolean isGeneric(Context ctxt, String groupName) {
	return ((ctxt.gentype & MetaBeanGenerator.GENERIC_META) != 0);
    }

    protected String getObjectServerClass() {
	if (isStandard())
	    return Def.SNMP_STANDARD_OSRV;
	if (isGeneric())
	    return Def.SNMP_GENERIC_OSRV;
	return "Object";
    }

    protected String[] getInterfacePkgs() {
	int count = 0;
	if (isStandard()) count += 1;
	if (isGeneric()) count += 1;
	if (count == 0) return null;
	String[] result =  new String[count];
	if (isGeneric()) {
	    result[--count] = Def.PKG_SNMP_GENERIC_OSRV;
	}
	if (isStandard()) {
	    result[--count] = Def.PKG_SNMP_STANDARD_OSRV;
	}
	return result;
    }

    protected void createObjectServer(StringBuffer result) {
	if (isStandard())
	    createStandardObjectServer(result);
	if (isGeneric())
	    createGenericObjectServer(result);
    }

    protected void createStandardObjectServer(StringBuffer result) {
	result.append(Def.TAB2 + "if (" + OBJSRV + " == null) \n");
	result.append(Def.TAB3 + OBJSRV + " = new " + 
		      getObjectServerClass() + "()" + 
		      Def.SEMICOLON);
    }

    protected void createGenericObjectServer(StringBuffer result) {
	result.append(Def.TAB2 + "if (" + OBJSRV + " == null) \n");
	result.append(Def.TAB3 + OBJSRV + " = new " + 
		      getObjectServerClass() + "(server)" + 
		      Def.SEMICOLON);
    }

    protected void buildPopulateHeader() throws IOException {
        populate_impl.append(Def.TAB +"/**\n" +
			     Def.TAB  + " * " + MessageHandler.getMessage("generate.mib.comment.init") + "\n" +
			     Def.TAB +" */\n" + 
			     Def.TAB + Def.PUBLIC + Def.VOID + Def.METH_T_POPULATE + "(MBeanServer server, ObjectName name) " + 
			     "\n" + Def.TAB2 + Def.THROWS + "Exception" + 
			     Def.LBRACE +
			     Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.oneCall") + "\n" +
			     Def.TAB2 + "//\n" + Def.TAB2 +
			     "if (" + ISINIT + " == true)" + Def.LBRACE + 
			     Def.TAB3 +
			     Def.RETURN + Def.SEMICOLON + Def.TAB2 + 
			     Def.RBRACE + "\n");
	createObjectServer(populate_impl);
	populate_impl.append("\n");
    }
  
    protected void buildGetStandardObjectServer(StringBuffer result) {
	result.append(Def.TAB + Def.PUBLIC + getObjectServerClass() + " " +
		      Def.METH_T_GETSTDOBJSRV + "()" 
		      + Def.LBRACE);
	createStandardObjectServer(result);
	result.append(Def.TAB2 + Def.RETURN + OBJSRV + Def.SEMICOLON);
	result.append(Def.TAB +  Def.RBRACE + "\n");
    }

    protected void buildRegisterTableMeta(StringBuffer result) {
	// public void registerTableMeta( String name, SnmpMibTable meta) {
	//
	result.append("\n" + Def.TAB + Def.LINE);
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + "// " + MessageHandler.getMessage("generate.mib.comment.implements", Def.METH_T_REGTABLEMETA,  Def.SNMP_MIB));
	result.append("\n" + Def.TAB + "// " + MessageHandler.getMessage("generate.mib.comment.seedoc", Def.SNMP_MIB));
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + Def.LINE + "\n\n");
	result.append(Def.TAB + Def.PUBLIC + Def.VOID + 
		      Def.METH_T_REGTABLEMETA + "( String name, " + 
		      Def.SNMP_TABLE + " meta)" + Def.LBRACE);
	
	//    if (metadatas == null) return null;
	//
	result.append(Def.TAB2 + "if (" + METAS + " == null) return" +
		      Def.SEMICOLON);

	//    if (name == null) return null;
	//
	result.append(Def.TAB2 + "if (name == null) return" + Def.SEMICOLON);

	//    metadatas.put(name,meta)
	//
	result.append(Def.TAB2 + METAS + ".put(name,meta)" + Def.SEMICOLON);

	// }
	//
	result.append(Def.TAB + Def.RBRACE + "\n");
    }
  
    protected void buildGetRegisteredMeta(StringBuffer result) {
	// public SnmpMibTable getRegisteredMeta( String name) {
	//
	result.append("\n" + Def.TAB + Def.LINE);
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + "// " + MessageHandler.getMessage("generate.mib.comment.implements", Def.METH_T_GETTABLEMETA, Def.SNMP_MIB));
	result.append("\n" + Def.TAB + "// " + MessageHandler.getMessage("generate.mib.comment.seedoc", Def.SNMP_MIB));
	result.append("\n" + Def.TAB + "// ");
	result.append("\n" + Def.TAB + Def.LINE + "\n\n");
	result.append(Def.TAB + Def.PUBLIC + Def.SNMP_TABLE + " " + 
		      Def.METH_T_GETTABLEMETA + "( String name )" + 
		      Def.LBRACE);
	
	//    if (metadatas == null) return null;
	//
	result.append(Def.TAB2 + "if (" + METAS + " == null) return null" +
		      Def.SEMICOLON);
	
	//    if (name == null) return null;
	//
	result.append(Def.TAB2 + "if (name == null) return null" + 
		      Def.SEMICOLON);

	//    return (SnmpTable) metadatas.get(name);
	//
	result.append(Def.TAB2 + Def.RETURN + "(" + Def.SNMP_TABLE + ") " +
		      METAS + ".get(name)" + Def.SEMICOLON);

	// }
	//
	result.append(Def.TAB + Def.RBRACE + "\n");
    }
  
    protected void buildInitPreRegHeader() throws IOException {
	// Generates comments
        initprereg_impl.append(Def.TAB +"/**\n");
	initprereg_impl.append(Def.TAB  + " * " + MessageHandler.getMessage("generate.mib.comment.preRegister") + "\n");
	initprereg_impl.append(Def.TAB +" */\n");

	// public ObjectName preRegister(MBeanServer server, ObjectName name)
	//        throws Exception {
	//
	initprereg_impl.append(Def.TAB + Def.PUBLIC + "ObjectName " + 
			       "preRegister(MBeanServer server, " + 
			       "ObjectName name)\n"); 
	initprereg_impl.append(Def.TAB3 + Def.THROWS + "Exception" + 
			       Def.LBRACE);

	// some comments
	//
	initprereg_impl.append(Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.oneCall") + "\n");
	initprereg_impl.append(Def.TAB2 + "//\n");

	// if (isIntitialized == true) {
	//    throw new InstanceAlreadyExistsException();
	// }
	//
	initprereg_impl.append(Def.TAB2 + "if (" + ISINIT + " == true)" + 
			       Def.LBRACE); 
	initprereg_impl.append(Def.TAB3 + Def.THROW_NEW + 
			       "InstanceAlreadyExistsException()" + 
			       Def.SEMICOLON);
	initprereg_impl.append(Def.TAB2 + Def.RBRACE + "\n");

	// some comments
	//
	initprereg_impl.append(Def.TAB2 + "// " + MessageHandler.getMessage("generate.mib.comment.cmf") + "\n");
	initprereg_impl.append(Def.TAB2 + "//\n");

	// this.server = server;
	//
	// populate(server,name);
	//
	initprereg_impl.append(Def.TAB2 + "this.server = server" + 
			       Def.SEMICOLON + "\n");
	initprereg_impl.append(Def.TAB2 + Def.METH_T_POPULATE +
			       "(server, name)" + Def.SEMICOLON + "\n");
    }
  
    protected void closeInit() throws IOException {
        init_impl.append(Def.TAB2 + ISINIT + " = true" + Def.SEMICOLON +
                         Def.TAB + Def.RBRACE + "\n");
    }
  
    protected void closePopulate() throws IOException {
	populate_impl.append(Def.TAB2 + ISINIT + " = true" + 
			     Def.SEMICOLON + Def.TAB + Def.RBRACE + "\n");
    }
  

    protected void closeInitPreReg() throws IOException {
        initprereg_impl.append(Def.TAB2 + ISINIT + " = true" + 
			       Def.SEMICOLON +
                               Def.TAB2 + Def.RETURN + "name" + 
			       Def.SEMICOLON +
                               Def.TAB + Def.RBRACE + "\n");
    }
  
    protected void writeVariableDefinition() throws IOException {
        write(Def.TAB + Def.PRIVATE + Def.BOOLEAN + 
	      ISINIT + " = false" + Def.SEMICOLON + "\n");
        write(Def.TAB + Def.PROTECTED + getObjectServerClass() +
	      " " + OBJSRV + Def.SEMICOLON + "\n");
        write(Def.TAB + Def.PROTECTED + Def.FINAL + "Hashtable " +
	      METAS + " = new Hashtable()" + Def.SEMICOLON);
    }
  
    
    // PRIVATE METHODS
    //----------------
  
    private String translateMibName() throws IOException {
        return translateMibName(module);
    }
  
    public static String translateMibName(String moduleName)
	throws IOException {
        String result= moduleName.trim();
        result= result.replace('-', '_');
        result= result.replace('.', '_');
        result= result.replace(',', '_');
        result= result.replace(',', '_');
        result= result.replace(' ', '_');
        return result;
    }

    public static String getMibClassName(String prefix, ASTMib aMib)
	throws IOException {
	return prefix + translateMibName(aMib.getModuleName());
    }

    public String setDomainSeparator(String s) {
	if (s!=null) domainSeparator = s;
	return domainSeparator;
    }

    // PRIVATE VARIABLES
    //------------------
  
    /**
     * Name of the module
     */
    protected String module= "";
  
    /**
     * Translated name of the module.
     */
    protected String translatedMibName= "";

    /**
     * Name of the file
     */
    protected String fileName= "";

    /**
     * Metadata type
     */
    protected int gentype = 0;
  
    /**
     * Init method
     */
    protected StringBuffer init_impl       = new StringBuffer();
    protected StringBuffer regmeta_impl    = new StringBuffer();
    protected StringBuffer getmeta_impl    = new StringBuffer();
    protected StringBuffer objsrv_impl     = new StringBuffer();
    protected StringBuffer initprereg_impl = new StringBuffer();
    protected StringBuffer populate_impl   = new StringBuffer();
    protected StringBuffer factory_impl    = new StringBuffer();
  
    /**
     * Variables used by the MIB.
     */
    protected StringBuffer var_def     = new StringBuffer();
    private String domainSeparator     = ".";
    static final private String OBJSRV = "objectserver";
    static final private String ISINIT = "isInitialized";
    static final private String METAS  = "metadatas";

}

