/*
 * @(#)file      MibGen.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.50
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


package com.sun.jdmk.tools;



import  java.io.*;
import  java.util.*;
import  com.sun.jdmk.tools.mibgen.*;

/**
 * The main class of the <CODE>mibgen</CODE> tool.
 * <P>
 * The class compiles a SNMP MIB expressed using SNMP V1 or SNMP V2 SMI
 * and generates code for implementing the MIB as a set of MBeans.
 * <P>
 * The compiler accepts SNMPv1 or SNMPv2 definitions mixed into a same MIB 
 * module.
 * A MIB file can contain several MIB modules.
 * <P>
 * In order to invoke the class, you need to invoke:
 * <UL>java com.sun.jdmk.tools.MibGen &lt;options&gt; &lt;mib files&gt;</UL>
 * where:
 * <UL><LI><B>-n</B>: Parses the mib files without generating code</LI>
 * <LI><B>-d dir</B>: Generates code in the specified target directory</LI>
 * <LI><B>-tp packageName</B>: Generates code within the specified Java 
 *     package</LI>
 * <LI><B>-desc</B>: Includes the "DESCRIPTION" clause of OBJECT-TYPE as a 
 *     comment in the generated code</LI>
 * <LI><B>-mo</B>: Only generates the metadata definitions file for the MIB 
 *     variables ({@link com.sun.management.snmp.SnmpOidTable} file);
 *     <BR>Incompatible with <CODE>-n</CODE></LI>
 * <LI><B>-mc</B>: Do not use the default MIB_CORE definitions file provided
 *  with Java Dynamic Management(TM) Kit;
 *  <BR>In this case, the user must specify the MIB_CORE definitions file
 *  as one of the &lt;mib files&gt; 
 *  <BR>(ex: java com.sun.jdmk.tools.MibGen -mc <I>mib</I> <I>my_mib_core</I>)
 *  </LI>
 * <LI><B>-a</B>: Generates code for all the mib files;
 * <BR>Without this option, the java code is generated only for the first mib 
 * file. In this case, the following mib files are simply used to resolve 
 * some definitions of the first mib file.</LI>
 * <LI><B>-p prefix</B>:  Uses the specified prefix for naming generated 
 *     classes</LI>
 * <LI><B>-g</B>:    Generates a <i>generic</i> version of the metadata that
 *                   will access the MBeans through the MBeanServer instead of 
 *                   using a direct reference.</LI>
 * <LI><B>-gp prefix</B>: Uses the specified prefix to name the <i>generic</i>
 *                   metadata classes. (e.g. the metadata class for group
 *                   system will be named System<i><b>prefix</b></i>Meta)</LI>
 * <LI><B>-sp prefix</B>: Uses the specified prefix to name the <i>standard</i>
 *                   metadata classes. (e.g. the metadata class for group
 *                   system will be named System<i><b>prefix</b></i>Meta)</LI>
 * <LI><b>-help</b>: Prints a usage message explaining how to invoke the 
 *                   compiler</LI>
 * <LI><b>&lt;mib files&gt;</b>: List of MIB files to compile</LI>
 * </UL>
 *
 * In order to directly invoke the class, Java Dynamic Management(TM) Kit 
 * provides a script called <b>mibgen</b>.<br>
 * On UNIX platforms the script is provided under 
 * &lt;install_dir&gt;/SUNWjdmk/5.1/bin.
 * On Windows platforms the script is provided under 
 * &lt;install_dir&gt;\SUNWjdmk\5.1\bin.
 *
 * <p>
 * Note:
 * <br>The order followed by mibgen to find the MIB_CORE definitions file is:
 * <ol>
 * <li>The user MIB_CORE definitions file specified in the &lt;mib files&gt; 
 * using the <CODE>-mc</CODE> mibgen option.</li>
 * <li>The user command line parameter specified using the 
 * <CODE>-Dmibcore.file</CODE> java option.</li>
 * <li>The default MIB_CORE definitions file provided with jdmk under 
 * &lt;install_dir&gt;/etc/mibgen (<I>mib_core.txt</I>).
 * <br>To succeed, you must be able to derive the jdmk installation directory 
 * from the CLASSPATH environment variable.
 * Otherwise, mibgen will look for the <I>mib_core.txt</I> file under
 * &lt;current_dir&gt;/etc/mibgen.</li>
 * </ol>
 *
 */
public final class MibGen {
  
    /**
     * Create a new MIB compiler.
     *
     * @param o Output stream to use for printing traces
     * @param prog Command name to use when printing messages
     */
    private MibGen(OutputStream o, String prog) {
        out     = o;
        Trace.setOutput(out);
        program = prog;
    }

    private void output(String msg) {
        try {
            for (int i = 0; i < msg.length() ; i++) {
                out.write(msg.charAt(i));
            }
      
            out.write('\n');
        } catch (IOException e) {
        }
    }
  
    private int error(String msg) { 
        Trace.error(msg);
        Trace.info(MessageHandler.getMessage("compile.error.stop"));
        return (1);
    }  
  
    private void usage() {
        Trace.info("Usage: " + program + " <options> <mib files>");
        Trace.info("\n" + MessageHandler.getMessage("usage.where"));
        Trace.info("  -n\t\t" + MessageHandler.getMessage("usage.n"));
        Trace.info("  -d <dir>\t" +  MessageHandler.getMessage("usage.dir"));
        Trace.info("  -tp <pkgName>\t" +  
                   MessageHandler.getMessage("usage.tp"));
        Trace.info("  -desc\t\t" +  MessageHandler.getMessage("usage.desc"));
        Trace.info("  -mo\t\t" + MessageHandler.getMessage("usage.mo"));
        Trace.info("  -mc\t\t" + MessageHandler.getMessage("usage.mc"));
        Trace.info("  -a\t\t" +  MessageHandler.getMessage("usage.a"));
        Trace.info("  -g\t\t" +  MessageHandler.getMessage("usage.g"));
        Trace.info("  -gp\t\t" +  MessageHandler.getMessage("usage.gp"));
        Trace.info("  -sp\t\t" +  MessageHandler.getMessage("usage.sp"));
        Trace.info("  -p <prefix>\t" +  MessageHandler.getMessage("usage.p"));
        Trace.info("  -X:<advanced-option>\t" + 
                   MessageHandler.getMessage("usage.x"));
        Trace.info("  -help\t\t" + MessageHandler.getMessage("usage.help"));
        //Trace.info("\tmib: " + MessageHandler.getMessage("usage.mib"));
    }

    private void xusage() {
        Trace.info(MessageHandler.getMessage("usage.x.option") + ":");
        Trace.info("  -X:define\t" + 
                   MessageHandler.getMessage("usage.x.define"));
        Trace.info("  -X:use-display-hint[:on|:off]\t" +
                   MessageHandler.getMessage("usage.x.display"));
        Trace.info("  -X:abstract-mib[:on|:off]\t" +
                   MessageHandler.getMessage("usage.x.abstract"));
        Trace.info("  -X:no-table-access[:on|:off]\t" +
                   MessageHandler.getMessage("usage.x.table.noaccess"));
        Trace.info("  -X:target[:<target release number>]\t" + 
                    MessageHandler.getMessage("usage.x.target"));
        // NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        Trace.info("  -X:use-unsigned-long[:on|:off]\t" + 
                    MessageHandler.getMessage("usage.x.ulong"));
        // end of NPCTE fix for bugId 4692891
        Trace.info("  -X:help\t" + 
                   MessageHandler.getMessage("usage.x.help"));
    }

    /**
     * Parse the arguments, set all private fields.
     * The vector v should contains the names of files to compile
     */
    private boolean parseArgs(String args[]) {
        fileList = new Vector();
        // NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        boolean useUnsigned = false;
        // end of NPCTE fix for bugId 4692891
    
        boolean setNoption= false;
        if (args.length == 0) {
            usage();
            return (false);
        }

        for (int i = 0 ; i < args.length ; i++) {
            if (args[i].equals("-X:target:5.0"))
                System.setProperty(MibGenProperties.CONFIG_RESOURCE, MibGenProperties.MIBGEN_RESOURCE_NAME_JDMK50);
        }

        for (int i = 0 ; i < args.length ; i++) {
            if (args[i].equals("-tp")) {
                if ((i + 1) < args.length) {
                    specificPackageName = args[++i];
                }
                else {
                    Trace.error(MessageHandler.
                                getMessage("compile.error.option.level", 
                                           "-tp"));        
                    usage();
                    return (false);
                }
            } 
            else if (args[i].equals("-d")) {
                if ((i + 1) < args.length) {
                    targetDir = args[++i];
                }
                else {
                    Trace.error(MessageHandler.
                                getMessage("compile.error.option.level", 
                                           "-d"));        
                    usage();
                    return (false);
                }
            }  
            else if (args[i].equals("-p")) {
                if ((i + 1) < args.length) {
                    prefix = args[++i];
                }
                else {
                    Trace.error(MessageHandler.
                                getMessage("compile.error.option.level", 
                                           "-p"));        
                    usage();
                    return (false);
                } 
            }
            else if (args[i].equals("-n")) {
                setNoption= true;
                generateCode= false;
                generateMgrCode= false;
            }
            else if (args[i].equals("-X:define")) {
                if ((i + 1) < args.length) {
                    final String property = args[++i];
                    try {
                        MibGenProperties.define(property);
                    } catch (Exception x) {
                        Trace.error(MessageHandler.getMessage(
                                    "compile.error.option.level", 
                                    "-X:define " + property));        
                        Trace.info("Syntax of -X:define option is:");
                        Trace.info("   -X:define " +
                                   "<property-name>=<property-value>\t" 
                                   +  MessageHandler.
                                   getMessage("usage.x.define"));
                        xusage();
                        return (false);
                    }
                }
                else {
                    Trace.error(MessageHandler.
                                getMessage("compile.error.option.level", 
                                           "-X:define"));        
                    Trace.info("Syntax of -X:define option is:");
                    Trace.info("   -X:define "+
                               "<property-name>=<property-value>\t" 
                               +  MessageHandler.getMessage("usage.x.define"));
                    xusage();
                    return (false);
                } 

            } 
            else if (args[i].equals("-X:use-display-hint") ||
                     args[i].equals("-X:use-display-hint:on")) {
                MibGenProperties.
                    setProperty(MibGenProperties.OPTION_USE_DISPLAY_HINT,
                                "true");
            }
            else if (args[i].equals("-X:use-display-hint:off")) {
                MibGenProperties.
                    setProperty(MibGenProperties.OPTION_USE_DISPLAY_HINT,
                                "false");
            }
            else if (args[i].equals("-X:abstract-mib")
                     || args[i].equals("-X:abstract-mib:on")) {
                MibGenProperties.
                    setProperty(MibGenProperties.OPTION_MIB_FACTORY_ABSTRACT,
                                "true");
            }
            else if (args[i].equals("-X:abstract-mib:off")) {
                MibGenProperties.
                    setProperty(MibGenProperties.OPTION_MIB_FACTORY_ABSTRACT,
                                "false");
            }
            else if (args[i].equals("-X:no-table-access")
                     || args[i].equals("-X:no-table-access:on")) {
                MibGenProperties.
                    setProperty(MibGenProperties.OPTION_MBEAN_TABLE_ACCESSOR,
                                "false");
            }
            else if (args[i].equals("-X:no-table-access:off")) {
                MibGenProperties.
                    setProperty(MibGenProperties.OPTION_MBEAN_TABLE_ACCESSOR,
                                "true");
            } else if (args[i].equals("-X:target:5.0")) {
                // Do nothing, done in previous loop.
            }
            else  if (args[i].equals("-X:use-unsigned-long") ||
                      args[i].equals("-X:use-unsigned-long:on")) {
                 useUnsigned= true;
            } else if (args[i].equals("-X:help")) {
                xusage();
                return false;
            } 
            else if (args[i].startsWith("-X:")) {
                Trace.error(MessageHandler.
                            getMessage("compile.error.option.invalid", 
                                       args[i]));
                xusage();
                return false;
            } 
            else if (args[i].equals("-g")) {
                gentype = MetaBeanGenerator.GENERIC_META;
            } 
            else if (args[i].equals("-gp")) {
                if ((i + 1) < args.length) {
                    genericPrefix = args[++i];
                }
                else {
                    Trace.error(MessageHandler.
                                getMessage("compile.error.option.level",
                                           "-gp"));        
                    usage();
                    return (false);
                }
            } 
            else if (args[i].equals("-sp")) {
                if ((i + 1) < args.length) {
                    standardPrefix = args[++i];
                }
                else {
                    Trace.error(MessageHandler.
                                getMessage("compile.error.option.level", 
                                           "-sp"));        
                    usage();
                    return (false);
                }
            } 
            else if (args[i].equals("-mo")) {
                generateMgrCodeOnly= true;
                generateCode= false;
                generateMgrCode= false;
            } 
            else if (args[i].equals("-a")) {
                forAll= true;
            } 
            else if (args[i].equals("-X:help")) {
                xusage();
                return false;
            } 
            else if (args[i].equals("-help")) {
                usage();
                return false;
            } 
            else if (args[i].equals("-h")) {
                usage();
                return false;
            } 
            else if (args[i].equals("-desc")) {
                commentRequested= true;
            } 
            else if (args[i].equals("-mc")) {
                defaultMibCore= false;
            } 
            else if (args[i].startsWith("-")) {
                Trace.error(MessageHandler.
                            getMessage("compile.error.option.invalid", 
                                       args[i]));
                usage();
                return false;
            } 
            else {
                fileList.addElement(args[i]);
                lastFile= args[i];
            }
        }
        // NPCTE fix for bugId 4692891, esc 537693, MR,  June 2002
        SyntaxMapper.fillTable(useUnsigned);
        // end of NPCTE fix for bugId 4692891
        
        if ((setNoption == true) && (generateMgrCodeOnly == true)) {
            Trace.error(MessageHandler.
                        getMessage("compile.error.option.incompat", "-n", 
                                   "-mo"));        
            usage();
            return (false);
        } 

        

        if (validTargetDir() == false)
            return (false);
        return (true);
    }
      
    private synchronized boolean doCompile() {
        String  name;
        Class   c;
        boolean errorStatus= true;
        ModulesHandler modulesHandler = new ModulesHandler();
    
        // If there is no specific mib-core defined in the command-line,
        // we use the default one.
        //
        ASTMibs mibs = null;
        if (defaultMibCore == true) {
            String mibCoreFile;
            if ((mibCoreFile = System.getProperty(com.sun.jdmk.defaults.
                                    JdmkProperties.MIB_CORE_FILE)) == null) {
                mibCoreFile = com.sun.jdmk.defaults.DefaultPaths.
                    getEtcDir("mibgen" + File.separator + "mib_core.txt");
            }
            Trace.info(MessageHandler.
                       getMessage("compile.info.start.mibcore",mibCoreFile));
            Parser parser= null;
      
            try {
                parser= new Parser(new FileInputStream(mibCoreFile));
                
                // The file is now open. Try to parser it !
                //-----------------------------------------
                mibs = parser.Mibs(modulesHandler);
                
            } catch (FileNotFoundException e) {
                Trace.warning(MessageHandler.
                              getMessage("compile.error.noMibCoreFile", 
                                         mibCoreFile));
            } catch (ParseException e) {
                Trace.error(e.getMessage());
                errorStatus= false;
            } catch(Exception err) {
                if (err instanceof RuntimeException)
                    throw (RuntimeException) err;
                Trace.error (err.getMessage());
                errorStatus=false;
            }
        }
        
        for (int i = fileList.size() - 1; i >= 0; i--) {
            name = (String)fileList.elementAt(i);     
            Trace.info(MessageHandler.getMessage("compile.info.start",  name));
            Parser parser= null;
      
            try {
                parser= new Parser(new FileInputStream(name));
            } catch (FileNotFoundException e) {
                Trace.error(MessageHandler.
                            getMessage("compile.error.noFile", name));
                return false;
            }
      
            // The file is now open. Try to parser it !
            //-----------------------------------------
            try {
                ASTMibs n = parser.Mibs(modulesHandler);
                lastMib= n;
                //n.dump("");
            } catch (ParseException e) {
                Trace.error(e.getMessage());
                errorStatus= false;
            } catch(Exception err) {
                if (err instanceof RuntimeException)
                    throw (RuntimeException) err;
                Trace.error (err.getMessage());
                errorStatus=false;
            }
        }
       
        // Resolve locally each single MIB module
        //
        if (!modulesHandler.resolve()) {
            // It means that an error occurs during resolution.
            // The unresolved symbols were all printed !
            //
            Trace.info(MessageHandler.getMessage("compile.error.stop"));
            return false;
        }
  
        // Build tree representations of the MIBs
        //
        if (!modulesHandler.buildMibTrees()){
            // Something wrong somewhere. Error messages were printed when the
            // errors were discovered. This allows to have all the errors 
            // through one compilation instead of stopping each time an 
            // error is detected...
            //
            Trace.info(MessageHandler.getMessage("compile.error.stop"));
            return false;
        }
        //modulesHandler.dumpMibTrees(">");
        //Trace.info("That's all folks !");
    
        // When using the option -a, we don't want to generate code for the 
        // default mib-core file,
        // so we have to remove it from the module handler.
        //
        if (defaultMibCore == true) {
            // Normally, if defaultMibCore == true, the mibs have been 
            // initialized...
            if (mibs != null) {
                for(Enumeration e = mibs.mibElements(); e.hasMoreElements();) {
                    ASTMib aMib= (ASTMib)e.nextElement();
                    modulesHandler.removeMibModule(aMib.getModuleName());
                }
            }
        }
        
        status= generateAgentCode(modulesHandler) && 
            generateMgrCode(modulesHandler) &&errorStatus;
        return status;
    }
    
    private boolean generateAgentCode(ModulesHandler modulesHandler) {    
        if (generateCode == false) {
            // no need to go any further
            //
            return true;
        }
    
        // Check if we have something to generate
        //
        if (lastMib == null) {
            Trace.info(MessageHandler.getMessage("compile.error.stop"));
            return false;
        }
    
        // Get the list of mibs for which we have to generate code
        //
        Enumeration e;
        if (forAll == true) {
            e= modulesHandler.mibElements();
        } else {
            e= lastMib.mibElements();
        }
      
        if (e == null) {
            Trace.info(MessageHandler.getMessage("compile.error.stop"));
            return false;
        }
    
        // instantiate a code generator
        //
        boolean status= true;

        Context ctxt = new Context();
        ctxt.genItfTableAccess = 
            MibGenProperties.getBooleanProperty(MibGenProperties.
                                                OPTION_MBEAN_TABLE_ACCESSOR,
                                                true);
        ctxt.genAFNGetter = 
            MibGenProperties.getBooleanProperty(MibGenProperties.
                                                OPTION_MBEAN_AFN_GETTER,
                                                true);
        ctxt.packageName    = specificPackageName;
        ctxt.prefix         = prefix;
        ctxt.dir            = targetDir;
        ctxt.genericPrefix  = genericPrefix;
        ctxt.standardPrefix = standardPrefix;
        ctxt.modules        = modulesHandler;
        
        CodeGenerator codeGenerator = 
            new CodeGenerator(new ResourceManager(), ctxt);
        for(; e.hasMoreElements();) {
            ASTMib aMib= (ASTMib) e.nextElement();

            // Quick hack to be able to specify that comments from the MIB
            // (the DESCRIPTION clause) be included.
            aMib.setDescriptionHandling(commentRequested);
      
            // The mib might need access to other MIB module ...
            //
            aMib.setModulesHandler(modulesHandler);
            Trace.info(MessageHandler.
                       getMessage("compile.info.start.agent", 
                                  aMib.getModuleName()));
            try {
                codeGenerator.generateCode(aMib,gentype);
            } catch(IOException a) {
                Trace.error(MessageHandler.getMessage("compile.error.io", 
                                                      aMib.getModuleName()));
                status= false;
            }
        }
        return status;
    }
    
 
    private boolean generateMgrCode(ModulesHandler modulesHandler) {    
        if ((generateMgrCode == false) && (generateMgrCodeOnly == false)) {
            // no need to go any further
            //
            return true;
        }
    
        // Check if we have something to generate
        //
        if (lastMib == null) {
            Trace.info(MessageHandler.getMessage("compile.error.stop"));
            return false;
        }
    
        // Get the list of mibs for which we have to generate code
        //
        Enumeration e;
        if (forAll == true) {
            e= modulesHandler.mibElements();
        } else {
            e= lastMib.mibElements();
        }
      
        if (e == null) {
            Trace.info(MessageHandler.getMessage("compile.error.stop"));
            return false;
        }
    
        // instantiate a code generator
        //
        Context ctxt = new Context();
        ctxt.genItfTableAccess = 
            MibGenProperties.getBooleanProperty(MibGenProperties.
                                                OPTION_MBEAN_TABLE_ACCESSOR,
                                                true);
        ctxt.genAFNGetter = 
            MibGenProperties.getBooleanProperty(MibGenProperties.
                                                OPTION_MBEAN_AFN_GETTER,
                                                true);
        ctxt.packageName = specificPackageName;
        ctxt.prefix      = prefix;
        ctxt.dir         = targetDir;
        ctxt.modules     = modulesHandler;

        StoreGenerator codeGenerator= 
            new StoreGenerator(new ResourceManager(),ctxt);

        for(; e.hasMoreElements();) {
            ASTMib aMib= (ASTMib) e.nextElement();
            ctxt.mib   = aMib;

            // The mib might need access to other MIB module ...
            //
            aMib.setModulesHandler(modulesHandler);
            Trace.info(MessageHandler.
                       getMessage("compile.info.start.oidtable", 
                                  aMib.getModuleName()));
            try {
                codeGenerator.generateCode(aMib);
            } catch(IOException a) {
            }
        }
        return true;
    }
  
  
    /**
     * Start compilation.
     */
    private void run() {
        if (fileList != null && fileList.size() > 0) {
            status = doCompile();
        }
    }
  
    /**
     * The <CODE>main</CODE> method of the <CODE>mibgen</CODE> compiler.
     * <P>This method creates an instance of <CODE>MibGen</CODE> and invokes 
     * the compiler.
     * <BR>
     * If an error occurs, the method exits the VM with a status of 1.
     * <BR>
     * If the compilation succeeds, the method exits the VM with a status of 0.
     * <P>This method allows you to specify optional command-line parameters 
     * listed {@link com.sun.jdmk.tools.MibGen above}.
     */
    public static void main(String args[]) {
   
        // Make sure we can have access to this message !
        //
        String outOfMemory= 
            MessageHandler.getMessage("compile.error.internal.outmemory");
        
        // Create compiler and compile
        try {
            int result = compile(args);
            Trace.conclude();
            System.exit(result);
        } catch(Throwable e) {
            if (e instanceof OutOfMemoryError)
                Trace.error(outOfMemory);
            else {
                Trace.error(MessageHandler.
                         getMessage("compile.error.internal", e.toString()));
                if (System.getProperty("LEVEL_DEBUG")!=null) 
                    e.printStackTrace();
                Trace.conclude();
            }
            System.exit(1);
        }
    }
  
    /**
   * The <CODE>compile</CODE> method of the <CODE>mibgen</CODE> compiler.
   * <P>This method creates an instance of <CODE>MibGen</CODE> and invokes the 
   * compiler.
   * <BR>
   * If an error occurs, the method returns with a status of 1.
   * <BR>
   * If the compilation succeeds, the method return with a status of 0.
   */
    public static int compile(String args[]) {
        MibGen compiler = new MibGen(System.out, "mibgen");
        return(compiler.startCompile(args) ? 0 : 1);
    }
  
    
    private synchronized boolean startCompile(String args[]) {
        // Start processing the args
        //

        try {
            if (!parseArgs(args)) {
                return (false);
            }
    
            if (fileList.size() == 0) {
                usage();
                return (false);
            }
            return (doCompile());
        } finally {
            final String filename = 
                System.getProperty("com.sun.jdmk.tools.mibgen.config.store");
            if (filename != null) try {
                MibGenProperties.store(filename);
            } catch (Throwable t) {
                // XXX Revisit: Must log an error!!!
                t.printStackTrace();
            }
        }
    }
    
    // VALIDATE SOME ARGUMENTS
    //
    private boolean validTargetDir() {
        // Only check the target fir if we need to generate code !!
        //
        if (generateCode == false)
            return true;

        File dir;
        try {
            dir = new File(targetDir);
        } catch (NullPointerException e) {
            return false;
        }
        if (dir.exists() == false) {
            error((MessageHandler.getMessage("compile.error.noDir", 
                                             targetDir)));
            return false;
        }
        if (dir.canWrite() == false) {
            error((MessageHandler.
                   getMessage("compile.error.noWritePermission", targetDir)));
            return false;
        }
        return true;
    }
  
    // VARIABLES
    //----------
  
    /**
     * Name of this program for error messages.
     */
    private String program;
  
    /**
     * The stream where error message are printed.
     */
    private OutputStream out;
  
    /**
     * Compilers flags
     */ 
    private String genericPrefix="";
    private String standardPrefix="";
    private String specificPackageName="";
    private String targetDir= ".";
    private String prefix= "";
    private boolean generateCode= true;
    private boolean generateMgrCode= true;
    private boolean generateMgrCodeOnly= false;
    private boolean standAlone= false;
    private boolean forAll= false;
    private String lastFile= "";
    private ASTMibs lastMib;
    private boolean status= true;
    private boolean commentRequested= false;
    private boolean defaultMibCore= true;
    private int gentype = MetaBeanGenerator.STANDARD_META;

    /**
     * List of files to compile
     */
    private Vector      fileList;
  
    /**
     * Version of the implementation
     */
    private static final String sccs_id = "@(#)MibGen.java 4.50 03/08/07 SMI";
}
