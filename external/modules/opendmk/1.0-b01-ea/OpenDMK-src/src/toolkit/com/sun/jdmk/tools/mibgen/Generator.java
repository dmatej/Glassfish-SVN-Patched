/*
 * @(#)file      Generator.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.21
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
public abstract class Generator implements Serializable {
  
    public Generator(ResourceManager manager, Context ctxt) {
        this.manager= manager;

        // do not call setContext() here because it would lead to 
        // initialization nightmares due to subclassing
        //
        packageName=  ctxt.packageName;
        this.prefix=  ctxt.prefix;
        targetDir= ctxt.dir;
        context = ctxt;
        defaultOidPrefix = ctxt.defaultOidPrefix;
    }
  
    public void setContext(Context ctxt) {
        packageName=  ctxt.packageName;
        this.prefix=  ctxt.prefix;
        targetDir= ctxt.dir;
        defaultOidPrefix = ctxt.defaultOidPrefix;
        context = ctxt;
    }

    public void setMib(ASTMib aMib) {
        mib= aMib;
        context.mib = aMib;
    }
  
    // ----------------------------------------------------------------
    // Returns true if the variable is of type RowStatus
    // ----------------------------------------------------------------
    //
    protected boolean isRowStatus(Context ctxt, String var) {
        String typeRef = ctxt.getTypeRef(var);
        if (typeRef == null) return false;
        return typeRef.equals("RowStatus");
    }

    //public abstract void generateCode(ASTMib aMib) throws IOException;
  
    protected RandomAccessFile openFile(String name) throws IOException {

        // Check validity of file name
        //
        File f= checkFile(name);

        // So far so good. Open the file.
        //
        return new RandomAccessFile(f, "rw");
    }
  
    protected synchronized void write(String msg)
        throws IOException {
        try {
            out.writeBytes(msg);

        } catch (IOException e) {
            throw new IOException(e.toString());
        }  
    }

    // Specify the version of mibgen used for generating the code
    //
    protected void writeVersion() throws IOException {
        String msgid = "generate.version";
        if (isGeneric())
            msgid = "generate.version.generic";
        else if (isStandard())
            msgid = "generate.version.standard";
        
        write("\n//\n// " + 
              MessageHandler.getMessage(msgid, mib.getModuleName()) +
              "\n//\n\n");
    }

    protected void writePkg() throws IOException {
        // Put package definition
        //
        if (packageName.length() != 0) {
            write(Def.PACKAGE + packageName + Def.SEMICOLON);
        }
     
        // Specify the version of mibgen used for generating the code
        //
        writeVersion();
    }
    
    /**
     * Replace all the occurrences of "." with "_".
     */
    protected  String translateOid(String oid) {
        return translateOid(context,oid);
    }

    public static String translateOid(Context ctxt, String oid) {
        if (ctxt.prefix == null || ctxt.prefix.length() == 0)
            return  ctxt.defaultOidPrefix + oid.replace('.', '_');
        return oid.replace('.', '_');
    }

    // PACKAGE METHODS
    //----------------

    boolean isStandard() {
        return ((context.gentype & MetaBeanGenerator.STANDARD_META) != 0);
    }

    boolean isGeneric() {
        return ((context.gentype & MetaBeanGenerator.GENERIC_META) != 0);
    }

    // PRIVATE METHODS
    //----------------

    /**
     * Functions to handle file operations
     */
    private File checkFile(String name) throws IOException {
        //
        // Check if the file exist to save it in
        // name~ in case of.
        //
        File f;
        if (targetDir.length ()!= 0)
            f = new File(targetDir, name);
        else 
            f= new File(name);

        if (f.exists()) {
            // 
            // Rename the file to name~
            // 
            String sname = name + "~";
            File   f1    = new File(targetDir,sname);

            if (f1.exists()) {
                f1.delete();
            }  
            
            // If we cannot rename the file, this means that it belongs to another user.
            //
            if (f.renameTo(f1) == false) {
                Trace.error(MessageHandler.getMessage("compile.error.noWritePermission", f.getName()));
                throw new IOException("The file " + f.getName() + " belongs to another user");
            }
        }

        return f;
    }

    public static String getClassName(Context ctxt, String oid) {
        // See if we know about the symbol ...
        //
        Hashtable oidTranslation= ctxt.mib.oidTranslation;
        String symbol= (String) oidTranslation.get(oid);
        if (symbol == null) {
            symbol= translateOid(ctxt,oid);
        }
    
        // Trimming operation ...
        //
        String result= symbol.trim();
        result= result.replace('-', '_');
        result= result.replace('.', '_');
        result= result.replace(',', '_');
        result= result.replace(',', '_');
        result= result.replace(' ', '_');
        //Trace.info("symbol " + symbol + " for oid= " + oid);
        return Character.toUpperCase(result.charAt(0)) + result.substring(1, result.length());
        // return (String) result;
    }
        
  
    protected String getClassName(String oid) {
        return getClassName(context, oid);
    }
  

    protected void closeIO() throws IOException {
        out.close();
    }
    // PRIVATE VARIABLES
    //

    /**
     * Mib for which code is generated
     */
    protected  ASTMib mib;
  
    /**
     * Package name of the generated code
     */
    protected  String packageName= "";
  
    /**
     * Prefix to use for the created file
     */
    protected String prefix= "";
  
    /**
     * Target directory
     */
    protected String targetDir= "";
    protected File targetFile;
  
    /**
     * a file descriptor
     */
    protected RandomAccessFile out;
  
    /**
     * Resource manager to use for this generator
     */
    ResourceManager manager;
    Context         context;

    /**
     * Use to name a class when there is no symbol associated nor prefix to use
     * for naming.
     */
    private String defaultOidPrefix = "oid_";
}
