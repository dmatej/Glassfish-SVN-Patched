/*
 * @(#)file      ModulesHandler.java
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



import java.util.*;


/**
 * Central repository of all parsed modules.
 *
 */

public class ModulesHandler {

  public ModulesHandler() {
    modules= new Hashtable();
    id_resolvers= new Hashtable();
    ref_resolvers= new Hashtable();
    ind_resolvers= new Hashtable();
  }
  
  /**
   * Register a mib module.
   */
  public void addMibModule(String mibName, ASTMib theNewMib) throws SemanticException {
    ASTMib aMib= (ASTMib) modules.get(mibName);
    if (aMib != null) {
      // Hey why do you want to parse the same thing twice ??
      //
      throw new SemanticException(MessageHandler.getMessage("compile.error.duplicate.module", mibName));
    }
    
    modules.put(mibName, theNewMib);
    Trace.info(MessageHandler.getMessage("compile.info.endParse", mibName));
    return;
  }
  
  /**
   * Remove a mib module.
   */
  public void removeMibModule(String mibName) {
    modules.remove(mibName);
  }
  
  /**
   * get an enumeration containing all the mib modules
   */
  public Enumeration mibElements() {
    return modules.elements();
  }
  
  /**
   * Resolve all the symbols defined in all the registered mibs.
   */
  public boolean resolve() {
    localResolve();
    globalResolve();
    if (!checkSymbolClosure())
      return false;
     if (!computeValues())
      return false;
    return true;
  }
  
  /**
   * Build tree representation of a Mib
   */
  public boolean buildMibTrees() {
    boolean result= true;
    
    for(Enumeration e= modules.elements(); e.hasMoreElements();) {
      ASTMib mib= (ASTMib) e.nextElement();
      boolean ok= mib.buildMibTree();
      if (ok == false)
	result= false;
    }
    return result;
  }
  
  public void dumpMibTrees(String prefix) {
    for(Enumeration e= modules.elements(); e.hasMoreElements();) {
      ASTMib mib= (ASTMib) e.nextElement();
      mib.printMibTree(prefix);
    }
  }
  
  /**
   * The method allows to retrieve a Mib node object across all the MIB definition ...
   */
  public MibNode findNodeWithName(String name) {
    MibNode result= null;
     for(Enumeration e= modules.elements(); e.hasMoreElements();) {
      ASTMib mib= (ASTMib) e.nextElement();
      MibTree tree= mib.getMibTree();
      MibNode root= tree.getRoot();
      result= root.findNodeWithName(name);
      if (result != null)
	return result;
     }
     return result;
  }
    
   /**
   * Returns the version of this class.
   */
  public static String getClassVersion () {
    return (sccs_id);
  }
  
  // PRIVATE METHODS
  //-----------------
   
  private boolean computeValues() {
    boolean ok= true;
     for(Enumeration e= modules.elements(); e.hasMoreElements();) {
       ASTMib mib= (ASTMib) e.nextElement();
       if (!mib.computeValues())
	 ok= false;
     }
     return ok;
  }
  
  private void localResolve() {
    for(Enumeration e= modules.elements(); e.hasMoreElements();) {
      ASTMib mib= (ASTMib) e.nextElement();
      mib.localResolve();
      id_resolvers.put(mib.getModuleName(), mib.getIdResolver());   
      ref_resolvers.put(mib.getModuleName(), mib.getRefResolver());
      ind_resolvers.put(mib.getModuleName(), mib.getIndexResolver());
    }
  }
  
  /**
   * Have the global resolution been performed by the MIB module itself. This should 
   * allow to add support for the import clause contained in the MIB.
   */
  private void globalResolve() {
    for(Enumeration e= modules.elements(); e.hasMoreElements();) {
      ASTMib mib= (ASTMib) e.nextElement();
      mib.globalResolve(id_resolvers, ref_resolvers, ind_resolvers);   
    }
  }
 
  /**
   * Print all the unresolved symbols at once ...
   */
  private boolean checkSymbolClosure() {
    boolean ok= true;
    for(Enumeration e= modules.elements(); e.hasMoreElements();) {
      ASTMib mib= (ASTMib) e.nextElement();
      if (!mib.printUndefinedSymbols())
	ok= false;
    }
    return ok;
  }
  
  /**
   * hashtable containing list of parsed modules. Each time a module is parsed, it is registered
   * in this hashtable.
   */
  protected Hashtable modules;
  
  
  /**
   * List of identifier resolvers.
   */
  protected Hashtable id_resolvers;
  
  /**
   * List of index resolvers.
   */
  protected Hashtable ind_resolvers;
  
  /**
   * List of syntax resolvers.
   */
  protected Hashtable ref_resolvers;
  
  /**
   * Version of the implementation
   */
   private static final String sccs_id = "@(#)ModulesHandler.java 4.12 03/08/07 SMI";

  
  

}
