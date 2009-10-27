/*
 * @(#)file      MibTree.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.8
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

/**
 * The class corresponds to a tree representation of a SNMP MIB.
 *
 */
public class MibTree  {
  
  /**
   * Build a tree using the list of declared objects (v1 and v2).
   */
  public MibTree(String moduleName) {
    this.moduleName= moduleName;
    root= new MibNode("");
   
  }
  
  public boolean buildTree(Hashtable v1, Hashtable v2) {
    return handleObjectList(v1) && handleObjectList(v2);
  }
  
  public void dump(String prefix) {
     Trace.info("MIB MODULE= " + moduleName);
     root.dump(prefix);
  }
  
  public MibNode getRoot() {
    return root;
  }
  
  // PRIVATE METHODS
  //----------------
  private boolean  handleObjectList(Hashtable table) {
    boolean result= true;
    for(Enumeration e= table.keys(); e.hasMoreElements();) {
      String key= (String) e.nextElement();
      ASTObjectTypeDefinition obj= (ASTObjectTypeDefinition) table.get(key);
      
      // process the object type definition
      //
      if (register(key, obj) == false)
	result= false;
    }
    
    return result;
  }
  
  private boolean register(String key, ASTObjectTypeDefinition objectType) {
    
    // Get the object definition
    //
    ObjectTypeDefinition definition= objectType.getDefinition();
      
    // Access the oid
    //
    ASTOidValue oid= definition.getOidNode();
    
    // Assume that the oid has been resolved ...
    //
    StringBuffer dotNotation= oid.getDotNotation();
    
    // Add the node
    //
    return addNode(dotNotation.toString(), key, objectType);
  }
  
  private boolean addNode(String oid, String key, ASTObjectTypeDefinition objectType) {
    StringTokenizer st= new StringTokenizer(oid, ".", false);
    MibNode curr= root;
    while(st.hasMoreTokens()) { 
      String id= st.nextToken(); 
      curr= curr.getId(id);
    }
    
    // Verify that the node is not already associated to an object type definition
    //
    if (curr.isAssociated()) {
      Object[] params= new Object[4];
      params[0]= (Object)curr.getSymbolName();
      params[1]= key;
      params[2]= oid;
      params[3]= moduleName;
      Trace.error(MessageHandler.getMessage("compile.error.duplicate.oid", params));
      return false;
    }
    
    // So far so good. Just register the definition...
    //
    curr.setAssociation(oid, key, objectType);
    return true;
  }
    
  // VARIABLES
  //----------
  
  /**
   * Tree representation of the mib
   */
  private MibNode root;
  
  /**
   * Module name
   */
  private String moduleName;
  
  
}
