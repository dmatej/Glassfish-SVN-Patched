/*
 * @(#)file      SymbolHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.9
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
 * A generic handler of symbols
 *
 */

public abstract class SymbolHandler {

  public SymbolHandler(String mibModule) {
    this.mibName= mibModule;
    resolved= new Hashtable();
    unresolved= new Hashtable();
  }
  
  /**
   * Add a new resolution
   */
  public void addResolution(String symbol, SimpleNode value) {

    SimpleNode val= (SimpleNode) resolved.get(symbol);
    if (val != null) {
       Trace.warning(MessageHandler.getMessage("compile.w.resolve", symbol));
       return;
    }
   
    resolved.put(symbol, value);
    
    //Trace.info("Add resolved symbol: " + symbol);
    
    // Update any existing unresolved symbols (if any)
    //
    Vector list= (Vector) unresolved.get(symbol);
    if (list == null)
      // so cool now and again ...
      //
      return;
    // Update the list of unresolved symbols
    //
    unresolved.remove(symbol);
    for(Enumeration e= list.elements(); e.hasMoreElements();) {
      SimpleNode node= (SimpleNode) e.nextElement();
      node.setResolver(value);
    }
  }
  
  
  /**
   * This method looks up a symbol.
   */
   public SimpleNode lookupIdentifier(String  symbolName) {
     
    return (SimpleNode) resolved.get(symbolName);
   }
  
  /**
   * Print all the unresolved symbols.
   */
  public boolean printUnresolved() {
   
    if (unresolved.isEmpty()) {
      return true;
    }
   
    for(Enumeration e= unresolved.keys(); e.hasMoreElements();) {
      String symbol= (String) e.nextElement();
      Trace.error(MessageHandler.getMessage("compile.error.undef", symbol, mibName));
    }
    return false;
  }
  
  public void resolve(Hashtable resolvers) {
    if (unresolved.isEmpty())
      // Too nice to happen like that ...
      return;
    
    for(Enumeration e= unresolved.keys(); e.hasMoreElements();) {
      String symbolName= (String) e.nextElement();
      for(Enumeration a= resolvers.keys(); a.hasMoreElements();) {
	String key= (String) a.nextElement();
	SymbolHandler external= (SymbolHandler) resolvers.get(key);
	SimpleNode result= external.lookupIdentifier(symbolName);
	if (result != null) {
	  Trace.info(MessageHandler.getMessage("compile.resolve.info", symbolName, mibName, external.getModuleName()));
	  addResolution(symbolName, result);
	  break;
	}
      } 
      
    }
  }
  
  /**
   * Returns the version of this class.
   */
  public static String getClassVersion () {
    return (sccs_id);
  }
  
   public String getModuleName() {
     return mibName;
   }
  
  /**
   * hashtable containing list of unresolved symbols
   */
  protected Hashtable unresolved;
  
  /**
   * hashtable containing a list of resolved symbols
   */
  protected Hashtable resolved;
  
  /**
   * MIB module the resolver is attached to.
   */
  protected String mibName;
  
  /**
   * Version of the implementation
   */
   private static final String sccs_id = "@(#)SymbolHandler.java 4.9 03/08/07 SMI";

}
