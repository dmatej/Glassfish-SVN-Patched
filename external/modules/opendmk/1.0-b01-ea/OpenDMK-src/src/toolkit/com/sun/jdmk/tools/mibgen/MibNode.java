/*
 * @(#)file      MibNode.java
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
 * A MIB node.
 */
public class MibNode {
  
  public MibNode(String nodeId) {
    this.nodeId= nodeId;
    computedOid= nodeId;
    children= new Hashtable();
  }
  
  public MibNode getId(String id) {
    
    if (children.containsKey(id)) {
      // Get the child ...
      //
      return (MibNode) children.get(id);
    }
      
    // Create the node
    //
    MibNode child= new MibNode(id);
    
    // Compute the child oid ...
    //
    if (nodeId.length() != 0) {
      child.setComputedOid(computedOid + "." + id);
    }
    
    // Register the child
    //
    children.put(id, child);
    return child;
  }
  
  public String getNodeId() {
    return nodeId;
  }
  
  public String getOid() {
    return oid;
  }
  
  public Hashtable getChildren() {
    return children;
  }
  
  public MibNode getChildWithName(String name) {
    if (children == null)
      return null;
    if (children.isEmpty())
      return null;
    
    // Go through the list
    //
    for(Enumeration e= children.elements(); e.hasMoreElements();) {
      MibNode child= (MibNode) e.nextElement();
      String sym= child.getRealSymbolName();
      if (name.equals(sym))
	return child;
    }
    return null;
  }
  
  public MibNode findNodeWithName(String name) {
     
    if (symbolName != null) {
      if (name.equals(symbolName))
	return this;
    }
    if (children == null)
      return null;
    if (children.isEmpty())
      return null;
    
    // Go through the list
    //
    MibNode result;
    for(Enumeration e= children.elements(); e.hasMoreElements();) {
      MibNode child= (MibNode) e.nextElement();
      result= child.findNodeWithName(name);
      if (result != null)
	return result;
    }
      
    return null;
  }
  
  public String getSymbolName() {
    if (symbolName == null)
      return null;
    String result= symbolName.trim();
    result= result.replace('-', '_');
    result= result.replace('.', '_');
    result= result.replace(',', '_');
    result= result.replace(',', '_');
    result= result.replace(' ', '_');
    return Character.toUpperCase(result.charAt(0)) + result.substring(1, result.length());
    //return result;
  }
  
  public String getRealSymbolName() {
    return symbolName;
  }

  public String getRealOidName() {
    return oidName;
  }
  
  public boolean isAssociated() {
    return (symbolName == null ? false: true);
  }
   
  public void setComputedOid(String oid) {
    computedOid= oid;
  }
  
  public String getComputedOid() {
    return computedOid;
  }
  
  public void setOidName(String name) {
    oidName= name;
  }
  
  public String getOidName() {
    return Character.toUpperCase(oidName.charAt(0)) + oidName.substring(1, oidName.length());
    //return oidName;
  }
  
  public boolean isEnumeratedType() {
    return enumerated;
  }
  
  public void setEnumerated(boolean x) {
    enumerated= x;
  }
  
  public void setEnumeratedType(String x) {
    enumeratedType= x;
    enumerated= true;
  }
  
  public String getEnumeratedType() {
    return enumeratedType;
  }
  
  public void setSnmpSyntax(String syntax) {
    snmpSyntax= syntax;
  }
  
  public String getSnmpSyntax() {
    return snmpSyntax;
  }
  
  
  public ASTObjectTypeDefinition getObjectType() {
    return objectType;
  }
  
  public void setAssociation(String oid, String symbolName, ASTObjectTypeDefinition objectType) {
    this.oid= oid;
    this.symbolName= symbolName;
    this.objectType= objectType;
  }
  
  /**
   * A group is a node which is not associated to an object type 
   * definition but for which one of the children has an object type ...
   *
   */
    public boolean isGroup()  {
	if (objectType != null) {
	    return false;
	}
	boolean result= false;
	// MibNode aGroup= null;
	for (Enumeration e= children.elements(); e.hasMoreElements();) {
	    MibNode n = (MibNode)e.nextElement();
	    if (n != null) {
		if (n.isAssociated() == true)
		    result= true;
		// if (n.isGroup() == true)
		// aGroup= n;
	    }
	}    
	//if ((result == true) && (aGroup != null)) {
	//  return false;
	//}
	//if (result == true) {
	//    java.lang.System.out.println(getComputedOid()+": is a group");
	//}
	return result;
    }

    public boolean oldIsValidGroup() throws IOException {
	if (objectType != null) {
	    return false;
	}
	boolean result= false;
	MibNode aGroup= null;
	for (Enumeration e= children.elements(); e.hasMoreElements();) {
	    MibNode n = (MibNode)e.nextElement();
	    if (n != null) {
		if (n.isAssociated() == true)
		    result= true;
		if (n.isGroup() == true)
		    aGroup= n;
	    }
	}    
	if ((result == true) && (aGroup != null)) {
	    throw new IOException(aGroup.getComputedOid());
	}
	return result;
    }

    // Check group validity - obsolete
    //
    public boolean isValidGroup() throws IOException {
	if (objectType != null) {
	    return false;
	}
	boolean result= false;
	for (Enumeration e= children.elements(); e.hasMoreElements();) {
	    MibNode n = (MibNode)e.nextElement();
	    if (n != null) {
		if (n.isAssociated()) {
		    result = true; continue;
		}
		if (n.isGroup() || n.hasNestedGroups()) {
		    // result = true;
		    continue ;
		}
		// throw new IOException(aGroup.getComputedOid());
	    }
	}
	return result;
    }
    
    // Return true if there are some groups below this node...
    //
    public boolean hasNestedGroups() {
	if (objectType != null) {
	    return false;
	}
	boolean result= false;
	for (Enumeration e= children.elements(); e.hasMoreElements();) {
	    MibNode n = (MibNode)e.nextElement();
	    if (n != null) {
		if (n.isAssociated() || n.isGroup()) {
		    result = true; continue;
		}
		if (n.hasNestedGroups()) 
		    result = true;
	    }
	}
	return result;
    }
    
    // Return true if this node is a group that doesn't have
    // any nested child groups.
    //
    public boolean isSimpleGroup() {
	return isGroup() && ! hasNestedGroups();
    }
    
    // Returns true if this node is a group which has nested
    // child groups.
    //
    public boolean isComplexGroup() {
	return isGroup() && hasNestedGroups();
    }
    
    // Return true if this node can be a table (if the father node
    // is a group, and this method returns true, then we have a table).
    // 
    // A table is a node that is directly below a group, and that
    // doesn't have any nested groups, but has children and an associated
    // OBJECT-TYPE definition.
    public boolean isTable() {
	if (isGroup())          return false;
	if (hasNestedGroups())  return false;
	if (children.isEmpty()) return false;
	if (!isAssociated())    return false;
	if (isEntry())          return false;
	if (objectType == null) return false;
	ASTNamedType syntax = objectType.getSyntax();
	if (syntax == null)     return false;
	if (syntax.isSequenceOfType()) return true;
	return false;
    }

    // Return true if this node can be an Entry
    //
    // Simplest way is to see whether we have an IndexParts in the
    // ObjectTypeDefinition...
    //
    public boolean isEntry() {
	if (!isAssociated())    return false;
	if (objectType == null) return false;
	ObjectTypeDefinition def = objectType.getDefinition();
	if (def == null) return false;
	if (def.getIndex() == null) return false;
	return true;
    }

    // Return true if this node can be a scalar/columnar object
    //
    // A columnar/scalar object is a node that is associated
    // and is neither a Table, nor an Entry
    //
    public boolean isVariable() {
	if (!isAssociated()) return false;
	if (isTable())        return false;
	else if (isEntry())   return false;
	return true;
    }

   public void dump(String prefix) {
    System.out.println(toString(prefix));
      for (Enumeration e= children.elements(); e.hasMoreElements();) {
	MibNode n = (MibNode)e.nextElement();
	if (n != null) {
	  n.dump(prefix + " ");
	}
      }
  }
  
  public String toString(String prefix) {  
    return prefix + toString();
  }
  
  public String toString() {
    if (isAssociated()) {
      return "Name= " + symbolName + " OID= " + oid + " GROUP= " + String.valueOf(isGroup());
    } 
    return "OID= " + computedOid + " Name= " + oidName + " GROUP= " + String.valueOf(isGroup());
  }
  
  public void addExternalIndex(MibNode aNode) {
    if (externalIndex.contains(aNode) == false)
      externalIndex.addElement(aNode);
  }
  
  public Vector getExternalIndex() {
    return externalIndex;
  }
  
  public void setFixedLength(long fixed) {
    length= fixed;
  }
  
  public long getFixedLength() {
    return length;
  }
  
  // PRIVATE VARIABLES
  //------------------
  private Hashtable children;
  
  private String symbolName= null;

  private String oid= null;
  
  private ASTObjectTypeDefinition objectType= null;
  
  private String nodeId= "";
  
  private String computedOid= "";
  
  private String oidName= "";
  
  private boolean enumerated= false;
  
  private String enumeratedType= "";
  
  private String snmpSyntax= "";
  
  private long length = -1;
 
  
  /**
   * The variable is used only when the node represents  a table.
   * It contains the list of indexes which are not defined in the table entry.
   */
  private Vector externalIndex= new Vector();
}
