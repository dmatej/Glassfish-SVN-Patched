/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.ui;

/**
 * MutableUINode extends UINode to add mutability.  Most WebBeans will
 * inherit from this class, but Renderers only see that immutable
 * UINode interface.
 * <p>
 * The indexed children manipulation methods--<code>addIndexedChild</code>,
 * <code>removeIndexedChild</code>, and <code>clearIndexedChildren</code>.
 * follow the same semantics as the indexed methods from the Java Collections
 * List and Collection classes respectively.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/MutableUINode.java#0 $) $Date: 10-nov-2005.18:50:14 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface MutableUINode extends UINode
{
  /**
   * Inserts an indexed child to the node.  Shifts the element currently at
   * that position, if any, and any subsequent elements to the right
   * (adds one to their indices).  Children may be added to the end of the
   * list of indexed children with the second version of addIndexedChild().
   * <p>
   * Unlike many other APIs, adding a child does not remove it from any other
   * parent nodes.  In fact, the same child UINode instance is allowed to
   * appear in multiple different indices of the same parent UINode.
   * <p>
   * @param childIndex the zero-based index to add the child at.
   * @param child the new child node
   * <p>
   * @see #removeIndexedChild
   * @see #clearIndexedChildren
   * @see java.util.List#add
   */
  public void addIndexedChild(
    int              childIndex,
    UINode           child);

  /**
   * Inserts an indexed child to the node, placing after all other nodes.
   * <p>
   * Unlike many other APIs, adding a child does not remove it from any other
   * parent nodes.  In fact, the same child UINode instance is allowed to
   * appear in multiple different indices of the same parent UINode.
   * <p>
   * @param child the new child node
   * <p>
   * @see #removeIndexedChild
   * @see #clearIndexedChildren
   * @see java.util.List#add
   */
  public void addIndexedChild(UINode child);


  /**
   * Removes an indexed child from the node.
   * @param childIndex the zero-based index of the child to remove
   * <p>
   * @see #clearIndexedChildren
   * @see java.util.List#remove
   */
  public UINode removeIndexedChild(int childIndex);
 
  /**
   * Removes all of the indexed children.
   * <p>
   * Although this method could be implemented in terms of
   * <CODE>removeIndexedChild</CODE>, it is present on this interface in
   * order to allow for more efficient implementations.
   * <p>
   * @see #removeIndexedChild
   * @see java.util.Collection#clear
   */
  public void clearIndexedChildren();
  

  /**
   * Replaces a single child.
   * <p>
   * Although this method could be implemented in terms of
   * <CODE>addIndexedChild</CODE> and <CODE>removeIndexedChild</CODE>,
    * it is present on this interface in
   * order to allow for more efficient implementations.
   * <p>
   * @param childIndex the zero-based index to add the child at.
   * @param child the new child node
   * <p>
   * @see #removeIndexedChild
   */
  public void replaceIndexedChild(
    int              childIndex,
    UINode           child);
   
  /**
   * Sets a named child on the node.  Any node attached with
   * that name will be removed.
   * <p>
   * @param childName the name of the child
   * @param namedChild the child;  passing null will remove any existing
   *      UINode with that name.
   */
  public void setNamedChild(
    String           childName,
    UINode           namedChild);

  /**
   * Sets an attribute value of the node.
   * @param name the name of the attribute
   * @param value the new value;  passing null will remove any
   *      existing attribute with that name.
   */
  public void setAttributeValue(
    AttributeKey     attrKey,
    Object           value);
      

  /**
   * Sets the page-wide unique client ID of this node.  The string set
   * must comply with the
   * <a href="http://www.w3.org/TR/2000/WD-xml-2e-20000814#NT-TokenizedType">
   * XML id specification</a>--namely it must begin
   * with a [a-z][A-z] and after that can contain as many of
   * [a-z][A-Z][0-9][._-:] as desired.
   * <p>
   * This property is typically only needed when writing client-side
   * JavaScript.
   * <p>
   * <strong>
   * This method is only present on this interface for backwards compatibility
   * and will be removed from this interface in a future version of UIX Components 
   * and moved to <code>org.apache.myfaces.trinidadinternal.ui.beans.BaseWebBean</code>
   * </strong>
   */
  public void setID(String id);
}
