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

import java.util.Iterator;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.ui.collection.AttributeMap;
import org.apache.myfaces.trinidadinternal.ui.collection.FlaggedAttributeMap;
import org.apache.myfaces.trinidadinternal.ui.collection.ContextMap;
import org.apache.myfaces.trinidadinternal.ui.collection.MapContextMap;
import org.apache.myfaces.trinidadinternal.ui.collection.UINodeList;
import org.apache.myfaces.trinidadinternal.ui.collection.ArrayUINodeList;

/**
 * Root implementation for storing a mutable node of a UIX Components UI tree.
 * This class, as a superclass of BaseWebBean, is an ancestor of all
 * the UIX Componein) WebBeans.
 * <p>
 * In addition to simply implementing the methods of MutableUINode,
 * BaseMutableUINode, also specifies how the named and indexed children
 * and attributes are stored, and allows the collections implementing
 * this storage to be set and retrieved.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/BaseMutableUINode.java#0 $) $Date: 10-nov-2005.18:50:10 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BaseMutableUINode extends BaseUINode implements MutableUINode
{
  /**
   * Creates a BaseMutableUINode, binding it to a namespace
   * and local name.
   */
  public BaseMutableUINode(
    String namespaceURI,
    String localName
    )
  {
    super(namespaceURI, localName);
  }



  /**
   * Creates a BaseMutableUINode, binding it to a namespace
   * and local name.
   */
  public BaseMutableUINode(
    String namespaceURI,
    String localName,
    int    initialCapacity
    )
  {
    super(namespaceURI, localName);

    if (initialCapacity != 0)
    {
      _indexedChildren = new ArrayUINodeList(initialCapacity);
    }
  }

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
   */
  public void setID(String newID)
  {
    setAttributeValue(ID_ATTR, newID);
  }

  /**
   * Sets whether the bean is rendered.  When set to false,
   * no output will be delivered for this bean.
   */
  public void setRendered(
    boolean rendered
    )
  {
    setAttributeValue(UIConstants.RENDERED_ATTR,
                      Boolean.valueOf(rendered));
  }


  /**
   * Sets whether the bean is rendered.  When set to false,
   * no output will be delivered for this bean.
   */
  public boolean isRendered()
  {
    return !Boolean.FALSE.equals(getAttributeValue(UIConstants.RENDERED_ATTR));
  }



  /**
   * Adds an indexed child to the node.  Unlike many other APIs,
   * adding a child does not remove it from any other parent nodes.
   * <p>
   * @param childIndex the zero-based index to add the child at
   * @param child the new child node
   */
  public void addIndexedChild(
    int              childIndex,
    UINode           child
    )
  {
    getIndexedNodeList(true).addUINode(childIndex, child);
  }


  /**
   * Convenience method for appending an indexed child.
   */
  public void addIndexedChild(
    UINode  child
    )
  {
    getIndexedNodeList(true).addUINode(child);
  }


  /**
   * Convenience method for appending an array of indexed children.
   */
  public final void addIndexedChildren(
    UINode[] indexedChildren
    )
  {
    NodeUtils.addIndexedChildren(this, indexedChildren);
  }


  /**
   * Convenience method for appending an enumeration of indexed children.
   */
  public final void addIndexedChildren(
    Iterator<UINode> indexedChildren
   )
  {
    NodeUtils.addIndexedChildren(this, indexedChildren);
  }

  /**
   * Adds a text child to the bean.  A convenience function
   * that automatically creates a TextNode.
   * @param text the text - does not need to be escaped
   */
  public void addIndexedChild(
    String text
    )
  {
    addIndexedChild(new TextNode(text));
  }


  /**
   * Removes an indexed child from the node.
   * @param childIndex the zero-based index of the child
   */
  public UINode removeIndexedChild(
    int childIndex
    )
  {
    UINodeList indexedChildren = getIndexedNodeList(false);

    if (indexedChildren == null)
      throw new IndexOutOfBoundsException();

    return indexedChildren.removeUINode(childIndex);
  }


  /**
   * Removes all of the indexed children.
   */
  public void clearIndexedChildren()
  {
    UINodeList indexedChildren = getIndexedNodeList(false);

    if (indexedChildren != null)
    {
      indexedChildren.clearUINodes();
    }
  }


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
    int    childIndex,
    UINode child
    )
  {
    getIndexedNodeList(true).setUINode(childIndex, child);
  }


  /**
   * Sets a named child on the node.  Any node attached with
   * that name will be removed.
   * <p>
   * @param childName the name of the child
   * @param namedChild the child;  passing null will remove any existing
   *      UINode with that name.
   */
  public void setNamedChild(
    String childName,
    UINode namedChild
    )
  {
    boolean doSet = namedChild != null;

    ContextMap childMap = getNamedChildMap(doSet);

    if (childMap != null)
    {
      childMap.set(childName, namedChild);
    }
  }


  /**
   * Returns the attribute value of the node;  unlike the version of
   * this method that takes a RenderingContext, this method must
   * return the actual object originally set on the node - BoundValues
   * will be directly returned.
   * <p>
   * @param name the name of the attribute
   */
  public final Object getAttributeValue(
    AttributeKey attrKey
    )
  {
    return getAttributeValueImpl(null, attrKey, false);
  }


  /**
   * Sets an attribute value of the node.
   * @param name the name of the attribute
   * @param value the new value;  passing null will remove any
   *      existing attribute with that name.
   */
  public void setAttributeValue(
    AttributeKey attrKey,
    Object       value
    )
  {
    AttributeMap attrDict = getAttributeMap((value != null));

    if (attrDict != null)
    {
      attrDict.setAttribute(attrKey, value);
    }
  }


  /**
   * Replaces the AttributeMap used to store the attributes.
   * All currently set attributes will be forgotten, and
   * all future requests to get or set attributes will go
   * to this dictionary.  Neither the ID of this node nor its
   * children are retrieved from this dictionary.
   */
  public void setAttributeMap(
    AttributeMap newAttrMap
    )
  {
    _attributes = newAttrMap;
  }

  /**
   * Replaces the UINodeList used to store the list of indexed children
   * for this UINode.  All current indexed children of this UINode are
   * forgotten and all request to get and set indexed children will go
   * to this UINodeList.
   * <p>
   * @see #setAttributeMap
   * @see #setNamedChildMap
   */
  public void setIndexedNodeList(
    UINodeList nodeList
    )
  {
    _indexedChildren = nodeList;
  }

  /**
   * Replaces the ContextMap used to store the named children for this
   * UINode.  All current named children of this UINode are forgotten and
   * all request to get and set named children will go to this ContextMap.
   */
  public void setNamedChildMap(
    ContextMap childMap
    )
  {
    _childMap = childMap;
  }

  /**
   * Returns the ContextMap used to store named children.
   */
  public final ContextMap getNamedChildMap()
  {
    return getNamedChildMap(true);
  }


  /**
   * Returns the ContextMap used to store named children.
   * <p>
   * @param createIfNull if true,  creates
   *   a ContextMap object if one has not yet been created.
   */
  @Override
  protected final ContextMap getNamedChildMap(
    boolean forMutating
    )
  {
    if (forMutating && (_childMap == null))
    {
      _childMap = createNamedChildMap();
    }

    return _childMap;
  }


  /**
   * Returns the AttributeMap used to store attributes.
   */
  public final AttributeMap getAttributeMap()
  {
    return getAttributeMap(true);
  }

  /**
   * Returns the AttributeMap used to store attributes.
   * <p>
   * @param createIfNull if true,  creates
   *   an AttributeMap object if one has not yet been created.
   */
  @Override
  protected final AttributeMap getAttributeMap(
    boolean createIfNull
    )
  {
    if (createIfNull && (_attributes == null))
    {
      _attributes = createAttributeMap();
    }

    return _attributes;
  }


  /**
   * Returns the UINodeList used to retrieve indexed children from
   * the node.
   * <code>getIndexedChild()</code>,
   * <code>getIndexedChildCount()</code>, and
   * <code>replaceIndexedChild()</code>
   *  will use the result of this
   * method - if not implemented, subclasses must override both
   * of those methods.
   * <p>
   * @see #getIndexedChild
   * @see #replaceIndexedChild
   * @see #getIndexedChildCount
   */
  public final UINodeList getIndexedNodeList()
  {
    return getIndexedNodeList(true);
  }


  /**
   * Returns the UINodeList used to retrieve indexed children from
   * the node.
   * <code>getIndexedChild()</code>,
   * <code>getIndexedChildCount()</code>, and
   * <code>replaceIndexedChild()</code>
   * will use the result of this
   * method - if not implemented, subclasses must override all
   * of those methods.
   * <p>
   * @see #getIndexedChild
   * @see #replaceIndexedChild
   * @see #getIndexedChildCount
   */
  @Override
  protected final UINodeList getIndexedNodeList(
    boolean createIfNull
    )
  {
    if (createIfNull && (_indexedChildren == null))
    {
      _indexedChildren = createIndexedNodeList();
    }

    return _indexedChildren;
  }

  /**
   * Creates the UINodeList that will be used for indexed
   * children of this node.
   */
  protected UINodeList createIndexedNodeList()
  {
    return new ArrayUINodeList();
  }


  /**
   * Creates the ContextMap that will be used for named
   * children of this node.
   */
  protected ContextMap createNamedChildMap()
  {
    return new MapContextMap(new ArrayMap<String, Object>());
  }

  /**
   * Creates the AttributeMap that will be used for
   * attributes of this node.
   */
  protected AttributeMap createAttributeMap()
  {
    return new FlaggedAttributeMap();
  }

  private ContextMap    _childMap;
  private AttributeMap  _attributes;
  private UINodeList    _indexedChildren;
}
