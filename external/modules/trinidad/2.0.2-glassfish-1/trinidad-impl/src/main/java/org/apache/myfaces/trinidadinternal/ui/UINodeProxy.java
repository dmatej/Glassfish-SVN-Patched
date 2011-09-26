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

import java.io.IOException;

import java.util.Iterator;

import javax.faces.component.UIComponent;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/UINodeProxy.java#0 $) $Date: 10-nov-2005.18:50:25 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class UINodeProxy implements UINode
{
  protected abstract UINode getUINode();

  public UIComponent getUIComponent()
  {
    return getUINode().getUIComponent();
  }


  /**
   * The name space URI that segregates the local name of this UINode so
   * that nodes with the same local name will not clash.  Together, the
   * namespace URI and the local identify the kind of UINode that this is.
   * <p>
   * Any namespace String returned by this method should be interned for
   * maximum performance.  This allows namespace comparisons to be
   * performed using object identity, versus the slower object equality.
   * If the String returned is a String constant, the Java VM will have done
   * this for you automatically.
   * <p>
   * As with all namespace URI's, this name is only used as an identifier.  No
   * other sematics are implied.
   * <p>
   * The <CODE>RendererManager</CODE> allows <CODE>RendererFactory</CODE>s to
   * be registered by namespace.
   * <p>
   * <i>TO DO: put in link to XML namespaces</i>
   * @see #getLocalName
   * @see org.apache.myfaces.trinidadinternal.ui.RendererManager
   * @see org.apache.myfaces.trinidadinternal.ui.RendererFactory
   */
  public String getNamespaceURI()
  {
    return getUINode().getNamespaceURI();
  }

  /**
   * Name used to distinguish the name of a UINode within a namespace.
   * <p>
   * Together with the UINode's namespace URI, the local name is typically
   * used to determine which Renderer to use to render the UINode.
   */
  public String getLocalName()
  {
    return getUINode().getLocalName();
  }

  /**
   * Returns the number of indexed children in this UINode.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getIndexedChild
   */
  public int getIndexedChildCount(
    UIXRenderingContext context
    )
  {
    context = getRenderingContext(context);
    return getUINode().getIndexedChildCount(context);
  }

  /**
   * Returns the indexed child at the specified index.  Indexed children are
   * used to represent homogenously treated children of UINodes.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getIndexedChildCount
   */
  public UINode getIndexedChild(
    UIXRenderingContext context,
    int              childIndex
    )
  {
    context = getRenderingContext(context);
    return getUINode().getIndexedChild(context, childIndex);
  }

  /**
   * Returns the child identified by <b>childName</b>.  Named children are
   * used to represent heterogenously treated children of UINodes.
   * <p>
   * Constants for named children used by UIX Components UINodes may be found in
   * the UIConstants interface.  The constants for the named children follow
   * the pattern <CODE>&lt;xxx&gt;_CHILD</CODE>.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getChildNames
   * @see org.apache.myfaces.trinidadinternal.ui.UIConstants
   */
  public UINode getNamedChild(
    UIXRenderingContext context,
    String           childName
    )
  {
    context = getRenderingContext(context);
    return getUINode().getNamedChild(context, childName);
  }


  /**
   * Returns an Iterator of the names that named children have been
   * added under.  The actual UINodes for these named children may be retrieved
   * by passing the Strings returned by this Iterator to
   * <CODE>getNamedChild</CODE>.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getNamedChild
   */
  public Iterator<String> getChildNames(
    UIXRenderingContext context
    )
  {
    context = getRenderingContext(context);
    return getUINode().getChildNames(context);
  }

  /**
   * Returns an Iterator of the names that attribute values have been
   * added under.  The actual values for these attributes may be retrieved
   * by passing the Strings returned by this Iterator to
   * <CODE>getAttributeValue</CODE>.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getAttributeValue
   */
  public Iterator<AttributeKey> getAttributeNames(
    UIXRenderingContext context
    )
  {
    context = getRenderingContext(context);
    return getUINode().getAttributeNames(context);
  }


  /**
   * Returns the value of the attribute with the specified name in the
   * RenderingContext.  If no attribute with the specified name exists
   * in this UINode, or a checked Exception occurs in retrieving the value of
   * the attribute, <CODE>null</CODE> will be returned.
   * <p>
   * Note that as with indexed children and named children, the presence of
   * of an attribute is no guarantee that the Renderer used to render this
   * UINode will actually use the attribute.  The presence of attributes
   * should only be considered as hints to the Renderer.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getAttributeNames
   */
  public Object getAttributeValue(
    UIXRenderingContext context,
    AttributeKey     attrKey
    )
  {
    context = getRenderingContext(context);
    return getUINode().getAttributeValue(context, attrKey);
  }


  public Object getRawAttributeValue(
    UIXRenderingContext context,
    AttributeKey     attrKey
    )
  {
    context = getRenderingContext(context);
    return getUINode().getRawAttributeValue(context, attrKey);
  }

  public UIXRenderingContext getRenderingContext(
    UIXRenderingContext context
    )
  {
    return context;
  }



  /**
   * Returns the role that this node occupies.
   */
  public NodeRole getNodeRole(UIXRenderingContext context)
  {
    context = getRenderingContext(context);
    return getUINode().getNodeRole(context);
  }

  /**
   * Renders this UINode.  Clients can implements this
   * method in any way desired.  All UIX Components-based implementations
   * will get a RendererManager from the RenderingContext,
   * get a Renderer, and defer rendering to that Renderer.
   */
  public final void render(
    UIXRenderingContext context
    )
    throws IOException
  {
    context = getRenderingContext(context);
    render(context, this);
  }

  public void render(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    context = getRenderingContext(context);
    getUINode().render(context, node);
  }

  @Override
  public String toString()
  {
    String debugString = getClass().getName();
    debugString += "[";
    debugString += getUINode().toString();
    debugString += "]";

    return debugString;
  }
}
