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
import java.util.Collections;

import javax.faces.component.UIComponent;

/**
 * Abstract Node implementation for text.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/AbstractTextNode.java#0 $) $Date: 10-nov-2005.18:50:09 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class AbstractTextNode implements UINode, UIConstants
{
  protected AbstractTextNode()
  {
  }

  public UIComponent getUIComponent()
  {
    return null;
  }



  /**
   * Convenience method returning any text contained in this node as a
   * <CODE>char[]</CODE>, regardless of how the text is actually stored in
   * the node.
   */
  public final char[] getText(
    UIXRenderingContext context
    )
  {
    Object text = getTextObject(context);

    if (text == null)
      return null;

    if (text instanceof char[])
    {
      return (char[])text;
    }
    else
    {
      return text.toString().toCharArray();
    }
  }


  /**
   * Returns the UIX Components namespace.
   */
  public String getNamespaceURI()
  {
    return UIConstants.MARLIN_NAMESPACE;
  }


  /**
   * Returns "text".
   */
  public String getLocalName()
  {
    return UIConstants.TEXT_NAME;
  }


  /**
   * Returns the number of indexed children; since TextNodes
   * contain no children, always returns 0
   */
  public int getIndexedChildCount(
    UIXRenderingContext context
    )
  {
    return 0;
  }

  /**
   * Returns the indexed child at this index;  since TextNodes
   * contain no children, always throws an exception.
   */
  public UINode getIndexedChild(
    UIXRenderingContext context,
    int              index
    )
  {
    throw new IndexOutOfBoundsException();
  }


  /**
   * Returns 1.
   */
  public int getAttributeCount(
    UIXRenderingContext context
    )
  {
    return 1;
  }


  /**
   * Returns an Iterator of the names that attribute values have been
   * added under;  always returns an enumeration containing only
   * TEXT_ATTR.
   */
  public Iterator<AttributeKey> getAttributeNames(
    UIXRenderingContext context
    )
  {
    return Collections.singletonList(UIConstants.TEXT_ATTR).iterator();
  }


  /**
   * Returns null.
   */
  public UINode getNamedChild(
    UIXRenderingContext context,
    String           childName
    )
  {
    return null;
  }

  /**
   * Returns null.
   */
  public Iterator<String> getChildNames(
    UIXRenderingContext context
    )
  {
    return null;
  }


  /**
   * Returns the value of an attribute, given its name.  The only
   * valid attribute name for TextNode is <CODE>UIConstants.TEXT_ATTR</CODE>.
   */
  public Object getAttributeValue(
    UIXRenderingContext context,
    AttributeKey     attrKey
    )
  {
    if (UIConstants.TEXT_ATTR == attrKey)
    {
      return getTextObject(context);
    }
    else
    {
      return null;
    }
  }


  public Object getRawAttributeValue(
    UIXRenderingContext context,
    AttributeKey     attrKey
    )
  {
    return getAttributeValue(context, attrKey);
  }


  /**
   * Renders this UINode.  Clients can implements this
   * method in any way desired.  All UIX Components based implementations
   * will get a RendererManager from the RenderingContext,
   * get a Renderer, and defer rendering to that Renderer.
   */
  public final void render(
    UIXRenderingContext context
    )
    throws IOException
  {
    render(context, this);
  }


  public void render(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    Renderer renderer =
      context.getRendererManager().getRenderer(getNamespaceURI(),
                                               getLocalName());
    if (renderer != null)
      renderer.render(context, node);
  }

  public NodeRole getNodeRole(UIXRenderingContext context)
  {
    // =-=AEW ???
    return STRUCTURAL_ROLE;
  }


  /**
   * Returns the text for the current rendering context.
   */
  protected abstract Object getTextObject(UIXRenderingContext context);
}
