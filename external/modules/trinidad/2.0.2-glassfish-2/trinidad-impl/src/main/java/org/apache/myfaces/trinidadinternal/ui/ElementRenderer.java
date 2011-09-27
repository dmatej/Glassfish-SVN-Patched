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

import javax.faces.component.UIComponent;

/**
 * Renderer implementation that can output elements.
 * This base implementation outputs no attributes;  clients
 * must override <code>renderAttributes()</code>.  It will
 * output the start and element tags, based on the return
 * value of <code>getElementName()</code>.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/ElementRenderer.java#0 $) $Date: 10-nov-2005.18:50:13 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ElementRenderer extends BaseRenderer
{
  /**
   * Returns a shared instance of this renderer.
   */
  static public Renderer getRenderer()
  {
    return _sInstance;
  }


  /**
   * Creates an ElementRenderer
   */
  public ElementRenderer()
  {
  }

  /**
   * Called to render the portion before the contents.  This
   * implementation will output a start element tag and call
   * <code>renderAttributes()</code>.  Uses the element
   * name returned by <code>getElementName()</code>.
   * @see #renderAttributes
   * @see #getElementName
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    String elementName = getElementName(context, node);
    if (elementName != null)
    {
      UIComponent component = NodeUtils.getUIComponent(context, node);
      context.getFacesContext().getResponseWriter().startElement(elementName,
                                                                 component);
      renderAttributes(context, node);
    }
  }


  /**
   * Called to render the portion after the contents.  This
   * implementation outputs a close element tag.  Uses the
   * element name returned by <code>getElementName()</code>.
   * @see #getElementName
   * @param context the rendering context
   * @param node the current UINode
   */
  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    String elementName = getElementName(context, node);
    if (elementName != null)
      context.getResponseWriter().endElement(elementName);
  }

  
  /**
   * Renders attributes of the current node.  The default
   * implementation does not render any attributes;  subclasses
   * should override this method if they need to output attributes.
   * @param context the rendering context
   * @param node the current UINode
   */
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
  }


  /**
   * Renders a single attribute.  Default implementation
   * does nothing if the value is null;  otherwise, uses
   * the current output method to render an attribute.
   * @param context the rendering context
   * @param name the attribute name for rendering
   * @param value the attribute value
   */
  protected void renderAttribute(
    UIXRenderingContext context,
    String           name,
    Object           value
    ) throws IOException        
  {
    if (value != null)
    {
      context.getResponseWriter().writeAttribute(name, value, null);
    }
  }


  /**
   * Renders a single attribute, getting the value from
   * the UINode.
   * @param context the rendering context
   * @param node the node to get the attribute from
   * @param name the attribute name for rendering
   * @param valueKey the key to use to retrieve the attribute
   */
  protected final void renderAttribute(
    UIXRenderingContext context,
    UINode           node,
    String           name,
    AttributeKey     valueKey
    ) throws IOException        
  {
    renderAttribute(context,
                    name,
                    node.getAttributeValue(context, valueKey));
  }


  /**
   * Renders a single attribute, getting the value from
   * the UINode, and substituting a default value if 
   * the node doesn't have that attribute set.
   * @param context the rendering context
   * @param node the node to get the attribute from
   * @param name the attribute name for rendering
   * @param valueKey the key to use to retrieve the attribute
   * @param defaultValue the default value to use if the attribute
   *                     isn't set
   */
  protected final void renderAttribute(
    UIXRenderingContext context,
    UINode           node,
    String           name,
    AttributeKey      valueKey,
    Object           defaultValue
    ) throws IOException        
  {
    Object value = node.getAttributeValue(context, valueKey);
    
    if (value == null)
    {
      value = defaultValue;
    }
 
    renderAttribute(context, name, value);
  }



  /**
   * Renders a single attribute, getting the value from
   * the UINode, and substituting a default value if 
   * the node doesn't have that attribute set.  This
   * convenience method will use the same name for rendering
   * the attribute as for retrieving it from the UINode.
   * @param context the rendering context
   * @param node the node to get the attribute from
   * @param key the attribute key for rendering and for retrieving
   *             the value
   * @param defaultValue the default value to use if the attribute
   *                     isn't set
   */
  protected final void renderAttribute(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey     key,
    Object           defaultValue
    ) throws IOException
  {
    renderAttribute(context,
                    node,
                    key.getAttributeName(),
                    key,
                    defaultValue);
  }


  /**
   * Called to retrieve the element name to render.  Default
   * implementation returns null.
   * @param context the rendering context
   * @param node the current node
   */
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return null;
  }

  static private final Renderer _sInstance = new ElementRenderer();
}
