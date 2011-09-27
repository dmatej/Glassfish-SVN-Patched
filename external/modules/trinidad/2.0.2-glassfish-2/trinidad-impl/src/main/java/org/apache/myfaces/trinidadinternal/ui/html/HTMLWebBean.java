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
package org.apache.myfaces.trinidadinternal.ui.html;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * HTMLWebBean simplifies adding raw HTML to output.  Attributes
 * will be passed through directly to the output and close
 * element tags will be written only if necessary.
 * <p>
 * While the RawTextBean can be used for the same purposes, clients
 * using this class use the ResponseWriter API, so HTMLWebBean users
 * automatically get features like:
 * <ul>
 *   <li>Automatic escaping
 *   <li>Pretty printing
 *   <li>XHTML syntax support
 *   <li>Debugging assistance
 * </ul>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/html/HTMLWebBean.java#0 $) $Date: 10-nov-2005.18:56:26 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class HTMLWebBean extends BaseMutableUINode
{
  /**
   * Creates an HTMLWebBean.
   * @param localName the HTML element name
   */
  public HTMLWebBean(
    String localName
    )
  {
    super(HTMLRendererFactory.HTML_NAMESPACE, localName.toLowerCase());
  }

  /**
   * Sets an HTML attribute value.
   * @param attrName the HTML attribute name
   * @param value the new value of that attribute
   */
  public void setHTMLAttributeValue(
    String attrName,
    Object value)
  {
    setAttributeValue(AttributeKey.getAttributeKey(attrName),
                      value);
  }

  /**
   * Gets an HTML attribute value.
   * @param attrName the HTML attribute name
   * @return the value of that attribute
   */
  public Object getHTMLAttributeValue(
    String attrName,
    Object value)
  {
    return getAttributeValue(AttributeKey.getAttributeKey(attrName));
  }


  /**
   * Returns the renderer to use on the bean.  Hardcoded
   * HTMLElementRenderer.
   */
  @Override
  public Renderer getRenderer(
    UIXRenderingContext context,
    UINode           dataNode
    )
  {
    return HTMLElementRenderer.getRenderer();
  }
}

