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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidad.skin.Icon;


/**
 * Renderer for the iconKey component.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/IconKeyRenderer.java#0 $) $Date: 10-nov-2005.18:53:57 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class IconKeyRenderer extends XhtmlLafRenderer
{
  @Override
  protected void renderContent(UIXRenderingContext context, UINode node)
    throws IOException
  {
    Object name = node.getAttributeValue(context, NAME_ATTR);
    if (ICON_REQUIRED.equals(name))
    {

      // Get the required Icon from the context
      Icon icon = context.getIcon(XhtmlLafConstants.REQUIRED_ICON_ALIAS_NAME);
      String title = getTranslatedString(context, "REQUIRED_TIP");
      XhtmlLafUtils.renderIcon(context, icon, title, null);
      
      ResponseWriter writer = context.getResponseWriter();

      writer.writeText(XhtmlLafConstants.NBSP_STRING, null);
      writer.writeText(getTranslatedValue(context, _REQUIRED_KEY), null);
    }
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return "span";
  }

  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node)
  {
    return XhtmlLafConstants.TIP_TEXT_STYLE_CLASS;
  }
  
  // translation keys
  private static final String _REQUIRED_KEY = 
    "af_objectLegend.REQUIRED_KEY";  
}
