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

import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.PreAndPostRenderer;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ScriptRenderer.java#0 $) $Date: 10-nov-2005.18:54:11 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ScriptRenderer extends BaseLafRenderer
  implements UIConstants, RoledRenderer, PreAndPostRenderer

{
  public NodeRole getNodeRole(
    UIXRenderingContext context,
    UINode           node)
  {
    return USER_INVISIBLE_ROLE;
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (_hasText(context, node)) ? "script" : null;
  }

  @Override
  public void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object source = node.getAttributeValue(context, SOURCE_ATTR);
    if (source != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", null);
      // =-=AEW We should be rendering an ID on both scripts if
      // there's both "source" and text
      if (!_hasText(context, node))
      {
        renderID(context, node);
      }

      writer.writeAttribute("language", "javascript", null);
      renderEncodedResourceURI(context, "src", source);
      _renderDeferAttribute(context, node);
      // Bug #3426092:
      // render the type="text/javascript" attribute in accessibility mode
      XhtmlLafRenderer.renderScriptTypeAttribute(context);       
      writer.endElement("script");
    }

    super.prerender(context, node);

    Object text = node.getAttributeValue(context, TEXT_ATTR);
    
    if (text != null)
      context.getResponseWriter().writeText(text, null);
  }

  @Override
  public void postrender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    super.postrender(context, node);
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // =-=JMW renderID as an attribute for Visual Editor
    renderID(context, node);
    renderAttribute(context, "language", "javascript");
    _renderDeferAttribute(context, node);
  }



  private boolean _hasText(
    UIXRenderingContext context,
    UINode           node)
  {
    // If there's no "source" attribute, then let's guess that
    // there's probably some text content we need to wrap.
    // Of course, if there's "text", then we definitely need
    // to output a <script> tag.
    return ((node.getAttributeValue(context, SOURCE_ATTR) == null) ||
            (node.getAttributeValue(context, TEXT_ATTR) != null));
  }

  /**
  * If the script doesn't generate content, as indicated by the 
  * GENERATES_CONTENT_ATTR, then try to render the defer attribute.
  * generating content means the script calls document.write/writeln.
  */
  private void _renderDeferAttribute(
    UIXRenderingContext context,
    UINode           node
    )  throws IOException
  {
    Object generatesContent = node.getAttributeValue(context, 
                                                     GENERATES_CONTENT_ATTR);
    if (!(Boolean.TRUE.equals(generatesContent)))
    {
      XhtmlLafRenderer.renderScriptDeferAttribute(context);
    }
  }
}
