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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;



import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;


/**
 * Renderer for form button nodes.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/oracle/desktop/ResetButtonRenderer.java#0 $) $Date: 10-nov-2005.18:51:52 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class ResetButtonRenderer extends ButtonRenderer
{

  /**
   * This renderes the buttons as image links.
   * All needed rendering to render buttons as image links is done in this
   * method.
   * @param context The rendering context
   * @param node the node to be rendered
   * @param response ImageProvider which descibes the button  image to render.
   * @throws IOException
   */
  @Override
  protected void renderImageContent(UIXRenderingContext context,
    UINode node,
    ImageProviderResponse response
    ) throws IOException
  {
     XhtmlLafUtils.addLib(context, "resetForm()");
     super.renderImageContent(context, node, response);
  }

  /**
   * Always have a link so that we are keyboard navigable in IE.  We will
   * return false from our onClick handler so that this link is never followed.
   */
  @Override
  protected Object getDestination(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (!supportsNavigation(context))
      return null;

    return "#";
  }

  /**
   * Show the modal dialog on click.
   */
  @Override
  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // get the name of the form that the user set
    Object formName = node.getAttributeValue(context, FORM_NAME_ATTR);

    // if the user set no form, look for a form above us
    if (formName == null)
      formName = getParentFormName(context);

    if (formName != null)
    {
      String functionCall = getFunctionCall(context,
                                            node,
                                            formName.toString());

      Object onClick = super.getOnClick(context, node);

      // chain together our onclick handler with a user handler
      // make sure user handler is called first, then call our handler:
      return BaseDesktopUtils.getChainedJS(onClick,
                                           functionCall,
                                           true);

    }
    else
    {

        if (_LOG.isWarning())
          _LOG.warning("NO_FORM_FOUND", node);


      return null;
    }
  }


  protected String getFunctionCall(
    UIXRenderingContext context,
    UINode           node,
    String           formName
    )
  {

    StringBuffer buffer = new StringBuffer(21 + formName.length());

    buffer.append("return resetForm('");
    buffer.append(formName);
    buffer.append("');");

    return buffer.toString();
  }

  /**
   * Override of ButtonRenderer.getAltRenderer() which returns the
   * alternate Renderer for reset buttons.
   * @return xhtml ResetButtonRenderer
   */
  @Override
  protected Renderer getAltRenderer()
  {
    return _ALTERNATE_RENDERER;
  }

  // Alternate renderer in screen reader mode
  private static final Renderer _ALTERNATE_RENDERER =
    new org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ResetButtonRenderer();

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ResetButtonRenderer.class);
}
