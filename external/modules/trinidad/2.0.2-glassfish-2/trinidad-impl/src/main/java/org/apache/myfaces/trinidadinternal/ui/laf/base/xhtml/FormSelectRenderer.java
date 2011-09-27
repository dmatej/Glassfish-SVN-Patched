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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FormSelectRenderer.java#0 $) $Date: 10-nov-2005.18:53:52 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class FormSelectRenderer extends OptionContainerRenderer
{
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    // render the disabled attribute
    renderAttribute(context, "disabled", getDisabled(context, node));

    // render the name attribute
    renderAttribute(context, "name", getTransformedName(context, node));
  }

  /**
   * Renders event handlers for the node.
   */
  @Override
  protected void renderEventHandlers(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderEventHandlers(context, node);

    // render the onblur event handler
    renderAttribute(context, "onblur",  getOnBlur(context, node));

    // render the onfocus event handler
    renderAttribute(context, "onfocus", getOnFocus(context, node));
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return SELECT_ELEMENT;
  }

  @Override
  protected final Renderer getOptionRenderer(UIXRenderingContext context)
  {
    return context.getRendererManager().getRenderer(MARLIN_NAMESPACE, 
                                                    SELECT_OPTION_NAME);
  }
}
