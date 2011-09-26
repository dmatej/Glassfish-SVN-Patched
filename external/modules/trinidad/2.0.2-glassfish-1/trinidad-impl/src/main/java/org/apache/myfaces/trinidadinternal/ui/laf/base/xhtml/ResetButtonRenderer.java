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



import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;


/**
 * Renderer for form reset buttons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ResetButtonRenderer.java#1 $) $Date: 11-nov-2005.14:59:41 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ResetButtonRenderer extends ButtonRenderer
{
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )throws IOException
  {
    XhtmlLafUtils.addLib(context, "resetForm()");
    super.prerender(context, node);
  }

  /**
   * Returns true if button tags are being used.  Provides a hook for
   * subclassers to override
   */
  @Override
  protected boolean useButtonTags(
    UIXRenderingContext context
    )
  {
    // we always use button tags
    return true;
  }

  /**
   * Override to change the type of the button
   */
  @Override
  protected String getButtonType()
  {
    return "reset";
  }


  /**
   * Show the modal dialog on click.
   * =-= bts copied code from
   *         org.apache.myfaces.trinidadinternal.ui.blaf.RestButtonRenderer
   */
  @Override
  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if ( supportsScripting(context) )
    {
      // get the name of the form that the user set
      Object formName = node.getAttributeValue(context,
                                             FORM_NAME_ATTR);

      // if the user set no form, look for a form above us
      if (formName == null)
        formName = getParentFormName(context);

      if (formName != null)
      {
        String functionCall = getFunctionCall(context, node, formName.toString());

        Object clientOnClick = getClientOnClick(context, node);

        // chain together our onclick handler with any user handler.
        // make sure user handler is called first, then call our handler:
        return XhtmlLafUtils.getChainedJS(clientOnClick,
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
    else
      return super.getOnClick(context, node);
  }

  /**
   * =-= bts bogus copied code from org.apache.myfaces.trinidadinternal.ui.laf.browser.ResetButtonRenderer
   */
  protected String getFunctionCall(
    UIXRenderingContext context,
    UINode           node,
    String           formName
    )
  {
    StringBuffer buffer = new StringBuffer(21 +
                                           formName.length());

    buffer.append("return resetForm('");
    buffer.append(formName);
    buffer.append("');");

    return buffer.toString();

  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ResetButtonRenderer.class);
}
