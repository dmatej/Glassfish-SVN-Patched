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

import javax.faces.component.UIComponent;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;


/**
 * Renderer for button nodes.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ButtonRenderer.java#0 $) $Date: 10-nov-2005.18:53:45 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ButtonRenderer extends LinkRenderer
{
  /**
   * Returns true if button tags are being used.  Provides a hook for
   * subclassers to override
   */
  protected boolean useButtonTags(
    UIXRenderingContext context
    )
  {
    // button tags are used if we support advanced forms and
    // we support events for handling the onclick and
    // we support javascript
    return supportsAdvancedForms(context) &&
           supportsIntrinsicEvents(context)  &&
           supportsScripting(context);
  }

  @Override
  protected boolean doRenderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // If we're rendering button tags, render the style class.
    // Otherwise LinkRenderer.prerender() handles this for us.
    return (useButtonTags(context));
  }

  @Override
  protected void renderDestination(
    UIXRenderingContext context,
    UINode           node,
    String           destination
    ) throws IOException
  {
    // if we don't support advanced forms, a link is as good as we can do
    if (!useButtonTags(context))
    {
      super.renderDestination(context, node, destination);
    }
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    if (useButtonTags(context))
    {
      renderAttribute(context, "type", getButtonType());

      if (!supportsAdvancedButtons(context))
      {
        renderAttribute(context, "value", getText(context, node));
      }

      renderAttribute(context, node, "disabled", DISABLED_ATTR);
    }


  }

  /**
   * Override to change the type of the button
   */
  protected String getButtonType()
  {
    return "button";
  }


  /**
   * The ID and the naem aren't the same for buttons and button subclasses
   */
  @Override
  protected boolean makeNameAndIDSame(
    UIXRenderingContext context
    )
  {
    return false;
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (useButtonTags(context))
    {
      if (supportsAdvancedButtons(context))
      {
        return "button";
      }
      else
      {
        return "input";
      }
    }
    else
    {
      // link support is as good as we can do
      return super.getElementName(context, node);
    }
  }

  /**
   * Return true if this link is empty ... has no children, text,
   * destination, or node name. We render nothing.
   */
  @Override
  protected boolean isEmpty(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // buttons are never empty
    return false;
  }

  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (useButtonTags(context))
    {
      String elementName = getElementName(context, node);

      if (elementName != null)
      {
        UIComponent component = NodeUtils.getUIComponent(context, node);
        context.getResponseWriter().startElement(elementName, component);
        renderAttributes(context, node);

        // If we've got a ClientAction, let it write its dependencies
        // before we start rendering the link
        ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                       node);
        if (action != null)
          action.writeDependencies(context, node);
      }
    }
    else
    {
      // use link behavior
      super.prerender(context, node);
    }
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (useButtonTags(context))
    {
      String elementName = getElementName(context, node);

      if (elementName != null)
      {
        context.getResponseWriter().endElement(elementName);
      }
    }
    else
    {
      // use link behavior
      super.postrender(context, node);
    }
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    )
    throws IOException
  {
    if (useButtonTags(context))
    {
      if (supportsAdvancedButtons(context))
      {
        renderAccessKeyText(context, node, getText(context, node), 
                            SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
      }
    }
    else
    {
      // a link is as well as we can do here
      super.renderContent(context, node);
    }
  }

  @Override
  protected Object getText(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object text = super.getText(context, node);

    // If the text is null, create a button with an empty label
    if (text == null)
      return _EMPTY_LABEL;

    return text;
  }

  /**
   * Override to provide Javascript for moving to the destination
   */
  @Override
   protected Object getOnClick(
      UIXRenderingContext context,
      UINode           node
      )
    {
      Object clientOnClick = getClientOnClick(context, node);

      if (useButtonTags(context))
      {
        String destination
          = BaseLafUtils.getStringAttributeValue(context, node,
                                                 DESTINATION_ATTR);

        if (destination != null)
        {
          String destinationBeforeEncode = destination;
          FacesContext facesContext = context.getFacesContext();
          if(facesContext != null)
            destination = facesContext.getExternalContext().encodeActionURL(destination);

          Object targetFrame = getTargetFrame(context, node);

          // if destination starts with "javascript:",
          // then just use the destination as is
          String onClickJS = destination;

          // if there's a target frame the destination should be an url
          if (targetFrame != null)
          {
            onClickJS = "top["         +
                        targetFrame    +
                        "].location='" +
                        destination    +
                        "'";
          }
          else if ( destinationBeforeEncode.length() < 11 ||
                    !"javascript:".equalsIgnoreCase(destinationBeforeEncode.substring(0,11)))
          {
            onClickJS = "document.location='" + destination + "'";
          }

          clientOnClick = XhtmlLafUtils.getChainedJS(clientOnClick,
                                                     onClickJS,
                                                     true);
        }
      }

      return clientOnClick;
  }

  protected final Object getClientOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return super.getOnClick(context, node);
  }

  private static final String _EMPTY_LABEL = "";
}
