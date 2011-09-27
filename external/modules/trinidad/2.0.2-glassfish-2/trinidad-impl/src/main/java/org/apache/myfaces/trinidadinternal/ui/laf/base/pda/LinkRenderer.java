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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.ui.action.FireAction;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class LinkRenderer extends org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkRenderer
{
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node)
  {
    Object styleClass = node.getAttributeValue(context, STYLE_CLASS_ATTR);
    if (styleClass != null)
      return styleClass;

    if (!LinkUtils.isDefaultStyleClassDisabled(context))
    {
      if (isDisabled(context, node))
        styleClass = LINK_DISABLED_STYLE_CLASS;
      else
      {
        boolean isSelected =  Boolean.TRUE.equals(
           node.getAttributeValue(context, SELECTED_ATTR));
        if (isSelected)
          styleClass = LINK_SELECTED_STYLE_CLASS;
        else
          styleClass = LINK_STYLE_CLASS;
      }
    }

    return styleClass;
  }
  
  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Check if the request is from a Non-JavaScript browsers and the 
    // destination is null. If so, we need to render an input tag element 
    if (_isInput(context,node))
    {
      return "input";
    }
    
    return super.getElementName(context, node);
  }
  
  
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // if the element is an input tag, it means the request is from
    // a Non-JavaScript browser so encode the param name and value in
    // the name attribute and set the type attribute to "submit"
    if (_isInput(context, node))
    {
      // render the id
      renderID(context, node);
      renderAttributesExceptID(context, node);
      ClientAction action = ClientActionUtils.getPrimaryClientAction
                                                     (context, node);
      String nameAttr = null;
      if(action != null)
      {
        String source = ((FireAction) action).getSource();
        nameAttr = XhtmlConstants.SOURCE_PARAM + 
                         XhtmlConstants.NO_JS_PARAMETER_KEY + source;
      }
      else
      {
        Object id = getID(context, node);
        if(id != null)
        {
          nameAttr = id.toString();
        }
      }
       
      renderAttribute(context, "type", "submit");
      if (nameAttr != null)
      {
        renderAttribute(context, "name", nameAttr);
      }
      renderAttribute(context, "value", getText(context,node));
      String linkConverter = 
              "border: none; background: inherit; text-decoration: underline;";
      renderAttribute(context, "style", linkConverter);
      if (isDisabled(context, node))
      {  
        renderAttribute(context, "disabled", Boolean.TRUE);
      }
    }
    else
    {
      super.renderAttributes(context, node);
    }
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (isEmpty(context, node))
      return;
    
    // Since the value attribute of input element is already set to the content,  
    // render the content only for elements other than input.
    if (!_isInput(context,node))
    {
      super.renderContent(context, node);
    }
  }

  @Override
  protected void renderID(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object id = getID(context, node);
 
    if (id != null)
    {
      if (supportsID(context))
      {
        // For links, "name" and thus "id" is a URI attribute.
        renderURIID(context, id);
      }
      // Don't render name attribute for input element since it is already been  
      // rendered with encoded param name and value pair in renderAttribute method.
      if (!_isInput(context, node) && supportsNameIdentification(context) 
                                    && makeNameAndIDSame(context))
      {
        renderURIAttribute(context, "name", id);
      }
    }
  }

  /**
   * Returns true if the request is from a Non-JavaScript browsers and the 
   * destination is not available. 
   */
 
   private boolean _isInput(
     UIXRenderingContext context,
     UINode           node
     )
   {
     String destination = getDestination(context, node);

     return (((destination == null) || ("#".equals(destination)))
                                          && (!supportsScripting(context)));
   }
}
