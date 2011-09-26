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

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.SubTabBarUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.FormValueRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ShowItemRenderer extends LinkRenderer
{
  @Override
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              currVisChildIndex,
    int              prevVisChildIndex,
    int              nextVisChildIndex,
    int              ithRenderedChild
    ) throws IOException
  {
    //  Do nothing here, my parent will take care of rendering my children
    //  if I have my 'selected' attribute set to true.
  }

  // Returns the partial change script that is usually rendered for onClick
  @Override
  protected String getPartialChangeScript(
    UIXRenderingContext context,
    UINode           node
    )
  {
    String partialTargets = getAncestorPartialTargets(context);
    String sourceParam = BaseLafUtils.getStringAttributeValue(context,
                                                       node, ID_ATTR);
    String formName = getParentFormName(context);
    
    // Lets render fullpage submit if PPR is not enabled
    if (partialTargets == null)
    {
      String fullPageSubmitScript = XhtmlLafUtils.
                                       getFullPageSubmitScript(formName, "0",
                                                               SHOW_EVENT,
                                                               sourceParam);
                                                               
      FormValueRenderer.addNeededValue(context, formName, EVENT_PARAM,
                                       SOURCE_PARAM, null,null);
      return fullPageSubmitScript;                                
    }
    
    String validate = 
              Boolean.TRUE.equals(SubTabBarUtils.isUnvalidated(context)) ? 
              "0" : "1"; 
              
    String partialChangeScript = XhtmlLafUtils.
                                getPartialPageSubmitScript(formName,
                                                           validate,
                                                           partialTargets,
                                                           SHOW_EVENT,
                                                           sourceParam);
                                                               
    FormValueRenderer.addNeededValue(context, formName, EVENT_PARAM,
                                     PARTIAL_TARGETS_PARAM, SOURCE_PARAM,
                                     PARTIAL_PARAM);
    return partialChangeScript;
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  { 
    // For Non-JavaScript browsers, render an input element(type=submit) to 
    // submit the page. Encode the name attribute with the parameter name 
    // and value thus it would enable the browsers to include the name of 
    // this element in its payLoad if it submits the page.
    if (!supportsScripting(context))
    {    
      // render the id
      renderID(context, node);
      renderAttributesExceptID(context, node);
      renderAttribute(context, "type", "submit");
      String sourceParam = BaseLafUtils.getStringAttributeValue(
                                        context, node, ID_ATTR);
      String nameAttri =  XhtmlUtils.getEncodedParameter
                                      (XhtmlConstants.SOURCE_PARAM)
                          + XhtmlUtils.getEncodedParameter(sourceParam)
                          + XhtmlUtils.getEncodedParameter
                                      (XhtmlConstants.EVENT_PARAM)
                          + SHOW_EVENT;
      renderAttribute(context, "name", nameAttri);
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

  /**
   * Returns the StyleClass to use to render this node.
   */
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Handle CSS for Basic HTML browser separately since we are rendering   
    // input element for Basic HTML browser instead of an anchor element
    if (!supportsScripting(context) && 
          Boolean.TRUE.equals(node.getAttributeValue(context, DISCLOSED_ATTR)))
    {
      return SkinSelectors.AF_SHOW_DETAIL_ITEM_SELECTED;
    }
    
    String nodeStyleClass = 
      (String)node.getAttributeValue(context, STYLE_CLASS_ATTR);
    if (nodeStyleClass != null)
      return nodeStyleClass;

    String styleClass = null;

    // We provide a default style class for links, as long
    // as our parent hasn't explicitly disabled this.
    if (!LinkUtils.isDefaultStyleClassDisabled(context))
    {
      styleClass = (isDisabled(context, node)) ? 
                      LINK_DISABLED_STYLE_CLASS :
                      LINK_STYLE_CLASS;
    }
    return styleClass;
  }
}
