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

import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.SubTabBarUtils;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Renderer for ShowItem
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ShowItemRenderer.java#0 $) $Date: 10-nov-2005.18:54:13 $
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
}
