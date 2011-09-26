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

import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;


/**
 * Renderer for Navigation Bars showing either single or multiple records.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/NavigationBarRenderer.java#0 $) $Date: 10-nov-2005.18:54:59 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NavigationBarRenderer extends
  org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.NavigationBarRenderer

{
  protected MutableUINode createSingleItemURLButton(
    UIXRenderingContext context,
    boolean          isBack,
    Object           buttonText,
    String           destinationURL
    )
  {
    MarlinBean urlLink = new MarlinBean(LINK_NAME);

    urlLink.setAttributeValue(TEXT_ATTR, buttonText);
    urlLink.setAttributeValue(DESTINATION_ATTR, destinationURL);
    urlLink.setAttributeValue(STYLE_CLASS_ATTR, NAV_BAR_ALINK_STYLE_CLASS);    

    return urlLink;
  }
  
  protected MutableUINode createSingleItemSubmitButton(
    UIXRenderingContext context,
    boolean          isBack,
    Object           buttonText,
    String           onClickJS
    )
  {
    MutableUINode submitButton = createSingleItemURLButton(
                                                    context,
                                                    isBack,
                                                    buttonText,
                                                    null);
                                                    
    submitButton.setAttributeValue(ON_CLICK_ATTR, onClickJS);
    
    return submitButton;                                   
  }


  /**
   * Returns true if disabled navigation items should be shown
   */
  @Override
  protected boolean disabledNavigationShown(
    UIXRenderingContext context
    )
  {
    //return false;
   return true;
  }

}


