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

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/TableLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:54:16 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class TableLayoutRenderer extends XhtmlLafRenderer
{  
  /**
   * Renders attributes of the current node.
   */
  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    renderHAlign(context, node);
        
    renderAttribute(context, node, "width",  WIDTH_ATTR);

    // For bug 1772696, if summary has not been set we render an empty summary
    // attribute on TableLayoutBeans
    if (!isInaccessibleMode(context))
    {
      Object summary = node.getAttributeValue(context, SUMMARY_ATTR);
      renderAttribute(context, "summary", (summary==null) ? "" : summary);
    }

    Object tableCap = getAgentCapability(context, TrinidadAgent.CAP_TABLES);
    
    boolean supportsAdvancedAttrs = false;
    boolean supportsAdvanced      = false;
    
    if (tableCap != null)
    {
      supportsAdvanced = (TrinidadAgent.TABLES_CAP_ADVANCED == tableCap);
      supportsAdvancedAttrs = supportsAdvanced ||
                              (TrinidadAgent.TABLES_CAP_ADVANCED_ATTRS == tableCap);
                              
      if (supportsAdvancedAttrs)
      {          
        renderAttribute(context, node, "border", BORDER_WIDTH_ATTR, ZERO);
        renderAttribute(context, node, "cellspacing", CELL_SPACING_ATTR, ZERO);
        renderAttribute(context, node, "cellpadding", CELL_PADDING_ATTR, ZERO);
        
        if (supportsAdvanced)
        {
          renderAttribute(context, node, "height", HEIGHT_ATTR);
        }
      }
    }
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return "table";
  }
}
