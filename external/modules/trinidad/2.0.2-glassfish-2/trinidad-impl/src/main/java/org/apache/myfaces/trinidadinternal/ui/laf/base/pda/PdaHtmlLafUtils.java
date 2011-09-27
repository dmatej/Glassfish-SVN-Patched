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

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager;
import org.apache.myfaces.trinidadinternal.ui.laf.NameAndAgentScorer;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/PdaHtmlLafUtils.java#0 $) $Date: 10-nov-2005.18:55:02 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PdaHtmlLafUtils extends XhtmlLafUtils
{
  protected PdaHtmlLafUtils()
  {
  }

  /**
   * Registers the pda html PC Look And Feel with the specified
   * LookAndFeelManager.
   */
  public static void registerLookAndFeel(
    LookAndFeelManager manager
    )
  {
    manager.registerLookAndFeel(_SCORER, new PdaHtmlLookAndFeel());
  }
  
  /**
   * This method returns the partial targets associated with the specified node
   * @param context the rendering context
   * @param node the current UINode
   * @param id the component's id
   */
  public static String[] getPartialTargets(
    UIXRenderingContext context,
    UINode           node,
    Object           id
    ) 
  {  
    // Make sure partial page rendering is supported
    if (!XhtmlLafRenderer.supportsPartialRendering(context))
      return null;

    if (!PartialPageUtils.isPPRActive(context.getFacesContext()))
      return null;

    // If the ID is null, get the ID from the node
    if (id == null)
      id = node.getAttributeValue(context, ID_ATTR);

    // If we don't have an ID, we don't have any targets
    if (id == null)
      return null;

    // We're all set... Use the node's ID as the partial target
    return new String[] { id.toString() };
  }

  private static final NameAndAgentScorer _SCORER =
    new NameAndAgentScorer(null,
                           null,
                           null,
                           null,
                           TrinidadAgent.TYPE_PDA);
}
