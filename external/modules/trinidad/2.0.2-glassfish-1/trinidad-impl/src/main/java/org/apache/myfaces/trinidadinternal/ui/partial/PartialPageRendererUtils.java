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
package org.apache.myfaces.trinidadinternal.ui.partial;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Utility methods for Renderers which support partial page rendering.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/partial/PartialPageRendererUtils.java#0 $) $Date: 10-nov-2005.18:50:29 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PartialPageRendererUtils
{
  private PartialPageRendererUtils()
  {
  }

  /**
   * Encodes the specified partial target IDs into a single value
   * which is suitable for use as the "partialTargets" event
   * parameter value.
   * <p>
   * Renderers should call this method to encode the set of partial
   * target IDs when generating URLs or forms which contain the
   * partialTargets event parameter.
   *
   * @param partialTargets The set of partial targets to encode
   * @return Returns a String that can be used as the
   * "partialTargets" event parameter value.
   */
  public static String encodePartialTargets(String[] partialTargets)
  {
    if ((partialTargets == null) || (partialTargets.length == 0))
      return null;

    if (partialTargets.length == 1)
      return partialTargets[0];

    // Pre-compute the length of the StringBuffer.  We leave room
    // for each partial target id plus white space separators.
    int length = partialTargets.length - 1;
    for (int i = 0; i < partialTargets.length; i++)
      length += partialTargets[i].length();

    StringBuffer buffer = new StringBuffer(length);
    for (int i = 0; i < partialTargets.length; i++)
    {
      buffer.append(partialTargets[i]);

      // Tack on the white space separator
      if (i < (partialTargets.length - 1))
        buffer.append(' ');
    }

    return buffer.toString();
  }

  /**
   * Returns true if we are performing a partial page render.
   */
  public static boolean isPartialRenderingPass(
    UIXRenderingContext context
    )
  {
    return (context.getPartialPageContext() != null);
  }

  /**
   * Tests whether partial page rendering is supported for the
   * current render.
   * <p>
   * Partial page rendering is not supported on all user agents.
   * This method returns false if partial page rendering is not supported
   * by the agent associated with the provided RenderingContext.
   * <p>
   * This method returns false if the disable-partial-rendering configuration 
   * element is set to true. Otherwise, this method returns true.
   * (PPR is considered accessible, so we do not check the accessibility mode)
   */
  public static boolean supportsPartialRendering(
    UIXRenderingContext context
    )
  {

    // First, make sure the agent supports partial rendering
    TrinidadAgent agent = context.getAgent();
    Object capPartial = agent.getCapability(TrinidadAgent.CAP_PARTIAL_RENDERING);
    if (!Boolean.TRUE.equals(capPartial))
      return false;

    return true;
  }

  public static boolean supportsBlocking(
    UIXRenderingContext context
    )
  {
    // At the moment we have blocking solved on IE and Mozilla
    if (supportsPartialRendering(context))
    {
      TrinidadAgent.Application application = context.getAgent().getAgentApplication();

      return ((application == TrinidadAgent.Application.IEXPLORER)
              || (application == TrinidadAgent.Application.GECKO));
    }
    return false;
  }
}
