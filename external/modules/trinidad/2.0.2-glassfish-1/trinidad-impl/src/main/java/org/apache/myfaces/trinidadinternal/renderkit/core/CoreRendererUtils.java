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
package org.apache.myfaces.trinidadinternal.renderkit.core;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.Agent;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.agent.CapabilityKey;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.RenderUtils;

public class CoreRendererUtils
{
  private CoreRendererUtils()
  {
  }

  public static String getRelativeId(
    FacesContext context,
    UIComponent  from,
    String       relativeId)
  {
    // Call through to public API
    return RenderUtils.getRelativeId(context, from, relativeId);
  }


  /**
   * Tests whether partial page rendering is supported for the
   * current render.
   * <p>
   * Partial page rendering is not supported on all user agents.
   * This method returns false if partial page rendering is not supported
   * by the agent associated with the provided rendering context.
   */
  public static boolean supportsPartialRendering(
    RenderingContext context
    )
  {
    return supportsBooleanCapability(context,
                                     TrinidadAgent.CAP_PARTIAL_RENDERING);
  }

  public static boolean supportsNameIdentification(
    RenderingContext context
    )
  {
    return supportsBooleanCapability(context,
                                     TrinidadAgent.CAP_NAME_IDENTIFICATION);
  }

  public static boolean supportsBooleanCapability(
    RenderingContext context,
    CapabilityKey cap
    )
  {
    Agent agent = context.getAgent();
    Object capPartial = agent.getCapabilities().get(cap);
    if (!Boolean.TRUE.equals(capPartial))
      return false;

    return true;
  }
}
