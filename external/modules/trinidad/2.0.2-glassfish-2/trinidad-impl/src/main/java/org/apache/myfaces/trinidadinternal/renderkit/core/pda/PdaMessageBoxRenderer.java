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
package org.apache.myfaces.trinidadinternal.renderkit.core.pda;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.output.CoreMessages;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.MessageBoxRenderer;


/**
 * Renderer for org.apache.myfaces.trinidad.Messages, family org.apache.myfaces.trinidad.Messages.
 *
 */
public class PdaMessageBoxRenderer extends MessageBoxRenderer
{
  public PdaMessageBoxRenderer()
  {
    this(CoreMessages.TYPE);
  }

  protected PdaMessageBoxRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    Agent agent = rc.getAgent();

    // BlackBerry and many pda browsers don't support inline style of
    // display:none. Thus, it is necessary to slip rendering entire
    // element if there is no message to display.
    // This method checks for the condition and returns true.

    if (agent != null &&
                 (Agent.AGENT_BLACKBERRY.equals(agent.getAgentName()) ||
                  Agent.AGENT_GENERICPDA.equals(agent.getAgentName()) ))
    {
      boolean hasMessages =
                    FacesContext.getCurrentInstance().getMessages().hasNext();
      if (!hasMessages)
      {
        return;
      }
    }
    super.encodeAll(context, rc, component, bean);
  }
}
