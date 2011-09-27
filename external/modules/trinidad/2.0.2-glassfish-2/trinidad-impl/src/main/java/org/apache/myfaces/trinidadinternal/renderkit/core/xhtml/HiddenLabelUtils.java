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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

public class HiddenLabelUtils
{
  /**
   * Identifies whether the current agent supports the hidden label
   * trick.
   */
  static public boolean supportsHiddenLabels(RenderingContext arc)
  {
    if (XhtmlRenderer.isInaccessibleMode(arc))
      return false;

    // Though the method is named "supports" hidden labels, it's really being called and used in
    // the sense of "do we want to render" hidden labels, and if this method returns false no label
    // is written at all. We always want labels in screen reader mode, so return true.
    if (XhtmlRenderer.isScreenReaderMode(arc))
      return true;

    // For this switch - and for getting the major version - tunnel
    // to CoreRenderingContext
    TrinidadAgent agent = ((CoreRenderingContext) arc).getTrinidadAgent();
    switch (agent.getAgentApplication())
    {
      case IEXPLORER:
        if (agent.getAgentOS() == TrinidadAgent.OS_WINDOWS)
        {
          // IE 4 doesn't support the label hack.
          if (agent.getAgentMajorVersion() == 4)
            return false;

          // JDev VE masquerades as IE Windows, but doesn't support this
          if (agent.getCapability(TrinidadAgent.CAP_IS_JDEV_VE) != null)
            return false;

          // IE 5+ supports hidden labels
          return true;
        }

        // IE on the Mac (which got as far as IE5 before Mac IE was dropped) doesn't support hidden labels
        return false;

      // Mozilla, Safari, and Chrome supports hidden labels
      case GECKO:
      case SAFARI:
        return true;

      // Assume everyone else doesn't (important for the many mobile agents)
      default:
        return false;
    }
  }

  /**
   * Returns true if we want a hidden label for
   * a particular ID.
   */
  static public boolean wantsHiddenLabel(
    RenderingContext arc,
    String              id)
  {
    if (id == null)
      return false;

    // Note this shortcut to figuring out if the field already has
    // a label - we assume that labels are close to their fields,
    // and that therefore the last label written out is what
    // counts.
    if (id.equals(arc.getProperties().get(_LAST_LABEL_KEY)))
      return false;

    return true;
  }

  /**
   * Outputs a hidden label. 
   */
  static public void outputHiddenLabelIfNeeded(
    FacesContext        context,
    RenderingContext arc,
    String              id,
    Object              text,
    UIComponent         component
    ) throws IOException
  {
    if (supportsHiddenLabels(arc) && wantsHiddenLabel(arc, id))
    {
      outputHiddenLabel(context, arc, id, text, component);
    }
  }


  /**
   * Outputs a hidden label. 
   */
  static public void outputHiddenLabel(
    FacesContext        context,
    RenderingContext arc,
    String              id,
    Object              text,
    UIComponent         component
    ) throws IOException
  {
    if (text != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("label", component);
      writer.writeAttribute("for", id, null);
      XhtmlRenderer.renderStyleClass(context, arc,
                                     SkinSelectors.HIDDEN_LABEL_STYLE_CLASS);
      writer.writeText(text, null);
      writer.endElement("label");
    }
  }


  /**
   * Remembers that a "normal" hidden label has already been outputted.
   */
  static public void rememberLabel(
    RenderingContext arc,
    Object              id)
  {
    if (id != null)
    {
      arc.getProperties().put(_LAST_LABEL_KEY, id.toString());
    }
  }


  // Key used for storing the "last" label ID
  private static final Object _LAST_LABEL_KEY = new Object();

  private HiddenLabelUtils()
  {
  }
}
