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
package org.apache.myfaces.trinidadinternal.renderkit.html.layout;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.partial.PartialPageRendererUtils;

/**
 *  Utility class for showOne category of components.
 *
 *  @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/html/layout/ShowOneUtils.java#0 $) $Date: 10-nov-2005.19:01:15 $
 */
public class ShowOneUtils
{
  /**
   *  Generates markup corresponding to Core attributes the component has.
   *
   * @param context the faces context object
   * @param component the UIComponent object
   * @throws IOException when can't write onto response writer
   */
  @SuppressWarnings("unchecked")
  static void renderGenericAttributes(UIXRenderingContext rCtx,
                                      UIComponent component,
                                      ResponseWriter out)
    throws IOException
  {
    Map<String, Object> attrMap = component.getAttributes();

    // there is a certain amount of controversy about writing this attribute.
    // see bug 1606882
    String shortDesc = (String) attrMap.get("shortDesc");
    if (shortDesc != null)
    {
      out.writeAttribute("title", shortDesc, null);
    }

    if (BaseLafRenderer.supportsStyleAttributes(rCtx))
    {
      Object inlineStyle = attrMap.get("inlineStyle");
      if (inlineStyle != null)
      {
        XhtmlLafRenderer.renderInlineStyleAttribute(rCtx, inlineStyle);
      }
    }

    if (BaseLafRenderer.supportsIntrinsicEvents(rCtx) )
    {
      // since the showOne components don't have disabled attribute,
      // it's safe enough to just write all the event handlers.
      String onclick = (String) attrMap.get("onclick");
      if (onclick != null)
      {
        out.writeAttribute("onclick", onclick, null);
      }
      String ondblclick = (String) attrMap.get("ondblclick");
      if (ondblclick != null)
      {
        out.writeAttribute("ondblclick", ondblclick, null);
      }
      String onmousedown = (String) attrMap.get("onmousedown");
      if (onmousedown != null)
      {
        out.writeAttribute("onmousedown", onmousedown, null);
      }
      String onmouseup = (String) attrMap.get("onmouseup");
      if (onmouseup != null)
      {
        out.writeAttribute("onmouseup", onmouseup, null);
      }
      String onmouseover = (String) attrMap.get("onmouseover");
      if (onmouseover != null)
      {
        out.writeAttribute("onmouseover", onmouseover, null);
      }
      String onmousemove = (String) attrMap.get("onmousemove");
      if (onmousemove != null)
      {
        out.writeAttribute("onmousemove", onmousemove, null);
      }
      String onmouseout = (String) attrMap.get("onmouseout");
      if (onmouseout != null)
      {
        out.writeAttribute("onmouseout", onmouseout, null);
      }
      String onkeypress = (String) attrMap.get("onkeypress");
      if (onkeypress != null)
      {
        out.writeAttribute("onkeypress", onkeypress, null);
      }
      String onkeydown = (String) attrMap.get("onkeydown");
      if (onkeydown != null)
      {
        out.writeAttribute("onkeydown", onkeydown, null);
      }
      String onkeyup = (String) attrMap.get("onkeyup");
      if (onkeyup != null)
      {
        out.writeAttribute("onkeyup", onkeyup, null);
      }
    }
  }


  /**
   *  Creates encoded partial targets for UIComponent.
   *  Adds the compId to the list of partial targets available in component
   *  attribute map.
   *
   * @param component the UIComponent object
   * @param compId the id to be added to already available partial targets
   * @return String encoded partialTarget string
   */
  static String getEncodedPartialTargets(UIComponent component, String compId)
  {
    String[] pprTargets =
      (String []) component.getAttributes().get("partialTargets");
    String encodedPartialTargets = null;
    if ( (pprTargets != null) && (pprTargets.length > 0)  )
    {
      String[] pprTargetsNew = new String[pprTargets.length + 1];
      System.arraycopy(pprTargets, 0, pprTargetsNew, 0, pprTargets.length);
      pprTargetsNew[pprTargets.length] = compId;
      encodedPartialTargets =
        PartialPageRendererUtils.encodePartialTargets(pprTargetsNew);
    }
    else
    {
      encodedPartialTargets =
        PartialPageRendererUtils.encodePartialTargets(new String[] {compId});
    }
    return encodedPartialTargets;
  }
}
