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

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelLabelAndMessage;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;


/**
 */
public class PanelLabelAndMessageRenderer extends LabelAndMessageRenderer
{
  public PanelLabelAndMessageRenderer()
  {
    super(CorePanelLabelAndMessage.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _forKey = type.findKey("for");
    _labelInlineStyleKey = type.findKey("labelStyle");
  }

  @Override
  protected boolean labelShowRequired(
    UIComponent component,
    FacesBean   bean)
  {
    // Simpler algorithm for panelLabelAndMessage
    return getShowRequired(component, bean);
  }

  @Override
  protected boolean isLeafRenderer()
  {
    return false;
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|panelLabelAndMessage";
  }

  @Override
  protected String getLabelFor(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    String forValue = getFor(component, bean);

    String val = null;
    if (forValue != null)
    {
      val = MessageUtils.getClientIdFor(context, component, forValue);
    }
    else
    {
      if (component.getChildCount() > 0)
      {
        UIComponent child = findForComponent(context, rc, component, bean);
        if (child != null)
        {
          val = child.getClientId(context);
        }
      }
    }

    return val;
  }

  @Override
  protected void renderFieldCellContents(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // The structure of this part of the DOM looks like this:
    // +------------------+-----------+
    // | indexed children | end facet |
    // +------------------+-----------+
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("table", component);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", null/*width*/);

    UIComponent end = getFacet(component, CorePanelLabelAndMessage.END_FACET);

    // Build the main row:
    rw.startElement("tr", null);
    rw.startElement("td", null);
    encodeAllChildren(context, component);
    rw.endElement("td");

    // For narrow-screen PDAs, End facet is rendered vertically
    // below the Help facet. So skip the End facet rendering here.
    if (end != null && !supportsNarrowScreen(rc))
    {
      rw.startElement("td", null);
      // =-= mcc TODO apply className for "af|panelLabelAndMessage::end-facet"
      // renderStyleClass(context, arc, ...);
      //apply className for "af|panelLabelAndMessage::help-facet"
      encodeChild(context, end);
      rw.endElement("td");
    }

    rw.endElement("tr");
    rw.endElement("table");
  }

  protected String getFor(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_forKey));
  }

  @Override
  protected String getLabelInlineStyleKey(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_labelInlineStyleKey));
  }

  /**
   * In the event that the {@link #getFor(FacesBean)} returns null,
   * this class finds the first child that implements {@link EditableValueHolder}
   *
   * @param context
   * @param arc
   * @param component
   * @param bean
   * @return
   */
  protected UIComponent findForComponent(
    FacesContext        context,
    RenderingContext    arc,
    UIComponent         component,
    FacesBean           bean)
  {
    // search children first
    for (Object obj : component.getChildren())
    {
      UIComponent child = (UIComponent)obj;
      if (obj instanceof EditableValueHolder)
      {
        return child;
      }
    }

    // recursively search the children of the children
    for (Object obj : component.getChildren())
    {
      UIComponent child = (UIComponent)obj;
      UIComponent result = findForComponent(context, arc, child, bean);
      if (result != null)
      {
        return result;
      }
    }

    return null;
  }

  /* This method is responsible for rendering the End facet for narrow-screen
   * PDAs. In the case of narrow-screen PDAs, End facet is rendered after the
   * Help  facet as shown below
   * +------+
   * |Label |
   * +------+
   * |Field |
   * +----------+
   * |Help facet|
   * +----------+
   * |End facet |
   * ------------
   * @param context a <code>FacesContext</code>
   * @param rc a <code>RenderingContext</code>
   * @param component a <code>UIComponent</code> the component to render
   * @param insideTableData a <code>boolean</code> indicates whether End
   *        Facet to be rendered is in inside a table data(<TD>)
   * @throws IOException if there are problems in rendering contents
   */
  @Override
  protected void renderEndFacetForNarrowPDA(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    boolean          insideTableData)
    throws IOException
  {
    if (!supportsNarrowScreen(rc))
      return;

    UIComponent end = getFacet(component, CorePanelLabelAndMessage.END_FACET);

    if (end != null)
    {
      ResponseWriter rw = context.getResponseWriter();

      if (insideTableData)
      {
        rw.startElement("div", null);
        encodeChild(context, end);
        rw.endElement("div");
      }
      else
      {
        rw.startElement("tr", null);
        rw.startElement("td", null);
        encodeChild(context, end);
        rw.endElement("td");
        rw.endElement("tr");
      }
    }
  }

  private PropertyKey _forKey;
  private PropertyKey _labelInlineStyleKey;
}
