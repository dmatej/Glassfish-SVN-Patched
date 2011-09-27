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

import java.util.ListIterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXShowDetail;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.RenderUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;

/**
 * Renderer for ShowOneChoice
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/html/layout/CoreShowOneChoiceRenderer.java#0 $) $Date: 10-nov-2005.19:00:41 $
 */
public class CorePanelChoiceRenderer extends ShowOneListRendererBase
{
  /**
   *  {@inheritDoc}
   */
  @Override
  protected void writeAdditionalJS(FacesContext context,
                                   UIComponent component)
    throws IOException
  {
    UIXRenderingContext rCtx = getRenderingContext(context, component);
    String formName = RenderUtils.getFormId(context, component);

    boolean jsRendered =
      XhtmlLafRenderer.isPreviouslyRendered(rCtx,
        _SHOWONECHOICE_SETCHOICE_JS_RENDERED);

    if ((formName != null) && (! jsRendered) )
    {
      String getSelectedScript = null;
      if (RequestContext.getCurrentInstance().isDebugOutput())
      {
        getSelectedScript =
          "function _socGetSelection(elem)" +
          "{" +
          "  var selectedElem = document.getElementById(elem);" +
          "  var selectedOptions = selectedElem.options;" +
          "  var numOptions = selectedOptions.length;" +
          "  for (i = 0; i < numOptions; i++)" +
          "  {" +
          "    var selectOption = selectedOptions.item(i);" +
          "    var isSelected = selectOption.selected;" +
          "    if (isSelected)" +
          "    {" +
          "      return selectOption.id;" +
          "      break;" +
          "    }" +
          "  }" +
          "}";
      }
      else
      {
        getSelectedScript =
          "function _socGetSelection(e)" +
          "{" +
          "var sE=document.getElementById(e);" +
          "var sO=sE.options;" +
          "var nO=sO.length;" +
          "for (i=0;i<nO;i++)" +
          "{" +
          "var seO=sO.item(i);" +
          "var iS=seO.selected;" +
          "if (iS)" +
          "{" +
          "return seO.id;" +
          "break;" +
          "}" +
          "}" +
          "}";
      }

      _writeScript(context, component, getSelectedScript);
    }
  }

  /**
   * @inheritDoc
   *
   */
  @Override
  protected void renderListDisplay(FacesContext context,
                                   UIComponent component,
                                   String disclosedChildId)
    throws IOException
  {
    // This renders the select controls alongwith javascript onchange handler.
    UIXRenderingContext rCtx =
      getRenderingContext(context, component);

    String compId = component.getClientId(context);

    ResponseWriter out = context.getResponseWriter();

    // draw table to contain the select UI control
    out.startElement("table", component);
    out.writeAttribute("id", compId + _CHOICE_TABLE_SUFFIX_ID_CONST, null);
    out.writeAttribute("border", "0", null);
    out.writeAttribute("cellspacing", "0", null);
    out.writeAttribute("cellpadding", "0", null);

    if (!XhtmlLafRenderer.isInaccessibleMode(rCtx))
    {
      out.writeAttribute("summary", "", null);
    }

    out.startElement("tr", component);

    renderSelectLabel(rCtx, component, out, compId);

    // Render filler / separator between label and select control
    renderSpacerTD(out, component, getLabelControlSeparatorSize());

    _renderSelectItemInTD(context,
                          component,
                          disclosedChildId);

    out.endElement("tr");
    out.endElement("table");
  }

  /**
   *  {@inheritDoc}
   */
  @Override
  protected String getHTMLControlID(String compId)
  {
    return compId + _CHOICE_SELECT_SUFFIX_ID_CONST;
  }

  /**
   *  Generates markup for rendering HTML select control.
   *
   *  The select control markup is contained within a TD element.
   */
  @SuppressWarnings("unchecked")
  private void _renderSelectItemInTD(FacesContext context,
                                     UIComponent component,
                                     String disclosedChildId)
    throws IOException
  {
    String compId = component.getClientId(context);
    ResponseWriter out = context.getResponseWriter();
    UIXRenderingContext rCtx = getRenderingContext(context, component);

    out.startElement("td", component);
    out.writeAttribute("valign", "top", null);
    out.writeAttribute("nowrap", Boolean.TRUE, null);

    out.startElement("select", component);
    out.writeAttribute("id", compId + _CHOICE_SELECT_SUFFIX_ID_CONST, null);
    out.writeAttribute("name", compId + _CHOICE_SELECT_SUFFIX_ID_CONST,null);

    XhtmlLafRenderer.renderStyleClassAttribute(rCtx, getFieldTextClass());

    // construct the javascript for submitting the form
    String onChangeJS = _getChoiceOnchangeJS(context,rCtx, component, compId);
    if (onChangeJS != null)
    {
      out.writeAttribute("onchange", onChangeJS, null);
    }

    // Render options now.
    ListIterator<UIComponent> children = component.getChildren().listIterator();
    while (children.hasNext())
    {
      UIComponent child = children.next();
      if (child instanceof UIXShowDetail)
      {
        UIXShowDetail detailItem = (UIXShowDetail) child;

        Boolean disabledObj =
          (Boolean) detailItem.getAttributes().get("disabled");

        boolean disabled = false; // by default is enabled.
        if (disabledObj != null)
        {
          disabled = disabledObj.booleanValue();
        }

        if (disabled)
        {
          // MSDN DHTML Reference says disabled not supported for option element
          // hence don't render disabled options at all to be consistent 
          // across browsers. See Bug 4561967.
          continue;
        }

        String childTitle = (String) detailItem.getAttributes().get("text");
        String childClientId = child.getClientId(context);

        out.startElement("option", component);
        out.writeAttribute("id", childClientId, null);
        if (childClientId.equals(disclosedChildId))
        {
          out.writeAttribute("selected", Boolean.TRUE, null);
        }

        if (childTitle != null)
          out.writeText(childTitle, null);

        out.endElement("option");
      }
    }
    out.endElement("select");

    out.endElement("td");
  }

  /**
   *  Gets onChange javascript to be associated with choice onchange event.
   *
   *  Checks if component is contained within a form, if not, returns null.
   *  Further, checks if PPR is supported and returns a script to be called
   *  for this case else returns a script where PPR is not required.
   */
  private String _getChoiceOnchangeJS(FacesContext context,
                                      UIXRenderingContext rCtx,
                                      UIComponent component,
                                      String compId)
  {
    String formName = RenderUtils.getFormId(context, component);
    if (formName == null)
    {
      return null;
    }

    // Check if PPR enabled, do a _submitPartialChange, else do a formSubmit.
    String onClickHandler = "";
    boolean pprEnabled = elementSupportsPartial(rCtx, compId);
    if (pprEnabled)
    {
      String encodedPartialTargets =
        ShowOneUtils.getEncodedPartialTargets(component, compId);
      StringBuffer jsBuff = new StringBuffer(220);
      jsBuff.append("var selectedOption = ")
            .append("_socGetSelection('")
            .append(compId + _CHOICE_SELECT_SUFFIX_ID_CONST)
            .append("');  ")
            .append("_submitPartialChange('")
            .append(formName)
            .append("','1', {partialTargets:'")
            .append(encodedPartialTargets)
            .append("', event:'show',source:selectedOption")
            .append("});return true;");

      onClickHandler = jsBuff.toString();
    }
    else
    {
      StringBuffer jsBuff = new StringBuffer(135);
      jsBuff.append("var selectedOption = ")
            .append("_socGetSelection('")
            .append(compId + _CHOICE_SELECT_SUFFIX_ID_CONST)
            .append("');  ")
            .append("submitForm('")
            .append(formName)
            .append("','1'")
            .append(",{event:'show',source:selectedOption")
            .append("});return true;");
      onClickHandler = jsBuff.toString();
    }
    return onClickHandler;
  }

  private void _writeScript(FacesContext context,
                            UIComponent component,
                            String js)
    throws IOException
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();

    ResponseWriter writer = context.getResponseWriter();
    
    writer.startElement("script", null);
    XhtmlRenderer.renderScriptDeferAttribute(context, arc);
    // Bug #3426092:
    // render the type="text/javascript" attribute in accessibility mode
    XhtmlRenderer.renderScriptTypeAttribute(context, arc);

    writer.writeText(js, null);
    writer.endElement("script");
  }

  private static final String _SHOWONECHOICE_SETCHOICE_JS_RENDERED =
    "_SHOWONECHOICE_SETSELECTCHOICE_JS_RENDERED";

  private static final String _CHOICE_TABLE_SUFFIX_ID_CONST = "_soc_tbl";
  private static final String _CHOICE_SELECT_SUFFIX_ID_CONST =
    _CHOICE_TABLE_SUFFIX_ID_CONST + "_chc";

}
