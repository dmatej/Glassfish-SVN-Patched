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

import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXShowDetail;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelAccordion;
import org.apache.myfaces.trinidad.component.core.layout.CoreShowDetailItem;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.DisclosureEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;


/**
 * Renderer for PanelAccordion
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/html/layout/CoreShowOneAccordionRenderer.java#0 $) $Date: 10-nov-2005.19:01:13 $
 */
public class PanelAccordionRenderer extends XhtmlRenderer
{
  public PanelAccordionRenderer()
  {
    super(CorePanelAccordion.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _discloseNoneKey = type.findKey("discloseNone");
    _discloseManyKey = type.findKey("discloseMany");
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    String       clientId)
  {
    Map<String, String> parameters =
      facesContext.getExternalContext().getRequestParameterMap();

    Object event = parameters.get(XhtmlConstants.EVENT_PARAM);
    if (XhtmlConstants.HIDE_EVENT.equals(event) ||
        XhtmlConstants.SHOW_EVENT.equals(event))
    {
      Object source = parameters.get(XhtmlConstants.SOURCE_PARAM);
      String id = clientId == null ? component.getClientId(facesContext) : clientId;

      if (id.equals(source))
      {
        boolean isDisclosed = XhtmlConstants.SHOW_EVENT.equals(event);
        String itemId = parameters.get("targetItem");
        if (itemId != null)
        {
          List<UIComponent> children = component.getChildren();
          for (UIComponent child : children)
          {
            if (!(child instanceof UIXShowDetail))
              continue;

            // Don't even bother with disabled showDetailItems -
            // a malicious user should not be able to force
            // a disabled item open by dummying up an event
            if (!child.isRendered() || _isItemDisabled(child))
              continue;

            if (itemId.equals(child.getClientId(facesContext)))
            {
              (new DisclosureEvent(child, isDisclosed)).queue();
              RequestContext rc = RequestContext.getCurrentInstance();

              // Don't force PPR on for the browsers that do not support Ajax
              Object cap = rc.getAgent().getCapabilities().get(TrinidadAgent.CAP_PARTIAL_RENDERING);
              if ((cap != null) && (Boolean.TRUE.equals(cap)))
              {
                RequestContext.getCurrentInstance().addPartialTarget(component);
                PartialPageUtils.forcePartialRendering(facesContext);
              }

              break;
            }
          }
        }
      }
    }
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  /**
   *  First: If nothing is disclosed, makes the first child disclosed.
   *
   *  Makes sure that the child being disclosed has rendered = true
   *  and is not disabled.
   *
   *  Second: Renders a vertical panel bar and children in individual panels.
   *
   *  For the panel bar, draws a DIV that forms the outline of panels.
   *  Within the DIV, iteratively calls the encodeBegin, encodeChildren and
   *  encodeEnd on the panel children (if they have rendered and disclosed
   *  set to true).
   *
   *  Non UIXShowDetail children are ignored.
   *  The title of each of the panels is the same as the text assigned to
   *  UIXShowDetail child. When text attribute is not specified,
   *  title remains blank.
   * @param context the faces context object
   * @param component the UIComponent object
   * @throws IOException when some issues while writing output
   */
  @SuppressWarnings("unchecked")
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    FormData fData = rc.getFormData();
    String formName = "";

    if (fData != null)
    {
      formName = fData.getName();
      if (formName == null)
      {
        _LOG.warning("PANELACCORDION_MUST_INSIDE_FORM");
        return;
      }
      // Hidden field to store parameter targetItem is needed for non
      // Ajax browsers to pass the target item Id back to the server.
      boolean pprEnabled =
        PartialPageUtils.supportsPartialRendering(rc);
      if (!pprEnabled)
      {
        fData.addNeededValue(XhtmlConstants.TARGETITEM_PARAM);
      }
    }

    List<UIComponent> children = component.getChildren();
    int numChildren = children.size();
    UIComponent disclosedChild = null;
    UIXShowDetail renderableChild = null;

    for (int indxChild = 0; indxChild < numChildren ; indxChild++ )
    {
      UIComponent child =  children.get(indxChild);
      if (! (child instanceof UIXShowDetail) )
      {
        continue;
      }

      UIXShowDetail detailChild =  (UIXShowDetail) children.get(indxChild);

      if (detailChild.isRendered())
      {
        // Mark the first renderable child
        if (_isItemDisabled(detailChild))
        {
          continue;
        }
        if (renderableChild == null)
        {
          renderableChild = detailChild;
        }
        if (detailChild.isDisclosed())
        {
          disclosedChild = detailChild;
          // A diclosed child found. return.
          break;
        }
      }
    }

    // If we have a minimum of 1 disclosed child and none have been disclosed
    // yet, disclose the first rendered one:
    if ( (disclosedChild == null) && !getDiscloseNone(component, bean) &&
      (renderableChild != null) && !renderableChild.isDisclosedTransient())
    {
      renderableChild.setDisclosed(true);
    }

    ResponseWriter out = context.getResponseWriter();
    String compId = component.getClientId(context);

    out.startElement("div", component);

    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);

    boolean discloseMany = getDiscloseMany(component, bean);
    boolean discloseNone = getDiscloseNone(component, bean);
    boolean disclosedFixed = false;
    if (discloseMany && !discloseNone) // must keep at least one item disclosed
    {
      // This is a special case where we must determine if we have to fix the
      // disclosure state of one of the items.
      int disclosedCount = 0;
      for (UIComponent child : (List<UIComponent>) component.getChildren())
      {
        if (!(child instanceof UIXShowDetail) ||
            !child.isRendered())
          continue;

        UIXShowDetail detailItem = (UIXShowDetail) child;
        if (detailItem.isDisclosed())
        {
          disclosedCount++;
          if (disclosedCount > 1)
          {
            break; // we have enough information at this point to stop counting
          }
        }
      }
      if (disclosedCount <= 1)
      {
        disclosedFixed = true;
      }
    }

    boolean childAlreadyRendered = false;
    for (UIComponent child : (List<UIComponent>) component.getChildren())
    {
      if (!(child instanceof UIXShowDetail) ||
          !child.isRendered())
        continue;

      UIXShowDetail detailItem = (UIXShowDetail) child;
      boolean disabled = _isItemDisabled(detailItem);
      String titleText = (String)
        detailItem.getAttributes().get(CoreShowDetailItem.TEXT_KEY.getName());
      boolean disclosed = detailItem.isDisclosed();

      if (childAlreadyRendered)
      {
        // The detail child should be disclosed only when all three criteria met
        // 1. is marked as disclosed
        // 2. is not disabled and
        // 3. if a child is not already disclosed. This occurs when more than
        //    one showDetail child has it's disclosed property set to true.
        disclosed = false;
      }

      // Header renderer section.
      out.startElement("div", detailItem);

      String detailItemId = detailItem.getClientId(context);
      String itemStyleClass;
      if (disabled)
        itemStyleClass = getHeaderDisabledStyleClass();
      else if (disclosed)
        itemStyleClass = getHeaderExpandedStyleClass();
      else
        itemStyleClass = getHeaderCollapsedStyleClass();

      renderStyleClass(context, rc, itemStyleClass);

      // Render the toolbar component, if any (we use float to keep
      // the toolbar on the right - or left, in RTL languages - so
      // it has to be rendered first)
      UIComponent toolbar = getFacet(detailItem,
                                     CoreShowDetailItem.TOOLBAR_FACET);
      if (toolbar != null)
      {
        out.startElement("div", detailItem);
        renderStyleClass(context, rc, SkinSelectors.AF_PANELACCORDION_TOOLBAR_STYLE_CLASS);
        encodeChild(context, toolbar);
        out.endElement("div");
      }

      boolean javaScriptSupport = supportsScripting(rc);

      if (javaScriptSupport)
      {
        out.startElement("a", null);
        out.writeAttribute("name", detailItemId, null);
      }
      else
      {
        // For Non-JavaScript browsers, render an input element(type=submit) to
        // submit the page. Encode the name attribute with the parameter name
        // and value thus it would enable the browsers to include the name of
        // this element in its payLoad if it submits the page.
        out.startElement("input", null);
        out.writeAttribute("type", "submit", null);
      }

      renderStyleClass(context, rc,
                       disabled
                         ? getLinkDisabledStyleClass()
                         : getLinkEnabledStyleClass());

      // If the child is disclosable and enabled...
      boolean disclosable =
        discloseNone || (! disclosed) || (discloseMany && !disclosedFixed);
      if ( disclosable && (! disabled) )
      {
        boolean isImmediate = detailItem.isImmediate();
        String event = disclosed ? "hide" : "show";

        if (javaScriptSupport)
        {
          String onClickHandler = _getFormSubmitScript(component,
                                                       rc,
                                                       event,
                                                       detailItemId,
                                                       formName,
                                                       compId,
                                                       isImmediate);
          out.writeAttribute("onclick", onClickHandler, null);
          out.writeAttribute("href", "#", null);
        }
        else
        {
          String nameAttri = XhtmlUtils.getEncodedParameter
                                          (XhtmlConstants.SOURCE_PARAM)
                             + XhtmlUtils.getEncodedParameter(compId)
                             + XhtmlUtils.getEncodedParameter
                                         (XhtmlConstants.EVENT_PARAM)
                             + XhtmlUtils.getEncodedParameter(event)
                             + XhtmlUtils.getEncodedParameter
                                         (XhtmlConstants.TARGETITEM_PARAM)
                             + detailItemId;

          out.writeAttribute("name", nameAttri, null);
        }
      }

      if (javaScriptSupport)
      {
        // =-=rbaranwa Per the UI Review, no icon to be rendered when
        // panel is disabled.
        if (! disabled)
        {
          ShowDetailRenderer.renderDisclosureIcon(context,
                                                   rc,
                                                   disclosed,
                                                   getDisclosedTipKey(),
                                                   getUndisclosedTipKey());
        }
        if (titleText != null)
        {
          out.writeText(titleText, null);
        }
        out.endElement("a");
      }
      else
      {
        // Since we cannot render any image element as a child of input element,
        // just render the icon symbol along with the text.
        String icon = disclosed ? XhtmlConstants.NON_JS_DETAIL_DISCLOSED_ICON :
                                  XhtmlConstants.NON_JS_DETAIL_UNDISCLOSED_ICON;
        if (titleText != null)
        {
          icon = icon + titleText;
        }

        out.writeAttribute("value", icon, null);

        if (disabled || !disclosable)
        {
          out.writeAttribute("disabled", Boolean.TRUE, "disabled");
        }

        out.endElement("input");
      }

      out.endElement("div"); // Ending div for an individual panel


      // The detail child should be disclosed only when all three criteria met
      // 1. is marked as disclosed
      // 2. is not disabled and
      // 3. if a child is not already disclosed. This occurs when more than
      //    one showDetail child has it's disclosed property set to true.
      if (disclosed && (! disabled) && (! childAlreadyRendered) )
      {
        _encodeDetailItem(context, rc, component, detailItem, out);
        if (!discloseMany)
        {
          childAlreadyRendered = true;
        }
      }
    }
    out.endElement("div");
  }

  @Override
  protected String getDefaultStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return SkinSelectors.AF_PANELACCORDION_STYLE_CLASS;
  }

  protected String getContentStyleClass()
  {
    return SkinSelectors.AF_PANELACCORDION_CONTENT_STYLE_CLASS;
  }

  protected String getHeaderDisabledStyleClass()
  {
    return SkinSelectors.AF_PANELACCORDION_HEADER_DISABLED_STYLE_CLASS;
  }

  protected String getHeaderExpandedStyleClass()
  {
    return SkinSelectors.AF_PANELACCORDION_HEADER_EXPANDED_STYLE_CLASS;
  }

  protected String getHeaderCollapsedStyleClass()
  {
    return SkinSelectors.AF_PANELACCORDION_HEADER_COLLAPSED_STYLE_CLASS;
  }

  protected String getLinkDisabledStyleClass()
  {
    return SkinSelectors.AF_PANELACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS;
  }

  protected String getLinkEnabledStyleClass()
  {
    return SkinSelectors.AF_PANELACCORDION_TITLE_LINK_STYLE_CLASS;
  }

  protected String getDisclosedTipKey()
  {
    return _DISCLOSED_TIP_KEY;
  }

  protected String getUndisclosedTipKey()
  {
    return _UNDISCLOSED_TIP_KEY;
  }

  /**
   *  Encodes the disclosed child.
   *
   *  Generates the required markup for the disclosed child.
   *
   * @param context the faces context object
   * @param component the UIComponent object
   * @param detailItem the UIXShowDetailItem thats disclosed
   * @param out the response writer object
   * @throws IOException when some issues while writing output
   */
  private void _encodeDetailItem(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    UIXShowDetail    detailItem,
    ResponseWriter   out
    ) throws IOException
  {
    out.startElement("table", component);
    out.writeAttribute("cellSpacing", "0", null);
    out.writeAttribute("cellPadding", "0", null);
    out.writeAttribute("summary", "", null);

    renderStyleClass(context, rc, getContentStyleClass());

    out.startElement("tr", component);
    out.startElement("td", component);

    encodeChild(context, detailItem);

    out.endElement("td");
    out.endElement("tr");

    out.endElement("table"); // Ending table for the contained child
  }

  /**
   *  Creates javascript used to submit the page.
   */
  private String _getFormSubmitScript(
    UIComponent      component,
    RenderingContext rc,
    String           event,
    String           detailItemId,
    String           formName,
    String           compId,
    boolean          isImmediate)
  {
    // Check if PPR enabled, do a _submitPartialChange, else do a formSubmit.
    String onClickHandler = "";
    boolean pprEnabled =
      PartialPageUtils.supportsPartialRendering(rc);

    String validate = "1";
    if (isImmediate)
    {
      validate = "0";
    }

    if (pprEnabled)
    {
      StringBuilder onClickHandlerBuff =
            new StringBuilder("_submitPartialChange('")
            .append(formName)
            .append("',")
            .append(validate)
            .append(", {event:'")
            .append(event)
            .append("',source:'")
            .append(compId)
            .append("',targetItem:'")
            .append(detailItemId)
            .append("'});return false;");

      onClickHandler = onClickHandlerBuff.toString();
    }
    else
    {
      StringBuilder onClickHandlerBuff = new StringBuilder("submitForm('")
                                  .append(formName)
                                  .append("',")
                                  .append(validate)
                                  .append(", {event:'")
                              .append(event)
                              .append("',source:'")
                              .append(compId)
                              .append("',targetItem:'")
                              .append(detailItemId)
                                  .append("'});return false;");

      onClickHandler = onClickHandlerBuff.toString();
    }
    return onClickHandler;
  }

  protected boolean getDiscloseMany(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_discloseManyKey);
    if (o == null)
      o = _discloseManyKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean getDiscloseNone(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_discloseNoneKey);
    if (o == null)
      o = _discloseNoneKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  private boolean _isItemDisabled(
    UIComponent component)
  {
    Object isDisabled = component.getAttributes().get(
      CoreShowDetailItem.DISABLED_KEY.getName());
    return Boolean.TRUE.equals(isDisabled);
  }

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(PanelAccordionRenderer.class);

  private PropertyKey _discloseNoneKey;
  private PropertyKey _discloseManyKey;

  private static final String _DISCLOSED_TIP_KEY =
    "af_panelAccordion.DISCLOSED_TIP";
  private static final String _UNDISCLOSED_TIP_KEY =
    "af_panelAccordion.UNDISCLOSED_TIP";
}