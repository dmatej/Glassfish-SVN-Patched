/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelPopup;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;


/**
 * @author Danny Robinson
 */
public class PanelPopupRenderer extends XhtmlRenderer
{
  public PanelPopupRenderer()
  {
    this(CorePanelPopup.TYPE);
  }

  protected PanelPopupRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _iconKey = type.findKey("icon");
    _titleKey = type.findKey("title");
    _triggerTypeKey = type.findKey("triggerType");
    _positionKey = type.findKey("position");
    _modalKey = type.findKey("modal");
    _contentStyleKey = type.findKey("contentStyle");
    _widthKey = type.findKey("width");
    _heightKey = type.findKey("height");
    _horzOffsetKey = type.findKey("xoffset");
    _vertOffsetKey = type.findKey("yoffset");
    _triggerRenderer = new TriggerRenderer();
  }

  protected String getText(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(resolveProperty(bean, _textKey));
  }

  protected String getIcon(
    UIComponent component,
    FacesBean   bean)
  {
    return toResourceUri(FacesContext.getCurrentInstance(), resolveProperty(bean, _iconKey));
  }

  protected String getTitle(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(resolveProperty(bean, _titleKey));
  }

  protected String getTriggerType(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(resolveProperty(bean, _triggerTypeKey, true));
  }

  protected String getPosition(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(resolveProperty(bean, _positionKey));
  }

  protected String getContentStyle(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(resolveProperty(bean, _contentStyleKey));
  }

  protected int getWidth(
    UIComponent component,
    FacesBean   bean)
  {
    return toInt(resolveProperty(bean, _widthKey, true));
  }

  protected int getHeight(
    UIComponent component,
    FacesBean   bean)
  {
    return toInt(resolveProperty(bean, _heightKey, true));
  }

  protected int getHorzOffset(
    UIComponent component,
    FacesBean   bean)
  {
    return toInt(resolveProperty(bean, _horzOffsetKey, true));
  }

  protected int getVertOffset(
    UIComponent component,
    FacesBean   bean)
  {
    return toInt(resolveProperty(bean, _vertOffsetKey, true));
  }

  protected boolean isModal(
    UIComponent component,
    FacesBean   bean)
  {
    return (Boolean)resolveProperty(bean, _modalKey, true);
  }

  protected boolean isCentered(
    UIComponent component,
    FacesBean   bean)
  {
    String centeredString = toString(resolveProperty(bean, _positionKey));
    return CorePanelPopup.POSITION_CENTERED.equalsIgnoreCase(centeredString);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    //HKuhn - in printable mode (scripting is disabled) we need only the trigger
    if (!supportsScripting(rc))
    {
      renderTrigger(context, rc, component, bean);
      return;
    }

    ResponseWriter writer = context.getResponseWriter();
    String clientId = getClientId(context, component);

    // Make sure we don't have anything to save
    assert(rc.getCurrentClientId() == null);
    // Set current clientId so we can access this in getOnclick
    rc.setCurrentClientId(clientId);

    // Render the outer span that is the component container,
    // this element is rendered so the component can be updated via ppr.
    writer.startElement(XhtmlConstants.SPAN_ELEMENT, component);
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE + _POPUP_TRIGGER_ID_SUFFIX,
        XhtmlUtils.getJSIdentifier(clientId), null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_POPUP_TRIGGER_STYLE_CLASS);

    renderTrigger(context, rc, component, bean);

    // Render the outer div that is the actual popup container
    writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, XhtmlUtils.getJSIdentifier(clientId)
        + _POPUP_CONTAINER_ID_SUFFIX, null);

    // Output the non-modifiable styles the keep the popup hidden initially
    writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
        _POPUP_CONTAINER_DIV_STYLES, null);

    // Render the skinnable container div
    writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_POPUP_CONTAINER_STYLE_CLASS);
    renderInlineStyle(context, rc, component, bean);

    renderTitleBar(context, rc, component, bean);

    // Render the child components in a content div
    writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_POPUP_CONTENT_STYLE_CLASS);

    // If width is set, then add that style to the content
    int height = getHeight(component, bean);
    if (height > 0)
    {
      String style = "overflow: auto; height:" + height + "px";
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, style, null);
    }

    encodeAllChildren(context, component);

    // Close skinnable content div
    writer.endElement(XhtmlConstants.DIV_ELEMENT);

    // Close skinnable container div
    writer.endElement(XhtmlConstants.DIV_ELEMENT);

    // Close outer show/hide div
    writer.endElement(XhtmlConstants.DIV_ELEMENT);

    // Close outer span
    writer.endElement(XhtmlConstants.SPAN_ELEMENT);

    // Reset current clientId
    rc.setCurrentClientId(null);
  }

  protected void renderTrigger(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Render the trigger, including the facet if specified
    delegateRendererBegin(context, rc, component, bean, _triggerRenderer);

    UIComponent triggerFacet = getFacet(component,
        CorePanelPopup.TRIGGER_FACET);
    if (triggerFacet != null)
      encodeChild(context, triggerFacet);

    //render trigger icon
    OutputUtils.renderImage(context, rc, getIcon(component, bean), null, null, null, "",
                            component, null, SkinSelectors.AF_PANEL_POPUP_ICON_STYLE_CLASS);

    delegateRendererEnd(context, rc, component, bean, _triggerRenderer);
  }

  protected void renderTitleBar(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    String title = getTitle(component, bean);
    if (title == null)
      return;

    // Render the skinnable title bar div
    writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_POPUP_TITLEBAR_STYLE_CLASS);

    // Render the skinnable title text
    writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_POPUP_TITLE_STYLE_CLASS);
    writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, "float:left;", null);

    writer.writeText(title, "title");

    writer.endElement(XhtmlConstants.DIV_ELEMENT);

    Icon icon = rc.getIcon(SkinSelectors.AF_PANEL_POPUP_CLOSE_ICON_STYLE_CLASS);

    if (isModal(component, bean))
    {
      // Render the skinnable container div
      writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
      renderStyleClass(context, rc, SkinSelectors.AF_PANEL_POPUP_CLOSE_ICON_STYLE_CLASS);

      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);

      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE,
          "TrPanelPopup.hidePopup(event); return false;",
          null);

      if (icon != null && !icon.isNull())
      {
        String closeText = rc.getTranslatedString("af_panelPopup.CLOSE");
        OutputUtils.renderIcon(context, rc, icon, closeText, null);
      }
      else
      {
        writer.writeText("X", "text");
      }
      writer.endElement(XhtmlConstants.LINK_ELEMENT);

      writer.endElement(XhtmlConstants.DIV_ELEMENT);
    }

    // Render an empty div to terminate title bar layout
    writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, "clear: left;", null);
    writer.endElement(XhtmlConstants.DIV_ELEMENT);

    // Close the outer titlebar div
    writer.endElement(XhtmlConstants.DIV_ELEMENT);

  }

  // Handles rendering of the trigger text/icon and or
  private class TriggerRenderer extends GoLinkRenderer
  {
    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      // Force rendering of the ID, so the trigger element can be found through scripting
      return true;
    }

    @Override
    protected String getClientId(
      FacesContext context,
      UIComponent  component)
    {
      // Ensure the ID is encoded, otherwise it will fail when included in tables.
      return XhtmlUtils.getJSIdentifier(
          PanelPopupRenderer.this.getClientId(context, component));
    }

    @Override
    protected String getOnclick(
      UIComponent component,
      FacesBean   bean)
    {
      String onclick = PanelPopupRenderer.this.getOnclick(component, bean);
      String script = null;

      // Only render onclick script for 'click'
      if (CorePanelPopup.TRIGGER_TYPE_CLICK.equalsIgnoreCase(getTriggerType(
        component, bean)))
      {
        String componentId = RenderingContext.getCurrentInstance().getCurrentClientId();

        script = getTriggerScript(component, bean, componentId);
      }

      return XhtmlUtils.getChainedJS(onclick, script, true);
    }

    @Override
    protected String getOnmouseover(
      UIComponent component,
      FacesBean   bean)
    {
      String onclick = super.getOnmouseover(component, bean);
      String script = null;

      // Only render onclick script for 'click'
      if (CorePanelPopup.TRIGGER_TYPE_HOVER.equalsIgnoreCase(getTriggerType(component, bean)))
      {
        String componentId = RenderingContext.getCurrentInstance().getCurrentClientId();

        script = getTriggerScript(component, bean, componentId);
      }

      return XhtmlUtils.getChainedJS(onclick, script, true);
    }

    @Override
    protected String getText(
      UIComponent component,
      FacesBean   bean)
    {
      return PanelPopupRenderer.this.getText(component, bean);
    }

    @Override
    protected String getInlineStyle(
      UIComponent component,
      FacesBean   bean)
    {
      return PanelPopupRenderer.this.getInlineStyle(component, bean);
    }

    @Override
    protected String getStyleClass(
      UIComponent component,
      FacesBean   bean)
    {
      return PanelPopupRenderer.this.getStyleClass(component, bean);
    }

    @Override
    protected String getDefaultStyleClass(
      UIComponent component,
      FacesBean   bean)
    {
      return SkinSelectors.AF_PANEL_POPUP_LINK_STYLE_CLASS;
    }

    protected String getTriggerScript(
      UIComponent component,
      FacesBean   bean,
      String      componentId)
    {
      String clientId = XhtmlUtils.getJSIdentifier(componentId);
      StringBuilder script = new StringBuilder();

      script.append("TrPanelPopup.showPopup('");
      script.append(clientId);
      script.append(_POPUP_CONTAINER_ID_SUFFIX);
      script.append("', '");
      script.append(clientId);
      script.append("', event, '");
      script.append(PanelPopupRenderer.this.getTriggerType(component, bean));
      script.append("','");
      script.append(PanelPopupRenderer.this.getPosition(component, bean));
      script.append("',");
      script.append(PanelPopupRenderer.this.isModal(component, bean));
      script.append(",");
      script.append(PanelPopupRenderer.this.getWidth(component, bean));
      script.append(",");
      script.append(PanelPopupRenderer.this.getHeight(component, bean));
      script.append(",");
      script.append(PanelPopupRenderer.this.getHorzOffset(component, bean));
      script.append(",");
      script.append(PanelPopupRenderer.this.getVertOffset(component, bean));

      script.append("); return false;");

      return script.toString();
    }
  }

  private static final TrinidadLogger _LOG = TrinidadLogger
      .createTrinidadLogger(PanelPopupRenderer.class);

  private PropertyKey _textKey;
  private PropertyKey _titleKey;
  private PropertyKey _triggerTypeKey;
  private PropertyKey _positionKey;
  private PropertyKey _modalKey;
  private PropertyKey _contentStyleKey;
  private PropertyKey _widthKey;
  private PropertyKey _heightKey;
  private PropertyKey _horzOffsetKey;
  private PropertyKey _vertOffsetKey;
  private XhtmlRenderer _triggerRenderer;

  private PropertyKey _iconKey;
  private static final String _POPUP_CONTAINER_ID_SUFFIX = "_popupContainer";

  private static final String _POPUP_TRIGGER_ID_SUFFIX = "_popupTrigger";

  /**
   * styles for container element that is shown/hidden. User can't style this
   * element, but there's an inner element for skin styling (e.g border,
   * padding, etc.).
   */
  private static final String _POPUP_CONTAINER_DIV_STYLES = "position:absolute; top:0px; left:0px; visibility:hidden;";

}
