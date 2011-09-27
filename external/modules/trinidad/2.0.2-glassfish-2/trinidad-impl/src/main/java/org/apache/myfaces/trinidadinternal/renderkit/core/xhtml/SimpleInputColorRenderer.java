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

import java.awt.Color;

import java.io.IOException;

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreInputColor;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.RenderUtils;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.convert.ColorConverter;
import org.apache.myfaces.trinidadinternal.renderkit.core.pages.GenericEntry;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.AliasedScriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.ColorFieldInfoScriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.ConfigurationScriptlet;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;


/**
 * @todo Resolve ALT keys
 * @todo is SkinSelectors.AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_STYLE_CLASS used anywhere?
 */
public class SimpleInputColorRenderer
  extends SimpleInputListOfValuesRenderer
{
  public SimpleInputColorRenderer()
  {
    this(CoreInputColor.TYPE);
  }

  public SimpleInputColorRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _compactKey = type.findKey("compact");
    _chooseIdKey = type.findKey("chooseId");
  }

  @Override
  protected void queueActionEvent(
    FacesContext context,
    UIComponent  component)
  {
    FacesBean bean = getFacesBean(component);
    // If there's a non-default action, then just launch away
    if (getActionExpression(component, bean) != null)
    {
      super.queueActionEvent(context, component);
    }
    // Otherwise, we'll fall back to launching the default dialog
    // (This should only happen on devices without support for
    // custom windows - everything else would have just launched
    // a calendar window with the _ldp JS function)
    else
    {
      Object submittedValue = getSubmittedValue(component, bean);
      try
      {
        getConvertedValue(context, component, submittedValue);
      }
      // Not a big deal;  just means that an invalid value was entered,
      // so we'll launch the dialog showing nothing
      catch (ConverterException ce)
      {
        _LOG.fine(ce);
      }


      RequestContext afContext = RequestContext.getCurrentInstance();

      // =-=AEW Parameters?  Shouldn't we pass in the color?
      Map<String, Object> parameters = null;
      afContext.launchDialog(GenericEntry.getGenericEntryViewRoot(context),
                             parameters,
                             component,
                             true,
                             null);
    }
  }


  /**
   * Give subclasses a chance to override the ReturnEvent.
   */
  @Override
  protected void queueReturnEvent(
    FacesContext context,
    UIComponent  component,
    ReturnEvent  event)
  {
    Object returnValue = event.getReturnValue();

    // If we got passed a Color object, send it back to String
    // land (where it needs to be for submitted values).
    if (returnValue instanceof Color)
    {
      FacesBean bean = getFacesBean(component);
      Converter converter = getConverter(component, bean);
      if (converter == null)
        converter = getDefaultConverter(context, component, bean);

      if (converter != null)
      {
        returnValue = converter.getAsString(context,
                                            component,
                                            returnValue);
      }
      else
      {
        returnValue = returnValue.toString();
      }

      event = new ReturnEvent(component,
                              returnValue,
                              event.getReturnParameters());
    }

    event.queue();
  }

  @Override
  protected void encodeAllAsElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (!_colorScriptletsRegistered)
    {
        ColorFieldInfoScriptlet.sharedInstance().registerSelf();
        (new AliasedScriptlet(_COLOR_FIELD_LIB, null,
                              new String[]{"openWindow()",
                                           "_getColorFieldFormat()",
                                           ColorFieldInfoScriptlet.COLOR_FIELD_INFO_KEY
                                           })).registerSelf();
        _colorScriptletsRegistered = true;
    }
    String chooseId = _computeChooseId(context, component, bean);
    rc.getProperties().put(_CACHED_CHOOSE_ID, chooseId);

    // Add the scriptlets required by the color field
    XhtmlUtils.addLib(context, rc, "_fixCFF()");
    XhtmlUtils.addLib(context, rc, _COLOR_FIELD_LIB);
    super.encodeAllAsElement(context, rc, component, bean);

    if (!getDisabled(component, bean))
    {
      // =-=AEW addOnSubmitConverterValidators() when compact???
      _renderFirstColorFieldScript(context, rc, component, _getChooseId(rc));
    }

    rc.getProperties().remove(_CACHED_CHOOSE_ID);
  }

  @Override
  protected void renderTextField(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (!isCompact(component, bean) ||
        !_supportsSwatchAndChooser(rc))
    {
      super.renderTextField(context, rc, component, bean);
    }
    else
    {
      if (isAutoSubmit(component, bean))
        AutoSubmitUtils.writeDependencies(context, rc);
      addOnSubmitConverterValidators(context, rc, component, bean);

      // In compact mode, write out a hidden field that is
      // the stub
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("input", component);
      writer.writeAttribute("type", "hidden", null);
      writer.writeAttribute("value",
                            getConvertedString(context,
                                               component,
                                               bean),
                            "value");
      String id = rc.getCurrentClientId();
      writer.writeAttribute("id", id, null);
      writer.writeAttribute("name", id, null);
      writer.endElement("input");
    }
  }

  @Override
  protected void renderContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          renderAsElement,
    boolean          isTextArea
    ) throws IOException
  {
    // Hook here to make sure we're inside the <span>
    assert(!isTextArea);
    super.renderContent(context, rc, component, bean,
                        renderAsElement, isTextArea);
    if (!renderAsElement)
    {
      renderAfterTextField(context, rc, component, bean);
    }
  }

  @Override
  protected void renderAfterTextField(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (!_supportsSwatchAndChooser(rc))
      return;

    if (!isCompact(component, bean))
    {
      // =-=AEW TODO: Make spacer a property?
      renderSpacer(context, rc, "8", "1");
      renderIcon(context, rc, component, bean);
    }
    else
    {
      renderIcon(context, rc, component, bean);
    }
  }

  @Override
  protected void renderIcon(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    boolean isEditable = ((_getChooseId(rc) == null) &&
                          !getDisabled(component, bean) &&
                          !getReadOnly(context, component, bean));
    if (isEditable)
    {
      XhtmlUtils.addLib(context,
                        rc,
                        ConfigurationScriptlet.sharedInstance().getScriptletKey());
    }

    _renderColorSwatch(context, rc, component, bean, isEditable);
  }

  /**
   * Return a default converter.
   */
  @Override
  protected Converter getDefaultConverter(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    return _DEFAULT_CONVERTER;
  }

  @Override
  protected String getLaunchOnclick(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // If the field has an action, use the default behavior.  Or,
    // if the field doesn't support launching a window at all,
    // use the default behavior.
    if ((getActionExpression(component, bean) != null) ||
        !Boolean.TRUE.equals(
            rc.getAgent().getCapabilities().get(TrinidadAgent.CAP_MULTIPLE_WINDOWS)))
      return super.getLaunchOnclick(context, rc, component, bean);

    String id = rc.getCurrentClientId();
    if ((id == null) || (rc.getFormData() == null))
      return null;

    // we want something big enough
    StringBuffer onClickBuffer = new StringBuffer(100);

    onClickBuffer.append("_lcp('");
    onClickBuffer.append(rc.getFormData().getName());
    onClickBuffer.append("','");

    onClickBuffer.append(id);
    onClickBuffer.append("'); return false");

    return onClickBuffer.toString();
  }

  @Override
  protected String getOnfocus(
    UIComponent component,
    FacesBean   bean)
  {
    String onfocus = super.getOnfocus(component, bean);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    if (!_supportsSwatchAndChooser(arc))
      return onfocus;

    String chooseId = _getChooseId(arc);
    // The special _dff handler is only needed for date fields
    // connected to a chooser;  the blur handler is needed all the time.
    if (chooseId != null)
    {
      int length = _FOCUS_PREFIX.length() + _FOCUS_SUFFIX.length() + chooseId.length();

      StringBuffer buffer = new StringBuffer(length);
      buffer.append(_FOCUS_PREFIX);
      buffer.append(chooseId);
      buffer.append(_FOCUS_SUFFIX);

      return XhtmlUtils.getChainedJS(buffer.toString(), onfocus, false);
    }
    else
    {
      return onfocus;
    }
  }

  @Override
  protected String getOnblur(
    UIComponent component,
    FacesBean   bean)
  {
    String onblur = super.getOnblur(component, bean);
    if (!_supportsSwatchAndChooser(RenderingContext.getCurrentInstance()))
      return XhtmlUtils.getChainedJS(_AUTO_FORMAT_SCRIPT,
                                     onblur, false);


    return XhtmlUtils.getChainedJS(_AUTO_FORMAT_SCRIPT + _AUTO_SWATCH_SCRIPT,
                                   onblur, false);
  }

  @Override
  protected Integer getDefaultColumns(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    Integer columnsInteger = null;

    Converter converter = getConverter(component, bean);

    // Ignoring the "default" converter code is intentional;  we'll just
    // fall through to _DEFAULT_COLUMNS here to save time
    if (converter instanceof ColorConverter)
    {
      int columns = ((ColorConverter) converter).getColumns(FacesContext.getCurrentInstance());
      columnsInteger = columns;
    }
    else
    {
      columnsInteger = _DEFAULT_COLUMNS;
    }

    return columnsInteger;
  }

  @Override
  protected Number getMaximumLength(
    UIComponent component,
    FacesBean   bean)
  {
    // Not supported for selectInputColor
    // =-=AEW We could have a good default
    return null;
  }

  @Override
  protected String getButtonIconName()
  {
    return SkinSelectors.AF_SELECT_INPUT_COLOR_LAUNCH_ICON_NAME;
  }

  protected String getChooseId(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_chooseIdKey));
  }

  protected boolean isCompact(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_compactKey);
    if (o == null)
      o = _compactKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputColor";
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputColor::content";
  }

  private String _getChooseId(
    RenderingContext rc)
  {
    return (String) rc.getProperties().get(_CACHED_CHOOSE_ID);
  }

  private String _computeChooseId(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    return RenderUtils.getRelativeId(context,
                                     component,
                                     getChooseId(component, bean));

  }

  // On PDAs, we only support a simple text field
  private boolean _supportsSwatchAndChooser(
    RenderingContext rc)
  {
    return (!isPDA(rc));
  }

  @Override
  protected String getSearchDesc(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    if (isInaccessibleMode(arc))
      return null;

    return arc.getTranslatedString(_LAUNCH_PICKER_TIP_KEY);
  }

  private String _getColorSwatchId(
    RenderingContext rc)
  {
    return rc.getCurrentClientId() + "$sw";
  }

  private void _renderColorSwatch(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          editable
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    if (editable)
    {
      writer.startElement("a", component);
      writer.writeAttribute("onclick",
                            getLaunchOnclick(context, rc, component, bean),
                            null);
      writer.writeURIAttribute("href", "#", null);
    }

    writer.startElement("img", component);

    String id = _getColorSwatchId(rc);

    // Render the ID directly on the swatch image.
    writer.writeAttribute("id", id, null);

    // Render other image attrs
    writer.writeAttribute("align",
                          isScreenReaderMode(rc) ? "middle" : "absmiddle",
                          null);
    writer.writeAttribute("width", _CELL_SIZE, null);
    writer.writeAttribute("height", _CELL_SIZE, null);

    Color color = _getColorValue(component, bean);

    // Render the style attributes
    String inlineStyle = _getInlineStyleForColor(color);
    renderStyleClass(context, rc, SkinSelectors.COLOR_FIELD_SWATCH_STYLE_CLASS);
    writer.writeAttribute("style", inlineStyle, null);

    Object altText = null;

    if (color != null && color.getAlpha() == 0)
    {
      Icon icon = rc.getIcon(XhtmlConstants.COLOR_PALETTE_TRANSPARENT_ICON_NAME);
      if (icon != null)
      {
        // FIXME: this should happen with just rendering the Icon, *not*
        // by extracting the URI
        renderEncodedResourceURI(context, "src", icon.getImageURI(context, rc));
      }

      String key = editable ?
                    "af_inputColor.LAUNCH_PICKER_TIP" :
                    "af_chooseColor.TRANSPARENT";
      altText = rc.getTranslatedString(key);
    }
    else
    {
      String transparentURI = getBaseImageUri(context, rc) + TRANSPARENT_GIF;
      renderEncodedResourceURI(context, "src", transparentURI);

      if (editable)
        altText = rc.getTranslatedString("af_inputColor.LAUNCH_PICKER_TIP");
      else
        altText = "";
    }

    OutputUtils.renderAltAndTooltipForImage(context, rc, altText);

    writer.endElement("img");

    if (editable)
    {
      Icon overlay = rc.getIcon(SkinSelectors.AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_NAME);
      if (overlay != null)
        OutputUtils.renderIcon(context, rc, overlay, "", "middle");
      writer.endElement("a");
    }
  }


  // @todo Should this deal with submittedValue, and String values?
  private Color _getColorValue(
    UIComponent component,
    FacesBean   bean)
  {
    Object value = getValue(component, bean);
    if (value instanceof Color)
      return ((Color) value);
    return null;
  }

  // Checks to see whether this is the first color field for
  // a given chooseId, and if so renders a script
  private void _renderFirstColorFieldScript(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    String           chooseId
    ) throws IOException
  {
    if (chooseId == null)
      return;

    if (!_supportsSwatchAndChooser(rc))
      return;

    if (rc.getFormData() == null)
      return;

    String id = getClientId(context, component);

    @SuppressWarnings("unchecked")
    Map<String, Boolean> chooseColorIds = (Map<String, Boolean>)
      rc.getProperties().get(_CHOOSE_COLOR_IDS_KEY);

    if (chooseColorIds == null)
    {
      chooseColorIds = new HashMap<String, Boolean>();
      rc.getProperties().put(_CHOOSE_COLOR_IDS_KEY, chooseColorIds);
    }

    // The first dateField that is rendered for each inlineDatePicker
    // is the "active" dateField.  Check to see if we already
    // have an active dateField for this inlineDatePicker.
    if (chooseColorIds.get(chooseId) == null)
    {
      // We don't already have an active selectInputColor, so
      // this one is it.  Render the script to activate
      // this selectInputColor.
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", component);
      renderScriptTypeAttribute(context, rc);
      renderScriptDeferAttribute(context, rc);

      writer.writeText("_cfBus['", null);
      writer.writeText(chooseId, null);
      writer.writeText("']=document.forms['", null);
      writer.writeText(rc.getFormData().getName(), null);
      writer.writeText("']['", null);
      writer.writeText(id, null);
      writer.writeText("'];", null);
      writer.endElement("script");

      // Mark the ChooseColor as having its script
      chooseColorIds.put(chooseId, Boolean.TRUE);
    }
  }

  private static String _getInlineStyleForColor(
    Color color)
  {
    if (color != null && color.getAlpha() > 0)
      return "background-color:" + CSSUtils.getColorValue(color);

    return null;
  }

  private PropertyKey _chooseIdKey;
  private PropertyKey _compactKey;

  private static final String _COLOR_FIELD_LIB = "ColorField";
  private static boolean _colorScriptletsRegistered = false;
  private static final Integer _DEFAULT_COLUMNS = 11;

  // AdfRenderingContext property key for the Map which tracks whether
  // a ChooseColor id has been encountered
  private static final Object _CHOOSE_COLOR_IDS_KEY = new Object();

  private static final String _LAUNCH_PICKER_TIP_KEY =
    "af_inputColor.SELECT_PICKER_ALT";

  // Script for onblur auto-formatting
  private static final String _AUTO_SWATCH_SCRIPT = "_cfsw(this);";
  private static final String _AUTO_FORMAT_SCRIPT = "_fixCFF(this);";

  // Script for onfocus hookup with the chooser
  private static final String _FOCUS_PREFIX = "_cfBus['";
  private static final String _FOCUS_SUFFIX = "']=this;";

  private static final String _CELL_SIZE = "15";

  private static final Converter _DEFAULT_CONVERTER = new ColorConverter();

  // Key for remembering the cached chooseId
  private static final Object _CACHED_CHOOSE_ID = new Object();

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SimpleInputColorRenderer.class);
}