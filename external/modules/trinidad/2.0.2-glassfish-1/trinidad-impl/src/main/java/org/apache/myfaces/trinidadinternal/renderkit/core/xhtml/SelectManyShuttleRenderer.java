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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectManyShuttle;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.uix.SelectItemSupport;


/**
 * Renders a shuttle element.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/SelectManyShuttleRenderer.java#0 $) $Date: 10-nov-2005.18:56:13 $
 */
public class SelectManyShuttleRenderer extends SimpleSelectManyRenderer
{
  public SelectManyShuttleRenderer()
  {
    this(CoreSelectManyShuttle.TYPE);
  }

  protected SelectManyShuttleRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _sizeKey = type.findKey("size");
    _leadingHeaderKey = type.findKey("leadingHeader");
    _leadingDescShownKey = type.findKey("leadingDescShown");

    _trailingHeaderKey = type.findKey("trailingHeader");
    _trailingDescShownKey = type.findKey("trailingDescShown");

    _leadingBox = new Box(type,
                          new ShuttleList(type, true),
                          true);
    _trailingBox = new Box(type,
                           new ShuttleList(type, false),
                           false);
  }

  @Override
  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    // Since we override getSubmittedValue() entirely,
    // detect auto submit manually
    detectAutoSubmit(context, component, clientId);

    String trailingId = clientId + ":trailing:items";
    String paramValue = (String) context.getExternalContext().
                                getRequestParameterMap().get(trailingId);
    if ((paramValue == null) || "".equals(paramValue))
      return new String[0];

    List<String> list = new ArrayList<String>();
    StringTokenizer tokenizer = new StringTokenizer(paramValue, ";");

    // don't let the submitted list get any bigger than the number of
    // total items in the shuttle
    int availableItems = SelectItemSupport.getSelectItemCount(component);
    int numItems = 0;
    while (tokenizer.hasMoreElements())
    {
      numItems++;
      if (numItems > availableItems)
      {
        _LOG.severe("SELECTED_SHUTTLE_ITEMS_EXCEEDED_TOTAL_NUMBER", clientId);
        return new String[0];
      }

      list.add(tokenizer.nextToken());
    }

    if (_LOG.isFiner())
    {
      _LOG.finer("Found " + list.size() + " entries for shuttle " + clientId);
    }

    return list.toArray(new String[list.size()]);
  }

  protected Integer getSize(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_sizeKey);
    if (o == null)
      o = _sizeKey.getDefault();

    if (o instanceof Integer)
      return (Integer) o;
    else if (o instanceof Number)
      return ((Number) o).intValue();

    return null;
  }

  protected boolean getReorderOnly(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  protected boolean getLeadingDescShown(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_leadingDescShownKey);
    if (o == null)
      o = _leadingDescShownKey.getDefault();
    return Boolean.TRUE.equals(o);
  }

  protected String getLeadingHeader(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_leadingHeaderKey));
  }

  protected boolean getTrailingDescShown(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_trailingDescShownKey);
    if (o == null)
      o = _trailingDescShownKey.getDefault();
    return Boolean.TRUE.equals(o);
  }

  protected String getTrailingHeader(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_trailingHeaderKey));
  }

  @Override
  protected boolean isAutoSubmit(
    UIComponent component,
    FacesBean   bean)
  {
    // No autoSubmit support yet
    return false;
  }

  @Override
  public boolean getSimple(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected boolean renderReadOnlyAsElement(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    return true;
  }

  @Override
  // Make read-only shuttles show disabled lists
  protected boolean getDisabled(
    UIComponent component,
    FacesBean   bean)
  {
    if (super.getDisabled(component, bean))
      return true;

    return super.getReadOnly(FacesContext.getCurrentInstance(), component, bean);
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectManyShuttle::content";
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|selectManyShuttle";
  }

  @Override
  protected void encodeElementContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> selectItems,
    int[]            selectedIndices,
    Converter        converter,
    boolean          valuePassThru
    ) throws IOException
  {
    FormData fData = rc.getFormData();
    if (fData == null)
    {
      _LOG.warning("COMPONENT_REQUIRES_FORM", component);
      return;
    }

    String clientId = getClientId(context, component);

    // Add the needed JS library
    XhtmlUtils.addLib(context, rc, "ShuttleProxy()");


    // Build up the two lists of select items

    // The easiest strategy for value passthru is to just
    // slam all the values with integers, freeing the rest
    // of the code from the responsibility of caring!
    if (!valuePassThru)
      _convertSelectItemListToIndices(selectItems);

    // Start the leading list off with everything, and the trailing list
    // off with nothing
    List<SelectItem> leadingSelectItems = new ArrayList<SelectItem>(selectItems);
    List<SelectItem> trailingSelectItems =
       new ArrayList<SelectItem>(selectedIndices.length);

    // Now, for every selected index, add an item over
    for (int i = 0; i < selectedIndices.length; i++)
    {
      int selectedIndex = selectedIndices[i];
      // -1 means a value couldn't be matched up to
      // a SelectItem.  That's programmer error, but we've
      // already logged a warning
      if (selectedIndex < 0)
        continue;

      trailingSelectItems.add(selectItems.get(selectedIndex));
    }

    // Now, sort the list
    Arrays.sort(selectedIndices);
    // And, in reverse order, remove those indices (reverse order
    // so we don't affect the indices of the items at the end)
    for (int i = selectedIndices.length - 1; i >= 0; i--)
    {
      int selectedIndex = selectedIndices[i];
      // -1 means a value couldn't be matched up to
      // a SelectItem.  That's programmer error, but we've
      // already logged a warning
      if (selectedIndex < 0)
        break;

      leadingSelectItems.remove(selectedIndex);
    }


    // Initialize global info
    ShuttleInfo info = _createAndSetShuttleInfo(rc, component, bean, leadingSelectItems,
      trailingSelectItems, clientId);
    leadingSelectItems.add(new SelectItem("", info.barText));
    trailingSelectItems.add(new SelectItem("", info.barText));
    
    Map<String, String> originalSkinResourceMap = rc.getSkinResourceKeyMap();
    rc.setSkinResourceKeyMap(getResourceKeyMap());

    /* FIXME: add this?
      XhtmlLafUtils.addOnSubmitRequiredValidator(
                                       context,
                                       node,
                                       UIXSelectMany.REQUIRED_MESSAGE_ID,
                                       getNodeName(context, node));

    */

    ResponseWriter rw = context.getResponseWriter();

    _addTranslations(context, rc);

    boolean onlyOneList = getReorderOnly(component, bean);

    rw.startElement("table", component);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", "10%");
    // Don't use renderID(), which because we're on formElement will try to
    // write out "name" too
    rw.writeAttribute("id", clientId, null);
    renderShortDescAttribute(context, rc, component, bean);
    renderEventHandlers(context, component, bean);
    renderRootDomElementStyles(context, rc, component, bean);

    _renderHeaderRow(context, rc, component, bean, onlyOneList);
    _renderContainerRow(context, rc, component, bean, onlyOneList, clientId);

    rw.endElement("table");

    rc.setSkinResourceKeyMap(originalSkinResourceMap);
    // remove info about this shuttle from context
    _clearContext(rc);
  }

  private void _renderScriptsAndValues(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           clientId,
    boolean          onlyOneList
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    ContainerInfo leadingInfo = _getContainerInfo(rc, true);
    ContainerInfo trailingInfo = _getContainerInfo(rc, false);

    // Write out some hidden values
    if (!onlyOneList)
    {
      rw.startElement("input", null);
      rw.writeAttribute("type", "hidden", null);
      rw.writeAttribute("name", leadingInfo.id + _ITEMS_COMPLETE,
                        null);
      rw.writeAttribute("value",
                        _getValue(leadingInfo.itemsList),
                        null);
      rw.endElement("input");
    }

    rw.startElement("input", null);
    rw.writeAttribute("type", "hidden", null);
    rw.writeAttribute("name", trailingInfo.id + _ITEMS_COMPLETE,
                      null);
    rw.writeAttribute("value",
                      _getValue(trailingInfo.itemsList),
                      null);
    rw.endElement("input");

    // Render the scripts
    rw.startElement("script", null);
    renderScriptDeferAttribute(context, rc);
    renderScriptTypeAttribute(context, rc);
    if (!onlyOneList && getLeadingDescShown(component, bean))
    {
      _writeDescriptionScript(context,
                              leadingInfo.itemsList,
                              leadingInfo.id + _DESCRIPTION_COMPLETE);
    }

    _writeDescriptionScript(context,
                            trailingInfo.itemsList,
                            trailingInfo.id + _DESCRIPTION_COMPLETE);
    _writeResetScript(context, rc, clientId);

    rw.endElement("script");
  }

  protected void renderReorderButtons(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           listId
    ) throws IOException
  {
  }

  private void _writeResetScript(
    FacesContext     context,
    RenderingContext rc,
    String           clientId
    ) throws IOException
  {
    // FIXME: Make sure we don't NPE when not in form

    // Get JS-valid identifiers for ourselves
    String formName = rc.getFormData().getName();
    String jsFormName = XhtmlUtils.getJSIdentifier(formName);
    String jsClientId = XhtmlUtils.getJSIdentifier(clientId);

    // Add the reset call to the form as a whole
    StringBuilder funcCallBuffer = new StringBuilder(
                                               19 +
                                               formName.length() +
                                               clientId.length());

    funcCallBuffer.append("TrShuttleProxy._resetItems('").append(clientId);
    funcCallBuffer.append("','").append(formName).append("');");
    FormRenderer.addResetCall(clientId, funcCallBuffer.toString());

    ResponseWriter rw = context.getResponseWriter();
    // And write out the "orig" script that retains knowledge of
    // the original state of the component
    rw.writeText( "window[\"_", null);
    rw.writeText(jsFormName, null);
    rw.writeText("_", null);
    rw.writeText(jsClientId, null);
    rw.writeText("_orig\"]=TrShuttleProxy._copyLists('", null);
    rw.writeText(clientId, null);
    rw.writeText("','", null);
    rw.writeText(formName, null);
    rw.writeText("');", null);

  }

  private void _writeDescriptionScript(
    FacesContext     context,
    List<SelectItem> selectItems,
    String           id
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    id = XhtmlUtils.getJSIdentifier(id);
    rw.writeText(id, null);
    rw.writeText("=new Array(", null);
    boolean writtenOne = false;
    for (SelectItem item : selectItems)
    {
      if (writtenOne)
        rw.writeText(",", null);
      else
        writtenOne = true;
      String description = item.getDescription();
      if (description == null)
        rw.writeText("''", null);
      else
      {
        rw.writeText("'", null);
        rw.writeText(XhtmlUtils.escapeJS(description, true), null);
        rw.writeText("'", null);
      }
    }

    rw.writeText(");", null);
  }

  /**
   * Rips through the list of SelectItems, and sets all item's
   * values to their index
   */
  static private void _convertSelectItemListToIndices(
    List<SelectItem>     itemsToConvert)
  {
    int length = itemsToConvert.size();
    // loop through each item to convert.
    for (int j=0; j < length; j++)
    {
      SelectItem oldSelectItem = itemsToConvert.get(j);
      // We have to create a new item - the old ones are not
      // necessarily ours, so we can't just mutate 'em
      SelectItem newSelectItem = new SelectItem(j,
                                               oldSelectItem.getLabel(),
                                               oldSelectItem.getDescription(),
                                               oldSelectItem.isDisabled());
      itemsToConvert.set(j, newSelectItem);
    }
  }


  private String _getValue(
    List<SelectItem> items)
  {
    StringBuilder vals = new StringBuilder();
    for (SelectItem item : items)
    {
      if (vals.length() > 0)
        vals.append(';');
      vals.append(item.getValue());
    }

    return vals.toString();
  }

  private void _addTranslations(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    // Add needed translations
    if (!rc.getProperties().containsKey(_TRANSLATED_VARS_EXIST_PROPERTY_KEY))
    {
      ResponseWriter rw = context.getResponseWriter();
      rc.getProperties().put(_TRANSLATED_VARS_EXIST_PROPERTY_KEY,
                             Boolean.TRUE);
      rw.startElement("script", null);
      renderScriptDeferAttribute(context, rc);
      // Bug #3426092:
      // render the type="text/javascript" attribute in accessibility mode
      renderScriptTypeAttribute(context, rc);

      String noItems = rc.getTranslatedString(_SHUTTLE_NO_ITEMS_FEEDBACK_KEY);
      String noItemsSelected =
        rc.getTranslatedString(_SHUTTLE_NO_ITEM_SELECTED_FEEDBACK_KEY);

      rw.writeText(_TRANSLATED_JS_FEEDBACK_NO_ITEMS, null);
      if (noItems != null)
        rw.writeText(XhtmlUtils.escapeJS(noItems, true), null);

      rw.writeText("';", null);
      rw.writeText(_TRANSLATED_JS_FEEDBACK_NO_ITEMS_SELECTED, null);
      if (noItemsSelected != null )
      {
        rw.writeText(XhtmlUtils.escapeJS(noItemsSelected, true), null);
      }
      rw.writeText("';", null);
      rw.endElement("script");
    }
  }

  private void _renderHeaderRow(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          onlyOneList
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("tr", null);
    if (!onlyOneList)
    {
      rw.startElement("td", null);
      renderStyleClass(context, rc, SkinSelectors.SHUTTLE_HEADER_STYLE_CLASS);
      rw.writeAttribute("valign", "bottom", null);
      String leadingHeader = getLeadingHeader(component, bean);
      if (leadingHeader != null)
        rw.writeText(leadingHeader, "leadingHeader");
      rw.endElement("td");

      rw.startElement("td", null);
      rw.endElement("td");
    }

    rw.startElement("td", null);
    renderStyleClass(context, rc, SkinSelectors.SHUTTLE_HEADER_STYLE_CLASS);
    rw.writeAttribute("valign", "bottom", null);
    if (getRequired(component, bean) || getShowRequired(component, bean))
    {
      // Get the required Icon from the context
      Icon icon = rc.getIcon(SkinSelectors.REQUIRED_ICON_ALIAS_NAME);
      if (icon != null)
      {
        OutputUtils.renderIcon(context, rc, icon,
                               rc.getTranslatedString("REQUIRED_TIP"),
                               null);
        renderSpacer(context, rc, "1", "1");
      }
    }

    String trailingHeader = getTrailingHeader(component, bean);
    if (trailingHeader != null)
      rw.writeText(trailingHeader, "trailingHeader");
    rw.endElement("td");
    rw.endElement("tr");
  }

  private void _renderContainerRow(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    boolean          onlyOneList,
    String           clientId
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    if (!onlyOneList)
    {
      rw.startElement("tr", null);
      rw.startElement("td", null);
      delegateRenderer(context,
                       rc,
                       component,
                       bean,
                       _leadingBox);
      rw.endElement("td");

      rw.startElement("td", null);
      rw.writeAttribute("align", "center", null);
      rw.writeAttribute("valign", "middle", null);
      rw.writeAttribute("nowrap", Boolean.TRUE, null);
      rw.writeAttribute("style", "padding:5px", null);
      _renderMoveButtons(context, rc, component, bean);
      rw.endElement("td");
    }

    rw.startElement("td", null);
    delegateRenderer(context,
                     rc,
                     component,
                     bean,
                     _trailingBox);

    // Put the values in this TD, so they're at least inside of the table
    _renderScriptsAndValues(context, rc, component, bean, clientId, onlyOneList);
    rw.endElement("td");

    rw.endElement("tr");
  }

  private void _renderMoveButtons(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    ShuttleInfo info = _getShuttleInfo(rc);
    String leadingId = info.leadingInfo.id;
    String trailingId = info.trailingInfo.id;

    // MOVE button
    boolean disabled = getDisabled(component, bean);
    String moveUrl = disabled ? null :
      "javascript:TrShuttleProxy._moveItems('" + leadingId + "','" + trailingId + "');";
    renderButton(context, rc,
                  SkinSelectors.AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME,
                  _SELECT_MANY_MOVE_TIP_KEY,
                  moveUrl);
    _renderLink(context, rc,
                _SELECT_MANY_MOVE_KEY, _SELECT_MANY_MOVE_TIP_KEY,
                moveUrl);
    rw.startElement("div", null);
    rw.writeAttribute("style", "margin-top:5px", null);
    rw.endElement("div");

    // MOVE ALL button
    String moveAllUrl = disabled ? null :
      "javascript:TrShuttleProxy._moveAllItems('" + leadingId + "','" + trailingId + "');";
    renderButton(context, rc,
                  SkinSelectors.AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME,
                  _SELECT_MANY_MOVE_ALL_TIP_KEY,
                  moveAllUrl);
    _renderLink(context, rc,
                _SELECT_MANY_MOVE_ALL_KEY, _SELECT_MANY_MOVE_ALL_TIP_KEY,
                moveAllUrl);
    rw.startElement("div", null);
    rw.writeAttribute("style", "margin-top:5px", null);
    rw.endElement("div");

    // REMOVE button
    String removeUrl = disabled ? null :
      "javascript:TrShuttleProxy._moveItems('" + trailingId + "','" + leadingId + "');";
    renderButton(context, rc,
                  SkinSelectors.AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME,
                  _SELECT_MANY_REMOVE_TIP_KEY,
                  removeUrl);
    _renderLink(context, rc,
                _SELECT_MANY_REMOVE_KEY, _SELECT_MANY_REMOVE_TIP_KEY,
                removeUrl);
    rw.startElement("div", null);
    rw.writeAttribute("style", "margin-top:5px", null);
    rw.endElement("div");

    // REMOVE ALL button
    String removeAllUrl = disabled ? null :
      "javascript:TrShuttleProxy._moveAllItems('" + trailingId + "','" + leadingId + "');";
    renderButton(context, rc,
                  SkinSelectors.AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME,
                  _SELECT_MANY_REMOVE_ALL_TIP_KEY,
                  removeAllUrl);
    _renderLink(context, rc,
                _SELECT_MANY_REMOVE_ALL_KEY, _SELECT_MANY_REMOVE_ALL_TIP_KEY,
                removeAllUrl);
  }

  private int _getBestListLen(
    UIComponent component,
    FacesBean   bean,
    int         leadingListCount,
    int         trailingListCount)
  {
    Integer rows = getSize(component, bean);
    if ( rows != null )
    {
      return Math.min(_MAXIMUM_LIST_LEN,
                      Math.max(_MINIMUM_LIST_LEN, rows.intValue()));
    }
    else
    {
      boolean higher1 = (leadingListCount > _MAXIMUM_LIST_LEN);
      boolean higher2 = (trailingListCount > _MAXIMUM_LIST_LEN);
      boolean between1 = (leadingListCount > _MINIMUM_LIST_LEN &&
                          leadingListCount < _MAXIMUM_LIST_LEN);
      boolean between2 = (trailingListCount > _MINIMUM_LIST_LEN &&
                          trailingListCount < _MAXIMUM_LIST_LEN);

      //if either higher than max, take max
      if( higher1 || higher2 )
      {
        return _MAXIMUM_LIST_LEN;
      }
      //if they are both between, take the lower so bars don't show
      else if ( between1 && between2 )
        return Math.min(leadingListCount, trailingListCount);
      //if one is between and the other isn't, take the one between
      else if (  between1 || between2 )
        return Math.max( leadingListCount, trailingListCount);
      // Otherwise, just default to the minimum
      else
        return _MINIMUM_LIST_LEN;
    }
  }

  private static final void _clearContext(
    RenderingContext rc
  )
  {
    // clear property from context
    rc.getProperties().remove(_SHUTTLE_INFO_KEY);
  }

  private void _startRow(
    FacesContext context,
    int          colspan
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("tr", null);
    rw.startElement("td", null);
    rw.writeAttribute("nowrap", Boolean.TRUE, null);
    rw.writeAttribute("valign", "middle", null);
    if (colspan > 1)
      rw.writeAttribute("colspan", colspan, null);
  }

  private void _endRow(
    FacesContext     context,
    RenderingContext rc,
    int              height
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.endElement("td");
    if (height > 0)
    {
      rw.startElement("td", null);
      // Shouldn't this just go on the TD?
      renderSpacer(context, rc, "1", Integer.toString(height));
      rw.endElement("td");
    }
    rw.endElement("tr");
  }

  protected void renderButton(
    FacesContext     context,
    RenderingContext rc,
    String           iconName,
    String           shortDescKey,
    String           href
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("a", null);
    // Write out Javascript
    // FIXME: this would be far better just written as Javascript
    // with onclick
    renderEncodedActionURI(context, "href", href);
    Icon icon = rc.getIcon(iconName);

    // FIXME: the icon previously had "p_OraDisplayBlock" rendered on it.
    // I suspect this came up in strict rendering mode
    if (icon != null)
      OutputUtils.renderIcon(context,
                             rc,
                             icon,
                             rc.getTranslatedString(shortDescKey),
                             null,
                             true);
    rw.endElement("a");
  }

  private void _renderLink(
    FacesContext     context,
    RenderingContext rc,
    String           textKey,
    String           shortDescKey,
    String           href
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("a", null);
    renderEncodedActionURI(context, "href", href);
    renderStyleClass(context,
                     rc,
                     href == null
                       ? SkinSelectors.LINK_DISABLED_STYLE_CLASS
                       : SkinSelectors.LINK_STYLE_CLASS);
    rw.writeAttribute("title", rc.getTranslatedString(shortDescKey), null);
    rw.writeText(rc.getTranslatedString(textKey), null);
    rw.endElement("a");
  }

  protected Map<String, String> getResourceKeyMap()
  {
    return _SHUTTLE_KEY_MAP;
  }

  private ShuttleInfo _getShuttleInfo(
    RenderingContext rc)
  {
    return (ShuttleInfo) rc.getProperties().get(_SHUTTLE_INFO_KEY);
  }

  private ContainerInfo _getContainerInfo(
    RenderingContext rc,
    boolean          isLeading)
  {
    ShuttleInfo shuttleInfo = _getShuttleInfo(rc);
    return isLeading ?
        shuttleInfo.leadingInfo : shuttleInfo.trailingInfo;
  }

  private ShuttleInfo _createAndSetShuttleInfo(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    List<SelectItem> leadingItems,
    List<SelectItem> trailingItems,
    String           clientId)
  {
    ShuttleInfo shuttleInfo = new ShuttleInfo();
    ContainerInfo leadingInfo = new ContainerInfo(leadingItems);
    ContainerInfo trailingInfo = new ContainerInfo(trailingItems);
    leadingInfo.id = clientId + _LEADING_COMPLETE;
    trailingInfo.id = clientId + _TRAILING_COMPLETE;

    shuttleInfo.listLen = _getBestListLen(component, bean,
                                          leadingInfo.listCount,
                                          trailingInfo.listCount);
    int barWidth = Math.max(Math.max(trailingInfo.maxWidth,
                                     leadingInfo.maxWidth),
                            _BARS_MINIMUM_WIDTH);

    // determine description area width
    // the description area width needs to be tweaked because
    // the description area should be roughly the same
    // size as the list, but just setting the
    // width to be the width of the list isn't enough
    int descWidth = barWidth;
    // On IE and Gecko, the  width was too narrow so increasing
    if ( isIE(rc) ||
         isGecko(rc))
      descWidth = (descWidth *6)/5;

    shuttleInfo.descWidth = descWidth;
    if( barWidth <= _BARS_MINIMUM_WIDTH)
    {
      shuttleInfo.barText =  _BARS_MINIMUM;
    }
    else
    {
      char[] addedChars = new char[barWidth - _BARS_MINIMUM_WIDTH];

      for(int i=0; i < barWidth - _BARS_MINIMUM_WIDTH; i++)
        addedChars[i] = '_';

      shuttleInfo.barText = _BARS_MINIMUM + new String(addedChars);
    }

    shuttleInfo.leadingInfo = leadingInfo;
    shuttleInfo.trailingInfo = trailingInfo;

    rc.getProperties().put(_SHUTTLE_INFO_KEY, shuttleInfo);
    return shuttleInfo;
  }


  private class ShuttleList extends SimpleSelectManyListboxRenderer
  {
    public ShuttleList(
      FacesBean.Type type,
      boolean        isLeading)
    {
      super(type);
      _isLeading = isLeading;
    }

    @Override
    protected String getClientId(
      FacesContext context,
      UIComponent  component)
    {
      RenderingContext rc = RenderingContext.getCurrentInstance();
      ContainerInfo info = _getContainerInfo(rc, _isLeading);
      return info.id;
    }

    @Override
    protected String getOndblclick(
      UIComponent component,
      FacesBean   bean)
    {
      RenderingContext rc = RenderingContext.getCurrentInstance();
      ContainerInfo info = _getContainerInfo(rc, _isLeading);
      ContainerInfo otherInfo = _getContainerInfo(rc, !_isLeading);

      StringBuilder builder = new StringBuilder();
      builder.append("TrShuttleProxy._moveItems('");
      builder.append(info.id);
      builder.append("','");
      builder.append(otherInfo.id);
      builder.append("','");
      builder.append(rc.getFormData().getName());
      builder.append("');");
      return builder.toString();
    }

    @Override
    protected String getShortDesc(
      UIComponent component,
      FacesBean   bean)
    {
      // Use the header as the description for the list
      if (_isLeading)
        return getLeadingHeader(component, bean);
      else
        return getTrailingHeader(component, bean);
    }

    @Override
    // Make read-only shuttles show disabled lists
    protected boolean getDisabled(
      UIComponent component,
      FacesBean   bean)
    {
      if (super.getDisabled(component, bean))
        return true;

      return super.getReadOnly(FacesContext.getCurrentInstance(), component, bean);
    }

    @Override
    protected boolean getReadOnly(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean)
    {
      return false;
    }

    @Override
    protected String getOnchange(
      UIComponent component,
      FacesBean   bean)
    {
      if (_isLeading)
      {
        if (!getLeadingDescShown(component, bean))
          return null;
      }
      else
      {
        if (!getTrailingDescShown(component, bean))
          return null;
      }

      StringBuilder builder = new StringBuilder();
      RenderingContext rc = RenderingContext.getCurrentInstance();
      ContainerInfo info = _getContainerInfo(rc, _isLeading);
      builder.append("TrShuttleProxy._displayDesc('");
      builder.append(info.id);
      builder.append("','");
      builder.append(rc.getFormData().getName());
      builder.append("');");
      return builder.toString();
    }

    @Override
    protected int[] getSelectedIndices(
      FacesContext     context,
      UIComponent      component,
      FacesBean        bean,
      List<SelectItem> selectItems,
      Converter        converter,
      boolean          valuePassThru)
    {
      return _EMPTY_INT_ARRAY;
    }

    @Override
    protected List<SelectItem> getSelectItems(
      UIComponent component,
      Converter   converter)
    {
      RenderingContext rc = RenderingContext.getCurrentInstance();
      ContainerInfo info = _getContainerInfo(rc, _isLeading);
      return info.itemsList;
    }

    @Override
    protected int getSize(
      UIComponent component,
      FacesBean   bean)
    {
      RenderingContext rc = RenderingContext.getCurrentInstance();
      ShuttleInfo info = _getShuttleInfo(rc);
      return info.listLen;
    }

    @Override
    protected boolean isAutoSubmit(
      UIComponent component,
      FacesBean   bean)
    {
      return false;
    }

    @Override
    public boolean getSimple(
      UIComponent component,
      FacesBean   bean)
    {
      return true;
    }

    @Override
    protected boolean getValuePassThru(
      UIComponent component,
      FacesBean   bean)
    {
      // Value passthru has already been accounted for
      return true;
    }

    @Override
    protected String getStyleClass(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getInlineStyle(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    private boolean _isLeading;
  }

  private class Box extends PanelBoxRenderer
  {
    public Box(
      FacesBean.Type type,
      ShuttleList    list,
      boolean        isLeading)
    {
      super(type);
      _list = list;
      _isLeading = isLeading;
    }

    @Override
    protected String getClientId(
      FacesContext context,
      UIComponent  component)
    {
      return null;
    }

    @Override
    protected boolean hasChildren(
      UIComponent component)
    {
      return true;
    }

    @Override
    protected String getShortDesc(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getStyleClass(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getInlineStyle(
      UIComponent component,
      FacesBean   bean)
    {
      return "width:100%";
    }

    @Override
    protected String getText(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getIcon(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getContentStyle(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getBackground(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected void encodeAllChildren(
      FacesContext context,
      UIComponent  component
      ) throws IOException
    {
      ResponseWriter rw = context.getResponseWriter();
      RenderingContext rc = RenderingContext.getCurrentInstance();
      FacesBean bean = getFacesBean(component);

      ShuttleInfo shuttleInfo = _getShuttleInfo(rc);
      ContainerInfo containerInfo = _getContainerInfo(rc, _isLeading);

      rw.startElement("table", null);
      OutputUtils.renderLayoutTableAttributes(context, rc, null, null);

      // Render the filter, if needed
      if (_isLeading)
      {
        UIComponent filter = getFacet(component,
                                      CoreSelectManyShuttle.FILTER_FACET);
        if (filter != null)
        {
          _startRow(context,  3);
          encodeChild(context, filter);
          _endRow(context, rc, _DEFAULT_FILTER_HEIGHT);
        }
      }

      _startRow(context, 1);
      delegateRenderer(context,
                       rc,
                       component,
                       bean,
                       _list);

      if (!_isLeading && isReorderable())
      {
        rw.endElement("td");
        renderReorderButtons(context, rc, component, bean, containerInfo.id);
        rw.startElement("td", null);
      }

      boolean hasLeadingDesc = getLeadingDescShown(component, bean);
      boolean hasTrailingDesc = getTrailingDescShown(component, bean);
      boolean hasDescArea = hasLeadingDesc || hasTrailingDesc;

      _endRow(context, rc, 0);

      if (hasDescArea)
      {
        // Write out a spacer row beween the list and the description
        _startRow(context, 1);
        _endRow(context, rc, 8);

        _startRow(context, 3);
        rw.startElement("span", null);
        renderStyleClass(context, rc, SkinSelectors.INSTRUCTION_TEXT_STYLE_CLASS);
        String label = rc.getTranslatedString(_SELECT_MANY_DESCRIPTION_LABEL_KEY);
        rw.writeText(label, null);
        rw.endElement("span");
        rw.startElement("div", null);
        rw.endElement("div");
        rw.startElement("textarea", null);
        rw.writeAttribute("rows", "2", null);
        String textareaId = containerInfo.id + _DESC_FIELD_COMPLETE;
        rw.writeAttribute("id", textareaId, null);
        rw.writeAttribute("name", textareaId, null);
        rw.writeAttribute("readonly", Boolean.TRUE, null);
        rw.writeAttribute("cols", shuttleInfo.descWidth, null);
        rw.writeAttribute("wrap", "soft", null);

        rw.endElement("textarea");

        HiddenLabelUtils.outputHiddenLabelIfNeeded(context,
                                                   rc,
                                                   textareaId,
                                                   label,
                                                   component);

        _endRow(context, rc, _DEFAULT_DESC_AREA_HEIGHT);
      }

      // And, finally, the footer row
      UIComponent leadingFooter = getFacet(component,
                                           CoreSelectManyShuttle.LEADING_FOOTER_FACET);
      UIComponent trailingFooter = getFacet(component,
                                           CoreSelectManyShuttle.TRAILING_FOOTER_FACET);
      if ((leadingFooter != null) && (trailingFooter != null))
      {
        // Write out a spacer row before the footer
        _startRow(context, 1);
        _endRow(context, rc, 8);
        _startRow(context, 3);
        UIComponent footer = _isLeading ? leadingFooter : trailingFooter;
        if (footer != null)
          encodeChild(context, footer);
        _endRow(context, rc, _DEFAULT_FOOTER_HEIGHT);

      }

      rw.endElement("table");
    }

    private ShuttleList _list;
    private boolean     _isLeading;
  }

  /*
   * Per render shuttle info
   */
  private static class ShuttleInfo
  {
    public String  barText;
    public int     listLen;
    public int     descWidth;

    public ContainerInfo leadingInfo = null;
    public ContainerInfo trailingInfo = null;
  }

  /*
   * Per render info for each container
   */
  private static class ContainerInfo
  {
    public ContainerInfo(List<SelectItem> itemsList)
    {
      this.itemsList = itemsList;
      this.listCount = itemsList.size();
      for (SelectItem item : itemsList)
      {
        String text  = item.getLabel();
        if (text != null)
        {
          maxWidth = Math.max( text.length(),
                               maxWidth);
        }
      }
    }

    public final List<SelectItem> itemsList;
    public final int listCount;

    public String id;
    public int    maxWidth  = 0;
  }

  private PropertyKey _sizeKey;
  private PropertyKey _leadingDescShownKey;
  private PropertyKey _leadingHeaderKey;
  private PropertyKey _trailingDescShownKey;
  private PropertyKey _trailingHeaderKey;

  private Box _leadingBox;
  private Box _trailingBox;

  private final static int _MAXIMUM_LIST_LEN   = 20;
  private final static int _MINIMUM_LIST_LEN   = 10;
  private final static int _BARS_MINIMUM_WIDTH = 15;

  private final static String _BARS_MINIMUM    = "_______________";

  private final static int _DEFAULT_DESC_AREA_HEIGHT = 68;
  private final static int _DEFAULT_FILTER_HEIGHT    = 36;
  private final static int _DEFAULT_FOOTER_HEIGHT    = 36;

  private final static String _LEADING_COMPLETE     = ":leading";
  private final static String _TRAILING_COMPLETE    = ":trailing";
  private final static String _ITEMS_COMPLETE       = ":items";
  private final static String _DESC_FIELD_COMPLETE       = ":desc";
  private final static String _DESCRIPTION_COMPLETE = "_desc";

  private final static String _TRANSLATED_JS_FEEDBACK_NO_ITEMS
                                 = "var _shuttle_no_items='";
  private final static String _TRANSLATED_JS_FEEDBACK_NO_ITEMS_SELECTED
                                 = "var _shuttle_no_items_selected='";
  private static final Object _TRANSLATED_VARS_EXIST_PROPERTY_KEY =
                                                             new Object();

  private final static Object _SHUTTLE_INFO_KEY = new Object();


  static private final int[] _EMPTY_INT_ARRAY = new int[0];

  // translation keys.
  // Feedback in javascript alert when user tries to use
  // move/remove buttons when there are no items
  private final static String _SHUTTLE_NO_ITEMS_FEEDBACK_KEY =
    "SHUTTLE_NO_ITEMS_FEEDBACK";
  private final static String _SHUTTLE_NO_ITEM_SELECTED_FEEDBACK_KEY =
    "SHUTTLE_NO_ITEM_SELECTED_FEEDBACK";

  protected final static String _SELECT_MANY_DESCRIPTION_LABEL_KEY =
    "af_selectManyShuttle.DESCRIPTION_LABEL";
  protected final static String _SELECT_MANY_MOVE_ALL_TIP_KEY =
    "af_selectManyShuttle.MOVE_ALL_TIP";
  protected final static String _SELECT_MANY_MOVE_TIP_KEY =
    "af_selectManyShuttle.MOVE_TIP";
  protected final static String _SELECT_MANY_REMOVE_ALL_TIP_KEY =
    "af_selectManyShuttle.REMOVE_ALL_TIP";
  protected final static String _SELECT_MANY_REMOVE_TIP_KEY =
    "af_selectManyShuttle.REMOVE_TIP";


  protected final static String _SELECT_MANY_MOVE_ALL_KEY =
    "af_selectManyShuttle.MOVE_ALL";
  protected final static String _SELECT_MANY_MOVE_KEY =
    "af_selectManyShuttle.MOVE";
  protected final static String _SELECT_MANY_REMOVE_ALL_KEY =
    "af_selectManyShuttle.REMOVE_ALL";
  protected final static String _SELECT_MANY_REMOVE_KEY =
    "af_selectManyShuttle.REMOVE";

  // map the selectMany translation keys to the selectOrder translation keys.
  // This renderer code uses the selectMany translation keys. If selectOrder
  // shuttle is being rendered, then context.getTranslatedValue will use
  private static final Map<String, String> _SHUTTLE_KEY_MAP =
    new HashMap<String, String>();

  static
  {
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_LIGHT_STYLE_CLASS,
                          SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BODY_STYLE_CLASS,
                          SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_BODY_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_START_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_TOP_START_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_TOP_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_END_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_TOP_END_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_START_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_START_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_CONTENT_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_CONTENT_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_END_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_END_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_BOTTOM_START_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_BOTTOM_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_MANY_SHUTTLE_PB_BOTTOM_END_STYLE_CLASS);
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SelectManyShuttleRenderer.class);
}
