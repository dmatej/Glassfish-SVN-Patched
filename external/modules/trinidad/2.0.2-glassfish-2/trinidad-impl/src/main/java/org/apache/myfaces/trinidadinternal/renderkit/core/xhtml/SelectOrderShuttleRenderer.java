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

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectOrderShuttle;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class SelectOrderShuttleRenderer extends SelectManyShuttleRenderer
{
  public SelectOrderShuttleRenderer()
  {
    super(CoreSelectOrderShuttle.TYPE);
  }

  protected SelectOrderShuttleRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _reorderOnlyKey = type.findKey("reorderOnly");
  }

  @Override
  protected boolean getReorderOnly(
    UIComponent component,
    FacesBean   bean)
  {
    if (_reorderOnlyKey == null)
      return false;

    return Boolean.TRUE.equals(bean.getProperty(_reorderOnlyKey));
  }

  @Override
  protected boolean isReorderable()
  {
    return true;
  }

  @Override
  protected Map<String, String> getResourceKeyMap()
  {
    return _SELECT_ORDER_KEY_MAP;
  }

  @Override
  protected void renderReorderButtons(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           listId
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", null);
    rw.writeAttribute("align", "center", null);
    rw.writeAttribute("valign", "middle", null);

    boolean disabled = getDisabled(component, bean);
    renderButton(context,
                  rc,
                  SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_TOP_ICON_NAME,
                  "af_selectOrderShuttle.REORDER_UP_ALL_TIP",
                  disabled ? null :
                  "javascript:TrShuttleProxy._orderTopBottomList(0,'" + listId + "');");
    rw.startElement("br", null);
    rw.endElement("br");

    renderButton(context,
                  rc,
                  SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_UP_ICON_NAME,
                  "af_selectOrderShuttle.REORDER_UP_TIP",
                  disabled ? null :
                  "javascript:TrShuttleProxy._orderList(0,'" + listId + "');");
    // we render 15 vertical spaces to separate the up from the down icons.
    renderSpacer(context, rc, "1", "15");
    rw.startElement("br", null);
    rw.endElement("br");

    renderButton(context,
                  rc,
                  SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_DOWN_ICON_NAME,
                  "af_selectOrderShuttle.REORDER_DOWN_TIP",
                  disabled ? null :
                  "javascript:TrShuttleProxy._orderList(1,'" + listId + "');");
    rw.startElement("br", null);
    rw.endElement("br");

    renderButton(context,
                 rc,
                 SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REORDER_BOTTOM_ICON_NAME,
                 "af_selectOrderShuttle.REORDER_DOWN_ALL_TIP",
                 disabled ? null :
                 "javascript:TrShuttleProxy._orderTopBottomList(1,'" + listId + "');");

    rw.endElement("td");
  }

  private PropertyKey _reorderOnlyKey;

  private final static String _SELECT_ORDER_DESCRIPTION_LABEL_KEY =
    "af_selectOrderShuttle.DESCRIPTION_LABEL";
  private final static String _SELECT_ORDER_MOVE_ALL_TIP_KEY =
    "af_selectOrderShuttle.MOVE_ALL_TIP";
  private final static String _SELECT_ORDER_MOVE_TIP_KEY =
    "af_selectOrderShuttle.MOVE_TIP";
  private final static String _SELECT_ORDER_REMOVE_ALL_TIP_KEY =
    "af_selectOrderShuttle.REMOVE_ALL_TIP";
  private final static String _SELECT_ORDER_REMOVE_TIP_KEY =
    "af_selectOrderShuttle.REMOVE_TIP";
  private final static String _SELECT_ORDER_MOVE_ALL_KEY =
    "af_selectOrderShuttle.MOVE_ALL";
  private final static String _SELECT_ORDER_MOVE_KEY =
    "af_selectOrderShuttle.MOVE";
  private final static String _SELECT_ORDER_REMOVE_ALL_KEY =
    "af_selectOrderShuttle.REMOVE_ALL";
  private final static String _SELECT_ORDER_REMOVE_KEY =
    "af_selectOrderShuttle.REMOVE";

  // this translation map to map the selectMany keys to the selectOrder keys.
  private static final Map<String, String> _SELECT_ORDER_KEY_MAP =
    new HashMap<String, String>();

  static
  {
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_DESCRIPTION_LABEL_KEY,
                              _SELECT_ORDER_DESCRIPTION_LABEL_KEY);
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_MOVE_ALL_TIP_KEY,
                              _SELECT_ORDER_MOVE_ALL_TIP_KEY);
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_MOVE_TIP_KEY,
                              _SELECT_ORDER_MOVE_TIP_KEY);
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_REMOVE_ALL_TIP_KEY,
                              _SELECT_ORDER_REMOVE_ALL_TIP_KEY);
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_REMOVE_TIP_KEY,
                              _SELECT_ORDER_REMOVE_TIP_KEY);

    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_MOVE_ALL_KEY,
                              _SELECT_ORDER_MOVE_ALL_KEY);
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_MOVE_KEY,
                              _SELECT_ORDER_MOVE_KEY);
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_REMOVE_ALL_KEY,
                              _SELECT_ORDER_REMOVE_ALL_KEY);
    _SELECT_ORDER_KEY_MAP.put(_SELECT_MANY_REMOVE_KEY,
                              _SELECT_ORDER_REMOVE_KEY);

    // add the icons that are shared between selectManyShuttle
    // and selectOrderShuttle to resource key map
    // Move, Move All, Remove, RemoveAll
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME,
                          SkinSelectors.AF_SELECT_ORDER_SHUTTLE_MOVE_ICON_NAME);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME,
                          SkinSelectors.AF_SELECT_ORDER_SHUTTLE_MOVE_ALL_ICON_NAME);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME,
                          SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REMOVE_ICON_NAME);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME,
                          SkinSelectors.AF_SELECT_ORDER_SHUTTLE_REMOVE_ALL_ICON_NAME);


    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_LIGHT_STYLE_CLASS,
                              SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BODY_STYLE_CLASS,
                          SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_BODY_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_START_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_TOP_START_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_TOP_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_TOP_END_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_TOP_END_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_START_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_START_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_CONTENT_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_CONTENT_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_END_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_END_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_BOTTOM_START_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_BOTTOM_STYLE_CLASS);
    _SELECT_ORDER_KEY_MAP.put(SkinSelectors.AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS,
                         SkinSelectors.AF_SELECT_ORDER_SHUTTLE_PB_BOTTOM_END_STYLE_CLASS);

  }
}
