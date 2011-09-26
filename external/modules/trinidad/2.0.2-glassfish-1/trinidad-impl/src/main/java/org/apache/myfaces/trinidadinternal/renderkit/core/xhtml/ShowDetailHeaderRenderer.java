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

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CoreShowDetailHeader;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.CoreRenderer;


public class ShowDetailHeaderRenderer
  extends PanelHeaderRenderer
{
  public ShowDetailHeaderRenderer()
  {
    super(CoreShowDetailHeader.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _detailRenderer = new DetailRenderer(type);
    _disclosedKey = type.findKey("disclosed");
  }

  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    @SuppressWarnings("unused")
    String       clientId)
  {
    // Delegate decoding to the showDetail renderer
    _detailRenderer.decode(facesContext, component);
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // map the resource value keys that are used in showDetail and header
    // to the keys we need to use in this renderer.
    Map<String, String> originalResourceKeyMap = rc.getSkinResourceKeyMap();
    try
    {

      rc.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      super.encodeAll(context, rc, component, bean);
    }
    finally
    {
      //restore original map
      rc.setSkinResourceKeyMap(originalResourceKeyMap);
    }
  }

  @Override
  protected void renderIcon(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    String           messageType
    ) throws IOException
  {
    delegateRenderer(context, rc, component, bean, _detailRenderer);
    //render the showDetailHeader icon. It uses skinning key af|panelHeader::icon-style
    //we can introduce new skinning key af|showDetailHeader::icon-style, if needed.
    super.renderIcon(context, rc, component, bean, messageType);
  }

  @Override
  protected String getMessageType(
    UIComponent component,
    FacesBean   bean)
  {
    // Not currently supported
    return null;
  }

  protected boolean getDisclosed(
    UIComponent component,
    FacesBean   bean)
  {
    Object o = bean.getProperty(_disclosedKey);
    if (o == null)
      o = _disclosedKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  @Override
  protected boolean shouldRenderChildren(
    UIComponent component,
    FacesBean   bean)
  {
    return getDisclosed(component, bean);
  }

  static private class DetailRenderer extends ShowDetailRenderer
  {
    public DetailRenderer(
      FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected void renderId(
      FacesContext context,
      UIComponent  component
      ) throws IOException
    {
    }

    @Override
    protected void renderAllAttributes(
      FacesContext     context,
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean)
      throws IOException
    {
    }

    @Override
    protected boolean renderAsInline()
    {
      return true;
    }

    @Override
    protected String getDisclosureText(
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean,
      boolean          disclosed)
    {
      return null;
    }
  }

  private CoreRenderer _detailRenderer;
  private PropertyKey  _disclosedKey;

  private static final Map<String, String> _RESOURCE_KEY_MAP;
  static
  {
    _RESOURCE_KEY_MAP  =  new HashMap<String, String>();

    _RESOURCE_KEY_MAP.put("af_showDetail.DISCLOSED" ,
                          "af_showDetailHeader.DISCLOSED");
    _RESOURCE_KEY_MAP.put("af_showDetail.UNDISCLOSED" ,
                          "af_showDetailHeader.UNDISCLOSED");
    _RESOURCE_KEY_MAP.put("af_showDetail.DISCLOSED_TIP" ,
                          "af_showDetailHeader.DISCLOSED_TIP");
    _RESOURCE_KEY_MAP.put("af_showDetail.UNDISCLOSED_TIP" ,
                          "af_showDetailHeader.UNDISCLOSED_TIP");
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_PANEL_HEADER_STYLE_CLASS,
                          SkinSelectors.AF_SHOW_DETAIL_HEADER_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
                          SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME);
    _RESOURCE_KEY_MAP.put(SkinSelectors.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
                          SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME);
  }
}
