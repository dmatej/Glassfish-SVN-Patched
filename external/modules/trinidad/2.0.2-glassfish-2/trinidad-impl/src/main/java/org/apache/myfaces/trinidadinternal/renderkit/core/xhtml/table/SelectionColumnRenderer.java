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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;


public class SelectionColumnRenderer extends SpecialColumnRenderer
{
  public SelectionColumnRenderer(FacesBean.Type tableType)
  {
    _singleRenderer = new TableSelectOneRenderer(tableType);
    _multiRenderer = new TableSelectManyRenderer(tableType);
  }

  @Override
  protected void renderKids(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext trc,
    UIComponent           column
    ) throws IOException
  {
    UIComponent table = trc.getTable();
    delegateRenderer(context, rc, table,
                     getFacesBean(table),
                     trc.hasSelectAll()
                     ? _multiRenderer
                     : _singleRenderer);
  }

  @Override
  protected String getHeaderText(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    TableRenderingContext tContext =
      TableRenderingContext.getCurrentInstance();

    String key = _isMultipleSelection(tContext)
     ? "af_tableSelectMany.SELECT_COLUMN_HEADER"
     : "af_tableSelectOne.SELECT_COLUMN_HEADER";

    return arc.getTranslatedString(key);
  }

  @Override
  protected boolean getNoWrap(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected String getFormatType(
    UIComponent component,
    FacesBean   bean)
  {
    return CoreColumn.ALIGN_CENTER;
  }

  @Override
  protected String getHeaderStyleClass(TableRenderingContext tContext)
  {
    return SkinSelectors.AF_COLUMN_HEADER_ICON_STYLE;
  }

  static private boolean _isMultipleSelection(TableRenderingContext tContext)
  {
    return tContext.hasSelectAll();
  }

  private final CoreRenderer _singleRenderer;
  private final CoreRenderer _multiRenderer;
}
