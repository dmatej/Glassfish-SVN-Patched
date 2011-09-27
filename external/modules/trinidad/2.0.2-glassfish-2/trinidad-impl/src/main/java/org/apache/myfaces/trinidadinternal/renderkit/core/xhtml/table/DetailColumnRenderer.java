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
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.component.core.data.CoreTable;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.ShowDetailRenderer;


public class DetailColumnRenderer extends SpecialColumnRenderer
{
  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    TableRenderingContext tContext =
      TableRenderingContext.getCurrentInstance();

    if (tContext.getRenderStage().getStage() == RenderStage.INITIAL_STAGE)
    {
      // if we have detail disclosure. then we need to use
      // explicit-header-id mode. see bug 3212297:
      tContext.setExplicitHeaderIDMode(true);
      // save the physical column index of the detail column, so that we can
      // use it to get at the detail header id, when we render the detail
      // cell:
      int physicalIndex = tContext.getColumnData().getPhysicalColumnIndex();
      tContext.setDetailColumnIndex(physicalIndex);
    }

    super.encodeAll(context, rc, component, bean);
  }

  @Override
  protected String getHeaderText(
    UIComponent component,
    FacesBean   bean)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    return arc.getTranslatedString("af_table.DETAIL_COLUMN_HEADER");
  }

  @Override
  protected boolean getNoWrap(
    UIComponent component,
    FacesBean   bean)
  {
    return true;
  }

  @Override
  protected void renderKids(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           column
    ) throws IOException
  {
    // Delegate to a ShowDetailRenderer, using the table as the underlying
    // component
    delegateRenderer(context,
                     rc,
                     tContext.getTable(),
                     getFacesBean(tContext.getTable()),
                     _detailRenderer);
  }

  static private class Detail extends ShowDetailRenderer
  {
    public Detail()
    {
      super(CoreTable.TYPE);
    }

    @Override
    protected void renderAllAttributes(
       FacesContext     context,
       RenderingContext rc,
      UIComponent       component,
       FacesBean        bean)
    {

    }

    @Override
    protected void renderPromptStart(
      FacesContext     context,
      RenderingContext rc,
      UIComponent      component,
      FacesBean        bean
      ) throws IOException
    {
      // bug 4688350:
      component = ((UIXTable) component).getDetailStamp();
      super.renderPromptStart(context, rc, component, bean);
    }

    @Override
    protected String getValueParameter(
      UIComponent component)
    {
      UIXCollection cb = (UIXCollection) component;
      int rowIndex = cb.getRowIndex();
      return IntegerUtils.getString(rowIndex);
    }

    @Override
    protected boolean isTableDetailDisclosure()
    {
      return true;
    }

    @Override
    protected boolean getDisclosed(
      UIComponent component,
      FacesBean   bean)
    {
      TableRenderingContext tContext = TableRenderingContext.getCurrentInstance();
      UIXTable table = (UIXTable) tContext.getCollectionComponent();
      return (table.getDisclosedRowKeys().isContained());
    }

    @Override
   protected String getLinkId(
      String  rootId,
      boolean disclosed)
   {
     return _getDetailLinkId(TableRenderingContext.getCurrentInstance());
   }

    @Override
    protected String getClientId(
      FacesContext context,
      UIComponent  component)
    {
      TableRenderingContext tContext = TableRenderingContext.getCurrentInstance();
      return tContext.getTableId();
    }

    // Returns the ID for the detail-disclosure link.  We use IDs of the
    // form: "<table name>dd<row index>"
    private static String _getDetailLinkId(
      TableRenderingContext tContext)
    {
      String tableId = tContext.getTableId();
      if (tableId == null)
        return null;

      int currentRow = tContext.getCollectionComponent().getRowIndex();
      String currentRowString = IntegerUtils.getString(currentRow);

      StringBuffer buffer = new StringBuffer(tableId.length()        +
                                             currentRowString.length() +
                                             2);

      buffer.append(tableId);
      buffer.append("dd");
      buffer.append(currentRowString);

      return buffer.toString();
    }
  }

  private final CoreRenderer _detailRenderer = new Detail();
}
