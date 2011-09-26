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

import java.util.Map;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SimpleSelectBooleanCheckboxRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;


public class TableSelectOneRenderer extends XhtmlRenderer
{
  public TableSelectOneRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _renderer = createCellRenderer(type);
  }

  //
  // Decode
  //
  @SuppressWarnings("unchecked")
  @Override
  protected void decode(
    FacesContext facesContext,
    UIComponent  component,
    @SuppressWarnings("unused")
    FacesBean    facesBean,
    @SuppressWarnings("unused")
    String       clientId)
  {
    UIXCollection table = (UIXCollection) component;
    Object oldKey = table.getRowKey();

    try
    {
      // Set the row key to null to force the clientId to be correct
      table.setRowKey(null);

      String selectionParam = __getSelectionParameterName(facesContext, table);

      Map<String, String> parameters =
        facesContext.getExternalContext().getRequestParameterMap();

      _LOG.finest("Params:{0}", parameters);

      String selection = parameters.get(selectionParam);

      if (selection != null)
      {
        final RowKeySet state;
        if (table instanceof UIXTable)
          state = ((UIXTable) table).getSelectedRowKeys();
        else
          state = ((UIXTree) table).getSelectedRowKeys();

        table.setClientRowKey(selection);
        // If the key is not already selected, or the state is more than one
        // (someone changed the table selection from multiple to single),
        // update the keys
        if (!state.isContained() || state.size() > 1)
        {
          RowKeySet unselected = state.clone();
          // TODO : do not mutate the selectedRowKeys here.
          // instead, mutate when event is broadcast:
          state.clear();
          state.add();
          // clone, so that subsequent mutations of "state" will
          // not affect the parameters of this event: bug 4733858:
          RowKeySet selected = state.clone();
          FacesEvent event = new SelectionEvent(table, unselected, selected);
          event.queue();
        }
      }
    }
    finally
    {
      table.setRowKey(oldKey);
    }
  }

  //
  // Encode
  //
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

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

    if (tContext == null)
    {
      _LOG.severe("TABLESELECT_COMPONENT_MAY_ONLY_INSIDE_TABLE_AND_TREETABLE");
      return;
    }

    RenderStage stage = tContext.getRenderStage();
    switch(stage.getStage())
    {
      case RenderStage.SUB_CONTROL_BAR_STAGE:
      case RenderStage.UPPER_CONTROL_BAR_STAGE:
      case RenderStage.LOWER_CONTROL_BAR_STAGE:
        break;

      case RenderStage.DATA_STAGE:
        renderCellContent(context, rc, tContext, component, bean);
        break;

      default:
        throw new AssertionError("bad renderStage:"+stage.getStage());
    }
  }

  protected boolean isSelectOne()
  {
    return true;
  }

  protected CoreRenderer createCellRenderer(
    FacesBean.Type type)
  {
    return new Radio(type);
  }

  protected void renderCellContent(
    FacesContext          context,
    RenderingContext      rc,
    TableRenderingContext tContext,
    UIComponent           component,
    FacesBean             bean
    ) throws IOException
  {
    rc.setCurrentClientId(tContext.getTableId());
    delegateRenderer(context, rc, component, bean, _renderer);
    rc.setCurrentClientId(null);
  }

  /**
   * Get the name of the parameter for the selection;  package-private
   * for testing.
   */
  static String __getSelectionParameterName(
    FacesContext context,
    UIComponent  table)
  {
    return (table.getClientId(context) +
            NamingContainer.SEPARATOR_CHAR +
            XhtmlConstants.SELECTED_KEY);
  }

  public static class Radio extends SimpleSelectBooleanCheckboxRenderer
  {
    public Radio(
      FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected String getCompositeId(
      String clientId)
    {
      return null;
    }

    /**
     * we do not want to render the simple span for the checkbox.
     */
    @Override
    protected boolean getRenderSimpleSpan(
      UIComponent component,
      FacesBean   bean)
    {
      return false;
    }

    /**
     * don't render a special content style class on the radio.
     */
    @Override
    protected String getContentStyleClass(
      UIComponent component,
      FacesBean   bean)
    {
     return null;
    }

    @Override
    protected void renderId(
      FacesContext context,
      UIComponent  component
      ) throws IOException
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      String param = (tContext.getTableId() +
                      NamingContainer.SEPARATOR_CHAR +
                      XhtmlConstants.SELECTED_KEY);
      ResponseWriter writer = context.getResponseWriter();
      writer.writeAttribute("name", param, null);
      // =-=AEW Inefficient.  We only need the "id" when there's
      // a shortDescription (which is when we'll get a label)
      if (getShortDesc(component, getFacesBean(component)) != null)
        writer.writeAttribute("id", getClientId(context, component), null);
    }

    @Override
    protected String getClientId(
      FacesContext context,
      UIComponent  component)
    {
      // We use the table's container client ID
      return component.getContainerClientId(context);
    }

    @Override
    protected Object getSubmittedValue(
    UIComponent component,
    FacesBean   bean)
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      return tContext.getSelectedRowKeys().isContained() ?
        Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    protected Object getType()
    {
      return "radio";
    }

    @Override
    protected Object getValueAttr(
      RenderingContext rc)
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      return ((UIXCollection) tContext.getCollectionComponent()).
                getClientRowKey();
    }

    @Override
    protected String getShortDesc(
    UIComponent component,
    FacesBean   bean)
    {
      String key = getDefaultShortDescKey();
      RenderingContext arc = RenderingContext.getCurrentInstance();
      return arc.getTranslatedString(key);
    }

    protected String getDefaultShortDescKey()
    {
      return "af_tableSelectOne.SELECT_COLUMN_HEADER";
    }

    @Override
    protected char getAccessKey(
      UIComponent component,
      FacesBean   bean)
    {
      return CHAR_UNDEFINED;
    }

    @Override
    protected boolean isImmediate(
      UIComponent component,
      FacesBean   bean)
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      return tContext.isImmediate();
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
    protected boolean getDisabled(
      UIComponent component,
      FacesBean   bean)
    {
      return false;
    }

    /**
     * @todo Support?
     */
    @Override
    protected String getOnblur(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    /**
     * @todo Support?
     */
    @Override
    protected String getOnfocus(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getOnchange(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    protected String getOnselect(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }

    @Override
    protected String getText(
      UIComponent component,
      FacesBean   bean)
    {
      return null;
    }
  }

  private CoreRenderer _renderer;

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TableSelectOneRenderer.class);
}
