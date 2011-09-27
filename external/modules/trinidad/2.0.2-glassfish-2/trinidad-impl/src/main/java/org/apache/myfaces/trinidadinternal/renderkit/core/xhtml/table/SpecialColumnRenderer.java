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

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.data.CoreColumn;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.ColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;

public class SpecialColumnRenderer extends ColumnRenderer
{
  // previously,the special column used to be public static, and we
  // shared it to render the selection, detail and focus columns.
  // However, this resulted in those columns sharing the same client ID
  // as well, which is bad. So each column, now gets its own special column
  // instance:
  private final UIComponent _specialColumn = new CoreColumn();

  public UIComponent getSpecialColumn()
  {
    return _specialColumn;
  }

  @Override
  protected boolean getHeaderNoWrap(
    UIComponent component,
    FacesBean   bean)
  {
    return true;
  }

  @Override
  protected boolean getSortable(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected String getSortProperty(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  @Override
  protected String getHeaderInlineStyle(
    RenderingContext rc)
  {
    if (XhtmlRenderer.isIE(rc))
      return "word-break:keep-all"; // bugs 2342291, 1999842

    return null;
  }

  @Override
  protected boolean isSpecialColumn()
  {
    return true;
  }
}
