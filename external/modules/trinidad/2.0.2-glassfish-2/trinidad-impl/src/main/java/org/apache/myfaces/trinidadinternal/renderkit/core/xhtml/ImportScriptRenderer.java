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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.CoreImportScript;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 *
 */
public class ImportScriptRenderer extends XhtmlRenderer
{
  public ImportScriptRenderer()
  {
    super(CoreImportScript.TYPE);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    _namesKey = type.findKey("names");
  }

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
    Object names = bean.getProperty(_namesKey);
    if (names instanceof Object[])
    {
      Object[] array = (Object[]) names;
      for (Object name : array)
        XhtmlUtils.addLib(context, rc, name);
    }
    else if (names != null)
    {
      XhtmlUtils.addLib(context, rc, names);
    }
  }

  private PropertyKey _namesKey;
}