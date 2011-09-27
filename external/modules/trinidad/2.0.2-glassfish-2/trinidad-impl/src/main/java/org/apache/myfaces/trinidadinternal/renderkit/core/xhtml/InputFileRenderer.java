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
import org.apache.myfaces.trinidad.component.core.input.CoreInputFile;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.LocaleInfoScriptlet;


public class InputFileRenderer extends InputLabelAndMessageRenderer
{
  public InputFileRenderer()
  {
    super(CoreInputFile.TYPE);
  }

  protected InputFileRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _simpleInputFile = new SimpleInputFileRenderer(type);
  }

  @Override
  protected final void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // =-=AEW inputFile is currently disabled for PDAs.  But this should
    // run off of an agent property.
    if (!isPDA(rc))
    {
      // ensure that the translation is sent down to the client as posting
      // an IFRAME with an invalid file can cause errors that need to be shown
      // to the user with JavaScript
      XhtmlUtils.addLib(context, rc, LocaleInfoScriptlet.LOCALE_INFO_KEY);

      super.encodeAll(context, rc, component, bean);
    }
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputFile";
  }

  @Override
  protected FormInputRenderer getFormInputRenderer()
  {
    return _simpleInputFile;
  }

  /**
   *
   * @param bean
   * @return false, since inputFile does not support the readOnly attribute
   */
  @Override
  protected boolean isReadOnly(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  private SimpleInputFileRenderer _simpleInputFile;
}
