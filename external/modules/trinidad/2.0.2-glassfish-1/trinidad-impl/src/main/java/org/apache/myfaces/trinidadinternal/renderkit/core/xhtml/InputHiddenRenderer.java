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
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.input.CoreInputHidden;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;


public class InputHiddenRenderer extends EditableValueRenderer
{
  public InputHiddenRenderer()
  {
    super(CoreInputHidden.TYPE);
  }

  @Override
  protected boolean wasSubmitted(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected final void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String id = getClientId(context, component);
    if (canSkipRendering(rc, id))
      return;

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("input", component);
    rw.writeAttribute("type", "hidden", null);
    rw.writeAttribute("id", id, "id");
    rw.writeAttribute("name", id, "id");
    rw.writeAttribute("value",
                      getConvertedString(context, component, bean),
                      "value");
    rw.endElement("input");

    FormData fd = rc.getFormData();
    if (fd != null)
      fd.addRenderedValue(id);

  }
}
