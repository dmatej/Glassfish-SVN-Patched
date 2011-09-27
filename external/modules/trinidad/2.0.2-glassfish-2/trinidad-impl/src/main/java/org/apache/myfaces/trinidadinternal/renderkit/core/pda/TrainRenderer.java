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
package org.apache.myfaces.trinidadinternal.renderkit.core.pda;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXProcess;
import org.apache.myfaces.trinidad.component.core.nav.CoreTrain;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;


public class TrainRenderer
  extends XhtmlRenderer
{
  /**
   * Constructor.
   */
  public TrainRenderer()
  {
    super(CoreTrain.TYPE);
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    if (rc.getFormData() == null)
    {
      _LOG.warning("TRAIN_MUST_INSIDE_FORM");
      return;
    }

    UIXProcess process = (UIXProcess) component;
    UIComponent stamp = process.getNodeStamp();

    if(stamp != null)
    {
      Object oldPath = process.getRowKey();
      boolean isNewPath = _setNewPath(process);
      if (isNewPath)
      {
        int selectedIndex = process.getRowIndex();
        int length = process.getRowCount();
        String pattern;
        String[] parameters;

        selectedIndex++;

        if (length == XhtmlConstants.MAX_VALUE_UNKNOWN)
        {
          pattern = rc.getTranslatedString(
            _SINGLE_RANGE_FORMAT_NO_TOTAL_STRING);

          parameters = new String[]
          {
            rc.getTranslatedString(_STEP_TEXT_KEY),
            IntegerUtils.getString(selectedIndex)
          };

        }
        else
        {
          pattern = rc.getTranslatedString(_SINGLE_RANGE_FORMAT_TOTAL_STRING);

          parameters = new String[]
          {
            rc.getTranslatedString(_STEP_TEXT_KEY),
            IntegerUtils.getString(selectedIndex),
            IntegerUtils.getString(length)
          };
        }

        ResponseWriter writer = context.getResponseWriter();
        writer.startElement("span", null);
        renderInlineStyle(context, rc, component, bean);
        String outputText = XhtmlUtils.getFormattedString(pattern, parameters);
        writer.writeText(outputText, null);
        writer.endElement("span");
        process.setRowKey(oldPath);
      }
    }
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  private boolean _setNewPath(
    UIXProcess component)
  {
    Object focusPath = component.getFocusRowKey();
    component.setRowKey(focusPath);
    return true;
  }

  static private final String _STEP_TEXT_KEY =
    "af_train.STEP";
  static private final String _SINGLE_RANGE_FORMAT_TOTAL_STRING =
    "af_train.FORMAT_TOTAL";
  static private final String _SINGLE_RANGE_FORMAT_NO_TOTAL_STRING =
    "af_train.FORMAT_NO_TOTAL";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TrainRenderer.class);
}
