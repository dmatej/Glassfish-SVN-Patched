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
package org.apache.myfaces.trinidadinternal.renderkit.uix;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.component.UIXProcess;

import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ProcessUtils;
import org.apache.myfaces.trinidadinternal.uinode.UINodeRendererBase;

/**
 * Renderer for process components: processTrain and processChoiceBar
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/ProcessRenderer.java#0 $) $Date: 10-nov-2005.19:00:32 $
 */
public class ProcessRenderer extends UINodeRendererBase
{
  /**
   */
  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    Map<String, String> requestMap = 
      context.getExternalContext().getRequestParameterMap();

    Object event = requestMap.get("event");

    if ((event != null) &&
        event.equals(UIConstants.GOTO_EVENT))
    {
      Object source = requestMap.get("source");

      if ( source != null &&
           source.equals(component.getClientId(context)))
      {

        Object valueObject = requestMap.get("value");

        // we piggyback on the size parameter.
        // 0 means we are moving to a previous step, 1 means we are
        // moving to the next step.
        Object sizeObject = requestMap.get("size");

        if ( valueObject != null)
        {
          int value = -1;

          try
          {
            value = Integer.parseInt(valueObject.toString());
          }
          catch ( NumberFormatException nfe)
          {
            _LOG.severe(nfe);
          }

          int size = 0;

          try
          {
            size = Integer.parseInt(sizeObject.toString());
          }
          catch ( NumberFormatException nfe)
          {
            _LOG.warning(nfe);
          }

          if (size < 0)
            size = 0;

          if ( value >= 0 )
          {
            UIXProcess process = (UIXProcess)component;
            Object oldPath = process.getRowKey();
            Object focusPath = process.getFocusRowKey();
            process.setRowKey(focusPath);
            UIComponent stamp = process.getNodeStamp();
            int index = process.getRowIndex();

            if (size == 0)
            {
              index = ProcessUtils.getBackIndex(process, stamp, index);
            }
            else
            {
              index = ProcessUtils.getNextIndex(process, stamp, index);
            }

            process.setRowIndex(index);
            new ActionEvent(stamp).queue();
            process.setRowKey(oldPath);
          }
        }
      }
    }
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ProcessRenderer.class);
}
