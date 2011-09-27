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

import org.apache.myfaces.trinidadinternal.renderkit.RenderKitDecorator;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

public class PdaRenderKit extends RenderKitDecorator
{
  public PdaRenderKit()
  {
    _addRenderer("Frame",
                 "Frame",             "FrameRenderer");
    _addRenderer("Panel",
                 "ButtonBar",         "PanelButtonBarRenderer");
    _addRenderer("Column",
                 "Column",            "PdaColumnRenderer");
    _addRenderer("FrameBorderLayout",
                 "FrameBorderLayout", "PdaFrameBorderLayoutRenderer");
    _addRenderer("Messages",
                 "Messages",          "PdaMessageBoxRenderer");
    _addRenderer("NavigationLevel",
                 "Pane",              "PdaNavigationPaneRenderer");
    _addRenderer("Table",
                 "Table",             "PdaTableRenderer");
    _addRenderer("Process",
                 "Train",             "TrainRenderer");
    _addRenderer("Command",
                 "Link",              "PdaCommandLinkRenderer");
  }

  private void _addRenderer(
     String family,
     String rendererType,
     String rendererClassName)
  {

    addRenderer(_trBase + family,
                _trBase + rendererType,
                _pdaBase + rendererClassName);
  }

  @Override
  protected String getDecoratedRenderKitId()
  {
    return CoreRenderKit.BASE_RENDER_KIT_ID;
  }

  static private final String _trBase = "org.apache.myfaces.trinidad.";
  static private final String _pdaBase =
             "org.apache.myfaces.trinidadinternal.renderkit.core.pda.";
}
