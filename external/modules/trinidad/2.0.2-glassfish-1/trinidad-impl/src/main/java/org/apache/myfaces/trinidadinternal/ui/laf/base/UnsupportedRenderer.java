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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import java.io.IOException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Renderer used to identify Nodes that aren't supported by the device.
 * These nodes will not render themselves, nor their children.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/UnsupportedRenderer.java#0 $) $Date: 10-nov-2005.18:53:10 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class UnsupportedRenderer implements Renderer
{
  /**
   * Render nothing.
   */
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // not supported so kick out warning
    if (_LOG.isSevere())
      _LOG.severe("UNSUPPORTED_UINODE", new Object[]{node.getLocalName(), context.getPath()});
  }

  public static Renderer getInstance()
  {
    return _INSTANCE;
  }

  private UnsupportedRenderer()
  {
  }

  private static final UnsupportedRenderer _INSTANCE=new UnsupportedRenderer();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UnsupportedRenderer.class);
}
