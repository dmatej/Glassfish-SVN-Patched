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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Renderer for processChoiceBar
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ProcessChoiceBarRenderer.java#0 $) $Date: 10-nov-2005.18:55:35 $
 * @todo copied from NavigationBarRenderer. will need to refactor to share
 * code.
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ProcessChoiceBarRenderer extends
   org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ProcessChoiceBarRenderer
{

  /**
   * Writes the separator between two elements
   */
  @Override
  protected void renderItemSpacer(
    UIXRenderingContext context
    ) throws IOException
  {
    renderSpacer(context, 5, 1);
  }
  
}
