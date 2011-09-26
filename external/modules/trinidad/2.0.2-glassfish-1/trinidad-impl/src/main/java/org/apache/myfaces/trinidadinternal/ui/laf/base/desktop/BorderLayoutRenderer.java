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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/BorderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:55:10 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BorderLayoutRenderer extends 
                                org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.BorderLayoutRenderer
{

  /**
   * Returns the default marign indent to use if no CELL_PADDING_ATTR
   * is specified
   */
  @Override
  protected int getDefaultMarginIndent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return _MARGIN_INDENT;
  }

  // # of pixels to use for the margin
  private static final int _BASE_MARGIN_INDENT = 12;
  private static final int _IE_DEFAULT_MARGIN = 10;
  private static final int _MARGIN_INDENT = _BASE_MARGIN_INDENT - 
                                            _IE_DEFAULT_MARGIN;
}
