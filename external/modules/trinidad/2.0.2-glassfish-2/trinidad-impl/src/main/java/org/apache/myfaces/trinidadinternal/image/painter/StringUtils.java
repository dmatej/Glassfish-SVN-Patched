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
package org.apache.myfaces.trinidadinternal.image.painter;

import java.util.Locale;

/**
 * StringUtils provides conversion functions for transforming
 * bidirectional strings so they can be drawn correctly, as well
 * as utilities for processing mnemonics in strings.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/StringUtils.java#0 $) $Date: 10-nov-2005.19:05:03 $
 */
class StringUtils
{

  /**
   * Gets a display string from an original (logical) string.  Accounts
   * for Arabic and Hebrew glyph positions within words, returning
   * a string that can be used for measuring and drawing.
   * @param text a logical string
   * @param context a painting context
   * @see org.apache.myfaces.trinidadinternal.image.painter.PaintContext
   */
  public static String getDisplayString(
    String       text,
    PaintContext context
    )
  {
    return getDisplayString(text,
                            context.getPaintLocale(),
                            context.getReadingDirection());
  }


  /**
   * Gets a display string from an original (logical) string.  Accounts
   * for Arabic and Hebrew glyph positions within words, returning
   * a string that can be used for measuring and drawing.
   * @param text a logical string
   * @param direction a direction constant
   */
  public static String getDisplayString(
    String text,
    Locale locale,
    int    direction
    )
  {
    return text;
  }
  
  private StringUtils()
  {
  }
}
