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

import java.awt.Font;


/**
 * A utility class for working with fonts.
 * <p>
 * FontUtils contains utility functions for:
 * <ul>
 * <li>Getting default instances of the different standard fonts
 * <li>Getting the average width of a glyph in a font
 * </ul>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/FontUtils.java#0 $) $Date: 10-nov-2005.19:04:56 $
 */
public class FontUtils
{
  private FontUtils()
  {
  }



  /**
   * Returns the default sans serif font.  This is the font to be used
   * in place of the deprecated "Helvetica" font.
   */
  public static Font getDefaultSansSerifFont()
  {
    if (_defaultSansSerifFont == null)
    {
      _defaultSansSerifFont = new Font("SansSerif", Font.PLAIN, 12);
    }

    return _defaultSansSerifFont;
  }

  private static Font _defaultSansSerifFont;
}
