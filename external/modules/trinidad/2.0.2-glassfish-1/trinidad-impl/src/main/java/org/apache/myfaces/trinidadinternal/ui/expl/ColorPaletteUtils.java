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
package org.apache.myfaces.trinidadinternal.ui.expl;

import java.awt.Color;

import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.util.ArrayMap;



/**
 * Utility class which defines built-in color palettes
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/expl/ColorPaletteUtils.java#0 $) $Date: 10-nov-2005.18:56:26 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ColorPaletteUtils
{
  public static Map<String, List<Color>> getColorPaletteMap()
  {
    return _PALETTE_MAP;
  }

  private ColorPaletteUtils() {}

  // Color palette names
  private static final String _DEFAULT_49_NAME = "default49";
  private static final String _OPAQUE_49_NAME  = "opaque49";
  private static final String _DEFAULT_80_NAME = "default80";
  private static final String _WEB_216_NAME    = "web216";

  // Colors for the default 49 color palette
  private static final Color[] _DEFAULT_49_COLORS = new Color[]
  {
    new Color(0,0,0,0),
    new Color(255,204,204),
    new Color(255,204,153),
    new Color(255,255,204),
    new Color(153,255,153),
    new Color(204,255,255),
    new Color(255,204,255),
    // 2nd-row
    new Color(255,255,255),
    new Color(255,102,102),
    new Color(255,204,51),
    new Color(255,255,153),
    new Color(102,255,153),
    new Color(102,255,255),
    new Color(255,153,255),
    // 3rd-row
    new Color(204,204,204),
    new Color(255,0,0),
    new Color(255,153,0),
    new Color(255,255,0),
    new Color(51,255,51),
    new Color(51,204,255),
    new Color(204,102,204),
    // 4th-row
    new Color(153,153,153),
    new Color(204,0,0),
    new Color(255,102,0),
    new Color(255,204,0),
    new Color(0,204,0),
    new Color(51,102,255),
    new Color(204,51,204),
    // 5th-row
    new Color(102,102,102),
    new Color(153,0,0),
    new Color(204,102,0),
    new Color(153,153,0),
    new Color(0,153,0),
    new Color(51,51,255),
    new Color(153,51,102),
    // 6th-row
    new Color(51,51,51),
    new Color(102,0,0),
    new Color(153,51,0),
    new Color(102,102,0),
    new Color(0,102,0),
    new Color(0,0,153),
    new Color(102,51,102),
    // 7th-row
    new Color(0,0,0),
    new Color(51,0,0),
    new Color(102,51,0),
    new Color(51,51,0),
    new Color(0,51,0),
    new Color(0,0,102),
    new Color(51,0,51),
  };

  private static final Color[] _DEFAULT_80_COLORS = new Color[]
  {
    new Color(0,0,0,0),
    new Color(255,204,204),
    new Color(255,204,153),
    new Color(255,255,204),
    new Color(204,255,204),
    new Color(153,255,153),
    new Color(204,255,255),
    new Color(153,204,255),
    new Color(204,204,255),
    new Color(255,204,255),
    // 2nd-row
    new Color(255,255,255),
    new Color(255,153,153),
    new Color(255,153,102),
    new Color(255,255,153),
    new Color(153,255,153),
    new Color(102,255,204),
    new Color(153,255,255),
    new Color(102,204,255),
    new Color(153,153,255),
    new Color(255,153,255),
    // 3rd-row
    new Color(204,204,204),
    new Color(255,102,102),
    new Color(255,102,51),
    new Color(255,255,102),
    new Color(102,255,102),
    new Color(102,204,204),
    new Color(102,255,255),
    new Color(51,153,255),
    new Color(153,102,255),
    new Color(255,102,255),
    // 4th-row
    new Color(102,102,102),
    new Color(255,0,0),
    new Color(255,102,0),
    new Color(255,255,51),
    new Color(0,255,0),
    new Color(0,204,204),
    new Color(0,204,255),
    new Color(51,102,255),
    new Color(153,51,255),
    new Color(204,51,204),
    // 5th-row
    new Color(102,102,102),
    new Color(204,0,0),
    new Color(255,51,0),
    new Color(255,255,0),
    new Color(0,153,0),
    new Color(0,153,153),
    new Color(0,153,255),
    new Color(0,0,204),
    new Color(153,0,204),
    new Color(255,0,153),
    // 6th-row
    new Color(51,51,51),
    new Color(153,0,0),
    new Color(204,51,0),
    new Color(255,204,0),
    new Color(0,102,0),
    new Color(0,102,102),
    new Color(0,102,255),
    new Color(0,0,153),
    new Color(102,51,153),
    new Color(204,0,153),
    // 7th-row
    new Color(51,51,51),
    new Color(102,0,0),
    new Color(153,51,0),
    new Color(204,153,0),
    new Color(0,51,0),
    new Color(51,102,102),
    new Color(0,51,255),
    new Color(0,0,102),
    new Color(102,0,153),
    new Color(153,0,102),
    // 8th-row
    new Color(0,0,0),
    new Color(51,0,0),
    new Color(102,51,0),
    new Color(153,102,51),
    new Color(0,51,0),
    new Color(0,51,51),
    new Color(0,51,153),
    new Color(0,0,102),
    new Color(51,0,102),
    new Color(102,0,102),
  };

  private static final Color[] _WEB_216_COLORS = new Color[]
  {
    new Color(255, 255, 255),
    new Color(255, 255, 204),
    new Color(255, 255, 153),
    new Color(255, 255, 102),
    new Color(255, 255, 51),
    new Color(255, 255, 0),

    new Color(255, 204, 255),
    new Color(255, 204, 204),
    new Color(255, 204, 153),
    new Color(255, 204, 102),
    new Color(255, 204, 51),
    new Color(255, 204, 0),

    new Color(255, 153, 255),
    new Color(255, 153, 204),
    new Color(255, 153, 153),
    new Color(255, 153, 102),
    new Color(255, 153, 51),
    new Color(255, 153, 0),

    new Color(255, 102, 255),
    new Color(255, 102, 204),
    new Color(255, 102, 153),
    new Color(255, 102, 102),
    new Color(255, 102, 51),
    new Color(255, 102, 0),

    new Color(255, 51, 255),
    new Color(255, 51, 204),
    new Color(255, 51, 153),
    new Color(255, 51, 102),
    new Color(255, 51, 51),
    new Color(255, 51, 0),

    new Color(255, 0, 255),
    new Color(255, 0, 204),
    new Color(255, 0, 153),
    new Color(255, 0, 102),
    new Color(255, 0, 51),
    new Color(255, 0, 0),

    new Color(204, 255, 255),
    new Color(204, 255, 204),
    new Color(204, 255, 153),
    new Color(204, 255, 102),
    new Color(204, 255, 51),
    new Color(204, 255, 0),

    new Color(204, 204, 255),
    new Color(204, 204, 204),
    new Color(204, 204, 153),
    new Color(204, 204, 102),
    new Color(204, 204, 51),
    new Color(204, 204, 0),

    new Color(204, 153, 255),
    new Color(204, 153, 204),
    new Color(204, 153, 153),
    new Color(204, 153, 102),
    new Color(204, 153, 51),
    new Color(204, 153, 0),

    new Color(204, 102, 255),
    new Color(204, 102, 204),
    new Color(204, 102, 153),
    new Color(204, 102, 102),
    new Color(204, 102, 51),
    new Color(204, 102, 0),

    new Color(204, 51, 255),
    new Color(204, 51, 204),
    new Color(204, 51, 153),
    new Color(204, 51, 102),
    new Color(204, 51, 51),
    new Color(204, 51, 0),

    new Color(204, 0, 255),
    new Color(204, 0, 204),
    new Color(204, 0, 153),
    new Color(204, 0, 102),
    new Color(204, 0, 51),
    new Color(204, 0, 0),

    new Color(153, 255, 255),
    new Color(153, 255, 204),
    new Color(153, 255, 153),
    new Color(153, 255, 102),
    new Color(153, 255, 51),
    new Color(153, 255, 0),

    new Color(153, 204, 255),
    new Color(153, 204, 204),
    new Color(153, 204, 153),
    new Color(153, 204, 102),
    new Color(153, 204, 51),
    new Color(153, 204, 0),

    new Color(153, 153, 255),
    new Color(153, 153, 204),
    new Color(153, 153, 153),
    new Color(153, 153, 102),
    new Color(153, 153, 51),
    new Color(153, 153, 0),

    new Color(153, 102, 255),
    new Color(153, 102, 204),
    new Color(153, 102, 153),
    new Color(153, 102, 102),
    new Color(153, 102, 51),
    new Color(153, 102, 0),

    new Color(153, 51, 255),
    new Color(153, 51, 204),
    new Color(153, 51, 153),
    new Color(153, 51, 102),
    new Color(153, 51, 51),
    new Color(153, 51, 0),

    new Color(153, 0, 255),
    new Color(153, 0, 204),
    new Color(153, 0, 153),
    new Color(153, 0, 102),
    new Color(153, 0, 51),
    new Color(153, 0, 0),

    new Color(102, 255, 255),
    new Color(102, 255, 204),
    new Color(102, 255, 153),
    new Color(102, 255, 102),
    new Color(102, 255, 51),
    new Color(102, 255, 0),

    new Color(102, 204, 255),
    new Color(102, 204, 204),
    new Color(102, 204, 153),
    new Color(102, 204, 102),
    new Color(102, 204, 51),
    new Color(102, 204, 0),

    new Color(102, 153, 255),
    new Color(102, 153, 204),
    new Color(102, 153, 153),
    new Color(102, 153, 102),
    new Color(102, 153, 51),
    new Color(102, 153, 0),

    new Color(102, 102, 255),
    new Color(102, 102, 204),
    new Color(102, 102, 153),
    new Color(102, 102, 102),
    new Color(102, 102, 51),
    new Color(102, 102, 0),

    new Color(102, 51, 255),
    new Color(102, 51, 204),
    new Color(102, 51, 153),
    new Color(102, 51, 102),
    new Color(102, 51, 51),
    new Color(102, 51, 0),

    new Color(102, 0, 255),
    new Color(102, 0, 204),
    new Color(102, 0, 153),
    new Color(102, 0, 102),
    new Color(102, 0, 51),
    new Color(102, 0, 0),

    new Color(51, 255, 255),
    new Color(51, 255, 204),
    new Color(51, 255, 153),
    new Color(51, 255, 102),
    new Color(51, 255, 51),
    new Color(51, 255, 0),

    new Color(51, 204, 255),
    new Color(51, 204, 204),
    new Color(51, 204, 153),
    new Color(51, 204, 102),
    new Color(51, 204, 51),
    new Color(51, 204, 0),

    new Color(51, 153, 255),
    new Color(51, 153, 204),
    new Color(51, 153, 153),
    new Color(51, 153, 102),
    new Color(51, 153, 51),
    new Color(51, 153, 0),

    new Color(51, 102, 255),
    new Color(51, 102, 204),
    new Color(51, 102, 153),
    new Color(51, 102, 102),
    new Color(51, 102, 51),
    new Color(51, 102, 0),

    new Color(51, 51, 255),
    new Color(51, 51, 204),
    new Color(51, 51, 153),
    new Color(51, 51, 102),
    new Color(51, 51, 51),
    new Color(51, 51, 0),

    new Color(51, 0, 255),
    new Color(51, 0, 204),
    new Color(51, 0, 153),
    new Color(51, 0, 102),
    new Color(51, 0, 51),
    new Color(51, 0, 0),

    new Color(0, 255, 255),
    new Color(0, 255, 204),
    new Color(0, 255, 153),
    new Color(0, 255, 102),
    new Color(0, 255, 51),
    new Color(0, 255, 0),

    new Color(0, 204, 255),
    new Color(0, 204, 204),
    new Color(0, 204, 153),
    new Color(0, 204, 102),
    new Color(0, 204, 51),
    new Color(0, 204, 0),

    new Color(0, 153, 255),
    new Color(0, 153, 204),
    new Color(0, 153, 153),
    new Color(0, 153, 102),
    new Color(0, 153, 51),
    new Color(0, 153, 0),

    new Color(0, 102, 255),
    new Color(0, 102, 204),
    new Color(0, 102, 153),
    new Color(0, 102, 102),
    new Color(0, 102, 51),
    new Color(0, 102, 0),

    new Color(0, 51, 255),
    new Color(0, 51, 204),
    new Color(0, 51, 153),
    new Color(0, 51, 102),
    new Color(0, 51, 51),
    new Color(0, 51, 0),

    new Color(0, 0, 255),
    new Color(0, 0, 204),
    new Color(0, 0, 153),
    new Color(0, 0, 102),
    new Color(0, 0, 51),
    new Color(0, 0, 0),
  };

  // Colors for the opaque 49 color palette
  private static final Color[] _OPAQUE_49_COLORS = new Color[]
  {
    new Color(255,255,255),
    new Color(255,204,204),
    new Color(255,204,153),
    new Color(255,255,204),
    new Color(153,255,153),
    new Color(204,255,255),
    new Color(255,204,255),
    // 2nd-row
    new Color(255,255,255),
    new Color(255,102,102),
    new Color(255,204,51),
    new Color(255,255,153),
    new Color(102,255,153),
    new Color(102,255,255),
    new Color(255,153,255),
    // 3rd-row
    new Color(204,204,204),
    new Color(255,0,0),
    new Color(255,153,0),
    new Color(255,255,0),
    new Color(51,255,51),
    new Color(51,204,255),
    new Color(204,102,204),
    // 4th-row
    new Color(153,153,153),
    new Color(204,0,0),
    new Color(255,102,0),
    new Color(255,204,0),
    new Color(0,204,0),
    new Color(51,102,255),
    new Color(204,51,204),
    // 5th-row
    new Color(102,102,102),
    new Color(153,0,0),
    new Color(204,102,0),
    new Color(153,153,0),
    new Color(0,153,0),
    new Color(51,51,255),
    new Color(153,51,102),
    // 6th-row
    new Color(51,51,51),
    new Color(102,0,0),
    new Color(153,51,0),
    new Color(102,102,0),
    new Color(0,102,0),
    new Color(0,0,153),
    new Color(102,51,102),
    // 7th-row
    new Color(0,0,0),
    new Color(51,0,0),
    new Color(102,51,0),
    new Color(51,51,0),
    new Color(0,51,0),
    new Color(0,0,102),
    new Color(51,0,51),
  };

  // ArrayMap-style Object array which maps color palette names
  // to DataObjectLists which contain the colors
  private static final Object[] _PALETTE_ARRAY = new Object[]
  {
    _DEFAULT_49_NAME,
    Arrays.asList(_DEFAULT_49_COLORS),
    _OPAQUE_49_NAME,
    Arrays.asList(_OPAQUE_49_COLORS),
    _DEFAULT_80_NAME,
    Arrays.asList(_DEFAULT_80_COLORS),
    _WEB_216_NAME,
    Arrays.asList(_WEB_216_COLORS),
  };

  // The Map of color palette names to color palettes.
  // Use the same technique for creating an unmodifiable
  // Map that we use for the Maps in UIImplicitObject.
  private static final Map<String, List<Color>> _PALETTE_MAP = 
    new AbstractMap<String, List<Color>>()
  {
    @SuppressWarnings("unchecked")
    @Override
    public Set<Map.Entry<String, List<Color>>> entrySet()
    {
      return Collections.EMPTY_SET;
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<Color> get(Object key)
    {
      return (List<Color>)ArrayMap.get(_PALETTE_ARRAY, key);
    }
  };
}
