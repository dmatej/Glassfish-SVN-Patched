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
package org.apache.myfaces.trinidadinternal.image.cache;

import java.awt.Color;

import java.util.Map;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * Colorized icon key class for core-color colorized icons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/CoreColorizedIconKey.java#0 $) $Date: 10-nov-2005.19:06:05 $
 */
public class CoreColorizedIconKey extends BaseColorizedIconKey
{
  /**
   * Creates a key using the specified properties.
   */
  public CoreColorizedIconKey(
    ImageContext context,
    Map<Object, Object> properties
    )
  {
    super(context, properties);
  }

  /**
   * Creates a key for the specified context, source and direction.
   */
  protected CoreColorizedIconKey(
    ImageContext context,
    String source,
    Class<?> lookAndFeel,
    int    direction,
    Color  color,
    Color  surroundingColor)
  {
    super(context, source, lookAndFeel, direction, color, surroundingColor);
  }

  /**
   * Creates a key for the specified context, source and direction.
   * @deprecated in favor of
   * {@link #CoreColorizedIconKey( ImageContext, String, Class, int, Color, Color)}
   */
  @Deprecated
  protected CoreColorizedIconKey(
    ImageContext context,
    String source,
    int    direction,
    Color  color,
    Color  surroundingColor)
  {
    super(context, source, null, direction, color, surroundingColor);
  }

  /**
   * Returns the color key for core colorized icons:
   * ImageConstants.DARK_COLOR_KEY.
   */
  @Override
  public Object getColorKey()
  {
    return ImageConstants.DARK_COLOR_KEY;
  }
}
