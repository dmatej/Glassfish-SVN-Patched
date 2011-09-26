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

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

/**
 * Base class for the two colorized icon key classes: CoreColorizedIconKey
 * and AccentColorizedIconKey.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/BaseColorizedIconKey.java#0 $) $Date: 10-nov-2005.19:05:55 $
 */
abstract public class BaseColorizedIconKey
  implements ImageProviderRequest, CacheKey, ImageConstants
{
  /**
   * Creates a BaseColorizedIconKey for the specified ImageContext
   * and properties.
   */
  protected BaseColorizedIconKey(
    ImageContext context,
    Map<Object, Object> properties
    )
  {
    String source = (String)properties.get(SOURCE_KEY);
    Class<?> lookAndFeel = (Class)properties.get(LAF_KEY);
    int direction = CacheUtils.getReadingDirection(context, properties);
    Color color = (Color)properties.get(getColorKey());
    Color surroundingColor = (Color)properties.get(SURROUNDING_COLOR_KEY);

    _init(source, lookAndFeel, direction, color, surroundingColor);
  }

  /**
   * Creates a key for the specified context, source and direction.
   */
  protected BaseColorizedIconKey(
    ImageContext context,
    String   source,
    Class<?> lookAndFeel,
    int      direction,
    Color    color,
    Color    surroundingColor)
  {
    _init(source, lookAndFeel, direction, color, surroundingColor);
  }

  /**
   * Creates a key for the specified context, source and direction.
   * @deprecated in favor of
   * {@link #BaseColorizedIconKey( ImageContext, String, Class, int, Color, Color)}
   */
  @Deprecated
  protected BaseColorizedIconKey(
    ImageContext context,
    String source,
    int    direction,
    Color  color,
    Color  surroundingColor)
  {
    _init(source, null, direction, color, surroundingColor);
  }

  /**
   * Returns the ImageConstants KEY constant for the color represented
   * by this BaseColorizedIconKey instance: DARK_COLOR_KEY or
   * DARK_ACCENT_COLOR_KEY.
   */
  abstract public Object getColorKey();

  /**
   * Implementation of ImageProviderRequest.getNamespaceURI().
   */
  public String getNamespaceURI()
  {
    return TECATE_NAMESPACE;
  }

  /**
   * Implementation of ImageProviderRequest.getLocalName().
   */
  public String getLocalName()
  {
    return COLORIZED_ICON_NAME;
  }

  /**
   * Override of Object.equals().
   */
  @Override
  public boolean equals(Object o)
  {
    // This equals implementation assumes that the same source icon name
    // is not used as both a CoreColorizedIcon and an AccentColorizedIcon.
    if (this == o)
      return true;
    if (!(o instanceof BaseColorizedIconKey))
      return false;

    BaseColorizedIconKey key = (BaseColorizedIconKey)o;

    return (_source.equals(key._source)     &&
             (_direction == key._direction) &&
             _color.equals(key._color)      &&
             _surroundingColor.equals(key._surroundingColor) &&
             _lookAndFeel.equals( key._lookAndFeel));
  }

  /**
   * Override of Object.hashCode().
   */
  @Override
  public int hashCode()
  {
    return (_source.hashCode() |
            _direction         |
            _color.hashCode()  |
            _surroundingColor.hashCode() |
            _lookAndFeel.hashCode());
  }

  /**
   * Implementation of ImageProviderRequest.getRenderProperties().
   */
  public Map<Object, Object> getRenderProperties(ImageContext context)
  {
    Map<Object, Object> properties = new ArrayMap<Object, Object>(_MAP_SIZE);
    properties.put(ImageConstants.SOURCE_KEY, getSource());
    properties.put(getColorKey(), getColor());
    properties.put(SURROUNDING_COLOR_KEY, _surroundingColor);
    properties.put(ImageConstants.DIRECTION_KEY, _direction);
    properties.put(LAF_KEY, _lookAndFeel);

    return properties;
  }

  /**
   * Returns the source property.
   */
  protected String getSource()
  {
    return _source;
  }

  /**
   * Returns the lookAndFeel property.
   */
  protected Class<?> getLookAndFeel()
  {
    return _lookAndFeel;
  }

  /**
   * Returns the direction property.
   */
  public final int getDirection()
  {
    return _direction;
  }

  /**
   * Returns the color property.
   */
  protected final Color getColor()
  {
    return _color;
  }

  /**
   * Sets the source property.
   */
  protected void setSource(String source)
  {
    _source = source;
  }

  /**
   * Sets the lookAndFeel property.
   */
  protected void setLookAndFeel(Class<?> lookAndFeel)
  {
    _lookAndFeel = lookAndFeel;
  }


  /**
   * Sets the direction property.
   */
  protected void setDirection(int direction)
  {
    _direction = direction;
  }

  /**
   * Sets the color property.
   */
  protected void setColor(Color color)
  {
    _color = color;
  }

  private void _init(
    String   source,
    Class<?> lookAndFeel,
    int      direction,
    Color    color,
    Color    surroundingColor
    )
  {
    _lookAndFeel = lookAndFeel;
    _source = source;
    _direction = (direction == LocaleUtils.DIRECTION_DEFAULT) ?
                   LocaleUtils.DIRECTION_LEFTTORIGHT :
                   direction;

    _color = (color == null) ? _NULL_COLOR : color;

    if ((surroundingColor == null) || (surroundingColor.getAlpha() == 0))
      _surroundingColor = CacheUtils.TRANSPARENT_COLOR;
    else
      _surroundingColor = surroundingColor;
  }

  private String      _source;
  private int         _direction;
  private Color       _color;
  private Color       _surroundingColor;
  private Class<?>    _lookAndFeel;

  // The size for the render properties ArrayMap.  We leave room for
  // the SOURCE_INPUT_STREAM_PROVIDER_KEY property, typically provided
  // via a subclass.
  private static final int _MAP_SIZE = 5;

  // This is really just for backwards compatibility.  Existing IMX files
  // may not specify a color, so we substitute this Color to
  // avoid null pointer exceptions
  private static final Color _NULL_COLOR = new Color(0, 0, 0);
}
