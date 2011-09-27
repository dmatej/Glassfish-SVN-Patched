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

import org.apache.myfaces.trinidad.share.io.InputStreamProvider;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.laf.browser.CompositeButtonPainter;
import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

/**
 * Cache key for buttons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/CompositeButtonKey.java#0 $) $Date: 10-nov-2005.19:06:03 $
 */
public class CompositeButtonKey
  implements ImageProviderRequest, CacheKey, ImageConstants
{
  public static final char ACCESS_KEY_UNDEFINED = (char)-1;

  /**
   * Creates a key for the specified Map properties.
   */
  public CompositeButtonKey(
    ImageContext context,
    Map<Object, Object> properties
    )
  {
    String lookAndFeelId = (String)properties.get(LOOK_AND_FEEL_ID_KEY);
    String text = (String)properties.get(TEXT_KEY);
    Color foreground = (Color)properties.get(FOREGROUND_KEY);
    Color background = (Color)properties.get(BACKGROUND_KEY);
    Color surroundingColor = (Color)properties.get(SURROUNDING_COLOR_KEY);
    FontProxy font = CacheUtils.getSharedFont(properties, FONT_KEY);
    int direction = CacheUtils.getReadingDirection(context, properties);
    boolean disabled = CacheUtils.getBoolean(properties, DISABLED_KEY, false);
    boolean textAntialias = CacheUtils.isTextAntialiased(properties);
    char accessKey = _getChar(properties, ACCESS_KEY_KEY);
    String name = (String)properties.get(NAME_KEY);

    InputStreamProvider startIcon = _getIcon(properties,
                                             BUTTON_START_ICON_KEY);
    InputStreamProvider endIcon = _getIcon(properties,
                                           BUTTON_END_ICON_KEY);
    InputStreamProvider topBackgroundIcon = _getIcon(properties,
                                             BUTTON_TOP_BACKGROUND_ICON_KEY);
    InputStreamProvider bottomBackgroundIcon = _getIcon(properties,
                                            BUTTON_BOTTOM_BACKGROUND_ICON_KEY);


    _init(context,
         lookAndFeelId,
         name,
         text,
         foreground,
         background,
         surroundingColor,
         font,
         direction,
         disabled,
         textAntialias,
         accessKey,
         startIcon,
         endIcon,
         topBackgroundIcon,
         bottomBackgroundIcon);
  }

  public CompositeButtonKey(
    ImageContext context,
    String       lookAndFeelId,
    String       name,
    String       text,
    Color        foreground,
    Color        background,
    Color        surroundingColor,
    FontProxy    font,
    boolean      disabled,
    boolean      textAntialias,
    char         accessKey,
    InputStreamProvider startIcon,
    InputStreamProvider endIcon,
    InputStreamProvider topBackgroundIcon,
    InputStreamProvider bottomBackgroundIcon
    )
  {
    _init(context,
          lookAndFeelId,
          name,
          text,
          foreground,
          background,
          surroundingColor,
          font,
          CacheUtils.getReadingDirection(context, null),
          disabled,
          textAntialias,
          accessKey,
          startIcon,
          endIcon,
          topBackgroundIcon,
          bottomBackgroundIcon);
  }

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
    return COMPOSITE_BUTTON_NAME;
  }

  /**
   * Override of Object.equals().
   */
  @Override
  public boolean equals(Object o)
  {
    if (this == o)
      return true;

    if (!(o instanceof CompositeButtonKey))
      return false;

    CompositeButtonKey key = (CompositeButtonKey)o;

    boolean isEqual =
           (_lookAndFeelId.equals(key._lookAndFeelId)       &&
            _text.equals(key._text)                         &&
            _foreground.equals(key._foreground)             &&
            _background.equals(key._background)             &&
            _surroundingColor.equals(key._surroundingColor) &&
            _fontEquals(key._font)                          &&
            (_direction == key._direction)                  &&
            (_disabled  == key._disabled)                   &&
            (_textAntialias == key._textAntialias)          &&
            (_accessKey == key._accessKey));

    return isEqual;
  }

  /**
   * Override of Object.hashCode().
   */
  @Override
  public int hashCode()
  {
    // In order to implement font name wildcarding, we don't hash on font
    // name.  That way, all buttons with the same properties (font name
    // excluded) end up in the same bucket.  Then, we can find matching
    // entries if the font name is null.
    int fontHash = _font.getStyle() ^ _font.getSize();

    // We don't expect the hash to be perfect.  In fact, to speed
    // up hash times, we only do a partial hash.  We expect like
    // images to end up in the the same hash bucket.  Fortunately,
    // we shouldn't have too many variations on a particular
    // image.  Just hash the the attributes that are likely to vary:
    return (_lookAndFeelId.hashCode()    ^
            _text.hashCode()             ^
            _foreground.hashCode()       ^
            _background.hashCode()       ^
            _surroundingColor.hashCode() ^
            fontHash                     ^
            _accessKey);
  }

  /**
   * Implementation of ImageProviderRequest.getRenderProperties().
   */
  public Map<Object, Object> getRenderProperties(ImageContext context)
  {
    ArrayMap<Object, Object> properties = new ArrayMap<Object, Object>(_MAP_SIZE);
    properties.put(LOOK_AND_FEEL_ID_KEY, _lookAndFeelId);
    properties.put(NAME_KEY, _name);
    properties.put(TEXT_KEY, _text);
    properties.put(FOREGROUND_KEY, _foreground);
    properties.put(BACKGROUND_KEY, _background);
    properties.put(SURROUNDING_COLOR_KEY, _surroundingColor);
    properties.put(FONT_KEY, _font);
    CacheUtils.putBoolean(properties, DISABLED_KEY, _disabled);
    CacheUtils.putBoolean(properties, TEXT_ANTIALIAS_KEY, _textAntialias);
    _putChar(properties, ACCESS_KEY_KEY, _accessKey);

    _putStartIcon(properties);
    _putEndIcon(properties);
    _putTopBackgroundIcon(properties);
    _putBottomBackgroundIcon(properties);

    return properties;
  }

  /**
   * Returns the InputStreamProvider for the start icon.
   */
  protected InputStreamProvider getStartIcon()
  {
    return null;
  }

  /**
   * Returns the InputStreamProvider for the end icon.
   */
  protected InputStreamProvider getEndIcon()
  {
    return null;
  }

  /**
   * Returns the InputStreamProvider for the top background icon.
   */
  protected InputStreamProvider getTopBackgroundIcon()
  {
    return null;
  }

  /**
   * Returns the InputStreamProvider for the bottom background icon.
   */
  protected InputStreamProvider getBottomBackgroundIcon()
  {
    return null;
  }

  @Override
  public String toString()
  {
    String propertiesString =
      "lookAndFeelId="        + _lookAndFeelId       +
      "text="                 + _text                +
      ",foreground="          + _foreground          +
      ",background="          + _background          +
      ",surroundingColor="    + _surroundingColor    +
      ",font="                + _font                +
      ",disabled="            + _disabled            +
      ",textAntialias="       + _textAntialias       +
      ",accessKey="           + _accessKey;

    return getClass().getName() + "[" + propertiesString + "]";
  }

  public final String getName()
  {
    return _name;
  }

  public final String getText()
  {
    return _text;
  }

  public final Color getForeground()
  {
    return _foreground;
  }

  public final Color getBackground()
  {
    return _background;
  }

  public final FontProxy getFont()
  {
    return _font;
  }

  public final int getDirection()
  {
    return _direction;
  }

  public final char getAccessKey()
  {
    return _accessKey;
  }

  public final boolean isDisabled()
  {
    return _disabled;
  }

  public final boolean isTextAntialiased()
  {
    return _textAntialias;
  }

  protected void setName(String name)
  {
    _name = name;
  }

  protected void setText(String text)
  {
    _text = text;
  }

  protected void setForeground(Color foreground)
  {
    _foreground = foreground;
  }

  protected void setBackground(Color background)
  {
    _background = background;
  }

  protected void setFont(FontProxy font)
  {
    _font = font;
  }

  protected void setDirection(int direction)
  {
    _direction = direction;
  }

  protected void setAccessKey(char accessKey)
  {
    _setAccessKey(accessKey);
  }

  protected void setDisabled(boolean disabled)
  {
    _disabled = disabled;
  }

  protected void setTextAntialiased(boolean textAntialiased)
  {
    _textAntialias = textAntialiased;
  }

  // Initializes the CompositeButtonKey's fields
  private void _init(
    ImageContext context,
    String       lookAndFeelId,
    String       name,
    String       text,
    Color        foreground,
    Color        background,
    Color        surroundingColor,
    FontProxy    font,
    int          direction,
    boolean      disabled,
    boolean      textAntialias,
    char         accessKey,
    InputStreamProvider startIcon,
    InputStreamProvider endIcon,
    InputStreamProvider topBackgroundIcon,
    InputStreamProvider bottomBackgroundIcon
    )
  {
    _lookAndFeelId = lookAndFeelId;
    _name = name;
    _text = (text == null) ? "" : text;
    _foreground = _getForeground(context, foreground, disabled);
    _background = _getBackground(context, background, disabled);
    _surroundingColor = _getSurroundingColor(surroundingColor);
    _font = _getFont(font);
    _direction = direction;
    _disabled = disabled;
    _textAntialias = textAntialias;
    _setAccessKey(accessKey);

    // Null InputStreamProvider is ok - subclass may create it later
    _startIcon = startIcon;
    _endIcon = endIcon;
    _topBackgroundIcon = topBackgroundIcon;
    _bottomBackgroundIcon = bottomBackgroundIcon;

  }

  private char _getChar(
    Map<Object, Object> properties,
    Object key
    )
  {
    Character value = (Character)properties.get(key);
    if (value == null)
      return ACCESS_KEY_UNDEFINED;

    return value.charValue();
  }

  private void _putChar(
    Map<Object, Object> properties,
    Object key,
    char value
    )
  {
    if (value != ACCESS_KEY_UNDEFINED)
      properties.put(key, Character.valueOf(value));
  }

  // Returns the default Foreground
  private Color _getForeground(
    ImageContext context,
    Color        foreground,
    boolean      disabled
    )
  {
    if (foreground != null)
      return foreground;

    return CompositeButtonPainter.getDefaultForeground(context, disabled);
  }

  // Returns the default Background
  private Color _getBackground(
    ImageContext context,
    Color        background,
    boolean      disabled
    )
  {
    if (background != null)
      return background;

    return CompositeButtonPainter.getDefaultBackground(context, disabled);
  }

  // Returns the surrounding color
  private Color _getSurroundingColor(Color color)
  {
    if ((color == null) || (color.getAlpha() == 0))
      return CacheUtils.TRANSPARENT_COLOR;

    return color;
  }

  // Returns the default Font
  private FontProxy _getFont(FontProxy font)
  {
    if (font != null)
      return font;

    return CompositeButtonPainter.getDefaultFont();
  }

  private void _setAccessKey(char accessKey)
  {
    _accessKey = Character.toLowerCase(accessKey);
  }

  private boolean _fontEquals(FontProxy font)
  {
    if ((_font.getName() == null) || (font.getName() == null))
    {
       return ((_font.getStyle() == font.getStyle())         &&
               (_font.getSize() == font.getSize()));
    }

    return _font.equals(font);
  }

  // Returns the InputStreamProvider for the icon with
  // the specific key.
  private InputStreamProvider _getIcon(
    Map<Object, Object> properties,
    Object     iconKey
    )
  {
    return (InputStreamProvider)properties.get(iconKey);
  }

  // Put the start icon in the specified properties dictionary
  private void _putStartIcon(Map<Object, Object> properties)
  {
    if (_startIcon == null)
      _startIcon = getStartIcon();

    if (_startIcon != null)
      properties.put(BUTTON_START_ICON_KEY, _startIcon);
  }

  // Put the end icon in the specified properties dictionary
  private void _putEndIcon(Map<Object, Object> properties)
  {
    if (_endIcon == null)
      _endIcon = getEndIcon();

    if (_endIcon != null)
      properties.put(BUTTON_END_ICON_KEY, _endIcon);
  }

  // Put the top background icon in the specified properties dictionary
  private void _putTopBackgroundIcon(Map<Object, Object> properties)
  {
    if (_topBackgroundIcon == null)
      _topBackgroundIcon = getTopBackgroundIcon();

    if (_topBackgroundIcon != null)
      properties.put(BUTTON_TOP_BACKGROUND_ICON_KEY, _topBackgroundIcon);
  }

  // Put the bottom background icon in the specified properties dictionary
  private void _putBottomBackgroundIcon(Map<Object, Object> properties)
  {
    if (_bottomBackgroundIcon == null)
      _bottomBackgroundIcon = getBottomBackgroundIcon();

    if (_bottomBackgroundIcon != null)
      properties.put(BUTTON_BOTTOM_BACKGROUND_ICON_KEY, _bottomBackgroundIcon);
  }

  private String    _lookAndFeelId;
  private String    _text;
  private Color     _foreground;
  private Color     _background;
  private Color     _surroundingColor;
  private FontProxy _font;
  private boolean   _disabled;
  private boolean   _textAntialias;
  private char      _accessKey;
  private int       _direction;

  // Name is sort of special - it isn't taken into account when
  // comparing for equality - it is only used as a hint to indicate
  // the desired image file name.
  private String    _name;

  private InputStreamProvider _startIcon;
  private InputStreamProvider _endIcon;
  private InputStreamProvider _topBackgroundIcon;
  private InputStreamProvider _bottomBackgroundIcon;

  private static final int _MAP_SIZE = 14;
}
