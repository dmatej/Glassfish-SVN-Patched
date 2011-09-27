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

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.laf.browser.ButtonPainter;

/**
 * PropertiesFilter implementation for composite buttons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/CompositeButtonPropertiesFilter.java#0 $) $Date: 10-nov-2005.19:06:04 $
 */
public class CompositeButtonPropertiesFilter implements PropertiesFilter
{
  /**
   * Implementation of PropertiesFilter.filterProperties()
   */
  public Map<Object, Object> filterProperties(
    ImageContext context,
    Map<Object, Object> properties
    )
  {
    return new Defaults(context, properties);
  }

  /**
   * This class wraps up the properties dictionary in a
   * WrappingMap which provides default values for
   * missing properties.
   */
  static private class Defaults extends WrappingMap<Object, Object>
    implements ImageConstants
  {
    public Defaults(
      ImageContext context,
      Map<Object, Object> properties
      )
    {
      super(properties);

      _context = context;
    }

    @Override
    public Object get(Object key)
    {
      Object value = super.get(key);

      if (value != null)
      {
        // We need to convert null font names to the default font name
        if (key.equals(ImageConstants.FONT_KEY) &&
             (value instanceof FontProxy))
        {
          FontProxy font = (FontProxy)value;
          if (font.getName() == null)
          {
            if (_font == null)
            {
              _font = new FontProxy(_DEFAULT_FONT_NAME,
                                    font.getStyle(),
                                    font.getSize());
            }

            value = _font;
          }
        }

        return value;
      }

      Map<Object, Object> properties = getWrappedMap();

      if (key.equals(ImageConstants.FOREGROUND_KEY))
        return _getDefaultForeground(properties);
      if (key.equals(ImageConstants.BACKGROUND_KEY))
        return _getDefaultBackground(properties);
      if (key.equals(ImageConstants.FONT_KEY))
        return ButtonPainter.getDefaultFont();
      if (key.equals(ImageConstants.BORDER_COLOR_KEY))
        return _getDefaultBorderColor(properties);

      return null;
    }

    private Color _getDefaultForeground(Map<Object, Object> properties)
    {
      return ButtonPainter.getDefaultForeground(_context,
                                                _isDisabled(properties));
    }

    private Color _getDefaultBackground(Map<Object, Object> properties)
    {
      return ButtonPainter.getDefaultBackground(_context,
                                                _isDisabled(properties));
    }


    private boolean _isDisabled(Map<Object, Object> properties)
    {
      Boolean disabled = (Boolean)properties.get(DISABLED_KEY);

      if (disabled == null)
        return false;

      return disabled.booleanValue();
    }

    private Color _getDefaultBorderColor(Map<Object, Object> properties)
    {
      return ButtonPainter.getDefaultBorderColor(_context,
                                                 _isDisabled(properties));
    }

    private ImageContext _context;
    private FontProxy    _font;

    private static final String _DEFAULT_FONT_NAME = "dialog";
  }
}
