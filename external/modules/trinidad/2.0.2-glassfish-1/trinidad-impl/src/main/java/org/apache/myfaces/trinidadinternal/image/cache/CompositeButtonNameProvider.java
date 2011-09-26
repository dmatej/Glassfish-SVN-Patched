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

import java.util.Map;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * NameProvider implementation for composite buttons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/CompositeButtonNameProvider.java#0 $) $Date: 10-nov-2005.19:06:04 $
 */
public class CompositeButtonNameProvider
  implements NameProvider, ImageConstants
{
  public String getName(
      ImageContext context, 
      Map<Object, Object> requestedProperties)
  {
    String name = (String)requestedProperties.get(ImageConstants.NAME_KEY);

    if (name == null)
    {
      String text = (String)requestedProperties.get(ImageConstants.TEXT_KEY);

      if ((text == null) || (text.length() == 0))
      {
        text = "empty";
      }
      else
      {
        // Let's just use the first few characters to keep the name small
        if (text.length() > _MAX_LENGTH)
          text = text.substring(0, _MAX_LENGTH);
      }

      name = text;
    }

    // Produce a semi-unique identifier based on a hash of
    // the image's properties
    int hashCode = _hashCode(context, requestedProperties);

    // Covert the hashCode to a base 64-encoded string
    String hashString = CacheUtils.base64Encode(hashCode);

    return _PREFIX + name + hashString;
  }

  // Produces a hash code based on the requested properties
  private int _hashCode(
    ImageContext context,
    Map<Object, Object> properties
    )
  {
    Object text = properties.get(TEXT_KEY);
    Object foreground = properties.get(FOREGROUND_KEY);
    Object background = properties.get(BACKGROUND_KEY);
    Object surroundingColor = properties.get(SURROUNDING_COLOR_KEY);
    Object font = properties.get(FONT_KEY);
    Object disabled = properties.get(DISABLED_KEY);
    Object textAntialias = properties.get(TEXT_ANTIALIAS_KEY);
    Object accessKey = properties.get(ACCESS_KEY_KEY);
    int direction = CacheUtils.getReadingDirection(context, properties);

    int hashCode = 0;

    if (text != null)             hashCode ^= text.hashCode();
    if (foreground != null)       hashCode ^= foreground.hashCode();
    if (background != null)       hashCode ^= background.hashCode();
    if (surroundingColor != null) hashCode ^= surroundingColor.hashCode();
    if (font != null)             hashCode ^= font.hashCode();
    if (disabled != null)         hashCode ^= disabled.hashCode();
    if (textAntialias != null)    hashCode ^= textAntialias.hashCode();
    if (accessKey != null)        hashCode ^= accessKey.hashCode();

    hashCode ^= direction;

    return hashCode;
  }

  static private final String _PREFIX = "c";

  // Number of chars from button label to use in name
  static private final int _MAX_LENGTH = 4;

}
