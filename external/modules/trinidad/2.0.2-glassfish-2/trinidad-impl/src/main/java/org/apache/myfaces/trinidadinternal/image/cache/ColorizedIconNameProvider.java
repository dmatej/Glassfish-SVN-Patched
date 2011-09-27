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

import java.io.File;
import java.util.Map;

import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;

/**
 * NameProvider implementation for colorized icons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/ColorizedIconNameProvider.java#0 $) $Date: 10-nov-2005.19:06:02 $
 */
public class ColorizedIconNameProvider implements NameProvider
{
  public String getName(ImageContext context, Map<Object, Object> properties)
  {
    String name = (String)properties.get(ImageConstants.NAME_KEY);

    if (name == null)
    {
      String source = (String)properties.get(ImageConstants.SOURCE_KEY);

      // Get just the file name out of the imageName
      int lastSepIndex = source.lastIndexOf(File.separatorChar);
      if ((lastSepIndex == -1) && (File.separatorChar != '/'))
        lastSepIndex = source.lastIndexOf('/');

      int dotIndex = source.lastIndexOf('.');
      if ((dotIndex == -1) || (dotIndex <= lastSepIndex))
        dotIndex = source.length();

      name = source.substring(lastSepIndex+1, dotIndex);
    }

    // Tack on a "-r" suffix if this is a RTL image
    String directionName = "";
    if (CacheUtils.getReadingDirection(context, properties) ==
         LocaleUtils.DIRECTION_RIGHTTOLEFT)
    {
      directionName = "-r";
    }

    return _PREFIX + name + directionName;
  }

  static private final String _PREFIX = "c";
}
