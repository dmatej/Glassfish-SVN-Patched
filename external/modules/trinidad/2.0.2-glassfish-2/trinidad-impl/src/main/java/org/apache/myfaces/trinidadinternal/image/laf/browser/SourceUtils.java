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
package org.apache.myfaces.trinidadinternal.image.laf.browser;

import java.awt.Image;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;



import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.painter.ImageUtils;

/**
 * Private utility methods for loading source icons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/SourceUtils.java#0 $) $Date: 10-nov-2005.19:05:11 $
 */
class SourceUtils
{
  public static Image getSourceIcon(
    ImageContext context,
    Map<Object, Object> properties
    )
  {
    return getSourceIcon(context,
                         properties,
                         ImageConstants.SOURCE_INPUT_STREAM_PROVIDER_KEY);
  }

  /**
   * Returns the source Image for the specified requested properties
   */
  public static Image getSourceIcon(
    ImageContext context,
    Map<Object, Object> properties,
    Object     key
    )
  {
    InputStreamProvider provider = (InputStreamProvider)
      properties.get(key);

    // If we can't get a provider, we can't get an Image
    if (provider == null)
    {
      _log(properties,
           _PROVIDER_ERROR + " (" + _getKeyName(key) + ")",
           null);

      return null;
    }

    InputStream in = null;

    try
    {
      in = provider.openInputStream();
    }
    catch (IOException e)
    {
      _log(properties, _INPUT_STREAM_ERROR, e);
      return null;
    }

    if (in == null)
    {
      _log(properties, _INPUT_STREAM_ERROR, null);
      return null;
    }

    // Note: getImageFromStream() closes up our InputStream,
    // so we do not explicitly call InputStream.close() here.
    Image source = ImageUtils.getImageFromStream(in);

    if (source == null)
    {
      _log(properties, _IMAGE_ERROR, null);
    }


    return source;
  }

  // Logs a warning
  private static void _log(
    Map<Object, Object> properties,
    String message,
    Throwable t
    )
  {
    if (_LOG.isWarning())
    {
      String source = (String) properties.get(ImageConstants.SOURCE_KEY);

      if (source != null)
        message += ("for source icon " + source);

      _LOG.warning(message, t);
    }
  }

  private static String _getKeyName(Object key)
  {
     return key.toString();
  }

  // Error messages
  private static final String _PROVIDER_ERROR =
    "Could not get InputStreamProvider";
  private static final String _INPUT_STREAM_ERROR =
    "Could not get InputStream";
  private static final String _IMAGE_ERROR =
    "Could not create image";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SourceUtils.class);
}
