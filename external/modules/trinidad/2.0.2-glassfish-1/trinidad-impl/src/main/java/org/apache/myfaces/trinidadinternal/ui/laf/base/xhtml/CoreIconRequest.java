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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.awt.Color;

import java.io.IOException;

import java.util.Map;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;

import org.apache.myfaces.trinidadinternal.image.cache.CoreColorizedIconKey;

import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;


/**
 * The ImageProviderRequest class that we use for requesting core
 * colorized icons.  It extends org.apache.myfaces.trinidadinternal.image.cache.CoreColorizedIconKey
 * by adding support for obtaining an InputStreamProvider for the source
 * icon.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/CoreIconRequest.java#0 $) $Date: 10-nov-2005.18:53:47 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class CoreIconRequest extends CoreColorizedIconKey
{
  /**
   * Static method used by CoreIconRequest and AccentIconRequest
   * to resolve source icon names
   * @deprecated Use resolveSourceIcon(context, name, sourceIconResolver)
   */
  @Deprecated
  public static InputStreamProvider resolveSourceIcon(
    String       name,
    NameResolver sourceIconResolver
    )
  {
    return resolveSourceIcon(null, name, sourceIconResolver);
  }

  // Static method used by CoreIconRequest and AccentIconRequest
  // to resolve source icon names
  public static InputStreamProvider resolveSourceIcon(
    ImageContext context,
    String       name,
    NameResolver sourceIconResolver
    )
  {
    try
    {
      return sourceIconResolver.getProvider(_ICON_PREFIX + name + _GIF);
    }
    catch (IOException e)
    {
      // If we've got an error log, log the IOException
      if (context != null)
        _LOG.warning(e);
    }

    return null;
  }

  public CoreIconRequest(
    ImageContext       context,
    String             source,
    Class<LookAndFeel> lookAndFeel,
    int                direction,
    Color              color,
    Color              surroundingColor,
    NameResolver       resolver
    )
  {
    super(context, source, lookAndFeel, direction, color, surroundingColor);

    _resolver = resolver;
  }

  // Override of getRenderProperties() which adds in the
  // InputStreamProvider for the source icon
  @Override
  public Map<Object, Object> getRenderProperties(ImageContext context)
  {
    Map<Object, Object> properties = super.getRenderProperties(context);

    properties.put(ImageConstants.SOURCE_INPUT_STREAM_PROVIDER_KEY,
                   resolveSourceIcon(context, getSource(), _resolver));

    return properties;
  }

  // Constants used by the CoreIconRequest and AccentIconRequest classes
  private static final String _ICON_PREFIX = "cImages/";
  private static final String _GIF = ".gif";

  private NameResolver _resolver;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CoreIconRequest.class);
}
