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

import java.util.Map;

import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;

import org.apache.myfaces.trinidadinternal.image.cache.AccentColorizedIconKey;

import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeel;

/**
 * The ImageProviderRequest class that we use for requesting accent
 * colorized icons.  It extends the AccentColorizedIconKey
 * by adding support for obtaining an InputStreamProvider for the source
 * icon.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/AccentIconRequest.java#0 $) $Date: 10-nov-2005.18:53:11 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public final class AccentIconRequest extends AccentColorizedIconKey
{
  public AccentIconRequest(
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
                   CoreIconRequest.resolveSourceIcon(null,
                                                     getSource(),
                                                     _resolver));
                                                     
    return properties;
  }

  private NameResolver _resolver;
}
