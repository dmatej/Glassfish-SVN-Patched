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

import java.io.InputStream;
import java.io.IOException;
import java.util.Map;



import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;



/**
 * Cache entry data structure which checks to see if the source
 * icon has changed.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/SourceCheckingCacheEntry.java#0 $) $Date: 10-nov-2005.19:06:12 $
 */
class SourceCheckingCacheEntry extends CacheEntry
{
  public SourceCheckingCacheEntry(
    String uri,
    int width,
    int height,
    String    encoding
    )
  {
    super(uri, width, height, encoding);
  }

  @Override
  public boolean isValid(
    ImageContext context,
    ImageProviderRequest request
    )
  {
    // Make sure we have an InputStreamProvider for the source
    // icon.  We need this to check for modifications
    if (_provider == _NULL_PROVIDER)
      return true;

    InputStreamProvider provider = (InputStreamProvider)_provider;

    if (provider == null)
    {
      provider = _getInputStreamProvider(context, request);

      if (provider == null)
      {
        _provider = _NULL_PROVIDER;
        return true;
      }

      _provider = provider;
    }

    return !provider.hasSourceChanged();
  }

  // Gets the source icon InputStreamProvider for the current request
  private static InputStreamProvider _getInputStreamProvider(
    ImageContext context,
    ImageProviderRequest request
    )
  {
    Map<Object, Object> properties = request.getRenderProperties(context);
    assert (properties != null);

    InputStreamProvider provider = (InputStreamProvider)properties.get(
                          ImageConstants.SOURCE_INPUT_STREAM_PROVIDER_KEY);
    if (provider == null)
      return null;

    // We need to actually open the input stream in order
    // to establish the original last modified time
    try
    {
      InputStream in = provider.openInputStream();
      in.close();
    }
    catch (IOException e)
    {
      _LOG.severe(e);
      return null;
    }

    return provider;
  }

  private Object _provider;

  private static final Object _NULL_PROVIDER = new Object();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SourceCheckingCacheEntry.class);
}
