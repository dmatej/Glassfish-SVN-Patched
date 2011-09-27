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

import java.util.Collection;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;

import org.apache.myfaces.trinidadinternal.image.encode.ImageEncoderManager;
import org.apache.myfaces.trinidadinternal.image.util.MapArea;

/** 
 * Cache entry data structure
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/CacheEntry.java#0 $) $Date: 10-nov-2005.19:05:59 $
 */
class CacheEntry implements ImageProviderResponse
{
  public CacheEntry(
    String uri,
    int    width,
    int    height
    )
  {
    this(uri, width, height, null);
  }

  public CacheEntry(
    String uri,
    int    width,
    int    height,
    String encoding
    )
  {
    _uri = uri;
    _width = width;
    _height = height;

    if (encoding == null)
    {
      _encoding = ImageEncoderManager.GIF_TYPE;
    }
    else
    {
      // We explicitly intern the encoding so that we can 
      // use identity comparisons on the encoding.  This assumes
      // that only encodings defined as ImageEncoderManager
      // *_TYPE constants are used, which seems fair.
      _encoding = encoding.intern();
    }
  }

  public String getImageURI()
  {
    return _uri;
  }

  public int getWidth()
  {
    return _width;
  }

  public int getHeight()
  {
    return _height;
  }

  public String getEncoding()
  {
    return _encoding;
  }

  public Collection<MapArea> getMapAreas()
  {
    return null;
  }

  public synchronized long getLastChecked()
  {
    return _lastChecked;
  }

  public CacheEntry getNext()
  {
    return _next;
  }

  // Checks to see whether the any resources used to create
  // this image (like the source icon) have changed.
  public boolean isValid(
    ImageContext context,
    ImageProviderRequest request
    )
  {
    return true;
  }

  public synchronized void setLastChecked(long lastChecked)
  {
    _lastChecked = lastChecked;
  }

  public void setNext(CacheEntry next)
  {
    _next = next;
  }

  private String _uri;
  private int    _width;
  private int    _height;
  private String _encoding;

  // Last time we checked whether the corresponding image file exists
  private long   _lastChecked;

  // The next entry in the list.  We may have more than one version
  // of the same image - as many as one per encoding.
  private CacheEntry _next;
}
