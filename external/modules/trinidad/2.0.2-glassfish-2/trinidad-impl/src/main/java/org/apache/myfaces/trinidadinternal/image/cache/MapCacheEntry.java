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


import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import org.apache.myfaces.trinidadinternal.image.util.MapArea;


/**
 * Cache entry data structure which contains an image map
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/MapCacheEntry.java#0 $) $Date: 10-nov-2005.19:06:10 $
 */
class MapCacheEntry extends CacheEntry
{
  public MapCacheEntry(
    String uri,
    int width,
    int height,
    MapArea[] areas,
    String    encoding
    )
  {
    super(uri, width, height, encoding);
    if (areas==null) 
    {
      _areas = Collections.emptyList();
    }
    else
      _areas = Collections.unmodifiableList(Arrays.asList(areas));
  }

  @Override
  public Collection<MapArea> getMapAreas()
  {
    return _areas;
  }

  private Collection<MapArea> _areas;
}
