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
package org.apache.myfaces.trinidad.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 */
class PathHelper 
{
  protected PathHelper()
  {
    _path = Collections.emptyList();
    _rowKey = null;
  }
  
  /**
   * Sets the current rowKey.
   * @param rowKey
   */
  public final void setRowKey(String rowKey)
  {
    _rowKey = rowKey;
  }
  
  /**
   * @return the current row key
   */
  public final String getRowKey()
  {
    return _rowKey;
  }
  
  /**
   * @return the current size of the path
   */
  public final int getPathSize()
  {
    return _path.size();
  }
  
  /**
   * Gets the current path.
   * @return a List, with each element a rowKey String
   */
  public final List<String> getPath()
  {
    int sz = _path.size();
    List<String> path = new ArrayList<String>(sz+1);
    if (sz > 0)
    {
      for(int i=0; i<sz; i++)
      {
        path.add(getPath(i));
      }
    }
    path.add(getRowKey());
    return path;
  }

  /**
   * Gets a rowKey at the specified location in the path.
   * @param index
   * @return the rowKey String
   */
  public final String getPath(int index)
  {
    return _getPathElement(index).rowKey;
  }
  
  /**
   * Sets the path.
   * @param path a List with each element a rowKey String
   */
  public final void setPath(List<String> path)
  {
    int sz = (path==null) ? 0 : path.size();

    if (sz > 0)
    {
      _path = new ArrayList<PathElement>(sz);
      int lastIndex = sz - 1;
  
      for(int i=0; i<lastIndex; i++)
      {
        setRowKey(path.get(i));
        pushPath();
      }
      setRowKey(path.get(lastIndex));
    }
    else
    {
      _path = Collections.emptyList();
      setRowKey(null);
    }
  }

  /**
   * Gets the path data at the specified index.
   * @param index
   * @return this is the path data that was set using 
   * {@link #pushPath(Object,String)}
   */
  protected final Object getPathData(int index)
  {
    return _getPathElement(index).node;
  }

  /**
   * @return the path data for the last element in the path.
   */
  protected Object getLastPathData()
  {
    int sz = getPathSize();
    return (sz > 0) ? getPathData(sz - 1) : null;
  }
  
  /**
   * Pushes the current rowKey onto the path.
   */
  public final void pushPath()
  {
    if (_rowKey == null)
      throw new IllegalStateException(_LOG.getMessage(
        "NULL_ROWKEY"));
  
    Object parentData = getLastPathData();
    Object data = pushPath(parentData, _rowKey);

    List<PathElement> comparant = Collections.emptyList();
    if (_path == comparant)
      _path = new ArrayList<PathElement>(5);

    _path.add(new PathHelper.PathElement(_rowKey, data));
    
    _rowKey = null;
  }
  
  /**
   * Gets the path data to use for a new path segment.
   * @param parentData the path data of the previous path segment
   * @param rowKey the path segment that is just about to be added.
   * @return new path data, which can be retrieved using
   * {@link #getPathData(int)}
   */
  protected Object pushPath(Object parentData, String rowKey)
  {
    return null;
  }
  
  /**
   * Removes the last rowKey from the end of this path.
   * The current rowKey is set to be this last rowKey.
   */
  public final void popPath()
  {
    int sz = _path.size();
    if (sz > 0)
    {
      PathHelper.PathElement lastPath = _path.remove(sz-1);
      _rowKey = lastPath.rowKey;
    }
    else
      throw new IllegalStateException(_LOG.getMessage(
        "NO_PATH_ELEMENT_TO_POP"));
  }

  private PathHelper.PathElement _getPathElement(int index)
  {
    return _path.get(index);
  }
  
  private String _rowKey;
  private List<PathElement> _path;
  
  private static final class PathElement
  {
    public final Object node;
    public final String rowKey;
    
    public PathElement(String rowKey, Object data)
    {
      this.rowKey = rowKey;
      this.node = data;
    }
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    PathHelper.class);
}
