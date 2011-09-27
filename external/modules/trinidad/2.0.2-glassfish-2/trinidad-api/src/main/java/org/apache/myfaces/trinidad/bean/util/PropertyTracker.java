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
package org.apache.myfaces.trinidad.bean.util;

import java.util.Iterator;

import java.util.NoSuchElementException;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;

/**
 * Utility class for tracking indexed PropertyKeys in a bitmask
 */
public class PropertyTracker implements Iterable<PropertyKey>
{
  /**
   * Constructor
   * @param type Facesbean Type for the bean that owns the tracker
   */
  public PropertyTracker(FacesBean.Type type)
  {
    _type = type;
  }
  
  /**
   * Adds a property to the bitmask
   * @param key property key to be tracked in a bitmask
   */
  public void addProperty(PropertyKey key)
  {
    int index = key.getIndex();
    _checkIndex(index);
    
    _propertiesMask |= (1L << index);
  }
  
  /**
   * Stops tracking property in a bitmask
   * @param key property key that should be removed from the bitmask
   */
  public void removeProperty(PropertyKey key)
  {
    int index = key.getIndex();
    _checkIndex(index);
    
    long mask  = ~(1L << index);
    _propertiesMask &= mask;
  }
  
  /**
   * Provides Iterator of the propoerty being tracked in a bitmask
   * @return iterator of property keys
   */
  public Iterator<PropertyKey> iterator()
  {
    return new PropertyBitIterator();
  }
  
  private void _checkIndex(int index)
  {
    if (index < 0 || index >= 64)
    {
      throw new IllegalArgumentException("Only indexed properties may be tracked");
    }
  }
  
  private class PropertyBitIterator implements Iterator<PropertyKey>
  {
    @Override
    public void remove()
    {
      throw new UnsupportedOperationException();
    }
    
    @Override
    public boolean hasNext()
    {
      if (_pos >= 64)
        return false;
      
      return _propertiesMask >= (1L << _pos); 
    }
    
    @Override
    public PropertyKey next()
    {
      while (_pos < 64)
      {
        int current = _pos++;
        
        if ((_propertiesMask &  (1L << current)) != 0)
        {
          return _type.findKey(current);
        }
      }
      throw new NoSuchElementException();
    }
    
    private int _pos = 0;
  }
  
  private long _propertiesMask = 0;
  private FacesBean.Type _type;
}
