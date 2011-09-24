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

import java.util.Map;

import java.util.Set;

import javax.el.ValueExpression;

import javax.faces.component.PartialStateHolder;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.PropertyMap;
import org.apache.myfaces.trinidad.util.ArrayMap;

import javax.faces.context.FacesContext;

public class PropertyArrayMap extends ArrayMap<PropertyKey,Object>
                             implements PropertyMap
{
  public PropertyArrayMap(
    int initialCapacity)
  {
    super(initialCapacity);
  }

  public PropertyArrayMap()
  {
    super();
  }

  public Object get(
    PropertyKey pKey)
  {
    if (pKey.getIndex() < 0)
      return get(pKey);
    return getByIdentity(pKey);
  }

  @Override
   public Object put(
     PropertyKey key,
     Object      value)
   {
     Object retValue = super.put(key, value);
     if (_createDeltas())
     {
       if (key.getMutable().isAtLeastSometimesMutable() || !_equals(value, retValue))
         _deltas.put(key, value);
     }
     else if (key.getMutable().isAtLeastSometimesMutable() && !(value instanceof ValueExpression))
     {
       _getMutableTracker(true).addProperty(key);
     }

     if (key.isPartialStateHolder())
     {
       _getPartialStateHolderTracker(true).addProperty(key);
     }

     return retValue;
   }

  @Override
   public Object remove(
     Object key)
   {
     boolean useDeltas = _createDeltas();

     if (useDeltas)
     {
       if (!super.containsKey(key))
         return null;

       // If this key is contained, it certainly must be a PropertyKey!
       assert(key instanceof PropertyKey);
       _deltas.put((PropertyKey) key, null);
     }

     if (key instanceof PropertyKey)
     {
       PropertyKey propKey  = (PropertyKey)key;
       if (propKey.isPartialStateHolder())
       {
         _getPartialStateHolderTracker(true).removeProperty(propKey);
       }

       if (!useDeltas &&  propKey.getMutable().isAtLeastSometimesMutable())
       {
         PropertyTracker mutableTracker = _getMutableTracker(false);

         if (mutableTracker != null)
           mutableTracker.removeProperty(propKey);
       }
     }

     return super.remove(key);
   }

  @Override
  public void putAll(Map<? extends PropertyKey, ? extends Object> t)
  {
    boolean useDeltas =_createDeltas();

    if (useDeltas)
      _deltas.putAll(t);

    Set<? extends PropertyKey> keys = t.keySet();
    for (PropertyKey key: keys)
    {
      if (key.isPartialStateHolder())
      {
        _getPartialStateHolderTracker(true).addProperty(key);
      }

      if (!useDeltas && key.getMutable().isAtLeastSometimesMutable())
      {
        Object value = t.get(key);

        if (!(value instanceof ValueExpression))
        {
          _getMutableTracker(true).addProperty(key);
        }
      }

    }

    super.putAll(t);
  }

  public Object saveState(FacesContext context)
  {
    if (_initialStateMarked)
    {
      if (_deltas == null)
        return null;

      return StateUtils.saveState(_deltas, context, getUseStateHolder());
    }
    else
    {
      return StateUtils.saveState(this, context, getUseStateHolder());
    }
  }

  public void restoreState(
    FacesContext context,
    FacesBean.Type type,
    Object state)
  {
    StateUtils.restoreState(this, context, type, state, getUseStateHolder());
  }

  protected PropertyMap createDeltaPropertyMap()
  {
    PropertyArrayMap map = new PropertyArrayMap(2);
    map.setUseStateHolder(getUseStateHolder());
    map.setType(_type);

    PropertyTracker tracker = _getMutableTracker(false);

    if (tracker != null)
    {
      for (PropertyKey key: tracker)
      {
        Object val = get(key);

        if (val != null)
        {
          map.put(key, val);
        }
      }

      _mutableTracker = null;
    }
    return map;
  }


  public boolean getUseStateHolder()
  {
    return _useStateHolder;
  }

  public void setUseStateHolder(boolean useStateHolder)
  {
    _useStateHolder = useStateHolder;
  }



  // =-=AEW CLEAR?

  public void markInitialState()
  {
    _initialStateMarked = true;

    // PropertyTracker uses a bitmask to track properties
    // We are tracking all properties that have CA_PARTIAL_STATE_HOLDER capability,
    // so that we do not have to check every property here
    PropertyTracker tracker = _getPartialStateHolderTracker(false);
    if (tracker != null)
    {
      for (PropertyKey key: tracker)
      {
        Object val = get(key);
        if (val != null)
        {
          ((PartialStateHolder)val).markInitialState();
        }
      }
    }
  }


  public void clearInitialState()
  {
    _initialStateMarked = false;
    _deltas = null;

    // PropertyTracker uses a bitmask to track properties
    // We are tracking all properties that have CA_PARTIAL_STATE_HOLDER capability,
    // so that we do not have to check every property here
    PropertyTracker tracker = _getPartialStateHolderTracker(false);
    if (tracker != null)
    {
      for (PropertyKey key: tracker)
      {
        Object val = get(key);
        if (val != null)
        {
          ((PartialStateHolder)val).clearInitialState();
        }
      }
    }
  }

  public boolean initialStateMarked()
  {
    return _initialStateMarked;
  }

  /**
   * Sets the the FacesBean type used by this map's owner bean
   * @param type FacesBean type
   */
  public void setType(FacesBean.Type type)
  {
    _type = type;
  }

  private boolean _createDeltas()
  {
    if (_initialStateMarked)
    {
      if (_deltas == null)
      {
        _deltas = createDeltaPropertyMap();
      }

      return true;
    }

    return false;
  }

  static private boolean _equals(Object a, Object b)
  {
    if (a == b)
      return true;

    if (a == null)
      return false;

    return a.equals(b);
  }
  
  private PropertyTracker _getPartialStateHolderTracker(boolean create)
  {
    if (_tracker == null && create)
    {
      if (_type == null)
      {
        throw new IllegalStateException("FacesBean.TYPE is required to track properties");
      }
      _tracker = new PropertyTracker(_type);
    }
    return _tracker;
  }

    private PropertyTracker _getMutableTracker(boolean create)
    {
      if (_mutableTracker == null && create)
      {
        if (_type == null)
        {
          throw new IllegalStateException("FacesBean.TYPE is required to track properties");
        }
        _mutableTracker = new PropertyTracker(_type);
      }
      return _mutableTracker;
    }


  private transient boolean _initialStateMarked;
  private transient PropertyMap _deltas;
  private boolean      _useStateHolder;
  private FacesBean.Type _type;
  private PropertyTracker _tracker;
  private transient PropertyTracker _mutableTracker;
}
