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
package org.apache.myfaces.trinidad.bean;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;

import java.util.Collection;
import java.util.Collections;

import javax.faces.component.PartialStateHolder;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.util.StateUtils;

/**
 * Class for managing attached object on the component that are maintained as a Map of Lists
 *
 */
public class AttachedObjects<K, T> implements PartialStateHolder
{
  /**
   * Adds attached object to this collection. The key for the object does not have to be unique.
   * @param key Object key
   * @param obj Object value
   */
  final public void addAttachedObject(K key, T obj)
  {    
    List<T> objects = _objectMap.get(key);
    if (objects == null)
    {
      objects = new ArrayList<T>(5);
      _objectMap.put(key, objects);
    }
    
    objects.add(obj);
    
    if (initialStateMarked())
    {
      // Clear initial state marker for the attached objects so that their full state is saved
      clearInitialState();
    }
  }
  /**
   * Removes an object from this collection
   * @param key Object key
   * @param obj Object value
   * @return true if the object fas found and removed, false otherwise
   */
  final public boolean removeAttachedObject(K key, T obj)
  {
    List<T> objects = _objectMap.get(key);
    if (objects == null)
    {
      return false;
    }
    
    boolean removed = objects.remove(obj);
    if (removed)
    {
      if (initialStateMarked())
      {
        // Clear initial state marker for the attached objects so that their full state is saved
        clearInitialState();
      }
    }
    return removed;
  }
  
  /**
   * Retrieves a non-null immutable list of objects for this key from the collection.
   * @param key Key value shared by all the objects in the List
   * @return a list of objects for the given key
   */
  final public List<T> getAttachedObjectList(K key)
  {
    List<T> objects = _objectMap.get(key);
    if (objects == null)
    {
      return Collections.emptyList();
    }
    return Collections.unmodifiableList(objects);
  }
  
  /**
   * Retreives a map of objects contained in this collection.
   * @return a non-null immutable map of objects
   */
  final public Map<K, List<T>> getAttachedObjectMap()
  {
    if (_readOnlyObjectMap == null)
    {
      _readOnlyObjectMap = Collections.unmodifiableMap(_objectMap);
    }
    return _readOnlyObjectMap;
  }
        
  @Override
  public void markInitialState()
  {
    for (Map.Entry<K, List<T>> e : _objectMap.entrySet())
    {
      for (T obj : e.getValue())
      {
        if (obj instanceof PartialStateHolder)
        {
          ((PartialStateHolder)obj).markInitialState();
        }
      }
    }
    _initialStateMarked = true;
  }
  
  @Override
  public void clearInitialState()
  {
    _initialStateMarked = false;
    for (Map.Entry<K, List<T>> e : _objectMap.entrySet())
    {
      for (T obj : e.getValue())
      {
        if (obj instanceof PartialStateHolder)
        {
          ((PartialStateHolder)obj).clearInitialState();
        }
      }
    }
  }
  
  @Override
  public boolean initialStateMarked()
  {
    return _initialStateMarked;
  }

  @Override
  public Object saveState(
    FacesContext facesContext)
  {
    Map<K, Object[]> state = new HashMap<K, Object[]>(_objectMap.size());
    for (Map.Entry<K, List<T>> e : _objectMap.entrySet())
    {
      List<T> l = e.getValue();
      Object[] entryState = new Object[l.size()];
      boolean stateWasSaved = false;
      for (int i = 0, size = entryState.length; i < size; ++i)
      {
        T obj = l.get(i);
        if (_initialStateMarked)
        {
          // JSF 2 state saving, only save the attched object's state if it is a state holder,
          // otherwise the re-application of the template will handle the re-creation of the
          // object in the correct state
          if (obj instanceof StateHolder)
          {
            entryState[i] = ((StateHolder)obj).saveState(facesContext);
          }
        }
        else
        {
          // Use JSF <= 1.2 state saving method as the initial state was not marked
          entryState[i] = StateUtils.saveStateHolder(facesContext, obj);
        }

        stateWasSaved = (entryState[i] != null) ? true : stateWasSaved;
      }

      if (stateWasSaved)
      {
        state.put(e.getKey(), entryState);
      }
    }
    
    Object [] savedState = null;
    if (!state.isEmpty())
    {
    
      // Record whether we used full state saving. This is necessary because methods like addClientBehvior() force
      // full state saving under certain circumstances.
      // To avoid using partial state restoration with the fully-saved state during restoreView(),  we will be using
      // the saved flag instead of _initialStateMarked 
      savedState = new Object[2];
      savedState[0] = _initialStateMarked; 
      savedState[1] = state;
    }
    return savedState;
  }

  @Override
  public void restoreState(
    FacesContext facesContext,
    Object       state)
  {
    if (state == null)
      return;
    
    Object stateArray[] = (Object [])state;
    boolean usePartialStateSaving = (Boolean)stateArray[0];
    
    @SuppressWarnings("unchecked")
    Map<K, Object[]> savedState = (Map<K, Object[]>) stateArray[1];

    if (usePartialStateSaving)
    {
      // In JSF 2 state saving, we only need to super impose the state onto the existing
      // attached object list of the current map as the attached objects will already be restored in
      // the same order that they were in the previous request (if not there is an application
      // bug).
      for (Map.Entry<K, Object[]> e : savedState.entrySet())
      {
        // Assume that the objects were correctly re-attached to the component and we only
        // need to restore the state onto the objects. The order must be maintained.
        List<T> l = _objectMap.get(e.getKey());
        Object[] entryState = e.getValue();
        for (int i = 0, size = entryState.length; i < size; ++i)
        {
          if (entryState[i] != null)
          {
            T obj = l.get(i);
            if (obj instanceof StateHolder)
            {
              ((StateHolder)obj).restoreState(facesContext, entryState[i]);
            }
          }
        }
      }
    }
    else
    {
      // For JSF <= 1.2 style state saving, we should ensure that we are empty and then
      // re-hydrate the attched objects directly from the state
      _objectMap.clear();

      for (Map.Entry<K, Object[]> e : savedState.entrySet())
      {
        Object[] entryState = e.getValue();
        // Assume the list is not going to grow in this request, so only allocate the size
        // of the list from the previous request
        List<T> list = new ArrayList<T>(entryState.length);
        for (int i = 0, size = entryState.length; i < size; ++i)
        {
          list.add((T)StateUtils.restoreStateHolder(facesContext, entryState[i]));
        }

        _objectMap.put(e.getKey(), list);
      }
    }
  }

  public boolean isTransient()
  {
    return _transient;
  }

  public void setTransient(
    boolean newTransientValue)
  {
    _transient = newTransientValue;
  }

     
  
  private Map<K, List<T>> _objectMap = new HashMap<K, List<T>>(5, 1.0f);
  
  private Map<K, List<T>> _readOnlyObjectMap = null;
  
  private boolean _initialStateMarked = false;
  private boolean _transient = false;
}
