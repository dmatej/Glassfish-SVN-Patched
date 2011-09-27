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
package org.apache.myfaces.trinidad.component;

import javax.faces.component.UIComponent;

import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * List for storing children.
 *
 */
class ChildArrayList extends ArrayList<UIComponent>
{
  public ChildArrayList(UIComponent parent)
  {
    _parent = parent;
  }

  @Override
  public void add(int index, UIComponent element)
  {
    if (element == null)
      throw new NullPointerException();

    if ((index < 0) || (index > size()))
      throw new IndexOutOfBoundsException(_LOG.getMessage(
        "INDEX_SIZE", new Object[]{index, size()}));

    UIComponent oldParent = element.getParent();
    if (oldParent != null)
    {
      int adjustedIndex = __removeFromParent(element, index);
      // Only adjust the index when the child is re-added to the same parent
      if (oldParent == _parent)
      {
        index = adjustedIndex; 
      }
    }
    
    // do not change the order of these calls, see TRINIDAD-1674 for more info
    super.add(index, element);
    element.setParent(_parent);
  }

  
  @Override
  public boolean add(UIComponent element)
  {
    add(size(), element);
    return true;
  }
  
  @Override
  public boolean addAll(Collection<? extends UIComponent> collection)
  {
    return addAll(size(), collection);
  }

  @Override
  public boolean addAll(
      int index, 
      Collection<? extends UIComponent> collection)
  {
    boolean changed = false;
    for(UIComponent element : collection)
    {
      if (element == null)
        throw new NullPointerException();

      add(index++, element);
      changed = true;
    }
    
    return changed;
  }

  @Override
  public UIComponent remove(int index)
  {
    UIComponent child = super.remove(index);
    child.setParent(null);

    return child;
  }

  @Override
  public boolean remove(Object element)
  {
    if (element == null)
      throw new NullPointerException();
    
    if (!(element instanceof UIComponent))
      return false;
  
    if (super.remove(element))
    {
      UIComponent child = (UIComponent) element;
      child.setParent(null);
      return true;
    }

    return false;
  }
  
  @Override
  public boolean removeAll(Collection<?> collection)
  {
    boolean result = false;
    for(Object element : collection)
    {
      if (remove(element))
        result = true;
    }
    
    return result;
  }

  @Override
  public UIComponent set(int index, UIComponent element)
  {
    if (element == null)
      throw new NullPointerException();
    
    if ((index < 0) || (index >= size()))
      throw new IndexOutOfBoundsException();

    UIComponent child = element;
    UIComponent previous = get(index);

    previous.setParent(null);
    
    child.setParent(_parent);
    super.set(index, element);
    
    return previous;
  }

  @SuppressWarnings("unchecked")
  static int __removeFromParent(
    UIComponent component,
    int index)
  {
    UIComponent parent = component.getParent();
    assert(parent != null);

    if (parent.getChildCount() > 0)
    {
      List<UIComponent> children = parent.getChildren();
      int size = children.size();
      for  (int i = 0; i < size; i++)
      {
        if  (children.get(i) == component)
        {
          children.remove(i);
          if (index > i)
            index--;
          return index;
        }
      }
    }
    
    Collection<UIComponent> facets = parent.getFacets().values();
    if (facets.contains(component))
    {
      facets.remove(component);
      return index;
    }

    // Not good - the child thought it was in a parent,
    // but it wasn't.
    assert(false);
    return index;
  }


  private final UIComponent _parent;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ChildArrayList.class);
  private static final long serialVersionUID = 1L;
}
