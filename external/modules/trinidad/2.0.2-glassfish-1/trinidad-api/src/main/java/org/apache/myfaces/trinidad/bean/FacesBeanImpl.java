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

import java.lang.reflect.Array;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.el.ValueExpression;

import javax.faces.component.PartialStateHolder;
import javax.faces.component.StateHolder;
import javax.faces.component.behavior.ClientBehavior;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.bean.util.FlaggedPropertyMap;
import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Base implementation of FacesBean.
 *
 */
abstract public class FacesBeanImpl implements FacesBean
{
  public FacesBeanImpl()
  {
  }

  /**
   * Get the type of a FacesBean
   */
  // TODO Use auto "TYPE" detection?
  abstract public Type getType();

  final public Object getProperty(PropertyKey key)
  {
    Object o = getLocalProperty(key);
    if (o != null)
      return o;

    // Look for a binding if and only if the key supports bindings
    if (key.getSupportsBinding())
    {
      ValueExpression expression = getValueExpression(key);
      if (expression != null)
      {
        FacesContext context = FacesContext.getCurrentInstance();
        return expression.getValue(context.getELContext());
      }
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  final public Object getRawProperty(PropertyKey key)
  {
    Object local = getLocalProperty(key);
    if (local != null)
      return local;

    // Look for a binding if and only if the key supports bindings
    return key.getSupportsBinding() ? getValueExpression(key) : null;
  }

  // TODO Need *good* way of hooking property-sets;  it's
  // currently not called from state restoring, so really, it shouldn't
  // be used as a hook, but EditableValueBase currently
  // requires hooking this method.
  public void setProperty(PropertyKey key, Object value)
  {
    _checkNotListKey(key);
    setPropertyImpl(key, value);
  }

  final public Object getLocalProperty(PropertyKey key)
  {
    _checkNotListKey(key);

    return getLocalPropertyImpl(key);
  }

  final public ValueExpression getValueExpression(PropertyKey key)
  {
    _checkNotListKey(key);

    PropertyMap map = _getExpressionsMap(false);
    if (map == null)
      return null;

    return (ValueExpression) map.get(key);
  }

  final public void setValueExpression(PropertyKey key,
                                       ValueExpression expression)
  {
    _checkNotListKey(key);

    if (!key.getSupportsBinding())
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_FIND_PROPERTY", key.getName()));
    }

    if (expression == null)
    {
      PropertyMap map = _getExpressionsMap(false);
      if (map != null)
        map.remove(key);
    }
    else
    {
      _getExpressionsMap(true).put(key, expression);
    }

  }

  @SuppressWarnings("deprecation")
  final public ValueBinding getValueBinding(PropertyKey key)
  {
    ValueExpression ve = getValueExpression(key);

    if (ve == null)
    {
      return null;
    }
    else
    {
      // wrap the ValueExpression if necessary
      return ValueExpressionValueBinding.getValueBinding(ve);
    }
  }

  @SuppressWarnings("deprecation")
  final public void setValueBinding(PropertyKey key, ValueBinding binding)
  {
    ValueExpression ve;

    if (binding == null)
    {
      ve = null;
    }
    else
    {
      ve = ValueBindingValueExpression.getValueExpression(binding);
    }

    setValueExpression(key, ve);
  }

  @SuppressWarnings("unchecked")
  final public void addEntry(PropertyKey listKey, Object value)
  {
    _checkListKey(listKey);

    List<Object> l = (List<Object>) getLocalPropertyImpl(listKey);
    if (l == null)
    {
      l = _createList();
      setPropertyImpl(listKey, l);
    }

    l.add(value);

    // If we've marked our initial state, forcibly update the property
    // so we know to write out the change
    if (_initialStateMarked)
      setPropertyImpl(listKey, l);
  }

  @SuppressWarnings("unchecked")
  final public void removeEntry(PropertyKey listKey, Object value)
  {
    _checkListKey(listKey);

    List<Object> l = (List<Object>) getLocalPropertyImpl(listKey);
    if (l != null)
    {
      l.remove(value);
    }

    // If we've marked our initial state, forcibly update the property
    // so we know to write out the change
    if (_initialStateMarked && (l != null))
      setPropertyImpl(listKey, l);
  }

  @SuppressWarnings("unchecked")
  final public Object[] getEntries(PropertyKey listKey, Class clazz)
  {
    _checkListKey(listKey);

    List<Object> l = (List<Object>) getLocalPropertyImpl(listKey);
    if (l == null)
      return (Object[]) Array.newInstance(clazz, 0);

    int size = l.size();
    ArrayList<Object> tempList = new ArrayList<Object>(size);
    for (int i = 0; i < size; i++)
    {
      Object o = l.get(i);
      if (clazz.isInstance(o))
        tempList.add(o);
    }

    return tempList.toArray((Object[]) Array.newInstance(clazz,
                                                         tempList.size()));
  }

  @SuppressWarnings("unchecked")
  final public boolean containsEntry(PropertyKey listKey, Class<?> clazz)
  {
    _checkListKey(listKey);

    List<Object> l = (List<Object>) getLocalPropertyImpl(listKey);
    if (l == null)
      return false;

    int size = l.size();
    for (int i = 0; i < size; i++)
    {
      Object o = l.get(i);
      if (clazz.isInstance(o))
        return true;
    }

    return false;
  }

  @SuppressWarnings("unchecked")
  final public Iterator<Object> entries(PropertyKey listKey)
  {
    _checkListKey(listKey);

    List<Object> l = (List<Object>) getLocalPropertyImpl(listKey);
    if (l == null)
      return Collections.emptyList().iterator();

    return l.iterator();
  }

  // TODO provide more efficient implementation for copying
  // from other FacesBeanImpl instances
  public void addAll(FacesBean from)
  {
    if (from == this)
      return;

    for(PropertyKey fromKey : from.keySet())
    {
      PropertyKey toKey = _convertKey(fromKey);
      if ((toKey != null) &&
          _isCompatible(fromKey, toKey))
      {
        if (!fromKey.isList())
        {
          setProperty(toKey, from.getLocalProperty(fromKey));
        }
        else
        {
          Iterator<? extends Object> entries = from.entries(fromKey);
          while (entries.hasNext())
            addEntry(toKey, entries.next());
        }
      }
    }

    for(PropertyKey fromKey : from.bindingKeySet())
    {
      PropertyKey toKey = _convertKey(fromKey);
      if (toKey.getSupportsBinding())
      {
        setValueExpression(toKey, from.getValueExpression(fromKey));
      }
    }
  }

  public Set<PropertyKey> keySet()
  {
    if (_properties == null)
      return Collections.emptySet();
    else
      return _properties.keySet();
  }

  @SuppressWarnings("unchecked")
  final public Set<PropertyKey> bindingKeySet()
  {
    if (_expressions == null)
      return Collections.emptySet();

    return _expressions.keySet();
  }

  public void markInitialState()
  {
    _initialStateMarked = true;

    if (_properties != null)
      _properties.markInitialState();

    if (_expressions != null)
      _expressions.markInitialState();
  }

  public void clearInitialState()
  {
    _initialStateMarked = false;

    if (_properties != null)
      _properties.clearInitialState();

    if (_expressions != null)
      _expressions.clearInitialState();
  }

  public boolean initialStateMarked()
  {
    return _initialStateMarked;
  }

  public void restoreState(FacesContext context, Object state)
  {
    if (_LOG.isFiner())
    {
      _LOG.finer("Restoring state into " + this);
    }

    if (state instanceof Object[])
    {
      Object[] asArray = (Object[]) state;
      if (asArray.length == 2)
      {
        Object propertyState = asArray[0];
        Object bindingsState = asArray[1];
        _getPropertyMap().restoreState(context, getType(), propertyState);
        _getExpressionsMap(true).restoreState(context, getType(), bindingsState);
        return;
      }
      else if (asArray.length == 1)
      {
        Object propertyState = asArray[0];
        _getPropertyMap().restoreState(context, getType(), propertyState);
        return;
      }
    }

    _getPropertyMap().restoreState(context, getType(), state);
  }

  public Object saveState(FacesContext context)
  {
    if (_LOG.isFiner())
    {
      _LOG.finer("Saving state of " + this);
    }

    Object propertyState = (_properties == null)
                            ? null
                            : _properties.saveState(context);
    Object bindingsState = (_expressions == null)
                            ? null
                            : _expressions.saveState(context);

    if (bindingsState != null)
    {
      return new Object[] { propertyState, bindingsState };
    }

    if (propertyState == null)
      return null;

    if (propertyState instanceof Object[])
    {
      Object[] asArray = (Object[]) propertyState;
      if (asArray.length <= 2)
        return new Object[]{propertyState};
    }

    return propertyState;
  }

  @Override
  public String toString()
  {
    String className = getClass().getName();
    int lastPeriod = className.lastIndexOf('.');
    if (lastPeriod < 0)
      return className;

    return className.substring(lastPeriod + 1);
  }

  protected void setPropertyImpl(PropertyKey key, Object value)
  {
    if (value == null)
      _getPropertyMap().remove(key);
    else
      _getPropertyMap().put(key, value);
  }

  protected Object getLocalPropertyImpl(PropertyKey key)
  {
    return _getPropertyMap().get(key);
  }

  protected PropertyMap createPropertyMap()
  {
    FlaggedPropertyMap map =  new FlaggedPropertyMap();
    // We are providing Type information to the Map implementation
    // to enable tracking certain properties in a bitmask.
    // For now, the properties we are tracking will never be specified
    // as expressions
    map.setType(getType());
    return map;
  }

  protected PropertyMap createExpressionsMap()
  {
    return new FlaggedPropertyMap();
  }

  // "listKey" is unused, but it seems plausible that
  // if this ever gets converted to a protected hook that
  // "listKey" may be useful in that context.
  private List<Object> _createList(/*PropertyKey listKey*/)
  {
    return new ArrayList<Object>();
  }

  /**
   * Converts a key from one type to another.
   */
  private PropertyKey _convertKey(PropertyKey fromKey)
  {
    Type type = getType();
    // If the "fromKey" has an index, then see if it's exactly
    // the same key as one of ours.
    PropertyKey toKey = type.findKey(fromKey.getIndex());
    if (toKey == fromKey)
      return toKey;

    // Otherwise, just look it up by name.
    String name = fromKey.getName();
    toKey = type.findKey(name);
    if (toKey != null)
      return toKey;

    // Finally, give up and create a transient key
    return new PropertyKey(name);
  }

  /**
   * Returns true if two keys are of compatible types.
   */
  static private boolean _isCompatible(
    PropertyKey fromKey, PropertyKey toKey)
  {
    return (fromKey.isList() == toKey.isList());
  }

  private PropertyMap _getPropertyMap()
  {
    if (_properties == null)
      _properties = createPropertyMap();

    return _properties;
  }

 
  private PropertyMap _getExpressionsMap(boolean createIfNew)
  {
    if (_expressions == null)
    {
      if (createIfNew)
      {
        _expressions = createExpressionsMap();
      }
    }

    return _expressions;
  }

  static private void _checkListKey(PropertyKey listKey)
    throws IllegalArgumentException
  {
    if (!listKey.isList())
      throw new IllegalArgumentException(_LOG.getMessage(
        ">KEY_CANNOT_BE_USED_FOR_LISTS", listKey));
  }

  static private void _checkNotListKey(PropertyKey key)
    throws IllegalArgumentException
  {
    if (key.isList())
      throw new IllegalArgumentException(_LOG.getMessage(
        "KEY_IS_LIST_KEY", key));
  }

  private PropertyMap _properties;
  private PropertyMap _expressions;
 
  private transient boolean                           _initialStateMarked;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
                                               FacesBeanImpl.class);
}
