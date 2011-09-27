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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.el.ValueExpression;

import javax.faces.component.behavior.ClientBehavior;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Base interface for FacesBean storage objects.
 *
 */
public interface FacesBean
{
  /**
   * Returns the Type of this bean.
   */
  public Type getType();

  /**
   * Returns a property.  If the property has not been explicitly
   * set, and the key supports bindings, and a ValueBinding has
   * been set for this key, that ValueBinding will be evaluated.
   *
   * @param key the property key
   * @exception IllegalArgumentException if key is a list key
   */
  // TODO Additional version that takes a FacesContext?
  public Object getProperty(PropertyKey key);

  /**
   *  Set a property.
   * @exception IllegalArgumentException if key is a list key
   */
  public void setProperty(PropertyKey key, Object value);

  /**
   * Return a property, ignoring any value bindings.
   *
   * @exception IllegalArgumentException if key is a list key
   */
  public Object getLocalProperty(PropertyKey key);

  /**
   * Return the value expression for a key.
   * @exception IllegalArgumentException if the property does
   *   not support value bindings.
   */
  public ValueExpression getValueExpression(PropertyKey key);

  /**
   * Return the value binding for a key.
   * @exception IllegalArgumentException if the property does
   *   not support value bindings.
   * @deprecated
   */
  public ValueBinding getValueBinding(PropertyKey key);

  /**
   * Gets the current unevaluated value for the specified property key.
   * <p>The method will first look for a local value. If it exists, it will
   * be returned. If it does not and the bean supports value expressions, the
   * method will look for an expression with the specified key and return it
   * directly if it exists without evaluatig its value.</p>
   * <p>This method is mainly used when:</p>
   * <ul>
   *   <li>The caller cannot ensure that FacesContext exists at the time
   *   of the call</li>
   *   <li>The FacesContext does not yet contains the managed bean
   *   referenced by the value binding</li>
   *   <li>The managed bean referenced by the value binding is not yet
   *   in a coherent state to evaluate the expression</li>
   * </ul>
   * <p>The most common use case of this method is for message attributes
   * set on converters and validators using a value binding referencing
   * a managed bean created by <code>&lt;f:loadBundle/&gt;<code>. Since
   * loadBundle only creates its bean during the render response phase
   * while converter and validators take action during process validation
   * phase, the message property's value binding must be stored in a
   * special <code>FacesMessage</code> implementation that will evaluate
   * the binding only during render response.</p>
   *
   * @param key the parameter key of the raw property value to get.
   *
   * @return the local value of the specified key if it exists, a
   *         <code>ValueExpression</code> object if the specified key
   *         supports expressions and an expression was specified for that
   *         property, <code>null</code> otherwise.
   *
   * @throws IllegalArgumentException if the specified key is a list key.
   *
   * @see #getLocalProperty(PropertyKey)
   * @see #getValueBinding(PropertyKey)
   * @see #getValueExpression(PropertyKey)
   */
  public Object getRawProperty(PropertyKey key);

  /**
   * Set the value expression for a key.
   * @exception IllegalArgumentException if the property does
   *   not support value expressions.
   */
  public void setValueExpression(PropertyKey key, ValueExpression expression);

  /**
   * Set the value binding for a key.
   * @exception IllegalArgumentException if the property does
   *   not support value bindings.
   * @deprecated
   */
  public void setValueBinding(PropertyKey key, ValueBinding binding);

  /**
   * Add an entry to a list.  The same value may be added
   * repeatedly;  null is also a legal value.  (Consumers of
   * this API can apply more stringent rules to specific keys
   * in cover functions.)
   * @exception IllegalArgumentException if the key is not a list key.
   */
  public void addEntry(PropertyKey listKey, Object value);

  /**
   * Remove an entry from a list.
   * @exception IllegalArgumentException if the key is not a list key.
   */
  public void removeEntry(PropertyKey listKey, Object value);

  /**
   * Return as an array all elements of this key that
   * are instances of the specified class.
   * @return an array whose instance type is the class
   * @exception IllegalArgumentException if the key is not a list key.
   */
  // TODO This can, of course, be implemented on top of entries();
  // consider moving to a utility function;  however, it's
  // universally needed by all consumers, so...
  public Object[] getEntries(PropertyKey listKey, Class<?> clazz);

  /**
   * Return true if at least one element of the list identified by
   * this key is an instance of the specified class.
   * @exception IllegalArgumentException if the key is not a list key.
   */
  public boolean containsEntry(PropertyKey listKey, Class<?> clazz);

  /**
   * Returns an iterator over all entries at this key.
   * @exception IllegalArgumentException if the key is not a list key.
   */
  // TODO is this iterator read-only or read-write?
  public Iterator<? extends Object> entries(PropertyKey listKey);

  /**
   * Copies all properties, bindings, and list entries from
   * one bean to another.  If the beans are of different types,
   * properties will be copied by name.  Incompatible properties will be
   * ignored;  specifically, properties that are lists on only one
   * of the beans or ValueBindings on the original bean that
   * are not allowed on the target bean.
   */
  public void addAll(FacesBean from);

  /**
   * Returns a Set of all PropertyKeys that have either lists
   *  or values attached.
   */
  public Set<PropertyKey> keySet();

  /**
   * Returns a Set of all PropertyKeys that have ValueBindings attached.
   */
  public Set<PropertyKey> bindingKeySet();

  /**
   * use a delta tracking state going forward
   */
  public void markInitialState();

  /**
   * @return true if delta state changes are being tracked, otherwise false
   */
  public boolean initialStateMarked();

  /**
   * Reset to a non-delta tracking state.
   */
  public void clearInitialState();

  /**
   * Saves the state of a FacesBean.
   */
  public Object saveState(FacesContext context);

  /**
   * Restores the state of a FacesBean.
   */
  public void restoreState(FacesContext context, Object state);

  /**
   * Type of a FacesBean, encapsulating the set of registered
   * PropertyKeys.
   */
  // TODO Extract as interface?
  public static class Type
  {
    public Type()
    {
      this(null);
    }

    public Type(Type superType)
    {
      _superType = superType;
      // todo initial size of map, and type of map
      // todo initial size of list, and type of list
      // todo build combined data structure
      _keyMap = new HashMap<String, PropertyKey>();
      _keyList = new ArrayList<PropertyKey>();
      _unmodifiableKeys = Collections.unmodifiableList(_keyList);

      if (_superType != null)
      {
        _keyMap.putAll(_superType._keyMap);
        _keyList.addAll(_superType._keyList);
        _index = _superType._index;
        _superType.lock();
      }

    }

    /**
     * Find an existing key by name.
     */
    public PropertyKey findKey(String name)
    {
      return _keyMap.get(name);
    }

    /**
     * Find an existing key by index.
     */
    public PropertyKey findKey(int index)
    {
      if ((index < 0) || (index >= _keyList.size()))
        return null;

      return _keyList.get(index);
    }

    /**
     * Register a new key.
     * @exception IllegalStateException if the type is already locked,
     *    or the key does not already exists.
     */
    public final PropertyKey registerKey(
      String   name,
      Class<?> type,
      Object   defaultValue)
    {
      return registerKey(name, type, defaultValue, 0);
    }

    /**
     * Register a new key.
     * @exception IllegalStateException if the type is already locked,
     *    or the key does not already exists.
     */
    public final PropertyKey registerKey(
      String   name,
      Class<?> type)
    {
      return registerKey(name, type, null, 0);
    }

    /**
     * Register a new key.
     * @exception IllegalStateException if the type is already locked,
     *    or the key does not already exists.
     */
    public final PropertyKey registerKey(
      String name)
    {
      return registerKey(name, Object.class, null, 0);
    }

    /**
     * Register a new key.
     * @exception IllegalStateException if the type is already locked,
     *    or the key does not already exists.
     */
    public final PropertyKey registerKey(
      String name,
      int    capabilities)
    {
      return registerKey(name, Object.class, null, capabilities);
    }

    /**
     * Register a new key.
     * @exception IllegalStateException if the type is already locked,
     *    or the key does not already exists.
     */
    public final PropertyKey registerKey(
      String   name,
      Class<?> type,
      int      capabilities)
    {
      return registerKey(name, type, null, capabilities);
    }

    public PropertyKey registerKey(
      String   name,
      Class<?> type,
      Object   defaultValue,
      int      capabilities)
    {
      return registerKey(name, type, defaultValue, capabilities, null);
    }

    /**
     * Add an alias to an existing PropertyKey.
     * @exception IllegalStateException if the type is already locked,
     *    or a key already exists at the alias.
     */
    public PropertyKey registerAlias(PropertyKey key, String alias)
    {
      _checkLocked();

      if (findKey(alias) != null)
        throw new IllegalStateException();

      _keyMap.put(alias, key);
      return key;
    }


    /**
     * Register a new key with a set of capabilities.
     * @exception IllegalStateException if the type is already locked,
     *    or the key already exists.
     */
    public PropertyKey registerKey(
      String              name,
      Class<?>            type,
      Object              defaultValue,
      int                 capabilities,
      PropertyKey.Mutable mutable)
    {
      _checkLocked();
      _checkName(name);

      if (mutable == null)
        mutable = PropertyKey.Mutable.IMMUTABLE;

      PropertyKey key = createPropertyKey(name,
                                          type,
                                          defaultValue,
                                          capabilities,
                                          getNextIndex(),
                                          mutable);
      addKey(key);
      return key;
    }


    /**
     * Locks the type object, preventing further changes.
     */
    public void lock()
    {
      _isLocked = true;
    }

    /**
     * Locks the type object, preventing further changes.
     */
    public void lockAndRegister(
       /*String renderKitId,*/
       String componentFamily,
       String rendererType)
    {
      lock();
      // =-=AEW We don't yet have the renderKitId available here yet
      TypeRepository.registerType(/*renderKitId, */
                                  componentFamily,
                                  rendererType,
                                  this);
    }

    /**
     * Returns the iterator of registered property keys, excluding aliases.
     */
    public Iterator<PropertyKey> keys()
    {
      return propertyKeys().iterator();
    }

    /**
     * Returns an unmodifiable <code>Collection</code> of registered property keys,
     * excluding aliases.
     *
     * @return unmodifiable <code>Collection</code> with registered
     */
    public Collection<PropertyKey> propertyKeys()
    {
      return _unmodifiableKeys;
    }

    protected PropertyKey createPropertyKey(
      String   name,
      Class<?> type,
      Object   defaultValue,
      int      capabilities,
      int      index)
    {
      return createPropertyKey(name, type, defaultValue, capabilities, index, 
                                                                    PropertyKey.Mutable.IMMUTABLE);
    }

    protected PropertyKey createPropertyKey(
      String              name,
      Class<?>            type,
      Object              defaultValue,
      int                 capabilities,
      int                 index,
      PropertyKey.Mutable mutable)
    {      
      if (_superType != null)
      {
        return _superType.createPropertyKey(name, type, defaultValue,
                                            capabilities, index, mutable);
      }

      return new PropertyKey(name, type, defaultValue, capabilities, index, mutable);
    }

    /**
     * Return the next available index.
     */
    protected int getNextIndex()
    {
      int index = _index;
      _index = index + 1;
      return index;
    }


    /**
     * Add a key to the type.
     * @exception IllegalStateException if the type is already locked,
     *    or a key with that name or index already exists.
     */
    protected void addKey(PropertyKey key)
    {
      _checkLocked();

      // Restore the old key
      PropertyKey oldValue = _keyMap.put(key.getName(), key);
      if (oldValue != null)
      {
        _keyMap.put(key.getName(), oldValue);
        throw new IllegalStateException(_LOG.getMessage(
          "NAME_ALREADY_REGISTERED", key.getName()));
      }

      int index = key.getIndex();
      if (index >= 0)
      {
        _expandListToIndex(_keyList, index);
        oldValue = _keyList.set(index, key);
        if (oldValue != null)
          {
            _keyList.set(index, oldValue);
            throw new IllegalStateException(_LOG.getMessage(
              "INDEX_ALREADY_REGISTERED", index));
          }
      }

      // Set the backpointer
      key.__setOwner(this);
    }


    static private void _expandListToIndex(ArrayList<PropertyKey> list, int count)
    {
      list.ensureCapacity(count + 1);
      int addCount = (count + 1) - list.size();
      for (int i = 0; i < addCount; i++)
        list.add(null);
    }


    private void _checkLocked()
    {
      if (_isLocked)
        throw new IllegalStateException(_LOG.getMessage(
          "TYPE_ALREADY_LOCKED"));
    }

    private void _checkName(String name)
    {
      if (findKey(name) != null)
      {
        throw new IllegalStateException(_LOG.getMessage(
          "NAME_ALREADY_REGISTERED", name));
      }
    }

    private final Map<String, PropertyKey> _keyMap;
    private final List<PropertyKey> _unmodifiableKeys;
    // keyList is used for optimized operations, like findKey(index)
    private final ArrayList<PropertyKey>   _keyList;
    private boolean   _isLocked;
    private int       _index;
    private final Type _superType;
    static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(Type.class);
  }
}
