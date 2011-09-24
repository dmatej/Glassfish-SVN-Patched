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

import java.io.Serializable;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Key for an entry in a FacesBean.
 */
public class PropertyKey
{
  /**
   * Capability indicating this property does not support bindings.
   */
  static public final int CAP_NOT_BOUND = 1;

  /**
   * Capability indicating this property is transient.
   */
  static public final int CAP_TRANSIENT = 2;

  /**
   * Capability indicating this property describes a list.  List
   * PropertyKeys will automatically be marked as not supporting
   * bindings.
   */
  static public final int CAP_LIST = 4;

  /**
   * Capability indicating this property can use the StateHolder API.
   */
  static public final int CAP_STATE_HOLDER = 8;

  /**
   * Capability indicating this property can use the PartialStateHolder API.
   */
  static public final int CAP_PARTIAL_STATE_HOLDER = 16;

  /**
   * Indicates whether or not a property is mutable, and if so how likely it is to actually
   * be mutated. For example an array is always mutable, but it may be a string array
   * that is very unlikely to be mutated
   */
  public enum Mutable {
     IMMUTABLE,
     RARELY,
     SOMETIMES,
     OFTEN;

     public boolean isAtLeastSometimesMutable()
     {
       return (compareTo(SOMETIMES) > -1);
     }
  };

  /**
   * Create a named PropertyKey, not attached to any type.
   * @see #getDefaultPropertyKey
   */
  static public PropertyKey createPropertyKey(String name)
  {
    return new PropertyKey(name);
  }

  private static final ConcurrentMap<String, PropertyKey> _sDefaultKeyCache =
                                            new ConcurrentHashMap<String, PropertyKey>();

  /**
   * Returns a named PropertyKey of type Object
   */
  public static PropertyKey getDefaultPropertyKey(String name)
  {
    PropertyKey cachedKey = _sDefaultKeyCache.get(name);

    if (cachedKey == null)
    {
      cachedKey = new PropertyKey(name);

      // we don't need putIfAbsent because we don't care about identity
      _sDefaultKeyCache.put(name, cachedKey);
    }

    return cachedKey;
  }

  //
  // Constructors, all package-private.  Only the constructor
  // that takes a simple String could be exposed at all safely;
  // note that state saving cannot possibly re-create an
  // anonymous PropertyKey with anything other than default metadata,
  // so it is only safe to create such a PropertyKey as part
  // of a FacesBean.Type.
  //
  PropertyKey(
    String name)
  {
    this(name, _TYPE_DEFAULT);
  }

  PropertyKey(
    String   name,
    Class<?> type)
  {
    this(name, type, null);
  }

  PropertyKey(
    String   name,
    Class<?> type,
    Object   defaultValue)
  {
    this(name, type, defaultValue, _CAPS_DEFAULT, -1);
  }

  // Needs to be protected for UINodePropertyKey implementation
  protected PropertyKey(
    String   name,
    Class<?> type,
    Object   defaultValue,
    int      capabilities,
    int      index)
  {
    this(name, type, defaultValue, capabilities, index, Mutable.IMMUTABLE);
  }

  // Needs to be protected for UINodePropertyKey implementation
  protected PropertyKey(
    String   name,
    Class<?> type,
    Object   defaultValue,
    int      capabilities,
    int      index,
    Mutable  mutable)
  {
    if (mutable == null)
      throw new NullPointerException();

    if (name == null)
      throw new NullPointerException();

    if (type == null)
      throw new NullPointerException();

    if (defaultValue != null)
    {
      // Ensure that default value is legal for this property type.
      Class<?> boxedType = _getBoxedType(type);
      if (!boxedType.isAssignableFrom(defaultValue.getClass()))
      {
        throw new IllegalStateException(_LOG.getMessage(
          "DEFAULT_VALUE_IS_NOT_ASSIGNED_TO_TYPE", new Object[]{defaultValue, type}));
      }
    }
    else
    {
      // Default the default value according to Java Language Specification
      defaultValue = _getJavaDefault(type);

      // simplify equality testing in .equals()
      if (defaultValue == null)
        defaultValue = _OBJECT_NULL;
    }

    if ((capabilities & ~_CAPS_ALL) != 0)
      throw new IllegalStateException(_LOG.getMessage(
        "CAPABILITY_MASK_NOT_UNDERSTOOD", (capabilities & ~_CAPS_ALL)));

    // Lists cannot be bound
    boolean hasListCapability = (capabilities & CAP_LIST) != 0;

    if (hasListCapability)
      capabilities = capabilities | CAP_NOT_BOUND;

    _name = name;
    _type = type;
    _default = defaultValue;
    _capabilities = capabilities;
    _index = index;
    _mutable = mutable;

    // save using StatUtils.saveList if the value is of type list
    _serializeAsList = hasListCapability || LIST_CLASS.isAssignableFrom(_type);
    _hashCode = _name.hashCode();
  }

  /**
   * Returns the type of this property.
   */
  public Class<?> getType()
  {
    return _type;
  }

  /**
   * Returns the default value of this property.
   */
  public Object getDefault()
  {
    return (_default != _OBJECT_NULL) ? _default : null;
  }

  /**
   * Returns the owning type for this property key.
   */
  public FacesBean.Type getOwner()
  {
    return _owner;
  }

  /**
   * Returns true if the property supports being bound.
   */
  public boolean getSupportsBinding()
  {
    return (_capabilities & CAP_NOT_BOUND) == 0;
  }

  /**
   * Returns true if the property is transient.
   */
  public boolean isTransient()
  {
    return (_capabilities & CAP_TRANSIENT) != 0;
  }

  /**
   * Returns true if the property is used to store a list.
   */
  public boolean isList()
  {
    return (_capabilities & CAP_LIST) != 0;
  }

  /**
   * Returns true if the type of this property is mutable
   */
  public Mutable getMutable()
  {
    return _mutable;
  }

  /**
   * Returns true if the property is used to store a PartialStateHolder.
   */
  public boolean isPartialStateHolder()
  {
    return (_capabilities & CAP_PARTIAL_STATE_HOLDER) != 0;
  }

  /**
   * Returns the name of this property.
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Returns the index of this property.
   */
  public int getIndex()
  {
    return _index;
  }


  public Object saveValue(
    FacesContext context,
    Object value)
  {
    if ((_capabilities & CAP_STATE_HOLDER) != 0)
      return StateUtils.saveStateHolder(context, value);

    // only serialize as list if this really is a list.  This is necessary because value
    // could be a ValueExpression
    if (this._serializeAsList && (value instanceof List))
      return StateUtils.saveList(context, value);

    // warn if we are state saving non-serializable values, as this will cause client
    // state saving and fail-over to fail.  See JIRA 1236
    if ((value != null) && !(value instanceof Serializable) && _LOG.isWarning())
      _LOG.warning(_LOG.getMessage("UNSERIALIZABLE_PROPERTY_VALUE_NO_CONTAINER",
                                   new Object[]{value, this}));

    return value;
  }

  public Object restoreValue(
    FacesContext context,
    Object savedValue)
  {
    if ((_capabilities & CAP_STATE_HOLDER) != 0)
      return StateUtils.restoreStateHolder(context, savedValue);

    if (this._serializeAsList && (savedValue instanceof Object[]))
      return StateUtils.restoreList(context, savedValue);

    return savedValue;
  }


  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;

    if (!(o instanceof PropertyKey))
      return false;

    PropertyKey that = (PropertyKey)o;

    // If we're not both in the same Type, we're not equals.
    if (_owner != that._owner)
      return false;

    // If the index is -1, then it might be an anonymous
    // type, in which case we have to compare names
    int index = this._index;
    if (index == -1)
    {
      return ((that._index == -1) &&
              (that._name.equals(_name)) &&
              (that._type.equals(_type)) &&
              (that._default.equals(_default)));
    }

    // But otherwise, since the types are the same, then the
    // same index would imply the same instance - which would
    // have been caught up above.  So, again, this ain't the same equal
    return false;
  }

  @Override
  public int hashCode()
  {
    return _hashCode;
  }

  @Override
  public String toString()
  {
    String className = getClass().getName();
    int lastPeriod = className.lastIndexOf('.');
    if (lastPeriod >= 0)
      className = className.substring(lastPeriod + 1);

    if (_index >= 0)
      return className + "[" + _name + "," + _index + "]";
    return className + "[" + _name + "]";
  }

  void __setOwner(FacesBean.Type owner)
  {
    _owner = owner;
  }

  static private Object _getJavaDefault(
    Class<?> type)
  {
    return _PRIMITIVE_DEFAULTS.get(type);
  }

  static private Class<?> _getBoxedType(
    Class<?> type)
  {
    Class<?> boxedType = _BOXED_PRIMITIVES.get(type);
    return (boxedType != null ? boxedType : type);
  }

  static private Map<Class<?>, Object> _createPrimitiveDefaults()
  {
    Map<Class<?>, Object> map = new HashMap<Class<?>, Object>();
    map.put(Boolean.TYPE, Boolean.FALSE);
    map.put(Byte.TYPE, Byte.valueOf((byte)0));
    map.put(Character.TYPE, Character.valueOf('\0'));
    map.put(Double.TYPE, Double.valueOf(0.0));
    map.put(Float.TYPE, Float.valueOf(0.0f));
    map.put(Integer.TYPE, Integer.valueOf(0));
    map.put(Long.TYPE, Long.valueOf(0L));
    map.put(Short.TYPE, Short.valueOf((short)0));

    return Collections.unmodifiableMap(map);
  }

  static private Map<Class<?>, Class<?>> _createBoxedPrimitives()
  {
    Map<Class<?>, Class<?>> map = new HashMap<Class<?>, Class<?>>();
    map.put(Boolean.TYPE, Boolean.class);
    map.put(Byte.TYPE, Byte.class);
    map.put(Character.TYPE, Character.class);
    map.put(Double.TYPE, Double.class);
    map.put(Float.TYPE, Float.class);
    map.put(Integer.TYPE, Integer.class);
    map.put(Long.TYPE, Long.class);
    map.put(Short.TYPE, Short.class);

    return Collections.unmodifiableMap(map);
  }

  static private final Map<Class<?>, Object>   _PRIMITIVE_DEFAULTS = _createPrimitiveDefaults();
  static private final Map<Class<?>, Class<?>> _BOXED_PRIMITIVES   = _createBoxedPrimitives();

  private final int     _hashCode;
  private final String   _name;
  private final int      _index;
  private final int      _capabilities;
  private final Class<?> _type;
  private final Object   _default;
  // true if we should use StateUtils.saveList() to save the state
  private final boolean  _serializeAsList;
  private       FacesBean.Type _owner;
  private final Mutable  _mutable;

  private static final Class<List> LIST_CLASS = List.class;

  static private final int _CAPS_DEFAULT =
    0;

  static private final int _CAPS_ALL =
    CAP_NOT_BOUND |
    CAP_TRANSIENT |
    CAP_LIST |
    CAP_STATE_HOLDER|
    CAP_PARTIAL_STATE_HOLDER;

  static private final Class<Object> _TYPE_DEFAULT = Object.class;

  static private final Object _OBJECT_NULL = new Object();
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    PropertyKey.class);
}




