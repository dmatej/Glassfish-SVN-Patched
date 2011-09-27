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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;

import javax.faces.component.StateHolder;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.PropertyMap;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Utilities for handling state persistance.
 */
public final class StateUtils
{
  static
  {
    // initialize checks for serialization verification
    boolean checkPropertyStateSerialization = false;
    boolean checkComponentStateSerialization = false;
    boolean checkComponentTreeStateSerialization = false;
    boolean checkSessionSerialization = false;
    boolean checkApplicationSerialization = false;
    boolean checkMangedBeanMutation = false;
    boolean checkAtEndRequest = false;

    String checkSerializationProperty;

    // determine if we need to aggressively check the serialization of components
    // we wrap the check in a try/catch in case of weird ecurity managers
    try
    {
      checkSerializationProperty =
                       System.getProperty("org.apache.myfaces.trinidad.CHECK_STATE_SERIALIZATION");
    }
    catch (Throwable t)
    {
      checkSerializationProperty = null;
    }

    if (checkSerializationProperty != null)
    {
      checkSerializationProperty = checkSerializationProperty.toUpperCase();

      // comma-separated list with allowed whitespace
      String[] paramArray = checkSerializationProperty.split(",");

      Set<String> serializationFlags = new HashSet<String>(Arrays.asList(paramArray));

      if (!serializationFlags.contains("NONE"))
      {
        if (serializationFlags.contains("ALL"))
        {
          checkPropertyStateSerialization = true;
          checkComponentStateSerialization = true;
          checkComponentTreeStateSerialization = true;
          checkSessionSerialization = true;
          checkApplicationSerialization = true;
          checkMangedBeanMutation = true;
          checkAtEndRequest                    = true;
        }
        else
        {
          checkPropertyStateSerialization = serializationFlags.contains("PROPERTY");
          checkComponentStateSerialization = serializationFlags.contains("COMPONENT");
          checkComponentTreeStateSerialization = serializationFlags.contains("TREE");
          checkSessionSerialization = serializationFlags.contains("SESSION");
          checkApplicationSerialization = serializationFlags.contains("APPLICATION");
          checkMangedBeanMutation = serializationFlags.contains("BEANS");
          checkAtEndRequest = serializationFlags.contains("REQUEST");
        }
      }
    }

    _CHECK_PROPERTY_STATE_SERIALIZATION = checkPropertyStateSerialization;
    _CHECK_COMPONENT_STATE_SERIALIZATION = checkComponentStateSerialization;
    _CHECK_COMPONENT_TREE_STATE_SERIALIZATION = checkComponentTreeStateSerialization;
    _CHECK_SESSION_SERIALIZATION = checkSessionSerialization;
    _CHECK_APPLICATION_SERIALIZATION = checkApplicationSerialization;
    _CHECK_MANAGED_BEAN_MUTATATION = checkMangedBeanMutation;
    _CHECK_AT_END_REQUEST = checkAtEndRequest;
  }

  private static final boolean _CHECK_COMPONENT_TREE_STATE_SERIALIZATION;
  private static final boolean _CHECK_COMPONENT_STATE_SERIALIZATION;
  private static final boolean _CHECK_PROPERTY_STATE_SERIALIZATION;
  private static final boolean _CHECK_SESSION_SERIALIZATION;
  private static final boolean _CHECK_APPLICATION_SERIALIZATION;
  private static final boolean _CHECK_MANAGED_BEAN_MUTATATION;
  private static final boolean _CHECK_AT_END_REQUEST;
  
  /**
   * Returns <code>true</code> if properties should be checked for
   * serializability when when generating the view's state object.
   * <p>
   * By default property state serialization checking is off.  It can be
   * enabled by setting the system property
   * <code>org.apache.myfaces.trinidad.CHECK_STATE_SERIALIZATION</code>
   * to <code>all</code> or, more rately, adding <code>property</code> to the
   * comma-separated list of serialization checks to perform.
   * <p>
   * As property serialization checking is expensive, it is usually
   * only enabled after component tree serialization checking has detected
   * a problem
   * @return
   * @see #checkComponentStateSerialization
   * @see #checkComponentTreeStateSerialization
   */
  private static boolean _checkPropertyStateSerialization()
  {
    return _CHECK_PROPERTY_STATE_SERIALIZATION;
  }

  /**
   * Returns <code>true</code> if components should be checked for
   * serializability when when generating the view's state object.
   * <p>
   * By default component state serialization checking is off.  It can be
   * enabled by setting the system property
   * <code>org.apache.myfaces.trinidad.CHECK_STATE_SERIALIZATION</code>
   * to <code>all</code> or, more rarely, adding <code>component</code> to the
   * comma-separated list of serialization checks to perform.
   * <p>
   * As property serialization checking is expensive, it is usually
   * only enabled after component tree serialization checking has detected
   * a problem.  In addition, since component serialization checking only
   * detects the problem component, it is usually combined with
   * property state serialization checking either by specifying <code>all</code>.
   * @return
   * @see #checkComponentTreeStateSerialization
   */
  public static boolean checkComponentStateSerialization(FacesContext context)
  {
    return _CHECK_COMPONENT_STATE_SERIALIZATION;
  }

  /**
   * Returns <code>true</code> if the component tree should be checked for
   * serializability when when generating the view's state object.
   * <p>
   * By default component tree state serialization checking is off.  It can be
   * enabled by setting the system property
   * <code>org.apache.myfaces.trinidad.CHECK_STATE_SERIALIZATION</code>
   * to <code>all</code> or, more commonly, adding <code>tree</code> to the
   * comma-separated list of serialization checks to perform.
   * <p>
   * Because unserializable objects defeat fail-over, it is important to
   * check for serializability when testing applications.  While component
   * tree state serializability checking isn't cheap, it is much faster to
   * initially only enable checking of the component tree and then switch
   * to <code>all</code> testing to determine the problem component and
   * property when the component tree testing determines a problem.
   * @return
   * @see #checkComponentStateSerialization
   */
  public static boolean checkComponentTreeStateSerialization(FacesContext context)
  {
    return _CHECK_COMPONENT_TREE_STATE_SERIALIZATION;
  }

  /**
   * Returns <code>true</code> if Object written to the ExternalContext's Session Map should be
   * checked for Serializability when <code>put</code> is called.
   * <p>
   * Configuring this property allows this aspect of high-availability to be tested without
   * configuring the server to run in high-availability mode.
   * <p>
   * By default session serialization checking is off.  It can be
   * enabled by setting the system property
   * <code>org.apache.myfaces.trinidad.CHECK_STATE_SERIALIZATION</code>
   * to <code>all</code> or, more commonly, adding <code>session</code> to the
   * comma-separated list of serialization checks to perform.
   * @return
   * @see #checkComponentStateSerialization
   * @see #checkComponentTreeStateSerialization
   * @see #checkApplicationSerialization
   * @see #checkManagedBeanMutation
   */
  public static boolean checkSessionSerialization(ExternalContext extContext)
  {
    return _CHECK_SESSION_SERIALIZATION;
  }

  /**
   * Returns <code>true</code> if Object written to the ExternalContext's Application Map should be
   * checked for Serializability when <code>put</code> is called.
   * <p>
   * Configuring this property allows this aspect of high-availability to be tested without
   * configuring the server to run in high-availability mode.
   * <p>
   * By default application serialization checking is off.  It can be
   * enabled by setting the system property
   * <code>org.apache.myfaces.trinidad.CHECK_STATE_SERIALIZATION</code>
   * to <code>all</code> or, more commonly, adding <code>application</code> to the
   * comma-separated list of serialization checks to perform.
   * @return
   * @see #checkComponentStateSerialization
   * @see #checkComponentTreeStateSerialization
   * @see #checkSessionSerialization
   * @see #checkManagedBeanMutation
   */
  public static boolean checkApplicationSerialization(ExternalContext extContext)
  {
    return _CHECK_APPLICATION_SERIALIZATION;
  }

  /**
   * Returns <code>true</code> if the attributes of the session and application Maps should be
   * checked for cases where the attribute was mutated but not dirtied for failover.  If
   * <code>checkSessionSerialization</code> returns <code>true</code>, the contents of the
   * Session should be checked.  If <code>checkApplicationSerialization</code> returns
   * <code>true</code>, the Serializable content of the Application should be checked.
   * @return true if the contents of scopes should be checked for mutation without dirtying.
   * @see #checkApplicationSerialization
   * @see #checkSessionSerialization
   */
  public static boolean checkManagedBeanMutation(ExternalContext extContext)
  {
    return _CHECK_MANAGED_BEAN_MUTATATION;
  }

  /**
   * Returns <code>true</code> if all attributes in the session Map should be
   * checked for serializability at the end of each request.  This check should
   * only be performed if <code>checkSessionSerialization</code> also returns <code>true</code>.
   * @see #checkSessionSerialization
   */
  public static boolean checkScopesAtEndOfRequest(ExternalContext extContext)
  {
    return _CHECK_AT_END_REQUEST;
  }

  /**
   * Persists a property key.
   */
  static public Object saveKey(PropertyKey key)
  {
    int index = key.getIndex();
    if (index < 0)
      return key.getName();

    if (index < 128)
      return Byte.valueOf((byte) index);

    return Integer.valueOf(index);
  }


  /**
   * Restores a persisted PropertyKey.
   */
  static public PropertyKey restoreKey(FacesBean.Type type, Object value)
  {
    PropertyKey key;
    if (value instanceof Number)
    {
      key = type.findKey(((Number) value).intValue());
      if (key == null)
        throw new IllegalStateException(_LOG.getMessage(
          "INVALID_INDEX"));
    }
    else
    {
      key = type.findKey((String) value);
      if (key == null)
        key = PropertyKey.createPropertyKey((String) value);
    }

    return key;
  }

  /**
   * Generic (unoptimized) version of PropertyMap state saving.
   */
  static public Object saveState(
    PropertyMap    map,
    FacesContext   context,
    boolean        useStateHolder)
  {
    int size = map.size();
    if (size == 0)
      return null;

    Object[] values = new Object[2 * size];
    int i = 0;
    for(Map.Entry<PropertyKey, Object> entry : map.entrySet())
    {
      PropertyKey key = entry.getKey();
      if (key.isTransient())
      {
        // TRINIDAD-1956: due to the view root caching functionality, the transient properties
        // may be retained too long. By removing the value here we can ensure that the next
        // request will not have the transient values.
        entry.setValue(null);
        continue;
      }

      Object value = entry.getValue();

      values[i] = saveKey(key);
      if (_LOG.isFinest())
      {
        _LOG.finest("SAVE {" + key + "=" + value + "}");
      }

      Object saveValue;

      if (useStateHolder)
        saveValue = saveStateHolder(context, value);
      else
        saveValue = key.saveValue(context, value);

      // aggressively check the serializability of the value
      if (_checkPropertyStateSerialization())
      {
        try
        {
          new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(saveValue);
        }
        catch (IOException e)
        {
          throw new RuntimeException(_LOG.getMessage("UNSERIALIZABLE_PROPERTY_VALUE",
                                                     new Object[]{saveValue, key, map}),
                                     e);
        }
      }

      values[i + 1] = saveValue;

      i+=2;
    }

    return values;
  }


  /**
   * Generic (unoptimized) version of PropertyMap state restoring.
   */
  static public void restoreState(
    PropertyMap    map,
    FacesContext   context,
    FacesBean.Type type,
    Object         state,
    boolean        useStateHolder)
  {
    if (state == null)
      return;

    Object[] values = (Object[]) state;
    int size = values.length / 2;
    for (int i = 0; i < size; i++)
    {
      Object savedKey = values[i * 2];
      if (savedKey == null)
        continue;

      Object savedValue = values[i * 2 + 1];
      PropertyKey key = restoreKey(type, savedKey);
      Object value;

      if (useStateHolder)
        value = restoreStateHolder(context, savedValue);
      else
        value = key.restoreValue(context, savedValue);

      if (_LOG.isFinest())
      {
        _LOG.finest("RESTORE {" + key + "=" + value + "}");
      }

      map.put(key, value);
    }
  }

  /**
   * Saves an object that may implement StateHolder.
   */
  static public Object saveStateHolder(
    FacesContext context,
    Object       value)
  {
    if (value == null)
      return null;

    Saver saver = null;
    if (value instanceof StateHolder)
    {
      if (((StateHolder) value).isTransient())
        return null;

      saver = new SHSaver();
    }
    else if (value instanceof Serializable)
    {
      return value;
    }
    else
    {
      saver = new Saver();
    }

    if (saver != null)
      saver.saveState(context, value);

    return saver;
  }

  /**
   * Restores an object that was saved using saveStateHolder()
   */
  static public Object restoreStateHolder(
    FacesContext context,
    Object       savedValue)
  {
    if (!(savedValue instanceof Saver))
      return savedValue;

    return ((Saver) savedValue).restoreState(context);
  }



  /**
   * Saves a List whose elements may implement StateHolder.
   */
  @SuppressWarnings("unchecked")
  static public Object saveList(
    FacesContext context,
    Object       value)
  {
    if (value == null)
      return null;

    List<Object> list = (List<Object>) value;
    int size = list.size();
    if (size == 0)
      return null;

    Object[] array = new Object[size];
    // 2006-08-01: -= Simon Lessard =-
    //             Inefficient loop if the list implementation
    //             ever change to a linked data structure. Use
    //             iterators instead
    //for (int i = 0; i < size; i++)
    //  array[i] = saveStateHolder(context, list.get(i));
    int index = 0;
    for(Object object : list)
    {
      array[index++] = saveStateHolder(context, object);
    }

    return array;
  }

  /**
   * Restores a List whose elements may implement StateHolder.
   */
  static public Object restoreList(
    FacesContext context,
    Object       savedValue)
  {
    if (savedValue == null)
      return null;

    Object[] array = (Object[]) savedValue;
    int length = array.length;
    if (length == 0)
      return null;

    List<Object> list = new ArrayList<Object>(length);
    for(Object state : array)
    {
      Object restored = restoreStateHolder(context, state);
      if (restored != null)
      {
        list.add(restored);
      }
    }

    return list;
  }



  /**
   * Instance used to save generic instances;  simply saves
   * the class name.
   */
  static private class Saver implements Serializable
  {
    public void saveState(FacesContext context, Object saved)
    {
      _name = saved.getClass().getName();
    }

    public Object restoreState(FacesContext context)
    {
      // we don't need to use concurrent map methods like putIfAbsent. If someone happens to
      // add a name/value pair again it's fine because as the doc for put in HashMap says
      // "If the map previously contained a mapping for this key, the old value is replaced."
      ConcurrentMap<String, Object> appMap =
                           RequestContext.getCurrentInstance().getApplicationScopedConcurrentMap();


      Map<String, Class> classMap = (Map<String, Class>) appMap.get(_CLASS_MAP_KEY);

      if (classMap == null)
      {
        // the classMap doesn't need to worry about synchronization,
        // if the Class is loaded twice that's fine.
        Map<String, Class> newClassMap = new HashMap<String, Class>();
        Map<String, Class> oldClassMap =
                              (Map<String, Class>) appMap.putIfAbsent(_CLASS_MAP_KEY, newClassMap);

        if (oldClassMap != null)
          classMap = oldClassMap;
        else
          classMap = newClassMap;
      }

      Class clazz = classMap.get(_name);

      if (clazz == null)
      {
        try
        {
          ClassLoader cl = _getClassLoader();
          clazz = cl.loadClass(_name);
          classMap.put(_name, clazz);
        }
        catch (Throwable t)
        {
          _LOG.severe(t);
          return null;
        }
      }

      try
      {
        return clazz.newInstance();
      }
      catch (Throwable t)
      {
        _LOG.severe(t);
        return null;
      }
    }

    private String _name;
    private static final long serialVersionUID = 1L;
  }


  /**
   * Instance used to save StateHolder objects.
   */
  static private class SHSaver extends Saver
  {
    @Override
    public void saveState(FacesContext context, Object value)
    {
      super.saveState(context, value);
      _save = ((StateHolder) value).saveState(context);
    }

    @Override
    public Object restoreState(FacesContext context)
    {
      Object o = super.restoreState(context);
      if (o != null)
        ((StateHolder) o).restoreState(context, _save);

      return o;
    }

    private Object _save;
    private static final long serialVersionUID = 1L;
  }


  //
  // Pick a ClassLoader
  //
  static private ClassLoader _getClassLoader()
  {
    ClassLoader cl = Thread.currentThread().getContextClassLoader();
    if (cl == null)
      cl = StateUtils.class.getClassLoader();

    return cl;
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StateUtils.class);

  private static final String _CLASS_MAP_KEY =
                                           "org.apache.myfaces.trinidad.bean.util.CLASS_MAP_KEY";


}
