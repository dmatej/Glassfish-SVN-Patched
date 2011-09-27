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
package org.apache.myfaces.trinidadinternal.convert;

import java.lang.reflect.Constructor;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.ConcurrentHashMap;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * ReverseDiscoveryGenericConverter implementation based on
 * Java Reflection
 */
public class ReflectionConverter extends ReverseDiscoveryGenericConverter
{
  
  public Object convert(Object source, Class<?> targetType)
  {
    Map<Class<?>, Constructor> constructors = _getConstructorMapForTarget(targetType);
    Class sourceClass = source.getClass();
    Constructor c = constructors.get(sourceClass);
    
    // If direct match failed, check if the types are assignable
    if (c == null)
    {
      for (Map.Entry<Class<?>, Constructor> entry: constructors.entrySet())
      {
        if (entry.getKey().isAssignableFrom(sourceClass))
        {
          c = entry.getValue();
          break;
        }
      }
    }
    // We will re-throw reflection-related execptions (which are errors in this code)
    // as runtime IllegalArgumentException
    Exception cause = null;
    
    if (c != null)
    {
      try
      {
        return c.newInstance(source);
      }
      catch(InvocationTargetException e)
      {
        throw new TypeConversionException(source, targetType, e.getTargetException());
      }
      catch(InstantiationException e)
      {
        _LOG.severe(e);
        cause = e;
      }
      catch(IllegalAccessException e)
      {
        _LOG.severe(e);
        cause = e;
      }
    }
     
    throw new IllegalArgumentException(_LOG.getMessage(
      "UNSUPPORTED_CONVERSION", new Object[]{source.getClass(), targetType}), cause);
  }
    
  
  public List<Class<?>> getSourceTypes(Class<?> targetType)
  {
    Map<Class<?>, Constructor> cachedConstructors = _getConstructorMapForTarget(targetType);
    
    if (cachedConstructors == _EMPTY_CONSTRUCTOR_MAP)
      return _EMPTY_SOURCE_LIST;
    else
      return new ArrayList<Class<?>>(cachedConstructors.keySet());
  }
  
  private Map<Class<?>, Constructor> _getConstructorMapForTarget(Class<?> targetType)
  {
    Map<Class<?>, Constructor> cachedConstructors = _cache.get(targetType);
    
    if (cachedConstructors == null)
    {
      cachedConstructors = _EMPTY_CONSTRUCTOR_MAP;      
      
      Constructor constructors[] = targetType.getConstructors();
      for (Constructor c:constructors)
      {
        // Use only public non-depricated constructors
        if (Modifier.isPublic(c.getModifiers()) && c.getAnnotation(Deprecated.class) == null)
        {
          Class<?> params[] = c.getParameterTypes();
          
          // We are looking for all single-parameter constructors
          if (params.length ==1)
          {
            if (cachedConstructors == _EMPTY_CONSTRUCTOR_MAP)
              cachedConstructors = new HashMap<Class<?>, Constructor>();
            
            cachedConstructors.put(params[0], c);
          }
        }
      }
      
      _cache.put(targetType, cachedConstructors);
    }
    return cachedConstructors;
  }
  
  private final ConcurrentHashMap<Class<?>, Map<Class<?>, Constructor>> _cache = 
                   new ConcurrentHashMap<Class<?>, Map<Class<?>, Constructor>>();
  
  private static final Map<Class<?>, Constructor> _EMPTY_CONSTRUCTOR_MAP = Collections.emptyMap();
  private static final List<Class<?>> _EMPTY_SOURCE_LIST = Collections.emptyList();
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ReflectionConverter.class);
}
