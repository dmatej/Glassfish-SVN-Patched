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

import java.io.InputStream;
import java.io.IOException;
import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ThreadLocalUtils;


/**
 * Base interface for FacesBean storage.
 *
 */
public class FacesBeanFactory
{
  /**
   * Create a FacesBean for a component class.
   */
  // TODO change from ownerClass to componentFamily?
  static public FacesBean createFacesBean(
    Class<?> ownerClass,
    String   rendererType)
  {
    if (ownerClass == null)
      return null;

    String className = ownerClass.getName();
    FacesBean bean = createFacesBean(className, rendererType);

    if (bean == null && rendererType != null)
    {
      bean = createFacesBean(className, null);
      _cacheFacesBeanClass(bean, className, rendererType);
    }
    
    if (bean == null)
    {
      bean = createFacesBean(ownerClass.getSuperclass(), rendererType);
      _cacheFacesBeanClass(bean, className, rendererType);
    }

    return bean;
  }

  static public FacesBean createFacesBean(
    String beanType,
    String rendererType)
  {
    String typeKey = _buildTypeKey(beanType, rendererType);

    Class<?> type = _TYPES_CLASS.get(typeKey);
      
    if(type == null)
    {
      String className = (String) _TYPES_MAP.get(typeKey);
      if (className == null)
        return null;
      
      // At this point we did not have a cached FacesBean class for the
      // typeKey, but we did have a cached className for the typeKey.
      //  Get the FacesBean class from the className and cache.
      // This will improve performance based on tests.
      try
      {
        type = _getClassLoader().loadClass(className);
        _TYPES_CLASS.put(typeKey, type);
      }
      catch (ClassNotFoundException cnfe)
      {
        _LOG.severe("CANNOT_FIND_FACESBEAN", className);
        _LOG.severe(cnfe);
      }
    }
  
    try
    {
      return (FacesBean) type.newInstance();
    }
    catch (IllegalAccessException iae)
    {
      _LOG.severe("CANNOT_CREATE_FACESBEAN_INSTANCE", type.getName());
      _LOG.severe(iae);
    }
    catch (InstantiationException ie)
    {
      _LOG.severe("CANNOT_CREATE_FACESBEAN_INSTANCE", type.getName());
      _LOG.severe(ie);
    }

    return null;
  }

  static private void _initializeBeanTypes()
  {
    _TYPES_MAP = new HashMap<Object, Object>();

    List<URL> list = new ArrayList<URL>();
    try
    {
      Enumeration<URL> en = _getClassLoader().getResources(
                                "META-INF/faces-bean.properties");
      while (en.hasMoreElements())
      {
        list.add(en.nextElement());
      }

      Collections.reverse(list);
    }
    catch (IOException ioe)
    {
      _LOG.severe(ioe);
      return;
    }

    if (list.isEmpty())
    {
      if (_LOG.isInfo())
        _LOG.info("NO_FACES_BEAN_PROPERTIES_FILES_LOCATED");
    }

    for(URL url : list)
    {
      _initializeBeanTypes(url);
    }
  }

  static private void _initializeBeanTypes(URL url)
  {
    try
    {
      Properties properties = new Properties();
      InputStream is = url.openStream();
      try
      {
        properties.load(is);
        if (_LOG.isFine())
          _LOG.fine("Loading bean factory info from " + url);
        
        _TYPES_MAP.putAll(properties);
      }
      finally
      {
        is.close();
      }
    }
    catch (IOException ioe)
    {
      _LOG.severe("CANNOT_LOAD_URL", url);
      _LOG.severe(ioe);
    }
  }


  static private ClassLoader _getClassLoader()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      loader = FacesBeanFactory.class.getClassLoader();
    return loader;
  }

  /* given non-null beanType & rendererType, concatenate together with 
   * a '|' in between
   */
  static private String _buildTypeKey(
    String beanType, 
    String rendererType)
  {
    if (rendererType != null)
    {
      StringBuilder typeKeyBuilder = _getSharedStringBuilder();
      
      typeKeyBuilder.append(beanType).append('|').append(rendererType);
      
      return typeKeyBuilder.toString();
    }
    else
      return beanType;
    
  }
  
  static private void _cacheFacesBeanClass(
    FacesBean bean,
    String beanType, 
    String rendererType)
  {
    // cache the typeKey and the bean's class, for performance
    if(bean != null)
    {
      String typeKey = _buildTypeKey(beanType, rendererType);
      _TYPES_CLASS.put(typeKey, bean.getClass());
    }
  }
  
  /**
   * <p>
   * This gets a single threadlocal shared stringbuilder instance, each time you call
   * _getSharedStringBuilder it sets the length of the stringBuilder instance to 0.
   * </p><p>
   * This allows you to use the same StringBuilder instance over and over.
   * You must call toString on the instance before calling _getSharedStringBuilder again.
   * </p>
   * Example that works
   * <pre><code>
   * StringBuilder sb1 = _getSharedStringBuilder();
   * sb1.append(a).append(b);
   * String c = sb1.toString();
   *
   * StringBuilder sb2 = _getSharedStringBuilder();
   * sb2.append(b).append(a);
   * String d = sb2.toString();
   * </code></pre>
   * <br><br>
   * Example that doesn't work, you must call toString on sb1 before
   * calling __getSharedStringBuilder again.
   * <pre><code>
   * StringBuilder sb1 = _getSharedStringBuilder();
   * StringBuilder sb2 = _getSharedStringBuilder();
   *
   * sb1.append(a).append(b);
   * String c = sb1.toString();
   *
   * sb2.append(b).append(a);
   * String d = sb2.toString();
   * </code></pre>
   *
   */
  static private StringBuilder _getSharedStringBuilder()
  {
    StringBuilder sb = _STRING_BUILDER.get();

    if (sb == null)
    {
      sb = new StringBuilder();
      _STRING_BUILDER.set(sb);
    }

    // clear out the stringBuilder by setting the length to 0
    sb.setLength(0);

    return sb;
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FacesBeanFactory.class);
  static private Map<Object, Object> _TYPES_MAP;
  static private Map<String, Class<?>> _TYPES_CLASS = new ConcurrentHashMap<String, Class<?>>();
  static private final ThreadLocal<StringBuilder> _STRING_BUILDER =
                                                         ThreadLocalUtils.newRequestThreadLocal();

  static
  {
    _initializeBeanTypes();
  }
}
