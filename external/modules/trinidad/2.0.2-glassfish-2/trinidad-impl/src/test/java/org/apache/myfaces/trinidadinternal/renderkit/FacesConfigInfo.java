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
package org.apache.myfaces.trinidadinternal.renderkit;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;

import javax.faces.application.Application;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.XMLValidityTestCase;
import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;
import java.util.List;

public class FacesConfigInfo
{
  public FacesConfigInfo()
  {
  }

  public void registerConverters(Application appl)
  {
    Iterator<String> convertersByType = _convertersByType.keySet().iterator();
    while (convertersByType.hasNext())
    {
      try
      {
        String type = convertersByType.next();
        String converterType = _convertersByType.get(type);
        appl.addConverter(Class.forName(type), converterType);
      }
      catch (Exception e)
      {
        _LOG.severe(e);
      }
    }

    Iterator<String> convertersById = _convertersById.keySet().iterator();
    while (convertersById.hasNext())
    {
      try
      {
        String id = convertersById.next();
        String converterType = _convertersById.get(id);
        appl.addConverter(id, converterType);
      }
      catch (Exception e)
      {
        _LOG.severe(e);
      }
    }
  }

  public void registerComponents(Application appl)
  {
    Iterator<String> components = _components.keySet().iterator();
    while (components.hasNext())
    {
      String type =components.next();
      ComponentInfo info = _components.get(type);
      String className = info.componentClass;
      appl.addComponent(type, className);
    }
  }

  public List<String> getRenderKitFactories()
  {
    return _renderKitFactories;
  }

  public List<String> getLifecycleFactories()
  {
    return _lifecycleFactories;
  }

  public List<String> getFactoriesByName(String factoryName)
  {
    if ("render-kit-factory".equals(factoryName))
      return getRenderKitFactories();
    else if ("lifecycle-factory".equals(factoryName))
      return getLifecycleFactories();
    else
      return Collections.<String>emptyList();
  }

  public static boolean isFactorySupported(String factoryName)
  {
    return ("render-kit-factory".equals(factoryName) ||
            "lifecycle-factory".equals(factoryName));
  }

  public Map<String, RenderKit> getRenderKits()
  {
    return _renderKits;
  }

  public void addConverterByType(String type, String converter)
  {
    _convertersByType.put(type, converter);
  }

  public void addConverterById(String id, String converter)
  {
    _convertersById.put(id, converter);
  }

  public void addComponent(
    ComponentInfo info)
  {
    String componentType = info.componentType;
    if (_LOG.isFine())
      _LOG.fine("Found component: type {0}, class {1}",
                new Object[]{componentType, info.componentClass});
    if (_components.containsKey(componentType))
      _LOG.warning("faces-config.xml contains duplicate definitions for " +
                   "component type {0}", componentType);

    _components.put(componentType, info);
  }

  public ComponentInfo getComponentInfo(String componentType)
  {
    return _components.get(componentType);
  }
  
  public void load(String file) throws IOException, SAXException
  {
    TreeBuilder builder = new TreeBuilder();
    XMLValidityTestCase.ER er = new XMLValidityTestCase.ER();
    String publicID = 
      "-//Sun Microsystems, Inc.//DTD JavaServer Faces Config 1.1//EN";
    // TODO: fall back on RI location if MyFaces can't be found
    URL dtdSource = getClass().getClassLoader().getResource(
       "org/apache/myfaces/resource/web-facesconfig_1_1.dtd");
    er.registerPublicId(publicID, dtdSource);
    builder.setEntityResolver(er);

    Enumeration<URL> resources = getClass().getClassLoader().getResources(file);
    while (resources.hasMoreElements())
    {
      URL resource = resources.nextElement();
      _LOG.info("PARSING " + resource);
      InputStream inputStream = null;
      // Try to get the inputStream off of a file
      if ("file".equalsIgnoreCase(resource.getProtocol()))
      {
        File resourceFile = new File(resource.getFile().replaceAll("%20", " "));
        if (resourceFile.exists())
        {
          inputStream = new FileInputStream(resourceFile);
        }
      }

      if (inputStream == null)
        inputStream = resource.openStream();

      try
      {
        InputSource source = new InputSource(inputStream);
        source.setSystemId(resource.toExternalForm());
        builder.parse(source, new FacesConfigParser(this));
      }
      finally
      {
        inputStream.close();
      }
    }
  }

  static public class ComponentInfo
  {
    public String componentType;
    public String componentClass;
    // Use a TreeMap so the properties always come back in 
    // sorted order.
    public Map<String, PropertyInfo> properties = 
      new TreeMap<String, PropertyInfo>();
    
    public PropertyInfo getPropertyInfo(String name)
    {
      return properties.get(name);
    }
  }

  static public class PropertyInfo
  {
    public Class<?> type;
    public Object defaultValue;
    public List<Object> enumValues;
  }

  private Map<String, ComponentInfo> _components = new HashMap<String, ComponentInfo>();
  private List<String> _renderKitFactories  = new ArrayList<String>();
  private List<String> _lifecycleFactories  = new ArrayList<String>();
  private Map<String, RenderKit> _renderKits  = new HashMap<String, RenderKit>();
  private Map<String, String> _convertersByType  = new HashMap<String, String>();
  private Map<String, String> _convertersById  = new HashMap<String, String>();

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(FacesConfigInfo.class);
}
