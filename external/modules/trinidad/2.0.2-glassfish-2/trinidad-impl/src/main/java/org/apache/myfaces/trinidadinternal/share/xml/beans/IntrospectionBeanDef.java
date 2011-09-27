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
package org.apache.myfaces.trinidadinternal.share.xml.beans;

import java.beans.BeanInfo;
import java.beans.PropertyDescriptor;

import java.util.HashMap;
import java.util.Map;

import org.xml.sax.Attributes;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.util.JavaIntrospector;

/**
 * Implementation of BeanDef that uses introspection
 * to identify properties.  The classes used by this
 * must have the following properties:
 * <ol>
 *  <li>Required: the class must be public
 *  <li>Required: a public, zero-arg constructor
 *  <li>Required: public "setter" methods for each supported property.
 *  <li>Optional: public "getter" methods for each supported property.
 * </ol>
 * The methods must be named per standard JavaBean naming conventions.
 * While getter methods are not strictly required, they are recommended
 * as some subclasses of IntrospectionBeanDef do need to get
 * properties after they've been set.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/beans/IntrospectionBeanDef.java#0 $) $Date: 10-nov-2005.18:59:20 $
 */
public class IntrospectionBeanDef extends BeanDef
{
  /**
   * Create an IntrospectionBeanDef.
   * @param className
   */
  public IntrospectionBeanDef(String className)
  {
    this(className, null);
  }


  /**
   * Create an IntrospectionBeanDef.
   * @param className
   */
  public IntrospectionBeanDef(String className, String defaultProperty)
  {
    _className = className;
    _defaultProperty = defaultProperty;
  }

  @Override
  public PropertyDef getPropertyDef(String name)
  {
    Map<String, PropertyDef> defs = _getPropertyDefs();
    if (defs != null)
    {
      return defs.get(name);
    }

    return null;
  }

  @Override
  public PropertyDef getPropertyDef(String namespace, String name)
  {
    if ("".equals(namespace))
      return getPropertyDef(name);

    return null;
  }

  @Override
  public Object createBean(
    String    namespaceURI,
    String    localName) throws ClassNotFoundException,
                                InstantiationException,
                                IllegalAccessException
  {
    Class<?> cls = _getClass();
    return cls.newInstance();
  }

  @Override
  public Object finishBean(Object bean)
  {
    return bean;
  }

  @Override
  public PropertyDef getElementPropertyDef(
    String namespace, 
    String name, 
    Attributes attrs)
  {
    return getPropertyDef(namespace, name);
  }

  @Override
  public PropertyDef getDefaultPropertyDef()
  {
    if (_defaultProperty == null)
      return null;

    return getPropertyDef(_defaultProperty);
  }

  @Override
  public boolean isInlineChildProperty(
    String namespace, 
    String name, 
    PropertyDef def)
  {
    return false;
  }

  /**
   * Force the definition of the bean to be loaded.  Subclasses
   * should call this method if they override <code>addProperty</code>
   * @see #addProperty
   */
  synchronized protected void loadProperties()
  {
    if (_defs == null)
    {
      Map<String, PropertyDef> defs;

      try
      {
        Class<?> objClass = _getClass();
        // Grab all the getters and setters using JavaBeans
        BeanInfo info = JavaIntrospector.getBeanInfo(objClass);
        PropertyDescriptor[] descriptors = info.getPropertyDescriptors();

        int length = (descriptors != null) ? descriptors.length : 0;
        defs = new HashMap<String, PropertyDef>(Math.max(length, 1));
        _defs = defs;

        for (int i = 0; i < length; i++)
        {
          PropertyDescriptor property = descriptors[i];
          String name   = property.getName();
          if ((name != null) && (property.getWriteMethod() != null))
          {
            addProperty(property);
          }
        }
      }
      catch (Exception e)
      {
        // =-=AEW ERROR???
        _defs = new HashMap<String, PropertyDef>(1);
      }
    }
  }

  /**
   * Hook for overriding the behavior of a found property.
   * By default, the PropertyDescriptor will be created by
   * calling createPropertyDef().
   * <p>
   * @param descriptor the PropertyDescriptor
   */
  protected void addProperty(PropertyDescriptor descriptor)
  {
    _defs.put(descriptor.getName(), createPropertyDef(descriptor));
  }

  /**
   * Hook for overriding the behavior of a found property.
   * By default, the PropertyDescriptor will be turned
   * directly into an IntrospectionPropertyDef, available
   * as a non-namespaced property.
   * <p>
   * @param descriptor the PropertyDescriptor
   * @return the property def
   */
  protected PropertyDef createPropertyDef(PropertyDescriptor descriptor)
  {
    return new IntrospectionPropertyDef(descriptor);
  }

  // Lazily retrive property defs.
  private Map<String, PropertyDef> _getPropertyDefs()
  {
    // Force the properties to be loaded
    loadProperties();
    return _defs;
  }

  // Lazily retrive the class.
  private Class<?> _getClass() throws ClassNotFoundException
  {
    if (_class == null)
    {
      _class = ClassLoaderUtils.loadClass(_className);
    }

    return _class;
  }


  private final String _className;
  private final String _defaultProperty;
  private Class<?>     _class;
  private Map<String, PropertyDef> _defs;
}
