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

import java.beans.FeatureDescriptor;

import java.lang.reflect.Array;

import java.util.Arrays;
import java.util.Iterator;

import java.util.List;
import java.util.Map;

import javax.el.ELContext;
import javax.el.ELResolver;
import javax.el.PropertyNotFoundException;
import javax.el.PropertyNotWritableException;

import javax.faces.context.FacesContext;
import javax.faces.el.PropertyResolver;
import javax.faces.el.VariableResolver;

public class MELResolver extends ELResolver
{
  public MELResolver(VariableResolver vr, PropertyResolver pr)
  {
    _vr = vr;
    _pr = pr;
  }
  
  @Override
  public Class<?> getCommonPropertyType(ELContext context, Object base)
  {
    if (base == null)
      return String.class;
    if (_isIndexed(base))
      return Integer.class;
    if (base instanceof Map)
      return Object.class;

    return String.class;
  }

  @Override
  public Iterator<FeatureDescriptor> getFeatureDescriptors(ELContext context, 
                                                           Object base)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public Class<?> getType(ELContext context, Object base, 
                          Object property)
  {
    if (base == null)
    {
      Object o = getValue(context, base, property);
      if (o == null)
        return null;
      return o.getClass();
    }
    else
    {
      if (_isIndexed(base))
        return _pr.getType(base, _getIndex(property));
      else
        return _pr.getType(base, property);
    }
  }

  @Override
  public Object getValue(ELContext context, Object base, 
                         Object property)
  {
    if (base == null)
    {
      if (property == null)
        return null;
      FacesContext fc = (FacesContext) context.getContext(FacesContext.class);
      return _vr.resolveVariable(fc, property.toString());
    }
    else
    {
      if (_isIndexed(base))
        return _pr.getValue(base, _getIndex(property));
      else
        return _pr.getValue(base, property);      
    }
  }

  @Override
  public boolean isReadOnly(ELContext context, Object base, 
                            Object property)
  {
    if (base == null)
    {
      return true;
    }
    else
    {
      if (_isIndexed(base))
        return _pr.isReadOnly(base, _getIndex(property));
      else
        return _pr.isReadOnly(base, property);      
    }
  }

  @Override
  public void setValue(ELContext eLContext, Object base, Object property, 
                       Object value)
  {
    if (base == null)
    {
      throw new PropertyNotWritableException();
    }
    else
    {
      if (_isIndexed(base))
        _pr.setValue(base, _getIndex(property), value);
      else
        _pr.setValue(base, property, value);
    }
  }
  
  private int _getIndex(Object property)
  {
    if (property == null)
      throw new PropertyNotFoundException();
    
    if (property instanceof Number)
      return ((Number) property).intValue();
    return Integer.valueOf(property.toString());
  }
  private boolean _isIndexed(Object base)
  {
    if (base == null)
      throw new IllegalArgumentException();
    
    if ((base instanceof List) ||
        base.getClass().isArray())
      return true;
    
    return false;
  }
  
  private final PropertyResolver _pr;
  private final VariableResolver _vr;
}
