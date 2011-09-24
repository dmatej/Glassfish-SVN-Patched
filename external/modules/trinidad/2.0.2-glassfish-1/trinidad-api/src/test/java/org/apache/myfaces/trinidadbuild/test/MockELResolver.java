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
package org.apache.myfaces.trinidadbuild.test;

import java.beans.FeatureDescriptor;

import java.util.Iterator;
import java.util.List;

import javax.el.ELResolver;
import javax.el.ELContext;

import javax.faces.context.FacesContext;
import javax.faces.el.VariableResolver;
import javax.faces.el.PropertyResolver;

/**
 * Mock ELResolver based on PropertyResolver and VariableResolver
 */
public class MockELResolver extends ELResolver
{
  public MockELResolver(VariableResolver vr, PropertyResolver pr)
  {
    _vr = vr;
    _pr = pr;
  }

  public Class<?> getCommonPropertyType(ELContext context, Object base) 
  {
    if (base == null)
      return Object.class;

    if (base instanceof List || base.getClass().isArray())
      return Integer.class;

    return String.class;
  }

  public Iterator<FeatureDescriptor> getFeatureDescriptors(
    ELContext context, Object base)
  {
    // Need a proper implementation
    throw new UnsupportedOperationException();
  }

  public  Class<?> getType(ELContext context, Object base, Object property)
  {
    FacesContext fc = FacesContext.getCurrentInstance();
    if (base == null)
    {
      if (property != null)
      {
        Object o = _vr.resolveVariable(fc, property.toString());
        if (o != null)
        {
          context.setPropertyResolved(true);
          return o.getClass();
        }
      }
    }
    else
    {
      if (property != null)
      {
        context.setPropertyResolved(true);
        if (property instanceof Number)
          return _pr.getType(base, ((Number) property).intValue());
        return _pr.getType(base, property);
      }
    }
    
    return null;
  }

  public Object getValue(ELContext context, Object base, Object property)
  {
    FacesContext fc = FacesContext.getCurrentInstance();
    if (base == null)
    {
      if (property != null)
      {
        Object o = _vr.resolveVariable(fc, property.toString());
        if (o != null)
        {
          context.setPropertyResolved(true);
          return o;
        }
      }
    }
    else
    {
      if (property != null)
      {
        context.setPropertyResolved(true);
        if (property instanceof Number)
          return _pr.getValue(base, ((Number) property).intValue());
        return _pr.getValue(base, property);
      }
    }

    return null;
  }

  public boolean isReadOnly(ELContext context, Object base, Object property)
  {
    FacesContext fc = FacesContext.getCurrentInstance();
    if (base == null)
    {
      // Can't handle this case - don't set "property resolved"
      return false;
    }
    else
    {
      if (property != null)
      {
        context.setPropertyResolved(true);
        if (property instanceof Number)
          return _pr.isReadOnly(base, ((Number) property).intValue());
        return _pr.isReadOnly(base, property);
      }
    }

    return false;
  }

  public void setValue(ELContext context, Object base, Object property, Object value) 
  {
    FacesContext fc = FacesContext.getCurrentInstance();
    if (base == null)
    {
      // No support for this case.
    }
    else
    {
      if (property != null)
      {
        context.setPropertyResolved(true);
        if (property instanceof Number)
          _pr.setValue(base, ((Number) property).intValue(), value);
        _pr.setValue(base, property, value);
      }
    }
  }

  private final PropertyResolver _pr;
  private final VariableResolver _vr;
}