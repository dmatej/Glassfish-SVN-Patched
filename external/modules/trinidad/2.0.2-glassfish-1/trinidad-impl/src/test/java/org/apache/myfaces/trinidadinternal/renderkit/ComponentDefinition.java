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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.application.Application;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import org.apache.myfaces.trinidadinternal.share.expl.Coercions;

public class ComponentDefinition
{
  public ComponentDefinition(String componentType, FacesConfigInfo info)
  {
    _componentType = componentType;
    _info = info;
  }

  public FacesConfigInfo.ComponentInfo getComponentInfo()
  {
    return _info.getComponentInfo(_componentType);
  }

  public boolean isUsesUpload()
  {
    return _usesUpload;
  }

  public void setUsesUpload(boolean usesUpload)
  {
    _usesUpload = usesUpload;
  }

  @SuppressWarnings("unchecked")
  public UIComponent createComponent(FacesContext context)
  {
    Application appl = context.getApplication();
    UIComponent comp = appl.createComponent(_componentType);
    FacesConfigInfo.ComponentInfo info = getComponentInfo();
    
    Iterator<String> names = _attributes.keySet().iterator();
    while (names.hasNext())
    {
      String name = names.next();
      String valueStr = (String) _attributes.get(name);
      if (isValueExpression(valueStr))
      {
        ValueBinding binding = context.getApplication().
          createValueBinding(valueStr);
        comp.setValueBinding(name, binding);
      }
      else
      {
        FacesConfigInfo.PropertyInfo propertyInfo = info.getPropertyInfo(name);
        Object value;
        if (propertyInfo.type == null)
          value = valueStr;
        else
          value = Coercions.coerce(null, valueStr, propertyInfo.type);
        comp.getAttributes().put(name, value);
      }
    }

    Iterator<ComponentDefinition> children = getChildren().iterator();
    while (children.hasNext())
    {
      ComponentDefinition cd = children.next();
      comp.getChildren().add(cd.createComponent(context));
    }

    Iterator<String> facets = getFacets().keySet().iterator();
    while (facets.hasNext())
    {
      String name = facets.next();
      ComponentDefinition cd = getFacets().get(name);
      comp.getFacets().put(name, cd.createComponent(context));
    }

    return comp;
  }

  static public boolean isValueExpression(String expression)
  {
    return ((expression != null) &&
            expression.startsWith("#{") &&
            expression.endsWith("}"));
  }

  public Map<String, Object> getAttributes()
  {
    return _attributes;
  }

  public Map<String, ComponentDefinition> getFacets()
  {
    return _facets;
  }

  public List<ComponentDefinition> getChildren()
  {
    return _children;
  }

  private final String _componentType;
  private boolean      _usesUpload;
  private final Map<String, Object> _attributes = new HashMap<String, Object>();
  private final Map<String, ComponentDefinition> _facets = new HashMap<String, ComponentDefinition>();
  private final List<ComponentDefinition> _children = new ArrayList<ComponentDefinition>();
  private final FacesConfigInfo _info;
}
