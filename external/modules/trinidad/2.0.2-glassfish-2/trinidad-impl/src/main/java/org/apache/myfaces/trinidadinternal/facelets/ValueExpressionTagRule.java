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
package org.apache.myfaces.trinidadinternal.facelets;

import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.MetaRule;
import javax.faces.view.facelets.Metadata;
import javax.faces.view.facelets.MetadataTarget;
import javax.faces.view.facelets.TagAttribute;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.el.ValueExpression;


import org.apache.myfaces.trinidad.logging.TrinidadLogger;

class ValueExpressionTagRule extends MetaRule
{
  
  static ValueExpressionTagRule Instance = new ValueExpressionTagRule();
  
  public Metadata applyRule(String name, TagAttribute attribute,
                            MetadataTarget meta)
  {
    // This rule should be used only for objects implementing setValueExpression().
    
    if (!attribute.isLiteral()) 
    {
      Class type = meta.getPropertyType(name);
      if (type == null) {
          type = Object.class;
      }
      return new ValueExpressionMetadata(name, type, attribute);
    }
    
    return null;
  }
  
  private final static class ValueExpressionMetadata extends Metadata
  {
    private final String _name;
    private final TagAttribute _attr;
    private final Class _type;
    
    public ValueExpressionMetadata(String name, Class type, TagAttribute attr) 
    {
      _name = name;
      _attr = attr;
      _type = type;
    }
    
    public void applyMetadata(FaceletContext ctx, Object instance) 
    {
      Class klass = instance.getClass();
      try
      {
        Method setter = klass.getMethod("setValueExpression", _SETTER_ARGS);
        setter.invoke(instance, new Object[]{_name, _attr.getValueExpression(ctx, _type)});
      }
      // No user-readable messages are needed since we should never install this rule
      // for objects not supportingg setvalueExpression() 
      catch (NoSuchMethodException ncm)
      {
        _LOG.severe(ncm);
      }
      catch(IllegalAccessException iae)
      {
        _LOG.severe(iae);
      }
      catch(InvocationTargetException ite)
      {
        _LOG.severe(ite);
      }
    }
    
    private static final Class _SETTER_ARGS[] = {String.class, ValueExpression.class};
  }
  
  private static final TrinidadLogger _LOG = 
        TrinidadLogger.createTrinidadLogger(ValueExpressionTagRule.class);
}
