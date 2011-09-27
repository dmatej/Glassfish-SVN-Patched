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

import org.apache.myfaces.trinidadinternal.taglib.util.TagUtils;

import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.MetaRule;
import javax.faces.view.facelets.Metadata;
import javax.faces.view.facelets.MetadataTarget;
import javax.faces.view.facelets.TagAttribute;

import javax.faces.view.facelets.TagAttributeException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.Locale;


class LocalePropertyTagRule extends MetaRule
{
  static LocalePropertyTagRule Instance = new LocalePropertyTagRule();
  
  private static class LiteralPropertyMetadata extends Metadata
  {
    public LiteralPropertyMetadata(Method method, TagAttribute attribute)
    {
      _method = method;
      _attribute = attribute;
    }
    
    public void applyMetadata(FaceletContext ctx, Object instance)
    {
      if (_params == null)
      {
        Locale loc = TagUtils.getLocale(_attribute.getValue(ctx));        
        _params = new Object[]{loc};
      }
      try
      {
        _method.invoke(instance, _params);
      }
      catch (InvocationTargetException e)
      {
        throw new TagAttributeException(_attribute, e.getCause());
      }
      catch (Exception e)
      {
        throw new TagAttributeException(_attribute, e);
      }
    }
    
    private final Method       _method;
    private final TagAttribute _attribute;
    private       Object[]     _params;
  }
  
  public Metadata applyRule(String name, TagAttribute attribute,
                            MetadataTarget meta)
  {
    if (meta.getPropertyType(name) == _LOCALE_TYPE && attribute.isLiteral())
    {
      Method m = meta.getWriteMethod(name);
      
      // if the property is writable
      if (m != null)
      {
        return new LiteralPropertyMetadata(m, attribute);
      }
    }
    
    return null;
  }
  

  
  static private final Class<? extends Locale> _LOCALE_TYPE = Locale.class;  
  
}
