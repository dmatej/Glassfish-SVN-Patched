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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.List;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.Collections;

import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.Metadata;
import javax.faces.view.facelets.MetadataTarget;
import javax.faces.view.facelets.MetaRule;
import javax.faces.view.facelets.TagAttribute;
import javax.faces.view.facelets.TagAttributeException;


/**
 *
 * @version $Id: StringArrayPropertyTagRule.java,v 1.1 2005/08/23 05:54:54 adamwiner Exp $
 */
final class StringArrayPropertyTagRule extends MetaRule
{
  public static final MetaRule Instance = new StringArrayPropertyTagRule();

  private static class LiteralPropertyMetadata extends Metadata
  {
    public LiteralPropertyMetadata(Method method, TagAttribute attribute)
    {
      _method = method;
      _attribute = attribute;
    }
    
    @Override
    public void applyMetadata(FaceletContext ctx, Object instance)
    {
      if (_itemList == null)
      {
        _itemList = _coerceToStringArray(_attribute.getValue());
      }
      
      try
      {
        // TRINIDAD-2034 - create a new String array instance every time to avoid issues
        // with sharing mutable objects
        Object params[] = new Object[]{_itemList.isEmpty() ? null : _itemList.toArray(new String[_itemList.size()])};
        _method.invoke(instance, params);
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
    private       List<String> _itemList;
  }
   

  @Override
  public Metadata applyRule(
     String name,
     TagAttribute attribute,
     MetadataTarget meta)
  {
    // Leave expressions to the underlying code
    if ((meta.getPropertyType(name) == _STRING_ARRAY_TYPE) &&
        attribute.isLiteral())
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

  static private List<String> _coerceToStringArray(String str)
  {
    if (str == null)
      return Collections.emptyList();

    ArrayList<String> list = new ArrayList<String>();
    StringTokenizer tokens = new StringTokenizer(str);
    while (tokens.hasMoreTokens())
    {
      list.add(tokens.nextToken());
    }
    
    return list;
  }

  static private final Class<? extends String[]> _STRING_ARRAY_TYPE = (new String[0]).getClass();
}
