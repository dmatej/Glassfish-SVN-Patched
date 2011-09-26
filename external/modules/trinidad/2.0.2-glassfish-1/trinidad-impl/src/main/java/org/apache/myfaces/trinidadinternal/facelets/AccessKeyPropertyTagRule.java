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

import javax.faces.view.facelets.FaceletContext;
import javax.faces.view.facelets.Metadata;
import javax.faces.view.facelets.MetadataTarget;
import javax.faces.view.facelets.MetaRule;
import javax.faces.view.facelets.TagAttribute;
import javax.faces.view.facelets.TagAttributeException;

import javax.el.ValueExpression;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidadinternal.taglib.util.VirtualAttributeUtils;
import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;

/**
 * 
 * @version $Id: StringArrayPropertyTagRule.java,v 1.1 2005/08/23 05:54:54 adamwiner Exp $
 */
final class AccessKeyPropertyTagRule extends MetaRule
{
  public static final MetaRule Instance = new AccessKeyPropertyTagRule();

  /**
   * Metadata for non-literal "AndAccessKey" attributes
   */
  private static class AccessKeyMetadata extends Metadata
  {
    public AccessKeyMetadata(
      String mainMethodName, 
      TagAttribute attribute)
    {
      _mainMethodName = mainMethodName;
      _attribute = attribute;
    }
    
    @Override
    @SuppressWarnings("deprecation")
    public void applyMetadata(FaceletContext ctx, Object instance)
    {
      ValueExpression expr = _attribute.getValueExpression(ctx, String.class);
      UIXComponent uixcomp = (UIXComponent) instance;
      FacesBean bean = uixcomp.getFacesBean();
      PropertyKey mainKey = bean.getType().findKey(_mainMethodName);
      if (mainKey == null)
        throw new TagAttributeException(_attribute,
                                        "No support for '" + _mainMethodName +
                                        "' attribute on " + instance);
      PropertyKey accessKeyKey = bean.getType().findKey("accessKey");
      if (accessKeyKey == null)
        throw new TagAttributeException(_attribute,
                                        "No support for 'accessKey' attribute on " + instance);
      VirtualAttributeUtils.setAccessKeyAttribute(bean, expr, mainKey, accessKeyKey);
    }

    private final String       _mainMethodName;
    private final TagAttribute _attribute;
  }

  /**
   * Metadata for literal (non-EL) "AndAccessKey" attributes
   */
  private static class LiteralAccessKeyMetadata extends Metadata
  {
    public LiteralAccessKeyMetadata(
      Method mainMethod, 
      Method accessKeyMethod, 
      TagAttribute attribute)
    {
      _mainMethod = mainMethod;
      _accessKeyMethod = accessKeyMethod;

      String text = attribute.getValue();

      int accessKeyIndex = StringUtils.getMnemonicIndex(text);
      if (accessKeyIndex != StringUtils.MNEMONIC_INDEX_NONE)
      {
        _accessKey = Character.valueOf(text.charAt(accessKeyIndex + 1));
        text = StringUtils.stripMnemonic(text);
      }
      else
      {
        _accessKey = null;
      }
      
      _text = text;
      _attribute = attribute;
    }
    
    @Override
    public void applyMetadata(FaceletContext ctx, Object instance)
    {
      try
      {
        if (_accessKey != null)
          _accessKeyMethod.invoke(instance, _accessKey);
        _mainMethod.invoke(instance, _text);
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

    private final Method       _mainMethod;
    private final Method       _accessKeyMethod; 
    private final TagAttribute _attribute;
    private final String       _text;
    private final Character    _accessKey;
  }
   

  @Override
  public Metadata applyRule(
     String name,
     TagAttribute attribute,
     MetadataTarget meta)
  {
    if (name.endsWith("AndAccessKey"))
    {
      String mainProperty = name.substring(0, name.length() - "AndAccessKey".length());
      Method mainM = meta.getWriteMethod(mainProperty);
      Method accessKeyM = meta.getWriteMethod("accessKey");
      
      // if the property is writable
      if ((mainM != null) && (accessKeyM != null))
      {
        if (attribute.isLiteral())
          return new LiteralAccessKeyMetadata(mainM, accessKeyM, attribute);
        else
          return new AccessKeyMetadata(mainProperty, attribute);
      }
    }
    return null;
  }
}
