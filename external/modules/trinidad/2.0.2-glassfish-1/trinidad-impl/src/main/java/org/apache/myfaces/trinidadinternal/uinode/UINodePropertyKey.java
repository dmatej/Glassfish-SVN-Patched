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
package org.apache.myfaces.trinidadinternal.uinode;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.bean.PropertyKey;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

/**
 * PropertyKey subclass that knows about AttributeKeys.
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class UINodePropertyKey extends PropertyKey
{
  public AttributeKey getAttributeKey()
  {
    return _attributeKey;
  }

  UINodePropertyKey(
    String   name,
    Class<?> type,
    Object   defaultValue,
    int      capabilities,
    int      index)
  {
    this(name, type, defaultValue, capabilities, index, Mutable.IMMUTABLE);
  }

  UINodePropertyKey(
    String              name,
    Class<?>            type,
    Object              defaultValue,
    int                 capabilities,
    int                 index,
    PropertyKey.Mutable mutable)
  {
    super(name, type, defaultValue, capabilities, index, mutable);
    _attributeKey = _UICONSTANTS_KEYS.get(name);
  }

  private final AttributeKey _attributeKey;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UINodePropertyKey.class);

  // Set of keys that are explicitly excluded, because the
  // old UIX 2 meaning differs from the new UIX 3 meaning.
  // Such keys must be explicitly mapped in subclasses of
  // UINodeFacesBean
  static private final Set<String> _EXCLUDED_KEYS = new HashSet<String>();
  static
  {
    _EXCLUDED_KEYS.add("required");
    _EXCLUDED_KEYS.add("id");
    _EXCLUDED_KEYS.add("value");

    // =-=AEW When we can push the context-relative URL code down
    // into UIX, these can go back to being normal AttributeKeys
    _EXCLUDED_KEYS.add("source");
    _EXCLUDED_KEYS.add("destination");
    _EXCLUDED_KEYS.add("longDescURL");
    _EXCLUDED_KEYS.add("icon");
    _EXCLUDED_KEYS.add("validators");
  }

  // Map of String names to AttributeKey objects for all
  // keys defined in
  static private final Map<String, AttributeKey> _UICONSTANTS_KEYS;
  static
  {
    _UICONSTANTS_KEYS = new HashMap<String, AttributeKey>();
    try
    {
      _findKeys();
    }
    catch (Throwable t)
    {
      _LOG.severe(t);
    }
  }

  // Locate all AttributeKeys defined in UIConstants
  static private void _findKeys() throws Throwable
  {
    Field[] fields = UIConstants.class.getFields();
    for (int i = 0; i < fields.length; i++)
    {
      Field field = fields[i];
      if ((field.getType() == AttributeKey.class) &&
          Modifier.isStatic(field.getModifiers()))
      {
        AttributeKey key = (AttributeKey) field.get(null);
        String attrName = key.getAttributeName();
        if (!_EXCLUDED_KEYS.contains(attrName))
        {
          _UICONSTANTS_KEYS.put(attrName, key);

          // handle the "hAlign", "vAlign" case
          if (Character.isLowerCase(attrName.charAt(0)) &&
              Character.isUpperCase(attrName.charAt(1)))
          {
            char[] chars = attrName.toCharArray();
            chars[1] = Character.toLowerCase(chars[1]);
            _UICONSTANTS_KEYS.put(new String(chars).intern(), key);
          }
          // Handle lower-cased versions of "onXXX"
          else if (attrName.startsWith("on") &&
                   Character.isUpperCase(attrName.charAt(2)))
          {
            // "onDoubleClick" --> ondblclick
            if (attrName.equals("onDoubleClick"))
              _UICONSTANTS_KEYS.put("ondblclick", key);
            else
              _UICONSTANTS_KEYS.put(attrName.toLowerCase(), key);
          }
        }
      }

    }

    _UICONSTANTS_KEYS.put("messageDescUrl", UIConstants.LONG_DESC_URL_ATTR);
    _UICONSTANTS_KEYS.put("messageTargetFrame", UIConstants.TARGET_FRAME_ATTR);
  }
}
