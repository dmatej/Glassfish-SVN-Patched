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
package org.apache.myfaces.trinidadinternal.taglib.util;

import javax.el.ValueExpression;

import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.FacesBean;

import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;

import org.apache.myfaces.trinidadinternal.binding.AccessKeyBinding;
import org.apache.myfaces.trinidadinternal.binding.StripAccessKeyBinding;

/**
 *
 */
public class VirtualAttributeUtils
{
  public static void setAccessKeyAttribute(
    FacesBean    bean,
    String       text,
    PropertyKey  textKey,
    PropertyKey  accessKeyKey)
  {
    // set the acess key, if any
    int accessKeyIndex = StringUtils.getMnemonicIndex(text);

    if (accessKeyIndex != StringUtils.MNEMONIC_INDEX_NONE)
    {
      bean.setProperty(accessKeyKey,
                       Character.valueOf(text.charAt(accessKeyIndex + 1)));
    }

    // set the stripped text on the node using the appropriate attribute name
    bean.setProperty(textKey,
                     StringUtils.stripMnemonic(text));
  }

  public static void setAccessKeyAttribute(
    FacesBean    bean,
    ValueExpression valueExpression,
    PropertyKey  textKey,
    PropertyKey  accessKeyKey)
  {
    bean.setValueExpression(accessKeyKey,
                            new AccessKeyBinding(valueExpression));
    bean.setValueExpression(textKey,
                         new StripAccessKeyBinding(valueExpression));
  }

  private VirtualAttributeUtils()
  {
  }
}
