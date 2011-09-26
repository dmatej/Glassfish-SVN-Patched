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
package org.apache.myfaces.trinidadinternal.skin;

import javax.el.ValueExpression;

import org.apache.myfaces.trinidad.context.LocaleContext;

/**
 * A  Skin class used when no skin is available, to avoid
 * null pointer exceptions, etc. (make this a singleton)
 *
 */
public class SkinNotAvailable extends SkinImpl
{
  /**
   * returns the singleton instance of this class.
   */
  public static SkinNotAvailable getSkinNotAvailable()
  {
    return _INSTANCE;
  }

  @Override
  public String getStyleSheetName()
  {
    return null;
  }

  public Object getTranslatedValue(
    LocaleContext lContext,
    String        namespace,
    String        key
    )
  {
    return "!!!No Skin[" + key + "]!!!";
  }
  
  @Override
  protected String getBundleName()
  {
    return null;
  }
  
  
  @Override
  protected ValueExpression getTranslationSourceValueExpression()
  {
    return null;
  }

  private SkinNotAvailable()
  {   
  }  
  
  private static final SkinNotAvailable _INSTANCE = new SkinNotAvailable();
}
