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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * BoundValue used to retrieve a translated String from the Skin,
 * with a specified key.
 * <p>
 * If either the the key can not be found, an error message
 * will be written to the error log.
 * <p>
 * @see org.apache.myfaces.trinidadinternal.share.nls.LocaleContext
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/SkinTranslatedBoundValue.java#0 $) $Date: 10-nov-2005.18:53:07 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SkinTranslatedBoundValue implements BoundValue
{
  /**
   * Create a SkinTranslatedBoundValue that will retrieve the translated String
   * specified by <b>key</b> from the Skin.
   * @param key Key to use to look up value in ResourceBundle
   */
  public SkinTranslatedBoundValue(
    String key
    )
  {
    if (key == null)
    {
      throw new IllegalArgumentException();
    }

    _key = key;
  }


  /**
   * Retrieves the translated value from the ResourceBundle.
   * @param context the rendering context
   */
  public Object getValue(
    UIXRenderingContext context
    )
  {

    return context.getTranslatedValue(_key); 
  }

  private String _key;
}
