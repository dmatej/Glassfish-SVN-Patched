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
package org.apache.myfaces.trinidadinternal.uinode.bind;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * BoundValue that will retrieve entries from a bean.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/bind/EntriesBoundValue.java#0 $) $Date: 10-nov-2005.18:50:06 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class EntriesBoundValue implements BoundValue
{
  /**
   * @param bean the FacesBean
   * @param key  the property name
   */
  public EntriesBoundValue(
    FacesBean   bean,
    String      name)
  {
    if ((bean == null) || (name == null))
      throw new NullPointerException();

    _bean = bean;
    _key = bean.getType().findKey(name);
  }

  /**
   * @param bean the FacesBean
   * @param key  the PropertyKey
   */
  public EntriesBoundValue(
    FacesBean   bean,
    PropertyKey key)
  {
    if ((bean == null) || (key == null))
      throw new NullPointerException();

    _bean = bean;
    _key = key;
  }

  public Object getValue(UIXRenderingContext context)
  {
    return (_key != null) ? _bean.entries(_key) : null;
  }

  private final FacesBean   _bean;
  private final PropertyKey _key;
}

