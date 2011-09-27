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
package org.apache.myfaces.trinidad.component;

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/component/TableRowEvent.java#0 $) $Date: 10-nov-2005.19:09:48 $
 * @todo To be removed when DataGrid is available in JSF (maybe!)
 */
public final class TableRowEvent extends WrapperEvent
{
  public TableRowEvent(
    UIComponent source,
    FacesEvent  event,
    Object currencyKey)
  {
    super(source, event);

    _currencyKey = currencyKey;
  }

  public Object getCurrencyKey()
  {
    return _currencyKey;
  }

  private final Object _currencyKey;
  private static final long serialVersionUID = 1L;
}
