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

import javax.faces.FacesException;
import javax.faces.component.ContextCallback;
import javax.faces.context.FacesContext;
import javax.faces.component.NamingContainer;

/**
 * A decorator component that aggregates and decorates collection components like
 * table, treeTable and tree
 * <p>
 * @version $Name:  $ ($Revision: 736165 $) $Date: 2009-01-20 15:18:43 -0800 (Tue, 20 Jan 2009) $
 */
public abstract class UIXDecorateCollectionTemplate extends UIXComponentBase
        implements NamingContainer
{
  /**
   * Gets the currency String for this decorate collection.
   * @return the current established currency
   * @see #setCurrencyString
   */
  public String getCurrencyString()
  {
    return _currencyString;
  }

  /**
   * Sets the currency String for this decorate collection. The decorator renders
   * aggregated components that are not in the component tree. If any of the aggregated
   * component is a naming container (for e.g. menubar), this method allows the currency to
   * be set to that naming container so that it can successfully decode its children.
   *
   * @param currency the currency to be established
   * @see #getCurrencyString
   */
  public void setCurrencyString(String currency)
  {
    _currencyString = currency;
  }

  /**
   * Gets the client-id of this component, without any NamingContainers.
   * This id changes depending on the currency Object.
   * Because this implementation uses currency strings, the local client ID is
   * not stable for very long. Its lifetime is the same as that of a
   * currency string.
   * @see #getCurrencyString
   * @return the local clientId
   */
  @Override
  public final String getContainerClientId(FacesContext context)
  {
    String id = getClientId(context);
    String key = getCurrencyString();
    if (key != null)
    {
      StringBuilder bld = __getSharedStringBuilder();
      bld.append(id).append(NamingContainer.SEPARATOR_CHAR).append(key);
      id = bld.toString();
    }

    return id;
  }



  @Override
  public boolean invokeOnComponent(FacesContext context,
                                   String clientId,
                                   ContextCallback callback)
    throws FacesException
  {
    // optimize case where clientId isn't in NamingContainer
    return invokeOnNamingContainerComponent(context, clientId, callback);
  }


  private String _currencyString = null;
}

