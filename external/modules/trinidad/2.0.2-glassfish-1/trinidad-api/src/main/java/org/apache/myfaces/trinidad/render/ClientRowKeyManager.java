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
package org.apache.myfaces.trinidad.render;

import java.io.Serializable;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * This class manages server-side
 * rowkey Objects with client-side string keys.
 * This class must be Serializable as it is state-saved along with the
 * UIComponent state.
 */
public abstract class ClientRowKeyManager implements Serializable
{
  /**
   * Gets a string version of a key that identifies the row with the given rowkey. 
   * This string key can be used on the client-side to identify the row.
   * If a string key for the given rowkey does not exist, then a new one is
   * created. The lifespan of this string rowkey is entirely upto each
   * implementation. Implementors must ensure that if a particular row is still
   * present on the client-side, then its string key must also continue to be valid.
   * @param rowKey the rowkey to convert into a client key. Note that
   * null is special and is not allowed.
   */
  public abstract String getClientRowKey(FacesContext context, UIComponent component, Object rowKey);

  /**
   * Gets the corresponding server-side rowkey object from the given client-side string
   * key. If the string key has expired, implementors should return null. However,
   * if any part of a row is still present on the client-side, its corresponding
   * string-key may not expire.
   * @param clientRowKey the string key
   * @return null, if the string key has expired, or never existed.
   */
  public abstract Object getRowKey(FacesContext context, UIComponent component, String clientRowKey);
  
  
  /**
   * Replaces an old row key with a new key if the old row key exists.  If the old row key is successfully replaced,
   * the new row key will be mapped to the existing client row key.
   * @param context
   * @param component
   * @param oldRowKey row key to replace (may not exist)
   * @param newRowKey new row key
   * @return <code>true</code> if old row key existed and was replaced, <code>false</code> otherwise
   */
  public boolean replaceRowKey(FacesContext context, UIComponent component, Object oldRowKey, Object newRowKey)
  {
    // default implementation to maintain backwards compatibility
    return false;
  }
  
  private static final long serialVersionUID = 1L;
}
