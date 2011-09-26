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
package org.apache.myfaces.trinidadinternal.ui.collection;

import java.util.Iterator;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * Map-like object for storing context-based information.  Since the
 * context is passed in to the <code>get</code> and <code>keys</code>
 * methods, ContextMaps that delegate all or some of their properties
 * through the RenderingContext.  The MutableUINode class uses
 * a ContextMap to store its named children, for this very reason, as it
 * enables developers to data bind the named children nodes.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/ContextMap.java#0 $) $Date: 10-nov-2005.18:57:32 $
 * <p>
 * @see org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode#setNamedChildMap
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface ContextMap
{
  /**
   * Returns the Object stored under the key, returning
   * <code>null</code> if no Object with that key exists.
   * <p>
   * @param context RenderingContext to use to fulfill this request
   * @param key key used to retrieve the value
   * @return The value stored under this key, or <code>null</code> if
   *         no value is found.
   * @throws IllegalArgumentException if the key is null
   */
  public Object get(UIXRenderingContext context, Object key);

  /**
   * Stores the <code>value</code> in the map, under the <code>key</code>.
   * <p>
   * As the RenderingContext is not passed to this function, sets do not
   * have the same databinding capabilities that gets do.
   * <p>
   * @param key key used to set the value
   * @param value The new value to set
   * @throws IllegalArgumentException if either the <code>key</code> or
   *         <code>value</code> is null.
   */
  public void set(String key, Object value);

  /**
   * Returns an Iterator of all of the keys in the ContextMap given
   * the specified context.
   * <p>
   * The entire contents of the ContextMap can
   * be retrieved by calling <code>ContextMap.get</code> for each of the keys
   * in the enumeration.
   * @param UIXRenderingContext context Context used to determine the set of
   *                                 keys.
   * @return The Iterator of keys in the RenderingContext
   */
  public Iterator<String> keys(UIXRenderingContext context);
}
