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

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;


/**
 * Map-like object for storing values under AttributeKeys and retrieving
 * the values, given a RenderingContext.
 * <p>
 * Unlike UINodes, AttributeMaps do not retrieve the value of BoundValues
 * stored inside AttributeMaps.  Thus, if a BoundValue is stored in an
 * AttributeMap, calling <code>AttributeMap.getAttribute</code> will
 * return the BoundValue instance, not <code>instance.getValue(context)</code>.
 * <p>
 * As the RenderingContext is passed to both the <code>getAttribtue</code>
 * and </code>keys</code> methods, the contents of the AttibuteMap can be
 * proxied through the RenderingContext to create AttributeMaps who's
 * contents are determined based on the current context.
 * <p>
 * The BaseMutableUINode interface allows the storage of attributes in the
 * UINode to be set by a call to <code>setAttributeMap</code>.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/AttributeMap.java#0 $) $Date: 10-nov-2005.18:57:31 $
 * @see AttributeKey
 * @see org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode#setAttributeMap
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface AttributeMap
{
  /**
   * Returns the value in the AttributeMap specified by the key.
   */
  public Object getAttribute(UIXRenderingContext context, AttributeKey key);

  /**
   * Sets an attribute value specified by the key to the new value.
   */
  public void setAttribute(AttributeKey key, Object value);


  /**
   * Returns an Iterator of all of the AttributeKeys in the AttributeMap
   * that values exist for.
   */
  public Iterator<AttributeKey> attributeKeys(UIXRenderingContext context);
}
