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
package org.apache.myfaces.trinidadinternal.ui.composite;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * <p>
 * This BoundValue is typically used to implement UINodeRenderers.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/PoppedAttributeBoundValue.java#0 $) $Date: 10-nov-2005.18:56:52 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PoppedAttributeBoundValue implements BoundValue
{
  public PoppedAttributeBoundValue(
    BoundValue   targetNodeValue,
    AttributeKey attrKey
    )
  {
    if (targetNodeValue == null)
      throw new IllegalArgumentException();

    if (attrKey == null)
      throw new IllegalArgumentException();
      
    _target  = targetNodeValue;
    _attrKey = attrKey;
  }
  
  
  /**
   * Called to retrieve a value based on the current rendering
   * context.
   * @param context the rendering context
   */
  public Object getValue(
    UIXRenderingContext context
    )
  {
    UIXRenderingContext parentContext = context.getParentContext();
    
    if (parentContext != null)
    {
      // Get the target value - using the ORIGINAL context,
      // not the parent context.
      Object targetValue = _target.getValue(context);

      if (targetValue != null)
      {
        return ((UINode)targetValue).getAttributeValue(parentContext, _attrKey);
      }
    }
    
    return null;
  }
      
  private BoundValue   _target;
  private AttributeKey _attrKey;
}
