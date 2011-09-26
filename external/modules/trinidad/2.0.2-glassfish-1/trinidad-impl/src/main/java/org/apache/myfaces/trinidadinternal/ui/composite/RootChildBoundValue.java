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

import java.util.Hashtable;

import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;


/**
 * BoudnValue that returns the specified child of the Root node.
 * <p>
 * This BoundValue is typically used to implement UINodeRenderers.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/RootChildBoundValue.java#0 $) $Date: 10-nov-2005.18:56:54 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RootChildBoundValue implements BoundValue
{
  public static RootChildBoundValue getBoundValue(
    String childName
    )
  {
    RootChildBoundValue node = _boundValues.get(childName);

    if (node == null)
    {
      node = new RootChildBoundValue(childName, -1);

      _boundValues.put(childName, node);
    }

    return node;
  }


  public static RootChildBoundValue getBoundValue(
    int childIndex
    )
  {
    Integer key = childIndex;

    RootChildBoundValue node = _boundValues.get(key);

    if (node == null)
    {
      node = new RootChildBoundValue(null, childIndex);

      _boundValues.put(key, node);
    }

    return node;
  }


  private RootChildBoundValue(
    String       childName,
    int          childIndex
    )
  {
    _childName  = childName;
    _childIndex = childIndex;
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
      UINode baseNode = parentContext.getAncestorNode(0);

      if (baseNode != null)
      {
        if (_childName != null)
        {
          return baseNode.getNamedChild(parentContext, _childName);
        }
        else
        {
          return baseNode.getIndexedChild(parentContext, _childIndex);
        }
      }
    }

    return null;
  }

  private static Hashtable<Object, RootChildBoundValue> _boundValues = 
    new Hashtable<Object, RootChildBoundValue>(51);

  private String _childName;
  private int    _childIndex;
}
