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
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * AttributeMap that treats a UINode as an attribute map.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/UINodeAttributeMap.java#0 $) $Date: 10-nov-2005.18:57:35 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class UINodeAttributeMap implements AttributeMap
{
  protected abstract UINode getUINode(UIXRenderingContext context);

  protected UIXRenderingContext getRenderingContext(UIXRenderingContext context)
  {
    return context;
  }

  public Object getAttribute(
    UIXRenderingContext context,
    AttributeKey     key
    )
  {
    UINode node = getUINode(context);

    context = getRenderingContext(context);

    if (node != null)
    {
      return node.getAttributeValue(context, key);
    }
    else
    {
      return null;
    }
  }


  public void setAttribute(
    AttributeKey key,
    Object       value
    )
  {
    UINode node = getUINode(null);

    if (node instanceof MutableUINode)
    {
      ((MutableUINode)node).setAttributeValue(key, value);
    }
  }

  public Iterator<AttributeKey> attributeKeys(
    UIXRenderingContext context
    )
  {
    UINode node = getUINode(context);

    if (node != null)
    {
      context = getRenderingContext(context);
      return node.getAttributeNames(context);
    }
    else
    {
      return null;
    }
  }
}
