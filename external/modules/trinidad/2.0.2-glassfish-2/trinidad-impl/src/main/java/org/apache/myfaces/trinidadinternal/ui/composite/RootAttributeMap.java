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

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.collection.UINodeAttributeMap;

/**
 * AttributeMap that delegates to the ParentContext's current UINode.  This
 * class is typically used inside of composite widgets to delegate attributes
 * to the root of the composite.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/RootAttributeMap.java#0 $) $Date: 10-nov-2005.18:56:53 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RootAttributeMap extends UINodeAttributeMap
{
  public static RootAttributeMap getAttributeMap()
  {
    return _INSTANCE;
  }
  
  protected RootAttributeMap()
  {
  }
  
  @Override
  protected UINode getUINode(
    UIXRenderingContext context
    )
  {
    if (context != null)
    {
      UIXRenderingContext parentContext = context.getParentContext();
      
      if (parentContext != null)
      {
        return parentContext.getAncestorNode(0);
      }
    }

    return null;
  }
  
  @Override
  protected UIXRenderingContext getRenderingContext(UIXRenderingContext context)
  {
    if (context == null)
      return null;

    return context.getParentContext();
  }

  private static final RootAttributeMap _INSTANCE = new RootAttributeMap();
}
