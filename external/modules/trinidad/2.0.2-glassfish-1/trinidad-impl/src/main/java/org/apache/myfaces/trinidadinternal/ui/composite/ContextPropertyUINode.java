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

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.BaseMutableUINode;
import org.apache.myfaces.trinidadinternal.ui.BaseRenderer;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.FixedBoundValue;

/**
 * Node that sets a property on the context.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/ContextPropertyUINode.java#0 $) $Date: 10-nov-2005.18:56:51 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ContextPropertyUINode extends BaseMutableUINode
{
  public ContextPropertyUINode(
   String propertyNamespace,
   Object propertyName,
   Object propertyValue)
  {
    this(propertyNamespace, propertyName, new FixedBoundValue(propertyValue));
  }

  public ContextPropertyUINode(
   String     propertyNamespace,
   Object     propertyName,
   BoundValue propertyValue)
  {
    super(UIConstants.MARLIN_NAMESPACE, "contextProperty");
    if (propertyValue == null)
      throw new NullPointerException();

    _propertyNamespace = propertyNamespace;
    _propertyName      = propertyName;
    _propertyValue     = propertyValue;
  }

  @Override
  public NodeRole getNodeRole(UIXRenderingContext context)
  {
    return UIConstants.STATE_ROLE;
  }

  @Override
  protected Renderer getRenderer(
    UIXRenderingContext context,
    UINode           dataNode
    )
  {
    return _renderer;
  }
  
  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  private class Rndr extends BaseRenderer
  {
    @Override
    public void render(
      UIXRenderingContext context,
      UINode           node
      ) throws IOException
    {
      Object value = _propertyValue.getValue(context);
      Object oldValue = context.getProperty(_propertyNamespace,
                                            _propertyName);

      // Set the property
      context.setProperty(_propertyNamespace,
                          _propertyName,
                          value);

      super.render(context, node);

      // Restore the property
      context.setProperty(_propertyNamespace,
                          _propertyName,
                          oldValue);
    }
  }

  private Renderer _renderer = new Rndr();

  private String _propertyNamespace;
  private Object _propertyName;

  private BoundValue _propertyValue;
}
