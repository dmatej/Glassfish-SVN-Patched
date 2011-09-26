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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.BaseRenderer;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/SwitcherRenderer.java#0 $) $Date: 10-nov-2005.18:53:07 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SwitcherRenderer extends BaseRenderer
  implements UIConstants, RoledRenderer
{
  public NodeRole getNodeRole(
    UIXRenderingContext context,
    UINode           node)
  {
    return STATE_ROLE;
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object value = node.getAttributeValue(context, CHILD_NAME_ATTR);
    String childName = null;

    if (value != null)
    {
      childName = value.toString();
    }

    // Try to render this case. If not found, try to render a default case.
    if (!_renderCase(context, node, childName))
    {
      Object defObject = node.getAttributeValue(context, DEFAULT_CASE_ATTR);

      // If the user has requested a specific name for the default case, use it
      if (defObject != null)
      {
        _renderCase(context, node, defObject.toString());
      }
    }
  }

  // _renderCase renders a particular case child of a switcher element.
  // Return value is true if the child name pointed to a named case child.
  private boolean _renderCase(
    UIXRenderingContext context,
    UINode           node,
    String           childName
    ) throws IOException
  {
    boolean returnVal = false;

    if (childName != null)
    {
      UINode childNode = node.getNamedChild(context, childName);
      if (childNode != null)
      {
        renderNamedChild(context, node, childNode, childName);
        returnVal = true;
      }
    }
    return returnVal;
  }

}
