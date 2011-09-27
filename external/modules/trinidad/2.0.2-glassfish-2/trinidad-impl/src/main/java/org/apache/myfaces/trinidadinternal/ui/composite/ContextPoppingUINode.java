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

import java.util.concurrent.ConcurrentHashMap;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.BaseUINode;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.data.DataObject;

/**
 * Renderer used by composite UINode renderers to render content.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/ContextPoppingUINode.java#0 $) $Date: 10-nov-2005.18:56:51 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ContextPoppingUINode extends BaseUINode
{
  public static ContextPoppingUINode getUINode(
    String childName
    )
  {
    ContextPoppingUINode node = _sContextPoppers.get(childName);

    if (node == null)
    {
      node = new ContextPoppingUINode(childName, -1);

      _sContextPoppers.put(childName, node);
    }

    return node;
  }


  public static ContextPoppingUINode getUINode(
    int childIndex
    )
  {
    Integer key = childIndex;

    ContextPoppingUINode node = _sContextPoppers.get(key);

    if (node == null)
    {
      node = new ContextPoppingUINode(null, childIndex);

      _sContextPoppers.put(key, node);
    }

    return node;
  }


  private ContextPoppingUINode(
    String childName,
    int    childIndex
    )
  {
    super(MARLIN_NAMESPACE, CONTEXT_POPPING_NAME);

    if ((childName == null) && (childIndex < 0))
      throw new IllegalArgumentException();

    _childName  = childName;
    _childIndex = childIndex;
  }

  @Override
  public int getIndexedChildCount(
    UIXRenderingContext context
    )
  {
    if (getPoppedNode(context) != null)
    {
      return 1;
    }
    else
    {
      return 0;
    }
  }

  @Override
  public UINode getIndexedChild(
    UIXRenderingContext context,
    int              childIndex
    )
  {
    if (childIndex == 0)
    {
      UINode poppedNode = getPoppedNode(context);

      if (poppedNode != null)
      {
        return poppedNode;
      }
    }

    throw new IndexOutOfBoundsException();
  }

  protected UINode getPoppedNode(
    UIXRenderingContext context
    )
  {
    if (context == null)
      return null;

    context = context.getParentContext();
    if (context == null)
      return null;

    UINode rootNode = context.getAncestorNode(0);

    if (_childName != null)
    {
      return rootNode.getNamedChild(context, _childName);
    }
    else
    {
      return rootNode.getIndexedChild(context, _childIndex);
    }
  }

  @Override
  protected Object getAttributeValueImpl(
    UIXRenderingContext context,
    AttributeKey     attrKey,
    boolean          returnBoundValue
    )
  {
    if (attrKey == UIConstants.RENDERED_ATTR)
    {
      UINode poppedNode = getPoppedNode(context);

      if (poppedNode != null)
      {
        // ask the popped node for the rendered attribute in its context
        return poppedNode.getAttributeValue(context.getParentContext(),
                                            attrKey);
      }
      else
      {
        return Boolean.FALSE;
      }
    }
    else
    {
      return null;
    }
  }

  @Override
  protected Renderer getRenderer(
    UIXRenderingContext context,
    UINode           dataNode
    )
  {
    // get the renderer for ourselves
    return _RENDERER;
  }

  String __getChildName()
  {
    return _childName;
  }

  int __getChildIndex()
  {
    return _childIndex;
  }

  @Override
  public String toString()
  {
    String text = super.toString();
    if (_childName != null)
      return text + "[name=" + _childName + "]";
    else
      return text + "[index=" + _childIndex + "]";
  }

  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  private static class ContextPoppingRenderer implements RoledRenderer
  {
    public void render(
      UIXRenderingContext context,
      UINode           node
      )
      throws IOException
    {
      ContextPoppingUINode poppingNode =
                              (ContextPoppingUINode)context.getAncestorNode(0);
      DataObject poppingCurrentDataObject  = context.getCurrentDataObject();

      UIXRenderingContext poppedContext = context.getParentContext();

      UINode poppedNode = poppingNode.getPoppedNode(context);

      // push on the child's path information
      poppedContext.pushChild(poppedNode,
                              poppingNode.__getChildName(),
                              poppingNode.__getChildIndex());
      poppedContext.pushRenderedChild(poppedContext, poppedNode);

      DataObject poppedCurrentDataObject =
        poppedContext.setCurrentDataObject(poppingCurrentDataObject);

      try
      {
        poppedNode.render(poppedContext);
      }
      finally
      {
        poppedContext.setCurrentDataObject(poppedCurrentDataObject);
        poppedContext.popRenderedChild(poppedContext);
        poppedContext.popChild();
      }
    }

    public NodeRole getNodeRole(
      UIXRenderingContext context,
      UINode           node)
    {
      return STATE_ROLE;
    }
  }

  private static final Renderer _RENDERER = new ContextPoppingRenderer();

  private static ConcurrentHashMap<Object, ContextPoppingUINode> _sContextPoppers = 
    new ConcurrentHashMap<Object, ContextPoppingUINode>(203);

  private String _childName;
  private int    _childIndex;
}
