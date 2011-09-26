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
package org.apache.myfaces.trinidadinternal.uinode;

import java.io.IOException;

import java.util.Iterator;

import javax.faces.component.UIComponent;

import java.lang.reflect.UndeclaredThrowableException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.RendererFactory;
import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.collection.AttributeMap;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.PreAndPostRenderer;

import org.apache.myfaces.trinidad.component.UIXComponent;

/**
 * Subclass for UIX components.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/UIXComponentUINode.java#0 $) $Date: 10-nov-2005.18:49:20 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class UIXComponentUINode extends UIComponentUINode
{
  public UIXComponentUINode(
   UIXComponent     component,
   String           namespace,
   AttributeMap     attributes)
  {
    super(component, namespace);

    _attributes = attributes;
  }

  /**
   * Returns the role that this node occupies.
   */
  @Override
  public NodeRole getNodeRole(UIXRenderingContext context)
  {
    if (context != null)
    {
      Renderer renderer = getRenderer(context, this);
      if (renderer instanceof RoledRenderer)
      {
        return ((RoledRenderer) renderer).getNodeRole(context, this);
      }
      // Null Renderer - for our components - means components
      // like switcher.
      else if (renderer == null)
      {
        return UIConstants.STATE_ROLE;
      }
    }

    return UIConstants.UNKNOWN_ROLE;
  }

  @Override
  public Iterator<AttributeKey> getAttributeNames(UIXRenderingContext context)
  {
    AttributeMap attributes = getAttributeMap(false);

    if (attributes != null)
    {
      return attributes.attributeKeys(context);
    }
    else
    {
      return null;
    }
  }

  @Override
  public void setAttributeValue(
    AttributeKey attrKey,
    Object       value
    )
  {
    AttributeMap attrDict = getAttributeMap((value != null));

    if (attrDict != null)
    {
      attrDict.setAttribute(attrKey, value);
    }
  }



  /**
   * Returns the value of the attribute with the specified name in the
   * RenderingContext.  If no attribute with the specified name exists
   * in this UINode, or a checked Exception occurs in retrieving the value of
   * the attribute, <CODE>null</CODE> will be returned.
   * <p>
   * Note that as with indexed children and named children, the presence of
   * of an attribute is no guarantee that the Renderer used to render this
   * UINode will actually use the attribute.  The presence of attributes
   * should only be considered as hints to the Renderer.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getAttributeNames
   */
  @Override
  public Object getAttributeValue(UIXRenderingContext context, AttributeKey attrKey)
  {
    return getAttributeValueImpl(context, attrKey, true);
  }


  /**
   * Returns the value of the attribute with a specified name, without
   * attempting to further resolve that value - as if , for instance,
   * it might be a BoundValue.
   * <p>
   * @see org.apache.myfaces.trinidadinternal.ui.data.BoundValue
   */
  @Override
  public Object getRawAttributeValue(UIXRenderingContext context, AttributeKey attrKey)
  {
    return getAttributeValueImpl(context, attrKey, false);
  }


  public void renderInternal(UIXRenderingContext context, UINode dataNode)
    throws IOException
  {
    Renderer renderer = null;

    try
    {
      renderer = getRenderer(context, dataNode);
    }
    catch( UndeclaredThrowableException e )
    {
      if (_LOG.isWarning())
        _LOG.warning(e.getMessage());
      return;
    }


    if (renderer != null)
    {
      // See if we need to push/pop ourselves.  This should only
      // happen in two cases:
      //   - We're the root of the tree.
      //   - We're a private bean, and a Renderer called render() directly
      //     instead of using composite widgets.
      boolean pushAndPop = (context.getRenderedAncestorNode(0) != dataNode);
      if (pushAndPop)
      {
        context.pushChild(dataNode, null, -1);
        context.pushRenderedChild(context, dataNode);
      }

      try
      {
        renderer.render(context, dataNode);
      }
      // Annotate all exceptions thrown by UIX to indicate
      // which component is the guilty one
      catch (RuntimeException re)
      {
        _handleRenderException(re);
      }
      finally
      {
        if (pushAndPop)
        {
          context.popRenderedChild(context);
          context.popChild();
        }
      }
    }
    else
    {
      _logNoRenderer(context);
    }
  }



  public void prerenderInternal(UIXRenderingContext context, UINode dataNode)
    throws IOException
  {
    Renderer renderer = null;

    try
    {
      renderer = getRenderer(context, dataNode);
    }
    catch( UndeclaredThrowableException e )
    {
      if (_LOG.isWarning())
        _LOG.warning(e.getMessage());
      return;
    }

    assert(renderer instanceof PreAndPostRenderer);

    // =-=AEW PUSH-POP-CATCH???
    if (renderer != null)
    {
      // See if we need to push/pop ourselves.  This should only
      // happen in two cases:
      //   - We're the root of the tree.
      //   - We're a private bean, and a Renderer called render() directly
      //     instead of using composite widgets.
      boolean pushAndPop = (context.getRenderedAncestorNode(0) != dataNode);
      if (pushAndPop)
      {
        context.pushChild(dataNode, null, -1);
        context.pushRenderedChild(context, dataNode);
      }

      try
      {
        ((PreAndPostRenderer) renderer).prerender(context, dataNode);
      }
      // Annotate all exceptions thrown by UIX to indicate
      // which component is the guilty one
      catch (RuntimeException re)
      {
        _handleRenderException(re);
      }
      finally
      {
        if (pushAndPop)
        {
          context.popRenderedChild(context);
          context.popChild();
        }
      }
    }
    else
    {
      _logNoRenderer(context);
    }
  }




  public void postrenderInternal(UIXRenderingContext context, UINode dataNode)
    throws IOException
  {
    Renderer renderer = null;

    try
    {
      renderer = getRenderer(context, dataNode);
    }
    catch( UndeclaredThrowableException e )
    {
      if (_LOG.isWarning())
        _LOG.warning(e.getMessage());
      return;
    }

    assert(renderer instanceof PreAndPostRenderer);

    // =-=AEW PUSH-POP-CATCH???
    if (renderer != null)
    {
      // See if we need to push/pop ourselves.  This should only
      // happen in two cases:
      //   - We're the root of the tree.
      //   - We're a private bean, and a Renderer called render() directly
      //     instead of using composite widgets.
      boolean pushAndPop = (context.getRenderedAncestorNode(0) != dataNode);
      if (pushAndPop)
      {
        context.pushChild(dataNode, null, -1);
        context.pushRenderedChild(context, dataNode);
      }

      try
      {
        ((PreAndPostRenderer) renderer).postrender(context, dataNode);
      }
      // Annotate all exceptions thrown by UIX to indicate
      // which component is the guilty one
      catch (RuntimeException re)
      {
        _handleRenderException(re);
      }
      finally
      {
        if (pushAndPop)
        {
          context.popRenderedChild(context);
          context.popChild();
        }
      }
    }
    else
    {
      _logNoRenderer(context);
    }
  }

  private void _handleRenderException(
    RuntimeException re) throws IOException
  {
    if (re instanceof UndeclaredThrowableException)
    {
      // Our UnsynchronizedPrintWriter catches IOExceptions and
      // rethrows these wrapped in UndeclaredThrowableExceptions.  If we
      // catch any UndeclaredThrowableExceptions which have an IOExceptions
      // as the root cause, let's just rethrow the original
      // IOException so that the original stack trace will be
      // preserved.
      Throwable rootCause = ((UndeclaredThrowableException)re).getCause();
      if (rootCause instanceof IOException)
        throw ((IOException)rootCause);
    }

    throw re;
  }

  private void _logNoRenderer(UIXRenderingContext context)
  {
    if (_LOG.isWarning())
    {
      RendererManager manager = context.getRendererManager();
      RendererFactory factory = manager.getFactory(getNamespaceURI());

      if (factory == null)
      {
        _LOG.warning("NO_RENDERERFACTORY_REGISTERED_COMPONENT", getNamespaceURI());
      }
      else
      {
       _LOG.warning("NO_RENDERER_REGISTERED", this);
      }
    }
  }

  protected Renderer getRenderer(
    UIXRenderingContext context,
    UINode           dataNode
    )
  {
    // get the renderer for ourselves
    RendererManager manager = context.getRendererManager();
    String localName = getLocalName();
    if (localName == null)
      return null;

    return manager.getRenderer(getNamespaceURI(), localName);
  }


  /**
   * Bottleneck method for all attribute getting.
   */
  protected Object getAttributeValueImpl(
    UIXRenderingContext context,
    AttributeKey     attrKey,
    boolean          returnBoundValue
    )
  {
    AttributeMap attributes = getAttributeMap(false);

    if (attributes != null)
    {
      Object value = attributes.getAttribute(context, attrKey);

      if (returnBoundValue && (value instanceof BoundValue))
      {
        value = ((BoundValue)value).getValue(context);
      }

      return value;
    }
    else
    {
      return null;
    }
  }

  /**
   * Returns the AttributeMap used to store attributes.
   * <p>
   * @param createIfNull if true,  creates
   *   an AttributeMap object if one has not yet been created.
   */
  protected final AttributeMap getAttributeMap(
    boolean createIfNull
    )
  {
    if (createIfNull && (_attributes == null))
    {
      // This should not happen;  we set the attributes in the
      // construtor
      throw new IllegalStateException();
    }

    return _attributes;
  }

  static UIXComponentUINode __getAdapter(UIComponent component)
  {
    // =-=AEW How to assert this is only called when allowable?
    return (UIXComponentUINode) __getUINode(component);
  }


  private AttributeMap _attributes;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UIXComponentUINode.class);
}
