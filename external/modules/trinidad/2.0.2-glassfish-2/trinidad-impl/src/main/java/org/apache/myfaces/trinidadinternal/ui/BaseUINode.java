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
package org.apache.myfaces.trinidadinternal.ui;

import java.io.IOException;

import java.util.Iterator;

import javax.faces.component.UIComponent;

import java.lang.reflect.UndeclaredThrowableException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.ui.collection.AttributeMap;
import org.apache.myfaces.trinidadinternal.ui.collection.ContextMap;
import org.apache.myfaces.trinidadinternal.ui.collection.UINodeList;

import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * Root implementation for storing a node of a UIX Components UI tree.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/BaseUINode.java#1 $) $Date: 11-nov-2005.14:59:41 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BaseUINode implements UINode, UIConstants
{
  /**
   * Creates a BaseUINode, binding it to a namespace
   * and local name.
   */
  public BaseUINode(
    String namespaceURI,
    String localName
    )
  {
    _namespaceURI = namespaceURI;
    _localName    = localName;
  }

  public UIComponent getUIComponent()
  {
    return null;
  }

  /**
   * The namespace URI that segregates the local name of this UINode so
   * that nodes with the same local name will not clash.  Together, the
   * namespace URI and the local identify the kind of UINode that this is.
   * <p>
   * Any namespace String returned by this method should be interned for
   * maximum performance.  This allows namespace comparisons to be
   * performed using object identity, versus the slower object equality.
   * If the String returned is a String constant, the Java VM will have done
   * this for you automatically.
   * <p>
   * As with all namespace URI's, this name is only used as an identifier.  No
   * other sematics are implied.
   * <p>
   * The <CODE>RendererManager</CODE> allows <CODE>RendererFactory</CODE>s to
   * be registered by namespace.
   * <p>
   * @see #getLocalName
   * @see org.apache.myfaces.trinidadinternal.ui.RendererManager
   * @see org.apache.myfaces.trinidadinternal.ui.RendererFactory
   */
  public String getNamespaceURI()
  {
    return _namespaceURI;
  }


  /**
   * Name used to distinguish the name of a UINode within a namespace.
   * <p>
   * Together with the UINode's namespace URI, the local name is typically
   * used to determine which Renderer to use to render the UINode.
   */
  public String getLocalName()
  {
    return _localName;
  }



  /**
   * Returns the number of indexed children in this UINode.
   */
  public int getIndexedChildCount(
    UIXRenderingContext context
    )
  {
    UINodeList indexedChildren = getIndexedNodeList(false);

    if (indexedChildren != null)
    {
      return indexedChildren.size(context);
    }
    else
    {
      return 0;
    }
  }


  /**
   * Returns the indexed child at the specified index.  Indexed children are
   * used to represent homogenously treated children of UINodes.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getIndexedChildCount
   */
  public UINode getIndexedChild(
    UIXRenderingContext context,
    int              childIndex
    )
  {
    UINodeList indexedChildren = getIndexedNodeList(false);

    if (indexedChildren != null)
    {
      return indexedChildren.getUINode(context, childIndex);
    }
    else
    {
      throw new IndexOutOfBoundsException();
    }
  }


  /**
   * Returns the child identified by <b>childName</b>.  Named children are
   * used to represent heterogenously treated children of UINodes.
   * <p>
   * Constants for named children used by UIX Components UINodes may be found in
   * the UIConstants interface.  The constants for the named children follow
   * the pattern <CODE>&lt;xxx&gt;_CHILD</CODE>.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getChildNames
   * @see org.apache.myfaces.trinidadinternal.ui.UIConstants
   */
  public UINode getNamedChild(
    UIXRenderingContext context,
    String           childName
    )
  {
    ContextMap childMap = getNamedChildMap(false);

    if (childMap != null)
    {
      return (UINode)childMap.get(context, childName);
    }
    else
    {
      return null;
    }
  }


  /**
   * Returns an Iterator of the names that named children have been
   * added under.  The actual UINodes for these named children may be retrieved
   * by passing the Strings returned by this Iterator to
   * <CODE>getNamedChild</CODE>.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getNamedChild
   */
  public Iterator<String> getChildNames(
    UIXRenderingContext context
    )
  {
    ContextMap childMap = getNamedChildMap(false);

    if (childMap != null)
    {
      return childMap.keys(context);
    }
    else
    {
      return null;
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
  public final Object getAttributeValue(
    UIXRenderingContext context,
    AttributeKey     attrKey
    )
  {
    return getAttributeValueImpl(context, attrKey, true);
  }


  public final Object getRawAttributeValue(
    UIXRenderingContext context,
    AttributeKey     attrKey
    )
  {
    return getAttributeValueImpl(context, attrKey, false);
  }


  /**
   * Returns an Iterator of the names that attribute values have been
   * added under.  The actual values for these attributes may be retrieved
   * by passing the AttributeKeys returned by this Iterator to
   * <CODE>getAttributeValue</CODE>.
   * <p>
   * <STRONG>If the UINode is mutable and may be modified and read in different
   * threads, it is the programmer's responsibility to ensure proper
   * synchronization.
   * </STRONG>
   * <p>
   * @see #getAttributeValue
   */
  public Iterator<AttributeKey> getAttributeNames(
    UIXRenderingContext context
    )
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


  /**
   * Returns the role that this node occupies.
   */
  public NodeRole getNodeRole(UIXRenderingContext context)
  {
    if (context != null)
    {
      UINode renderedNode = getRenderedUINode(context);
      Renderer renderer = getRenderer(context, renderedNode);
      if (renderer instanceof RoledRenderer)
      {
        return ((RoledRenderer) renderer).getNodeRole(context, renderedNode);
      }
    }

    return UNKNOWN_ROLE;
  }


  /**
   * Renders this UINode.  Clients can implements this
   * method in any way desired.  All UIX Components-based implementations
   * will get a RendererManager from the RenderingContext,
   * get a Renderer, and defer rendering to that Renderer.
   */
  public final void render(
    UIXRenderingContext context
    )
    throws IOException
  {
    render(context, getRenderedUINode(context));
  }

  /**
   * Renders this UINode.  Clients can implements this
   * method in any way desired.  All UIX Components based implementations
   * will get a RendererManager from the RenderingContext,
   * get a Renderer, and defer rendering to that Renderer.
   */
  public void render(
    UIXRenderingContext context,
    UINode           dataNode
    )
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
        _LOG.warning(e.getMessage(), e);
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
      catch (RuntimeException re)
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

  }

  protected final Renderer getRenderer(
    UIXRenderingContext context
    )
  {
    throw new IllegalStateException(_LOG.getMessage(
      "METHOD_CHANGED_TO_GETRENDERER"));
  }


  /**
   * Returns the Renderer used to render this UINode.  Although UINodes
   * typically delegate the binding of the Renderer to the RenderManager
   * on the RenderingContext, UINodes are free to implement
   * <CODE>getRenderer</CODE> to return any Renderer it wishes.  An example
   * of this is <CODE>TextNode</CODE>, which always returns
   * <CODE>TextRenderer</CODE> as its renderer.
   */
  protected Renderer getRenderer(
    UIXRenderingContext context,
    UINode           dataNode
    )
  {
    // get the renderer for ourselves
    return context.getRendererManager().getRenderer(getNamespaceURI(), getLocalName());
  }

  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer(40);
    String className = getClass().getName();
    int periodIndex = className.lastIndexOf('.');
    if (periodIndex >= 0)
      className = className.substring(periodIndex + 1);
    buffer.append(className);

    buffer.append(", localName='");
    buffer.append(getLocalName());
    buffer.append("'");

    Object annotation = getAttributeValue(null,
                                          UIConstants.ANNOTATION_ATTR);
    if (annotation != null)
    {
      buffer.append('[');
      buffer.append(annotation.toString());
      buffer.append(']');
    }

    return buffer.toString();
  }


  /**
   * Returns the child array used to retrieve indexed children from
   * the node.  By default, returns null.
   * <code>getIndexedChild()</code> and
   * <code>getIndexedChildCount()</code> will use the result of this
   * method - if not implemented, subclasses must override both
   * of those methods.  Subclasses should not copy the array - so
   * subclasses must be extremely careful when modifying this array
   * or returning to "untrusted" classes.
   * <p>
   * @see #getIndexedChild
   * @see #getIndexedChildCount
   */
  protected final UINode[] getChildArray()
  {
    // =-= bts FIX ME remove since replaced by getIndexedNodeList()
    throw new IllegalStateException(_LOG.getMessage(
      "REPLACED_BY_GETINDEXEDNODELIST"));
  }


  /**
   * Returns the dictionary used to store named children.  This
   * class just returns null.
   * <p>
   * @param createIfNull if true, the subclass should create
   *   a NamedChildMap object if one has not yet been created.
   */
  protected ContextMap getNamedChildMap(
    boolean createIfNull
    )
  {
    return null;
  }


  /**
   * Returns the AttributeMap used to store attributes.  This
   * class just returns null.
   * <p>
   * @param createIfNull if true, the subclass must create
   * a Dictionary object if one has not yet been created.
   */
  protected AttributeMap getAttributeMap(
    boolean createIfNull
    )
  {
    return null;
  }


  /**
   * Returns the UINodeList used to retrieve indexed children from
   * the node.  By default, returns null.
   * <code>getIndexedChild()</code> and
   * <code>getIndexedChildCount()</code> will use the result of this
   * method - if not implemented, subclasses must override both
   * of those methods.  Subclasses should not clone the UINodeList - so
   * subclasses must be extremely careful when modifying this list
   * or returning to "untrusted" classes.
   * <p>
   * @see #getIndexedChild
   * @see #getIndexedChildCount
   */
  protected UINodeList getIndexedNodeList(
    boolean createIfNull
    )
  {
    return null;
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
   * Returns the UINode to render and to use to dtermine the renderer.
   * Subclasses can override this method to return a different rendered UINode
   * hierarchy than that apparent from the client's view of the UINode
   * hierarchy, enabling composite UINodes to be created.
   */
  protected UINode getRenderedUINode(
    UIXRenderingContext context
    )
  {
    return this;
  }


  private String _namespaceURI;
  private String _localName;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BaseUINode.class);
}
