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

import org.apache.myfaces.trinidadinternal.share.util.NamespaceMap;


/**
 * Abstract base class adding the following support to
 * LogicalNodeRenderingContext
   <ul>
     <li>Current DataObject</li>
     <li>Rendered Nodes</li>
     <li>Node Properties</li>
     <li>Local Properties<li>
   </ul>
 * <p>
 * This class is subclassed by RenderingContexts that don't delegate
 * the storage of their rendering state to another RenderingContext.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/RenderedNodeRenderingContext.java#0 $) $Date: 10-nov-2005.18:50:16 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class RenderedNodeRenderingContext extends
                                                   LogicalNodeRenderingContext
{
  public RenderedNodeRenderingContext()
  {
    int defaultStackSize = getDefaultNodeStackSize();

    _renderedNodeStack = new UINode[defaultStackSize];

    _nodePropertyMap   = new StackFrameMap(defaultStackSize);
    _properties = new NamespaceMap(getDefaultPropertyMapSize());
  }



  /**
   * Returns the number of rendered nodes in the path form the current
   * node being rendered to the root of the tree of nodes being rendered.
   */
  public int getRenderedAncestorNodeCount()
  {
    return _renderedStackDepth;
  }


  /**
   * Returns an ancestor of the node currently being processed.  The
   * zero-based index from least to most distant -
   * <code>getRenderedAncestor(0)</code> will always return the current
   * rendering node.  Returns null if the index is greater than or equal to
   * the number of ancestors.
   */
  public UINode getRenderedAncestorNode(int index)
  {
    if (index < 0)
      throw new IllegalArgumentException();

    int stackDepth = _renderedStackDepth;

    // skip the current UINode
    if (index < stackDepth)
    {
      return _renderedNodeStack[stackDepth - index - 1];
    }
    else
    {
      // index higher than number of parents passed in
      return null;
    }
  }

  @Override
  public void pushRenderedChild(
    UIXRenderingContext currentContext,
    UINode renderedChild
    )
  {
    int renderedStackDepth = _renderedStackDepth;
    _renderedStackDepth++;

    // if the stack is full, grow the stack
    if (renderedStackDepth >= _renderedNodeStack.length)
    {
      UINode[] newStack = new UINode[_renderedNodeStack.length * 2];

      System.arraycopy(_renderedNodeStack,
                       0,
                       newStack,
                       0,
                       _renderedNodeStack.length);

      _renderedNodeStack = newStack;
    }

    _renderedNodeStack[renderedStackDepth] = renderedChild;

    // push a stack frame for the rendered child
    _nodePropertyMap.pushFrame();

    // handle initialization of dataobjects
    super.pushRenderedChild(currentContext, renderedChild);
  }

  @Override
  public void popRenderedChild(UIXRenderingContext currentContext)
  {
    // handle cleaning up any of the dataproviders
    super.popRenderedChild(currentContext);

    int renderedStackDepth = _renderedStackDepth - 1;
    _renderedStackDepth = renderedStackDepth;

    assert (renderedStackDepth >= 0);

    // Let GC do its work
    _renderedNodeStack[renderedStackDepth] = null;

    // pop the stack frame of the rendered child
    _nodePropertyMap.popFrame();
  }


  /**
   * Gets a property stored on the context.
   */
  public Object getProperty(
    String namespace,
    Object key
    )
  {
    if (!UIConstants.MARLIN_NAMESPACE.equals(namespace))
    {
      return _properties.get(namespace, key);
    }

    return getRenderingProperty(key);
  }


  /**
   * Stores a property on the context.  Since RendererContexts
   * are not persistent, state stored on a RendererContext will
   * not be present in subsequent rendering passes.
   */
  public void setProperty(
    String namespace,
    Object key,
    Object value
    )
  {
    if (!UIConstants.MARLIN_NAMESPACE.equals(namespace))
    {
      _properties.put(namespace, key, value);
    }

    setRenderingProperty(key, value);
  }

  abstract protected Object getRenderingProperty(Object key);
  abstract protected void setRenderingProperty(Object key, Object value);

  /**
   * Sets a property on the stack frame of the currently rendering UINode.
   * <p>
   * This method is for use by rendering code that needs to save some
   * state while it is rendering.
   * <p>
   * @param key Key used to identify this property on the stack frame.
   * @param value Value to store.  <code>null</code> is an acceptable value.
   * <p>
   * @see #getLocalProperty
   */
  public void setLocalProperty(
    Object key,
    Object value
    )
  {
    _nodePropertyMap.set(key, value);
  }


  /**
   * Retrieves the specifed property from the stack frame of the currently
   * rendering UINode.  If the property is not present,
   * <code>defaultValue</code> is returned.
   * <p>
   * This method is for use by rendering code that needs to save some
   * state while it is rendering.
   * <p>
   * @param key Key used to identify the property on the stack frame to
   *            retrieve.
   * @param defaultValue Value to return if the property doesn't exist
   *                     in the stack frame.
   * <p>
   * @see #setLocalProperty
   */
  public Object getLocalProperty(
    int    ancestorIndex,
    Object key,
    Object defaultValue
    )
  {
    return _nodePropertyMap.get(ancestorIndex, key, defaultValue);
  }

  @Override
  public Object clone()
  {
    RenderedNodeRenderingContext context = (RenderedNodeRenderingContext)
                                           super.clone();

    // clone the global properties
    context._properties = (NamespaceMap)_properties.clone();

    // clone the local properties
    context._nodePropertyMap = (StackFrameMap)_nodePropertyMap.clone();

    //
    // clone the rendered node stack
    //
    UINode[] renderedNodeStack = new UINode[_renderedStackDepth];

    System.arraycopy(_renderedNodeStack,
                     0,
                     renderedNodeStack,
                     0,
                     _renderedStackDepth);

    context._renderedNodeStack = renderedNodeStack;

    return context;
  }


  /**
   * Returns the default initial number of nodes in the stack of
   * logical nodes.
   * <p>
   * The default for this property is relatively small.
   */
  @Override
  protected int getDefaultNodeStackSize()
  {
    return _DEFAULT_STACK_SIZE;
  }


  /**
   * Returns the default size of the PropertyMap used to store
   * properties on the RenderingContext.
   * <p>
   * The default for this property is relatively small.
   */
  protected int getDefaultPropertyMapSize()
  {
    return _DEFAULT_PROPERTY_MAP_SIZE;
  }


  /**
   * Reset the PortletContext after use so that it can be resued without
   * pinning unnecessary objects into memory
   */
  @Override
  protected void reset()
  {
    // reset the superclass
    super.reset();

    // make sure that the _nodePropertyMap is clear
    _nodePropertyMap.clear();

    //
    // make sure that the renderedStack is reset
    //
    for (int i = 0; i < _renderedStackDepth; i++)
    {
      _renderedNodeStack[i] = null;
    }

    _properties.clear();
  }


  /**
   * Lame method needed to support resetProperties on RootRenderingContext
   */
  protected void resetProperties()
  {
    throw new UnsupportedOperationException();
  }


  /**
   * Class implementing the stack frame properties for Renderers
   */
  private static class StackFrameMap implements Cloneable
  {
    public StackFrameMap()
    {
      this(_INITIAL_CAPACITY);
    }

    public StackFrameMap(
      int initialCapacity
      )
    {
      if (initialCapacity < 0)
        throw new IllegalArgumentException();

      if (initialCapacity != 0)
      {
        // expect an average of 4 properties per stack frame
        _keyValues = new Object[initialCapacity * 4];
      }
      else
      {
        _keyValues = _EMPTY_ARRAY;
      }

      _frameStarts = new int[initialCapacity];
    }


    public void pushFrame()
    {
      _frameIndex++;

      // grow the array if necessary
      if (_frameStarts.length <= _frameIndex)
      {
        // double old size
        int[] newStarts = new int[_frameStarts.length * 2];

        System.arraycopy(_frameStarts,
                         0,
                         newStarts,
                         0,
                         _frameStarts.length);

        _frameStarts = newStarts;
      }

      _frameStarts[_frameIndex] = _nextKeyIndex;
    }


    public void popFrame()
    {
      //
      // clear out the old stack frame for GC
      //
      int startIndex = _getFrameKeyStartIndex();

      if (startIndex >= 0)
      {
        int endIndex = _nextKeyIndex;

        for (int i = startIndex; i < endIndex; i++)
        {
          _keyValues[i] = null;
        }
      }

      // start assigning new key values where this frame started
      _nextKeyIndex = startIndex;

      _frameIndex--;

      if (_frameIndex < -1)
      {
        throw new IllegalStateException();
      }
    }

    public Object get(
      int    index,
      Object key,
      Object defaultValue
      )
    {
      int startIndex = _getFrameKeyStartIndex(index);

      if (startIndex >= 0)
      {
        int endIndex = _getFrameKeyEndIndex(index);

        for (int i = startIndex; i < endIndex; i += 2)
        {
          // compare by identity for speed
          if (key == _keyValues[i])
          {
            return _keyValues[i + 1];
          }
        }
      }

      return defaultValue;
    }

    public void set(
      Object key,
      Object value
      )
    {
      if (key == null)
        throw new IllegalArgumentException();

      int startIndex = _getFrameKeyStartIndex();

      int endIndex = _nextKeyIndex;

      //
      // Search if key already exists
      //
      if (startIndex >= 0)
      {
        for (int i = startIndex; i < endIndex; i += 2)
        {
          // compare by identity for speed
          if (key == _keyValues[i])
          {
            _keyValues[i + 1] = value;
            return;
          }
        }
      }

      //
      // append key, value pair
      //
      int currSize = _keyValues.length;

      // grow the array if necessary
      if (currSize < endIndex + 2)
      {
        int newSize = (currSize <= 0)
                        ? _MIN_STACK_SIZE * 2
                        : currSize * 2;

        // double old size
        Object[] newKeyValues = new Object[newSize];

        System.arraycopy(_keyValues, 0, newKeyValues, 0, currSize);

        _keyValues = newKeyValues;
      }

      _keyValues[endIndex]     = key;
      _keyValues[endIndex + 1] = value;

      _nextKeyIndex += 2;
    }

    /**
     * Clears the stack frame map
     */
    public void clear()
    {
      //
      // clear out any remaining keyvalues
      //
      if (_frameIndex >= 0)
      {
        int endIndex = _nextKeyIndex;

        for (int i = 0; i < endIndex; i++)
        {
          _keyValues[i] = null;
        }
      }

      _nextKeyIndex = 0;
      _frameIndex   = -1;
    }

    @Override
    public Object clone()
    {
      StackFrameMap stack;

      try
      {
        stack = (StackFrameMap)super.clone();
      }
      catch (CloneNotSupportedException e)
      {
        // this should never happen
        throw new IllegalStateException();
      }

      //
      // clone the key/value pairs
      //
      Object[] keyValues = new Object[_nextKeyIndex];

      System.arraycopy(_keyValues, 0, keyValues, 0, _nextKeyIndex);

      stack._keyValues = keyValues;

      //
      // clone the starting indices of the stack frames
      //
      int frameCount = _frameIndex + 1;

      int[] frameStarts = new int[frameCount];

      System.arraycopy(_frameStarts, 0, frameStarts, 0, frameCount);

      stack._frameStarts = frameStarts;

      return stack;
    }

    private int _getFrameKeyStartIndex()
    {
      return _getFrameKeyStartIndex(0);
    }

    private int _getFrameKeyStartIndex(int ancestorCount)
    {
      if (_frameIndex >= ancestorCount)
      {
        return _frameStarts[_frameIndex - ancestorCount];
      }
      else
      {
        return -1;
      }
    }

    private int _getFrameKeyEndIndex(int ancestorCount)
    {
      if (ancestorCount == 0)
        return _nextKeyIndex;
      else
        return _getFrameKeyStartIndex(ancestorCount - 1);
    }


    private static final int _INITIAL_CAPACITY = 20;
    static private final int _MIN_STACK_SIZE   = 4;

    private static final Object[] _EMPTY_ARRAY = new Object[0];

    private Object[] _keyValues;
    private int[]    _frameStarts;
    private int      _nextKeyIndex = 0;
    private int      _frameIndex = -1;
  }

  static private final int _DEFAULT_STACK_SIZE = 10;

  static private final int _DEFAULT_PROPERTY_MAP_SIZE = 31;

  private transient StackFrameMap _nodePropertyMap;
  private int                     _renderedStackDepth;
  private UINode[]                _renderedNodeStack;
  private NamespaceMap            _properties;
}
