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

import java.util.Map;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadinternal.ui.data.DataObject;

import org.apache.myfaces.trinidadinternal.ui.path.Path;
import org.apache.myfaces.trinidadinternal.ui.path.PathImpl;

/**
 * Abstract RenderingContext implementation providing support for
 * managing logical ancestors and DataProviders.  Although this
 * class supports both root and child RenderingContexts, it is
 * configured by default to support child-style RenderingContexts.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/LogicalNodeRenderingContext.java#0 $) $Date: 10-nov-2005.18:50:13 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public abstract class LogicalNodeRenderingContext implements UIXRenderingContext
{
  /**
   * Creates a AbstractRenderingContext.
   */
  public LogicalNodeRenderingContext()
  {
    _path      = new PathImpl();
    _nodeStack = new UINode[getDefaultNodeStackSize()];
  }


  public FacesContext getFacesContext()
  {
    return null;
  }

  /**
   * Returns a DataObject for the current node (such as a row
   * of a table).
   */
  public DataObject getCurrentDataObject()
  {
    return _currentDataObject;
  }


  /*
   * Sets the new currentDataObject, returning the current
   * currentDataObject.
   * <p>
   * @see #getCurrentDataObject
   */
  public DataObject setCurrentDataObject(
    DataObject newDataObject
    )
  {
    DataObject oldDataObject = getCurrentDataObject();

    _currentDataObject = newDataObject;

    return oldDataObject;
  }

  /**
   * Returns the number of nodes in the path form the current node to
   * the root of the tree of nodes being rendered.
   */
  public int getAncestorNodeCount()
  {
    return _stackDepth;
  }


  /**
   * Returns an ancestor of the node currently being processed.  The
   * zero-based index from least to most distant -
   * <code>getAncestorNode(0)</code> will always return the current
   * node.  Returns null if the index is greater than or equal to
   * the number of ancestors.
   */
  public UINode getAncestorNode(
    int index
    )
  {
    if (index < 0)
      throw new IllegalArgumentException();

    int stackDepth = _stackDepth;

    // skip the current UINode
    if (index < stackDepth)
    {
      return _nodeStack[stackDepth - index - 1];
    }
    else
    {
      // index higher than number of parents passed in
      return null;
    }
  }


  /**
   * Returns the path to the current node.
   */
  public Path getPath()
  {
    return _path;
  }


  /**
   * Adds the UINode about to be rendered to the rendering stack.
   * Clients should never call this method. They should instead
   * subclass BaseRenderer, which calls this method when needed.
   */
  public void pushChild(
    UINode child,
    String childName,
    int    childIndex
    )
  {
    int stackDepth = _stackDepth;
    _stackDepth++;

    // if the stack is full, grow the stack by doubling its size
    if (stackDepth >= _nodeStack.length)
    {
      UINode[] newStack = new UINode[_nodeStack.length * 2];

      System.arraycopy(_nodeStack, 0, newStack, 0, _nodeStack.length);

      _nodeStack = newStack;
    }

    _nodeStack[stackDepth] = child;

    // only push if depth is > 0 since the top element on the
    // stack doesn't get added to the path
    if (stackDepth > 0)
    {
      if (childIndex >= 0)
      {
        _path.add(childIndex);
      }
      else
      {
        _path.add(childName);
      }
    }
  }


  /**
   * Removes a UINode from the stack.  Clients should never call this
   * method;  they should instead subclass BaseRenderer, which calls
   * this method when needed.
   */
  public void popChild()
  {
    int stackDepth = _stackDepth - 1;
    _stackDepth = stackDepth;

    assert (stackDepth >= 0);

    // Let GC do its work
    _nodeStack[stackDepth] = null;

    // only pop if depth is > 0 since the top element on the stack was
    // never added to the path
    if (stackDepth > 0)
    {
      _path.removeLastElement();
    }
  }


  public void pushRenderedChild(
    UIXRenderingContext currentContext,
    UINode child
    )
  {
  }


  public void popRenderedChild(UIXRenderingContext currentContext)
  {
  }


  /**
   * Internal version of getDataObject().  <em>Do not call this
   * function</em> unless you are yourself a RenderingContext.
   */
  public DataObject getDataObject(
    UIXRenderingContext context,
    String namespaceURI,
    String name)
  {
    return null;
  }

  /**
   * Returns the DataObject for the given namespace and name pair.
   * It will call each data provider added with addDataProvider,
   * starting with the most recently added, until one returns non-null.
   * Then, it will look for DataObjects added with setDataObject().
   */
  public DataObject getDataObject(
    String namespaceURI,
    String name
    )
  {
    return getDataObject(this, namespaceURI, name);
  }


  /**
   * Returns the default initial number of nodes in the stack of
   * nodes.  Subclassers implementing RenderingContexts that
   * may support deep node depths, should override this method
   * to return a larger number.
   */
  protected int getDefaultNodeStackSize()
  {
    return _DEFAULT_STACK_SIZE;
  }




  
  /**
   * Given a key, look in the skin resource key map for the mapped value. 
   * If there isn't a value or the map is null, 
   * return the key itself. 
   * @param key 
   * @return String key, if in skin resource key map, then the mapped key.
   */
  protected String getSkinResourceMappedKey(String key)
  {
    String mappedKey = null;
    Map<String, String> keyMap = getSkinResourceKeyMap();
    
    if (keyMap != null)
    {
      mappedKey = keyMap.get(key);
      // if it isn't in the map, just use the key itself.
      if (mappedKey == null)
        mappedKey = key;
    }
    else
      mappedKey = key;
    
    return mappedKey;
  }

  /**
   * Reset the state of the RenderingContext for reuse, even in case
   * where RenderingContext did not complete cleanly.
   *
   * =-= bts  Most of this cleanup only really needs to be done
   *          if we don't handle catching exceptions and popping
   *          the stack correctly when rendering
   */
  protected void reset()
  {
    //
    // clear node stack.
    //
    for (int i = 0; i < _stackDepth; i++)
    {
      _nodeStack[i] = null;
    }

    _stackDepth = 0;

    //
    // clear path
    //
    int elementCount = _path.getElementCount();

    while (elementCount-- > 0)
    {
      _path.removeLastElement();
    }


    // release the current data object
    _currentDataObject = null;

  }

  @Override
  public Object clone()
  {
    LogicalNodeRenderingContext context;

    try
    {
      context = (LogicalNodeRenderingContext)super.clone();
    }
    catch (CloneNotSupportedException e)
    {
      throw new IllegalStateException();
    }

    //
    // clone the node stack
    //
    UINode[] nodeStack = new UINode[_stackDepth];

    System.arraycopy(_nodeStack, 0, nodeStack, 0, _stackDepth);

    context._nodeStack = nodeStack;

    //
    // clone the Path
    //
    context._path = (PathImpl)_path.clone();


    return context;
  }

  private int          _stackDepth;
  private UINode[]     _nodeStack;
  private PathImpl     _path;
  

  private DataObject              _currentDataObject;

  static private final int _DEFAULT_STACK_SIZE = 10;

  
}
