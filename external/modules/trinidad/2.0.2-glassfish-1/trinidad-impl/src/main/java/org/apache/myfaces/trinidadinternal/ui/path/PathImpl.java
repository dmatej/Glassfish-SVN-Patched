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
package org.apache.myfaces.trinidadinternal.ui.path;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * Default implementation of the path interface.  This subclass
 * is mutable.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/path/PathImpl.java#0 $) $Date: 10-nov-2005.18:50:28 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PathImpl implements Path
{
  /**
   * Creates an empty path.
   */
  public PathImpl()
  {
    // _length = 0;
    _elements  = new Object[8];
  }


  /**
   * Returns the number of elements in the path.
   */
  public int getElementCount()
  {
    return _length;
  }


  /**
   * Returns true if the element at the given index is a named child,
   * as opposed to an indexed child.
   * @param elementIndex the zero-based index into the path;  if
   *  negative, counts back from the end of the path
   */
  public boolean isElementNamed(int elementIndex)
  {
    elementIndex = _normalizeIndex(elementIndex);
    return !(_elements[elementIndex] instanceof Integer);
  }


  /**
   * Returns the child index of a path element;  returns -1
   * if that part of the path is a named child.
   * @param elementIndex the zero-based index into the path;  if
   *  negative, counts back from the end of the path
   */
  public int getElementIndex(int elementIndex)
  {
    elementIndex = _normalizeIndex(elementIndex);
    if (elementIndex < 0)
      return -1;

    Object element = _elements[elementIndex];
    if (element instanceof Integer)
    {
      return ((Integer) element).intValue();
    }

    return -1;
  }


  /**
   * Returns the chid name of a path element;  returns null
   * if that part of the path is an indexed child.
   * @param elementIndex the zero-based index into the path;  if
   *  negative, counts back from the end of the path
   */
  public String getElementName(int elementIndex)
  {
    elementIndex = _normalizeIndex(elementIndex);
    Object element = _elements[elementIndex];
    if (elementIndex < 0)
      return null;

    if (element instanceof String)
    {
      return ((String) element);
    }

    return null;
  }


  /**
   * Add a named child onto the path.
   */
  public void add(String namedChild)
  {
    _add(namedChild);
  }


  /**
   * Add an indexed child onto the path.
   */
  public void add(int indexedChild)
  {
    _add(indexedChild);
  }


  /**
   * Appends a path to this one.
   */
  public void add(Path path)
  {
    int length = _length;
    int fromLength = path.getElementCount();

    _ensureLength(length + fromLength);

    // Hack for PathImpls
    if (path instanceof PathImpl)
    {
      System.arraycopy(((PathImpl) path)._elements, 0,
                       _elements, length, fromLength);
    }
    else
    {
      for (int i = 0; i < fromLength; i++)
      {
        if (path.isElementNamed(i))
          _elements[length + i] = path.getElementName(i);
        else
          _elements[length + i] =
             path.getElementIndex(i);
      }
    }

    _length = length + fromLength;
  }


  /**
   * Pops a single element off the end of the path.
   */
  public void removeLastElement()
  {
    int length = _length;
    if (length <= 0)
      throw new IllegalStateException();
    _length = length - 1;
  }


  /**
   * Returns the result of partially following the path from a
   * starting node.  Returns null if any of the steps of the path
   * cannot be followed.
   * @param from the node to start from
   * @param start the number of steps to skip;  if 0,
   *   starts from the begining.  If negative, counts from
   *   the end.
   * @param depth the number of steps in the path to follow; if 0,
   *   will just return <code>from</code>
   */
  public UINode followPath(
    UIXRenderingContext context,
    UINode           from,
    int              start,
    int              depth
    )
  {
    start = _normalizeIndex(start);
    if ((start + depth > _length) || (start < 0) || (depth < 0))
      return null;

    Object[] elements = _elements;
    for (int i = 0; i < depth; i++)
    {
      from = _follow(context, from, elements[i + start]);
      if (from == null)
        return null;
    }

    return from;
  }


  /**
   * Follows the entire path.
   * Returns null if any of the steps of the path
   * cannot be followed.
   * @param from the node to start from
   */
  final public UINode followPath(
    UIXRenderingContext context,
    UINode           from
    )
  {
    return followPath(context, from, 0, getElementCount());
  }


  /**
   * Clones the path.
   */
  @Override
  public Object clone()
  {
    try
    {
      PathImpl pi = (PathImpl) super.clone();
      pi._elements = pi._elements.clone();
      return pi;
    }
    catch (CloneNotSupportedException cnse)
    {
      // Should never get here.
      return null;
    }
  }


  /**
   * Returns a string representation of the path.
   */
  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer("Path[");
    boolean first = true;
    for (int i = 0; i < _length; i++)
    {
      if (!first)
        buffer.append(',');
      else
        first = false;
      Object o = _elements[i];
      if (o == null)
        buffer.append("null");
      else
        buffer.append(o.toString());
    }

    buffer.append(']');
    return new String(buffer);
  }

  private void _add(Object o)
  {
    int length = _length;
    _ensureLength(length + 1);
    _elements[length] = o;
    _length = length + 1;
  }

  private int _normalizeIndex(int index)
  {
    return (index < 0) ? _length + index : index;
  }

  private void _ensureLength(int length)
  {
    int oldLength = _elements.length;
    if (oldLength < length)
    {
      Object[] newElements = new Object[oldLength + _DEFAULT_INCREMENT];
      System.arraycopy(_elements, 0, newElements, 0, oldLength);
      _elements = newElements;
    }
  }

  // Creates a PathImpl based on an array of elements.
  PathImpl(Object[] elements)
  {
    if (elements != null)
    {
      _elements = elements;
      _length   = elements.length;
    }
  }

  static private UINode _follow(
    UIXRenderingContext context,
    UINode           from,
    Object           element
    )
  {
    if (element instanceof String)
    {
      return from.getNamedChild(context, (String) element);
    }
    else
    {
      int index = ((Integer) element).intValue();
      if (index >= from.getIndexedChildCount(context))
        return null;

      return from.getIndexedChild(context, index);
    }
  }


  private int      _length;
  private Object[] _elements;

  static private final int _DEFAULT_INCREMENT = 8;
}
