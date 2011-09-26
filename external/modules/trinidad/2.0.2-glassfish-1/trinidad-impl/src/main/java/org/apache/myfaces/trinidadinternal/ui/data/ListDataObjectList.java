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
package org.apache.myfaces.trinidadinternal.ui.data;

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

/**
 * This class creates a UIX Components DataObjectList to which DataObjects can be 
 * added and removed. This class is not synchronized.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/ListDataObjectList.java#0 $) $Date: 10-nov-2005.18:56:33 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ListDataObjectList implements DataObjectList 
{
  /**
   * Constructs an empty list
   */
  public ListDataObjectList()
  {
    _list = new ArrayList<DataObject>();
  }

  /**
   * Constructs an empty list with the specified initial capacity.
   * @param initialCapacity  the initial capacity of the list.
   * @see ArrayList#ArrayList(int)
   */
  public ListDataObjectList(int initialCapacity)
  {
    _list = new ArrayList<DataObject>(initialCapacity);
  }

  /**
   * @param list the Vector to use as the internal list. Every element of this
   *  Vector must be of type DataObject
   * @see DataObject
   */
  public ListDataObjectList(Vector<DataObject> list)
  {
    _list = list;
  }

  /**
   * Adds an item to this list
   * @param data the item to add to this list.
   */
  public void addItem(DataObject data)
  {
    _list.add(data);
  }

  /**
   * @param index the index of the item to be removed
   * @return the removed item.
   */
  public DataObject removeItem(int index)
  {
    return _list.remove(index);
  }

  /**
   * Removes all of the elements from this DataObjectList
   * @see List#clear()
   */
  public void clear()
  {
    _list.clear();
  }

  /**
   * Searches for the first occurrence of the given argument,
   *  testing for equality using the equals method.
   * @param data the DataObject to search for
   * @return -1 if the object is not found
   * @see List#indexOf(Object)
   */
  public int indexOf(Object data)
  {
    return _list.indexOf(data);
  }

  /**
   * @see List#toArray(Object[])
   */
  public Object[] toArray(Object[] anArray)
  {
    return _list.toArray(anArray);
  }

  /**
   * @param i is an index into this list
   * @return the element at the given index
   * @see DataObjectList#getItem(int)
   */
  public DataObject getItem(int i) 
  {
    return _list.get(i);
  }

  /**
   * @return the length of this list
   * @see DataObjectList#getLength()
   */
  public int getLength() 
  {
    return _list.size();
  }

  private final List<DataObject> _list;
}
