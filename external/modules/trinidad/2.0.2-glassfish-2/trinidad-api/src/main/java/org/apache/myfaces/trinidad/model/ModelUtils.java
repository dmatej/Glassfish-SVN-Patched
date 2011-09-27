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
package org.apache.myfaces.trinidad.model;

import java.beans.IntrospectionException;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import javax.faces.model.ArrayDataModel;
import javax.faces.model.DataModel;
import javax.faces.model.ListDataModel;
import javax.faces.model.ScalarDataModel;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Provides convenience methods for converting objects into models, and
 * working with models.
 */
public final class ModelUtils
{

  /**
   * Gets an iteration of all the rowKeys in a collection.
   * The collection must not be modified while this iterator is being used.
   * The Iterator is not modifiable.
   */
  public static Iterator<Object> getRowKeyIterator(final CollectionModel model)
  {
    Iterator<Object> iter = new Iterator<Object>()
    {
      public boolean hasNext()
      {
        return _next != null;
      }

      public Object next()
      {
        if (_next == null)
          throw new NoSuchElementException();
        Object value = _next;
        _next = _next();
        return value;
      }

      public void remove()
      {
        throw new UnsupportedOperationException();
      }

      private Object _next()
      {
        int oldIndex = model.getRowIndex();
        try
        {
          model.setRowIndex(_rowIndex++);
          if (model.isRowAvailable())
            return model.getRowKey();
        } finally
        {
          model.setRowIndex(oldIndex);
        }
        return null;
      }

      private Object _next = Boolean.TRUE;// bogus value used for initialization
      private int _rowIndex = 0;
    };
    iter.next(); //initialize
    return iter;
  }

  /**
   * finds the last index in the given RowKeyIndex that has data and returns the
   * next index. This is useful when the {@link RowKeyIndex#getRowCount} of the
   * RowKeyIndex is not known.
   * @return a positive number if there is data. Returns zero if there is no data.
   * Note that -1 is never returned.
   */
  public static int getRowCount(RowKeyIndex model)
  {
    int rowCount = model.getRowCount();
    // is the rowCount known?
    if (rowCount >= 0)
    {
      return rowCount;
    }

    int lowerBound = 0;
    int upperBound = 100;
    for(model.setRowIndex(upperBound); model.isRowAvailable();)
    {
      lowerBound = upperBound;
      upperBound <<= 1;
      model.setRowIndex(upperBound);
    }
    return findLastIndex(model, lowerBound, upperBound);
  }

  /**
   * finds the last index in the given RowKeyIndex that has data and returns the
   * next index. This is useful when the {@link RowKeyIndex#getRowCount} of the
   * RowKeyIndex is not known.
   * @param startIndex starts the search from this index. Use zero to start from
   * the beginning.
   * @param endIndex the search will stop just before this index.
   * @return a number >= startIndex. Note that -1 is never returned.
   */
  public static int findLastIndex(RowKeyIndex table,
                                  int startIndex, int endIndex)
  {
    int rowCount = table.getRowCount();
    // is the rowCount known?
    if (rowCount >= 0)
    {
      // rowCount is known
      if (rowCount < endIndex)
        endIndex = rowCount;

      // however the rowCount might have lied. see bug 4157186
    }

    if (table.isRowAvailable(endIndex - 1))
      return endIndex;

    final int old = table.getRowIndex();
    try
    {
      while(startIndex < endIndex)
      {
        // TODO: issues here with overflow:
        int middle = (startIndex + endIndex) / 2;
        // it is possible that middle == startIndex.
        // however, it is not possible for middle == endIndex:
        assert (middle != endIndex) :
          "something is grossly wrong with integer division";

        table.setRowIndex(middle);
        if (table.isRowAvailable())
          startIndex = middle + 1;
        else
          endIndex = middle;
      }
      return endIndex;
    }
    finally
    {
      table.setRowIndex(old);
    }
  }


  /**
   * Converts an instance into a TreeModel
   */
  public static TreeModel toTreeModel(Object value)
  {
    if (value instanceof TreeModel)
      return (TreeModel) value;

    // if we don't have a treeModel at this point then
    // try to convert the value into a list and create a
    // tree that has no subtrees. it will just be a bunch of
    // root nodes.
    // Later on, if we can recognize some common POJO pattern as
    // being a tree, we can adapt it here.
    return new ChildPropertyTreeModel(value, null);
  }

  /**
   * Converts an instance into a MenuModel
   */
  public static MenuModel toMenuModel(Object value)
  {
    if (value instanceof MenuModel)
      return (MenuModel) value;
    else
    {
      try
      {
        return new ViewIdPropertyMenuModel(value, null);
      }
      catch (IntrospectionException e)
      {
        IllegalArgumentException re =
          new IllegalArgumentException(_LOG.getMessage(
            "CANNOT_CONVERT_INTO_MENUMODEL",value));
        re.initCause(e);
        throw re;
      }
    }
  }



  /**
   * Converts an instance into a CollectionModel.
   * @param value This can be a DataModel, List, Array or other CollectionModel.
   */
  public static CollectionModel toCollectionModel(Object value)
  {
    if (value instanceof CollectionModel)
      return (CollectionModel) value;
    else
    {
      return new SortableModel(value);
    }
  }

  /**
   * Converts an instance into a DataModel.
   * @param value Supported instances include java.util.List and
   * arrays.
   */
  public static DataModel toDataModel(Object value)
  {
    if (value instanceof DataModel)
    {
      return (DataModel) value;
    }

    if (value instanceof Object[])
    {
      return new ArrayDataModel((Object[]) value);
    }

    if (value == null)
    {
      return new ListDataModel(Collections.emptyList());
    }
    else if (value instanceof List)
      return new ListDataModel((List) value);

    return new ScalarDataModel(value);
  }

  private ModelUtils()
  {
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ModelUtils.class);
}
