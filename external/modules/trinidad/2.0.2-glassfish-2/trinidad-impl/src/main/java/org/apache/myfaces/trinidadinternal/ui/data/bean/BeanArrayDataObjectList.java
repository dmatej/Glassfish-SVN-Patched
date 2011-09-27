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
package org.apache.myfaces.trinidadinternal.ui.data.bean;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.ui.data.DataObject;
import org.apache.myfaces.trinidadinternal.ui.data.DataObjectList;

/**
 * DataObjectList class that will automatically turn each
 * bean into a DataObject.  See the BeanAdapterUtils for
 * more information on this process.
 * <p>
 * Individual elements of the bean array do not all
 * need to be the same type - though if you specify
 * the type in the constructor, that is effectively the
 * same thing.  Also, elements of the array that implement
 * the DataObject interface will be handled without extra
 * overhead, and null elements are allowed as well.
 * <p>
 * @see BeanAdapterUtils
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bean/BeanArrayDataObjectList.java#0 $) $Date: 10-nov-2005.18:56:48 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class BeanArrayDataObjectList implements DataObjectList
{
  /**
   * Creates a BeanArrayDataObjectList.
   * @param array the array of beans
   */
  public BeanArrayDataObjectList(Object[] array)
  {
    if (array == null)
      throw new NullPointerException();

    _array = array;
  }


  /**
   * Returns the number of items in the data set.
   */
  public int getLength()
  {
    return _array.length;
  }

  /**
   * Returns the DataObject at the index.
   */
  public DataObject getItem(int index)
  {
    Object o = _array[index];

    try
    {
      return BeanAdapterUtils.getAdapter(o);
    }
    catch (IllegalAccessException iae)
    {
        _LOG.severe(iae);
    }
    catch (InstantiationException ie)
    {
        _LOG.severe(ie);
    }

    return null;
  }

  private final Object[] _array;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BeanArrayDataObjectList.class);
}
