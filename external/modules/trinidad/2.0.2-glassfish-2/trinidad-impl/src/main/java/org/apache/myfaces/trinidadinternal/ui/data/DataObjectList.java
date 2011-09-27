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

/**
 * DataObjectList provides indexed access to a set of data objects.
 * <p>
 * DataObjectList implementations will not, in general, exist on their own;
 * this interface exists simply to provide indexed access to a set of
 * DataObjects selected from a parent DataObject.
 * <p>
 * @see org.apache.myfaces.trinidadinternal.ui.data.DataObject
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/DataObjectList.java#0 $) $Date: 10-nov-2005.18:56:31 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public interface DataObjectList
{
  /**
   * Returns the number of items in the data set.
   */
  public int getLength();

  /**
   * Returns the DataObject at the index.
   */
  public DataObject getItem(int index);
}
