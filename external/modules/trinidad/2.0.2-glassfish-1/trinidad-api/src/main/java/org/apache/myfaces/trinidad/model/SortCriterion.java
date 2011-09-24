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

import java.io.Serializable;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * This class pairs together a property and a direction by which a 
 * CollectionModel can be sorted.
 * @see CollectionModel#getSortCriteria
 */
public class SortCriterion implements Serializable
{
  public SortCriterion(String property, boolean isAscending)
  {
    if (property == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_PROPERTY"));

    _property = property;
    _sortOrder = isAscending;
  }

  /**
   * Gets the direction in which the property of this class is sorted.
   * @return true if the property identified by this class is sorted in
   * ascending order.
   */
  public boolean isAscending()
  {
    return _sortOrder;
  }
  
  /**
   * Gets the property that is identified by this class. This is the property
   * that must be sorted by. If a collection of beans is being sorted, bean rules
   * will be used to find a suitable getter method that matches this property.
   * The value returned by the getter method will be sorted on.
   * If a collection of Maps is being sorted, this property will be used
   * as the key into each Map to get at the value being sorted.
   */
  public String getProperty()
  {
    return _property;
  }

  @Override
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;

    if (obj instanceof SortCriterion)
    {
      SortCriterion that = (SortCriterion) obj;
      return (this.getProperty().equals(that.getProperty())) &&
        (this.isAscending() == that.isAscending());
    }

    return false;
  }
  
  @Override
  public int hashCode()
  {
    int hc = getProperty().hashCode();
    return isAscending() ? hc : -hc;
  }
  
  private final String _property;
  private final boolean _sortOrder;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SortCriterion.class);

  private static final long serialVersionUID = 1L;
}
