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
package org.apache.myfaces.trinidad.event;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;

import org.apache.myfaces.trinidad.model.SortCriterion;

/**
 * The Event generated when a Collection is to be sorted.
 */
public final class SortEvent extends FacesEvent
{
  /**
   * Creates a new SortEvent.
   * @param criteria each element must be of type SortCriterion
   * @see org.apache.myfaces.trinidad.model.SortCriterion
   */
  public SortEvent(UIComponent source, List<SortCriterion> criteria)
  {
    super(source);
    _criteria = criteria;
  }

  /**
   * Each element of this List is of type SortCriterion
   * @see org.apache.myfaces.trinidad.model.SortCriterion
   */
  public List<SortCriterion> getSortCriteria()
  {
    return _criteria;
  }

  @Override
  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof SortListener);
  }

  @Override
  public void processListener(FacesListener listener)
  {
    ((SortListener) listener).processSort(this);
  }
  
  
  private final List<SortCriterion> _criteria;

  private static final long serialVersionUID = 1L;
}
