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

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.model.RowKeySet;

/**
 * Event that is generated when the contents of a RowKeySet changes.
 */
public abstract class RowKeySetChangeEvent extends FacesEvent
{
  /**
   * Creates a new Event.
   * @param addedSet This is the Set of keys that have just been added.
   * @param removedSet This is the Set of keys that have just been removed.
   */
  // FIXME: What is the parametrized type inside RowKeySet?
  public RowKeySetChangeEvent(
      UIComponent source, 
      RowKeySet   removedSet,
      RowKeySet   addedSet)
  {
    this(source, removedSet, addedSet, false);
  }

  /**
   * This constructor lazily computes the difference between the 
   * oldSet and the newSet.
   * @param oldSet This is the Set of keys before any changes.
   * @param newSet This is the Set of keys after any changes.
   */
  // FIXME: What is the parametrized type inside RowKeySet?
  public RowKeySetChangeEvent(
      RowKeySet   oldSet,
      RowKeySet   newSet,
      UIComponent source)
  {
    // "oldSet" is very often the actual instance-on-the-component.
    // so make sure that we clone this object, so that subsequent mutations of
    // the instance-on-the-component will
    // not affect the parameters of this event: bug 4733858:
    // we do the clone in the _diff() method:
    this(source, oldSet, newSet, true);
  }

  // FIXME: What is the parametrized type inside RowKeySet?
  private RowKeySetChangeEvent(
      UIComponent source,
      RowKeySet   oldRemoved,
      RowKeySet   newAdded,
      boolean     needsDiff)
                               
  {
    super(source);
    _newAdded = newAdded;
    _oldRemoved = oldRemoved;
    _needsDiff = needsDiff;
  }

  /**
   * Gets the Set of keys that have just been added.
   */
  public RowKeySet getAddedSet()
  {
    _diff();
    return _newAdded;
  }

  /**
   * Gets the Set of keys that have just been removed.
   */
  public RowKeySet getRemovedSet()
  {
    _diff();
    return _oldRemoved;
  }
  
  private void _diff()
  {
    if (_needsDiff)
    {
      RowKeySet removed = _oldRemoved.clone();
      removed.removeAll(_newAdded);
      RowKeySet added = _newAdded.clone();
      added.removeAll(_oldRemoved);

      _needsDiff = false;
      _oldRemoved = removed;
      _newAdded = added;
    }
  }
  
  // set1 - oldSet or removed elements
  // set2 = newSet or added elements
  private RowKeySet _oldRemoved;
  private RowKeySet _newAdded;
  private boolean _needsDiff;
  private static final long serialVersionUID = 1L;
}
