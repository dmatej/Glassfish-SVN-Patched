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

import java.util.EventObject;

/**
 * Event that is generated when RowKey changes.
 * This event is fired when a row is updated, inserted or deleted
 * The oldRowKey or new RowKey can be NULL
 * 
 * The serialization behavior depends on the implementation of the collectionModel that consumes 
 * the RowKeyChangeEvent. If the collectionModel implementation is serializable and keeps track 
 * of row key change events that it recevies, then these events are serialized with the model; 
 * otherwise, the events are not serialized
 */
public class RowKeyChangeEvent extends EventObject 
{
  /**
   * The operation that triggers row key to change
   */
  public enum Cause
  {
     UPDATE,
     INSERT,
     DELETE
  };
  
  /**
  * Creates a new RowKeyChangeEvent
  * @param source    the source of the event
  * @param oldRowKey the old RowKey.
  * @param newRowKey the new RowKey.
  * @param cause the operation that triggers this event
  */  
  public RowKeyChangeEvent(CollectionModel source, Object oldRowKey, Object newRowKey, Cause cause)
  {
    super(source);

    _oldRowKey = oldRowKey;
    _newRowKey = newRowKey;
    _cause = cause;
  }
   
  /**
  * retrieve the old RowKey from the event
  * @return the old RowKey of the event.
  */
  public Object getOldRowKey()
  {
    return _oldRowKey;
  }
  
  /**
  * retrieve the new row key from the event
  * @return the new row key of the event.
  */
  public Object getNewRowKey()
  {
    return _newRowKey;
  }
  
  public Cause getCause()
  {
    return _cause;
  }
  
  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    else if (!(o instanceof RowKeyChangeEvent))
      return false;
    else
    {
      RowKeyChangeEvent otherEvent = (RowKeyChangeEvent)o;

      return _oldRowKey.equals(otherEvent._oldRowKey) &&
             _newRowKey.equals(otherEvent._newRowKey) &&
             _cause == otherEvent._cause;
    }    
  }

  @Override
  public int hashCode()
  {
      int hashCode = 17;
      hashCode = 31 * hashCode + _oldRowKey.hashCode();
      hashCode = 31 * hashCode + _newRowKey.hashCode(); 
      hashCode = 31 * hashCode + _cause.hashCode();
      return hashCode;
  }
  
  @Override
  public String toString()
  {
    return super.toString() +
           "[oldRowKey=" + _oldRowKey.toString() +
           ", newRowKey=" + _newRowKey.toString() + 
           ", cause=" + _cause.toString() + "]";
  }
  
  private final Object _oldRowKey;
  private final Object _newRowKey;
  private final Cause _cause;
  private static final long serialVersionUID = 1L;
}
