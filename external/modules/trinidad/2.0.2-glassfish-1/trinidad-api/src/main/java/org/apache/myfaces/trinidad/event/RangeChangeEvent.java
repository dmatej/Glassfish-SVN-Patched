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
import javax.faces.event.FacesListener;


/**
 * A RangeChangeEvent is a notification that the range of the 
 * source component has been changed as a result of user interface activity. 
 * It contains the old start and end values and the new start and end values.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/event/RangeChangeEvent.java#0 $) $Date: 10-nov-2005.19:09:04 $
 */
public class RangeChangeEvent extends FacesEvent
{
  /**
   * Construct a new event object from the specified source component, 
   * old start (inclusive), old end (exclusive), new start (inclusive)
   * and new end (exclusive).
   * @param source - Source UIComponent for this event
   * @param oldStart - The previous start of this UIComponent's selected range, 
   *                   inclusive
   * @param oldEnd - The previous end of this UIComponent's selected range, 
   *                 exclusive
   * @param newStart - The new start of this UIComponent's selected range, 
   *                 inclusive
   * @param newEnd - The new end of this UIComponent's selected range, 
   *                 exclusive
   */
  public RangeChangeEvent(
    UIComponent source, 
    int oldStart, 
    int oldEnd, 
    int newStart, 
    int newEnd)
  {
    super(source);
    _oldStart = oldStart;
    _oldEnd   = oldEnd;    
    _newStart = newStart;
    _newEnd   = newEnd;

  }

  /**
   * Returns the old start of the range, inclusive. E.g., 
   * if the old range was for the first 5 items, oldStart would be 0.
   */
  public int getOldStart()
  {
    return _oldStart;
  }
  
  /**
   * Returns the old end of the range, exclusive.E.g., 
   * if the old range was for the first 5 items, oldEnd would be 5.
   */
  public int getOldEnd()
  {
    return _oldEnd;
  }
  /**
   * Returns the new start, inclusive. E.g., 
   * if the new range is for the second 5 items, newStart would be 5.
   */  
  public int getNewStart()
  {
    return _newStart;
  }
  
  /**
   * Returns the new end, exclusive. E.g., 
   * if the new range is for the second 5 items, newEnd would be 10.
   */  
  public int getNewEnd()
  {
    return _newEnd;
  }
   
  @Override
  public void processListener(FacesListener listener)
  {
    ((RangeChangeListener)listener).processRangeChange(this);
  }

  @Override
  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof RangeChangeListener);
  }

  @Override
  public int hashCode()
  {
    return (_newStart << 4) ^
           (getPhaseId().hashCode() << 8) ^
           (getComponent().hashCode());
  }

  @Override
  public boolean equals(Object o)
  {
    if (o instanceof RangeChangeEvent)
    {
      RangeChangeEvent that = (RangeChangeEvent)o;
      return (this._newStart == that._newStart &&
              this._newEnd == that._newEnd &&
              this._oldStart == that._oldStart &&
              this._oldEnd== that._oldEnd &&
              this.getComponent().equals(that.getComponent()) &&
              this.getPhaseId().equals(that.getPhaseId()));
    }

    return false;
  }

  @Override
  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append(getClass().getName());
    sb.append("[phaseId=");
    sb.append(getPhaseId());
    sb.append(",component=");
    sb.append(getComponent());
    sb.append(",newStart=");
    sb.append(getNewStart());
    sb.append(",newEnd=");
    sb.append(getNewEnd());   
    sb.append(",oldStart=");
    sb.append(getOldStart());
    sb.append(",oldEnd=");
    sb.append(getOldEnd());     
    sb.append(']');
    return sb.toString();
  }
  
  private final int _oldStart;
  private final int _oldEnd;  
  private final int _newStart;
  private final int _newEnd;  

  private static final long serialVersionUID = 1L;
}
