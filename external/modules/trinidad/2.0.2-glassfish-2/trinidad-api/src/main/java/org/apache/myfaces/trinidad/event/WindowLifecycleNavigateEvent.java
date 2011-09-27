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

import org.apache.myfaces.trinidad.context.Window;

/**
 * WindowLifecycleEvent delivered when the current window is being unloaded
 * in order to navigate to a new location
 */
public final class WindowLifecycleNavigateEvent extends WindowLifecycleEvent
{
  public WindowLifecycleNavigateEvent(Window source, String destination)
  {
    super(source, WindowLifecycleEvent.Cause.NAVIGATE);
    
    if (destination == null)
      throw new NullPointerException();
    
    _destination = destination;
  }
  
  /**
   * Returns the URL to which the page is navigating.
   * <p>
   * The destination is not guaranteed to be normalized;  it may
   * be absolute, page-relative, or server-relative.  It is also
   * not guaranteed to be correct, as a browser
   * may be redirected to an alternate destination.
   */
  public String getDestination()
  {
    return _destination;
  }

  @Override  
  public int hashCode()
  {
    return super.hashCode() * 37 + _destination.hashCode();
  }


  @Override
  public boolean equals(Object o)
  {
    if (this == o)
      return true;
    else if ((o != null) && (o instanceof WindowLifecycleNavigateEvent))
    {
      return subclassEquals((WindowLifecycleEvent)o) &&
             _destination.equals(((WindowLifecycleNavigateEvent)o)._destination);
    }
    else
    {
      return false;
    }
  }

  @Override
  public String toString()
  {
    return super.toString() + ",destination=" + _destination;
  }


  private final String _destination;
  
  private static final long serialVersionUID = 1L;
}
