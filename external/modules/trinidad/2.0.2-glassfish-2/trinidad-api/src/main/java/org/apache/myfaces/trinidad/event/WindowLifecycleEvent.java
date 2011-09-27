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
import org.apache.myfaces.trinidad.context.Window.LifecycleState;

/**
 * Event delivered when the LifecycleState of a Window changes.  The <code>cause</code>
 * indicates the cause ot the state change.  The state diagram for theWindow LifecycleStates
 * is
 <pre>
                      +-----------load---------------+
                      |                              |       ---expire---
                      V      /---unload----\         |      /            \
 <start> ---open--->OPEN-----               ----->UNLOADED--              -->CLOSED
                      |      \--navigate---/         ^      \            /
                      |                              |       ---close----
                      +---------closing--------------+

 </pre>
 * The new LifecycleStates can be retrieved by calling <code>getLifecycleState</code> on the
 * source Window or by calling the <code>getNewLifecycleState</code> convenience function
 * on the WindowLifecycleEvent
 * @see org.apache.myfaces.trinidad.context.Window
 * @see org.apache.myfaces.trinidad.context.Window.LifecycleState
 */
public class WindowLifecycleEvent extends WindowEvent
{
  /**
   * What caused the delivery of the WindowLifecycleEvent.
   */
  public enum Cause
  {
    /**
     * Delivered when a new Window is open
     */
    OPEN,
    /**
     * Delivered when the content of a Window have been unloaded but cause of the unloading
     * isn't known.
     */
    UNLOAD,
    /**
     * Delivered when the content of a Window have been unloaded as a result of
     * navigating within the application
     */
    NAVIGATE,
    /**
     * Delivered when the content of a Window have been unloaded in order to
     * close the window
     */
    CLOSING,
    
    /**
     * The contents of an existing Window are being reloaded
     */
    RELOAD,
    
    /**
     * The Window is believed to have been closed by the user
     */
    EXPIRE,

    /**
     * The Window is believed to have been closed by the user
     */
    CLOSE
  }

  /**
   * Creates a WindowOpenEvent event for the specified Window and cause.
   */
  public WindowLifecycleEvent(Window source, Cause cause)
  {
    super(source);
    
    if (cause == null)
      throw new NullPointerException();
    
    _cause = cause;
  }
 
  /**
   * @return the cause of the WindowOpen event.
   */
  public Cause getCause()
  {
    return _cause;
  }
  
  /**
   * Returns the new LifecycleState that the Window has moved to.
   */
  public final LifecycleState getNewLifecycleState()
  {
    return getSource().getLifecycleState();
  }

  @Override  
  public int hashCode()
  {
    return getSource().hashCode() * 37 + _cause.hashCode();
  }
  
  @Override
  public boolean equals(Object o)
  {
    if (this == o)
      return true;
    else if ((o != null) && (o.getClass() == WindowLifecycleEvent.class))
    {
      return subclassEquals((WindowLifecycleEvent)o);
    }
    else
    {
      return false;
    }
  }
  
  @Override
  public String toString()
  {
    return super.toString() + ",cause=" + _cause;
  }
  
  /**
   * Called by subclass <code>equals</code> implementation to check the WindowEvent
   * portion of equivalence.
   * @param e Non-null WindowEvent to compare for equality
   * @return <code>true</code> if the the WindowEvent satisfies the WindowEvent portion
   * of equivalence.
   */
  protected final boolean subclassEquals(WindowLifecycleEvent e)
  {
    return super.subclassEquals(e) && _cause.equals(e._cause);
  }

  private final Cause _cause;
  
  private static final long serialVersionUID = 1L;
}
