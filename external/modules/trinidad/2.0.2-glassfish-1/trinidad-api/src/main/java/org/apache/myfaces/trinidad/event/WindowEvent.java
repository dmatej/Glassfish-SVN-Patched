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

import java.util.EventObject;
import org.apache.myfaces.trinidad.context.Window;

/**
 * Represents an event delivered with a Window as the source.
 * @see org.apache.myfaces.trinidad.context.Window
 * @see org.apache.myfaces.trinidad.event.WindowLifecycleListener
 */
public abstract class WindowEvent extends EventObject
{
  /**
   * Constructs a WindowEvent for the specified Window
   * @param source the Window that tis the source of this event.
   */
  protected WindowEvent(Window source)
  {
    super(source);
    
    if (source == null)
      throw new NullPointerException();
  }
 
  /**
   * @return the Window that this event ocurred on.
   */
  @Override
  public Window getSource()
  {
    return (Window)super.getSource();
  }

  /**
   * Called by subclass <code>equals</code> implementation to check the WindowEvent
   * portion of equivalence.
   * @param e Non-null WindowEvent to compare for equality
   * @return <code>true</code> if the the WindowEvent satisfies the WindowEvent portion
   * of equivalence.
   */
  protected final boolean subclassEquals(WindowEvent e)
  {
    return getSource().equals(e.getSource());
  }

  private static final long serialVersionUID = 1L;
}