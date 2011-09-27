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
package org.apache.myfaces.trinidad.bean;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;

/**
 * A silly StateHolder implementation that will increment itself
 * by one when its state is saved, and one more when its state
 * is restored.  So, after a round trip, the original instance will
 * be at 1, and the new instance at 2.
 */
public class SillyStateHolder implements StateHolder
{
  public SillyStateHolder()
  {
    _count = 0;
  }

  @Override
  public String toString()
  {
    return "" + _count;
  }

  public Object saveState(FacesContext context)
  {
    _count++;
    return new Integer(_count);
  }

  public void restoreState(FacesContext context, Object state)
  {
    _count = ((Integer) state).intValue() + 1;
  }

  public boolean isTransient()
  {
    return _transient;
  }

  public void setTransient(boolean newTransientValue)
  {
    _transient = newTransientValue;
  }

  private boolean _transient;
  private int _count;
}
