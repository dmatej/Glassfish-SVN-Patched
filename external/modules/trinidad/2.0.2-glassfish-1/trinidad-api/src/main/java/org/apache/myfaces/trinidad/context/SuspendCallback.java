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
package org.apache.myfaces.trinidad.context;

/**
 * Class to use with {@link ComponentContextManager#partialSuspend} to control how far
 * into the context change stack to suspend to. This is useful for components that need to
 * suspend to a certain point in the change stack, but not the entire stack.
 */
public abstract class SuspendCallback
{
  /**
   * Return value for the {@link #getSuspendResult(ComponentContextChange)} function to specify
   * when to stop suspending the context change stack.
   */
  public enum SuspendResult
  {
    /**
     * Stop checking changes, stop without suspending the current change.
     */
    STOP,

    /**
     * Stop checking changes, stop after suspending the current change.
     */
    STOP_AFTER_CURRENT,

    /**
     * The desired change has not yet been found, keep suspending.
     */
    CONTINUE
  }

  /**
   * Determine how suspension should behave with regards to partial suspension.
   *
   * @param change The change to consider suspending
   * @return enumeration specifying how far to suspend to
   */
  public abstract SuspendResult getSuspendResult(
    ComponentContextChange change);
}
