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
package org.apache.myfaces.trinidadinternal.config;

import java.util.concurrent.atomic.AtomicReference;

import org.apache.myfaces.trinidad.util.ThreadLocalUtils.ThreadLocalLifecycle;
import org.apache.myfaces.trinidad.util.ThreadLocalUtils.ThreadLocalManager;

/**
 * Implementation of ThreadLocalLifecycle for request-scoped ThreadLocals, registered
 * through META-INF/SERVICES.  This class cooperates with GlobalConfiguratorImpl in order
 * to ensure that the request-scoped ThreadLocals are cleaned up at the end of each
 * request.
 */
public class ThreadLocalResetter extends ThreadLocalLifecycle
{
  /**
   * All class to be instatiated through reflection
   */
  public ThreadLocalResetter()
  {
  }
  
  /**
   * Called by ThreadLocalUtils after instantiating the ThreadLocalResetter in order to
   * pass the ThreadLocalManager to use to clean up the request-scoped ThreadLocals.
   */
  public void init(ThreadLocalManager manager)
  {
    if (manager == null)
      throw new NullPointerException();
    
    // save the ThreadLocalManager to use to clean up the request-scoped ThreadLocals 
    _threadLocalManager.set(manager);
    
    // slam a reference to our instance on the GlobalConfiguratorImpl so that it can
    // call __removeRequestThreadLocals when the request is finished 
    GlobalConfiguratorImpl.getInstance().__setThreadLocalResetter(this);
  }
  
  /**
   * Called by the GlobalConfiguratorImpl when the request is finished so that the
   * ThreadLocalResetter can ask the ThreadLocalManager to clean itself up
   */
  void __removeThreadLocals()
  {
    ThreadLocalManager threadLocalManager = _threadLocalManager.get();
    
    if (threadLocalManager != null)
    {
      // clean up all of the request-scoped ThreadLocals
      threadLocalManager.removeThreadLocals();
    }
  }

  // ThreadLocalManager to clean up
  private AtomicReference<ThreadLocalManager> _threadLocalManager = new 
                                                   AtomicReference<ThreadLocalManager>();
}
