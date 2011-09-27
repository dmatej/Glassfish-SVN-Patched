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
package org.apache.myfaces.trinidad.util;

import java.lang.ref.WeakReference;

import java.util.Collection;
import java.util.Iterator;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * Utility functions related to ThreadLocals.
 * This class provides utilities for managing the lifecycles of ThreadLocals.
 * In particular, many application servers do not clean up the ThreadLocals
 * added to the request thread before returning the request thread to
 * its thread pool.  This may have several severe side-effects, including
 * <ol>
 *   <li>Leakage of objects referenced by the thread local</li>
 *   <li>Possible security holes if the ThreadLocals contents are reused
 *       on a request for a different user.</li>
 *   <li>Leakage of the entire class loader in live patching scenarios</li>
 *   <li>ClassCastExceptions in live patching scenarios</li>
 * </ol>
 * To avoid these problems, this class provides utilities for creating
 * and registering ThreadLocals that will be removed from the thread
 * automatically at the end of the current request.
 * @see #newRequestThreadLocal
 * @see #registerRequestThreadLocal
 */
public final class ThreadLocalUtils
{
  /**
   * Integration interface implemented by object holding onto ThreadLocals
   * with a specified lifetime
   * @see ThreadLocalLifecycle
   */
  public static abstract class ThreadLocalManager
  {
    /**
     * Called by the ThreadLocalLifeCycle when the ThreadLocals managed by
     * this ThreadLocalManager need to be cleaned up by removing their
     * instances from the current Thread.
     */
    public abstract void removeThreadLocals();
  }
  
  /**
   * Integration interface implemented by an object that cleans up the
   * ThreadLocals in a ThreadLocalManager with the specified lifetime.
   * The ThreadLocalLifecycle cleans up the ThreadLocals by calling
   * <code>removeThreadLocals</code> on the ThreadLocalManager passed
   * to it in <code>init</code> at the appropriate time.
   * @see ThreadLocalManager
   * @see #init
   */
  public static abstract class ThreadLocalLifecycle
  {
    /**
     * Called after instantiating the ThreadLocalLifecycle so that the
     * ThreadLocalLifecycle can clean up the ThreadLocalManager at the
     * appropriate moment.
     * @param manager Manager of ThreadLocals that will be cleaned up by
     * this ThreadLocalLifecycle.
     * @see ThreadLocalManager#removeThreadLocals
     */
    public abstract void init(ThreadLocalManager manager);
  }
  
  // don't allow instantiation 
  private ThreadLocalUtils()
  {
  }

  /**
   * Creates and returns a new ThreadLocal that will be automatically removed from
   * the each request thread when the request finishes.
   * @return The ThreadLocal
   * @see #registerRequestThreadLocal
   */
  public static <T> ThreadLocal<T> newRequestThreadLocal()
  {
    return registerRequestThreadLocal(new ThreadLocal<T>());
  }

  /**
   * Registers and returns the ThreadLocal to be automatically removed from
   * the each request thread when the request finishes.
   * @param threadLocal ThreadLocal to register for automatic removal
   * @return The reigistered ThreadLocal
   * @see #newRequestThreadLocal
   */
  public static <T> ThreadLocal<T> registerRequestThreadLocal(ThreadLocal<T> threadLocal)
  {
    return _getThreadLocalManager(_REQUEST_SERVICE).registerThreadLocal(threadLocal);
  }
  
  /**
   * Returns the ResettableThreadLocalManager for the specified threadLocalGroup name.  If no
   * ResettableThreadLocalManager is currently registered for the threadLocalGroup, one will
   * be instantiated, its related ThreadLocalLifecycle instantiated and the ThreadLocalManager
   * registered with the ThreadLocalLifecycle
   * @param threadLocalGroup Identifier for this group
   * @return the ResettableThreadLocalManager for this threadLocalGroup name
   */
  private static ResettableThreadLocalManager _getThreadLocalManager(String threadLocalGroup)
  {
    // see if we already have a ResettableThreadLocalManager registered
    ResettableThreadLocalManager threadLocalManager = _threadLocalManagers.get(threadLocalGroup);
    
    // if we don't, create one
    if (threadLocalManager == null)
    {
      threadLocalManager = new ResettableThreadLocalManager();
      
      // handle simultaneous registration
      ResettableThreadLocalManager oldThreadLocalManager =
                            _threadLocalManagers.putIfAbsent(threadLocalGroup, threadLocalManager);
      
      if (oldThreadLocalManager != null)
      {
        // simultaneous registration, use the one that was registered first
        threadLocalManager = oldThreadLocalManager;
      }
      else
      {
        // find the ThreadLocalLifecycle for this threadLocalGroup by looking in
        // META-INF/Services, instantiate it and register ourselves with it so that we
        // can be cleaned up
        List<ThreadLocalLifecycle> lifecycles = ClassLoaderUtils.getServices(threadLocalGroup);
        
        if (!lifecycles.isEmpty())
        {
          lifecycles.get(0).init(threadLocalManager);
        }
      }
    }
    
    return threadLocalManager;
  }
  
  /**
   * ThreadLocalManager implementation class
   */
  private static class ResettableThreadLocalManager extends ThreadLocalManager
  {
    public ResettableThreadLocalManager()
    {
      // create the list of resettable ThreadLocals for this group
      _threadLocals = new ConcurrentLinkedQueue<WeakReference<ThreadLocal<?>>>();
    }
    
    /**
     * Registers the ThreadLocal for cleanup when this ThreadLocalManager is cleaned up
     * @param threadLocal ThreadLocal to register for clean up
     * @return The registered ThreadLocal
     */
    public <T> ThreadLocal<T> registerThreadLocal(ThreadLocal<T> threadLocal)
    {
      if (threadLocal == null)
        throw new NullPointerException();
      
      // WeakReference might be overkill here, but make sure we don't pin ThreadLocals
      _threadLocals.add(new WeakReference<ThreadLocal<?>>(threadLocal));
      
      return threadLocal;
    }
    
    /**
     * Called by the ThreadLocalLifecycle to clean up all of the ThreadLocals registered
     * with this ThreadLocalManager
     */
    public void removeThreadLocals()
    {
      Iterator<WeakReference<ThreadLocal<?>>> iterator = _threadLocals.iterator();
      
      while (iterator.hasNext())
      {
        ThreadLocal<?> threadLocal = iterator.next().get();
        
        // if the threadLocal is null, that means it has been released and we would really
        // like to reclaim the entry, however remove isn't supported on CopyOnWriteArrayLists
        // and the synchronization required to safely remove this item probably isn't
        // worthy the small increase in memory of keeping around this empty item, so we don't
        // bother cleaning up this entry
        if (threadLocal != null)
        {
          // reset the threadlocal for this thread
          threadLocal.remove();
        }
      }
    }
    
    private final Collection<WeakReference<ThreadLocal<?>>> _threadLocals;
  }

  // Name used for the request-scoped ThreadLocalManager
  private static final String _REQUEST_SERVICE = ThreadLocalUtils.class.getName() + 
                                                                    ".ThreadLocalLifecycle.REQUEST";
  
  // threadLocalGroup -> ThreadLocalManager Map
  private static final ConcurrentMap<String, ResettableThreadLocalManager>_threadLocalManagers
                     = new ConcurrentHashMap<String, ResettableThreadLocalManager>();
}
