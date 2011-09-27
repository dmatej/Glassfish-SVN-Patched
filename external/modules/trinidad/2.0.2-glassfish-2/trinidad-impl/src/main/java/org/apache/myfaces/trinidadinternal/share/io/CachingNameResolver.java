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
package org.apache.myfaces.trinidadinternal.share.io;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import java.util.Hashtable;
import java.util.Iterator;

import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;


/**
 * NameResolver that adds caching support.  Providers
 * that receive cached results (via setCachedResult) will
 * be stored in a dictionary for later retrieval.
 * <p>
 * @see InputStreamProvider#setCachedResult
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/io/CachingNameResolver.java#0 $) $Date: 10-nov-2005.19:00:06 $
 */
public class CachingNameResolver implements NameResolver
{
  /**
   * Creates a caching name resolver.
   * @param base a base NameResolver to wrap
   * @param storage the Map to store results, or null
   *           to use a default cache.
   * @param checkModified if true, last-modified times
   *         will be checked on every request
   */
  public CachingNameResolver(
    NameResolver base,
    Map<Object, InputStreamProvider> storage,
    boolean      checkModified)
  {
    this(base,
         storage,
         checkModified ? _DEFAULT_MILLISECONDS_BETWEEN_CHECKS :
                         _DO_NOT_CHECK_MODIFIED);
  }

  /**
   * Creates a caching name resolver that will check modification times
   * on each request, with the ability to override the default amount of time
   * that must go by before a modification time is rechecked.
   *
   * @param base a base NameResolver to wrap
   * @param storage the Map to store results, or null
   *           to use a default cache.
   * @param msBetweenChecks The number of milliseconds required to elapse
   *                        before checking the modification time again.
   */
  public CachingNameResolver(
    NameResolver base,
    Map<Object, InputStreamProvider> storage,
    int          msBetweenChecks)
  {
    if (base == null)
      throw new NullPointerException();

    _base = base;
    if (storage == null)
      storage = new Hashtable<Object, InputStreamProvider>(197);

    _cachedFiles = storage;

    // Any negative value other than DO_NOT_CHECK_MODIFIED is invalid, so use
    // the default in that case
    if ((msBetweenChecks == _DO_NOT_CHECK_MODIFIED) || (msBetweenChecks >= 0))
      _msBetweenChecks = msBetweenChecks;
    else
      _msBetweenChecks = _DEFAULT_MILLISECONDS_BETWEEN_CHECKS;
  }

  /**
   * Given a name, returns an InputStreamProvider.  This
   * function should never return null - if the target
   * cannot be resolved, throw an IOException.
   * @param name the name of the target
   */
  public InputStreamProvider getProvider(String name) throws IOException
  {
    InputStreamProvider provider = _base.getProvider(name);
    InputStreamProvider cached = _getCachedProvider(provider.getIdentifier());
    if (cached != null)
      return cached;

    return new CachingProvider(provider, _cachedFiles, _msBetweenChecks);
  }

  /**
   * Return the new NameResolver that should be used to resolve
   * names relative to a given name.  This function should never
   * return null - if the target cannot be resolved, return a
   * resolver that can only support absolute names.
   * @param name the name of the target
   */
  public NameResolver getResolver(String name)
  {
    NameResolver resolver = _base.getResolver(name);
    return new CachingNameResolver(resolver,
                                   _cachedFiles,
                                   _msBetweenChecks);
  }

  @Override
  public String toString()
  {
    return super.toString() + "[" + _base.toString() + "]";
  }

  private boolean _checkModified()
  {
    return _msBetweenChecks != _DO_NOT_CHECK_MODIFIED;
  }

  private InputStreamProvider _getCachedProvider(Object o)
  {
    // We don't require the storage to be synchronized
    synchronized (_cachedFiles)
    {
      InputStreamProvider provider = _cachedFiles.get(o);
      if ((provider != null) && _checkModified())
      {
        if (provider.hasSourceChanged())
        {
          _cachedFiles.remove(o);
          provider = null;
        }
      }

      return provider;
    }
  }

  static void __addToCache(
      CachingProvider provider, 
      Map<Object, InputStreamProvider> storage)
  {
    // We don't require the storage to be synchronized
    synchronized (storage)
    {
      storage.put(provider.getIdentifier(), provider);
    }
  }


  // InputStreamProvider implementation
  // DO NOT make non-static: CachingProvider objects are non-transient,
  // but CachingNameResolvers explicitly must be (see bug 2170236)
  static private class CachingProvider extends InputStreamProviderProxy
                                       implements CachingInputStreamProvider
  {
    public CachingProvider(InputStreamProvider wrapped,
                           Map<Object, InputStreamProvider> storage,
                           int msBetweenChecks)
    {
      _wrapped = wrapped;
      _storage = storage;
      _msBetweenChecks = msBetweenChecks;
    }

    @Override
    public void setCachedResult(Object value)
    {
      _lastChecked  = System.currentTimeMillis();
      if (value != null)
        __addToCache(this, _storage);
      super.setCachedResult(value);
    }

    // Check not just whether we've changed, but whether any
    // of our dependencies have.
    @Override
    public boolean hasSourceChanged()
    {
      // Don't check unless at least _msBetweenChecks has passed
      long currentTime = System.currentTimeMillis();
      if ((currentTime - _lastChecked) < _msBetweenChecks)
        return false;

      // However, if in checking we discover that the file _has_
      // changed, then don't bother updating "_lastChecked".  This
      // same provider may have been included from other sources,
      // and we want those other sources to be able to immediately
      // discover the file has changed.
      boolean changed = super.hasSourceChanged();
      if (changed)
        return true;

      ArrayList<InputStreamProvider> dependencies = _dependencies;
      if (dependencies != null)
      {
        for (int i = dependencies.size() - 1; i >= 0; i--)
        {
          if (dependencies.get(i).hasSourceChanged())
          {
            return true;
          }
        }
      }

      _lastChecked = currentTime;
      return false;
    }

    public void addCacheDependency(InputStreamProvider dependency)
    {
      // Catch any obviously circular dependencies
      if ((dependency == this) ||
                         (dependency == _wrapped))
                         {
                           throw new IllegalArgumentException("Circular dependency - dependency");
                         }

      if (dependency == null)
        return;

      if (_dependencies == null)
        _dependencies = new ArrayList<InputStreamProvider>(5);
      _dependencies.add(dependency);
    }

    public Iterator<InputStreamProvider> getCacheDependencies()
    {
      ArrayList<InputStreamProvider> dependencies = _dependencies;
      if (dependencies == null)
        return null;

      return dependencies.iterator();
    }

    @Override
    protected InputStreamProvider getProvider()
    {
      return _wrapped;
    }

    private ArrayList<InputStreamProvider>  _dependencies;
    private Map<Object, InputStreamProvider> _storage;
    private final InputStreamProvider _wrapped;
    private long       _lastChecked  = -1;
    private int       _msBetweenChecks;
  }

  private NameResolver   _base;
  private Map<Object, InputStreamProvider> _cachedFiles;

  // this param is set to _DO_NOT_CHECK_MODIFIED to indicate not to check at all
  private int            _msBetweenChecks;


  // Only check a given files modification date once every 1000
  // milliseconds.  Ideally, this wouldn't be hardwired so deeply.
  // This value should be as high as possible without disturbing
  // developers - that is, how fast can a developer view a page,
  // save a changed page and hit reload?
  static private final int _DEFAULT_MILLISECONDS_BETWEEN_CHECKS = 1000;

  /**
   * Value for the msBetweenChecks parameter that means "don't check
   * the modification time at all.
   */
  static private final int _DO_NOT_CHECK_MODIFIED = -1;
}
