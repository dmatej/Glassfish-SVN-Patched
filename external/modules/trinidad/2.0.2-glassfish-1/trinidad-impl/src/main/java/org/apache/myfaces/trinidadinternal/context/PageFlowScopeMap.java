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
package org.apache.myfaces.trinidadinternal.context;

import java.io.Serializable;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.util.SubKeyMap;
import org.apache.myfaces.trinidadinternal.util.TokenCache;

/**
 *
 */
class PageFlowScopeMap implements Map<String, Object>, Serializable
{
  /**
   * Return a PageFlowScopeMap stored with a token.
   */
  static public PageFlowScopeMap getPageFlowScopeMap(
    FacesContext context, String token, int lifetime)
  {
    TokenCache cache = _getRootTokenCache(context, lifetime);
    PageFlowScopeMap map = _getPageFlowScopeMap(context, cache, token);

    if (_LOG.isFine())
    {
      _LOG.fine("pageFlowScope: found map {0} at token {1}",
                new Object[]{(map == null) ? (Object) "null" : map, token});
    }

    if (map == null)
    {
      return null;
    }
    else
    {
      // Don't return the same instance of PageFlowScopeMap as was used
      // on the previous page;  otherwise, for instance, we'll overwrite
      // its token as we mutate.  Instead, create a new PageFlowScopeMap,
      // but reuse the _map;  we'll clone the _map itself if we mutate
      return new PageFlowScopeMap(map._map,
                                 token,
                                 map._sharedData);
    }
  }

  static private PageFlowScopeMap _getPageFlowScopeMap(
    FacesContext context, TokenCache cache, String token)
  {
    if (token == null)
      throw new NullPointerException();

    int lastSeparator = token.lastIndexOf(TokenCache.SEPARATOR_CHAR);
    String parentToken;
    String childToken;
    if (lastSeparator < 0)
    {
      parentToken = null;
      childToken = token;
    }
    else
    {
      parentToken = token.substring(0, lastSeparator);
      childToken = token.substring(lastSeparator + 1);
    }

    Map<String, Object> storeMap = _createMapToStore(context, parentToken);
    return (PageFlowScopeMap) storeMap.get(childToken);
  }

  /**
   * Only for serialization
   */
  public PageFlowScopeMap()
  {
  }


  public PageFlowScopeMap(int lifetime)
  {
    this(new HashMap<String, Object>(13), null, new SharedData(lifetime));
  }

  private PageFlowScopeMap(
    HashMap<String, Object> map,
    String token,
    SharedData sharedData)
  {
    _map = map;
    _sharedData = sharedData;
    _token = token;
  }

  //
  // Create a PageFlowScopeMap pointing at a parent
  //
  private PageFlowScopeMap(PageFlowScopeMap parent, boolean copyParent)
  {
    assert(parent != null);

    _sharedData = new SharedData(parent._sharedData._lifetime);
    _sharedData._parent = parent;

    _map = new HashMap<String, Object>();
    if (copyParent)
      _map.putAll(parent._map);
  }


  public PageFlowScopeMap getParent()
  {
    return _sharedData._parent;
  }

  synchronized public String getToken(FacesContext context)
  {
    if (_token != null)
      return _token;

    // Don't need a token when nothing's in the map, and we
    // don't have a parent
    if (isEmpty() &&
        (_sharedData._children == null) &&
        (_sharedData._parent == null))
      return null;

    String parentToken;
    TokenCache cache;
    if (_sharedData._parent != null)
    {
      parentToken = _sharedData._parent.getToken(context);
      cache = _sharedData._parent._getTokenCache();
    }
    else
    {
      parentToken = null;
      cache = _getRootTokenCache(context, _sharedData._lifetime);
    }

    Map<String, Object> store = _createMapToStore(context, parentToken);

    String token = cache.addNewEntry(this, store);

    if (parentToken != null)
      token = parentToken + TokenCache.SEPARATOR_CHAR + token;

    _token = token;

    // With a new token, there cannot be any shared children
    // with a prior request.
    if (_sharedData._children != null)
    {
      // =-=AEW NEED TO CLONE SHARED DATA
      _LOG.fine("Discarding child PageFlowScopes; new token is {0}", token);
      _sharedData._children = null;
    }

    return _token;
  }

  @SuppressWarnings("unchecked")
  static private Map<String, Object> _createMapToStore(
    FacesContext context,
    String       parentToken)
  {
    String fullToken;
    if (parentToken == null)
    {
      fullToken = _PAGE_FLOW_SCOPE_CACHE + TokenCache.SEPARATOR_CHAR;
    }
    else
    {
      fullToken = (_PAGE_FLOW_SCOPE_CACHE + TokenCache.SEPARATOR_CHAR +
                    parentToken + TokenCache.SEPARATOR_CHAR);

    }

    return new SubKeyMap(context.getExternalContext().getSessionMap(),
                         fullToken);
  }

  @Override
  public boolean equals(Object o)
  {
    if (o instanceof PageFlowScopeMap)
      o = ((PageFlowScopeMap) o)._map;

    return _map.equals(o);
  }

  @Override
  public int hashCode()
  {
    return _map.hashCode();
  }

  public int size()
  {
    return _map.size();
  }

  public boolean isEmpty()
  {
    return _map.isEmpty();
  }

  public boolean containsKey(Object key)
  {
    return _map.containsKey(key);
  }

  public boolean containsValue(Object value)
  {
    return _map.containsValue(value);
  }

  public Collection<Object> values()
  {
    // Use an unmodifiableCollection to save me the headache
    // of catching mutations
    return Collections.unmodifiableCollection(_map.values());
  }


  public Set<Map.Entry<String, Object>> entrySet()
  {
    // Use an unmodifiableSet to save me the headache
    // of catching mutations
    return Collections.unmodifiableSet(_map.entrySet());
  }

  public Set<String> keySet()
  {
    // Use an unmodifiableSet to save me the headache
    // of catching mutations
    return Collections.unmodifiableSet(_map.keySet());
  }

  public Object get(Object key)
  {
    return _map.get(key);
  }

  public Object put(String key, Object value)
  {
    _detachIfNeeded();
    if (_LOG.isFine())
    {
      _LOG.fine("pageFlowScope: put({0}, {1})", new Object[]{key, value});
    }

    return _map.put(key, value);
  }

  public Object remove(Object key)
  {
    _detachIfNeeded();
    if (_LOG.isFine())
    {
      _LOG.fine("pageFlowScope: remove({0})", key);
    }

    return _map.remove(key);
  }

  public void putAll(Map<? extends String, ? extends Object> t)
  {
    _detachIfNeeded();
    if (_LOG.isFine())
    {
      _LOG.fine("pageFlowScope: putAll({0})", t);
    }
    _map.putAll(t);
  }

  public void clear()
  {
    _detachIfNeeded();
    if (_LOG.isFine())
    {
      _LOG.fine("pageFlowScope: clear()");
    }
    _map.clear();
  }


  public PageFlowScopeMap createChild(boolean copyParent)
  {
    return new PageFlowScopeMap(this, copyParent);
  }

  public void discard()
  {
    FacesContext context = FacesContext.getCurrentInstance();

    String token = getToken(context);
    int lastSeparator = token.lastIndexOf(TokenCache.SEPARATOR_CHAR);

    String parentToken;
    String childToken;
    if (lastSeparator < 0)
    {
      parentToken = null;
      childToken = token;
    }
    else
    {
      parentToken = token.substring(0, lastSeparator);
      childToken = token.substring(lastSeparator + 1);
    }

    // Remove ourselves
    if (_sharedData._parent != null)
    {
      Map<String, Object> storeMap = _createMapToStore(context, parentToken);
      _sharedData._parent._sharedData._children.removeOldEntry(childToken,
                                                               storeMap);
    }

    // And clean up all of our children
    _removeAllChildren(context, token);
  }

  private void _removeAllChildren(FacesContext context, String token)
  {
    // Clear everything - note that because of naming conventions,
    // this will in fact automatically recurse through all children
    // grandchildren etc. - which is kind of a design flaw of SubKeyMap,
    // but one we're relying on
    Map<String, Object> store = _createMapToStore(context, token);
    store.clear();
    _sharedData._children = null;
  }

  @Override
  public String toString()
  {
    return "PageFlowScopeMap@" + System.identityHashCode(this) +
           "[_map=" + _map + ", _token=" + _token +
           ",_children=" + _sharedData._children + "]";
  }

  static private TokenCache _getRootTokenCache(FacesContext context,
                                               int lifetime)
  {
    return TokenCache.getTokenCacheFromSession(context.getExternalContext(),
                                               _PAGE_FLOW_SCOPE_CACHE,
                                               true,
                                               lifetime);
  }

  private TokenCache _getTokenCache()
  {
    if (_sharedData._children == null)
      _sharedData._children = new TokenCache(_sharedData._lifetime);

    return _sharedData._children;
  }

  // =-=AEW This strategy assumes that the PageFlowScopeMap
  // will be inherited from a prior request, have things
  // added and removed prior to Render Response *without
  // the token being requested*, then have the token used
  // repeatedly during Render Response *without further
  // mutations*.  Both of these assumptions seem very
  // dubious!
  @SuppressWarnings("unchecked")
  private void _detachIfNeeded()
  {
    if (_token != null)
    {
      _map = (HashMap<String, Object>) _map.clone();
      _token = null;

      // =-=AEW When do we discard children?
    }
  }

  static public class SharedData implements Serializable
  {
    public SharedData()
    {
    }

    public SharedData(int lifetime)
    {
      _lifetime = lifetime;
    }

    private int             _lifetime;
    // =-=AEW Make transient for efficiency
    private PageFlowScopeMap _parent;
    private TokenCache      _children;
    private static final long serialVersionUID = 1L;
  }

  // DELETE AFTER DIALOG SERVICE IS CLEANED UP
  boolean    __invalid;

  private SharedData _sharedData;
  private String     _token;
  private HashMap<String, Object> _map;

  private static final String _PAGE_FLOW_SCOPE_CACHE =
    "org.apache.myfaces.trinidadinternal.application.PageFlowScope";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PageFlowScopeMap.class);
  private static final long serialVersionUID = 1L;
}
