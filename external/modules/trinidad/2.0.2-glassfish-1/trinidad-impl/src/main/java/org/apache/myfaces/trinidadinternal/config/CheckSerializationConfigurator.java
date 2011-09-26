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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.lang.reflect.InvocationHandler;

import java.lang.reflect.Method;

import java.lang.reflect.Proxy;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.faces.context.ExternalContext;
import javax.faces.context.ExternalContextWrapper;

import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpSession;

import javax.servlet.http.HttpSessionContext;

import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.CollectionUtils;
import org.apache.myfaces.trinidad.util.CollectionUtils.MapMutationHooks;
import org.apache.myfaces.trinidad.util.TransientHolder;

import org.apache.myfaces.trinidadinternal.context.external.ServletApplicationMap;
import org.apache.myfaces.trinidadinternal.util.ObjectInputStreamResolveClass;

/**
 * Configurator that uses both wrapped ExternalContext (for Portlet cases) and wrapped
 * ServletContext and HttpSession (for HttpServlet cases) to validate that
 * only Serializable Objects are placed in the Sesssion Map and that mutations to the
 * Session and ApplicationMap content dirty the entries.
 * @version $Revision$ $Date$
 */
public final class CheckSerializationConfigurator extends Configurator
{

  /**
   * Override to return our ExternalContext wrapped if session serialization checking is enabled
   * @param extContext
   * @return
   */
  @Override
  public ExternalContext getExternalContext(ExternalContext extContext)
  {
    // retrieve our wrapped ExternalContext, creating it if necessary.  We wrap the external
    // context to trap calls to retrieve the Session and Application Maps in the portal case.
    // For the HttpServlet case, we rely on 
    ExternalContext checkingContext = 
                             SerializationCheckingWrapper.getSerializationWrapper(extContext, true);
    
    if (checkingContext != null)
    {
      return checkingContext;
    }
    else
    {
      return extContext;
    }
  }
  
  /**
   * Check if any of the non-dirtied checked managed beans have been mutated in this request
   * @param extContext
   */
  @Override
  public void endRequest(ExternalContext extContext)
  {
    // get the wrapper without creating it if it doesn't already exist
    SerializationCheckingWrapper checkingWrapper =
                          SerializationCheckingWrapper.getSerializationWrapper(extContext, false);

    
    if (checkingWrapper != null)
    {
      checkingWrapper.checkForMutations();
    }
  }

  /**
   * Returns the FilterConfig to use for initializing the filters so that we can wrap it
   * if necessary
   * @param filterConfig
   * @return
   */
  public static FilterConfig getFilterConfig(FilterConfig filterConfig)
  {
    // skankily don't pass the ExternalContext since we don't have one when the filters are
    // initialized.  This only really works because checkApplicvationSerialization doesn't
    // need the ExternalContext
    if (StateUtils.checkApplicationSerialization(null))
    {
      return new FilterConfigWrapper(filterConfig);
    }
    else
    {
      return filterConfig;
    }
  }

  /**
   * Returns the HttpServletRequest to use for this request, so that we can wrap it if
   * necessary.
   * @param extContext
   * @param request
   * @return
   */
  public static HttpServletRequest getHttpServletRequest(
    ExternalContext    extContext,
    HttpServletRequest request)
  {
    SerializationChecker checker = SerializationChecker.getSerializationChecker(extContext, true);
    
    if (checker != null)
    {
      return checker.getWrappedRequest(request);
    }
    else
    {
      return request;
    }
  }
  
  
  /**
   * Unregisters the checking of the specified session attribute
   * @param external ExternalContext
   * @param key      Name of session attribute to unregister
   */
  public static void unregisterSessionAttribute(ExternalContext external, String key)
  {
    SerializationChecker checker = SerializationChecker.getSerializationChecker(external, false);
    
    if (checker != null)
    {
      checker.unregisterSessionAttribute(external, key);
    }    
  }

  /**
   * Unregisters the checking of the specified application attribute
   * @param external ExternalContext
   * @param key      Name of session attribute to unregister
   */
  public static void unregisterApplicationAttribute(ExternalContext external, String key)
  {
    SerializationChecker checker = SerializationChecker.getSerializationChecker(external, false);
    
    if (checker != null)
    {
      checker.unregisterApplicationAttribute(external, key);
    }    
  }

  /**
   * Returns the list of in-flight MutatedBeanCheckers for the specified Map and its associated
   * lock object
   * @param checkedMap   Map checked by the MutatedBeanCheckers
   * @param mapWriteLock Lock object to synchronize on when mutatating the amp
   * @return
   */
  private static List<MutatedBeanChecker> _getMutatedBeanList(
    Map<String, Object> checkedMap,
    Object              mapWriteLock)
  {
    Object holder = checkedMap.get(_CHECKED_MAPS_KEY);
    
    TransientHolder<List<MutatedBeanChecker>> listHolder;
    List<MutatedBeanChecker> mutatedBeans = null;
    
    if (holder != null)                                         
    {
      listHolder = (TransientHolder<List<MutatedBeanChecker>>)holder;
      
      mutatedBeans = listHolder.getValue();
    }
                                                 
    if (mutatedBeans == null)
    {
      // make sure that the list is only created once per map
      synchronized(mapWriteLock)
      {
        // check again in case we value was written by the previous lock holder
        holder = checkedMap.get(_CHECKED_MAPS_KEY);

        if (holder != null)                                         
        {
          listHolder = (TransientHolder<List<MutatedBeanChecker>>)holder;
          
          mutatedBeans = listHolder.getValue();
        }
        
        if (mutatedBeans == null)
        {
          // mutations to the list itself need to be thread-safe
          mutatedBeans = new CopyOnWriteArrayList<MutatedBeanChecker>();
          
          // use a TransientHolder since we don't care if this gets failed over
          checkedMap.put(_CHECKED_MAPS_KEY, TransientHolder.newTransientHolder(mutatedBeans));
        }
      }
    }
    
    return mutatedBeans;
  }

  /**
   * Remove the modified key from all of the in-flight bean checking requests
   * @param beanCheckers
   * @param key
   */
  private static void _notifyBeanCheckersOfChange(
    List<MutatedBeanChecker> beanCheckers,
    Object                   key)
  {
    for (MutatedBeanChecker beanChecker : beanCheckers)
    {
      beanChecker._unmutatedKeyValues.remove(key);
    }
  }

  /**
   * Serialize an object and return the byte[]
   * @param objectToSerialize
   * @return
   * @throws IOException
   */
  private static byte[] _serialize(Object objectToSerialize) throws IOException
  {
    ByteArrayOutputStream outputByteStream = new ByteArrayOutputStream();
    
    new ObjectOutputStream(outputByteStream).writeObject(objectToSerialize);
    
    return outputByteStream.toByteArray();    
  }

  /**
   * Extract the detailed messages for a serialization failure
   * @param keyMessage
   * @param value
   * @return
   */
  private static String _extractFailureString(String keyMessage, Object value)
  {
    List<Object> failureStack = new ArrayList<Object>();
    
    failureStack.add(keyMessage);
    failureStack.add(value);
    
    _extractFailure(failureStack, value);
    
    String joinMessage = _LOG.getMessage("EXTRACT_SERIALIZATION_DETAIL");
    
    StringBuilder message = new StringBuilder();
    
    Iterator<Object> failures = failureStack.iterator();
    
    do
    {
      String keyInfo = (String)failures.next();
      Object failedValue = failures.next();
      
      message.append(keyInfo);
      message.append(joinMessage);
      message.append(failedValue);
      
      if (failures.hasNext())
        message.append(", ");
      else
        break;
      
    } while (true);
    
    return message.toString();
  }

  /**
   * Extract the detailed causes of a serialization failure onto the failureStack
   * @param failureStack
   * @param failedObject
   */
  private static void _extractFailure(
    List<Object> failureStack,
    Object       failedObject)
  {
    if (failedObject instanceof Map)
    {
      _MAP_EXTRACTOR.extractFailure(failureStack, (Map<Object, Object>)failedObject);
    }
    else if (failedObject instanceof List)
    {
      _LIST_EXTRACTOR.extractFailure(failureStack, (List<Object>)failedObject);
    }
    else if (failedObject instanceof Collection)
    {
      _COLLECTION_EXTRACTOR.extractFailure(failureStack, (List<Object>)failedObject);
    }
    else if (failedObject instanceof Object[])
    {
      _ARRAY_EXTRACTOR.extractFailure(failureStack, (Object[])failedObject);
    }
  }
  
  private static ServletContext _getServletContextProxy(ServletContext orig, Map<String,Object> map)
  {
    return (ServletContext) Proxy.newProxyInstance(ClassLoaderUtils.getContextClassLoader(),
                                                         new Class[] {ServletContext.class},
                                                         new ServletContextMonitorInvocationHandler(orig, map));
  }

  /**
   * Wraps the FilterConfig so that we can wrap the ServletContext that it returns so that
   * we can trap calls the setting and removing ServletContext attributes.  Phew!
   */
  private static class FilterConfigWrapper implements FilterConfig
  {
    FilterConfigWrapper(FilterConfig filterConfig)
    {
      _delegate = filterConfig;
      
      // create ServletContext wrapper to catch sets and removes from the ServletContext
      _wrappedContext = _getServletContextProxy(filterConfig.getServletContext(), null);
    }
    
    public String getFilterName()
    {
      return _delegate.getFilterName();
    }

    public ServletContext getServletContext()
    {
      return _wrappedContext;
    }

    public String getInitParameter(String paramName)
    {
      return _delegate.getInitParameter(paramName);
    }

    public Enumeration getInitParameterNames()
    {
      return _delegate.getInitParameterNames();
    }
    
    private final FilterConfig   _delegate;
    private final ServletContext _wrappedContext;
  }


  /**
   * ExternalContextWrapper that returns wrapped versions of the Session and ApplicationMaps that
   * we can track changes to.  This is needed for the Portlet case.  For the HttpServel case,
   * it is redundant with the ServletContext and Session wrapping but shouldn't do any harm.
   */
  private static class SerializationCheckingWrapper extends ExternalContextWrapper
  {
    /**
     * Retrieves the current SerializationCheckingWrapper for this request
     * @param extContext
     * @param create If <code>true</code>, create the SerializationCheckingWrapper for this
     *               request if it doesn't already exist.
     * @return 
     */
    public static SerializationCheckingWrapper getSerializationWrapper(
      ExternalContext extContext,
      boolean         create)
    {
      // get the SerializationCheckingWrapper for this request
      Map<String, Object> requestMap = extContext.getRequestMap();
      
      Object wrapper = requestMap.get(_SERIALIZATION_WRAPPER_KEY);
      
      if (wrapper != null)
      {
        return (SerializationCheckingWrapper)wrapper;
      }
      else if (create)
      {
        // create the wrapper for this request and store it on the request so that we don't
        // recreate it
        SerializationChecker checker = SerializationChecker.getSerializationChecker(extContext,
                                                                                    create);
        
        if (checker != null)
        {
          SerializationCheckingWrapper checkingWrapper = new SerializationCheckingWrapper(extContext,
                                                                                          checker);

          requestMap.put(_SERIALIZATION_WRAPPER_KEY, checkingWrapper);
       
          return checkingWrapper;
        }
      }
      
      return null;
    }

    /**
     * Create a SerializationCheckingWrapper
     * @param extContext ExternalContext to wrap
     * @param checker    SerializationChecker to call back on mutations
     */
    private SerializationCheckingWrapper(
      ExternalContext      extContext,
      SerializationChecker checker)
    {
      _extContext = extContext;
      _checker    = checker;
    }

    /**
     * Check for mutations to beans trancked by the SerializationCheckingWrapper
     */
    public void checkForMutations()
    {
      _checker.checkForMutations();
    }

    @Override
    public ExternalContext getWrapped()
    {
      return _extContext;
    }

    /**
     * Override to delegate to the SerializationChecker
     * @return
     */
    @Override
    public Map<String, Object> getSessionMap()
    {
      return _checker.getSessionMap();
    }

    /**
     * Override to delegate to the SerializationChecker
     * @return
     */
    @Override
    public Map<String, Object> getApplicationMap()
    {
      return _checker.getApplicationMap();
    }

    private static final String _SERIALIZATION_WRAPPER_KEY = 
                                         CheckSerializationConfigurator.class.getName() + "#WRAPPER";
    
    private final ExternalContext      _extContext;
    private final SerializationChecker _checker;
  }

  /**
   * Checks a Map for mutations to the contents of its Serializable attributes that weren't
   * dirtied in the current request.
   * The mutations are checked by snapshotting the serialized bytes of the current Serializable
   * values of the Map at the beginning of the request.  We then catch all puts and removes to
   * the Map across any request and remove those entries, since they have been correctly dirtied.
   * At the end of the request. <code>checkForMutations</code> is called and we iterate over
   * the remaining entries and compared their serialized bytes against the serialized bytes
   * of the current values.  If the bytes are different, we assume that some part of the
   * attribute's object subtree has been changed without appropriately dirtying it and we
   * log an errror.
   * @see #checkForMutations
   */
  private static class MutatedBeanChecker implements MapMutationHooks<String, Object>
  {
    /**
     * Creates a MutatedBeanChecker to check for undirtied mutations to the Serialized state
     * of a Map in the current request.
     * @param checkedMap Map containing attributes to check for mutations
     * @param mapName Name of map used when logging
     * @param mapLock Lock to use when mutating the map
     * @param requireSerialization <code>true</code> if all of the attributes are required to be
     *                             Serializable (Sesssion Map attributes are.  Application Map
     *                             aren't)
     */
    public MutatedBeanChecker(
      Map<String, Object> checkedMap,
      String              mapName,
      Object              mapLock,
      boolean             requireSerialization)
    {
      _checkedMap           = checkedMap;
      _mapName              = mapName;
      _mapLock              = mapLock;
            
      // snapshot the initial serialized bytes of the mutable values in the Map so that we
      // can compare them at the end of the request to see if they have changed
      _unmutatedKeyValues = new ConcurrentHashMap<String, Object>(checkedMap.size() * 2);
      _unmutatedKeyValues.putAll(checkedMap);
      
      // register this request with the list of checkers for this map.
      // I think there is a possible race condition between the keys being added to the map and
      // our registering ourselves as listening for puts and removes made by other requests.
      // This makes me sad
      _getMutatedBeanList().add(this);
      
      // loop through the map's valid keys getting the serialized bytes
      for (String key : _unmutatedKeyValues.keySet())
      {
        Object value = checkedMap.get(key);
        
        // get the serialized bytes for this key.  If the key's vale isn't serializable or is
        // immutable, this will be an empty array and we won't need to check it
        byte[] serializedBytes = _getSerializedValue(key, value, requireSerialization);
        
        if (serializedBytes.length > 0)
        {
          // save the bytes for comparing at the end of the request
          _unmutatedKeyValues.put(key, serializedBytes);
        }
        else
        {
          // either not serializable or immutable, so we don't need to worry about checking it
          _unmutatedKeyValues.remove(key);
        }
      }
    }

    /**
     * Unregisters the checking of the specified attribute
     * @param key      Name of session attribute to unregister
     */
    public void unregisterAttribute(String key)
    {
      CheckSerializationConfigurator._notifyBeanCheckersOfChange(_getMutatedBeanList(), key);
    }
    
    /** Implement to catch writes to the Map, since that means that the Map has been dirtied */
    @Override
    public void writeNotify(Map<String, Object> map, String key, Object value)
    {
      unregisterAttribute(key);
    }

    /** Implement to catch remove from the Map, since that means the attribute won't
     *  be failed over
     */
    @Override
    public void removeNotify(Map<String, Object> map, Object key)
    {      
      unregisterAttribute((String)key);
    }

    /** Implement to catch clear of Map, since that means the attributes won't
     *  be failed over
     */
    @Override
    public void clearNotify(Map<String, Object> map)
    {
      // clear all of the keys across all of the beans      
      for (String key : map.keySet())
      {
        unregisterAttribute(key);
      }
    }
    
    /**
     * Clear all of the values we were checking.  This is called if the Session is invalidated,
     * for example.
     */
    public void clearCheckedValues()
    {
      for (MutatedBeanChecker beanChecker : _getMutatedBeanList())
      {
        beanChecker._unmutatedKeyValues.clear();
      }
    }
    
    /**
     * Check for any undirtied mutations of this map, logging severe messages if there are
     */
    public void checkForMutations()
    {
      // loop through the unmodified items in the Map and verify that the curent
      // Serialized values haven't changed
      for (Map.Entry<String, Object> checkedEntry : _unmutatedKeyValues.entrySet())
      {
        String key = checkedEntry.getKey();
        
        Object currValue = _checkedMap.get(key);
        byte[] currentBytes = _getSerializedValue(key, currValue, false);
        byte[] oldBytes     = (byte[])checkedEntry.getValue();
        
        // check if the bytes are different
        if (!Arrays.equals(oldBytes, currentBytes))
        {
          // deserialize the original object so we can dump it out (hopefully it has a
          // good toString())  We also do this so we can 
          Object oldValue = _deserializeObject(oldBytes);
          
          // deserialize the new bytes so that we are comparing two deserialized objects
          Object newValue = _deserializeObject(currentBytes);
          
          // This doesn't do anything, but is a handy comparison of debugging when things go awry
          oldValue.equals(newValue);
          
          // build up the message 
          String message = _LOG.getMessage("SERIALIZABLE_ATTRIBUTE_MUTATED",
                                           new Object[]{_mapName, key, oldValue, newValue});
         
          // Log message because user might not notice exception since we are at the end of the
          // request
          _LOG.severe(message);
        }
      }
      
      // we no longer need to track changes to the map
      _getMutatedBeanList().remove(this);
    }
    

    /**
     * Given the serialized bytes of an Object, return the Object itself
     * @param serializedBytes
     * @return
     */
    private Object _deserializeObject(byte[] serializedBytes)
    {
      Object deserializedObject;
      
      try
      {
        // copy the bytes before passing to the ByteArrayInputStream, since it mutates
        // the array
        byte[] copyBytes = Arrays.copyOf(serializedBytes, serializedBytes.length);
        
        ByteArrayInputStream baos = new ByteArrayInputStream(copyBytes);
        ObjectInputStream ois = new ObjectInputStreamResolveClass(baos);
        deserializedObject = ois.readObject();
        ois.close();
      }
      catch (IOException e)
      {
        throw new IllegalArgumentException(e);
      }
      catch (ClassNotFoundException e)
      {
        throw new IllegalArgumentException(e);
      }
      
      return deserializedObject;
    }
    
    /**
     * Returns the List of MutatedBeanCheckers across all in flight requests listening for changes
     * to this Map
     * @return
     */
    private List<MutatedBeanChecker> _getMutatedBeanList()
    {
      return CheckSerializationConfigurator._getMutatedBeanList(_checkedMap, _mapLock);
    }
    
    /**
     * Returns the serialized value of the object as a byte[] or an empty array if the object is
     * immutable and therefore doesn't need to be checked
     * @param key   Key in map to get the serialized value of
     * @param value Value in map to serialize
     * @param requireSerialization <code>true</code> if this value is required to be serializable
     * @return The serialized bytes for the Object if serializable and mutable
     * @throws IllegalStateException if the Object is not serializable or serialization fails
     */
    private byte[] _getSerializedValue(String key, Object value, boolean requireSerialization)
    {
      if (value == null)
        return _EMPTY_BYTE_ARRAY;
      
      Class valueClass = value.getClass();
      
      // check against the list of classes to ignore for performance reasons
      if (_INGNORE_CLASS_NAMES.contains(valueClass.getName()))
        return _EMPTY_BYTE_ARRAY;
      
      if (!(value instanceof Serializable))
      {
        if (requireSerialization)
        {
          String message = _LOG.getMessage("ATTRIBUTE_NOT_SERIALIZABLE",
                                           new Object[]{_mapName, key, valueClass});
      
          _LOG.severe(message);
        }
        
        return _EMPTY_BYTE_ARRAY;
      }
      
      
      // verify that the contents of the value are in fact Serializable
      try
      {
        return _serialize(value);
      }
      catch (Throwable e)
      {
        if (requireSerialization)
        {
          String baseMessage = _LOG.getMessage("ATTRIBUTE_SERIALIZATION_FAILED_KEY_VALUE",
                                           new Object[]{_mapName, key});
          
          String message = _extractFailureString(baseMessage, value);
          
          _LOG.severe(message, e);            
        }

        return _EMPTY_BYTE_ARRAY;
      }      
    }

    private static final byte[] _EMPTY_BYTE_ARRAY = new byte[0];
    
    private static final Set<String> _INGNORE_CLASS_NAMES;
    
    static
    {
      // initialize Set of class names to ignore for Serialization tracking because they
      // are immutable or not serialiable
      String[] classNames = new String[]
      {
       "java.lang.Boolean",    // immutable
       "java.lang.Character",  // immutable
       "java.lang.Double",     // immutable
       "java.lang.Float",      // immutable
       "java.lang.Integer",    // immutable
       "java.lang.Long",       // immutable
       "java.lang.Short",      // immutable
       "java.lang.String",     // immutable
       "java.math.BigDecimal", // immutable
       "java.math.BigInteger", // immutable
       "org.apache.myfaces.trinidad.util.TransientHolder" // Not serializable
      };
        

      _INGNORE_CLASS_NAMES = new HashSet<String>();
      _INGNORE_CLASS_NAMES.addAll(Arrays.asList(classNames));        
    }
    
    private static final TrinidadLogger _LOG = 
                                   TrinidadLogger.createTrinidadLogger(MutatedBeanChecker.class);

    private final Map<String, Object> _unmutatedKeyValues;
    private final Map<String, Object> _checkedMap;
    private final String _mapName;
    private final Object _mapLock;
  }
  
  /**
   * Wraps the ServletContext so that we can catch modifications to the attributes.  This 
   * had to be changed to support a dynmaic proxy so that it could be use with both
   * Server 2.1 and Servlet 3.0 Specifications.
   */
  private static final class ServletContextMonitorInvocationHandler implements InvocationHandler
  {
    public ServletContextMonitorInvocationHandler(ServletContext context, Map<String, Object> appMap)
    {
      _delegate = context;
      
      // if we already have an Application Map, use it, otherwise create a wrapper around
      // the ServletContext
      if (appMap != null)
      {
        _applicationMap = appMap;
      }
      else
      {
        _applicationMap = new ServletApplicationMap(context);
      }
    }

    @Override
    public Object invoke(Object object, Method method, Object[] objects) throws Throwable
    {
      
      //Notifys the system that an attribute has changed on set or remove
      String name = method.getName();

      //This method delegates all methods to the delegate
      Object result = method.invoke(_delegate, objects);

      if("setAttribute".equals(name) || "removeAttribute".equals(name))
      {
        _notifyBeanCheckersOfChange(_getMutatedBeanList(), objects[0]);                
      }
          
      return result;
    }

    private List<MutatedBeanChecker> _getMutatedBeanList()
    {
      return CheckSerializationConfigurator._getMutatedBeanList(_applicationMap, _delegate);
    }

    private final ServletContext      _delegate;
    private final Map<String, Object> _applicationMap;

  }  

  /**
   * Performs any configured serialization checking of the Session or Application Maps including
   * whether the contents are Serializable and whether the contents have changed.
   */
  private static class SerializationChecker
  {
    /**
     * Get the current SerializaionChecker for this request, potentially creating one
     * @param extContext ExternalContext to use to create the SerializationChecker
     * @param create If <code>true</code> a SerializationChecker will be created and registered
     *               for this request if one does not alreadfy exist.
     * @return
     */
    public static SerializationChecker getSerializationChecker(
      ExternalContext extContext,
      boolean         create)
    {
      Map<String, Object> requestMap = extContext.getRequestMap();
      
      Object checker = requestMap.get(_SERIALIZATION_CHECKER_KEY);
      
      if (checker != null)
      {
        return (SerializationChecker)checker;
      }
      else if (create)
      {
        boolean checkSession = StateUtils.checkSessionSerialization(extContext);
        boolean checkApplication = StateUtils.checkApplicationSerialization(extContext);
        boolean checkManagedBeanMutation = StateUtils.checkManagedBeanMutation(extContext);
        
        // check the possible conditions under which we would need to create a SerializationChecker
        if (checkSession || checkApplication || checkManagedBeanMutation)
        {
          boolean checkSessionAtEnd = StateUtils.checkScopesAtEndOfRequest(extContext);
            
          SerializationChecker serializationChecker = new SerializationChecker(
                                                                           extContext,
                                                                           checkSession,
                                                                           checkApplication,
                                                                           checkManagedBeanMutation,
                                                                           checkSessionAtEnd);
          
          requestMap.put(_SERIALIZATION_CHECKER_KEY, serializationChecker);
          
          return serializationChecker;
        }
      }
      
      return null;
    }
    
    /**
     * Creates a SerializationChecker for this request
     * @param extContext               ExternalContext to use to initialize the SerializationChecker
     * @param checkSession If true check serializability of session attributes 
     * @param checkApplication if true, check serializability of application attributes
     * @param checkManagedBeanMutation if true, check for mutations to attributes in the session
     *                                 if checkSession is true and the application if
     *                                 checkApplication is true.
     */
    private SerializationChecker(
      ExternalContext extContext,
      boolean checkSession,
      boolean checkApplication,
      boolean checkManagedBeanMutation,
      boolean checkSessionAtEnd)
    {
      Map<String, Object> sessionMap = extContext.getSessionMap();
      Map<String, Object> applicationMap = extContext.getApplicationMap();
      
      if (checkManagedBeanMutation)
      {
        // note that the mutated bean checekd implicitly checks for attribute serialization as well.
        _sessionBeanChecker = new MutatedBeanChecker(sessionMap,
                                                     "Session",
                                                     extContext.getSession(true),
                                                     true);
        sessionMap = CollectionUtils.newMutationHookedMap(sessionMap, _sessionBeanChecker);
        
        // only check the application for mutations if the application checking is enabled
        if (checkApplication)
        {
          _applicationBeanChecker = new MutatedBeanChecker(applicationMap,
                                                           "Application",
                                                           extContext.getContext(),
                                                           false);
          applicationMap = CollectionUtils.newMutationHookedMap(applicationMap,
                                                                _applicationBeanChecker);
        }
        else
        {
          _applicationBeanChecker = null;
        }
      }
      else
      {
        _sessionBeanChecker     = null;
        _applicationBeanChecker = null;
        
        if (checkSession)
        {
          sessionMap = CollectionUtils.getCheckedSerializationMap(sessionMap, true);
        }

        if (checkApplication)
        {
          applicationMap =  CollectionUtils.getCheckedSerializationMap(applicationMap, false);
        }        
      }
            
      _sessionMap     = sessionMap;
      _applicationMap = applicationMap;
      _checkSessionAttrs = checkSessionAtEnd;
    }

    /**
     * Unregisters the checking of the specified session attribute
     * @param external ExternalContext
     * @param key      Name of session attribute to unregister
     */
    public void unregisterSessionAttribute(ExternalContext external, String key)
    {
      SerializationChecker checker = SerializationChecker.getSerializationChecker(external, false);
      
      if (checker != null)
      {
        if (_sessionBeanChecker != null)
        {
          _sessionBeanChecker.unregisterAttribute(key);
        }
      }    
    }

    /**
     * Unregisters the checking of the specified session attribute
     * @param external ExternalContext
     * @param key      Name of session attribute to unregister
     */
    public void unregisterApplicationAttribute(ExternalContext external, String key)
    {
      SerializationChecker checker = SerializationChecker.getSerializationChecker(external, false);
      
      if (checker != null)
      {
        if (_applicationBeanChecker != null)
        {
          _applicationBeanChecker.unregisterAttribute(key);
        }
      }    
    }

    /**
     * Return a wrapped HttpServletRequest if necessary to implement the checking features
     * @param request
     * @return
     */
    public HttpServletRequest getWrappedRequest(HttpServletRequest request)
    {
      if (_sessionBeanChecker != null)
      {
        return new SessionBeanTracker(request, _sessionBeanChecker, _sessionMap, _applicationMap);
      }
      else
      {
        return request;
      }
    }
    
    /**
     * Returns the potentially wrapped Session Map
     * @return
     */
    public Map<String, Object> getSessionMap()
    {
      return _sessionMap;
    }

    /**
     * Returns the potentially wrapped Application Map
     * @return
     */
    public Map<String, Object> getApplicationMap()
    {
      return _applicationMap;
    }
 
    /**
     * Check the session and application for mutations if configured to do so
     */
    public void checkForMutations()
    {
      if (_sessionBeanChecker != null)
        _sessionBeanChecker.checkForMutations();
    
      if (_applicationBeanChecker != null)
        _applicationBeanChecker.checkForMutations();
      
      // check that all of the attributes in the Session are Serializable
      if (_checkSessionAttrs)
      {
        for (Map.Entry<String, Object> attrEntry : _sessionMap.entrySet())
        {
          String key = attrEntry.getKey();
          Object value = attrEntry.getValue();
          
          try
          {
            _serialize(value);
          }
          catch (Throwable e)
          {
            try
            { 
              String sessionAttributeString = _LOG.getMessage("SESSION_SERIALIZATION_ATTRIBUTE", key);
              String failureDetails = _extractFailureString(sessionAttributeString, value);

              String message = _LOG.getMessage("SERIALIZATION_TESTING_FAILURE", failureDetails);
              
              _LOG.severe(message);
            }
            catch (Throwable ee)
            {
              String failureMessage = _LOG.getMessage("SERIALIZATION_LOGGING_FAILURE");
              
              _LOG.severe(failureMessage, ee);              
            }
          }
        }
      }
    }
   
    /**
     * Wraps the HttpServletRequest so that we can return a wrapped Session so that we can catch
     * changes to the Session attributes and/or return a wrapped ServletContext so that we can
     * catch changes to the SevletContext attributes.
     */
    private static class SessionBeanTracker extends HttpServletRequestWrapper
    {
      public SessionBeanTracker(
        HttpServletRequest  request,
        MutatedBeanChecker  sessionBeanChecker,
        Map<String, Object> sessionMap,
        Map<String, Object> applicationMap)
      {
        super(request);
        
        _wrappedSession = new SessionWrapper(request.getSession(),
                                             sessionBeanChecker,
                                             sessionMap,
                                             applicationMap);
      }

      @Override
      public HttpSession getSession()
      {
        return _wrappedSession;
      }
      
      @Override
      public HttpSession getSession(boolean p1)
      {
        return _wrappedSession;
      }
      
      /**
       * Wraps the HttpSession sso that we can catch
       * changes to the Session attributes and/or return a wrapped ServletContext so that we can
       * catch changes to the SevletContext attributes.
       */
      private static final class SessionWrapper implements HttpSession
      {        
        SessionWrapper(
          HttpSession         session,
          MutatedBeanChecker  sessionChecker,
          Map<String, Object> sessionMap,
          Map<String, Object> applicationMap)
        {
          _delegate        = session;
          _sessionChecker  = sessionChecker;
          _sessionMap      = sessionMap;
          // determine whether we need to return a wrapped ServletContext as well
          if (applicationMap != null)
          {
            _wrappedContext = _getServletContextProxy(session.getServletContext(), applicationMap);
          }
          else
          {
            _wrappedContext = null;
          }
        }
        
        public long getCreationTime()
        {
          return _delegate.getCreationTime();
        }

        public String getId()
        {
          return _delegate.getId();
        }

        public long getLastAccessedTime()
        {
          return _delegate.getLastAccessedTime();
        }

        public ServletContext getServletContext()
        {
          if (_wrappedContext != null)
          {
            return _wrappedContext;
          }
          else
          {
            return _delegate.getServletContext();
          }
        }

        public void setMaxInactiveInterval(int maxInterval)
        {
          _delegate.setMaxInactiveInterval(maxInterval);
        }

        public int getMaxInactiveInterval()
        {
          return _delegate.getMaxInactiveInterval();
        }

        public HttpSessionContext getSessionContext()
        {
          return _delegate.getSessionContext();
        }

        public Object getAttribute(String attrName)
        {
          return _delegate.getAttribute(attrName);
        }

        public Object getValue(String attrName)
        {
          return _delegate.getValue(attrName);
        }

        public Enumeration getAttributeNames()
        {
          return _delegate.getAttributeNames();
        }

        public String[] getValueNames()
        {
          return _delegate.getValueNames();
        }

        /**
         * Implement to delegate and inform the sessionChecker that the attribute is dirty
         * @param key
         * @param value
         */
        public void setAttribute(String key, Object value)
        {
          _delegate.setAttribute(key, value);
          _sessionChecker.writeNotify(_sessionMap, key, value);
        }

        /**
         * Implement to delegate and inform the sessionChecker that the attribute is dirty
         * @param key
         * @param value
         */
        public void putValue(String key, Object value)
        {
          _delegate.putValue(key, value);
          _sessionChecker.writeNotify(_sessionMap, key, value);
        }

        /**
         * Implement to delegate and inform the sessionChecker that the attribute is dirty
         * @param key
         */
        public void removeAttribute(String key)
        {
          _delegate.removeAttribute(key);
          _sessionChecker.removeNotify(_sessionMap, key);
        }

        /**
         * Implement to delegate and inform the sessionChecker that the attribute is dirty
         * @param key
         */
        public void removeValue(String key)
        {
          _delegate.removeValue(key);
          _sessionChecker.removeNotify(_sessionMap, key);
        }

        /**
         * Implement to delegate and inform the sessionChecker that all atrributes are dirty since
         * the session has been blown away
         */
        public void invalidate()
        {
          _delegate.invalidate();
          _sessionChecker.clearCheckedValues();
        }

        public boolean isNew()
        {
          return _delegate.isNew();
        }
                
        private final HttpSession         _delegate;
        private final MutatedBeanChecker  _sessionChecker;
        private final Map<String, Object> _sessionMap;
        private final ServletContext      _wrappedContext;
      }

      private final HttpSession _wrappedSession;
    }
       
    private static final TrinidadLogger _LOG = 
                                   TrinidadLogger.createTrinidadLogger(SerializationChecker.class);
      
    private final MutatedBeanChecker _sessionBeanChecker;
    private final MutatedBeanChecker _applicationBeanChecker;

    private final Map<String, Object> _sessionMap;
    private final Map<String, Object> _applicationMap;
    private final boolean             _checkSessionAttrs;
  }

  /**
   * Class for extracting more detailed failure information for a particular object type
   * that has failed Serialization
   * 
   * @param <T>
   */
  private static abstract class SerializationFailureExtractor<T>
  {
    /**
     * Called to extract failure information about the failedObject onto the
     * failureStack
     */
    public abstract void extractFailure(List<Object> failureStack, T faileddObject);
  }
  
  /**
   * Extract information about failures in Maps
   */
  private static class MapExtractor extends SerializationFailureExtractor<Map<Object, Object>>
  {
    public void extractFailure(List<Object> failureStack, Map<Object, Object> failedMap)
    {
      for (Map.Entry<Object, Object> entry : failedMap.entrySet())
      {
        Object key = entry.getKey();
        
        try
        {
          _serialize(key);
        }
        catch (Throwable e)
        {
          failureStack.add("Failed map key:" + key);
          failureStack.add(key);
          
          _extractFailure(failureStack, key);
        }

        Object value = entry.getValue();

        try
        {
          _serialize(value);
        }
        catch (Throwable e)
        {
          failureStack.add("Failed map value for key=" + key + " value:" + value);
          failureStack.add(value);
          
          _extractFailure(failureStack, value);
        }
      }
    }
  }

  /**
   * Extract information about serialization failures in Lists
   */
  private static class ListExtractor extends SerializationFailureExtractor<List<Object>>
  {
    public void extractFailure(List<Object> failureStack, List<Object> failedList)
    {
      int size = failedList.size();
      
      for (int i = 0; i < size; i++)
      {
        Object value = failedList.get(i);
        
        try
        {
          _serialize(value);
        }
        catch (Throwable e)
        {
          failureStack.add("Failed List value for index=" + i + " value:" + value);
          failureStack.add(value);
          
          _extractFailure(failureStack, value);
        }
      }
    }
  }

  /**
   * Extract information about serialization failures in Arrays
   */
  private static class ArrayExtractor extends SerializationFailureExtractor<Object[]>
  {
    public void extractFailure(List<Object> failureStack, Object[] failedArray)
    {
      int size = failedArray.length;
      
      for (int i = 0; i < size; i++)
      {
        Object value = failedArray[i];
        
        try
        {
          _serialize(value);
        }
        catch (Throwable e)
        {
          failureStack.add("Failed array value for index=" + i + " value:" + value);
          failureStack.add(value);
          
          _extractFailure(failureStack, value);
        }
      }
    }
  }

  /**
   * Extract information about serialization failures in Collections
   */
  private static class CollectionExtractor extends SerializationFailureExtractor<Collection<Object>>
  {
    public void extractFailure(List<Object> failureStack, Collection<Object> failedCollection)
    {      
      for (Object value : failedCollection)
      {        
        try
        {
          _serialize(value);
        }
        catch (Throwable e)
        {
          failureStack.add("Failed Collection value:" + value);
          failureStack.add(value);
          
          _extractFailure(failureStack, value);
        }
      }
    }
  }

  private static final TrinidadLogger _LOG = 
                        TrinidadLogger.createTrinidadLogger(CheckSerializationConfigurator.class);
 
  private static final MapExtractor _MAP_EXTRACTOR = new MapExtractor();
  private static final ListExtractor _LIST_EXTRACTOR = new ListExtractor();
  private static final CollectionExtractor _COLLECTION_EXTRACTOR = new CollectionExtractor();
  private static final ArrayExtractor _ARRAY_EXTRACTOR = new ArrayExtractor();
  
  private static final String _CHECKED_MAPS_KEY = MutatedBeanChecker.class.getName() +"#MAPS";

  private static final String _SERIALIZATION_CHECKER_KEY = 
                                       CheckSerializationConfigurator.class.getName() + "#CHECKER";
}
