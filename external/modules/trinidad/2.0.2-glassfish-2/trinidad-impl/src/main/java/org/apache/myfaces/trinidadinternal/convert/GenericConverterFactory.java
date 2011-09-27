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
package org.apache.myfaces.trinidadinternal.convert;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * A factory of GenericConverters.
 * GenericConverters may be registered with this factory.
 * The factory supports converting between the types supported
 * by each individual converter. The factory also supports converting
 * between types supported by combining individual converters.
 * 
 */
public class GenericConverterFactory
{
  private GenericConverterFactory()
  {
    _cache = new ConcurrentHashMap<Key, TypeConverter>(16);
    _converters = new ArrayList<GenericConverter>(3);
    _reverseDiscoveryConverters = new ArrayList<ReverseDiscoveryGenericConverter>(2);
    registerConverter(new SqlConverter());
    registerConverter(new BaseConverter());
    registerReverseDiscoveryConverter(new ReflectionConverter());
  }
  
  /**
   * Gets a converter that is capable of converting from the given
   * sourceType to the given targetType.
   * This method first searches to see if any of the registered converters
   * are capable of making this conversion alone. If one is found, it is
   * returned. Otherwise, this method will search to see if some combination
   * of registered converters can be used to perform this conversion.
   * If so, a composite converter will be returned.
   * <P>
   * The returned converter (or lack thereof) is cached so that subsequent
   * requests for the same source and target types will be fast.
   * @return null if there is no such converter.
   */
  public TypeConverter getConverter(Class<?> sourceType, Class<?> targetType)
  {
    Key key = new Key(sourceType, targetType);
    // check the cache first:
    Object cached = _cache.get(key);
    if (cached != null)
    {
      return (cached == _NULL) ? null : (TypeConverter) cached;
    }

    // we are going to start searching to see if some chain of converters
    // can be used to perform this conversion.
    // initial node in chain:
    Node start = new Node(null, null, sourceType);
    LinkedList<Node> sourcesToBeSearched = new LinkedList<Node>();
    sourcesToBeSearched.add(start);
    // cache to store all the classes we've tested already. This is to
    // avoid our chains from looping indefinitely:
    Set<Class<?>> cache  = new HashSet<Class<?>>(16);
    
    // Try to find a converter chain without "reverse discovery" type converetrs.
    // Our particular implementation of the "reverse discovery" type converter uses Java Reflection,
    // so it is better to use a chain of normal generic converters
    TypeConverter converter = _findConverter(sourcesToBeSearched, targetType, cache, false);
    
    // If we failed, try to use "reverse discovery" converters
    if (converter == null && _reverseDiscoveryConverters.size() > 0)
    {
      // We will use a "reverse discovery" type converter only if target-to-source conversion
      // is available without the use of "reverse discovery" type converters. This is done to ensure
      // we do not find a converter dynamically for the target type when the source type is
      // not convertable from the target type (converter's availability has to be 'reflective').
      // Regular generic converters are pre-defined, so they normally do not have this problem.
      
      TypeConverter reverseConv = null;
      reverseConv = _cache.get(new Key(targetType, sourceType));
      if (reverseConv == null)
      {
        cache.clear();
        sourcesToBeSearched.add( new Node(null, null, targetType));
        reverseConv = _findConverter(sourcesToBeSearched, sourceType, cache, false);
      }
      
      if (reverseConv != null)
      {
        cache.clear();
        sourcesToBeSearched.clear();
        sourcesToBeSearched.add(start);
        converter = _findConverter(sourcesToBeSearched, targetType, cache, true);
      }
    }
    
    if (converter == null)
    {
      // cache the fact that no such converter exists:
      _cache.put(key, _NULL);
    }
    else
    {
      _cache.put(key, converter);
    }

    return converter;
  }
  
  /**
   * tries to find a converter, or create a chain of converters that can
   * convert from one of the given sourceTypes to the specified targetType.
   * @param sourcesToBeSearched each element is a Node. Each Node is a pairing of
   * sourceType and the chain of converters needed to produce this sourceType.
   * @param targetType the type that is needed
   * @param cache used to record which classes we've searched already.
   * @param useRevserseDiscovery true if "reverse discovery" converters should be used, false otherwise
   * @return null if no converter was found.
   */
  private TypeConverter _findConverter(
    LinkedList<Node> sourcesToBeSearched,
    Class<?> targetType, 
    Set<Class<?>> cache,
    boolean useRevserseDiscovery)
  {
    while(!sourcesToBeSearched.isEmpty())
    {
      Node source = sourcesToBeSearched.removeFirst();
      TypeConverter match = null;
      
      // loop through all the converters and see what types they can turn 
      // the current sourceType into 
      // (the current sourceType is source.targetType):
      for(int j=0,jsz=_converters.size(); j<jsz; j++)
      {
        GenericConverter conv = _converters.get(j);
        // loop though all the targetTypes on this converter to see
        // if we can find the one we're looking for:
        if (_searchTargetTypes(sourcesToBeSearched, source, conv, targetType,
                               cache))
        {
          match = conv;
        }
      }
      
      if (match == null && useRevserseDiscovery)
        match = _searchSourceTypes(source.targetType, targetType);
      
      if (match != null)
      {
        // see if there is no chain:
        if (source.previous == null)
          return match;
          
        // there is a chain:
        return new CompositeConverter(source, match, targetType);
      }
    }
    return null;
  }
  
  /**
   * Searches the targetTypes of the given converter to see if we
   * can find the type we are searching for.
   * @param sourcesToBeSearched each element is a Node. Each Node is a pairing of
   * sourceType and the chain of converters needed to produce this sourceType.
   * @param currentSource a chain of converters has been used to produce the
   * type identified by this Node. The targetType of this Node will be used
   * to search the currentConverter.
   * @param searchType the targetType we are searching for.
   * @param cache used to record which classes we've searched already.
   * @return true if the currentConverter can convert from 
   * currentSource.targetType into searchType.
   */
  private boolean _searchTargetTypes(
    List<Node> sourcesToBeSearched,
    Node currentSource,
    GenericConverter currentConverter,
    Class<?> searchType,
    Set<Class<?>> cache)
  { 
    Class<?> sourceType = currentSource.targetType;
    List<Class<?>> targetTypes = currentConverter.getTargetTypes(sourceType);
    for(int i=0,sz=targetTypes.size(); i<sz; i++)
    {
      Class<?> targetType = targetTypes.get(i);
      // check to see if we've seen this targetType before:
      if (cache.add(targetType))
      {
        // check to see if the targetType is a subclass of the searchType:
        if (searchType.isAssignableFrom(targetType))
          return true;
          
        // create a new node in the chain by adding this targetType and converter
        Node newSource = new Node(currentSource, currentConverter, targetType);
        
        // add the new node so that we can continue searching by seeing if
        // we can convert the targetType into the searchType using some other
        // converter:
        sourcesToBeSearched.add(newSource);
      }
    }
    return false;
  }
  
  /**
   * Finds a suitable converter by searching source types supported for a
   * given target type
   * @param sourceType - source type
   * @param targetType - target type
   * @return a suitable TypeConverter is found, null otherwise
   */
  private TypeConverter _searchSourceTypes(Class<?> sourceType, Class<?> targetType)
  {
    for (ReverseDiscoveryGenericConverter conv:_reverseDiscoveryConverters)
    {
      List<Class<?>> sourceTypes = conv.getSourceTypes(targetType);
      for (Class<?> type: sourceTypes)
      {
        if (type.isAssignableFrom(sourceType))
          return conv;
      }
    }
    return null;
  }
  
  /**
   * Registers a converter. Registering a new converter causes the internal
   * cache to be cleared.
   */
  public void registerConverter(GenericConverter converter)
  {
    _converters.add(converter);
    _cache.clear();
  }
  
  /**
   * Registers a "reverse discovery" converter. Registering a new converter causes the internal
   * cache to be cleared.
   */
  public void registerReverseDiscoveryConverter(ReverseDiscoveryGenericConverter converter)
  {
    _reverseDiscoveryConverters.add(converter);
    _cache.clear();
  }

  /**
   * converts the given source instance into an object of the targetType.
   * @param source the object to convert
   * @param targetType the required type.
   * @return null, if the source is null.
   */
  public Object convert(Object source, Class<?> targetType)
  {
    if (source == null)
      return null;
      
    if (targetType.isAssignableFrom(source.getClass()))
      return source;
  
    TypeConverter converter = getConverter(source.getClass(), targetType);
    if (converter != null)
    {
      return converter.convert(source, targetType);
    }
    throw new TypeConversionException(source, targetType);
  }
  
  /**
   * Checks to see if it is possible to convert the given instance 
   * into the specified targetType
   * @return true if conversion is possible.
   */
  public boolean isConvertible(Object source, Class<?> targetType)
  {
    if (source == null)
      return false; // bug 4589048
  
    if (targetType.isAssignableFrom(source.getClass()))
      return true;
    
    TypeConverter converter = getConverter(source.getClass(), targetType);
    return converter != null;
  }
  
  private final Map<Key, TypeConverter> _cache;
  private final List<GenericConverter> _converters;
  private final List<ReverseDiscoveryGenericConverter> _reverseDiscoveryConverters;
  // 2006-08-02: -= Simon Lessard =-
  //             Using a GenericConverter null value instead 
  //             of Node.class to be typesafe
  private static final TypeConverter _NULL = new TypeConverter()
  {
    public Object convert(Object source, Class<?> targetType)
    {
      return null;
    }
  };

  private static final class Node
  {
    public Node(Node previous, TypeConverter converter, Class<?> targetType)
    {
      this.previous = previous;
      this.converter = converter;
      this.targetType = targetType;
    }
    
    public Object convert(Object source)
    {
      if (previous != null)
      {
        source = previous.convert(source);
        source = converter.convert(source, targetType);
      }
      return source;
    }
    
    public final Node previous;
    public final TypeConverter converter;
    public final Class<?> targetType;
  }
  
  private static final class Key
  {
    public Key(Class<?> source, Class<?> target)
    {
      assert !source.equals(target);

      _source = source;
      _target = target;
      
      _hc = source.hashCode() + target.hashCode();
    }
  
    @Override
    public int hashCode()
    {
      return _hc;
    }
    
    @Override
    public boolean equals(Object other)
    {
      if (this == other)
        return true;
      if (other instanceof Key)
      {
        Key that = (Key) other;
        return (_source.equals(that._source) && _target.equals(that._target));
      }
      return false;
    }
    
    private final int _hc;  
    private final Class<?> _source;
    private final Class<?> _target;
  }
  
  private static final class CompositeConverter implements TypeConverter
  {
    public CompositeConverter(Node source, TypeConverter conv, Class<?> targetType)
    {
      assert source != null;
      _chain = new Node(source, conv, targetType) ;
    }

    public Object convert(Object source, Class<?> targetType)
    {
      if (targetType.isAssignableFrom(_chain.targetType))
      {
        return _chain.convert(source);
      }
      else
        throw new IllegalArgumentException(_LOG.getMessage(
          "CANNOT_CONVERT", new Object[]{source, targetType.getName()}));
    }
    
    private final Node _chain;
  }
  
  public static GenericConverterFactory getCurrentInstance(ExternalContext extContext)
  {
    // TODO: once getCurrentInstance() taking ExternalContext is in ADE,
    // we need to modify FacesDatabindingConfigurator to pass in the context.
    // Then we can switch to storing factory instance on the application map
    
    /*if (extContext == null)
      extContext = FacesContext.getCurrentInstance().getExternalContext();
    Map appMap = extContext.getApplicationMap();
    GenericConverterFactory factory = (GenericConverterFactory)appMap.get(_INSTANCE_KEY);
    if (factory == null)
    {
      factory = new GenericConverterFactory();
      appMap.put(_INSTANCE_KEY, factory);
    }
    return factory;*/
    return _INSTANCE;
  }
  
  public static GenericConverterFactory getCurrentInstance()
  {
    return getCurrentInstance(null);
  }
  

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    GenericConverterFactory.class);
  
  //private static final String _INSTANCE_KEY = "org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory";
  private static final GenericConverterFactory _INSTANCE = new GenericConverterFactory();
}
