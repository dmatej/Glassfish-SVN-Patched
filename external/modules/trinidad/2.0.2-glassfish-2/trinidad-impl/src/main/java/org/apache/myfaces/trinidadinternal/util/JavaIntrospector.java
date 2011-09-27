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
package org.apache.myfaces.trinidadinternal.util;

import java.awt.Image;

import java.beans.BeanDescriptor;
import java.beans.BeanInfo;
import java.beans.EventSetDescriptor;
import java.beans.FeatureDescriptor;
import java.beans.IndexedPropertyDescriptor;
import java.beans.IntrospectionException;
import java.beans.MethodDescriptor;
import java.beans.ParameterDescriptor;
import java.beans.PropertyDescriptor;
import java.beans.PropertyVetoException;
import java.beans.SimpleBeanInfo;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import java.util.Enumeration;
import java.util.EventListener;
import java.util.EventObject;
import java.util.Hashtable;
import java.util.TooManyListenersException;
import java.util.Vector;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;


/**
 * The Introspector class provides a standard way for tools to learn about
 * the properties, events, and methods supported by a target Java Bean.
 * <p>
 * For each of those three kinds of information, the Introspector will
 * separately analyze the bean's class and superclasses looking for
 * either explicit or implicit information and use that information to
 * build a BeanInfo object that comprehensively describes the target bean.
 * <p>
 * For each class "Foo", explicit information may be available if there exists
 * a corresponding "FooBeanInfo" class that provides a non-null value when
 * queried for the information.   We first look for the BeanInfo class by
 * taking the full package-qualified name of the target bean class and
 * appending "BeanInfo" to form a new class name.  If this fails, then
 * we take the final classname component of this name, and look for that
 * class in each of the packages specified in the BeanInfo package search
 * path.
 * <p>
 * Thus for a class such as "sun.xyz.OurButton" we would first look for a
 * BeanInfo class called "sun.xyz.OurButtonBeanInfo" and if that failed we'd
 * look in each package in the BeanInfo search path for an OurButtonBeanInfo
 * class.  With the default search path, this would mean looking for
 * "sun.beans.infos.OurButtonBeanInfo".
 * <p>
 * If a class provides explicit BeanInfo about itself then we add that to
 * the BeanInfo information we obtained from analyzing any derived classes,
 * but we regard the explicit information as being definitive for the current
 * class and its base classes, and do not proceed any further up the superclass
 * chain.
 * <p>
 * If we don't find explicit BeanInfo on a class, we use low-level
 * reflection to study the methods of the class and apply standard design
 * patterns to identify property accessors, event sources, or public
 * methods.  We then proceed to analyze the class's superclass and add
 * in the information from it (and possibly on up the superclass chain).
 * <p>
 * This class differs from the standard Introspector in the following ways:
   <OL>
     <LI>Supports "has" as a prefix for boolean getters.
     <LI>Is smarter about not accidentally creating IndexedPropertyDescriptors.
     <LI>It handles SecurityManagers that don't allow access to all of the
         declared methods of a class gracefully.
     <LI>It caches only the method objects that it cares about.  Saving memory.
     <LI>It caches BeanInfos in more cases and makes fewer unnecessary copies
         when returning BeanInfos.
   </OL>
 * <p>
 * @since EWT 3.0
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/JavaIntrospector.java#0 $) $Date: 10-nov-2005.18:49:10 $
 */

public class JavaIntrospector
{
  // Flags that can be used to control getBeanInfo:
  public final static int USE_ALL_BEANINFO           = 1;
  public final static int IGNORE_IMMEDIATE_BEANINFO  = 2;
  public final static int IGNORE_ALL_BEANINFO        = 3;

  //======================================================================
  //                 Public methods
  //======================================================================

  /**
   * Introspect on a Java bean and learn about all its properties, exposed
   * methods, and events.
   *
   * @param beanClass  The bean class to be analyzed.
   * @return  A BeanInfo object describing the target bean.
   * @exception IntrospectionException if an exception occurs during
   *              introspection.
   */
  public static BeanInfo getBeanInfo(
    Class<?> beanClass
    ) throws IntrospectionException
  {
    // first look for the bean info in the cache
    GenericBeanInfo bi = (GenericBeanInfo)_sBeanInfoCache.get(beanClass);

    if (bi == null)
    {
      // bean info wasn't found in the cache, so create an introspector
      // instance to create it
      JavaIntrospector introspector = new JavaIntrospector(beanClass,
                                                           null,
                                                           USE_ALL_BEANINFO);

      // generate the bean info using the introspector
      bi = introspector._getBeanInfo();

      // cache the generated bean info
      _sBeanInfoCache.put(beanClass, bi);
    }

    // Make an independent copy of the BeanInfo so that clients can't corrupt
    // our cached version.
    return new GenericBeanInfo(bi);
  }


  /**
   * Introspect on a Java bean and learn about all its properties, exposed
   * methods, and events, subnject to some comtrol flags.
   *
   * @param beanClass  The bean class to be analyzed.
   * @param flags  Flags to control the introspection.
   *     If flags == USE_ALL_BEANINFO then we use all of the BeanInfo
   *         classes we can discover.
   *     If flags == IGNORE_IMMEDIATE_BEANINFO then we ignore any
   *           BeanInfo associated with the specified beanClass.
   *     If flags == IGNORE_ALL_BEANINFO then we ignore all BeanInfo
   *           associated with the specified beanClass or any of its
   *         parent classes.
   * @return  A BeanInfo object describing the target bean.
   * @exception IntrospectionException if an exception occurs during
   *              introspection.
   */
  public static BeanInfo getBeanInfo(
    Class<?> beanClass,
    int      flags
    ) throws IntrospectionException
  {
    // if they want all of the bean info, call the caching version of this
    // method so that we cache the result
    if (flags == USE_ALL_BEANINFO)
    {
      return getBeanInfo(beanClass);
    }
    else
    {
      //
      // use the uncached version
      //
      return new JavaIntrospector(beanClass, null, flags)._getBeanInfo();
    }
  }


  /**
   * Introspect on a Java bean and learn all about its properties, exposed
   * methods, below a given "stop" point.
   *
   * @param bean The bean class to be analyzed.
   * @param stopClass The baseclass at which to stop the analysis.  Any
   *    methods/properties/events in the stopClass or in its baseclasses
   *    will be ignored in the analysis.
   * @exception IntrospectionException if an exception occurs during
   *              introspection.
   */
  public static BeanInfo getBeanInfo(
    Class<?> beanClass,
    Class<?> stopClass
    ) throws IntrospectionException
  {
    // if they don't want to stop at any class, call the caching version of this
    // method so that we cache the result
    if (stopClass == null)
    {
      return getBeanInfo(beanClass);
    }
    else
    {
      return new JavaIntrospector(beanClass,
                                  stopClass,
                                  USE_ALL_BEANINFO)._getBeanInfo();
    }
  }


  /**
   * Utility method to take a string and convert it to normal Java variable
   * name capitalization.  This normally means converting the first
   * character from upper case to lower case, but in the (unusual) special
   * case when there is more than one character and both the first and
   * second characters are upper case, we leave it alone.
   * <p>
   * Thus "FooBah" becomes "fooBah" and "X" becomes "x", but "URL" stays
   * as "URL".
   *
   * @param  name The string to be decapitalized.
   * @return  The decapitalized version of the string.
   */
  public static String decapitalize(
    String name
    )
  {
    if (name == null || name.length() == 0)
    {
      return name;
    }

    if ((name.length() > 1)                   &&
        Character.isUpperCase(name.charAt(1)) &&
        Character.isUpperCase(name.charAt(0)))
    {
      return name;
    }

    char chars[] = name.toCharArray();
    chars[0] = Character.toLowerCase(chars[0]);

    return new String(chars);
  }


  /**
   * @return  The array of package names that will be searched in
   *        order to find BeanInfo classes.
   * <p>     This is initially set to {"sun.beans.infos"}.
   */
  public static String[] getBeanInfoSearchPath()
  {
    if (_sSearchPath == null)
      return null;

    return _sSearchPath.clone();
  }


  /**
   * Change the list of package names that will be used for
   *        finding BeanInfo classes.
   * @param path  Array of package names.
   */
  public static void setBeanInfoSearchPath(
    String path[]
    )
  {
    _sSearchPath = path == null ? null : path.clone();
  }


  /**
   * Flush all of the Introspector's internal caches.  This method is
   * not normally required.  It is normally only needed by advanced
   * tools that update existing "Class" objects in-place and need
   * to make the Introspector re-analyze existing Class objects.
   */
  public static void flushCaches()
  {
    _sBeanInfoCache.clear();
    _sPublicMethodCache.clear();
  }


  /**
   * Flush the Introspector's internal cached information for a given class.
   * This method is not normally required.  It is normally only needed
   * by advanced tools that update existing "Class" objects in-place
   * and need to make the Introspector re-analyze an existing Class object.
   *
   * Note that only the direct state associated with the target Class
   * object is flushed.  We do not flush state for other Class objects
   * with the same name, nor do we flush state for any related Class
   * objects (such as subclasses), even though their state may include
   * information indirectly obtained from the target Class object.
   *
   * @param targetClass  Class object to be flushed.
   */

  public static void flushFromCaches(
    Class<?> targetClass
    )
  {
    _sBeanInfoCache.remove(targetClass);
    _sPublicMethodCache.remove(targetClass);
  }


  //======================================================================
  //             Private implementation methods
  //======================================================================

  private JavaIntrospector(
    Class<?> beanClass,
    Class<?> stopClass,
    int   flags
    ) throws IntrospectionException
  {
    _beanClass = beanClass;

    // Check stopClass is a superClass of startClass.
    if (stopClass != null)
    {
      boolean isSuper = false;

      for (Class<?> c = beanClass.getSuperclass();
           c != null;
           c = c.getSuperclass())
      {
        if (c == stopClass)
        {
          isSuper = true;
        }
      }

      if (!isSuper)
      {
        throw new IntrospectionException(_LOG.getMessage(
          "NOT_SUPERCLASS_OF", new Object[]{stopClass.getName(), beanClass.getName()}));
      }
    }

    // collect any bean info the bean creator may have provided
    // for us
    if (flags == USE_ALL_BEANINFO)
    {
      _informant = _findInformant(beanClass);
    }

    Class<?> superClass = beanClass.getSuperclass();

    if (superClass != stopClass)
    {
      int newFlags = flags;

      if (newFlags == IGNORE_IMMEDIATE_BEANINFO)
      {
        newFlags = USE_ALL_BEANINFO;
      }

      //
      // recursively create all of the bean info for our superclasses
      //
      if ((stopClass == null) && (newFlags == USE_ALL_BEANINFO))
      {
        // We avoid going through getBeanInfo as we don't need
        // it to copy the BeanInfo.
        _superBeanInfo = _sBeanInfoCache.get(superClass);

        if (_superBeanInfo == null)
        {
          JavaIntrospector ins = new JavaIntrospector(superClass,
                                                      null,
                                                      USE_ALL_BEANINFO);
          _superBeanInfo = ins._getBeanInfo();
          _sBeanInfoCache.put(superClass, _superBeanInfo);
        }
      }
      else
      {
        JavaIntrospector ins = new JavaIntrospector(superClass,
                                                    stopClass,
                                                    newFlags);
        _superBeanInfo = ins._getBeanInfo();
      }
    }

    // collect the additional bean info provided us by the bean creator
    if (_informant != null)
    {
      _additionalBeanInfo = _informant.getAdditionalBeanInfo();
    }

    if (_additionalBeanInfo == null)
    {
      // no additional BeanInfo, so allocate an empty array as a place holder.
      _additionalBeanInfo = new BeanInfo[0];
    }
  }


  private GenericBeanInfo _getBeanInfo() throws IntrospectionException
  {
    return new GenericBeanInfo(this, _informant);
  }


  /**
   * Find any bean info information that the creator of the bean may
   * have left around for us
   */
  private BeanInfo _findInformant(
    Class<?> beanClass
    )
  {
    String name = beanClass.getName() + "BeanInfo";

    try
    {
      return (BeanInfo)_instantiate(beanClass, name);
    }
    catch (Exception ex)
    {
      // Just drop through
      ;
    }

    // Now try checking if the bean is its own BeanInfo.
    try
    {
      if (_isSubclass(beanClass, BeanInfo.class))
      {
        return (BeanInfo)beanClass.newInstance();
      }
    }
    catch (Exception ex)
    {
      // Just drop through
      ;
    }

    // Now try looking for <SearchPath>.fooBeanInfo
    while (name.indexOf('.') > 0)
    {
      name = name.substring(name.indexOf('.') + 1);
    }

    for (int i = 0; i < _sSearchPath.length; i++)
    {
      try
      {
        String fullName = _sSearchPath[i] + "." + name;

        return (BeanInfo)_instantiate(beanClass, fullName);
      }
      catch (Exception ex)
      {
        // Silently ignore any errors.
        ;
      }
    }

    return null;
  }


  /**
   * @return An array of PropertyDescriptors describing the editable
   * properties supported by the target bean.
   */
  PropertyDescriptor[] __getTargetPropertyInfo()
    throws IntrospectionException
  {

    // properties maps from String names to PropertyDescriptors
    // -= Simon Lessard =-
    // TODO: Check if synchronization is really needed.
    Hashtable<String, PropertyDescriptor> properties = 
      new Hashtable<String, PropertyDescriptor>();

    // Check if the bean has its own BeanInfo that will provide
    // explicit information.
    PropertyDescriptor[] explicit = null;

    // if at least some of our superclasses have their own BeanInfos,
    // use them as a first cut before using introspection
    if (_informant != null)
    {
      explicit = _informant.getPropertyDescriptors();
      int ix = _informant.getDefaultPropertyIndex();

      if ((ix >= 0) && (ix < explicit.length))
      {
        _defaultPropertyName = explicit[ix].getName();
      }
    }

    if ((explicit == null) && (_superBeanInfo != null))
    {
      // We have no explicit BeanInfo properties.  Check with our parent.
      PropertyDescriptor supers[] = _superBeanInfo.getPropertyDescriptors();

      //
      // add all of our parent's properties to us to start with
      //
      for (int i = 0 ; i < supers.length; i++)
      {
        _addProperty(properties, supers[i]);
      }


      //
      // if our parent has a valid default property, make it our default
      // property
      //
      int ix = _superBeanInfo.getDefaultPropertyIndex();

      if ((ix >= 0) && (ix < supers.length))
      {
        _defaultPropertyName = supers[ix].getName();
      }
    }

    //
    // Add any additional bean info onto our parent's properties, overriding
    // our parent's properties if there is a conflict.
    //
    for (int i = 0; i < _additionalBeanInfo.length; i++)
    {
      PropertyDescriptor additional[] =
                                _additionalBeanInfo[i].getPropertyDescriptors();

      if (additional != null)
      {
        for (int j = 0 ; j < additional.length; j++)
        {
          _addProperty(properties, additional[j]);
        }
      }
    }

    if (explicit != null)
    {
      // Add the explicit informant data to our results.
      for (int i = 0 ; i < explicit.length; i++)
      {
        _addProperty(properties, explicit[i]);
      }
    }
    else
    {
      //
      // Apply some reflection to the current class to generate BeanInfo.
      //

      // First get an array of all the bean's public methods at this level
      Method methodList[] = _getPublicMethods(_beanClass);


      //
      // Now analyze each method to see if it fits our rules for default
      // PropertyDescriptor generation.
      //
      for (int i = 0; i < methodList.length; i++)
      {
        Method method = methodList[i];

        // skip static methods.
        if (Modifier.isStatic(method.getModifiers()))
        {
          continue;
        }

        String     name       = method.getName();
        Class<?>[] argTypes   = method.getParameterTypes();
        Class<?>   resultType = method.getReturnType();

        int argCount = argTypes.length;

        PropertyDescriptor pd = null;

        try
        {
          if (argCount == 0)
          {
            //
            // methods that take no arguments could be simple getters
            //
            if (name.startsWith(_READ_PREFIX))
            {
              // Simple getter
              pd = new PropertyDescriptor(
                            decapitalize(name.substring(_READ_PREFIX.length())),
                            method,
                            null);
            }
            else if (resultType == boolean.class)
            {
              //
              // check the lsit of allowed boolean prefixes for a match.
              //
              for (int j = 0; j < _BOOLEAN_READ_PREFIXES.length; j++)
              {
                String currPrefix = _BOOLEAN_READ_PREFIXES[j];

                if (name.startsWith(currPrefix))
                {
                  // Success, so create a new property descriptor, trimming off
                  // the prefix
                  pd = new PropertyDescriptor(
                             decapitalize(name.substring(currPrefix.length())),
                             method,
                             null);
                }
              }
            }
          }
          else if (argCount == 1)
          {
            //
            // Methods that take 1 argument could be indexed getters or
            // setters
            //

            if ((argTypes[0] == int.class) && name.startsWith(_READ_PREFIX))
            {
              // It's an indexed getter
              pd = new IndexedPropertyDescriptor(
                         decapitalize(name.substring(_READ_PREFIX.length())),
                         null,
                         null,
                         method,
                         null);
            }
            else if ((resultType == void.class) &&
                     name.startsWith(_WRITE_PREFIX))
            {
              // It's a simple setter
              pd = new PropertyDescriptor(
                             decapitalize(name.substring(_WRITE_PREFIX.length())),
                             null,
                             method);

              if (_throwsException(method, PropertyVetoException.class))
              {
                pd.setConstrained(true);
              }
            }
          }
          else if (argCount == 2)
          {
            if ((argTypes[0] == int.class) && name.startsWith(_WRITE_PREFIX))
            {
              // it's an indexed setter
              pd = new IndexedPropertyDescriptor(
                             decapitalize(name.substring(_WRITE_PREFIX.length())),
                             null,
                             null,
                             null,
                             method);

              if (_throwsException(method, PropertyVetoException.class))
              {
                pd.setConstrained(true);
              }
            }
          }
        }
        catch (IntrospectionException ex)
        {
          // This happens if a PropertyDescriptor or IndexedPropertyDescriptor
          // constructor finds that the method violates details of the design
          // pattern, e.g. by having an empty name, or a getter returning
          // void , or whatever.
          pd = null;
        }

        if (pd != null)
        {
          // If this class or one of its base classes is a PropertyChange
          // source, then we assume that any properties we discover are "bound".
          if (_propertyChangeSource)
          {
            pd.setBound(true);
          }

          // add the property to our list of properties
          _addProperty(properties, pd);
        }
      }
    }


    //
    // prune the list of properties, removing bogusly introspected properties.
    //
    Enumeration<PropertyDescriptor> elements = properties.elements();

    while (elements.hasMoreElements())
    {
      Object currElement = elements.nextElement();

      //
      // Prune out IndexedPropertyDescriptors that have no indexedGetMethods
      // and take two ints as parameters.
      //
      if (currElement instanceof IndexedPropertyDescriptor)
      {
        IndexedPropertyDescriptor indexedElement =
                                        (IndexedPropertyDescriptor)currElement;

        if (indexedElement.getIndexedReadMethod() == null)
        {
          Method writeMethod = indexedElement.getIndexedWriteMethod();

          if ((writeMethod != null) &&
              (writeMethod.getParameterTypes()[1] == int.class))
          {
            // prune this descriptor
            properties.remove(indexedElement.getName());
          }
        }
      }
    }


    //
    // Allocate and populate the result array.
    //
    PropertyDescriptor result[] = new PropertyDescriptor[properties.size()];

    elements = properties.elements();

    for (int i = 0; i < result.length; i++)
    {
      result[i] = elements.nextElement();

      if ((_defaultPropertyName != null) &&
          _defaultPropertyName.equals(result[i].getName()))
      {
        _defaultPropertyIndex = i;
      }
    }

    return result;
  }


  /**
   * Add a property to our list of properties, merging the property if there
   * is a conflict.
   */
  private void _addProperty(
    Hashtable<String, PropertyDescriptor> properties,
    PropertyDescriptor                    newDescriptor
    )
  {
    //
    // descriptor to put in the cache.  In most cases, we will just put the
    // new descriptor into the cache directly.
    //
    PropertyDescriptor putDescriptor = newDescriptor;

    //
    // see if we have another property with this name already in our cache
    //
    String name = newDescriptor.getName();

    PropertyDescriptor oldDescriptor = properties.get(name);

    //
    // A descriptor with this name is already in the cache, so determine
    // what to do about it
    //
    if (oldDescriptor != null)
    {
      //
      // If the property type hasn't changed we need to merge the descriptors.
      //
      Class<?> oldDescriptorType = oldDescriptor.getPropertyType();
      Class<?> newDescriptorType = newDescriptor.getPropertyType();

      if (!((oldDescriptorType != newDescriptorType) &&
            (oldDescriptorType != null)              &&
            (newDescriptorType != null)))
      {
        if ((oldDescriptor instanceof IndexedPropertyDescriptor) ||
            (newDescriptor instanceof IndexedPropertyDescriptor))
        {
          // create a composite IndexedPropertyDescriptor
          putDescriptor = _createMergedIndexedDescriptor(oldDescriptor,
                                                         newDescriptor);

          // an error occurred creating the composite, so something is
          // messed up.  This is almost certainly because the introspector
          // blew it and thought something was an IndexedPropertyDescriptor
          // when it wasn't so toast the one that is an
          // IndexedPropertyDescriptor.  If both are, arbitrarily chose the
          // newer one.
          if (putDescriptor == null)
          {
            putDescriptor = (!(oldDescriptor instanceof IndexedPropertyDescriptor))
                              ? oldDescriptor
                              : newDescriptor;
          }
        }
        else
        {
          putDescriptor = _createMergedPropertyDescriptor(oldDescriptor,
                                                          newDescriptor);
        }
      }
    }

    // put the correct descriptor in the cache
    properties.put(name, putDescriptor);
  }


  /**
   * @return An array of EventSetDescriptors describing the kinds of
   * events fired by the target bean.
   */
  EventSetDescriptor[] __getTargetEventInfo()
    throws IntrospectionException
  {

    // events maps from String names to EventSetDescriptors
    // -= Simon Lessard =-
    // TODO: Check if synchronization is really needed.
    Hashtable<String, EventSetDescriptor> events = 
      new Hashtable<String, EventSetDescriptor>();

    // Check if the bean has its own BeanInfo that will provide
    // explicit information.
    EventSetDescriptor[] explicit = null;

    if (_informant != null)
    {
      explicit = _informant.getEventSetDescriptors();
      int ix = _informant.getDefaultEventIndex();

      if ((ix >= 0) && (ix < explicit.length))
      {
        _defaultEventName = explicit[ix].getName();
      }
    }

    if ((explicit == null) && (_superBeanInfo != null))
    {
      // We have no explicit BeanInfo events.  Check with our parent.
      EventSetDescriptor supers[] = _superBeanInfo.getEventSetDescriptors();

      for (int i = 0 ; i < supers.length; i++)
      {
        _addEvent(events, supers[i]);
      }

      int ix = _superBeanInfo.getDefaultEventIndex();

      if ((ix >= 0) && (ix < supers.length))
      {
        _defaultEventName = supers[ix].getName();
      }
    }

    for (int i = 0; i < _additionalBeanInfo.length; i++)
    {
      EventSetDescriptor additional[] =
                                _additionalBeanInfo[i].getEventSetDescriptors();

      if (additional != null)
      {
        for (int j = 0 ; j < additional.length; j++)
        {
          _addEvent(events, additional[j]);
        }
      }
    }

    if (explicit != null)
    {
      // Add the explicit informant data to our results.
      for (int i = 0 ; i < explicit.length; i++)
      {
        _addEvent(events, explicit[i]);
      }
    }
    else
    {
      // Apply some reflection to the current class.

      // Get an array of all the beans methods at this level
      Method methodList[] = _getPublicMethods(_beanClass);

      // Find all suitable "add" and "remove" methods.
      // -= Simon Lessard =-
      // TODO: Check if synchronization is really needed.
      Hashtable<String, Method> adds    = new Hashtable<String, Method>();
      Hashtable<String, Method> removes = new Hashtable<String, Method>();

      for (int i = 0; i < methodList.length; i++)
      {
        Method method = methodList[i];

        //
        // skip static methods.
        //
        if (Modifier.isStatic(method.getModifiers()))
        {
          continue;
        }

        String name = method.getName();

        Class<?>[] argTypes   = method.getParameterTypes();
        Class<?>   resultType = method.getReturnType();

        if (name.startsWith("add") &&
            (argTypes.length == 1) &&
            (resultType == Void.TYPE))
        {
          String compound = name.substring(3) + ":" + argTypes[0];
          adds.put(compound, method);
        }
        else if (name.startsWith("remove") &&
                 (argTypes.length == 1)    &&
                 (resultType == Void.TYPE))
        {
          String compound = name.substring(6) + ":" + argTypes[0];
          removes.put(compound, method);
        }
      }

      // Now look for matching addFooListener+removeFooListener pairs.
      Enumeration<String> keys = adds.keys();

      while (keys.hasMoreElements())
      {
        String compound = keys.nextElement();

        // Skip any "add" which doesn't have a matching "remove".
        if (removes.get(compound) == null)
        {
          continue;
        }

        // Method name has to end in "Listener"
        if (compound.indexOf("Listener:") <= 0)
        {
          continue;
        }

        String listenerName = compound.substring(0, compound.indexOf(':'));
        String eventName    = decapitalize(
                             listenerName.substring(0,
                                                    listenerName.length() - 8));
        Method   addMethod    = adds.get(compound);
        Method   removeMethod = removes.get(compound);
        Class<?> argType      = addMethod.getParameterTypes()[0];

        // Check if the argument type is a subtype of EventListener
        if (!JavaIntrospector._isSubclass(argType, _EVENT_LISTENER_TYPE))
        {
          continue;
        }

        // generate a list of Method objects for each of the target methods:
        Method allMethods[] = argType.getMethods();

        int count = 0;

        for (int i = 0; i < allMethods.length; i++)
        {
          if (_isEventHandler(allMethods[i]))
          {
            count++;
          }
          else
          {
            allMethods[i] = null;
          }
        }

        Method methods[] = new Method[count];

        int j = 0;

        for (int i = 0; i < allMethods.length; i++)
        {
          if (allMethods[i] != null)
          {
            methods[j++] = allMethods[i];
          }
        }

        EventSetDescriptor esd = new EventSetDescriptor(eventName,
                                                        argType,
                                                        methods,
                                                        addMethod,
                                                        removeMethod);

        // If the adder method throws the TooManyListenersException then it
        // is a Unicast event source.
        if (_throwsException(addMethod,TooManyListenersException.class))
        {
          esd.setUnicast(true);
        }

        _addEvent(events, esd);
      }
    }

    // Allocate and populate the result array.
    EventSetDescriptor[]            result   = new EventSetDescriptor[events.size()];
    Enumeration<EventSetDescriptor> elements = events.elements();

    for (int i = 0; i < result.length; i++)
    {
      result[i] = elements.nextElement();

      if ((_defaultEventName != null)
          && _defaultEventName.equals(result[i].getName()))
      {
        _defaultEventIndex = i;
      }
    }

    return result;
  }


  private void _addEvent(
    Hashtable<String, EventSetDescriptor> events,
    EventSetDescriptor descriptor
    )
  {
    String key = descriptor.getName() + descriptor.getListenerType();

    if (descriptor.getName().equals("propertyChange"))
    {
      _propertyChangeSource = true;
    }

    EventSetDescriptor oldDescriptor = events.get(key);

    if (oldDescriptor == null)
    {
      events.put(key, descriptor);
      return;
    }

    EventSetDescriptor composite = _createMergedEventSetDescriptor(
                                                               oldDescriptor,
                                                               descriptor);
    events.put(key, composite);
  }


  /**
   * @return An array of MethodDescriptors describing the public
   * methods supported by the target bean.
   */
  MethodDescriptor[] __getTargetMethodInfo()
    throws IntrospectionException
  {
    // Check if the bean has its own BeanInfo that will provide
    // explicit information.
    MethodDescriptor[] explicit = null;

    // hash table to associate the method objects with their method
    // descriptors
    // -= Simon Lessard =-
    // TODO: Check if synchronization is really needed.
    Hashtable<String, MethodDescriptor> methods = 
      new Hashtable<String, MethodDescriptor>();

    if (_informant != null)
    {
      explicit = _informant.getMethodDescriptors();
    }

    if ((explicit == null) && (_superBeanInfo != null))
    {
      // We have no explicit BeanInfo methods.  Check with our parent.
      MethodDescriptor supers[] = _superBeanInfo.getMethodDescriptors();

      for (int i = 0 ; i < supers.length; i++)
      {
        _addMethod(methods, supers[i]);
      }
    }

    for (int i = 0; i < _additionalBeanInfo.length; i++)
    {
      MethodDescriptor additional[] =
                                  _additionalBeanInfo[i].getMethodDescriptors();

      if (additional != null)
      {
        for (int j = 0 ; j < additional.length; j++)
        {
          _addMethod(methods, additional[j]);
        }
      }
    }

    if (explicit != null)
    {
      // Add the explicit informant data to our results.
      for (int i = 0 ; i < explicit.length; i++)
      {
        _addMethod(methods, explicit[i]);
       }
    }
    else
    {
      // Apply some reflection to the current class.

      // First get an array of all the public beans methods at this level
      Method methodList[] = _getPublicMethods(_beanClass);

      // Add each method
      for (int i = 0; i < methodList.length; i++)
      {
        _addMethod(methods, new MethodDescriptor(methodList[i]));
      }
    }

    // Allocate and populate the result array.
    MethodDescriptor result[] = new MethodDescriptor[methods.size()];

    Enumeration<MethodDescriptor> elements = methods.elements();

    for (int i = 0; i < result.length; i++)
    {
      result[i] = elements.nextElement();
    }

    return result;
  }


  private void _addMethod(
    Hashtable<String, MethodDescriptor> methods,
    MethodDescriptor descriptor
    )
  {
    // We have to be careful here to distinguish method by both name
    // and argument lists.
    // This method gets called a *lot, so we try to be efficient.
    String name = descriptor.getMethod().getName();

    MethodDescriptor old = methods.get(name);

    if (old == null)
    {
      // This is the common case.
      methods.put(name, descriptor);
      return;
    }

    // We have a collision on method names.  This is rare.

    // Check if old and descriptor have the same type.
    Class   p1[]  = descriptor.getMethod().getParameterTypes();
    Class   p2[]  = old.getMethod().getParameterTypes();
    boolean match = false;

    if (p1.length == p2.length)
    {
      match = true;
      for (int i = 0; i < p1.length; i++)
      {
        if (p1[i] != p2[i])
        {
          match = false;
          break;
        }
      }
    }

    if (match)
    {
      MethodDescriptor composite = _createMergedMethodDescriptor(old,
                                                                 descriptor);
      methods.put(name, composite);
      return;
    }

    // We have a collision on method names with different type signatures.
    // This is very rare.
    String longKey = _makeQualifiedMethodName(descriptor);

    old = methods.get(longKey);

    if (old == null)
    {
      methods.put(longKey, descriptor);
      return;
    }

    MethodDescriptor composite = _createMergedMethodDescriptor(old,
                                                               descriptor);
    methods.put(longKey, composite);
  }


  private String _makeQualifiedMethodName(
    MethodDescriptor descriptor
    )
  {
    Method        m = descriptor.getMethod();
    StringBuffer sb = new StringBuffer();

    sb.append(m.getName());
    sb.append("=");

    Class params[] = m.getParameterTypes();

    for (int i = 0; i < params.length; i++)
    {
      sb.append(":");
      sb.append(params[i].getName());
    }

    return sb.toString();
  }


  int __getTargetDefaultEventIndex()
  {
    return _defaultEventIndex;
  }


  int __getTargetDefaultPropertyIndex()
  {
    return _defaultPropertyIndex;
  }


  BeanDescriptor __getTargetBeanDescriptor()
    throws IntrospectionException
  {
    // Use explicit info, if available,
    if (_informant != null)
    {
      BeanDescriptor bd = _informant.getBeanDescriptor();

      if (bd != null)
      {
        return bd;
      }
    }

    // OK, fabricate a default BeanDescriptor.
    return (new BeanDescriptor(_beanClass));
  }


  private boolean _isEventHandler(
    Method m
    ) throws IntrospectionException
  {
    // We assume that a method is an event handler if it has a single
    // argument, whose type inherit from java.util.Event.
    try
    {
      Class argTypes[] = m.getParameterTypes();

      if (argTypes.length != 1)
      {
        return false;
      }

      return (_isSubclass(argTypes[0], EventObject.class));
    }
    catch (Exception ex)
    {
      throw new IntrospectionException("Unexpected reflection exception: " + ex);
    }
  }


  /**
   * Return all of the public methods for the target class
   */
  private static synchronized Method[] _getPublicMethods(
    Class<?> targetClass
    )
  {
    // Looking up Class.getMethods is relatively expensive,
    // so we cache the results.
    Method[] declaredMethods = _sPublicMethodCache.get(targetClass);

    //
    // if the declared methods aren't in the cache, generate them and stuff
    // them in the cache
    //
    if (declaredMethods == null)
    {
      //
      // if we are allowed to get the declared methods, the easy way, do
      // so.
      //
      if (_sDeclaredAccessOK)
      {
        try
        {
          // get all of the methods declared at this level
          declaredMethods = targetClass.getDeclaredMethods();
        }
        catch (SecurityException e)
        {
          // I guess the SecurityManager won't let us try this
          _sDeclaredAccessOK = false;
        }
      }

      //
      // Try this the hard way using just the public methods and pruning out
      // our superclass's methods.  Hopefully the security manager won't choke
      // on this also.
      //
      if (declaredMethods == null)
      {
        try
        {
          // get all of the public methods for this class and its superclasses
          declaredMethods = targetClass.getMethods();
        }
        catch (SecurityException e)
        {
          // nothing that we can do at this point
          ;
        }
      }

      //
      // prune out all of the non-public methods not declared by this
      // class.
      //
      if (declaredMethods != null)
      {
        int numDeclaredMethods = declaredMethods.length;

        //
        // remove all of the non-public methods
        //
        if (numDeclaredMethods > 0)
        {
          // -= Simon Lessard =-
          // TODO: Check if synchronization is really needed.
          Vector<Method> publicMethods = new Vector<Method>(numDeclaredMethods);

          for (int i = 0; i < numDeclaredMethods; i++)
          {
            Method currMethod = declaredMethods[i];

            //
            // only add the method if it is public and is declared
            // by this class.  The later test handles the case where
            // we had to get our methods by gettting all of the methods
            // of the class
            //
            if (Modifier.isPublic(currMethod.getModifiers()) &&
                (currMethod.getDeclaringClass() == targetClass))
            {
              publicMethods.addElement(currMethod);
            }
          }

          // convert the vector to an array
          declaredMethods = new Method[publicMethods.size()];
          publicMethods.copyInto(declaredMethods);
        }
      }

      _sPublicMethodCache.put(targetClass, declaredMethods);
    }

    return declaredMethods;
  }


  //======================================================================
  // Package private support methods.
  //======================================================================

  /**
   * Internal support for finding a target methodName on a given class.
   */
  // -= Simon Lessard
  // FIXME: Never used locally
  @SuppressWarnings("unused")
  private static Method _internalFindMethod(
    Class<?> start,
    String   methodName,
    int      argCount
    )
  {
    // For overriden methods we need to find the most derived version.
    // So we start with the given class and walk up the superclass chain.
    for (Class<?> cl = start; cl != null; cl = cl.getSuperclass())
    {
      Method methods[] = _getPublicMethods(cl);

      for (int i = 0; i < methods.length; i++)
      {
        Method method = methods[i];

        // skip static and non-public methods.
        int mods = method.getModifiers();

        if (Modifier.isStatic(mods) || !Modifier.isPublic(mods))
        {
          continue;
        }

        if (method.getName().equals(methodName) &&
            (method.getParameterTypes().length == argCount))
        {
          return method;
        }
      }
    }

    // Now check any inherited interfaces.  This is necessary both when
    // the argument class is itself an interface, and when the argument
    // class is an abstract class.
    Class ifcs[] = start.getInterfaces();

    for (int i = 0 ; i < ifcs.length; i++)
    {
      Method m = _internalFindMethod(ifcs[i], methodName, argCount);

      if (m != null)
      {
        return m;
      }
    }

    return null;
  }

  /**
   * Return true if class a is either equivalent to class b, or
   * if class a is a subclass of class b, i.e. if a either "extends"
   * or "implements" b.
   * Note tht either or both "Class" objects may represent interfaces.
   */
  static  boolean _isSubclass(
    Class<?> a,
    Class<?> b
    )
  {
    // We rely on the fact that for any given java class or
    // primtitive type there is a unqiue Class object, so
    // we can use object equivalence in the comparisons.
    if (a == b)
    {
      return true;
    }

    if ((a == null) || (b == null))
    {
      return false;
    }

    for (Class<?> x = a; x != null; x = x.getSuperclass())
    {
      if (x == b)
      {
        return true;
      }

      if (b.isInterface())
      {
        Class interfaces[] = x.getInterfaces();

        for (int i = 0; i < interfaces.length; i++)
        {
          if (_isSubclass(interfaces[i], b))
          {
            return true;
          }
        }
      }
    }

    return false;
  }


  /**
   * Return true iff the given method throws the given exception.
   */
  private boolean _throwsException(
    Method method,
    Class<?> exception
    )
  {
    Class exs[] = method.getExceptionTypes();

    for (int i = 0; i < exs.length; i++)
    {
      if (exs[i] == exception)
      {
        return true;
      }
    }

    return false;
  }


  /**
   * Try to create an instance of a named class.  First try the classloader of
   * "sibling", then try the context loader followed by the system
   * classloader.
   */
  private static Object _instantiate(
    Class<?> sibling,
    String   className
    ) throws InstantiationException,
             IllegalAccessException,
             ClassNotFoundException
  {
    // First check with sibling's classloader (if any).
    ClassLoader cl = sibling.getClassLoader();

    if (cl != null)
    {
      try
      {
        Class<?> cls = cl.loadClass(className);

        return cls.newInstance();
      }
      catch (Exception ex)
      {
        // Just drop through and try the system classloader.
        ;
      }
    }

    // Now try the system classloader.
    Class<?> cls = ClassLoaderUtils.loadClass(className);
    return cls.newInstance();
  }


  /**
   * Merge information from two PropertyDescriptors into a third
   * IndexedPropertyDescriptor.
   * In the event of other conflicts, the second argument
   * <code>primaryDescriptor</code> is given priority over the first argument
   * <code>secondaryDescriptor</code>.
   * <p>
   * @param secondaryDescriptor  The lower priority PropertyDescriptor
   * @param primaryDescriptor  The higher priority PropertyDescriptor
   * @param mergedDescriptor The IndexedPropertyDescriptor to merge the
   *                         information into.
   */
  private static IndexedPropertyDescriptor _createMergedIndexedDescriptor(
    PropertyDescriptor secondaryDescriptor,
    PropertyDescriptor primaryDescriptor
    )
  {
    Method readMethod         = _getMergedReadMethod(secondaryDescriptor,
                                                     primaryDescriptor);
    Method writeMethod        = _getMergedWriteMethod(secondaryDescriptor,
                                                      primaryDescriptor);
    Method indexedReadMethod  = null;
    Method indexedWriteMethod = null;

    if (secondaryDescriptor instanceof IndexedPropertyDescriptor)
    {
      IndexedPropertyDescriptor iSecondaryDescriptor =
                                (IndexedPropertyDescriptor)secondaryDescriptor;

      readMethod         = iSecondaryDescriptor.getReadMethod();
      writeMethod        = iSecondaryDescriptor.getWriteMethod();
      indexedReadMethod  = iSecondaryDescriptor.getIndexedReadMethod();
      indexedWriteMethod = iSecondaryDescriptor.getIndexedWriteMethod();
    }


    if (primaryDescriptor instanceof IndexedPropertyDescriptor)
    {
      IndexedPropertyDescriptor iPrimaryDescriptor =
                                (IndexedPropertyDescriptor)primaryDescriptor;

      Method tempMethod = iPrimaryDescriptor.getIndexedReadMethod();

      if (tempMethod != null)
      {
        indexedReadMethod = tempMethod;
      }

      tempMethod = iPrimaryDescriptor.getIndexedWriteMethod();

      if (tempMethod != null)
      {
        indexedWriteMethod = tempMethod;
      }
    }

    //
    // create the IndexedPropertyDescriptor with the indexed portions already
    // merged.  This is necessary because there are no setIndexedReadMethod(),
    // or setIndexedWriteMthod() until JDK 1.2
    //
    try
    {
      IndexedPropertyDescriptor mergedIndexedDescriptor =
        new IndexedPropertyDescriptor(primaryDescriptor.getName(),
                                      readMethod,
                                      writeMethod,
                                      indexedReadMethod,
                                      indexedWriteMethod);

      // merge superclasses
      _mergePropertyDescriptors(secondaryDescriptor,
                                primaryDescriptor,
                                mergedIndexedDescriptor);

      return mergedIndexedDescriptor;
    }
    catch (Exception e)
    {
      // This will happen if the introspector screwed up about a property
      // being an IndexedPropertyDescriptor.  In this case, the
      // IndexedPropertyDescriptor constructor will throw an exception.
      return null;
    }
  }


  private static PropertyDescriptor _createMergedPropertyDescriptor(
    PropertyDescriptor secondaryDescriptor,
    PropertyDescriptor primaryDescriptor
    )
  {
    try
    {
      PropertyDescriptor mergedDescriptor =
               new PropertyDescriptor(primaryDescriptor.getName(),
                                      _getMergedReadMethod(secondaryDescriptor,
                                                           primaryDescriptor),
                                      _getMergedWriteMethod(secondaryDescriptor,
                                                            primaryDescriptor));

      // merge the superclasses
      _mergePropertyDescriptors(secondaryDescriptor,
                                primaryDescriptor,
                                mergedDescriptor);

      return mergedDescriptor;
    }
    catch (Exception e)
    {
      // _LOG.severe(e);
      return null;
    }
  }


   private static MethodDescriptor _createMergedMethodDescriptor(
     MethodDescriptor secondaryDescriptor,
     MethodDescriptor primaryDescriptor
     )
   {
     ParameterDescriptor[] parameterDescriptors =
                                   primaryDescriptor.getParameterDescriptors();

    if (parameterDescriptors == null)
    {
      parameterDescriptors = secondaryDescriptor.getParameterDescriptors();
    }

    MethodDescriptor mergedDescriptor = new MethodDescriptor(
                                             primaryDescriptor.getMethod(),
                                             parameterDescriptors);

    // merge the superclasses
    _mergeFeatureDescriptors(secondaryDescriptor,
                             primaryDescriptor,
                             mergedDescriptor);

    return mergedDescriptor;
  }



 static EventSetDescriptor __createMergedEventSetStub(
    EventSetDescriptor oldDescriptor,
    MethodDescriptor[] listenerDescriptors
    )
  {
    try
    {
      EventSetDescriptor stubDescriptor = new EventSetDescriptor(
                                    oldDescriptor.getName(),
                                    oldDescriptor.getListenerType(),
                                    listenerDescriptors,
                                    oldDescriptor.getAddListenerMethod(),
                                    oldDescriptor.getRemoveListenerMethod());

      // set the unicast attribute
      stubDescriptor.setUnicast(oldDescriptor.isUnicast());

      return stubDescriptor;
    }
    catch (Exception e)
    {
      //    _LOG.severe(e);
      return null;
    }
  }


  private static EventSetDescriptor _createMergedEventSetDescriptor(
    EventSetDescriptor secondaryDescriptor,
    EventSetDescriptor primaryDescriptor
    )
  {
    //
    // merge the listener descriptors
    //
    MethodDescriptor[] listenerDescriptors =
                            primaryDescriptor.getListenerMethodDescriptors();

    if (listenerDescriptors == null)
    {
      listenerDescriptors = secondaryDescriptor.getListenerMethodDescriptors();
    }

    // create the stub EvetnSetDescriptor
    EventSetDescriptor mergedDescriptor = __createMergedEventSetStub(
                                                     primaryDescriptor,
                                                     listenerDescriptors);

    //
    // merge isDefaultEventSet
    //
    mergedDescriptor.setInDefaultEventSet(
                                  primaryDescriptor.isInDefaultEventSet() &&
                                  secondaryDescriptor.isInDefaultEventSet());

    // merge the superclasses
    _mergeFeatureDescriptors(secondaryDescriptor,
                             primaryDescriptor,
                             mergedDescriptor);

    return mergedDescriptor;
  }


  /**
   * Necessary because no support for PropertyDescriptor.setReadMethod()
   * until JDK 1.2.
   */
  private static Method _getMergedReadMethod(
    PropertyDescriptor secondaryDescriptor,
    PropertyDescriptor primaryDescriptor
    )
  {
    //
    // Figure out the merged read method.
    //
    Method primaryReadMethod   = primaryDescriptor.getReadMethod();
    Method secondaryReadMethod = secondaryDescriptor.getReadMethod();

    Method readMethod = primaryReadMethod;

    // Normally give priority to the primary readMethod.
    if (readMethod == null)
    {
      readMethod = secondaryReadMethod;
    }
    else
    {
      //
      // However, if both x and y reference read methods in the same class,
      // give priority to a secondary boolean getter with an alternate prefix
      // over a primary getter with the standard _READ_PREFIX
      //
      if ((secondaryReadMethod != null)                          &&
          (secondaryReadMethod.getDeclaringClass() ==
           primaryReadMethod.getDeclaringClass())                &&
          (secondaryReadMethod.getReturnType() == boolean.class) &&
          (primaryReadMethod.getReturnType() == boolean.class)   &&
          primaryReadMethod.getName().startsWith(_READ_PREFIX))
      {
        // check each of the boolean prefixes
        for (int i = 0; i < _BOOLEAN_READ_PREFIXES.length; i++)
        {
          String currPrefix = _BOOLEAN_READ_PREFIXES[i];

          if (secondaryReadMethod.getName().startsWith(currPrefix))
          {
            readMethod = secondaryReadMethod;
            break;
          }
        }
      }
    }

    return readMethod;
  }


  /**
   * Necessary because no support for PropertyDescriptor.setWriteMethod()
   * until JDK 1.2.
   */
  private static Method _getMergedWriteMethod(
    PropertyDescriptor secondaryDescriptor,
    PropertyDescriptor primaryDescriptor
    )
  {
    //
    // merge the write method
    //
    Method writeMethod = primaryDescriptor.getWriteMethod();

    // Give priority to the primary write method.
    if (writeMethod == null)
    {
      writeMethod = secondaryDescriptor.getWriteMethod();
    }

    return writeMethod;
  }



  /**
   * Merge information from two PropertyDescriptors into a third
   * PropertyDescriptor.
   * In the event of other conflicts, the second argument
   * <code>primaryDescriptor</code> is given priority over the first argument
   * <code>secondaryDescriptor</code>.
   * <p>
   * @param secondaryDescriptor  The lower priority PropertyDescriptor
   * @param primaryDescriptor  The higher priority PropertyDescriptor
   * @param mergedDescriptor The PropertyDescriptor to merge the information
   *                         into.
   */
  private static void _mergePropertyDescriptors(
    PropertyDescriptor secondaryDescriptor,
    PropertyDescriptor primaryDescriptor,
    PropertyDescriptor mergedDescriptor
    )
  {
    // merge the superclasses
    _mergeFeatureDescriptors(secondaryDescriptor,
                             primaryDescriptor,
                             mergedDescriptor);

    //
    // merge the property editor class
    //
    Class<?> editorClass = primaryDescriptor.getPropertyEditorClass();

    // Give priority to the primary propertyEditor.
    if (editorClass == null)
    {
      editorClass = secondaryDescriptor.getPropertyEditorClass();
    }

    mergedDescriptor.setPropertyEditorClass(editorClass);

    // merge the bound property
    mergedDescriptor.setBound(secondaryDescriptor.isBound() |
                              primaryDescriptor.isBound());

    // merge the constrained property
    mergedDescriptor.setConstrained(secondaryDescriptor.isConstrained() |
                                    primaryDescriptor.isConstrained());
  }


  /**
   * Merge information from two FeatureDescriptors into a third
   * FeatureDescriptor.
   * The merged hidden and expert flags are formed by or-ing the values.
   * In the event of other conflicts, the second argument
   * <code>primaryDescriptor</code> is given priority over the first argument
   * <code>secondaryDescriptor</code>.
   * <p>
   * @param secondaryDescriptor  The lower priority FeatureDescriptor
   * @param primaryDescriptor  The higher priority FeatureDescriptor
   * @param mergedDescriptor The FeatureDescriptor to merge the information
   *                         into.
   */
  private static void _mergeFeatureDescriptors(
    FeatureDescriptor secondaryDescriptor,
    FeatureDescriptor primaryDescriptor,
    FeatureDescriptor mergedDescriptor
    )
  {
    // merge the expert property
    mergedDescriptor.setExpert(secondaryDescriptor.isExpert() |
                               primaryDescriptor.isExpert());

    // merge the hidden property
    mergedDescriptor.setHidden(secondaryDescriptor.isHidden() |
                               primaryDescriptor.isHidden());

    // the primary's name is the merged name
    mergedDescriptor.setName(primaryDescriptor.getName());


    //
    // Merge the short description
    //
    String shortDescription = primaryDescriptor.getShortDescription();

    if (shortDescription == null)
    {
      shortDescription = primaryDescriptor.getShortDescription();
    }

    mergedDescriptor.setShortDescription(shortDescription);


    //
    // Merge the display name
    //
    String displayName = primaryDescriptor.getDisplayName();

    if (displayName == null)
    {
      displayName = primaryDescriptor.getDisplayName();
    }

    mergedDescriptor.setDisplayName(displayName);

    //
    // merge in the Feature values, merging in the secondary descriptor's
    // attributes first, so that any of the primary descriptor's attributes
    // that conflict can override them.
    //
    __addFeatureValues(secondaryDescriptor, mergedDescriptor);
    __addFeatureValues(primaryDescriptor, mergedDescriptor);
  }


  /**
   * Add all of the attributes of one FeatureDescriptor to thos
   * of another, replacing any attributes that conflict.
   */
  static void __addFeatureValues(
    FeatureDescriptor addingDescriptor,
    FeatureDescriptor destinationDescriptor
    )
  {
    Enumeration<String> keys = addingDescriptor.attributeNames();

    if (keys != null)
    {
      while (keys.hasMoreElements())
      {
        String key = keys.nextElement();
        Object value = addingDescriptor.getValue(key);
        destinationDescriptor.setValue(key, value);
      }
    }
  }



//===========================================================================

  private static final Class<EventListener> _EVENT_LISTENER_TYPE = EventListener.class;
  private static final String _READ_PREFIX = "get";
  private static final String _WRITE_PREFIX = "set";
  private static final String[] _BOOLEAN_READ_PREFIXES = {"is", "has"};

  private boolean _propertyChangeSource = false;

  // clas of object that we are building the BeanInfo of
  private Class<?> _beanClass;

  // the BeanInof of the bean class' is superClass
  private BeanInfo _superBeanInfo;

  // bean creator-provided bean info for the bean class
  // scavenged from various places on the class path
  private BeanInfo _informant;

  private BeanInfo[] _additionalBeanInfo;
  private String _defaultEventName;
  private String _defaultPropertyName;
  private int _defaultEventIndex = -1;
  private int _defaultPropertyIndex = -1;

  // cache mapping classes to their BeanInfos
  private static Hashtable<Class<?>, BeanInfo> _sBeanInfoCache = 
    new Hashtable<Class<?>, BeanInfo>();

  // Cache of public Class.getDeclaredMethods:
  // -= Simon Lessard =-
  // TODO: Check if synchronization is really needed.
  private static Hashtable<Class<?>, Method[]> _sPublicMethodCache = 
    new Hashtable<Class<?>, Method[]>();

  // true if SecurityManager will give us access to all declared fields
  // of the class.
  private static boolean _sDeclaredAccessOK = true;

  private static String[] _sSearchPath = { "sun.beans.infos" };
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(JavaIntrospector.class);
}


  //======================================================================

/**
 * Package private implementation support class for Introspector's
 * internal use.
 */
class GenericBeanInfo extends SimpleBeanInfo
{
  public GenericBeanInfo(
    JavaIntrospector introspector,
    BeanInfo         targetBeanInfo
    )
  {
    _introspector   = introspector;
    _targetBeanInfo = targetBeanInfo;
  }


  /**
   * Package-private dup constructor
   * This must isolate the new object from any changes to the old object.
   */
  GenericBeanInfo(
    GenericBeanInfo old
    )
  {
    _oldBeanInfo = old;
    _targetBeanInfo = old._targetBeanInfo;
  }

  @Override
  public PropertyDescriptor[] getPropertyDescriptors()
  {
    if (_properties == null)
    {
      if (_introspector != null)
      {
        try
        {
          _properties = _introspector.__getTargetPropertyInfo();

          // since the default property depends on the properties, might as
          // well caculate it now
          _defaultProperty = _introspector.__getTargetDefaultPropertyIndex();
        }
        catch (IntrospectionException e)
        {
          // do nothing
          ;
        }
      }
      else
      {
        PropertyDescriptor[] oldProperties =
                                        _oldBeanInfo.getPropertyDescriptors();

        if (oldProperties != null)
        {
          int len = oldProperties.length;
          PropertyDescriptor[] newProperties = new PropertyDescriptor[len];

          for (int i = 0; i < len; i++)
          {
            PropertyDescriptor oldProperty = oldProperties[i];

            if (oldProperty instanceof IndexedPropertyDescriptor)
            {
              newProperties[i] = _cloneIndexedPropertyDescriptor(
                                      (IndexedPropertyDescriptor)oldProperty);
            }
            else
            {
              newProperties[i] = _clonePropertyDescriptor(oldProperty);
            }
          }

          _properties = newProperties;
        }

        _defaultProperty = _oldBeanInfo.getDefaultPropertyIndex();
      }
    }

    return _properties;
  }

  @Override
  public int getDefaultPropertyIndex()
  {
    if (_defaultProperty == _DEFAULT_VALUE)
    {
      // properties have to be calculated before we can calculate
      // the default property index
      getPropertyDescriptors();
    }

    return _defaultProperty;
  }

  @Override
  public EventSetDescriptor[] getEventSetDescriptors()
  {
    if (_events == null)
    {
      //
      // if we have an introspector, get the data from there, otherwise
      // get it from the old bean info
      //
      if (_introspector != null)
      {
        try
        {
          _events = _introspector.__getTargetEventInfo();

          // since the default event depends on the events, might as
          // well caculate it now
          _defaultEvent = _introspector.__getTargetDefaultEventIndex();
        }
        catch (IntrospectionException e)
        {
          // do nothing
          ;
        }
      }
      else
      {
        EventSetDescriptor[] oldEventSet =
                                        _oldBeanInfo.getEventSetDescriptors();

        if (oldEventSet != null)
        {
          int len = oldEventSet.length;
          EventSetDescriptor[] newEventSet = new EventSetDescriptor[len];

          for (int i = 0; i < len; i++)
          {
            newEventSet[i] = _cloneEventSetDescriptor(oldEventSet[i]);
          }

          _events = newEventSet;
        }

        _defaultEvent = _oldBeanInfo.getDefaultEventIndex();
      }
    }

    return _events;
  }

  @Override
  public int getDefaultEventIndex()
  {
    if (_defaultEvent == _DEFAULT_VALUE)
    {
      // event have to be calculated before we can calculate
      // the default event index
      getEventSetDescriptors();
    }

    return _defaultEvent;
  }

  @Override
  public MethodDescriptor[] getMethodDescriptors()
  {
    if (_methods == null)
    {
      if (_introspector != null)
      {
        try
        {
          _methods = _introspector.__getTargetMethodInfo();
        }
        catch (IntrospectionException e)
        {
          // do nothing
          ;
        }
      }
      else
      {
        MethodDescriptor[] oldMethods = _oldBeanInfo.getMethodDescriptors();

        if (oldMethods != null)
        {
          int len  = oldMethods.length;
          MethodDescriptor[] newMethods = new MethodDescriptor[len];

          for (int i = 0; i < len; i++)
          {
            newMethods[i] = _cloneMethodDescriptor(oldMethods[i]);
          }

          _methods = newMethods;
        }
      }
    }

    return _methods;
  }

  @Override
  public BeanDescriptor getBeanDescriptor()
  {
    if (_beanDescriptor == null)
    {
      if (_introspector != null)
      {
        try
        {
          _beanDescriptor = _introspector.__getTargetBeanDescriptor();
        }
        catch (IntrospectionException e)
        {
          // do nothing
          ;
        }
      }
      else
      {
        _beanDescriptor =
                       _cloneBeanDescriptor(_oldBeanInfo.getBeanDescriptor());
      }
    }

    return _beanDescriptor;
  }

  @Override
  public Image getIcon(
    int iconKind
    )
  {
    if (_targetBeanInfo != null)
    {
      return _targetBeanInfo.getIcon(iconKind);
    }

    return super.getIcon(iconKind);
  }

  private static void _copyPropertyDescriptor(
    PropertyDescriptor oldDescriptor,
    PropertyDescriptor newDescriptor
    )
  {
    newDescriptor.setBound(oldDescriptor.isBound());
    newDescriptor.setConstrained(oldDescriptor.isConstrained());
    newDescriptor.setPropertyEditorClass(
                                     oldDescriptor.getPropertyEditorClass());

    // copy in superclass
    _copyFeatureDescriptor(oldDescriptor, newDescriptor);
  }


  private static void _copyFeatureDescriptor(
    FeatureDescriptor oldDescriptor,
    FeatureDescriptor newDescriptor
    )
  {
    newDescriptor.setName(oldDescriptor.getName());
    newDescriptor.setDisplayName(oldDescriptor.getDisplayName());
    newDescriptor.setExpert(oldDescriptor.isExpert());
    newDescriptor.setHidden(oldDescriptor.isHidden());
    newDescriptor.setShortDescription(oldDescriptor.getShortDescription());

    JavaIntrospector.__addFeatureValues(oldDescriptor, newDescriptor);
  }


  private static IndexedPropertyDescriptor _cloneIndexedPropertyDescriptor(
    IndexedPropertyDescriptor oldDescriptor
    )
  {
    try
    {
      IndexedPropertyDescriptor newDescriptor = new IndexedPropertyDescriptor(
                                      oldDescriptor.getName(),
                                      oldDescriptor.getReadMethod(),
                                      oldDescriptor.getWriteMethod(),
                                      oldDescriptor.getIndexedReadMethod(),
                                      oldDescriptor.getIndexedWriteMethod());

      // copy the rest of the attributes
      _copyPropertyDescriptor(oldDescriptor, newDescriptor);

      return newDescriptor;
    }
    catch (Exception e)
    {
      _LOG.severe(e);
      return null;
    }
  }


  private static PropertyDescriptor _clonePropertyDescriptor(
    PropertyDescriptor oldDescriptor
    )
  {
    try
    {
      PropertyDescriptor newDescriptor = new PropertyDescriptor(
                                             oldDescriptor.getName(),
                                             oldDescriptor.getReadMethod(),
                                             oldDescriptor.getWriteMethod());

      // copy the rest of the attributes
      _copyPropertyDescriptor(oldDescriptor, newDescriptor);

      return newDescriptor;
    }
    catch (Exception e)
    {
      _LOG.severe(e);
      return null;
    }
  }


  private static BeanDescriptor _cloneBeanDescriptor(
    BeanDescriptor oldDescriptor
    )
  {
    try
    {
      BeanDescriptor newDescriptor = new BeanDescriptor(
                                          oldDescriptor.getBeanClass(),
                                          oldDescriptor.getCustomizerClass());

      // copy the rest of the attributes
      _copyFeatureDescriptor(oldDescriptor, newDescriptor);

      return newDescriptor;
    }
    catch (Exception e)
    {
      _LOG.severe(e);
      return null;
    }
  }


  private static MethodDescriptor _cloneMethodDescriptor(
    MethodDescriptor oldDescriptor
    )
  {
    try
    {
      MethodDescriptor newDescriptor = new MethodDescriptor(
                                    oldDescriptor.getMethod(),
                                    oldDescriptor.getParameterDescriptors());

      // copy the rest of the attributes
      _copyFeatureDescriptor(oldDescriptor, newDescriptor);

      return newDescriptor;
    }
    catch (Exception e)
    {
      _LOG.severe(e);
      return null;
    }
  }


  private static EventSetDescriptor _cloneEventSetDescriptor(
    EventSetDescriptor oldDescriptor
    )
  {
    EventSetDescriptor newDescriptor =
           JavaIntrospector.__createMergedEventSetStub(
                              oldDescriptor,
                              oldDescriptor.getListenerMethodDescriptors());

    newDescriptor.setInDefaultEventSet(oldDescriptor.isInDefaultEventSet());

    return newDescriptor;
  }

  private static final int _DEFAULT_VALUE = -2;

  private JavaIntrospector     _introspector;
  private GenericBeanInfo      _oldBeanInfo;
  private BeanDescriptor       _beanDescriptor;
  private EventSetDescriptor[] _events;
  private int                  _defaultEvent = _DEFAULT_VALUE;
  private PropertyDescriptor[] _properties;
  private int                  _defaultProperty = _DEFAULT_VALUE;
  private MethodDescriptor[]   _methods;
  private BeanInfo             _targetBeanInfo;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(GenericBeanInfo.class);
}

