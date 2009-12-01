/*
 * JBoss, Home of Professional Open Source
 * Copyright 2009, Red Hat, Inc. and/or its affiliates, and individual
 * contributors by the @authors tag. See the copyright.txt in the
 * distribution for a full listing of individual contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jboss.interceptor.model;

import java.lang.reflect.Method;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jboss.interceptor.util.InterceptionUtils;
import org.jboss.interceptor.util.ReflectionUtils;
import org.jboss.interceptor.registry.InterceptorClassMetadataRegistry;
import org.jboss.interceptor.InterceptorException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorClassMetadataImpl implements InterceptorClassMetadata, Serializable
{

   private Logger log = LoggerFactory.getLogger(InterceptorClassMetadataImpl.class);

   private Class<?> interceptorClass;

   private Map<InterceptionType, List<Method>> methodMap = new HashMap<InterceptionType, List<Method>>();

   private boolean hasInterceptorMethods;

   public InterceptorClassMetadataImpl(Class<?> interceptorClass)
   {
      this.interceptorClass = interceptorClass;

      Class<?> currentClass = interceptorClass;

      Set<MethodHolder> foundMethods = new HashSet<MethodHolder>();
      do
      {
         Set<InterceptionType> detectedInterceptorTypes = new HashSet<InterceptionType>();

         for (Method method : currentClass.getDeclaredMethods())
         {
            for (InterceptionType interceptionType : InterceptionTypeRegistry.getSupportedInterceptionTypes())
            {
               if (InterceptionUtils.isInterceptorMethod(interceptionType, method))
               {
                  if (methodMap.get(interceptionType) == null)
                     methodMap.put(interceptionType, new LinkedList<Method>());
                  if (detectedInterceptorTypes.contains(interceptionType))
                     throw new InterceptorMetadataException("Same interception type cannot be specified twice on the same class");
                  else
                     detectedInterceptorTypes.add(interceptionType);
                  // add method in the list - if it is there already, it means that it has been added by a subclass
                  ReflectionUtils.ensureAccessible(method);
                  if (!foundMethods.contains(MethodHolder.of(method, false)))
                  {
                     methodMap.get(interceptionType).add(0, method);
                     hasInterceptorMethods = true;
                  }
               }
            }
            foundMethods.add(MethodHolder.of(method, false));
         }
         currentClass = currentClass.getSuperclass();
      } while (!Object.class.equals(currentClass));
   }

   public Class<?> getInterceptorClass()
   {
      return interceptorClass;
   }

   public List<Method> getInterceptorMethods(InterceptionType interceptionType)
   {
      List<Method> methods = methodMap.get(interceptionType);
      return methods == null ? Collections.EMPTY_LIST : methods;
   }
   
   public boolean isInterceptor()
   {
      return hasInterceptorMethods;
   }

   private Object writeReplace()
   {
      return new InterceptorClassMetadataSerializationProxy(getInterceptorClass().getName());
   }

   static class InterceptorClassMetadataSerializationProxy implements Serializable
   {
      private String className;

      InterceptorClassMetadataSerializationProxy(String className)
      {
         this.className = className;
      }

      private Object readResolve()
      {
         try
         {
            return InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(ReflectionUtils.classForName(className));
         }
         catch (ClassNotFoundException e)
         {
            throw new InterceptorException("Failed to deserialize the interceptor class metadata", e);
         }
      }

   }


}
