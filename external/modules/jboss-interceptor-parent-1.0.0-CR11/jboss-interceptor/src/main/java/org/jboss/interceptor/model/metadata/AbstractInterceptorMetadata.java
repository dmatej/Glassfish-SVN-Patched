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

package org.jboss.interceptor.model.metadata;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jboss.interceptor.model.InterceptionType;
import org.jboss.interceptor.model.InterceptionTypeRegistry;
import org.jboss.interceptor.model.InterceptorMetadata;
import org.jboss.interceptor.model.InterceptorMetadataException;
import org.jboss.interceptor.model.MethodHolder;
import org.jboss.interceptor.model.metadata.ClassReference;
import org.jboss.interceptor.model.metadata.MethodReference;
import org.jboss.interceptor.util.InterceptionUtils;
import org.jboss.interceptor.util.ReflectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public abstract class AbstractInterceptorMetadata implements InterceptorMetadata, Serializable
{

   private Logger log = LoggerFactory.getLogger(AbstractInterceptorMetadata.class);
   private ClassReference interceptorClass;
   private Map<InterceptionType, List<MethodReference>> methodMap;
   private boolean targetClass;

   protected AbstractInterceptorMetadata(ClassReference interceptorClass, boolean targetClass)
   {
      this.interceptorClass = interceptorClass;
      this.methodMap = buildMethodMap(interceptorClass, targetClass);
      this.targetClass = targetClass;
   }

   private Map<InterceptionType, List<MethodReference>> buildMethodMap(ClassReference interceptorClass, boolean isTargetClass)
   {
      Map<InterceptionType, List<MethodReference>> methodMap = new HashMap<InterceptionType, List<MethodReference>>();
      ClassReference currentClass = interceptorClass;
      Set<MethodHolder> foundMethods = new HashSet<MethodHolder>();
      do
      {
         Set<InterceptionType> detectedInterceptorTypes = new HashSet<InterceptionType>();

         for (MethodReference method : currentClass.getDeclaredMethods())
         {
            for (InterceptionType interceptionType : InterceptionTypeRegistry.getSupportedInterceptionTypes())
            {
               if (InterceptionUtils.isInterceptorMethod(interceptionType, method, isTargetClass))
               {
                  if (methodMap.get(interceptionType) == null)
                  {
                     methodMap.put(interceptionType, new LinkedList<MethodReference>());
                  }
                  if (detectedInterceptorTypes.contains(interceptionType))
                  {
                     throw new InterceptorMetadataException("Same interception type cannot be specified twice on the same class");
                  }
                  else
                  {
                     detectedInterceptorTypes.add(interceptionType);
                  }
                  // add method in the list - if it is there already, it means that it has been added by a subclass
                  ReflectionUtils.ensureAccessible(method.getJavaMethod());
                  if (!foundMethods.contains(MethodHolder.of(method, false)))
                  {
                     methodMap.get(interceptionType).add(0, method);
                  }
               }
            }
            foundMethods.add(MethodHolder.of(method, false));
         }
         currentClass = currentClass.getSuperclass();
      }
      while (!Object.class.equals(currentClass.getJavaClass()));
      return methodMap;
   }

   public ClassReference getInterceptorClass()
   {
      return interceptorClass;
   }

   public List<MethodReference> getInterceptorMethods(InterceptionType interceptionType)
   {
      if (methodMap != null)
      {
         List<MethodReference> methods = methodMap.get(interceptionType);
         return methods == null ? Collections.<MethodReference>emptyList() : methods;
      }
      else
      {
         return Collections.<MethodReference>emptyList();
      }
   }

   public boolean isInterceptor()
   {
      return !methodMap.keySet().isEmpty();
   }

   private Object writeReplace()
   {
      return createSerializableProxy();
   }

   protected abstract Object createSerializableProxy();

   public boolean isTargetClass()
   {
      return targetClass;
   }


}
