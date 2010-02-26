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

import org.jboss.interceptor.InterceptorException;

import java.lang.reflect.Method;
import java.util.*;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptionModelImpl<T, I> implements InterceptionModel<T, I>
{

   private Map<InterceptionType, List<I>> globalInterceptors = new HashMap<InterceptionType, List<I>>();

   private Map<InterceptionType, Map<MethodHolder, List<I>>> methodBoundInterceptors = new HashMap<InterceptionType, Map<MethodHolder, List<I>>>();

   private Set<MethodHolder> methodsIgnoringGlobals = new HashSet<MethodHolder>();

   private Set<I> allInterceptors = new LinkedHashSet<I>();

   private T interceptedEntity;

   public InterceptionModelImpl(T interceptedEntity)
   {
      this.interceptedEntity = interceptedEntity;
   }

   public List<I> getInterceptors(InterceptionType interceptionType, Method method)
   {
      if (interceptionType.isLifecycleCallback() && method != null)
      {
         throw new IllegalArgumentException("On a lifecycle callback, the associated method must be null");
      }

      if (!interceptionType.isLifecycleCallback() && method == null)
      {
         throw new IllegalArgumentException("Around-invoke and around-timeout interceptors are defined for a given method");
      }

      if (interceptionType.isLifecycleCallback())
      {
         if (globalInterceptors.containsKey(interceptionType))
         {
            return globalInterceptors.get(interceptionType);
         }
      }
      else
      {
         ArrayList<I> returnedInterceptors = new ArrayList<I>();
         if (!methodsIgnoringGlobals.contains(methodHolder(method)) && globalInterceptors.containsKey(interceptionType))
         {
            returnedInterceptors.addAll(globalInterceptors.get(interceptionType));
         }
         if (methodBoundInterceptors.containsKey(interceptionType) && methodBoundInterceptors.get(interceptionType).containsKey(methodHolder(method)))
         {
            returnedInterceptors.addAll(methodBoundInterceptors.get(interceptionType).get(methodHolder(method)));
         }
         return returnedInterceptors;
      }
      return Collections.EMPTY_LIST;
   }

   public Set<I> getAllInterceptors()
   {
      return Collections.unmodifiableSet(allInterceptors);
   }

   public T getInterceptedEntity()
   {
      return this.interceptedEntity;
   }

   public void setIgnoresGlobals(Method method, boolean ignoresGlobals)
   {
      if (ignoresGlobals)
      {
         methodsIgnoringGlobals.add(methodHolder(method));
      }
      else
      {
         methodsIgnoringGlobals.remove(methodHolder(method));
      }
   }

   public void appendInterceptors(InterceptionType interceptionType, Method method, I... interceptors)
   {
      if (null == method)
      {
         List<I> interceptorsList = globalInterceptors.get(interceptionType);
         if (interceptorsList == null)
         {
            interceptorsList = new ArrayList<I>();
            globalInterceptors.put(interceptionType, interceptorsList);
         }
         appendInterceptorClassesToList(interceptionType, interceptorsList, interceptors);
      }
      else
      {
         if (null == methodBoundInterceptors.get(interceptionType))
         {
            methodBoundInterceptors.put(interceptionType, new HashMap<MethodHolder, List<I>>());
         }
         List<I> interceptorsList = methodBoundInterceptors.get(interceptionType).get(methodHolder(method));
         if (interceptorsList == null)
         {
            interceptorsList = new ArrayList<I>();
            methodBoundInterceptors.get(interceptionType).put(methodHolder(method), interceptorsList);
         }
         if (globalInterceptors.containsKey(interceptionType))
         {
            validateDuplicateInterceptors(interceptionType, globalInterceptors.get(interceptionType), interceptors);
         }
         appendInterceptorClassesToList(interceptionType, interceptorsList, interceptors);
      }
      allInterceptors.addAll(Arrays.asList(interceptors));
   }

   private void appendInterceptorClassesToList(InterceptionType interceptionType, List<I> interceptorsList, I... interceptors)
   {
      validateDuplicateInterceptors(interceptionType, interceptorsList, interceptors);
      interceptorsList.addAll(Arrays.asList(interceptors));
   }

   private void validateDuplicateInterceptors(InterceptionType interceptionType, List<I> interceptorsList, I[] interceptors)
   {
      for (I interceptor : interceptors)
      {
         if (interceptorsList.contains(interceptor))
         {
            if (interceptionType != null)
            {
               throw new InterceptorException("Duplicate interceptor class definition when binding" + interceptor + " on " + interceptionType.name());
            }
         }
      }
   }

   private static MethodHolder methodHolder(Method method)
   {
      return MethodHolder.of(method, true);
   }

}
