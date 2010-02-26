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

package org.jboss.interceptor.proxy;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.Queue;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.io.Serializable;

import javax.interceptor.InvocationContext;

import org.jboss.interceptor.model.metadata.MethodReference;
import org.jboss.interceptor.util.ReflectionUtils;
import org.jboss.interceptor.InterceptorException;
import org.jboss.interceptor.model.InterceptionType;
import org.jboss.interceptor.model.InterceptorMetadata;

/**
 * @author Marius Bogoevici
 */
public abstract class AbstractClassInterceptionHandler implements InterceptionHandler, Serializable
{
   private InterceptorMetadata interceptorMetadata;

   public abstract Object getInterceptorInstance();

   public AbstractClassInterceptionHandler(InterceptorMetadata interceptorMetadata)
   {
      this.interceptorMetadata = interceptorMetadata;
   }


   public Object invoke(Object target, InterceptionType interceptionType, InvocationContext invocationContext) throws Exception
   {
      List<MethodReference> methods = interceptorMetadata.getInterceptorMethods(interceptionType);
      if (methods != null)
      {
         DelegatingInvocationContext delegatingInvocationContext = new DelegatingInvocationContext(invocationContext, getInterceptorInstance(), methods, interceptionType);
         return delegatingInvocationContext.proceed();
      }
      else
      {
         throw new InterceptorException(target.toString() + " was requested to perform " + interceptionType.name() + " but no such method is defined on it");
      }
   }

   public InterceptorMetadata getInterceptorMetadata()
   {
      return interceptorMetadata;
   }

   public class DelegatingInvocationContext implements InvocationContext
   {

      private InvocationContext delegateInvocationContext;

      private Object targetObject;
      private InterceptionType interceptionType;

      private Queue<MethodReference> invocationQueue;

      public DelegatingInvocationContext(InvocationContext delegateInvocationContext, Object targetObject, List<MethodReference> methods, InterceptionType interceptionType)
      {
         this.delegateInvocationContext = delegateInvocationContext;
         this.targetObject = targetObject;
         this.interceptionType = interceptionType;
         this.invocationQueue = new ConcurrentLinkedQueue<MethodReference>(methods);
      }

      public Map<String, Object> getContextData()
      {
         return delegateInvocationContext.getContextData();
      }

      public Method getMethod()
      {
         return delegateInvocationContext.getMethod();
      }

      public Object[] getParameters()
      {
         return delegateInvocationContext.getParameters();
      }

      public Object getTarget()
      {
         return delegateInvocationContext.getTarget();
      }

      public Object proceed() throws Exception
      {
         if (!invocationQueue.isEmpty())
         {
            try
            {
               if (AbstractClassInterceptionHandler.this.interceptorMetadata.isTargetClass() && interceptionType.isLifecycleCallback())
               {
                  Iterator<MethodReference> methodIterator = invocationQueue.iterator();
                  while (methodIterator.hasNext())
                  {
                     MethodReference interceptorMethod = methodIterator.next();
                     ReflectionUtils.ensureAccessible(interceptorMethod.getJavaMethod());
                     // interceptor methods defined on
                     interceptorMethod.getJavaMethod().invoke(targetObject);
                  }
                  return null;
               }
               else
               {
                  MethodReference interceptorMethod = invocationQueue.remove();
                  ReflectionUtils.ensureAccessible(interceptorMethod.getJavaMethod());
                  if (interceptorMethod.getJavaMethod().getParameterTypes().length == 0)
                  {
                     return interceptorMethod.getJavaMethod().invoke(targetObject);
                  }
                  else
                  {
                     return interceptorMethod.getJavaMethod().invoke(targetObject, this);
                  }
               }
            }
            catch (InvocationTargetException e)
            {
               if (e.getCause() instanceof Exception)
               {
                  throw (Exception) e.getCause();
               }
               else
               {
                  throw new InterceptorException(e);
               }
            }
         }
         else
         {
            return delegateInvocationContext.proceed();
         }
      }

      public void setParameters(Object[] params)
      {
         delegateInvocationContext.setParameters(params);
      }

      public Object getTimer()
      {
         return delegateInvocationContext.getTimer();
      }

   }
}
