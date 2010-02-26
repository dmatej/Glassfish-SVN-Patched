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

import javax.interceptor.InvocationContext;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.jboss.interceptor.model.InterceptionType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptionChain
{

   private final Logger log = LoggerFactory.getLogger(InterceptionChain.class);

   private Object target;

   private Object[] parameters;

   private Method targetMethod;

   private int currentPosition;

   private List<InterceptionHandler> interceptorHandlers;
   private final InterceptionType interceptionType;

   public InterceptionChain(List<InterceptionHandler> interceptorHandlers, InterceptionType interceptionType, Object target, Method targetMethod, Object[] parameters)
   {
      this.interceptorHandlers = interceptorHandlers;
      this.interceptionType = interceptionType;
      this.parameters = parameters;
      this.target = target;
      this.targetMethod = targetMethod;
      this.currentPosition = 0;
   }

   public Object invokeNext(InvocationContext invocationContext) throws Throwable
   {

      if (hasNext())
      {
         InterceptionHandler nextInterceptorHandler = interceptorHandlers.get(currentPosition++);
         log.debug("Invoking next interceptor in chain:" + nextInterceptorHandler.getClass().getName());
         return nextInterceptorHandler.invoke(target, interceptionType, invocationContext);
      }
      else
      {
         if (targetMethod != null)
         {
            try
            {
               return targetMethod.invoke(target, invocationContext.getParameters());
            }
            catch (InvocationTargetException e)
            {
               throw e.getCause();
            }
         }
         else
         {
            return null;
         }
      }
   }

   public boolean hasNext()
   {
      return currentPosition < interceptorHandlers.size();
   }


}
