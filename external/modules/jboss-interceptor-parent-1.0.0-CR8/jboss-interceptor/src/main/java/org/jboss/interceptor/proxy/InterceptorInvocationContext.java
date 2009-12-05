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

import org.jboss.interceptor.InterceptorException;

import javax.interceptor.InvocationContext;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.lang.reflect.Method;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorInvocationContext implements InvocationContext
{

   private Map<String, Object> contextData = new HashMap<String, Object>();

   private Method method;

   private Object[] parameters;
   
   private Object target;

   private InterceptionChain interceptionChain;

   private Object timer;
   private static List<Class<?>> WIDENING_SEQUENCE = Arrays.<Class<?>>asList(byte.class, short.class, int.class, long.class, float.class, double.class);

   private static Map<Class<?>, Class<?>> WRAPPER_CLASSES;
   private static Map<Class<?>, Class<?>> REVERSE_WRAPPER_CLASSES;

   static
   {
      WRAPPER_CLASSES = new HashMap<Class<?>, Class<?>>();
      WRAPPER_CLASSES.put(boolean.class, Boolean.class);
      WRAPPER_CLASSES.put(byte.class, Byte.class);
      WRAPPER_CLASSES.put(char.class, Character.class);
      WRAPPER_CLASSES.put(short.class, Short.class);
      WRAPPER_CLASSES.put(int.class, Integer.class);
      WRAPPER_CLASSES.put(long.class, Long.class);
      WRAPPER_CLASSES.put(float.class, Float.class);
      WRAPPER_CLASSES.put(double.class, Double.class);

      WRAPPER_CLASSES = Collections.unmodifiableMap(WRAPPER_CLASSES);

      REVERSE_WRAPPER_CLASSES = new HashMap<Class<?>, Class<?>>();
      for (Map.Entry<Class<?>, Class<?>> classEntry: WRAPPER_CLASSES.entrySet())
      {
         REVERSE_WRAPPER_CLASSES.put(classEntry.getValue(), classEntry.getKey());
      }

      REVERSE_WRAPPER_CLASSES = Collections.unmodifiableMap(REVERSE_WRAPPER_CLASSES);
   }

   public InterceptorInvocationContext(InterceptionChain interceptionChain, Object target, Method targetMethod, Object[] parameters)
   {
      this.interceptionChain = interceptionChain;
      this.method = targetMethod;
      this.parameters = parameters;
      this.target = target;
      this.timer = null;
   }

   public InterceptorInvocationContext(InterceptionChain interceptionChain, Object target, Method targetMethod, Object timer)
   {
      this.interceptionChain = interceptionChain;
      this.method = targetMethod;
      this.timer = timer;
      this.target = target;
      this.parameters = null;
   }

   public Map<String, Object> getContextData()
   {
      return contextData;
   }

   public Method getMethod()
   {
      return method;
   }

   public Object[] getParameters()
   {
      return parameters;
   }

   public Object getTarget()
   {
      return target;
   }

   public Object proceed() throws Exception
   {
      try
      {
         return interceptionChain.invokeNext(this);
      }
      catch (Exception e)
      {
         throw e;
      }
      catch (Throwable t)
      {
         throw new InterceptorException(t);
      }
   }

   private static boolean isWideningPrimitive(Class argumentClass, Class targetClass)
   {
      int argumentClassIndex = WIDENING_SEQUENCE.indexOf(argumentClass);
      return argumentClassIndex >= 0 && WIDENING_SEQUENCE.indexOf(targetClass) >= argumentClassIndex;
   }

   private static Class<?> getWrapperClass(Class<?> primitiveClass)
   {
      if (!WRAPPER_CLASSES.containsKey(primitiveClass))
      {
         return primitiveClass;
      }
      else
      {
         return WRAPPER_CLASSES.get(primitiveClass);
      }
   }

   private static Class<?> getPrimitiveClass(Class<?> wrapperClass)
   {
      if (!REVERSE_WRAPPER_CLASSES.containsKey(wrapperClass))
      {
         return wrapperClass;
      }
      else
      {
         return REVERSE_WRAPPER_CLASSES.get(wrapperClass);
      }
   }

   public void setParameters(Object[] params)
   {
      if (method != null)
      {
         // there is no requirement to do anything if params is null
         // but this is theoretically possible only if the target method has no arguments
         int newParametersCount = params == null? 0 : params.length;
         if (method.getParameterTypes().length != newParametersCount)
            throw new IllegalArgumentException("Wrong number of parameters: method has " + method.getParameterTypes().length
                  + ", attempting to set " + newParametersCount + (params != null?"": " (argument was null)"));
         if (params != null)
         {
            for (int i=0; i<params.length; i++)
            {
               Class<?> methodParameterClass = method.getParameterTypes()[i];
               if (params[i] != null)
               {
                  //identity ok
                  Class<? extends Object> newArgumentClass = params[i].getClass();
                  if (newArgumentClass.equals(methodParameterClass))
                     break;
                  if (newArgumentClass.isPrimitive())
                  {
                     // argument is primitive - never actually a case for interceptors
                     if (methodParameterClass.isPrimitive())
                     {
                        //widening primitive
                        if (!isWideningPrimitive(newArgumentClass, methodParameterClass))
                        {
                           throw new IllegalArgumentException("Incompatible parameter type on position: " + i + " :" + newArgumentClass + " (expected type was " + methodParameterClass.getName() + ")");
                        }
                     }
                     else
                     {
                        //boxing+widening reference
                        Class<?> boxedArgumentClass = getWrapperClass(newArgumentClass);
                        if (!methodParameterClass.isAssignableFrom(boxedArgumentClass))
                        {
                           throw new IllegalArgumentException("Incompatible parameter type on position: " + i + " :" + newArgumentClass + " (expected type was " + methodParameterClass.getName() + ")");
                        }
                     }
                  }
                  else
                  {
                     // argument is non-primitive
                     if (methodParameterClass.isPrimitive())
                     {
                        // unboxing+widening primitive
                        Class<?> unboxedClass = getPrimitiveClass(newArgumentClass);
                        if (!isWideningPrimitive(unboxedClass, methodParameterClass))
                        {
                           throw new IllegalArgumentException("Incompatible parameter type on position: " + i + " :" + newArgumentClass + " (expected type was " + methodParameterClass.getName() + ")");
                        }
                     }
                     else
                     {
                        //widening reference
                        if (!methodParameterClass.isAssignableFrom(newArgumentClass))
                        {
                           throw new IllegalArgumentException("Incompatible parameter type on position: " + i + " :" + newArgumentClass + " (expected type was " + methodParameterClass.getName() + ")");
                        }
                     }
                 }
               }
               else
               {
                  // null is never acceptable on a primitive type
                  if (method.getParameterTypes()[i].isPrimitive())
                  {
                     throw new IllegalArgumentException("Trying to set a null value on a " + method.getParameterTypes()[i].getName());
                  }
               }
            }
            this.parameters = params;
         }
      }
      else
      {
         throw new IllegalStateException("Illegal invocation to setParameters() during lifecycle invocation");
      }
   }

   public Object getTimer()
   {
      return timer;
   }

}
