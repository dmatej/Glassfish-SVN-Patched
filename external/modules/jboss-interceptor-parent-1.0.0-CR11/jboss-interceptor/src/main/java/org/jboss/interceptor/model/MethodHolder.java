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
import java.util.Arrays;
import java.io.Serializable;

import org.jboss.interceptor.model.metadata.MethodReference;
import org.jboss.interceptor.util.ReflectionUtils;
import org.jboss.interceptor.InterceptorException;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class MethodHolder implements Serializable
{
   private String methodName;

   private Class<?>[] parameterTypes;

   private Class<?> declaringClass;


   public static MethodHolder of(Method method, boolean withDeclaringClass)
   {
      return new MethodHolder(method, withDeclaringClass);
   }

   public static MethodHolder of(MethodReference method, boolean withDeclaringClass)
   {
      return new MethodHolder(method.getJavaMethod(), withDeclaringClass);
   }

   private MethodHolder(Method method, boolean withDeclaringClass)
   {
      this.methodName = method.getName();
      this.parameterTypes = method.getParameterTypes();
      if (withDeclaringClass)
         this.declaringClass = method.getDeclaringClass();
   }

   private MethodHolder(String methodName, Class<?>[] parameterTypes, Class<?> declaringClass)
   {
      this.methodName = methodName;
      this.parameterTypes = parameterTypes;
      this.declaringClass = declaringClass;
   }

   @Override
   public boolean equals(Object o)
   {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;

      MethodHolder that = (MethodHolder) o;

      if (declaringClass != null ? !declaringClass.equals(that.declaringClass) : that.declaringClass != null)
         return false;
      if (methodName != null ? !methodName.equals(that.methodName) : that.methodName != null) return false;
      if (!Arrays.equals(parameterTypes, that.parameterTypes)) return false;

      return true;
   }

   @Override
   public int hashCode()
   {
      int result = methodName != null ? methodName.hashCode() : 0;
      result = 31 * result + (parameterTypes != null ? Arrays.hashCode(parameterTypes) : 0);
      result = 31 * result + (declaringClass != null ? declaringClass.hashCode() : 0);
      return result;
   }

   private Object writeReplace()
   {
      return new MethodHolderSerializationProxy(this);
   }

   static class MethodHolderSerializationProxy implements Serializable
   {
      private String className;
      private String methodName;
      private String parameterClassNames[];

      MethodHolderSerializationProxy(MethodHolder methodHolder)
      {
         className = methodHolder.declaringClass != null? methodHolder.declaringClass.getName() : null;
         methodName = methodHolder.methodName;
         if (methodHolder.parameterTypes != null)
         {
            parameterClassNames = new String[methodHolder.parameterTypes.length];
            int i = 0;
            for (Class<?> parameterType: methodHolder.parameterTypes)
            {
               parameterClassNames[i++] = parameterType.getName();
            }
         }
      }
      
      private Object readResolve()
      {

         try
         {
            Class<?>[] parameterTypes = null;
            if (parameterClassNames != null)
            {
               parameterTypes = new Class<?>[parameterClassNames.length];
               for (int i = 0; i<parameterClassNames.length; i++)
               {
                  parameterTypes[i] = ReflectionUtils.classForName(parameterClassNames[i]);
               }
            }
            Class<?> declaringClass = null;
            if (className != null)
            {
               declaringClass = ReflectionUtils.classForName(className);
            }
            return new MethodHolder(methodName, parameterTypes, declaringClass);
         }
         catch (ClassNotFoundException e)
         {
            throw new InterceptorException("Error while deserializing intercepted instance", e);
         }
      }
   }
}
