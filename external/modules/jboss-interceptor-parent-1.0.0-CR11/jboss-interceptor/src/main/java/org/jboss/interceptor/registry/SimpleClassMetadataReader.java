/*
 * JBoss, Home of Professional Open Source
 * Copyright 2010, Red Hat, Inc. and/or its affiliates, and individual
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

package org.jboss.interceptor.registry;

import org.jboss.interceptor.InterceptorException;
import org.jboss.interceptor.model.InterceptorMetadata;
import org.jboss.interceptor.model.metadata.AbstractInterceptorMetadata;
import org.jboss.interceptor.model.metadata.AbstractInterceptorMetadataSerializationProxy;
import org.jboss.interceptor.model.metadata.ClassReference;
import org.jboss.interceptor.model.metadata.ReflectiveClassReference;
import org.jboss.interceptor.util.ReflectionUtils;

/**
 * @author Marius Bogoevici
 */
public class SimpleClassMetadataReader implements ClassMetadataReader
{
   private static SimpleClassMetadataReader instance = new SimpleClassMetadataReader();

   public InterceptorMetadata getInterceptorMetadata(ClassReference clazz, final boolean isInterceptorTargetClass)
   {
      return new AbstractInterceptorMetadata(clazz, isInterceptorTargetClass)
      {
         @Override
         protected Object createSerializableProxy()
         {
            return new SimpleInterceptorMetadataSerializationProxy(getInterceptorClass().getClassName(), isTargetClass());
         }

         private Object writeReplace()
         {
            return createSerializableProxy();
         }

      };
   }

   public static ClassMetadataReader getInstance()
   {
      return instance;
   }

   private static class SimpleInterceptorMetadataSerializationProxy extends AbstractInterceptorMetadataSerializationProxy
   {
      public SimpleInterceptorMetadataSerializationProxy(String className, boolean targetClass)
      {
         super(className, targetClass);
      }

      @Override
      protected InterceptorMetadata loadInterceptorMetadata() throws ClassNotFoundException
      {
         Class<?> clazz = ReflectionUtils.classForName(getClassName());
         return SimpleClassMetadataReader.instance.getInterceptorMetadata(ReflectiveClassReference.of(clazz), isInterceptionTargetClass());
      }

      private Object readResolve()
      {
         try
         {
            return loadInterceptorMetadata();
         }
         catch (ClassNotFoundException e)
         {
            throw new InterceptorException("Failed to deserialize the interceptor class metadata", e);
         }
      }

   }
}
