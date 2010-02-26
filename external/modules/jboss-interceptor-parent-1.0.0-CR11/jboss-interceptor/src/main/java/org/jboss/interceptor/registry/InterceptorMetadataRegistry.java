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

package org.jboss.interceptor.registry;

import org.jboss.interceptor.model.metadata.AbstractInterceptorMetadata;
import org.jboss.interceptor.model.InterceptorMetadata;
import org.jboss.interceptor.model.metadata.ClassReference;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorMetadataRegistry<T>
{
   private final Map<Key, InterceptorMetadata> interceptorClassMetadataMap = new ConcurrentHashMap<Key, InterceptorMetadata>();

   private ClassMetadataReader classMetadataReader;

   private final Lock lock = new ReentrantLock();

   public InterceptorMetadataRegistry(ClassMetadataReader classMetadataReader)
   {
      this.classMetadataReader = classMetadataReader;
   }

   public InterceptorMetadata getInterceptorClassMetadata(ClassReference interceptorClass)
   {
      return this.getInterceptorClassMetadata(interceptorClass, false);
   }

   public InterceptorMetadata getInterceptorClassMetadata(ClassReference interceptorClass, boolean isInterceptorTargetClass)
   {
      Key key = new Key(interceptorClass, isInterceptorTargetClass);
      if (!interceptorClassMetadataMap.containsKey(key))
      {
         try
         {
            lock.lock();
            //verify that metadata hasn't been added while waiting for the lock
            if (!interceptorClassMetadataMap.containsKey(key))
            {
               interceptorClassMetadataMap.put(key, classMetadataReader.getInterceptorMetadata(interceptorClass, isInterceptorTargetClass));
            }
         }
         finally
         {
            lock.unlock();
         }
      }

      return interceptorClassMetadataMap.get(key);

   }

   public void cleanup()
   {
      this.interceptorClassMetadataMap.clear();
   }

   public static final class Key
   {
      private String className;

      private boolean isInterceptorTargetClass;

      private Key(ClassReference clazz, boolean interceptorTargetClass)
      {
         this.className = clazz.getClassName();
         isInterceptorTargetClass = interceptorTargetClass;
      }

      @Override
      public boolean equals(Object o)
      {
         if (this == o)
         {
            return true;
         }
         if (o == null || getClass() != o.getClass())
         {
            return false;
         }

         Key key = (Key) o;

         if (isInterceptorTargetClass != key.isInterceptorTargetClass)
         {
            return false;
         }
         if (className != null ? !className.equals(key.className) : key.className != null)
         {
            return false;
         }

         return true;
      }

      @Override
      public int hashCode()
      {
         int result = className != null ? className.hashCode() : 0;
         result = 31 * result + (isInterceptorTargetClass ? 1 : 0);
         return result;
      }
   }

}
