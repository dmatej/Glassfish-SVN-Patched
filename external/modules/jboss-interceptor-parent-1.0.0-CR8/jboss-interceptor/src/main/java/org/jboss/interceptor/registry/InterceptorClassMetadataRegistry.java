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

import org.jboss.interceptor.model.InterceptorClassMetadataImpl;
import org.jboss.interceptor.model.InterceptorClassMetadata;

import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorClassMetadataRegistry
{
   private static InterceptorClassMetadataRegistry interceptorMetadataRegistry;

   private final Map<Class<?>, InterceptorClassMetadata> interceptorClassMetadataMap = new ConcurrentHashMap<Class<?>, InterceptorClassMetadata>();

   private final Lock lock = new ReentrantLock();

   static
   {
      interceptorMetadataRegistry = new InterceptorClassMetadataRegistry();
   }

   public static InterceptorClassMetadataRegistry getRegistry()
   {
      return interceptorMetadataRegistry;
   }

   public InterceptorClassMetadata getInterceptorClassMetadata(Class<?> interceptorClass)
   {
      if (!interceptorClassMetadataMap.containsKey(interceptorClass))
      {
         try
         {
            lock.lock();
            //verify that metadata hasn't been added while waiting for the lock
            if (!interceptorClassMetadataMap.containsKey(interceptorClass))
            {
               interceptorClassMetadataMap.put(interceptorClass, new InterceptorClassMetadataImpl(interceptorClass));
            }
         }
         finally 
         {
            lock.unlock();
         }
      }

      return interceptorClassMetadataMap.get(interceptorClass);

   }

}
