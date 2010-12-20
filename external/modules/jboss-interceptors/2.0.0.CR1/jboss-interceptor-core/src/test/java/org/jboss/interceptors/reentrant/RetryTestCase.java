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

package org.jboss.interceptors.reentrant;

import org.jboss.interceptor.builder.InterceptionModelBuilder;
import org.jboss.interceptor.proxy.DefaultInvocationContextFactory;
import org.jboss.interceptor.proxy.DirectClassInterceptorInstantiator;
import org.jboss.interceptor.proxy.InterceptorProxyCreatorImpl;
import org.jboss.interceptor.reader.ReflectiveClassMetadata;
import org.jboss.interceptor.reader.cache.DefaultMetadataCachingReader;
import org.jboss.interceptor.reader.cache.MetadataCachingReader;
import org.jboss.interceptor.spi.metadata.ClassMetadata;
import org.jboss.interceptor.spi.model.InterceptionModel;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author Marius Bogoevici
 */
public class RetryTestCase
{
   MetadataCachingReader metadataCachingReader = new DefaultMetadataCachingReader();

   @Test
   public void testClassOnly()
   {
      ClassMetadata<SimpleSelfInterceptingClass> classMetadata = ReflectiveClassMetadata.of(SimpleSelfInterceptingClass.class);
      InterceptionModelBuilder<ClassMetadata<?>, ?> builder =
             InterceptionModelBuilder.<ClassMetadata<?>>newBuilderFor(classMetadata);

      InterceptionModel<ClassMetadata<?>,?> classMetadataInterceptionModel = builder.build();

      InterceptorProxyCreatorImpl ipc = new InterceptorProxyCreatorImpl(new DirectClassInterceptorInstantiator(), new DefaultInvocationContextFactory(), classMetadataInterceptionModel);
      SimpleSelfInterceptingClass subclassingProxy = ipc.createSubclassingProxy(classMetadata, new Class<?>[]{int.class}, new Object[]{Integer.valueOf(3)});
      subclassingProxy.doSomething();
      Assert.assertEquals(2, subclassingProxy.getInterceptionsCount()); // the interception is retried twice
      Assert.assertEquals(3, subclassingProxy.getTries()); // the actual method is invoked three times
   }

   @Test
   public void testClassWithInterceptor()
   {
      ClassMetadata<SimpleClass> classMetadata = ReflectiveClassMetadata.of(SimpleClass.class);
      InterceptionModelBuilder<ClassMetadata<?>, ?> builder =
             InterceptionModelBuilder.<ClassMetadata<?>>newBuilderFor(classMetadata);
      builder.interceptAll().with(metadataCachingReader.getInterceptorMetadata(RetriableInterceptor.class));
      builder.interceptAll().with(metadataCachingReader.getInterceptorMetadata(InternalInterceptor.class));

      InterceptionModel<ClassMetadata<?>,?> classMetadataInterceptionModel = builder.build();

      InterceptorProxyCreatorImpl ipc = new InterceptorProxyCreatorImpl(new DirectClassInterceptorInstantiator(), new DefaultInvocationContextFactory(), classMetadataInterceptionModel);
      SimpleClass subclassingProxy = ipc.createSubclassingProxy(classMetadata, new Class<?>[]{int.class}, new Object[]{Integer.valueOf(3)});
      subclassingProxy.doSomething();
      Assert.assertEquals(2, RetriableInterceptor.interceptionsCount); // the interception is retried twice
      Assert.assertEquals(3, InternalInterceptor.interceptionsCount); // the interception is retried twice
      Assert.assertEquals(3, subclassingProxy.getTries()); // the actual method is invoked three times
   }
}
