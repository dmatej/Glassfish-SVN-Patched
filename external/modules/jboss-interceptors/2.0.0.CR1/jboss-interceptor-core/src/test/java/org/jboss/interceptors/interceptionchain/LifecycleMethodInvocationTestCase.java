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
package org.jboss.interceptors.interceptionchain;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashSet;

import javax.interceptor.InvocationContext;

import junit.framework.Assert;

import org.jboss.interceptor.proxy.DefaultInvocationContextFactory;
import org.jboss.interceptor.proxy.InterceptorInvocation;
import org.jboss.interceptor.proxy.SimpleInterceptionChain;
import org.jboss.interceptor.reader.cache.DefaultMetadataCachingReader;
import org.jboss.interceptor.reader.cache.MetadataCachingReader;
import org.jboss.interceptor.spi.context.InterceptionChain;
import org.jboss.interceptor.spi.metadata.InterceptorMetadata;
import org.jboss.interceptor.spi.model.InterceptionType;
import org.junit.Test;

/**
 * Tests that the lifecylce method invocations on interceptors
 * and target instances work as expected
 *
 * @author Jaikiran Pai
 * @version $Revision: $
 */
public class LifecycleMethodInvocationTestCase
{

   MetadataCachingReader metadataCachingReader = new DefaultMetadataCachingReader();
   /**
    * The interceptors spec allows the @PostConstruct method to have public, private, protected, or package level access. 
    * This test ensures that non-public @PostConstruct methods can be invoked during an interceptor invocation
    */
   @Test
   public void testNonPublicPostConstructMethod() throws Exception
   {
      // create the interceptor invocation for @PostConstruct
      InterceptorMetadata interceptorMetaData = metadataCachingReader.getInterceptorMetadata(SimpleInterceptor.class);
      SimpleInterceptor interceptor = new SimpleInterceptor();
      InterceptorInvocation<?> interceptorInvocation = new InterceptorInvocation(interceptor, interceptorMetaData, InterceptionType.POST_CONSTRUCT);
      Collection<InterceptorInvocation<?>> interceptorInvocations = new HashSet<InterceptorInvocation<?>>();
      interceptorInvocations.add(interceptorInvocation);

      // create a interception chain for the bean and the interceptors
      SimpleBean bean = new SimpleBean();
      Method postConstructMethod = bean.getClass().getDeclaredMethod("onConstruct", null);
      InterceptionChain interceptorChain = new SimpleInterceptionChain(interceptorInvocations, InterceptionType.POST_CONSTRUCT, bean, postConstructMethod);
      DefaultInvocationContextFactory invocationCtxFactory = new DefaultInvocationContextFactory();
      InvocationContext invocationCtx = invocationCtxFactory.newInvocationContext(interceptorChain, bean, postConstructMethod, null);
      // invoke post-construct
      invocationCtx.proceed();
      
      // test post-construct invocation on the target object
      Assert.assertTrue("@PostConstruct was not invoked on " + SimpleBean.class, bean.wasPostConstructInvoked());
      // test post construct invocation on the interceptor instance
      Assert.assertTrue("@PostConstruct was not invoked on " + SimpleInterceptor.class, interceptor.wasPostConstructInvoked());
   }

   
   
}
