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

package org.jboss.interceptors.metadata;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import org.jboss.interceptor.model.InterceptorClassMetadata;
import org.jboss.interceptor.model.InterceptionType;
import org.jboss.interceptor.model.InterceptorMetadataException;
import org.jboss.interceptor.registry.InterceptorClassMetadataRegistry;
import org.jboss.interceptor.InterceptorException;

import java.lang.reflect.Method;
import java.util.List;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorClassMetadataTest
{

   @Test
   public void testInterceptorWithAllMethods()
   {
      InterceptorClassMetadata interceptorClassMetadata = InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(InterceptorWithAllMethods.class);

      List<Method> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(true, postConstructMethods.size() == 1);
      assertEquals(postConstructMethods.get(0).getName(), "doPostConstruct");

      List<Method> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 1);
      assertEquals(preDestroyMethods.get(0).getName(), "doPreDestroy");

      List<Method> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 1);
      assertEquals(aroundInvokeMethods.get(0).getName(), "doAroundInvoke");

      List<Method> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 1);
      assertEquals(postActivateMethods.get(0).getName(), "doPostActivate");


      List<Method> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 1);
      assertEquals(prePassivateMethods.get(0).getName(), "doPrePassivate");

   }

   @Test
   public void testInterceptorWithSomeMethods()
   {
      InterceptorClassMetadata interceptorClassMetadata = InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(InterceptorWithSomeMethods.class);

      List<Method> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(true, postConstructMethods.size() == 0);

      List<Method> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 1);
      assertEquals(preDestroyMethods.get(0).getName(), "doPreDestroy");

      List<Method> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 1);
      assertEquals(aroundInvokeMethods.get(0).getName(), "doAroundInvoke");

      List<Method> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 1);
      assertEquals(postActivateMethods.get(0).getName(), "doPostActivate");


      List<Method> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 0);

   }

   @Test
   public void testSimpleInheritance()
   {
      InterceptorClassMetadata interceptorClassMetadata = InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(SimpleInheritanceChildInterceptor.class);

      List<Method> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(true, postConstructMethods.size() == 1);
      assertEquals(postConstructMethods.get(0).getName(), "doPostConstruct");

      List<Method> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 0);

      List<Method> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 1);
      assertEquals(aroundInvokeMethods.get(0).getName(), "doAroundInvoke");

      List<Method> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 0);

      List<Method> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 0);

   }

   @Test
   public void testInheritanceWithAndWithoutOverriding()
   {
      InterceptorClassMetadata interceptorClassMetadata = InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(OverrideChildInterceptor.class);

      List<Method> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(true, postConstructMethods.size() == 1);
      assertEquals(postConstructMethods.get(0).getName(), "methodOverriddenAndUsedAsInterceptor");

      List<Method> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 0);

      List<Method> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 2);
      assertEquals(aroundInvokeMethods.get(0).getName(), "methodDefinedOnParentAndUsedAsInterceptor");
      assertEquals(aroundInvokeMethods.get(1).getName(), "methodDefinedOnChildAndUsedAsInterceptor");

      List<Method> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 0);


      List<Method> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 0);

   }

   @Test(expected = InterceptorMetadataException.class)
   public void testDuplicateAnnotations()
   {
      InterceptorClassMetadata interceptorClassMetadata = InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(InterceptorWithDuplicateAnnotations.class);

   }



}
