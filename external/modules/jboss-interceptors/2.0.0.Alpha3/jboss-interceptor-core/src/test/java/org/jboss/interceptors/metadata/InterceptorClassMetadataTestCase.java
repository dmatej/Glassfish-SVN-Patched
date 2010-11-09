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

import static org.junit.Assert.assertEquals;

import java.util.List;

import junit.framework.Assert;

import org.jboss.interceptor.reader.cache.DefaultMetadataCachingReader;
import org.jboss.interceptor.reader.cache.MetadataCachingReader;
import org.jboss.interceptor.spi.metadata.ClassMetadata;
import org.jboss.interceptor.spi.metadata.InterceptorMetadata;
import org.jboss.interceptor.spi.metadata.MethodMetadata;
import org.jboss.interceptor.spi.model.InterceptionType;
import org.jboss.interceptor.util.InterceptorMetadataException;
import org.junit.Test;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorClassMetadataTestCase
{
   MetadataCachingReader metadataCachingReader = new DefaultMetadataCachingReader();

   @Test
   public void testInterceptorWithAllMethods()
   {
      InterceptorMetadata interceptorClassMetadata = metadataCachingReader.getInterceptorMetadata(InterceptorWithAllMethods.class);

      List<MethodMetadata> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(true, postConstructMethods.size() == 1);
      assertEquals(postConstructMethods.get(0).getJavaMethod().getName(), "doPostConstruct");

      List<MethodMetadata> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 1);
      assertEquals(preDestroyMethods.get(0).getJavaMethod().getName(), "doPreDestroy");

      List<MethodMetadata> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 1);
      assertEquals(aroundInvokeMethods.get(0).getJavaMethod().getName(), "doAroundInvoke");

      List<MethodMetadata> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 1);
      assertEquals(postActivateMethods.get(0).getJavaMethod().getName(), "doPostActivate");

      List<MethodMetadata> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 1);
      assertEquals(prePassivateMethods.get(0).getJavaMethod().getName(), "doPrePassivate");

   }

   @Test
   public void testInterceptorWithSomeMethods()
   {
      InterceptorMetadata interceptorClassMetadata = metadataCachingReader.getInterceptorMetadata(InterceptorWithSomeMethods.class);

      List<MethodMetadata> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(true, postConstructMethods.size() == 0);

      List<MethodMetadata> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 1);
      assertEquals(preDestroyMethods.get(0).getJavaMethod().getName(), "doPreDestroy");

      List<MethodMetadata> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 1);
      assertEquals(aroundInvokeMethods.get(0).getJavaMethod().getName(), "doAroundInvoke");

      List<MethodMetadata> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 1);
      assertEquals(postActivateMethods.get(0).getJavaMethod().getName(), "doPostActivate");

      List<MethodMetadata> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 0);

   }

   @Test
   public void testSimpleInheritance()
   {
      InterceptorMetadata interceptorClassMetadata = metadataCachingReader.getInterceptorMetadata(SimpleInheritanceChildInterceptor.class);

      List<MethodMetadata> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(1, postConstructMethods.size());
      assertEquals(postConstructMethods.get(0).getJavaMethod().getName(), "doPostConstruct");

      List<MethodMetadata> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 0);

      List<MethodMetadata> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 1);
      assertEquals(aroundInvokeMethods.get(0).getJavaMethod().getName(), "doAroundInvoke");

      List<MethodMetadata> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 0);

      List<MethodMetadata> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 0);

   }

   @Test
   public void testInheritanceWithAndWithoutOverriding()
   {
      InterceptorMetadata interceptorClassMetadata = metadataCachingReader.getInterceptorMetadata(OverrideChildInterceptor.class);

      List<MethodMetadata> postConstructMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_CONSTRUCT);
      assertEquals(true, postConstructMethods.size() == 1);
      assertEquals(postConstructMethods.get(0).getJavaMethod().getName(), "methodOverriddenAndUsedAsInterceptor");

      List<MethodMetadata> preDestroyMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_DESTROY);
      assertEquals(true, preDestroyMethods.size() == 0);

      List<MethodMetadata> aroundInvokeMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.AROUND_INVOKE);
      assertEquals(true, aroundInvokeMethods.size() == 2);
      assertEquals(aroundInvokeMethods.get(0).getJavaMethod().getName(), "methodDefinedOnParentAndUsedAsInterceptor");
      assertEquals(aroundInvokeMethods.get(1).getJavaMethod().getName(), "methodDefinedOnChildAndUsedAsInterceptor");

      List<MethodMetadata> postActivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.POST_ACTIVATE);
      assertEquals(true, postActivateMethods.size() == 0);


      List<MethodMetadata> prePassivateMethods = interceptorClassMetadata.getInterceptorMethods(InterceptionType.PRE_PASSIVATE);
      assertEquals(true, prePassivateMethods.size() == 0);

   }

   @Test(expected = InterceptorMetadataException.class)
   public void testDuplicateAnnotations()
   {
      InterceptorMetadata interceptorClassMetadata = metadataCachingReader.getInterceptorMetadata(InterceptorWithDuplicateAnnotations.class);

   }

   /**
    * Tests that the {@link InterceptorMetadata#isEligible(InterceptionType)} method works 
    * as expected against interceptors with different {@link InterceptionType}s
    */
   @Test
   public void testEligibilityForInterceptionType()
   {
      // test an interceptor which has interceptor methods for all InterceptionTypes
      InterceptorMetadata interceptorWithAllInterceptionTypes = metadataCachingReader.getInterceptorMetadata(InterceptorWithAllMethods.class);
      
      Assert.assertTrue("Interceptor was expected to be eligible for: " + InterceptionType.POST_CONSTRUCT, interceptorWithAllInterceptionTypes.isEligible(InterceptionType.POST_CONSTRUCT));
      Assert.assertTrue("Interceptor was expected to be eligible for: " + InterceptionType.PRE_DESTROY, interceptorWithAllInterceptionTypes.isEligible(InterceptionType.PRE_DESTROY));
      Assert.assertTrue("Interceptor was expected to be eligible for: " + InterceptionType.PRE_PASSIVATE, interceptorWithAllInterceptionTypes.isEligible(InterceptionType.PRE_PASSIVATE));
      Assert.assertTrue("Interceptor was expected to be eligible for: " + InterceptionType.POST_ACTIVATE, interceptorWithAllInterceptionTypes.isEligible(InterceptionType.POST_ACTIVATE));
      Assert.assertTrue("Interceptor was expected to be eligible for: " + InterceptionType.AROUND_INVOKE, interceptorWithAllInterceptionTypes.isEligible(InterceptionType.AROUND_INVOKE));
   
      // now test an interceptor which has interceptor methods for only a few InterceptionTypes
      InterceptorMetadata interceptorWithAroundInvokeAndPostConstruct = metadataCachingReader.getInterceptorMetadata(InterceptorWithPostConstructAndAroundInvoke.class);
      Assert.assertTrue("Interceptor was expected to be eligible for: " + InterceptionType.POST_CONSTRUCT, interceptorWithAroundInvokeAndPostConstruct.isEligible(InterceptionType.POST_CONSTRUCT));
      Assert.assertTrue("Interceptor was expected to be eligible for: " + InterceptionType.AROUND_INVOKE, interceptorWithAroundInvokeAndPostConstruct.isEligible(InterceptionType.AROUND_INVOKE));
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.PRE_DESTROY, interceptorWithAroundInvokeAndPostConstruct.isEligible(InterceptionType.PRE_DESTROY));
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.PRE_PASSIVATE, interceptorWithAroundInvokeAndPostConstruct.isEligible(InterceptionType.PRE_PASSIVATE));
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.POST_ACTIVATE, interceptorWithAroundInvokeAndPostConstruct.isEligible(InterceptionType.POST_ACTIVATE));
      
      
      // test with a simple class which isn't eligible for any of the interception types
      InterceptorMetadata notAnInterceptor = metadataCachingReader.getInterceptorMetadata(NotAnInterceptor.class);
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.POST_CONSTRUCT, notAnInterceptor.isEligible(InterceptionType.POST_CONSTRUCT));
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.AROUND_INVOKE, notAnInterceptor.isEligible(InterceptionType.AROUND_INVOKE));
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.PRE_DESTROY, notAnInterceptor.isEligible(InterceptionType.PRE_DESTROY));
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.PRE_PASSIVATE, notAnInterceptor.isEligible(InterceptionType.PRE_PASSIVATE));
      Assert.assertFalse("Interceptor was expected to be ineligible for: " + InterceptionType.POST_ACTIVATE, notAnInterceptor.isEligible(InterceptionType.POST_ACTIVATE));

      
   }

   /**
    * Tests that the {@link InterceptorMetadata#getInterceptorClass()} returns the correct
    * {@link ClassMetadata}
    */
   @Test
   public void testInterceptorClassMetaData()
   {
      InterceptorMetadata interceptorWithAllInterceptionTypes = metadataCachingReader.getInterceptorMetadata(InterceptorWithAllMethods.class);
      
      ClassMetadata<?> interceptorClass = interceptorWithAllInterceptionTypes.getInterceptorClass();
      Assert.assertNotNull("ClassMetadata not found on interceptor metadata created out of class: " + InterceptorWithAllMethods.class, interceptorClass);
      Assert.assertEquals("Unexpected ClassMetadata found on interceptor metadata created out of class: " + InterceptorWithAllMethods.class, InterceptorWithAllMethods.class.getName(), interceptorClass.getClassName());
   }

}
