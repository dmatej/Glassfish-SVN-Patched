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

package org.jboss.interceptors.proxy;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javassist.util.proxy.MethodHandler;
import org.jboss.interceptor.model.InterceptionModel;
import org.jboss.interceptor.model.InterceptionModelBuilder;
import org.jboss.interceptor.model.metadata.ReflectiveClassReference;
import org.jboss.interceptor.proxy.DirectClassInterceptionHandlerFactory;
import org.jboss.interceptor.proxy.InterceptorProxyCreator;
import org.jboss.interceptor.proxy.InterceptorProxyCreatorImpl;
import org.jboss.interceptor.registry.InterceptorMetadataRegistry;
import org.jboss.interceptor.registry.InterceptorRegistry;
import org.jboss.interceptor.registry.SimpleClassMetadataReader;
import org.jboss.interceptor.util.InterceptionUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptionTestCase
{
   private static final String TEAM_NAME = "Ajax Amsterdam";

   private String[] expectedLoggedValues = {
         "org.jboss.interceptors.proxy.FirstInterceptor_postConstruct",
         "org.jboss.interceptors.proxy.Team_postConstruct",
         "org.jboss.interceptors.proxy.FootballTeam_postConstruct",
         "org.jboss.interceptors.proxy.FirstInterceptor_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.SecondInterceptor_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.FootballTeam_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.FootballTeam_getName",
         "org.jboss.interceptors.proxy.FootballTeam_aroundInvokeAfter",
         "org.jboss.interceptors.proxy.SecondInterceptor_aroundInvokeAfter",
         "org.jboss.interceptors.proxy.FirstInterceptor_aroundInvokeAfter",
         "org.jboss.interceptors.proxy.SecondInterceptor_preDestroy"
   };

   private String[] expectedLoggedValuesWithGlobalsIgnored = {
         "org.jboss.interceptors.proxy.FirstInterceptor_postConstruct",
         "org.jboss.interceptors.proxy.Team_postConstruct",
         "org.jboss.interceptors.proxy.FootballTeam_postConstruct",
         "org.jboss.interceptors.proxy.SecondInterceptor_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.FootballTeam_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.FootballTeam_getName",
         "org.jboss.interceptors.proxy.FootballTeam_aroundInvokeAfter",
         "org.jboss.interceptors.proxy.SecondInterceptor_aroundInvokeAfter",
         "org.jboss.interceptors.proxy.SecondInterceptor_preDestroy"
   };

   private String[] expectedLoggedValuesOnSerialization = {
         "org.jboss.interceptors.proxy.FirstInterceptor_postConstruct",
         "org.jboss.interceptors.proxy.Team_postConstruct",
         "org.jboss.interceptors.proxy.FootballTeam_postConstruct",
         "org.jboss.interceptors.proxy.FootballTeam_prePassivating",
         "org.jboss.interceptors.proxy.FootballTeam_postActivating",
         "org.jboss.interceptors.proxy.FirstInterceptor_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.SecondInterceptor_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.FootballTeam_aroundInvokeBefore",
         "org.jboss.interceptors.proxy.FootballTeam_getName",
         "org.jboss.interceptors.proxy.FootballTeam_aroundInvokeAfter",
         "org.jboss.interceptors.proxy.SecondInterceptor_aroundInvokeAfter",
         "org.jboss.interceptors.proxy.FirstInterceptor_aroundInvokeAfter",
   };

   private String[] expectedLoggedValuesWhenRaw = {
         "org.jboss.interceptors.proxy.FootballTeam_getName",
   };

   private InterceptorRegistry<Class<?>, Class<?>> interceptorRegistry;

   private DirectClassInterceptionHandlerFactory interceptionHandlerFactory;

   private InterceptorMetadataRegistry interceptorMetadataRegistry;

   @Before
   public void setUp()
   {
      interceptorMetadataRegistry = new InterceptorMetadataRegistry(SimpleClassMetadataReader.getInstance());
      interceptionHandlerFactory = new DirectClassInterceptionHandlerFactory(interceptorMetadataRegistry);
   }

   public void resetLogAndSetupClassesForMethod() throws Exception
   {
      InterceptorTestLogger.reset();
      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);
      builder.interceptAroundInvoke(FootballTeam.class.getMethod("getName")).with(FirstInterceptor.class, SecondInterceptor.class);
      builder.interceptPostConstruct().with(FirstInterceptor.class);
      builder.interceptPreDestroy().with(SecondInterceptor.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel;      
      interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

   }

   public void resetLogAndSetupClassesGlobally() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAll().with(FirstInterceptor.class, SecondInterceptor.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel;      
      interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

   }

   public void resetLogAndSetupClassesMixed() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);
      builder.interceptAll().with(FirstInterceptor.class);
      builder.interceptPreDestroy().with(SecondInterceptor.class);
      builder.interceptAroundInvoke(FootballTeam.class.getMethod("getName")).with(SecondInterceptor.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel;      
      interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

   }

   public void resetLogAndSetupClassesWithGlobalsIgnored() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);
      builder.interceptAll().with(FirstInterceptor.class);
      builder.interceptPreDestroy().with(SecondInterceptor.class);
      builder.interceptAroundInvoke(FootballTeam.class.getMethod("getName")).with(SecondInterceptor.class);
      builder.ignoreGlobalInterceptors(FootballTeam.class.getMethod("getName"));
      InterceptionModel<Class<?>, Class<?>> interceptionModel;      
      interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

   }


   @Test
   public void testInterceptionWithMethodRegisteredInterceptors() throws Exception
   {
      resetLogAndSetupClassesForMethod();
      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      InterceptionUtils.executePostConstruct(proxy);
      Assert.assertEquals(TEAM_NAME, proxy.getName());
      InterceptionUtils.executePredestroy(proxy);
      Object[] logValues = InterceptorTestLogger.getLog().toArray();
      Assert.assertArrayEquals(iterateAndDisplay(logValues), expectedLoggedValues, logValues);
      assertRawObject(proxy);
   }

   @Test
   public void testInterceptionWithGlobalInterceptors() throws Exception
   {
      resetLogAndSetupClassesGlobally();
      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      InterceptionUtils.executePostConstruct(proxy);
      Assert.assertEquals(TEAM_NAME, proxy.getName());
      InterceptionUtils.executePredestroy(proxy);
      assertRawObject(proxy);
   }

   @Test
   public void testInterceptionWithMixedInterceptors() throws Exception
   {
      resetLogAndSetupClassesMixed();
      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      InterceptionUtils.executePostConstruct(proxy);
      Assert.assertEquals(TEAM_NAME, proxy.getName());
      InterceptionUtils.executePredestroy(proxy);
      Object[] logValues = InterceptorTestLogger.getLog().toArray();
      Assert.assertArrayEquals(iterateAndDisplay(logValues), expectedLoggedValues, logValues);
      assertRawObject(proxy);
   }

   @Test
   public void testInterceptionWithGlobalsIgnored() throws Exception
   {
      resetLogAndSetupClassesWithGlobalsIgnored();
      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      InterceptionUtils.executePostConstruct(proxy);
      Assert.assertEquals(TEAM_NAME, proxy.getName());
      InterceptionUtils.executePredestroy(proxy);
      Object[] logValues = InterceptorTestLogger.getLog().toArray();
      Assert.assertArrayEquals(iterateAndDisplay(logValues), expectedLoggedValuesWithGlobalsIgnored, logValues);
      assertRawObject(proxy);
   }


   @Test
   public void testInterceptionWithSerializedProxy() throws Exception
   {
      resetLogAndSetupClassesForMethod();
      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      InterceptionUtils.executePostConstruct(proxy);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      new ObjectOutputStream(baos).writeObject(proxy);
      proxy = (FootballTeam) new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray())).readObject();
      Assert.assertEquals(TEAM_NAME, proxy.getName());
      Object[] logValues = InterceptorTestLogger.getLog().toArray();
      Assert.assertArrayEquals(iterateAndDisplay(logValues), expectedLoggedValuesOnSerialization, logValues);
      assertRawObject(proxy);
   }


   @Test
   public void testMethodParameterOverriding() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echo", String.class)).with(ParameterOverridingInterceptor.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(42, proxy.echo("1"));
   }

   @Test
   public void testMethodParameterOverridingWithPrimitive() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echoInt", int.class)).with(ParameterOverridingInterceptorWithInteger.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(42, proxy.echoInt(1));
   }

   @Test(expected = IllegalArgumentException.class)
   public void testMethodParameterOverridingWithObject() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echoLongAsObject", Long.class)).with(ParameterOverridingInterceptorWithInteger.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(new Long(42), proxy.echoLongAsObject(1l));
   }

   @Test
   public void testMethodParameterOverridingWithObjectSucceed() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echoLongAsObject", Long.class)).with(ParameterOverridingInterceptorWithLong.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(new Long(42), proxy.echoLongAsObject(1l));
   }

   @Test
   public void testMethodParameterOverridingWithPrimitiveWidening() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echoLong", long.class)).with(ParameterOverridingInterceptorWithInteger.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(42, proxy.echoLong(1));
   }

   @Test(expected = IllegalArgumentException.class)
   public void testMethodParameterOverridingWithPrimitiveNarrowing() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echoInt", int.class)).with(ParameterOverridingInterceptorWithLong.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy =proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(42, proxy.echoInt(1));
   }

   @Test
   public void testMethodParameterOverridingWithArray() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echoObjectArray", Object[].class)).with(ParameterOverridingInterceptorWithLongArray.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(new Long[]{42l}, proxy.echoObjectArray(new Object[]{}));
   }

   @Test(expected = IllegalArgumentException.class)
   public void testMethodParameterOverridingWithArrayOnString() throws Exception
   {
      InterceptorTestLogger.reset();

      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echoStringArray", String[].class)).with(ParameterOverridingInterceptorWithLongArray.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      this.interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);

      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(new Long[]{42l}, proxy.echoStringArray(new String[]{}));
   }

   @Test
   public void testMethodParameterOverridingWithSubclass() throws Exception
   {
      InterceptorTestLogger.reset();


      InterceptionModelBuilder<Class<?>, Class<?>> builder = InterceptionModelBuilder.newBuilderFor(FootballTeam.class, (Class) Class.class);

      builder.interceptAroundInvoke(FootballTeam.class.getMethod("echo2", ValueBearer.class)).with(ParameterOverridingInterceptor2.class);
      InterceptionModel<Class<?>, Class<?>> interceptionModel = builder.build();
      this.interceptorRegistry = new InterceptorRegistry<Class<?>, Class<?>>();
      interceptorRegistry.registerInterceptionModel(FootballTeam.class, interceptionModel);
      
      FootballTeam proxy = proxifyInstance(new FootballTeam(TEAM_NAME), FootballTeam.class);
      Assert.assertEquals(42, proxy.echo2(new ValueBearerImpl(1)));
   }


   public void assertRawObject(FootballTeam proxy)
   {
      InterceptorTestLogger.reset();
      FootballTeam rawInstance = InterceptionUtils.getRawInstance(proxy);
      Assert.assertEquals(TEAM_NAME, rawInstance.getName());
      Object[] logValues = InterceptorTestLogger.getLog().toArray();
      Assert.assertArrayEquals(iterateAndDisplay(logValues), expectedLoggedValuesWhenRaw, logValues);
   }

   private String iterateAndDisplay(Object[] logValues)
   {
      StringBuffer buffer = new StringBuffer();
      for (Object logValue : logValues)
      {
         buffer.append(logValue.toString()).append("\n");
      }
      return buffer.toString();
   }


   private <T> T proxifyInstance(T instance, Class<? extends T> targetClass)
   {
      InterceptorProxyCreator ipc = new InterceptorProxyCreatorImpl(interceptorRegistry, interceptionHandlerFactory);
      MethodHandler methodHandler = ipc.createMethodHandler(instance, targetClass, interceptorMetadataRegistry.getInterceptorClassMetadata(ReflectiveClassReference.of(targetClass), true));
      return ipc.createProxyInstance(InterceptionUtils.createProxyClassWithHandler(targetClass, methodHandler), methodHandler);
   }
}
