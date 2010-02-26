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

import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.ProxyFactory;
import javassist.util.proxy.ProxyObject;
import org.jboss.interceptor.model.InterceptionType;
import org.jboss.interceptor.model.InterceptorClassMetadata;
import org.jboss.interceptor.model.InterceptionModel;
import org.jboss.interceptor.registry.InterceptorRegistry;
import org.jboss.interceptor.registry.InterceptorClassMetadataRegistry;
import org.jboss.interceptor.InterceptorException;
import org.jboss.interceptor.util.InterceptionUtils;
import org.jboss.interceptor.util.proxy.TargetInstanceProxy;

import javax.interceptor.AroundInvoke;
import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.util.*;

import sun.reflect.ReflectionFactory;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorProxyCreatorImpl implements InterceptorProxyCreator
{

   private List<InterceptorRegistry<Class<?>, ?>>  interceptorRegistries;

   private List<InterceptionHandlerFactory<?>> interceptionHandlerFactories;

   public InterceptorProxyCreatorImpl(List<InterceptorRegistry<Class<?>, ?>> interceptorRegistries, List<InterceptionHandlerFactory<?>> interceptionHandlerFactories)
   {
      this.interceptorRegistries = interceptorRegistries;
      this.interceptionHandlerFactories = interceptionHandlerFactories;
   }

   public InterceptorProxyCreatorImpl(InterceptorRegistry<Class<?>, ?> interceptorRegistries, InterceptionHandlerFactory<?> interceptionHandlerFactories)
   {
      this.interceptorRegistries =  Collections.<InterceptorRegistry<Class<?>, ?>>singletonList(interceptorRegistries);
      this.interceptionHandlerFactories = Collections.<InterceptionHandlerFactory<?>>singletonList(interceptionHandlerFactories);
   }


   public <T> T createProxyFromInstance(final Object target, Class<T> proxyClass, Class<?>[] constructorTypes, Object[] constructorArguments)
   {
      ProxyFactory proxyFactory = new ProxyFactory();
      if (proxyClass != null)
         proxyFactory.setSuperclass(proxyClass);

      proxyFactory.setInterfaces(new Class<?>[]{LifecycleMixin.class, TargetInstanceProxy.class});
      InterceptorMethodHandler interceptorMethodHandler = new InterceptorMethodHandler(target, proxyClass, getModelsFor(proxyClass), interceptionHandlerFactories);
      proxyFactory.setHandler(interceptorMethodHandler);

      try
      {
         Class<T> clazz = proxyFactory.createClass();
         ReflectionFactory reflectionFactory = ReflectionFactory.getReflectionFactory();
         Constructor<T> c = reflectionFactory.newConstructorForSerialization(clazz, Object.class.getDeclaredConstructor());
         T proxyObject = c.newInstance();
         ((ProxyObject)proxyObject).setHandler(interceptorMethodHandler);
         return proxyObject;
      } catch (Exception e)
      {
         throw new InterceptorException(e);
      }
   }

   private <T> List<InterceptionModel<Class<?>, ?>> getModelsFor(Class<T> proxyClass)
   {
      List<InterceptionModel<Class<?>, ?>> interceptionModels = new ArrayList<InterceptionModel<Class<?>,?>>();
      for (InterceptorRegistry interceptorRegistry: interceptorRegistries)
      {
         interceptionModels.add(interceptorRegistry.getInterceptionModel(proxyClass));
      }
      return interceptionModels;
   }

   /*
   public <T> T createInstrumentedInstance(Class<T> proxyClass, Class<?>[] constructorTypes, Object[] constructorArguments)
   {
      ProxyFactory proxyFactory = new ProxyFactory();
      if (proxyClass != null)
         proxyFactory.setSuperclass(proxyClass);


      proxyFactory.setInterfaces(new Class<?>[]{LifecycleMixin.class});

      proxyFactory.setHandler(new AutoProxifiedMethodHandler(proxyClass, interceptorRegistries));

      try
      {
         return (T) proxyFactory.create(constructorTypes, constructorArguments);
      } catch (Exception e)
      {
         throw new InterceptorException(e);
      }
   }
   */

   public <T> T constructInstrumentedInstance(final Object target, Class<T> proxyClass, Class<?>[] constructorTypes, Object[] constructorArguments) throws IllegalAccessException, InstantiationException
   {
      ProxyFactory proxyFactory = new ProxyFactory();
      if (proxyClass != null)
         proxyFactory.setSuperclass(target.getClass());

      proxyFactory.setHandler(new InterceptorMethodHandler(target, proxyClass, getModelsFor(proxyClass), interceptionHandlerFactories));

      try
      {
         return (T) proxyFactory.create(constructorTypes, constructorArguments);
      } catch (Exception e)
      {
         throw new InterceptorException(e);
      }
   }

   public <T> T createProxyFromInstance(final Object target, Class<T> proxyClass) throws IllegalAccessException, InstantiationException
   {
      return createProxyFromInstance(target, proxyClass, new Class[0], new Object[0]);
   }

   public MethodHandler createInstanceProxifyingMethodHandler(final Object target, Class<?> proxyClass)
   {
      return new InterceptorMethodHandler(target, proxyClass, getModelsFor(proxyClass), interceptionHandlerFactories);
   }

   /*
   private class AutoProxifiedMethodHandler implements MethodHandler
   {
      private InterceptorRegistry<Class<?>, I> registry;
      private Map<I, InterceptionHandler> interceptorHandlerInstances = new HashMap<I, InterceptionHandler>();
      private Class<?> targetClazz;
      private InterceptorClassMetadata targetClassInterceptorMetadata;


      public AutoProxifiedMethodHandler(Class<?> targetClazz, InterceptorRegistry<Class<?>, I> registry)
      {
         if (targetClazz == null)
            throw new IllegalArgumentException("Target class must not be null");

         this.targetClazz = targetClazz;
         this.registry = registry;

         for (I interceptorClazz : registry.getInterceptionModel(this.targetClazz).getAllInterceptors())
         {
            interceptorHandlerInstances.put(interceptorClazz, interceptionHandlerFactories.createFor(interceptorClazz));
         }
         targetClassInterceptorMetadata = InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(targetClazz);
      }

      public Object invoke(Object self, Method thisMethod, Method proceed, Object[] args) throws Throwable
      {
         // do not intercept interceptor methods
         if (!thisMethod.getDeclaringClass().equals(LifecycleMixin.class))
         {
            if (thisMethod.getAnnotation(AroundInvoke.class) != null)
               return proceed.invoke(self, args);
            return executeInterception(self, thisMethod, proceed, args, InterceptionType.AROUND_INVOKE);
         } else
         {
            if (thisMethod.getName().equals(InterceptionUtils.POST_CONSTRUCT))
            {
               return executeInterception(self, null, null, null, InterceptionType.POST_CONSTRUCT);
            } else if (thisMethod.getName().equals(InterceptionUtils.PRE_DESTROY))
            {
               return executeInterception(self, null, null, null, InterceptionType.PRE_DESTROY);
            }
         }

         return null;
      }

      private Object executeInterception(Object self, Method thisMethod, Method proceed, Object[] args, InterceptionType interceptionType) throws Throwable
      {

         List<I> interceptorClasses = registry.getInterceptionModel(targetClazz).getInterceptors(interceptionType, thisMethod);
         List<InterceptionHandler> interceptionHandlers = new ArrayList<InterceptionHandler>();
         for (I interceptorReference : interceptorClasses)
         {
            interceptionHandlers.add(interceptorHandlerInstances.get(interceptorReference));
         }

         if (targetClassInterceptorMetadata.getInterceptorMethods(interceptionType) != null && !targetClassInterceptorMetadata.getInterceptorMethods(interceptionType).isEmpty())
         {
            interceptionHandlers.add(new DirectClassInterceptionHandler<Class<?>>(targetClazz));
         }
         InterceptionChain chain = new InterceptionChain(interceptionHandlers, interceptionType, self, proceed, args);
         return chain.invokeNext(new InterceptorInvocationContext(chain, self, proceed, args));
      }


   }      */

}


