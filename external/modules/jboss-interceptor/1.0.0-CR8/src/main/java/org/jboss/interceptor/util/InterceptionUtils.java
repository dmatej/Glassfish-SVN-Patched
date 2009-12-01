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

package org.jboss.interceptor.util;

import org.jboss.interceptor.model.InterceptionType;
import org.jboss.interceptor.model.InterceptionTypeRegistry;
import org.jboss.interceptor.model.InterceptionModel;
import org.jboss.interceptor.proxy.InterceptionHandlerFactory;
import org.jboss.interceptor.proxy.InterceptorProxyCreatorImpl;
import org.jboss.interceptor.proxy.LifecycleMixin;
import org.jboss.interceptor.registry.InterceptorRegistry;
import org.jboss.interceptor.InterceptorException;
import org.jboss.interceptor.util.proxy.TargetInstanceProxy;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.interceptor.InvocationContext;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.annotation.Annotation;
import java.util.List;
import java.util.Collections;

import javassist.util.proxy.ProxyObject;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptionUtils
{
   private static final Logger LOG = LoggerFactory.getLogger(InterceptionUtils.class);
   public static final String POST_CONSTRUCT = "lifecycle_mixin_$$_postConstruct";
   public static final String PRE_DESTROY = "lifecycle_mixin_$$_preDestroy";


   private static Class<? extends Annotation> INTERCEPTORS_ANNOTATION_CLASS = null;
   private static Class<? extends Annotation> EXCLUDE_CLASS_INTERCEPTORS_ANNOTATION_CLASS = null;

   static
   {
      try
      {
         INTERCEPTORS_ANNOTATION_CLASS = (Class<? extends Annotation>) Class.forName("javax.interceptor.Interceptors");
         EXCLUDE_CLASS_INTERCEPTORS_ANNOTATION_CLASS = (Class<? extends Annotation>) Class.forName("javax.interceptor.ExcludeClassInterceptors");
      }
      catch (ClassNotFoundException e)
      {
         //do nothing
      };
   }

   public static void executePostConstruct(Object proxy)
   {
      if (proxy instanceof LifecycleMixin)
      {
         LifecycleMixin lifecycleMixin = (LifecycleMixin) proxy;
         lifecycleMixin.lifecycle_mixin_$$_postConstruct();
      }
   }

   public static void executePredestroy(Object proxy)
   {
      if (proxy instanceof LifecycleMixin)
      {
         LifecycleMixin lifecycleMixin = (LifecycleMixin) proxy;
         lifecycleMixin.lifecycle_mixin_$$_preDestroy();
      }
   }

   /**
    * @param method
    * @return true if the method has none of the interception type annotations, and is public and not static
    *         false otherwise
    */
   public static boolean isInterceptionCandidate(Method method)
   {
      // just a provisory implementation - any method which is not an interceptor method
      // is an interception candidate
      int modifiers = method.getModifiers();
      if (Modifier.isStatic(modifiers))
         return false;
      for (InterceptionType interceptionType : InterceptionTypeRegistry.getSupportedInterceptionTypes())
      {
         if (method.getAnnotation(InterceptionTypeRegistry.getAnnotationClass(interceptionType)) != null)
         {
            return false;
         }
      }
      return true;
   }

   /**
    * @param interceptionType
    * @param method
    * @return
    */
   public static boolean isInterceptorMethod(InterceptionType interceptionType, Method method)
   {

      if (method.getAnnotation(InterceptionTypeRegistry.getAnnotationClass(interceptionType)) == null)
      {
         return false;
      }

      if (interceptionType.isLifecycleCallback())
      {
         if (!Void.TYPE.equals(method.getReturnType()))
         {
            LOG.warn("Method " + method.getName() + " on class " + method.getDeclaringClass().getName()
                  + " is annotated with " + interceptionType.getAnnotationClassName()
                  + " but does not have a void return type");
            return false;
         }

         Class<?>[] parameterTypes = method.getParameterTypes();

         if (parameterTypes.length > 1)
         {
            LOG.warn("Method " + method.getName() + " on class " + method.getDeclaringClass().getName()
                  + " is annotated with " + interceptionType.getAnnotationClassName()
                  + " but has more than 1 parameter");
            return false;
         }

         if (parameterTypes.length == 1 && !InvocationContext.class.equals(parameterTypes[0]))
         {
            LOG.warn("Method " + method.getName() + " on class " + method.getDeclaringClass().getName()
                  + " is annotated with " + interceptionType.getAnnotationClassName()
                  + " but does not have a " + InvocationContext.class.getName() + " parameter ");
            return false;
         }

         return true;
      }
      else
      {
         if (!Object.class.equals(method.getReturnType()))
         {
            LOG.warn("Method " + method.getName() + " on class " + method.getDeclaringClass().getName()
                  + " is annotated with " + interceptionType.getAnnotationClassName()
                  + " but does not return a " + Object.class.getName());
            return false;
         }

         Class<?>[] parameterTypes = method.getParameterTypes();

         if (parameterTypes.length != 1)
         {
            LOG.warn("Method " + method.getName() + " on class " + method.getDeclaringClass().getName()
                  + " is annotated with " + interceptionType.getAnnotationClassName()
                  + " but does not have exactly 1 parameter");
            return false;
         }

         if (!InvocationContext.class.equals(parameterTypes[0]))
         {
            LOG.warn("Method " + method.getName() + " on class " + method.getDeclaringClass().getName()
                  + " is annotated with " + interceptionType.getAnnotationClassName()
                  + " but does not have a " + InvocationContext.class.getName() + " parameter ");
            return false;
         }

         return true;
      }
   }

   public static <T> T proxifyInstance(T instance, Class<?> superClass, List<InterceptorRegistry<Class<?>, ?>> interceptorRegistries, List<InterceptionHandlerFactory<?>> interceptionHandlerFactory)
   {
      try
      {
        InterceptorProxyCreatorImpl proxyCreator = new InterceptorProxyCreatorImpl(interceptorRegistries, interceptionHandlerFactory);
         return (T) proxyCreator.createProxyFromInstance(instance, superClass);
      }
      catch (Exception e)
      {
         throw new InterceptorException(e);
      }
   }

    public static <T> T proxifyInstance(T instance, Class<?> superClass, InterceptorRegistry<Class<?>, ?> interceptorRegistry, InterceptionHandlerFactory<?> interceptionHandlerFactory)
   {
      try
      {
        InterceptorProxyCreatorImpl proxyCreator = new InterceptorProxyCreatorImpl(Collections.<InterceptorRegistry<Class<?>, ?>>singletonList(interceptorRegistry), Collections.<InterceptionHandlerFactory<?>>singletonList(interceptionHandlerFactory));
         return (T) proxyCreator.createProxyFromInstance(instance, superClass);
      }
      catch (Exception e)
      {
         throw new InterceptorException(e);
      }
   }

   public static boolean supportsEjb3InterceptorDeclaration()
   {
      return INTERCEPTORS_ANNOTATION_CLASS != null && EXCLUDE_CLASS_INTERCEPTORS_ANNOTATION_CLASS != null;
   }


   public static Class<? extends Annotation> getInterceptorsAnnotationClass()
   {
      return INTERCEPTORS_ANNOTATION_CLASS;
   }

   public static Class<? extends Annotation> getExcludeClassInterceptorsAnnotationClass()
   {
      return EXCLUDE_CLASS_INTERCEPTORS_ANNOTATION_CLASS;
   }

   public static <T> T getRawInstance(T proxy)
   {
      while (proxy instanceof TargetInstanceProxy)
      {
         proxy = ((TargetInstanceProxy<T>)proxy).getTargetInstance();
      }
      return proxy;
   }

}
