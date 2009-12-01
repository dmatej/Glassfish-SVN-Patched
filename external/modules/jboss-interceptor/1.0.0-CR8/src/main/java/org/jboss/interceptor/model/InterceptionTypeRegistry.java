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

package org.jboss.interceptor.model;


import java.util.Collection;
import java.util.Map;
import java.util.HashMap;
import java.lang.annotation.Annotation;

import org.jboss.interceptor.util.ReflectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public final class InterceptionTypeRegistry
{
  
   private static final Logger LOG = LoggerFactory.getLogger(InterceptionTypeRegistry.class);
   private static Map<InterceptionType, Class<? extends Annotation>> interceptionAnnotationClasses;

   public static Class<? extends Annotation> TIMEOUT_ANNOTATION_CLASS = null;

   static
   {
      interceptionAnnotationClasses = new HashMap<InterceptionType, Class<? extends Annotation>>();

      for (InterceptionType interceptionType: InterceptionType.values())
      {
         try
         {
            interceptionAnnotationClasses.put(interceptionType, (Class<? extends Annotation>) ReflectionUtils.classForName(interceptionType.getAnnotationClassName()));
         } catch (Exception e)
         {
            LOG.warn("Class '" + interceptionType.getAnnotationClassName() + "' not found, interception based on it is not enabled" );
         }
      }

      try
      {
         TIMEOUT_ANNOTATION_CLASS = (Class<? extends Annotation>) ReflectionUtils.classForName("javax.ejb.Timeout");
      } catch (ClassNotFoundException e)
      {
         // no-op
      }
   }

   public static Collection<InterceptionType> getSupportedInterceptionTypes()
   {
      return interceptionAnnotationClasses.keySet();
   }

   public static boolean supportsTimeoutMethods()
   {
      return TIMEOUT_ANNOTATION_CLASS != null;
   }

   public static Class<? extends Annotation> getAnnotationClass(InterceptionType interceptionType)
   {
      return interceptionAnnotationClasses.get(interceptionType);
   }
}
