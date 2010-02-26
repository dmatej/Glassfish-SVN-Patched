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

package org.jboss.interceptor.model.metadata;

import java.io.Serializable;

import org.jboss.interceptor.InterceptorException;
import org.jboss.interceptor.model.InterceptorMetadata;

public abstract class AbstractInterceptorMetadataSerializationProxy implements Serializable
{
   private String className;

   private boolean interceptionTargetClass;

   protected AbstractInterceptorMetadataSerializationProxy(String className, boolean interceptionTargetClass)
   {
      this.className = className;
      this.interceptionTargetClass = interceptionTargetClass;
   }

   private Object readResolve()
   {
      try
      {
         return loadInterceptorMetadata();
      }
      catch (ClassNotFoundException e)
      {
         throw new InterceptorException("Failed to deserialize the interceptor class metadata", e);
      }
   }

   protected String getClassName()
   {
      return className;
   }

   protected boolean isInterceptionTargetClass()
   {
      return interceptionTargetClass;
   }

   protected abstract InterceptorMetadata loadInterceptorMetadata() throws ClassNotFoundException;
}
