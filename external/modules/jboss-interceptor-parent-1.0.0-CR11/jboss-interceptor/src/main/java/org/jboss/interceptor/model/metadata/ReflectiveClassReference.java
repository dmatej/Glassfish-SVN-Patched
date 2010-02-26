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
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Iterator;

/**
 * @author Marius Bogoevici
 */
public class ReflectiveClassReference implements ClassReference, Serializable
{

   private Class<?> clazz;

   private ReflectiveClassReference(Class<?> clazz)
   {
      this.clazz = clazz;
   }

   public static ClassReference of(Class<?> clazz)
   {
      return new ReflectiveClassReference(clazz);
   }

   public String getClassName()
   {
      return clazz.getName();
   }

   public Iterable<MethodReference> getDeclaredMethods()
   {
      return new Iterable<MethodReference>()
      {
         public Iterator<MethodReference> iterator()
         {
             return new ImmutableIteratorWrapper<Method>(new ArrayIterator(ReflectiveClassReference.this.clazz.getDeclaredMethods()))
             {
                @Override
                protected MethodReference wrapObject(Method method)
                {
                   return ReflectiveMethodReference.of(method);
                }
             };
         }
      };     
   }

   public Class<?> getJavaClass()
   {
      return clazz;
   }   

   public ClassReference getSuperclass()
   {
      Class<?> superClass = clazz.getSuperclass();
      return superClass == null? null : new ReflectiveClassReference(superClass);
   }

}
