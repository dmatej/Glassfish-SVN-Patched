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

import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;
import java.io.Serializable;

/**
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public interface InterceptionModel<T, I> extends Serializable
{

   /**
    * Returns the interceptors applicable for the given interception type and method
    *
    * @param interceptionType
    * @param method
    * @return list of interceptors
    * @throws IllegalArgumentException if interceptionType is business method or around timeout
    *                                  but method is null, as well as if interceptionType is callback and method is not null
    */
   public List<I> getInterceptors(InterceptionType interceptionType, Method method);

   /**
    * Returns all interceptor classes that are applicable to the given intercepted entity
    * @return
    */
   public Set<I> getAllInterceptors();

   public T getInterceptedEntity();

}
