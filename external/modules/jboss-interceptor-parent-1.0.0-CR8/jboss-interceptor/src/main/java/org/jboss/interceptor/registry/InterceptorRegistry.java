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

package org.jboss.interceptor.registry;

import org.jboss.interceptor.model.InterceptionModel;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

/**
 * Metadata store for information on how the an entity of a given type needs to be intercepted.
 *
 * @author <a href="mailto:mariusb@redhat.com">Marius Bogoevici</a>
 */
public class InterceptorRegistry<T, I> implements Serializable
{
   private Map<T, InterceptionModel<T, I>> interceptionModelMap = new HashMap<T, InterceptionModel<T, I>>();

   public void registerInterceptionModel(T interceptedEntity, InterceptionModel<T, I> interceptionModel)
   {
      this.interceptionModelMap.put(interceptedEntity, interceptionModel);
   }

   public InterceptionModel<T, I> getInterceptionModel(T interceptedEntity)
   {
      return this.interceptionModelMap.get(interceptedEntity);
   }
}
