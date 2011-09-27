/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.util;

import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * <p>
 * API for retrieving a service from an object.  Services 
 * remove the need for a class to directly implement an interface;
 * instead a class can implement the Service.Provider inner
 * interface.  This makes it possible to "decorate" another
 * generic class - like RenderKit - without needing to know
 * in advance what optional interfaces the decorated object
 * might implement, or to even switch on and off implementations
 * dynamically.  A developer can still choose to directly
 * implement an interface.  The {@link #getService} method
 * supports both direct implementation and the use of 
 * the Service.Provider interface.
 * </p>
 * 
 * <p>
 * <h4>Example:</h4>
 * <pre>
 *    RenderKit rk = facesContext.getRenderKit();
 *    // Retrieve the DialogService generically.
 *    DialogService service = (DialogService)
 *      ServiceUtils.getService(rk, DialogService.class);
 * </pre>
 * </p>
 */
public class Service
{
  /**
   * Inner interface that should be implemented if a class needs to
   * provide services other than by the default approach.  Most often,
   * this is used by decorators to re-expose any services implemented
   * by the decorated object, but it may also be used to hide interfaces
   * that should not be exposed (e.g., if a subclass wishes to hide
   * interfaces implemented by its parent).
   */
  static public interface Provider
  {
    public <T> T getService(Class<T> serviceClass);
  }


  /**
   * Returns a service that can be cast to the provided serviceClass,
   * as vended by the <code>from</code> object.  If the class
   * implements Provider, its <code>getService()</code> method will
   * be used.  Otherwise, the default behavior will be to see
   * if <code>from</code> is an instance of <code>serviceClass</code>,
   * and return it if it is.
   * @param from the object that is vending the service
   * @param serviceClass the type of object that must be returned
   * @return an object of type <code>serviceClass</code>, or null
   *   if no such object could be located
   */
  @SuppressWarnings("unchecked")
  static public <T> T getService(Object from, Class<T> serviceClass)
  {
    if (from == null)
      throw new NullPointerException();

    if (from instanceof Provider)
    {
      T o = ((Provider) from).getService(serviceClass);
      if (o != null)
      {
        if (!serviceClass.isAssignableFrom(o.getClass()))
          throw new IllegalStateException(_LOG.getMessage(
            "PROVIDER_NOT_RETURN_IMPLEMENTING_OBJECT", new Object[]{from, serviceClass.getName()}));

        return o;
      }
    }

    if (serviceClass.isAssignableFrom(from.getClass()))
      return (T) from;

    return null;
  }

  /**
   * A convenience for retrieving a service from the current RenderKit.
   */
  static public <T> T getRenderKitService(FacesContext context,
                                          Class<T> serviceClass)
  {
    // Provide a better exception than an NPE from inside of
    // Service.getService().
    RenderKit rk = context.getRenderKit();
    if (rk == null)
      throw new NullPointerException(_LOG.getMessage(
        "OBTAIN_NULL_RENDERKIT_WHILE_GETTING_SERVICE", serviceClass.getName()));
 

    return getService(rk, serviceClass);
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    Service.class);
}
