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
package org.apache.myfaces.trinidad.config;

import javax.faces.context.ExternalContext;
import javax.servlet.ServletRequest;

import org.apache.myfaces.trinidad.util.RequestStateMap;

/**
 * This defines an abstract class for the Configurator. Classes implementing
 * this abstraction should be listed in the jar's /META-INF/services folder
 * inside of a text file named "org.apache.myfaces.trinidad.config.Configurator".
 * These services will then be run by Trinidad's global configurator.
 * <p/>
 * This abstract class allows Trinidad and various renderkits to move some of
 * their initialization and request/response wrapping code into a configurator
 * so that they may be handled in a container agnostic fashion supporting
 * both Portlets and Servlets. This allows Trinidad and its various
 * renderkits to offload some of their filter logic such as multi-part
 * request handling (file uploads), skin initialization, and other tasks.
 * <p/>
 * Depending on the container,these methods may be called at different times
 * during the actual request.  The only thing guaranteed to Configurator
 * developers are the constraints listed below in the following methods.
 * <p/>
 * Use of this abstract class is encouraged, where possible, instead of using
 * Trinidad filter services.  While configurators and filter services should
 * be able to coexist, any services provided by the filter service will not
 * run in a portal and should therefore be considered optional to the run of
 * the renderkit or application if Portal compatibility is important.
 *
 *
 * @version $Revision$ $Date$
 */
public abstract class Configurator
{
  /**
   * Initializes the Configurator.  This method is guaranteed to run before
   * any other method within this Configurator and will be called only once
   * per webapp context.  This init is guaranteed to be executed before
   * completion of the first call to the
   * {@link javax.faces.context.FacesContextFactory#getFacesContext}
   * is completed.
   *
   * <strong>Note:</strong>the ExternalContext provided to this method may not
   * contain any of the Request/Response functionality within the external
   * context and will NOT contain any ExternalContext wrapping provided by
   * the {@link #getExternalContext(ExternalContext)} method.  This object
   * is intended only to be used as a container abstraction to the native
   * Context object.
   *
   * @param externalContext a mocked up external context providing access
   *                        to the native context object.
   */
  public void init(ExternalContext externalContext) {}

  /**
   * Cleans up the Configurator.  This method is guaranteed to run at some
   * point after the Servlet or Portlet context falls out of scope and will
   * typically be determined by the context listener.
   */
  public void destroy() {}

  /**
   * This is called at the beginning of each "physical" request, sometime
   * before {@link #getExternalContext(ExternalContext)} or
   * {@link #endRequest(ExternalContext)}.  When using the
   * TrinidadFilter, this will be called during filter execution, but
   * is not guaranteed to happen until just before the creation of the
   * Trinidad FacesContext wrapper.
   *
   * All Configurator services will have thier beginRequest() methods
   * called before any calls to getExternalContext().  So any context
   * wrapping done by this method will not happen until after the
   * beginRequest() is called.</p>
   *
   * It is also important to note that the ExternalContext provided
   * may or may not be the same as the ExternalContext provided to
   * getExternalContext().  But it will have a valid request and response
   * object.
   *
   * By contract, the {@link org.apache.myfaces.trinidad.context.RequestContext}
   * object will be initialized and available when this method is run.
   *
   * @param externalContext a mocked up or life externalContext providing
   *                        access to the native request, response, and context
   *                        objects.
   */
  public void beginRequest(ExternalContext externalContext) {}

  /**
   * Returns an ExternalContext wrapper based on the provided ExternalContext.
   * This method is executed durring the creation of the FacesContext when using
   * Trinidad's FacesContextFactory.  The ExternalContext returned from this
   * method will be a part of the FacesContext that is returned from this factory
   * but may also be wrapped be externalContext's from other services.  Especially
   * in Portlet environments, this method may be executed multiple times after a
   * call to {@link #beginRequest(ExternalContext)}.
   *
   * Please note that it is important that this method returns a wrapper of
   * the original context, or the behavior provided by other services and by
   * Trinidad may not function
   *
   * By contract, the {@link org.apache.myfaces.trinidad.context.RequestContext}
   * object will be initialized and available when this method is run.
   *
   * @param externalContext the ExternalContext to wrap
   * @return a wrapper of the ExternalContext
   */
  public ExternalContext getExternalContext(ExternalContext externalContext)
  {
    return externalContext;
  }

  /**
   * Executed at the end of each "physical" request.  There will be a call to
   * endRequest after each call to {@link #beginRequest(ExternalContext)}.
   *
   * It is also important to note that the ExternalContext provided
   * may or may not be the same as the ExternalContext provided to
   * getExternalContext().  But it will have a valid request and response
   * object.
   *
   * By contract, the {@link org.apache.myfaces.trinidad.context.RequestContext}
   * object will be initialized and available when this method is run.
   *
   * @param externalContext the external context
   */
  public void endRequest(ExternalContext externalContext){}

  /**
   * Disables Configurator services for the current request.  When this method
   * has been called on a request, then the {{@link #beginRequest(ExternalContext)},
   * {@link #endRequest(ExternalContext)}, and
   * {@link #getExternalContext(ExternalContext)} methods will not be called durring
   * a request.
   *
   * <strong>Note:</strong> is this method is called after the beginRequest() method,
   * an error will be logged in the trinidad logger and the services will continue to
   * execute.
   *
   * @param srq the servlet request
   */
  public static final void disableConfiguratorServices(ServletRequest srq)
  {
    RequestStateMap.getInstance(srq).put(_DISABLE_SERVICES, Boolean.TRUE);
  }

  /**
   * Returns <code>true</code> if the {@link #disableConfiguratorServices(ServletRequest)}
   * has been executed on the current request and <code>false</code> if it has not.
   *
   * <strong>Note:</strong>it is important to understand that this method will not
   * properly reflect if the services have actually been disabled or not.  It simply
   * returns whether they "should" have been disabled.  If the disableConfiguratorServices
   * was executed after the beginRequest methods have been executed, then the services
   * will continue to function so that {{@link #getExternalContext(ExternalContext)}
   * and {@link #endRequest(ExternalContext)} will still be called.
   *
   * @param ec the ExternalContext object
   *
   * @return a <code>boolean</code> containing <code>true</code> if the
   *         <code>disableConfiguratorServices()</code> method has been
   *         called and <code>false</code> if it has not.
   */
  protected static final boolean isConfiguratorServiceDisabled(ExternalContext ec)
  {
    return Boolean.TRUE.equals(RequestStateMap.getInstance(ec).get(_DISABLE_SERVICES));
  }

  static private final String _DISABLE_SERVICES =  Configurator.class.getName()+".DISABLE_SERVICES";
}
