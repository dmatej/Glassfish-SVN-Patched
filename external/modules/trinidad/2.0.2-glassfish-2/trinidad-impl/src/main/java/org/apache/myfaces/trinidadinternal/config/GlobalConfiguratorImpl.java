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

package org.apache.myfaces.trinidadinternal.config;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import java.util.concurrent.locks.ReentrantLock;

import javax.faces.context.ExternalContext;

import javax.servlet.ServletRequest;
import javax.servlet.ServletRequestWrapper;
import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.RequestContextFactory;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.ComponentReference;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.util.RequestStateMap;
import org.apache.myfaces.trinidad.util.RequestType;
import org.apache.myfaces.trinidadinternal.context.RequestContextFactoryImpl;
import org.apache.myfaces.trinidadinternal.context.external.ServletCookieMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestHeaderMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestHeaderValuesMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestParameterMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestParameterValuesMap;
import org.apache.myfaces.trinidadinternal.skin.SkinFactoryImpl;
import org.apache.myfaces.trinidadinternal.skin.SkinUtils;

/**
 * This is the implementation of the Trinidad's Global configurator. It provides the entry point for
 * all other configurators. This class is responsible for enforcing the contract outlined by the
 * Configurator abstract class, but allows a more "relaxed" implementation of the APIs called for by
 * the Configurator class, making it more convenient to use ConfiguratorServices from within the
 * Trinidad renderkit. Where appropriate, these differences will be documented for the benifit of
 * the Trindad developer.
 *
 * @see org.apache.myfaces.trinidad.config.Configurator
 * @version $Revision$ $Date$
 */
public final class GlobalConfiguratorImpl
  extends Configurator
{
  /**
   * Returns a GlobalConfigurator instance for the current context's class loader. The
   * GlobalConfigurator is responsible for enforcing the contract on the other methods of this
   * class. This means that if {@link #init(ExternalContext)} is called multiple times, the global
   * configurator will call all subordinate configurators only once.
   *
   * Likewise, the GlobalConfigurator will return exceptions when the contract is expressly violated
   * (like if {@link #getExternalContext(ExternalContext)} is called before a {{@link #beginRequest(ExternalContext)}.
   *
   * @return a GlobalConfigurator or <code>null</code> is one was unable to be obtained.
   */
  static public final GlobalConfiguratorImpl getInstance()
  {
    final ClassLoader loader = Thread.currentThread().getContextClassLoader();

    if (loader == null)
    {
      _LOG.severe("CANNOT_FIND_CONTEXT_CLASS_LOADER");
    }
    else
    {
      synchronized (_CONFIGURATORS)
      {
        GlobalConfiguratorImpl config = _CONFIGURATORS.get(loader);
        if (config == null)
        {
          try
          {
            config = new GlobalConfiguratorImpl();
            _CONFIGURATORS.put(loader, config);
          }
          catch (final RuntimeException e)
          {
            // OC4J was not reporting these errors properly:
            _LOG.severe(e);
            throw e;
          }
          _LOG.fine("GlobalConfigurator has been created.");
        }
        return config;
      }
    }
    return null;
  }

  /**
   * Returns true if the request has not been stated for the current "virtual"
   * request.  In the servlet environment this will be true after
   * {@link #beginRequest(ExternalContext)} is executed and before
   * {@link #endRequest(ExternalContext)} is executed.  This will generally
   * happen once per request.  In the Portlet Environment, the request must be
   * be started and ended at the beginning and end of both the actionRequest
   * and the RenderRequest.
   *
   * @param ec
   * @return
   */
  static public boolean isRequestStarted(ExternalContext ec)
  {
    return (RequestStateMap.getInstance(ec).get(_REQUEST_TYPE) != null);
  }

  /**
   * Returns "true" if the services should be considered enabled or disabled.
   *
   * @param ec
   * @return
   */
  static private final boolean _isDisabled(final ExternalContext ec)
  {
    final Boolean inRequest = (Boolean) RequestStateMap.getInstance(ec).get(_IN_REQUEST);

    if (inRequest == null)
    {
      return isConfiguratorServiceDisabled(ec);
    }
    else
    {
      final boolean disabled = inRequest.booleanValue();
      if (disabled != isConfiguratorServiceDisabled(ec))
      {
        _LOG.warning("Configurator services were disabled after beginRequest was executed.  Cannot disable configurator services");
      }

      return disabled;
    }
  }

  /**
   * Private default constructor. Right now this class is not serializable. If serialization is
   * required, we may wish to make this public. We really don't want people using this though.
   */
  private GlobalConfiguratorImpl()
  {
  }

  /**
   * Executes the beginRequest methods of all of the configurator services. This method will also
   * initizlize the configurator if it has not already been initialized, so there may be no need to
   * call the {@link #init(ExternalContext)} method directly.
   *
   * This method also ensures that the requestContext is attached before the beginRequest methods
   * are called, so there is no reason to initialize the request context before calling this method.
   * In portal environments, it is important to execute this method once for each Portlet action and
   * render request so that the requestContext may be properly initialized even though the
   * underlying services will be called only once per physical request.
   *
   * @param ec the externalContext to use to begin the request.
   *
   * @see Configurator#beginRequest(javax.faces.context.ExternalContext)
   */
  @SuppressWarnings("unchecked") // TODO: remove this for Faces 1.2
  @Override
  public void beginRequest(ExternalContext ec)
  {
    // asserts for debug which disappear in production
    assert ec != null;
    RequestStateMap state = RequestStateMap.getInstance(ec);
    RequestType requestType = (RequestType) state.get(_REQUEST_TYPE);

    // Do per-virtual request stuff
    if (requestType == null)
    {
      // RequestType may change in a portal environment. Make sure it's set up to enforce the
      // contracts
      requestType = ExternalContextUtils.getRequestType(ec);
      RequestStateMap.getInstance(ec).put(_REQUEST_TYPE, requestType);

      // By contract, Configurators beginRequest is only called once per physical request.
      // The globalConfigurator may be called multiple times, however, so we need to enforce
      // the contract.
      if (!_isDisabled(ec))
      {
        // If this hasn't been initialized then please initialize
        if (!_initialized.get())
        {
          init(ec);
        }

        _attachRequestContext(ec);

        if (state.get(_IN_REQUEST) == null)
        {
          _startConfiguratorServiceRequest(ec);
        }
      }
      else
      {
        _LOG.fine("GlobalConfigurator: Configurators have been disabled for this request.");
      }
    }
    else if (!requestType.equals(ExternalContextUtils.getRequestType(ec)))
    {
      // This will happen if the actionRequest was not ended before dispatching to the render
      // request
      throw new IllegalStateException("The previous action request was not ended.");
    }
    else
    {
      _LOG.fine("BeginRequest called multiple times for this request");
    }
  }

  /**
   * Cleans up the current configurator. This will execute the destroy method on all of the
   * configurator services. Generally this will be called by Trinidad's context listener when the
   * context is destroyed, but it may be used manually to allow the configurator to be
   * re-initialized.
   *
   * Calling this method while the configurator is not initialized will not re-execute the destroy
   * methods on the services.
   *
   * @see org.apache.myfaces.trinidad.config.Configurator#destroy()
   */
  @Override
  public void destroy()
  {
    
    if (_initialized.get())
    { 
      try
      {
        //Forces atomic operations with init.  If we are in the middle of an init or another destroy, we'll
        //wait on this lock until our operations are complete.  We then have to recheck our initialized state.
        
        _initLock.lock();
        if(_initialized.get())
        {
          for (final Configurator config: _services)
          {
            try
            {
              config.destroy();
            }
            catch (final Throwable t)
            {
              // we always want to continue to destroy things, so log errors and continue
              _LOG.severe(t);
            }
          }
          _services = null;
          _initialized.set(false);
        }
      }
      finally
      {
        //release any managed threadlocals that may have been used durring destroy
        _releaseManagedThreadLocals();
        _initLock.unlock();
      }
    }
  }

  /**
   * Ends the currently begun request. It is important to note that this should be executed only
   * once per physical request.
   *
   * @see org.apache.myfaces.trinidad.config.Configurator#endRequest(javax.faces.context.ExternalContext)
   */
  @Override
  public void endRequest(ExternalContext ec)
  {
    RequestStateMap state = RequestStateMap.getInstance(ec);

    // do per virtual-request stuff
    if (state.get(_REQUEST_TYPE) != null)
    {
      if (!_isDisabled(ec))
      {
        try
        {
          //Only end services at the end of a writable response.  This will
          //generally be RENDER, RESOURCE, and SERVLET.
          if (ExternalContextUtils.isResponseWritable(ec))
          {
            _endConfiguratorServiceRequest(ec);
          }
        }
        finally
        {
          state.saveState(ec);
          _releaseRequestContext(ec);
        }
      }
      state.remove(_REQUEST_TYPE);
    }
  }

  /**
   * Returns an externalContext for this configurator and all of the configurator services. If this
   * method is executed before {@link #beginRequest(ExternalContext)} then this method will call
   * beginRequest(). It is important to note, however, that even though beginRequest does not need
   * to be explicitly called, {{@link #endRequest(ExternalContext)} does need to be called when the
   * request has completed or the contract to the configurators will be broken.
   *
   * @param ec
   *          the ExternalContext object that should be wrapped.
   *
   * @return a decorated ExternalContext object
   */
  @Override
  public ExternalContext getExternalContext(ExternalContext ec)
  {
    RequestStateMap state = RequestStateMap.getInstance(ec);
    RequestType type = (RequestType) state.get(_REQUEST_TYPE);

    if (type == null)
    {
      beginRequest(ec);
      type = (RequestType) state.get(_REQUEST_TYPE);
    }
    else if (!ExternalContextUtils.getRequestType(ec).equals(type))
    {
      throw new IllegalStateException("The expected request type is not the same as the current request type.");
    }

    if (!_isDisabled(ec))
    {
      if (!type.isPortlet() && _isSetRequestBugPresent(ec))
      {
        //This handles bug 493 against the JSF-RI 1.2_03 and earlier.  If the bug
        //is present in the current system, add a wrapper to fix it

        //TODO sobryan this is somewhat inefficient so should be removed when we
        //are no longer dependant on JSF1.2_03 or earlier.  Still, we only wrap
        //when we have to so it should be no biggy under normal circumstances.
        ec = new ClearRequestExternalContext(ec);
      }

      // Wrap ExternalContexts
      for (Configurator config: _services)
      {
        ec = config.getExternalContext(ec);
      }
    }

    return ec;
  }

  /**
   * Initializes the global configurator and the configurator services. This method need not be
   * called directly as it will be called from {@link #beginRequest(ExternalContext)} if needed. It
   * is also possible to execute this method more then once, although if initialization has already
   * happened then a call to this method will not do anything. To re-initialize this class, call
   * {@link #destroy()} first and then call this method.
   *
   * @param ec
   *          the externalContext needed to initialize this class
   *
   * @see org.apache.myfaces.trinidad.config.Configurator#init(javax.faces.context.ExternalContext)
   */
  @Override
  public void init(ExternalContext ec)
  {
    assert ec != null;
    
    if (!_initialized.get())
    {
      try
      {
        //For thread saftey.  It is possible for two threads to enter this code at the same time.  When
        //the do the second one will wait at the lock until initialization is complete.  Then the AtomicBoolean
        //is checked again for validity.
        _initLock.lock();
        //Check the AtomicBoolean for a change
        if(!_initialized.get())
        {
          _services = ClassLoaderUtils.getServices(Configurator.class.getName());
  
          // Create a new RequestContextFactory is needed
          if (RequestContextFactory.getFactory() == null)
          {
            RequestContextFactory.setFactory(new RequestContextFactoryImpl());
          }
  
          // Create a new SkinFactory if needed.
          if (SkinFactory.getFactory() == null)
          {
            SkinFactory.setFactory(new SkinFactoryImpl());
          }
  
          // register the base skins
          SkinUtils.registerBaseSkins();
  
          for (final Configurator config: _services)
          {
            config.init(ec);
          }
  
          // after the 'services' filters are initialized, then register
          // the skin extensions found in trinidad-skins.xml. This
          // gives a chance to the 'services' filters to create more base
          // skins that the skins in trinidad-skins.xml can extend.
          SkinUtils.registerSkinExtensions(ec);
          _initialized.set(true);
        }
      }
      finally
      {
        //Do cleanup of anything which may have use the thread local manager during
        //init.
        _releaseManagedThreadLocals();
        _initLock.unlock();
      }
    }
    else
    {
      _LOG.warning("CONFIGURATOR_SERVICES_INITIALIZED");
    }
  }

  /**
   * Hackily called by the ThreadLocalResetter to register itself so that the
   * GlobalConfiguratorImpl can tell the ThreadLocalResetter to clean up the
   * ThreadLocals at the appropriate time.
   */
  void __setThreadLocalResetter(ThreadLocalResetter resetter)
  {
    if (resetter == null)
      throw new NullPointerException();

    _threadResetter.set(resetter);
  }

  /**
   * @param ec
   * @return
   */
  @SuppressWarnings("unchecked")
  private void _attachRequestContext(ExternalContext ec)
  {
    // If someone didn't release the RequestContext on an earlier request,
    // then it'd still be around, and trying to create a new one
    // would trigger an exception. We don't want to take down
    // this thread for all eternity, so clean up after poorly-behaved code.
    RequestContext context = RequestContext.getCurrentInstance();
    if (context != null)
    {
      if (_LOG.isWarning())
      {
        _LOG.warning("REQUESTCONTEXT_NOT_PROPERLY_RELEASED");
      }
      _releaseRequestContext(ec);
    }

    // See if we've got a cached RequestContext instance; if so,
    // reattach it
    Object cachedRequestContext = RequestStateMap.getInstance(ec).get(_REQUEST_CONTEXT);

    // Catch both the null scenario and the
    // RequestContext-from-a-different-classloader scenario
    if (cachedRequestContext instanceof RequestContext)
    {
      context = (RequestContext) cachedRequestContext;
      context.attach();
    }
    else
    {
      RequestContextFactory factory = RequestContextFactory.getFactory();
      assert factory != null;
      context = factory.createContext(ec);
      RequestStateMap.getInstance(ec).put(_REQUEST_CONTEXT, context);
    }
  }

  private void _releaseRequestContext(ExternalContext ec)
  {
    RequestContext context = RequestContext.getCurrentInstance();
    if (context != null)
    {
      // ensure that any deferred ComponentReferences are initialized
      _finishComponentReferenceInitialization(ec);

      context.release();
      _releaseManagedThreadLocals();

      assert RequestContext.getCurrentInstance() == null;
    }
  }

  /**
   * Ensure that any ThreadLocals initialized during this request are cleared
   */
  private void _releaseManagedThreadLocals()
  {
    ThreadLocalResetter resetter = _threadResetter.get();

    if (resetter != null)
    {
      resetter.__removeThreadLocals();
    }
  }

  /**
   * Ensure that all DeferredComponentReferences are fully initialized before the
   * request completes
   */
  private void _finishComponentReferenceInitialization(ExternalContext ec)
  {
    Map<String, Object> requestMap = ec.getRequestMap();
    
    Collection<ComponentReference<?>> initializeList = (Collection<ComponentReference<?>>)
                                             requestMap.get(_FINISH_INITIALIZATION_LIST_KEY);
    
    if ((initializeList != null) && !initializeList.isEmpty())
    {
      for (ComponentReference<?> reference : initializeList)
      {
        reference.ensureInitialization();
      }
      
      // we've initialized everything, so we're done
      initializeList.clear();
    }
  }

  private void _endConfiguratorServiceRequest(final ExternalContext ec)
  {
    // Physical request has now ended
    // Clear the in-request flag
    RequestStateMap.getInstance(ec).remove(_IN_REQUEST);
    if (_services != null)
    {
      for (Configurator config: _services)
      {
        try
        {
          config.endRequest(ec);
        }
        catch (Throwable t)
        {
          _LOG.severe(t);
        }
      }
    }
  }

  @SuppressWarnings("unchecked")
  private void _startConfiguratorServiceRequest(final ExternalContext ec)
  {
    // Physical request has now begun
    final boolean disabled = isConfiguratorServiceDisabled(ec);

    // Tell whether the services were disabled when the requests had begun
    RequestStateMap.getInstance(ec).put(_IN_REQUEST, disabled);

    // If this hasn't been initialized then please initialize
    for (Configurator config: _services)
    {
      try
      {
        config.beginRequest(ec);
      }
      catch (Throwable t)
      {
        _LOG.severe(t);
      }
    }
  }

  static private boolean _isSetRequestBugPresent(ExternalContext ec)
  {
    // This first check is here in order to skip synchronization until
    // absolutely necessary.
    if (!_sSetRequestBugTested)
    {
      synchronized (GlobalConfiguratorImpl.class)
      {
        //This second check is here in case a couple of things enter before the
        //boolean is set.  This is only an exception case and will make it so
        //the initialization code runs only once.
        if (!_sSetRequestBugTested)
        {
          ServletRequest orig = (ServletRequest) ec.getRequest();
          // Call getInitParameterMap() up front
          ec.getInitParameterMap();

          ec.setRequest(new TestRequest(orig));

          _sHasSetRequestBug = !TestRequest.isTestParamPresent(ec);
          _sSetRequestBugTested = true;

          ec.setRequest(orig);
        }
      }
    }

    return _sHasSetRequestBug;
  }

  // This handles an issue with the ExternalContext object prior to
  // JSF1.2_04.

  static private class ClearRequestExternalContext
    extends ExternalContextDecorator
  {
    private ExternalContext _ec;
    private Map<String, Object> _requestCookieMap;
    private Map<String, String> _requestHeaderMap;
    private Map<String, String[]> _requestHeaderValuesMap;
    private Map<String, Object> _requestMap;
    private Map<String, String> _requestParameterMap;
    private Map<String, String[]> _requestParameterValuesMap;

    public ClearRequestExternalContext(ExternalContext ec)
    {
      _ec = ec;
    }

    @Override
    protected ExternalContext getExternalContext()
    {
      return _ec;
    }

    @Override
    public void setRequest(Object request)
    {
      super.setRequest(request);

      // And clear out any of the cached maps, since we should
      // go back and look in the map
      _requestCookieMap = null;
      _requestHeaderMap = null;
      _requestHeaderValuesMap = null;
      _requestMap = null;
      _requestParameterMap = null;
      _requestParameterValuesMap = null;
    }

    @Override
    public Map<String, Object> getRequestCookieMap()
    {
      _checkRequest();
      if (_requestCookieMap == null)
      {

        _requestCookieMap = new ServletCookieMap(_getHttpServletRequest());
      }
      return _requestCookieMap;
    }

    @Override
    public Map<String, String> getRequestHeaderMap()
    {
      if (_requestHeaderMap == null)
      {
        _requestHeaderMap = new ServletRequestHeaderMap(_getHttpServletRequest());
      }
      return _requestHeaderMap;
    }

    @Override
    public Map<String, String[]> getRequestHeaderValuesMap()
    {
      if (_requestHeaderValuesMap == null)
      {
        _requestHeaderValuesMap = new ServletRequestHeaderValuesMap(_getHttpServletRequest());
      }
      return _requestHeaderValuesMap;
    }

    @Override
    public Map<String, Object> getRequestMap()
    {
      _checkRequest();
      if (_requestMap == null)
      {
        _requestMap = new ServletRequestMap((ServletRequest) getRequest());
      }
      return _requestMap;
    }

    @Override
    public Map<String, String> getRequestParameterMap()
    {
      _checkRequest();
      if (_requestParameterMap == null)
      {
        _requestParameterMap = new ServletRequestParameterMap((ServletRequest) getRequest());
      }
      return _requestParameterMap;
    }

    @Override
    public Map<String, String[]> getRequestParameterValuesMap()
    {
      _checkRequest();
      if (_requestParameterValuesMap == null)
      {
        _requestParameterValuesMap =
            new ServletRequestParameterValuesMap((ServletRequest) getRequest());
      }
      return _requestParameterValuesMap;
    }

    private void _checkRequest()
    {
      if (super.getRequest() == null)
      {
        throw new UnsupportedOperationException("Request is null on this context.");
      }
    }

    private HttpServletRequest _getHttpServletRequest()
    {
      _checkRequest();
      if (!(getRequest() instanceof HttpServletRequest))
      {
        throw new IllegalArgumentException("Only HttpServletRequest supported");
      }

      return (HttpServletRequest) getRequest();
    }
  }


  private static volatile boolean _sSetRequestBugTested = false;
  private static boolean _sHasSetRequestBug = false;
  
  private final ReentrantLock _initLock = new ReentrantLock();

  private AtomicBoolean _initialized = new AtomicBoolean(false);
  private List<Configurator> _services;
  static private final Map<ClassLoader, GlobalConfiguratorImpl> _CONFIGURATORS =
    new HashMap<ClassLoader, GlobalConfiguratorImpl>();
  static private final String _IN_REQUEST =
    GlobalConfiguratorImpl.class.getName() + ".IN_REQUEST";
  static private final String _REQUEST_CONTEXT =
    GlobalConfiguratorImpl.class.getName() + ".REQUEST_CONTEXT";
  static private final String _REQUEST_TYPE =
    GlobalConfiguratorImpl.class.getName() + ".REQUEST_TYPE";

  static private class TestRequest
    extends ServletRequestWrapper
  {
    public TestRequest(ServletRequest request)
    {
      super(request);
    }

    @Override
    public String getParameter(String string)
    {
      if (_TEST_PARAM.equals(string))
      {
        return "passed";
      }

      return super.getParameter(string);
    }

    static public final boolean isTestParamPresent(ExternalContext ec)
    {
      return RequestStateMap.getInstance(ec).get(_TEST_PARAM) != null;
    }

    static private String _TEST_PARAM = TestRequest.class.getName() + ".TEST_PARAM";
  }

  // skanky duplication of key from ComponentReference Class
  private static final String _FINISH_INITIALIZATION_LIST_KEY = ComponentReference.class.getName() +
                                                                "#FINISH_INITIALIZATION";

  // hacky reference to the ThreadLocalResetter used to clean up request-scoped
  // ThreadLocals
  private AtomicReference<ThreadLocalResetter> _threadResetter =
    new AtomicReference<ThreadLocalResetter>();

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(GlobalConfiguratorImpl.class);
}
