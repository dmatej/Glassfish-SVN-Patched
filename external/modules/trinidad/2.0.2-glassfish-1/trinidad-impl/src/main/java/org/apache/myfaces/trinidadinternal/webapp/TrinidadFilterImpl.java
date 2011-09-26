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
package org.apache.myfaces.trinidadinternal.webapp;

import java.io.IOException;
import java.io.Serializable;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.faces.FactoryFinder;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.FacesContextFactory;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import javax.servlet.http.HttpSession;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.util.RequestStateMap;
import org.apache.myfaces.trinidadinternal.config.CheckSerializationConfigurator;
import org.apache.myfaces.trinidadinternal.config.GlobalConfiguratorImpl;
import org.apache.myfaces.trinidadinternal.config.dispatch.DispatchResponseConfiguratorImpl;
import org.apache.myfaces.trinidadinternal.config.dispatch.DispatchServletResponse;
import org.apache.myfaces.trinidadinternal.config.upload.FileUploadConfiguratorImpl;
import org.apache.myfaces.trinidadinternal.config.upload.UploadRequestWrapper;
import org.apache.myfaces.trinidadinternal.config.xmlHttp.XmlHttpConfigurator;
import org.apache.myfaces.trinidadinternal.context.DialogServiceImpl;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.context.external.ServletExternalContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.webapp.wrappers.BasicHTMLBrowserRequestWrapper;

/**
 * Actual implementation of the Trinidad servlet filter.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/webapp/AdfFacesFilterImpl.java#0 $) $Date: 10-nov-2005.18:48:59 $
 * @todo Allow configuration of the maximum allowed number of bytes in
 *   an entire request
 */
public class TrinidadFilterImpl implements Filter
{
  static public void verifyFilterIsInstalled(FacesContext context)
  {
    Object isInstalled =
      context.getExternalContext().getRequestMap().get(_FILTER_EXECUTED_KEY);
    if (!Boolean.TRUE.equals(isInstalled))
    {
      _LOG.warning("REQUIRED_TRINIDADFILTER_NOT_INSTALLED");

    }
  }

  static public FacesContext getPseudoFacesContext()
  {
    return _PSEUDO_FACES_CONTEXT.get();
  }

  /**
   * Returns true if the filter is in the middle of executing the
   * "return from dialog"
   */
  static public boolean isExecutingDialogReturn(FacesContext context)
  {
    return Boolean.TRUE.equals(
      context.getExternalContext().getRequestMap().get(_IS_RETURNING_KEY));
  }

  public void init(FilterConfig filterConfig) throws ServletException
  {
    // potentially wrap the FilterConfig to catch Serialization changes
    filterConfig = CheckSerializationConfigurator.getFilterConfig(filterConfig);
    
    _servletContext = filterConfig.getServletContext();
            
    //There is some functionality that still might require servlet-only filter services.
    _filters = ClassLoaderUtils.getServices(TrinidadFilterImpl.class.getName());

    ExternalContext externalContext = new ServletExternalContext(
                                        _servletContext, null, null);

    PseudoFacesContext facesContext = new PseudoFacesContext(externalContext);
    facesContext.setAsCurrentInstance();

    try
    {
      for(Filter f:_filters)
      {
        f.init(filterConfig);
      }
    }
    finally
    {
      facesContext.release();
    }
  }

  public void destroy()
  {
    //Destroy filter services
    for(Filter f:_filters)
    {
      f.destroy();
    }

    _filters = null;
  }


  @SuppressWarnings("unchecked")
  public void doFilter(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
    //Execute the filter services
    if (!_filters.isEmpty())
      chain = new FilterListChain(_filters, chain);

    // Set a flag so that we can detect if the filter has been
    // properly installed.
    request.setAttribute(_FILTER_EXECUTED_KEY, Boolean.TRUE);                                                                     

    // potentially wrap the request in order to check managed bean HA
    if (request instanceof HttpServletRequest)
    {
      request = CheckSerializationConfigurator.getHttpServletRequest(
                                    new ServletExternalContext(_servletContext, request, response),
                                    (HttpServletRequest)request);
    }
    
    // potentially wrap the ServletContext in order to check managed bean HA
    ExternalContext externalContext = new ServletExternalContext(
                                        _getPotentiallyWrappedServletContext(request),
                                        request,
                                        response);

    // provide a (Pseudo-)FacesContext for configuration tasks
    PseudoFacesContext facesContext = new PseudoFacesContext(externalContext);
    facesContext.setAsCurrentInstance();
    
    GlobalConfiguratorImpl config = GlobalConfiguratorImpl.getInstance();
    config.beginRequest(externalContext);
    
    String noJavaScript = request.getParameter(XhtmlConstants.NON_JS_BROWSER);
        
    // Wrap the request only for Non-javaScript browsers
    if(noJavaScript != null &&
              XhtmlConstants.NON_JS_BROWSER_TRUE.equals(noJavaScript))
    {
      request = new BasicHTMLBrowserRequestWrapper((HttpServletRequest)request);
    } 

    //To maintain backward compatibilty, wrap the request at the filter level
    Map<String, String[]> addedParams = FileUploadConfiguratorImpl.getAddedParameters(externalContext);
    
    if(addedParams != null)
    {
      FileUploadConfiguratorImpl.apply(externalContext);
      request = new UploadRequestWrapper((HttpServletRequest)request, addedParams);
    }

    // release the PseudoFacesContext, since _doFilterImpl() has its own FacesContext
    facesContext.release();

    try
    {
      _doFilterImpl(request, response, chain);
    }
    // For PPR errors, handle the request specially
    catch (Throwable t)
    {
      boolean isPartialRequest;
      if (addedParams != null)
      {
        isPartialRequest = CoreRenderKit.isPartialRequest(addedParams);
      }
      else
      {
        isPartialRequest = CoreRenderKit.isPartialRequest(externalContext);
      }

      if (isPartialRequest)
      {
        XmlHttpConfigurator.handleError(externalContext, t);
      }
      else
      {
        // For non-partial requests, just re-throw.  It is not
        // our responsibility to catch these
        if (t instanceof RuntimeException)
          throw ((RuntimeException) t);
        if (t instanceof Error)
          throw ((Error) t);
        if (t instanceof IOException)
          throw ((IOException) t);
        if (t instanceof ServletException)
          throw ((ServletException) t);

        // Should always be one of those four types to have
        // gotten here.
        _LOG.severe(t);
      }

    }
    finally
    {
      config.endRequest(externalContext);
    }
  }


  @SuppressWarnings("unchecked")
  private void _doFilterImpl(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
    // -= Scott O'Bryan =-
    // Added for backward compatibility
    // potentially wrap the ServletContext to check ManagerBean HA
    ExternalContext ec = new ServletExternalContext(_getPotentiallyWrappedServletContext(request),
                                                    request,
                                                    response);
    
    boolean isHttpReq = ExternalContextUtils.isHttpServletRequest(ec);
    
    if(isHttpReq)
    {
      response = _getResponse(ec);
      ec.setResponse(response);
    }
     
    // Set up a PseudoFacesContext with the actual request and response
    // so that RequestContext can be more functional in the interval
    // between now and when the FacesServlet starts.
    PseudoFacesContext pfc = new PseudoFacesContext(ec);
    _PSEUDO_FACES_CONTEXT.set(pfc);
    try
    {
      if(isHttpReq)
      {
        // If there are existing "launchParameters", then that means
        // we've returned from a "launch", and we need to re-execute the
        // faces lifecycle.  ViewHandlerImpl will be responsible for ensuring
        // that we re-execute the lifecycle on the correct page.
        // -= Simon Lessard =-
        // FIXME: Using <String, String[]> for now to accomodate ReplaceParametersRequestWrapper.
        //        However, the Servlet specification suggest <String, Object> so this 
        //        could lead to some nasty problems one day. Especially if JEE spec includes 
        //        generics for its Servlet API soon.
        //
        // -= Scott O'Bryan =- 
        // TODO: The following should be made available to the Portal.  This is not trivial 
        //       because this just re-invokes the filter chain with a new set of parameters.
        //       In the portal environment, this must rerun the portlet without the use of 
        //       filters until Portlet 2.0.
        request = _getRequest(ec);
        ec.setRequest(request);
      }
      
      chain.doFilter(request, response);

      if(isHttpReq)
      {
        _handleDialogReturn(ec);
      }
    }
    finally
    {
      _PSEUDO_FACES_CONTEXT.remove();
    }
  }
  
  private String _getKey(String uid)
  {
    return _LAUNCH_KEY+"_"+uid;
  }
  
  private ServletResponse _getResponse(ExternalContext ec)
  {
    HttpServletResponse dispatch = new DispatchServletResponse(ec);
    DispatchResponseConfiguratorImpl.apply(ec);
    return dispatch;
  }
  
  private ServletRequest _getRequest(ExternalContext ec)
  {
    String uid = ec.getRequestParameterMap().get(_LAUNCH_KEY);
    if(uid != null)
    {
      /**
       * We use pageflow scope so that if something fails on the redirect, we
       * have a chance of getting cleaned up early.  This will not always happen
       * so the object may stick around for a while.
       */
      Map<String, Object> sessionMap = ec.getSessionMap();
      
      LaunchData data = (LaunchData)sessionMap.remove(_getKey(uid));
      
      //We are returning from a dialog:
      if(data != null)
      {
        Map<String, Object> requestMap = ec.getRequestMap();

        //Setting the flag to properly support isExecutingDialogReturn.
        //This is needed for isExecutingDialogReturn.
        requestMap.put(_IS_RETURNING_KEY, Boolean.TRUE);  
        
        UIViewRoot launchView = data.getLaunchView();
        if(launchView != null)
        { 
          requestMap.put(RequestContextImpl.LAUNCH_VIEW, data.getLaunchView());
        }
        
        return new ReplaceParametersRequestWrapper(
             (HttpServletRequest) ec.getRequest(), 
             data.getLaunchParam());
      }
    }
    
    return (ServletRequest)ec.getRequest();
  }
  
  private void _handleDialogReturn(ExternalContext ec)
    throws IOException
  {
    Map<String, Object> reqMap = ec.getRequestMap();
    
    if(Boolean.TRUE.equals(reqMap.get(DialogServiceImpl.DIALOG_RETURN)))
    {
      /**
       * We use pageflow scope so that if something fails on the redirect, we
       * have a chance of getting cleaned up early.  This will not always happen
       * so the object may stick around for a while.
       */
      Map<String, Object> sessionMap = ec.getSessionMap();
      String uid = UUID.randomUUID().toString();
      LaunchData data = new LaunchData((UIViewRoot)reqMap.get(RequestContextImpl.LAUNCH_VIEW), (Map<String, String[]>) reqMap.get(RequestContextImpl.LAUNCH_PARAMETERS));
      sessionMap.put(_getKey(uid), data);
      
      //Construct URL
      //TODO: sobryan I believe some of this can be added to the RequestContextUtils to allow
      //      this url to be constructed for both portlet and servlet environments.  We'll want to research.
      HttpServletRequest req = (HttpServletRequest) ec.getRequest();
      StringBuffer url = req.getRequestURL().append("?");
      String queryStr = req.getQueryString();
      if((queryStr != null) && (queryStr.trim().length() >0))
      {
        url.append(queryStr)
           .append("&");
      }
      
      url.append(_LAUNCH_KEY)
         .append("=")
         .append(uid);

      //Extensions to Trinidad may have alternatve means of handling PPR.  This
      //flag allows those extensions to for the <redirect> AJAX message to be returned.
      if (RequestContext.getCurrentInstance().isPartialRequest(_PSEUDO_FACES_CONTEXT.get()) || 
          Boolean.TRUE.equals(RequestStateMap.getInstance(ec).get(_FORCE_PPR_DIALOG_RETURN)))
      {
        //Special handling for XmlHttpRequest.  Would be cool to handle this much cleaner.
        HttpServletResponse resp = (HttpServletResponse) ec.getResponse();
        XmlHttpConfigurator.sendXmlRedirect(resp.getWriter(), url.toString());
      }
      else
      {
        ec.redirect(url.toString());
      }
    }
  }
  
  private static final class LaunchData implements Serializable
  {
    private UIViewRoot _launchView;
    private Map<String, String[]> _launchParam;
    
    
    public LaunchData(UIViewRoot launchView, Map<String, String[]> launchParam)
    {
      _launchView = launchView;
      if(launchParam != null)
      {
        _launchParam = launchParam;
      }
      else
      {
        _launchParam = Collections.emptyMap();
      }
    }

    private UIViewRoot getLaunchView()
    {
      return _launchView;
    }

    private Map<String, String[]> getLaunchParam()
    {
      return _launchParam;
    }

    private static final long serialVersionUID = 1L;
  }

  private static final class FilterListChain implements FilterChain
  {
    private final List<Filter> _filters;
    private final FilterChain _last;
    private final int _index;
    
    public FilterListChain(List<Filter> filters, FilterChain last)
    {
      this(filters, last, 0);
    }

    private FilterListChain(List<Filter> filters, FilterChain last, int index)
    {
      assert index < filters.size();
      _filters = filters;
      _last = last;
      _index = index;
    }
    
    public void doFilter(ServletRequest request, ServletResponse response)
      throws IOException, ServletException
    {
      int nextIndex = _index+1;
      final FilterChain next;
      // if there are more filters to chain, then keep using
      // FilterListChain; otherwise, just use the last chain:
      if (nextIndex < _filters.size())
        next = new FilterListChain(_filters, _last, nextIndex);
      else
        next = _last;

      _filters.get(_index).doFilter(request, response, next);
    }
  }
  
  /**
   * Returns a potentially wrapped ServletContext for ManagedBean HA
   */
  private ServletContext _getPotentiallyWrappedServletContext(ServletRequest request)
  {
    if (request instanceof HttpServletRequest)
    {
      HttpSession session = ((HttpServletRequest)request).getSession(false);
      
      if (session != null)
      {
        return session.getServletContext();
      }
    }
    
    return _servletContext;
  }

  private ServletContext _servletContext;
  private List<Filter> _filters = null;

  private static final String _LAUNCH_KEY = "_dlgDta";
  private static final String _IS_RETURNING_KEY =
    "org.apache.myfaces.trinidadinternal.webapp.AdfacesFilterImpl.IS_RETURNING";
  private static final String _FILTER_EXECUTED_KEY =
    "org.apache.myfaces.trinidadinternal.webapp.AdfacesFilterImpl.EXECUTED";
  
  //This allows extension which do not use Trinidad's PPR to for an XML dialog return
  private static final String _FORCE_PPR_DIALOG_RETURN =
    "org.apache.myfaces.trinidad.webapp.FORCE_XML_DIALOG_RETURN";

  private static ThreadLocal<PseudoFacesContext> _PSEUDO_FACES_CONTEXT = 
    new ThreadLocal<PseudoFacesContext>();

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TrinidadFilterImpl.class);
}
