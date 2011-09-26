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
package org.apache.myfaces.trinidadinternal.context;

import java.util.List;

import javax.faces.context.ExternalContext;
import javax.servlet.ServletContext;

import javax.servlet.ServletRequest;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.RequestContextFactory;
import org.apache.myfaces.trinidad.context.PageFlowScopeProvider;
import org.apache.myfaces.trinidad.context.PageResolver;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.config.ConfigParser;
import org.apache.myfaces.trinidadinternal.context.external.ServletExternalContext;

/**
 */
public class RequestContextFactoryImpl extends RequestContextFactory
{
  public RequestContextFactoryImpl()
  {
  }

  @Override
  @Deprecated
  public RequestContext createContext(Object context, Object request)
  {
    return createContext(new ServletExternalContext((ServletContext)context, (ServletRequest)request, null));
  }

  @Override
  public RequestContext createContext(ExternalContext externalContext)
  {
    RequestContextImpl impl =  new RequestContextImpl(_getBean(externalContext));
    impl.init(externalContext);
    impl.__setPageResolver(_pageResolver);
    impl.__setPageFlowScopeProvider(_pageFlowScopeProvider);
    return impl;
  }

  private RequestContextBean _getBean(ExternalContext externalContext)
  {
    if (_bean == null)
    {
      synchronized (this)
      {
        if(externalContext != null)
        {
          _bean = ConfigParser.parseConfigFile(externalContext);
        }
        else
        {
          _bean = new RequestContextBean();
        }

        // And let's load the PageResolver and PageFlowScopeProvider
        // while we're in here.
        {
          List<PageResolver> list = ClassLoaderUtils.getServices(_PAGE_RESOLVER_URL);
          _pageResolver = list.isEmpty() ? null : list.get(0);
        }
        if (_pageResolver == null)
        {
          _pageResolver = PageResolverDefaultImpl.sharedInstance();
        }

        {
          List<PageFlowScopeProvider> list = 
            ClassLoaderUtils.getServices(_PAGE_FLOW_SCOPE_PROVIDER_URL);
          _pageFlowScopeProvider = list.isEmpty() ? null : list.get(0);
        }
        if (_pageFlowScopeProvider == null)
        {
          _pageFlowScopeProvider = PageFlowScopeProviderImpl.sharedInstance();
        }
      }      
    }

    return _bean;
  }

  private volatile RequestContextBean _bean;
  private PageResolver        _pageResolver;
  private PageFlowScopeProvider _pageFlowScopeProvider;

  static private final String _PAGE_RESOLVER_URL =
    "org.apache.myfaces.trinidad.context.PageResolver";
  static private final String _PAGE_FLOW_SCOPE_PROVIDER_URL =
    "org.apache.myfaces.trinidad.context.PageFlowScopeProvider";

}
