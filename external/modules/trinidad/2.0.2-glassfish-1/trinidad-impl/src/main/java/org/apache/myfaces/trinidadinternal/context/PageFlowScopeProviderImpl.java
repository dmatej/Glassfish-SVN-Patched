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

import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.PageFlowScopeProvider;

import org.apache.myfaces.trinidadinternal.share.url.EncoderUtils;

public class PageFlowScopeProviderImpl extends PageFlowScopeProvider
{
  // =-=AEW Should be able to make this private
  static public final String TOKEN_PARAMETER_NAME = "_afPfm";

  static public PageFlowScopeProvider sharedInstance()
  {
    return _SHARED_INSTANCE;
  }

  private PageFlowScopeProviderImpl()
  {
  }

  @Override
  public Map<String, Object> getPageFlowScope(FacesContext context)
  {
    PageFlowScopeMap pageFlowScope = _getPageFlowScope(context);
    if (pageFlowScope == null)
    {
      RequestContextImpl impl = (RequestContextImpl)
        RequestContext.getCurrentInstance();
      int lifetime = impl.__getPageFlowScopeLifetime();
      String token = _getToken(context);
      if (token != null)
      {
        pageFlowScope =
          PageFlowScopeMap.getPageFlowScopeMap(context,
                                               token,
                                               lifetime);
      }

      if (pageFlowScope == null)
      {
        pageFlowScope = new PageFlowScopeMap(lifetime);
        if (token != null)
          pageFlowScope.__invalid = true;
      }

      __setPageFlowScope(context, pageFlowScope);
    }

    return pageFlowScope;
  }

  @Override
  public Map<String, Object> pushPageFlowScope(FacesContext context, boolean copyParent)
  {
    PageFlowScopeMap oldPageFlowScope = (PageFlowScopeMap) getPageFlowScope(context);
    if (oldPageFlowScope.getToken(context) != null)
    {
      // Fork the pageFlowScope.  But only do so if there's
      // actually some reason to keep around the old pageFlow scope.
      // Note that in returnFromPageFlow(), we'll pop to the parent
      // pageFlowScope no matter what - which means that in the scenario
      // here where we didn't fork, we'll end up popping up to a null
      // pageFlowScope.  That's fine (and what we want).
      __setPageFlowScope(context,
                         oldPageFlowScope.createChild(copyParent));
    }

    return oldPageFlowScope;
  }

  @Override
  public Map<String, Object> popPageFlowScope(FacesContext context, boolean discardScope)
  {
    PageFlowScopeMap oldPageFlowScope = _getPageFlowScope(context);

    // Move back to the parent.  Note that we don't necessarily
    // free up the child, because the back button might mean that
    // we'd re-enter the child scope.
    if (oldPageFlowScope != null)
    {
      __setPageFlowScope(context, oldPageFlowScope.getParent());
      if (discardScope)
        oldPageFlowScope.discard();
    }

    return oldPageFlowScope;
  }

  @Override
  public String encodeCurrentPageFlowScopeURL(FacesContext context, String url)
  {
    PageFlowScopeMap pageFlowScope = (PageFlowScopeMap) getPageFlowScope(context);
    if (pageFlowScope == null)
      return url;

    String token = pageFlowScope.getToken(context);
    if (token == null)
      return url;

    return EncoderUtils.appendURLArguments(url,
            new String[]{TOKEN_PARAMETER_NAME, token});
  }

  private String _getToken(FacesContext context)
  {
    String token = (String)
      context.getExternalContext().
        getRequestParameterMap().get(TOKEN_PARAMETER_NAME);
    return token;
  }

  // Get the page flow scope off the FacesContext.  It can't
  // be stored as an instance variable, because this is a
  // shared singleton
  static private PageFlowScopeMap _getPageFlowScope(FacesContext context)
  {
    return (PageFlowScopeMap)
      context.getExternalContext().getRequestMap().get(_PAGE_FLOW_SCOPE_KEY);
  }

  // Store the page flow scope
  // =-=AEW MAKE THIS PRIVATE WHNE OBSOLETE DIALOGSERVICE METHODS
  // ARE REMOVED
  @SuppressWarnings("unchecked")
  static void __setPageFlowScope(FacesContext context,
                                 PageFlowScopeMap pageFlowScope)
  {
    context.getExternalContext().getRequestMap().put(_PAGE_FLOW_SCOPE_KEY,
                                                     pageFlowScope);
  }

  static private PageFlowScopeProvider _SHARED_INSTANCE = 
    new PageFlowScopeProviderImpl();

  static private final String _PAGE_FLOW_SCOPE_KEY = 
  "org.apache.myfaces.trinidadinternal.context.PageFlowScope";   
}
