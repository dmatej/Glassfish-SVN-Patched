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
package org.apache.myfaces.trinidadinternal.config.dispatch;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.util.RequestStateMap;
import org.apache.myfaces.trinidad.util.RequestType;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

public class DispatchResponseConfiguratorImpl extends Configurator
{

  @Override
  public ExternalContext getExternalContext(ExternalContext externalContext)
  {
    if(!isApplied(externalContext))
    {
      RequestType type = ExternalContextUtils.getRequestType(externalContext);
      
      switch(type)
      {
        case RESOURCE:
          externalContext.setResponse(new DispatchResourceResponse(externalContext));
          break;
        case SERVLET:
          externalContext.setResponse(new DispatchServletResponse(externalContext));
          break;
        case RENDER:
          //Are we in a portlet 2.0 container?  If we are not there is no need to provide a
          //wrapper.  This confgigurator is intended to work around an issue with WLS where
          //the content type is reset durring a forward.  In Portlet 1.0, the bridge performs
          //an inlcude rather then a forward.
          if(_PORTLET2)
          {
            //spec is Portlet 2.0 or above.  Use the real wrappers
            externalContext.setResponse(new DispatchRenderResponse(externalContext));
          }
      }
      apply(externalContext);
    }

    //return the original
    return externalContext;
  }

  @SuppressWarnings("unchecked")
  static public String getContentType(
    FacesContext context)
  {
    return (String) RequestStateMap.getInstance(context.getExternalContext()).get(__CONTENT_TYPE_KEY);
  }
  
  /**
   * Returns <code>true</code> if the request wrapper has been applied.
   *
   * @param context
   * @return
   */
  static public boolean isApplied(ExternalContext context)
  {
    return (RequestStateMap.getInstance(context).get(_APPLIED)!=null);
  }

  /**
   *
   */
  @SuppressWarnings("unchecked")
  static public void apply(ExternalContext context)
  {
    RequestStateMap.getInstance(context).put(_APPLIED, Boolean.TRUE);
  }

  static private final String _APPLIED =
    DispatchResponseConfiguratorImpl.class.getName()+".APPLIED";
  static private final boolean _PORTLET2 = ExternalContextUtils.isRequestTypeSupported(RequestType.RESOURCE);
  static final String __CONTENT_TYPE_KEY =
    DispatchResponseConfiguratorImpl.class.getName() + ".CONTENT_TYPE";
}
