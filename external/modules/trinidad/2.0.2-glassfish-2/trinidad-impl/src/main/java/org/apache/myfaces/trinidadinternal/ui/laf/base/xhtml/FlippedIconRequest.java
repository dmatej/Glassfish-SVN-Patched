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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.util.Map;

import java.io.IOException;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.cache.FlippedIconKey;

import org.apache.myfaces.trinidadinternal.share.io.ServletNameResolver;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * The ImageProviderRequest class that we use for requesting
 * flipped user icons.  It extends org.apache.myfaces.trinidadinternal.image.cache.FlippedIconKey
 * by adding support for obtaining an InputStreamProvider for the source
 * icon.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FlippedIconRequest.java#0 $) $Date: 10-nov-2005.18:53:48 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
final class FlippedIconRequest extends FlippedIconKey
{

  public FlippedIconRequest(
    UIXRenderingContext context,
    String           source
    )
  {

    super(source);

    ExternalContext external = context.getFacesContext().getExternalContext();
    Object requestObj = external.getRequest();
    if (requestObj instanceof ServletRequest)
      _request = (ServletRequest) requestObj;

    // We need to resolve source now and save it relative to context root
    setSource( _resolveSourceName( external,  source));
  }


  // Override of getRenderProperties() which adds in the
  // InputStreamProvider for the source icon
  @Override
  public Map<Object, Object> getRenderProperties(ImageContext context)
  {
    Map<Object, Object> properties = super.getRenderProperties(context);

    FacesContext fContext = FacesContext.getCurrentInstance();
    Object objContext = fContext.getExternalContext().getContext();
    if (objContext instanceof ServletContext)
    {
      ServletContext servletContext = (ServletContext) objContext;

      NameResolver resolver = new ServletNameResolver( _request,
                                                       servletContext);
      InputStreamProvider provider = resolveSourceIcon(context,
                                                       getSource(),
                                                       resolver);

      properties.put(ImageConstants.SOURCE_INPUT_STREAM_PROVIDER_KEY, provider);
    }

    return properties;
  }



  // Static method used to resolve source icon names
  public static InputStreamProvider resolveSourceIcon(
    ImageContext context,
    String       name,
    NameResolver sourceIconResolver
    )
  {
    try
    {
      return sourceIconResolver.getProvider( name );
    }
    catch (IOException e)
    {
      // If we've got an error log, log the IOException
      if (context != null)
      {
        _LOG.warning(e);
      }
    }

    return null;
  }

 /*
  * Resolve the file name relative to the servlet root if possible
  */
  private String _resolveSourceName(
    ExternalContext  external,
    String           name)
  {
    // if the path is a full URI, get it so it's from the context root
    if ( name.charAt(0) == '/' )
    {
      String contextPath = external.getRequestContextPath();
      if ( name.regionMatches( 0, contextPath, 0, contextPath.length()))
      {
        name = name.substring ( contextPath.length());
      }
      else
      {
        if (_LOG.isWarning())
          _LOG.warning("UNABLE_FLIP_ICON", new Object[]{name, contextPath});
      }
    }
    // otherwise it must be a relative path and needs to be converted to
    // a path from the context root.
    else
    {
      String rootName = external.getRequestServletPath();
      if (rootName != null)
      {
        int endIndex = rootName.lastIndexOf('/') + 1;

        name = rootName.substring(0, endIndex ) + name;
      }
    }

    return name;
  }

  private ServletRequest _request;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FlippedIconRequest.class);
}
