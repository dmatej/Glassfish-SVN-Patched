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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;
import java.io.Writer;

import java.util.Iterator;
import java.util.Map;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PartialPageContextImpl;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.XmlResponseWriter;


/**
 * Utility methods for Renderers which support partial page rendering.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/PartialPageUtils.java#0 $) $Date: 10-nov-2005.19:01:41 $
 */
public final class PartialPageUtils
{
  private PartialPageUtils()
  {
  }

  /**
   * Returns <code>true</code> if optimized PPR is enabled for this request.
   * @return <code>true</code> if optimized PPR is enabled for this request
   */
  public static boolean isOptimizedPPREnabled(FacesContext context, boolean checkIsPPR)
  {
    boolean optimizedPPREnabled = false;
          
    if (!checkIsPPR ||
        (PartialPageUtils.isPartialRequest(context) && PartialPageUtils.isPPRActive(context)))
    {
      Map<Object, Object> contextAttributes = context.getAttributes();
      
      Object optimizedPPR = contextAttributes.get(_OPTIMIZED_PPR_ENABLED_PROP);
      
      if (optimizedPPR != null)
      {
        optimizedPPREnabled = ((Boolean)optimizedPPR).booleanValue();
      }
      else
      {
        // default optimized ppr to off
        optimizedPPREnabled = "on".equalsIgnoreCase(_getPprOptimization(context));
        
        // cache the result into the context attributes
        contextAttributes.put(_OPTIMIZED_PPR_ENABLED_PROP, optimizedPPREnabled);
      }
    }
    
    return optimizedPPREnabled;
  }
  
  /**
   * Check if a NamingContainer has any partial targets
   */
  public static boolean containsPprTargets(
    RenderingContext rc,
    UIComponent      component,
    String           clientId)
  {
    // This function can only be called with NamingContainers, so
    // throw an exception if anyone tries otherwise
    if (!(component instanceof NamingContainer)) 
      throw new IllegalArgumentException();

    // If PPR is off (ppc == null), or we're already rendering
    // (isInsidePartialTarget()), then we have to render, so return true
    PartialPageContext ppc = rc.getPartialPageContext();
    if ((ppc == null) || ppc.isInsidePartialTarget())
      return true;

    // And if we're a partial target ourselves, return true
    if (ppc.isPartialTarget(clientId))
      return true;

    // See if anything starts with our prefix
    String clientIdPrefix = clientId + NamingContainer.SEPARATOR_CHAR;
    Iterator<String> targets = ppc.getPartialTargets();
    while (targets.hasNext())
    {
      String target = targets.next();
      if (target == null)
        continue;
      // Found one!
      if (target.startsWith(clientIdPrefix))
        return true;
    }
    
    // Couldn't find any:  bail
    return false;
  }

  /**
   * This method delegates to the RequestContext.isPartialRequest() with the 
   * exception that JSF Ajax render="@all" requests are reported as non-partial
   * @param context
   * @return
   */
  public static boolean isPartialRequest(FacesContext context)
  {
    RequestContext rc = RequestContext.getCurrentInstance();
    if (rc == null)
      return false;
    boolean isPartial = rc.isPartialRequest(context);
    
    if (isPartial && context.getPartialViewContext().isRenderAll())
    {
      // We do not want to create PartialPageContext and use the tree visit (if enabled)
      // for the 'render all' <f:ajax> case
      isPartial = false;
    }
    
    return isPartial;
  }


  /**
   * Force partial rendering on for requests that may not have sent
   * a "partial" parameter.
   * @todo This is probably unnecessary.
   */
  @SuppressWarnings("unchecked")
  public static void forcePartialRendering(FacesContext context)
  {
    // FIXME: unused
  }

  public static PartialPageContext createPartialPageContext(
    FacesContext    context,
    RequestContext afContext)
  {
    if (isPartialRequest(context))
    {
      // Create the PartialPageContext
      return new PartialPageContextImpl(context, afContext);
    }

    return null;
  }

  /**
   * Returns true if we are performing a partial page render.
   */
  public static boolean isPartialRenderingPass(
    RenderingContext arc
    )
  {
    PartialPageContext pprContext = arc.getPartialPageContext();
    return (pprContext != null);
  }

  /**
   * Tests whether partial page rendering is supported for the
   * current render.
   * <p>
   * Partial page rendering is not supported on all user agents.
   * This method returns false if partial page rendering is not supported
   * by the agent associated with the provided RenderingContext.
   * <p>
   * This method returns false if the disable-partial-rendering configuration 
   * element is set to true. Otherwise, this method returns true.
   * (PPR is considered accessible, so we do not check the accessibility mode)
   */
  public static boolean supportsPartialRendering(
    RenderingContext arc
    )
  {

    // First, make sure the agent supports partial rendering
    Agent agent = arc.getAgent();
    Object capPartial = agent.getCapabilities().get(TrinidadAgent.CAP_PARTIAL_RENDERING);
    if (!Boolean.TRUE.equals(capPartial))
      return false;

    return true;
  }

  public static boolean supportsBlocking(
    RenderingContext arc
    )
  {
    // At the moment we have blocking solved on IE and Mozilla
    if (supportsPartialRendering(arc))
    {
      return (XhtmlRenderer.isIE(arc) || XhtmlRenderer.isGecko(arc));
    }
    return false;
  }


  /**
   * Test if PPR is active during rendering.
   */
  @SuppressWarnings("unchecked")
  public static boolean isPPRActive(FacesContext context)
  {
    Map<String, Object> requestScope =
      context.getExternalContext().getRequestMap();
    
    return Boolean.TRUE.equals(requestScope.get(_PPR_ACTIVE_FLAG_NAME));
  }

  /**
   * Mark that PPR is in fact active during rendering.
   */
  @SuppressWarnings("unchecked")
  public static void markPPRActive(FacesContext context)
  {
    Map<String, Object> requestScope =
      context.getExternalContext().getRequestMap();
    
    requestScope.put(_PPR_ACTIVE_FLAG_NAME, Boolean.TRUE);
  }
  
  /**
   * Forces optimized PPR (using tree visit to render components)
   * @param context
   */
  public static void forceOptimizedPPR(FacesContext context)
  {
    context.getAttributes().put(_OPTIMIZED_PPR_ENABLED_PROP, Boolean.TRUE);
  }
  
  
  /**
   * This method writes a <noop/> to the response. 
   * 
   * @param context the FacesContext
   * @throws IOException 
   */
  public static void renderNoopResponse(FacesContext context) 
    throws IOException
  {
    ExternalContext external = context.getExternalContext();
    Writer writer = ExternalContextUtils.getResponseWriter(external);
    Object response = external.getResponse();
    
    if (response instanceof HttpServletResponse) 
    {
      HttpServletResponse httpResponse = (HttpServletResponse) response;
  
      // Prevent caching
      httpResponse.setHeader("Cache-Control", "no-cache");
      httpResponse.setHeader("Pragma", "no-cache");
      httpResponse.setHeader("Expires", "-1");
    }
    
    XmlResponseWriter xrw = new XmlResponseWriter(writer, "utf-8");
    xrw.startDocument();
    
    xrw.startElement("partial-response", null);
    xrw.startElement("noop", null);
    xrw.endElement("noop");      
    xrw.endElement("partial-response");

    xrw.endDocument();
    xrw.close();
  }

  /**
   * Returns the value of the PPR optimization parameter.  We currently support "on" and "off"
   * @param context
   * @return
   */
  private static String _getPprOptimization(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    
    Map<String, Object> applicationMap = external.getApplicationMap();
    
    // first check if this has been overridden at the application level
    String pprOptimization = (String)applicationMap.get(_PPR_OPTIMIZATION_PROP);
    
    if (pprOptimization == null)
    {
      // the value hasn't been set, so check the initialization parameter
      pprOptimization = external.getInitParameter(_PPR_OPTIMIZATION_PROP);
      
      // default to "off"
      if (pprOptimization == null)
        pprOptimization = "off";
      
      // cache in the application so that we don't need to fetch this again
      applicationMap.put(_PPR_OPTIMIZATION_PROP, pprOptimization);
    }
    
    return pprOptimization;
  }

  // System property controlling whether client ID caching is enabled
  private static final String _PPR_OPTIMIZATION_PROP = 
                                                    "org.apache.myfaces.trinidad.PPR_OPTIMIZATION";

  // Flag used to store info on the context about whether
  // an iFrame is built yet.
  private static final String _PPR_ACTIVE_FLAG_NAME =
          "org.apache.myfaces.trinidadinternal.renderkit._pprActiveOnPage";
  
  
  private static final Object _OPTIMIZED_PPR_ENABLED_PROP = new Object();
}
