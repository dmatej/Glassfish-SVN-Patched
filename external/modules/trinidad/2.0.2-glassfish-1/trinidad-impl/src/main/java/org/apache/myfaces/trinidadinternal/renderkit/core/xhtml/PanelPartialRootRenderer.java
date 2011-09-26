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

import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXComponent;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitResult;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Renderer for the panelPartialRoot.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/PanelPartialRootRenderer.java#0 $) $Date: 10-nov-2005.19:01:40 $
 */
public class PanelPartialRootRenderer extends XhtmlRenderer
{
  protected PanelPartialRootRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  protected void renderContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // determinie whether we should try and optimize the PPR rendering
    boolean encodeAllChildren = !PartialPageUtils.isOptimizedPPREnabled(context, true);

    if (encodeAllChildren)
    {
      // No PPR optimization, so encode all children
      encodeAllChildren(context, component);
    }
    else
    {
      // perform an optimized partial visit of the children
      PartialPageContext pprContext = rc.getPartialPageContext();

      VisitContext visitContext = pprContext.getVisitContext();

      try
      {
        for (UIComponent currChild : component.getChildren())
        {
          if (UIXComponent.visitTree(visitContext, currChild, _ENCODE_ALL_CALLBACK))
            break;
        }
      }
      catch (FacesException e)
      {
        Throwable cause = e.getCause();

        // unwrap and throw IOExceptions
        if (cause instanceof IOException)
          throw (IOException)cause;
        else
          throw e;
      }
    }
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    XhtmlUtils.addLib(context, rc, "openWindow()");

    if (PartialPageUtils.isPartialRenderingPass(rc))
    {
      // Mark that PPR is in fact active
      PartialPageUtils.markPPRActive(context);

      try
      {
        renderContent(context, rc, component, bean);
      }
      // For RuntimeExceptions and Errors, make sure we don't
      // just drop the error on the ground during PPR requests.
      // ViewHandler.renderView() would be a much, much better place to
      // put this code.  But sadly, ServletExceptions generally
      // swallow whatever they contain instead of exposing it
      // as a cause (at least in OC4J 9.0.4 and Tomcat 5.0)
      catch (RuntimeException re)
      {
        _LOG.severe("ERR_PARTIAL_PAGE_RENDERING", re);
        throw re;
      }
      catch (Error error)
      {
        _LOG.severe("ERR_PARTIAL_PAGE_RENDERING", error);
        throw error;
      }

      renderAtEnd(context, rc);
    }
    else
    {
      boolean alreadyRenderedPPR = PartialPageUtils.isPPRActive(context);
      // @TODO: Find out the reason for the second half of this "or"
      if (!(alreadyRenderedPPR ||
            PartialPageUtils.isPartialRenderingPass(rc)))
      {
        // Render the iframe that we use to make partial page requests
        if (PartialPageUtils.supportsPartialRendering(rc))
        {
          PartialPageUtils.markPPRActive(context);
          renderPPRSupport(context, rc, component, bean);
        }
      }

      renderContent(context, rc, component, bean);
      renderAtEnd(context, rc);
    }
  }

  protected void renderAtEnd(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
  }

  protected boolean isEmbedded()
  {
    return true;
  }

  // Is this a partial page rendering pass?
  protected static boolean isPartialPass(
    RenderingContext rc)
  {
    return (PartialPageUtils.isPartialRenderingPass(rc));
  }

  protected void renderPPRSupport(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    // Render anything that will be needed to block clicks when the
    // partial render is in progress
    _renderPartialBlocking(context, rc, component);
  }

  // Renders the DIV element which is used to block user input during the
  // handling of a partial update.
  private static void _renderPartialBlocking(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component
    ) throws IOException
  {
    if (PartialPageUtils.supportsBlocking(rc))
    {
      ResponseWriter writer = context.getResponseWriter();

      writer.startElement("div",  component);

      writer.writeAttribute("id", _PARTIAL_DIV_ID,
                            null);
      writer.writeAttribute("onclick",
                            _PARTIAL_DIV_CLICK_HANDLER, null);
      writer.writeAttribute("style",
                            _PARTIAL_DIV_STYLE, null);
      writer.writeAttribute("onkeydown", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onkeyup", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onmousedown", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onmouseup", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onkeypress", _PARTIAL_DIV_EAT_KEY_HANDLER, null);

      writer.endElement("div");
    }
  }

  /**
   * Callback for encoding subtrees during optimized PPR tree visits
   */
  private static final class EncodeAllCallback implements VisitCallback
  {
    public VisitResult visit(
      VisitContext context,
      UIComponent  target)
    {
      try
      {
        // we have the subtree we want, render it
        target.encodeAll(context.getFacesContext());
      }
      catch (IOException ioe)
      {
        // launder the IOException as a FacesException, we'll unwrap this later
        throw new FacesException(ioe);
      }

      PartialPageContext pprContext = RenderingContext.getCurrentInstance().getPartialPageContext();

      // if we finished rendering all of the destired targets, return that we are
      // done.  Otherwise, reject this subtree so that we don't traverse into it, since
      // we have already rendered all of the targets in it
      if (pprContext.areAllTargetsProcessed())
        return VisitResult.COMPLETE;
      else
        return VisitResult.REJECT;
    }
  }

  // Div element used for blocking
  private static final String _PARTIAL_DIV_ID  = "tr_pprBlockingDiv";
  private static final String _PARTIAL_DIV_CLICK_HANDLER =
          "return _pprConsumeClick(event);";
  private static final String _PARTIAL_DIV_EAT_KEY_HANDLER = "return false;";
  private static final String _PARTIAL_DIV_STYLE =
          "position:absolute;left:0;top:0;width:0;height:0;cursor:wait;";

  // callback used to render optimized PPR
  private static final VisitCallback _ENCODE_ALL_CALLBACK = new EncodeAllCallback();

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
   PanelPartialRootRenderer.class);
}
