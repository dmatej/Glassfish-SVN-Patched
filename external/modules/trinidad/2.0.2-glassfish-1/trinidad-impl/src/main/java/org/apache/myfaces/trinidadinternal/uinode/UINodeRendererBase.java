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
package org.apache.myfaces.trinidadinternal.uinode;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidadinternal.webapp.TrinidadFilterImpl;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 *
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/UINodeRendererBase.java#1 $) $Date: 11-nov-2005.14:59:37 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class UINodeRendererBase extends Renderer
{
  @Override
  public void encodeBegin(FacesContext context,
                          UIComponent component)
    throws IOException
  {
    if (!getRendersChildren())
    {
      UIXComponentUINode adapter = UIXComponentUINode.__getAdapter(component);
      UIXRenderingContext rContext = getRenderingContext(context, component);
      adapter.prerenderInternal(rContext, adapter);
    }
  }

  @Override
  public void encodeChildren(FacesContext context,
                             UIComponent component)
    throws IOException
  {
    // Children-encoding is always handled in encodeEnd()
  }

  @Override
  public void encodeEnd(FacesContext context,
                        UIComponent component)
    throws IOException
  {
    UIXComponentUINode adapter = UIXComponentUINode.__getAdapter(component);
    UIXRenderingContext rContext = getRenderingContext(context, component);

    if (getRendersChildren())
    {
      adapter.renderInternal(rContext, adapter);
    }
    else
    {
      adapter.postrenderInternal(rContext, adapter);
    }
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * Get - and create if necessary - a RenderingContext.
   */
  static protected UIXRenderingContext getRenderingContext(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    return getRenderingContext(context, component, true);
  }

  /**
   * Get - and create if necessary - a RenderingContext.
   */
  static protected UIXRenderingContext getRenderingContext(
    FacesContext context,
    UIComponent  component,
    boolean      createIfNull) throws IOException
  {
    UIXRenderingContext rContext = __getRenderingContext(context);

    if (rContext == null && createIfNull)
    {
      FacesRenderingContext frContext =
        FacesRenderingContext.createRenderingContext(context);
      rContext = frContext;
    }

    return rContext;
  }


  @SuppressWarnings("unchecked")
  static UIXRenderingContext __setRenderingContext(
    FacesContext     fContext,
    UIXRenderingContext rContext)
  {
    if (rContext == null)
      throw new NullPointerException();

    UIXRenderingContext oldContext = __getRenderingContext(fContext);
    if (oldContext == rContext)
      return null;

    fContext.getExternalContext().getRequestMap().put(_CONTEXT_KEY, rContext);
    return oldContext;
  }

  protected boolean skipDecode(FacesContext context)
  {
    // =-=AEW When executing a "dialog return" from the filter,
    // we've generally saved off the original parameters such that
    // decoding again isn't a problem.  But we can run into some problems:
    //  (1) A component that doesn't know about ReturnEvents:  it'll
    //    decode again, thereby firing the event again that launched
    //    the dialog (and you go right back to the dialog)
    //  (2) The component does know about ReturnEvents, but
    //      someone launches a dialog in response to the ReturnEvent,
    //      after setting the value of an input field.  But since
    //      we've still saved off the original parameters,
    //      now we're back in
    // The best fix would really be somehow skipping the Apply Request
    // Values phase altogether, while still queueing the ReturnEvent
    // properly.
    return TrinidadFilterImpl.isExecutingDialogReturn(context);
  }

  @SuppressWarnings("unchecked")
  static void __restoreRenderingContext(
    FacesContext     fContext,
    UIXRenderingContext rContext)
  {
    if (rContext != null)
      fContext.getExternalContext().getRequestMap().put(_CONTEXT_KEY, rContext);
  }

  static UIXRenderingContext __getRenderingContext(FacesContext fContext)
  {
    return (UIXRenderingContext)
      fContext.getExternalContext().getRequestMap().get(_CONTEXT_KEY);
  }

  static private final String _CONTEXT_KEY = "org.apache.myfaces.trinidadinternal.uinode.RenderingContext";
}
