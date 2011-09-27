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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

/**
 * Base class for Java code that is capable of adding scripts to a page.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/Scriptlet.java#1 $) $Date: 11-nov-2005.14:59:39 $
 */
abstract public class Scriptlet
{
  /**
   * Creates the scriptlet.
   */
  public Scriptlet()
  {
  }

  /**
   * Returns the key that uniquely identifies the scriptlet.
   */
  abstract public Object getScriptletKey();

  /**
   * Outputs the scriptlet if needed.  If the scriptlet is unnecessary
   * (or has already been output), this is a no-op.
   */
  public void outputScriptlet(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    if (!_isAlreadyWritten(arc) &&
        !__isOutsidePartialPage(arc))
    {
      _markAsWritten(arc);
      outputScriptletImpl(context, arc);
    }
  }



  // In partial-page rendering, it is possible that a UINode which is not
  // a partial target might include a JavaScript library.  In this case,
  // the library include  won't written out in the generated content, but
  // the appropriate property will be set on the RenderingContext to
  // indicate that the library has been included - unless we explicitly
  // prevent that from happening here.
  //
  // Also note that this method is overridden for the MarlinCore
  // library.  See XhtmlScriptletFactory.
  boolean __isOutsidePartialPage(RenderingContext arc)
  {
    PartialPageContext pprContext = arc.getPartialPageContext();
    if (pprContext == null)
      return false;

    return !pprContext.isInsidePartialTarget();
  }


  /**
   * Registers the scriptlet.
   */
  public void registerSelf()
  {
    registerSelfWithKey(getScriptletKey());
  }

  public void embedInScriptTag(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    if (!_isAlreadyWritten(arc))
    {
      _markAsWritten(arc);
      embedInScriptTagImpl(context, arc);
    }
  }

  /**
   * Outputs the scriptlet.
   */
  protected void outputScriptletImpl(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("script", null);
    XhtmlRenderer.renderScriptDeferAttribute(context, arc);
    // Bug #3426092:
    // Render the type="text/javascript" attribute in accessibility mode
    XhtmlRenderer.renderScriptTypeAttribute(context, arc);

    outputScriptletContent(context, arc);
    writer.endElement("script");
  }

  /**
   * Outputs the scriptlet.
   */
  protected void embedInScriptTagImpl(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    outputScriptletContent(context, arc);
  }

  /**
   * Outputs the content of the scriptlet - the text or attributes
   * between the script elements.
   */
  abstract protected void outputScriptletContent(
    FacesContext        context,
    RenderingContext arc) throws IOException;




  /**
   * Outputs a scriptlet that this scriptlet depends on.
   */
  protected void outputDependency(
    FacesContext        context,
    RenderingContext arc,
    Object              dependency) throws IOException
  {
    XhtmlUtils.addLib(context, arc, dependency);
  }

  /**
   * Registers a scriptlet with a key, perhaps other
   * than the scriptlet's own key.
   */
  public void registerSelfWithKey(Object key)
  {
    XhtmlUtils.registerScriptlet(key, this);
  }

  //
  // Returns true if this scriptlet has already been written.
  //
  private boolean _isAlreadyWritten(RenderingContext arc)
  {
    return arc.getProperties().get(getScriptletKey()) != null;
  }

  //
  // Marks that the scriptlet has been written.
  //
  private void _markAsWritten(RenderingContext arc)
  {
    arc.getProperties().put(getScriptletKey(), Boolean.TRUE);
  }
}
