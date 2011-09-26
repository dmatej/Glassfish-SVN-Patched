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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.html.HtmlFrameBorderLayout;
import org.apache.myfaces.trinidad.component.html.HtmlHtml;
import org.apache.myfaces.trinidad.context.RenderingContext;


/**
 * Renderer for rendering the root document element
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/HtmlRenderer.java#0 $) $Date: 10-nov-2005.19:01:30 $
 */
public class HtmlRenderer extends XhtmlRenderer
{
  /**
   * Identify if standards mode has been disabled.
   * =-=AEW Is this the correct place?
   */
  static public boolean isStandardsModeDisabled(
    FacesContext context)
  {
    String disableStandardsMode =
      context.getExternalContext().getInitParameter(_DISABLE_STANDARDS_MODE);

    return ((disableStandardsMode != null) &&
            disableStandardsMode.equalsIgnoreCase("true"));
  }

  public HtmlRenderer()
  {
    this(HtmlHtml.TYPE);
  }

  protected HtmlRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _modeKey = type.findKey("mode");
  }

  @Override
  protected void encodeBegin(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    String docType = getDocType(context, comp, bean);
    if (docType != null)
      writer.write(docType);

    writer.startElement("html", comp);

    boolean isXML = isXMLDocument(context);

    //
    // Render the document namespace
    //
    if (isXML)
    {
      String documentNamespace = getDocumentNamespace();

      if (documentNamespace != null)
      {
        writer.writeAttribute("xmlns", documentNamespace, null);
      }
    }

    String direction = rc.isRightToLeft() ? "rtl" : "ltr";
    writer.writeAttribute("dir", direction, null);

    // render the correct language
    String lang = rc.getLocaleContext().getTranslationIANALocaleString();
    if (lang != null)
    {
      if (isXML)
        writer.writeAttribute("xml:lang", lang, null);
      else
        writer.writeAttribute("lang", lang, null);
    }
  }

  @Override
  public void encodeEnd(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.endElement("html");
  }

  protected String getMode(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_modeKey));
  }

  /**
   * Subclasses should override to return their doctype
   */
  protected String getDocType(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    // See bug 1893192 - we don't want to render the DTD
    // in Mozilla until we can fix our code to work in their
    // no-quirks mode.
    // however, the fix for bug 2342217 allows us to render the DTD again
    //      if (context.getAgent().getAgentApplication() ==
    //          Agent.APPLICATION_MOZILLA)
    //        return null;

    if (_hasFrameSet(component))
    {
      return getFrameSetDocType(context);
    }
    else
    {
      return getDocumentDocType(context, component, bean);
    }
  }

  /**
   * Returns the document type to use when rendering a frame set, as
   * opposed to a document.
   */
  protected String getFrameSetDocType(
    FacesContext context
    )
  {
    if (isXMLDocument(context))
    {
      return XHTML_FRAMESET_DOCTYPE;
    }
    else
    {
      return HTML_FRAMESET_DOCTYPE;
    }
  }

  /**
   * Returns the document type to use when rendering a document, as opposed
   * to a frameset.
   */
  protected String getDocumentDocType(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    )
  {
    String mode = getMode(component, bean);
    // default to transitional, rather than strict
    if (isXMLDocument(context))
    {
      if (HtmlHtml.MODE_STRICT.equals(mode))
        return XHTML_STRICT_DOCTYPE;
      else
        return XHTML_TRANSITIONAL_DOCTYPE;
    }
    else
    {
      if (isStandardsModeDisabled(context) || HtmlHtml.MODE_QUIRKS.equals(mode))
        return HTML_QUIRKS_DOCTYPE;
      else if (HtmlHtml.MODE_STRICT.equals(mode))
        return HTML_STRICT_DOCTYPE;
      else
        return HTML_STANDARDS_DOCTYPE;
    }
  }

  /**
   * Determines whether we have a frameset component as a child
   * for determining which doctype to return
   */
  @SuppressWarnings("unchecked")
  private boolean _hasFrameSet(UIComponent component)
  {
    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (HtmlFrameBorderLayout.COMPONENT_FAMILY.equals(child.getFamily()))
      {
        return true;
      }
    }

    return false;
  }

  /**
   * Returns the XML namespace to use for this document
   */
  protected String getDocumentNamespace()
  {
    return "http://www.w3.org/1999/xhtml";
  }

  /**
   * Returns true if we are rendering an XML document
   */
  protected boolean isXMLDocument(
    FacesContext context
    )
  {
    String contentType = null;
  	contentType = context.getResponseWriter().getContentType();

    return "text/xml".equals(contentType) ||
           "application/xhtml+xml".equals(contentType) ||
           "application/xml".equals(contentType);
  }

  private PropertyKey _modeKey;

  // DOCTYPE for transitional + quirks
  protected static final String HTML_QUIRKS_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">";

  // DOCTYPE for transitional + standards
  protected static final String HTML_STANDARDS_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">";

  // DOCTYPE for strict + standards
  protected static final String HTML_STRICT_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">";

  // loose plus FRAMESET instead of BODY
  protected static final String HTML_FRAMESET_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">";

  protected static final String XHTML_STRICT_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">";

  protected static final String XHTML_TRANSITIONAL_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">";

  protected static final String XHTML_FRAMESET_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">";

  // basic xhtml doctype
  protected static final String BASIC_XHTML_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.0//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd\">";

  protected static final String XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";

  static private final String _DISABLE_STANDARDS_MODE=
    "org.apache.myfaces.trinidad.ENABLE_QUIRKS_MODE";
}
