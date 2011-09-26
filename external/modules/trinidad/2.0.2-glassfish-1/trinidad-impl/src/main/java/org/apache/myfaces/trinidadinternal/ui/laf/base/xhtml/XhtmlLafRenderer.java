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

import java.io.IOException;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.validator.Validator;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.image.ImageConstants;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProvider;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.style.Style;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.IconURIBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.LafIconProvider;
import org.apache.myfaces.trinidadinternal.ui.laf.base.NodeRoleUtils;
import org.apache.myfaces.trinidadinternal.ui.partial.PartialPageRendererUtils;
import org.apache.myfaces.trinidadinternal.util.FormattedTextParser;
import org.apache.myfaces.trinidad.util.IntegerUtils;


/**
 * Base Rendering class for HTML renderers
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/XhtmlLafRenderer.java#1 $) $Date: 11-nov-2005.14:59:38 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class XhtmlLafRenderer extends BaseLafRenderer
                                   implements XhtmlLafConstants
{
  public static final String TRANSPARENT_GIF = "t.gif";

  /**
   * Override to handle case where this element isn't in a supported module
   * of the current user agent
   */
  @Override
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (isSupportedNode(context, node))
    {
      // render the UINOde
      super.render(context, node);
    }
    else
    {
      // not supported so kick out warning
      if (_LOG.isWarning())
        _LOG.warning("UNSUPPORTED_UINODE", node.getLocalName());
    }
  }


  /**
   * Method returning true if the UINode is supported.  Designed to handle
   * cases where a particular node can't be supported because it relies
   * on a module of XHTML that isn't supported by the current user agent.
   */
  public boolean isSupportedNode(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // assume that all UINodes are supported
    return true;
  }

  /**
   * @param node the structural role of this node is compared with expectedRole
   * @param expectedRole the role that must be satisfied
   * @return true iff the structural role of node satisfies the expectedRole.
   * */
  public static boolean structureSatisfiesRole(UIXRenderingContext context,
                                               UINode node,
                                               NodeRole expectedRole)
  {
    if (node != null)
    {
      NodeRole role = NodeRoleUtils.getStructuralRole(context, node);
      return  (role != null) && role.satisfiesRole(expectedRole);
    }
    return false;
  }

  /**
   * Returns true if the agent supports partial rendering of content.
   */
  public static boolean supportsPartialRendering(
    UIXRenderingContext context
    )
  {
    return PartialPageRendererUtils.supportsPartialRendering(context);
  }

  /**
   * Returns true if the agent supports the <code>button</code> tag.
   */
  public static boolean supportsAdvancedButtons(
    UIXRenderingContext context
    )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_ADVANCED_BUTTONS);
  }

  /**
   * Returns true if the agent supports vertical alignment
   */
  public static boolean supportsVAlign(
    UIXRenderingContext context
    )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_VALIGN);
  }

  /**
   * Returns true if the agent supports wrapping
   */
  public static boolean supportsWrappingDisabled(
    UIXRenderingContext context
    )
  {
    return getBooleanAgentCapability(context, TrinidadAgent.CAP_NOWRAP);
  }

  /**
   * Returns true if the agent supports alt as a tooltip on images
   */
  public static boolean supportsAltRendersTooltipOnImage(
    UIXRenderingContext context
    )
  {
    return getBooleanAgentCapability(context,
                                     TrinidadAgent.CAP_ALT_RENDERS_TOOLTIP_ON_IMAGE);
  }

  /**
   * Returns true if we should render the style elements instead of the
   * style attributes
   */
  public static boolean renderStyleElements(
    UIXRenderingContext context
    )
  {

    return !supportsStyleAttributes(context) &&
           supportsTextPresentation(context);
  }


  /**
   * Override to add support for rendering syle elements
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);

    if (doRenderStyleAttrs(context, node))
    {
      Object styleClass = _getLocalStyleClass(context, node);
      //Object inlineStyle = _getLocalInlineStyle(context, node);

      if (renderStyleElements(context))
      {
        startRenderingStyleElements(context,
                                    null,
                                    /*=-=AEW inlineStyle*/ styleClass);
      }

      // Push the style attrs onto the style attr stack
      XhtmlLafUtils.pushStyleAttrs(context,
                                   (styleClass != null)
                                     ? styleClass.toString()
                                     : null,
                                   null /*=-=AEW inlineStyle*/);
    }
  }

  /**
   * Override to add support for rendering syle elements
   */
  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (doRenderStyleAttrs(context, node))
    {
      if (renderStyleElements(context))
      {
        XhtmlLafUtils.endRenderingStyleElements(context);
      }

      XhtmlLafUtils.popStyleAttrs(context);
    }

    super.postrender(context, node);
  }

  protected void renderShortDesc(UIXRenderingContext context,
                                 UINode node) throws IOException
  {
    // there is a certain amount of controversy about writing this attribute.
    // see bug 1606882
    renderAttribute(context, node, "title", SHORT_DESC_ATTR);
  }


  protected void renderAttributesExceptID(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderShortDesc(context, node);

    // render the styles if the style attributes are supported
    if (supportsStyleAttributes(context) &&
        doRenderStyleAttrs(context, node))
    {
      renderStyleAttrs(context, node);
    }

    // render the events if the event attribtues are supported
    if (supportsIntrinsicEvents(context))
    {
      renderEventHandlers(context, node);
    }
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // render the id
    renderID(context, node);

    renderAttributesExceptID(context, node);
  }


  /**
   * Renders the id of the UINode for html elements that have deprecated name
   * in favor of id. These elements are: &lt;a&gt;, &lt;applet&gt;,
   * &lt;form&gt;, &lt;frame&gt;, &lt;iframe&gt;, &lt;img&gt;, &lt;map&gt;
   * <p>
   * This method is expected to be called by an override of
   * <code>renderID</code> in renderers that render the above elements.
   * Such renderers should also override <code>getID</code> to call
   * <code>getIDOrName</code>.
   * @see #getIDOrName
   * @see #getCachedIDOrName */
  protected final void renderNameAndID(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object id = getID(context, node);

    if (id != null)
    {
      ResponseWriter writer = context.getResponseWriter();

      // only ouput the id if the id is supported
      renderID( context, id, false);

      // only output the name if the name is supported
      if (supportsNameIdentification(context))
      {
        writer.writeAttribute("name", id, null);
      }
    }
  }

  public static void renderScriptOnce(
    UIXRenderingContext context,
    String script,
    Object key
    ) throws IOException
  {
    if (supportsScripting(context))
    {
      // check to see if we've written this already
      if (!isPreviouslyRendered(context, key))
      {
        // nope, write it out now
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement(UIConstants.SCRIPT_NAME, null);
        renderScriptDeferAttribute(context);

        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        XhtmlLafRenderer.renderScriptTypeAttribute(context);

        writer.writeText(script, null);
        writer.endElement(UIConstants.SCRIPT_NAME);
      }
    }
  }

  /**
   * Use this method to make sure that certain code is called only once per
   * render cycle.
   * @param key This key is used to set a property on the context to indicate
   * that this method has been previously called.
   * @return The first time this method is called with a specific context and
   * key, it returns false. On each subsequent call, it returns true.  */
  public static boolean isPreviouslyRendered(UIXRenderingContext context,
                                             Object key)
  {
    if (context.getProperty(MARLIN_NAMESPACE, key) == null)
    {
      context.setProperty(MARLIN_NAMESPACE, key, key);
      return false;
    }
    return true;
  }

  /**
   * Returns the id of the UINode for html elements that have deprecated name
   * in favor of id. These elements are: &lt;a&gt;, &lt;applet&gt;,
   * &lt;form&gt;, &lt;frame&gt;, &lt;iframe&gt;, &lt;img&gt;, &lt;map&gt;
   * <p>
   * This method is expected to be called by an override of
   * <code>getID</code> in renderers that render the above elements.
   * Such renderers should also override <code>renderID</code> to call
   * <code>renderNameAndID</code>.
   * @see #renderNameAndID
   * @see #getCachedIDOrName */
  protected final Object getIDOrName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object id = node.getAttributeValue(context, ID_ATTR);

    if (id == null)
    {
      id = getTransformedName(context, node);
    }

    return id;
  }


  /**
   * Version of getIDOrName, using local property caching
   * <p>
   * @see #getIDOrName
   */
  protected final Object getCachedIDOrName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object result = context.getLocalProperty(0, ID_ATTR, _DOES_NOT_EXIST);

    if (result == _DOES_NOT_EXIST)
    {
      // cache the attribute value for the next time
      result = node.getAttributeValue(context, ID_ATTR);

      if (result == null)
      {
        result = getTransformedName(context, node);
      }

      context.setLocalProperty(ID_ATTR, result);
    }

    return result;
  }


  /**
   * Returns the value associated with the text attribute
   */
  protected Object getText(UIXRenderingContext context,  UINode  node)
  {
    Object o = node.getAttributeValue(context, TEXT_ATTR);

    if (o == null)
      return null;

    return o.toString();
  }


  /**
   * Returns the StyleClass to use to render this node.
   */
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, STYLE_CLASS_ATTR);
  }


  /**
   * Returns the inline Style used to render this node.
   */
  protected Object getInlineStyle(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, INLINE_STYLE_ATTR);
  }


  /**
   * Returns true if the style attributes should be rendered for this node.
   * <p>
   * Clients should override this method if they need to move the rendering
   * of the style attributes to a different element, or if the the user agent
   * doesn't support style attributes.
   * <p>
   * @see #renderStyleAttrs
   */
  protected boolean doRenderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return true;
  }
  
  public static void renderStyleAndClass(
    UIXRenderingContext context,
    String           inlineStyle,
    Object           styleClass
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if (styleClass != null)
    {
      if (supportsClassAttribute(context))
      {
        _renderStyleClassAttributeImpl(context, styleClass);
      }
      else
      {
        // I'm assuming that the inline style doesn't happen to use
        // any of the same styles as the styleClass.  This is a
        // bad assumption which may eventually break, but it makes
        // this function much, much more efficient.
        CoreStyle inlineStyleClass = XhtmlLafUtils.getClassStyle(context,
                                                             styleClass);
        renderInlineStyleAttribute(context, inlineStyleClass);

        if (inlineStyle != null)
        {
          writer.writeAttribute("style", ";", null);
          writer.writeAttribute("style", inlineStyle, null);
        }

        // RETURN IMMEDIATELY
        return;
      }
    }

    writer.writeAttribute("style", inlineStyle, null);
  }
  /**
   * Actually renders the style attributes styleClass and inlineStyle on
   * the component, as well as the defaultStyleClass.
   * @see #doRenderStyleAttrs
   * @see #supportsStyleAttributes
   */
  protected void renderStyleAttrs(
    UIXRenderingContext context,
    UINode           node,
    String           defaultStyleClass
    ) throws IOException
  {
    // Get the style class and inline style
    Object styleClass = getStyleClass(context, node);
    if (defaultStyleClass != null)
    {
      if(styleClass!= null )
      {
        renderStyleClassAttributes(context,
        new Object[]{styleClass, defaultStyleClass});
      }
      else
      {
        renderStyleClassAttribute(context, defaultStyleClass);
      }
    }
    else if (styleClass != null)
    {
      renderStyleClassAttribute(context, styleClass);
    }

    Object inlineStyle = getInlineStyle(context, node);

    //render the inlineStyles
     renderInlineStyleAttribute(context, inlineStyle);
  }

  /**/

  /**
   * Actually renders the style attributes.
   * @see #doRenderStyleAttrs
   * @see #supportsStyleAttributes
   */
  protected void renderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderStyleAttrs(context, node, null);
  }


  /**
   * Renders the inline style attribute for the specified node
   */
  protected final void renderInlineStyleAttribute(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderInlineStyleAttribute(context, getInlineStyle(context, node));
  }

  /**
   * Renders the inline style attribute for the specified node
   */
  public static void renderInlineStyleAttribute(
    UIXRenderingContext context,
    Object            inlineStyle
    ) throws IOException
  {
    if (inlineStyle != null)
    {
      if (supportsStyleAttributes(context))
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.writeAttribute("style", inlineStyle, null);
      }

      // Push the inline style onto the stack
      context.setLocalProperty(_INLINE_STYLE_KEY, inlineStyle);
    }
    else
    {
      // Push the lack of an inline style onto the stack
      context.setLocalProperty(_INLINE_STYLE_KEY, _DOES_NOT_EXIST);
    }
  }

 /**
   * Renders style class attributes, using a short style class
   * if one is available.
   * @param inlineStyleString this is appended to the end of any inline styles
   *   that are rendered. This is rendered regardless of whether any
   *   other inline style is rendered.
   */
  public static void renderStyleClassAttributes(
    UIXRenderingContext context,
    String           styleClass1,
    String           styleClass2,
    String           inlineStyleString
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    if ( styleClass1 == null )
    {
      renderStyleClassAttribute(context, styleClass2);
      writer.writeAttribute("style", inlineStyleString, null);
      return;
    }

    if ( styleClass2 == null )
    {
      renderStyleClassAttribute(context, styleClass1);
      writer.writeAttribute("style", inlineStyleString, null);
      return;
    }

    // turn styleClass1 into an inline style at the end?
    boolean inlineStyleClass1 = true;
    // turn styleClass2 into an inline style at the end?
    boolean inlineStyleClass2 = true;


    if (supportsClassAttribute(context))
    {
      if (supportsMultipleCssSelectors(context))
      {
        inlineStyleClass1 = false;
        inlineStyleClass2 = false;

        StringBuffer classes = new StringBuffer(styleClass1.length() +
                                                styleClass2.length() +
                                                1);

        Object shortStyleClass1 = XhtmlLafUtils.getShortStyleClass(context,
                                                             styleClass1);

        Object shortStyleClass2 = XhtmlLafUtils.getShortStyleClass(context,
                                                             styleClass2);
        classes.append(shortStyleClass1);
        classes.append(' ');
        classes.append(shortStyleClass2);

        writer.writeAttribute("class", classes.toString(), null);
      }
      else
      {
        // render one style class, but render the rest as inline style below
        inlineStyleClass1 = false;
        renderStyleClassAttribute( context, styleClass1);

      }

    }

    int inlineLength = 0;

    if ( inlineStyleString != null)
      inlineLength = inlineStyleString.length();

    if ( inlineStyleClass1 )
      inlineLength = inlineLength + styleClass1.length();

    if ( inlineStyleClass2)
      inlineLength = inlineLength + styleClass2.length();

    StringBuffer inline = new StringBuffer(inlineLength);

    if (  inlineStyleClass1 )
      inline.append( XhtmlLafUtils.getClassStyle(context, styleClass1).toInlineString()).append(';');

    if (  inlineStyleClass2 )
      inline.append( XhtmlLafUtils.getClassStyle(context, styleClass2).toInlineString()).append(';');

    if ( inlineStyleString != null )
      inline.append(inlineStyleString);

    if ( inline.length() > 0 )
      writer.writeAttribute("style", inline.toString(), null);
  }


  /**
   * Renders style class attributes, using a short style class
   * if available.
   *
   * If multiple selectors not supported, renders as inline style
   */
  public static void renderStyleClassAttributes(
    UIXRenderingContext context,
    Object[]         styleClasses
    ) throws IOException
  {
    renderStyleClassAttributes( context, styleClasses, null);
  }

  /**
   * Renders style class attributes, using a short style class
   * if one is available.
   * @param renderInlineStyles if style classes aren't supported or
   *   multiple style selectors aren't supported,
   *   tells whether or not to render inline versions of styles
   * @param inlineStyleString this is appended to the end of any inline styles
   *   that are rendered. This is rendered regardless of whether any
   *   other inline style is rendered.
   */
  public static void renderStyleClassAttributes(
    UIXRenderingContext context,
    Object[]         styleClasses,
    String           inlineStyleString
    ) throws IOException
  {
    int j = 0;

    if (supportsClassAttribute(context))
    {
      if (supportsMultipleCssSelectors(context))
      {
        j = styleClasses.length;
        StringBuffer classes = new StringBuffer(20);

        for ( int i = 0; i < styleClasses.length; i++ )
        {
          Object shortStyleClass = XhtmlLafUtils.getShortStyleClass(context,
                                                               styleClasses[i]);

          Object styleClass = null;

          if (shortStyleClass != null)
            styleClass = shortStyleClass;

          if ( styleClass != null )
          {
            if ( i == 0 )
            {
              classes.append(styleClass);
            }
            else
            {

                classes.append(' ');
                classes.append(styleClass);
            }
          }
        }

        ResponseWriter writer = context.getResponseWriter();
        writer.writeAttribute("class", classes.toString(), null);
      }
      else
      {
        // render one style class, but render the rest as inline style below
        renderStyleClassAttribute( context, styleClasses[0]);
        j = 1;
      }

    }

    StringBuffer inline = new StringBuffer();

    for ( ; j < styleClasses.length; j++ )
    {
      Object styleClass = styleClasses[j];
      Style inlineStyle = XhtmlLafUtils.getClassStyle(context, styleClass);

      if (inlineStyle != null )
        inline.append(inlineStyle.toInlineString());
    }

    if ( inlineStyleString != null )
      inline.append( inlineStyleString);

    ResponseWriter writer = context.getResponseWriter();

    if ( inline.length() > 0 )
      writer.writeAttribute("style", inline.toString(), null);

  }


  /**
   * Renders the style class attribute, using a short style class
   * if one is available.
   */
  public static void renderStyleClassAttribute(
    UIXRenderingContext context,
    Object           styleClass
    ) throws IOException
  {
    if (supportsClassAttribute(context))
    {
      Object shortStyleClass = XhtmlLafUtils.getShortStyleClass(context,
                                                                styleClass);
      if (shortStyleClass != null)
        styleClass = shortStyleClass;

      ResponseWriter writer = context.getResponseWriter();
      writer.writeAttribute("class", styleClass, null);
    }
    else
    {
      renderInlineStyleAttribute(context,
                                 XhtmlLafUtils.getClassStyle(context,
                                                             styleClass));
    }
  }

  /**
   * Renders the style class attribute, using a short style class
   * if one is available.
   */
  private static void _renderStyleClassAttributeImpl(
    UIXRenderingContext context,
    Object           styleClass
    ) throws IOException
  {
    Object shortStyleClass = XhtmlLafUtils.getShortStyleClass(context,
                                                              styleClass);
    if (shortStyleClass != null)
      styleClass = shortStyleClass;

    ResponseWriter writer = context.getResponseWriter();
    writer.writeAttribute("class", styleClass, null);
  }

  /**
   * Renders a pre-shortened style class attribute.  This method
   * may only be called with a "shortStyleClass" that has already
   * been passed through getShortStyleClass.
   * @see XhtmlLafUtils#getShortStyleClass
   */
  public static void renderShortStyleClassAttribute(
    UIXRenderingContext context,
    Object           shortStyleClass
    ) throws IOException
  {
    /*
    // This method may only be called with an already abbreviated
    // style...
    if (Assert.DEBUG)
      Assert.assertion(shortStyleClass == getShortStyleClass(context, shortStyleClass);
     */
    if (supportsClassAttribute(context))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.writeAttribute("class", shortStyleClass, null);
    }
    else
    {
      renderInlineStyleAttribute(context,
                                 XhtmlLafUtils.getClassStyle(context,
                                                             shortStyleClass));
    }
  }

  /**
   * Method for rendering one pixel lines.
   * @param context the <code>RenderingContext</code>
   * @throws IOException
   */
  public static void renderOnePixelLine(UIXRenderingContext context) throws IOException {
      final String style = "p_OraOnePixelLine";
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("div",null);
      renderStyleClassAttribute(context,style);
      writer.endElement("div");
  }


  /**
   * Renders the "defer" attribute for a script element.
   * In order to support partial page rendering, scripts must
   * be rendered using the "defer" attribute to ensure that
   * they are executed in the appropriate context.  However,
   * some browsers (eg. IE) do not reliably load scripts
   * which are deferred.  This method detects whether the
   * target agent can handle the defer attribute correctly and
   * only renders this attribute when as appropriate.
   * <p>
   * Note: ResponseWriter.startElement("script", null) must be called
   * before calling this method.
   */
  public static void renderScriptDeferAttribute(UIXRenderingContext context)
    throws IOException
  {
    // At the moment we only render the defer attribute if
    // we are in the partial rendering pass.  This is to
    // avoid the "object expected" JavaScript errors that we
    // see (bug 2251656).  We need to do further browser
    // testing to determine whether we might enable deferred
    // scripts in more cases.

    // Note that we only want to defer scripts once we are actually
    // inside the body content - and writing content to the
    // ScriptBufferingResponseWriter.  Otherwise, we run into bug
    // 2466017.
    if (getRenderingProperty(context, __DEFER_SCRIPTS_KEY) != null)
      context.getResponseWriter().writeAttribute("defer", Boolean.TRUE, null);
  }


  /**
   * Checks whether in screen reader mode, and if so, renders "type" attribute
   * for a script element.
   * <p>
   *
   * Note: ResponseWriter.startElement("script", null) must be called
   * before calling this method.
   *
   * [ =-= mll added 20-Apr-04 to address bug 3426092 ]
   *
   */
  public static void renderScriptTypeAttribute(UIXRenderingContext context)
    throws IOException
  {
    if (isScreenReaderMode(context))
    {
      context.getResponseWriter().writeAttribute("type", _ACCESSIBILITY_SCRIPT_TYPE, null);
    }
  }


  /**
   * Renders the combination of inline and class style attributes
   * as elements
   */
  protected final void startRenderingStyleElements(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    startRenderingStyleElements(context,
                                null/*=-=AEW getInlineStyle(context, node)*/,
                                getStyleClass(context, node));
  }


  /**
   * Renders the combination of inline and class style attributes
   * as elements
   */
  protected final void startRenderingStyleElements(
    UIXRenderingContext context,
    CoreStyle            inlineStyle,
    Object           className
    ) throws IOException
  {
    XhtmlLafUtils.startRenderingStyleElements(
                                context,
                                inlineStyle,
                                XhtmlLafUtils.getClassStyle(context, className));
  }


  /**
   * Renders event handlers for the node.
   */
  protected void renderEventHandlers(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // don't write onclick and ondblclick handlers for disabled nodes
    if (!isDisabled(context, node))
    {
      renderAttribute(context, "onclick",    getOnClick(context, node));
      renderAttribute(context, "ondblclick", getOnDoubleClick(context, node));
    }

    renderAttribute(context, "onmousedown", getOnMouseDown(context, node));
    renderAttribute(context, "onmouseup",   getOnMouseUp(context, node));
    renderAttribute(context, "onmouseover", getOnMouseOver(context, node));
    renderAttribute(context, "onmousemove", getOnMouseMove(context, node));
    renderAttribute(context, "onmouseout",  getOnMouseOut(context, node));

    renderAttribute(context, "onkeypress", getOnKeyPress(context, node));
    renderAttribute(context, "onkeydown",  getOnKeyDown(context, node));
    renderAttribute(context, "onkeyup",    getOnKeyUp(context, node));
  }


  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_CLICK_ATTR);
  }

  protected Object getOnDoubleClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_DOUBLE_CLICK_ATTR);
  }

  protected Object getOnKeyDown(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_KEY_DOWN_ATTR);
  }

  protected Object getOnKeyPress(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_KEY_PRESS_ATTR);
  }

  protected Object getOnKeyUp(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_KEY_UP_ATTR);
  }

  protected Object getOnMouseDown(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_MOUSE_DOWN_ATTR);
  }

  protected Object getOnMouseUp(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_MOUSE_UP_ATTR);
  }

  protected Object getOnMouseOver(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_MOUSE_OVER_ATTR);
  }

  protected Object getOnMouseMove(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_MOUSE_MOVE_ATTR);
  }

  protected Object getOnMouseOut(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_MOUSE_OUT_ATTR);
  }

  protected void renderSpacer(
    UIXRenderingContext      context,
    Object                width,
    Object                height,
    Object                id
    ) throws IOException
  {
    String widthString = _toString(width);
    String heightString = _toString(height);

    renderTransparent(context, widthString, heightString, true, id);
  }


  protected void renderSpacer(
    UIXRenderingContext      context,
    Object                width,
    Object                height
    ) throws IOException
  {
    renderSpacer(context, width, height, null);
  }

  protected void renderSpacer(
    UIXRenderingContext context,
    Integer          width,
    Integer          height
    ) throws IOException
  {
    int widthInt = (width != null)
                     ? width.intValue()
                     : -1;

    int heightInt = (height != null)
                     ? height.intValue()
                     : -1;

    renderSpacer(context, widthInt, heightInt);
  }


  protected void renderSpacer(
    UIXRenderingContext context,
    int              width,
    int              height
    ) throws IOException
  {
    // replace with more compact javascript representation
    String widthString = (width >= 0)
                           ? IntegerUtils.getString(width)
                           : null;

    String heightString = (height >= 0)
                           ? IntegerUtils.getString(height)
                           : null;

    renderTransparent(context, widthString, heightString, false);
  }

  /**
   *
   * @param context
   * @param width
   * @throws IOException
   */
  protected void renderHorizontalSpacer(UIXRenderingContext context, String width)
      throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("b", null);
    writer.writeAttribute("style", "margin-left:" + width + "px", null);
    writer.endElement("b");
  }

  protected void renderVerticalSpacer(UIXRenderingContext context, Object height)
      throws IOException
  {
    if (height != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("div", null);
      writer.writeAttribute("style", "margin-top:" + height + "px", null);
      writer.endElement("div");
    }
  }

  /**
   * Renders a transparent gif using a script to save space.
   */
  protected void renderTransparent(
    UIXRenderingContext context,
    String           width,
    String           height,
    boolean          needsQuoting
    ) throws IOException
  {
    renderTransparent(context, width, height, needsQuoting, null);
  }

  /**
   * Renders a transparent gif using a script to save space.
   */
  protected void renderTransparent(
    UIXRenderingContext context,
    String           width,
    String           height,
    boolean          needsQuoting,
    Object           id
    ) throws IOException
  {
    Counter counter =
      (Counter) getRenderingProperty(context, _SCRIPT_SPACER_COUNT);
    if (counter == null)
    {
      counter = new Counter();
      setRenderingProperty(context, _SCRIPT_SPACER_COUNT, counter);
    }
    int count = counter.count++;

    // do not use the script spacer, if we have already rendered an enormous
    // number of spacers. bug 3786394:
    boolean useScript =
      ((count < 800)
       && (TrinidadAgent.SCRIPTING_SPEED_CAP_FAST ==
           getAgentCapability(context, TrinidadAgent.CAP_SCRIPTING_SPEED))
       && (null ==
           getAgentCapability(context, TrinidadAgent.CAP_IS_JDEV_VE)));
    _renderTransparent(context, width, height, needsQuoting, id, useScript);
  }

  /**
   * @param useScript use javascript to render the spacer. if this is false,
   * then an html IMG tag will be used.
   */
  private void _renderTransparent(
    UIXRenderingContext context,
    String           width,
    String           height,
    boolean          needsQuoting,
    Object           id,
    boolean          useScript
    ) throws IOException
  {
    PartialPageContext pContext = context.getPartialPageContext();

    // cannot use t() in MarlinCore.js on a partial rendering pass
    // just render the icon.
    if (!useScript || (pContext != null))
    {
      renderIcon(context, TRANSPARENT_GIF, width, height, id);
    }
    else
    {
      // IE has fast javascript, so render has a js function call
      ResponseWriter writer = context.getResponseWriter();

      Boolean isTransparentURLSet =
        (Boolean) getRenderingProperty(context,
                                      __TRANSPARENT_URL_KEY);

      if (isTransparentURLSet == null)
      {
        setRenderingProperty(context, __TRANSPARENT_URL_KEY, Boolean.TRUE);

        // determine the transparent image's URL
        String transparentURL = getBaseImageURI(context) + TRANSPARENT_GIF;

        // make sure the transparent image function is loaded
        XhtmlLafUtils.addLib(context, "t()");

        writer.startElement("script", null);
        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        XhtmlLafRenderer.renderScriptTypeAttribute(context);

        if (id != null)
          renderID(context, id, false);

        // store transparentURL as javascript variable
        // which is used in t()
        writer.write("var _tURL=\"" + transparentURL + "\";");

        // store accessibility mode as javascript variable
        // which is used in t()
        writer.write("var _axm");
        if (!isInaccessibleMode(context))
          writer.write("=1");
        writer.write(";");
      }
      else
      {
        writer.startElement("script", null);
        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        XhtmlLafRenderer.renderScriptTypeAttribute(context);
        if (id != null)
          renderID(context, id, false);
      }

      // write a reference to the transparent gif function
      writer.write("t(");

      if ((width != null) || (height != null))
      {
        String widthParam = "void 0";

        if (width != null)
        {
          widthParam = width;

          if (needsQuoting)
          {
            writer.write("'");
          }
        }

        writer.write(widthParam);

        if (needsQuoting && (width != null))
        {
          writer.write("'");
        }

        if (height != null)
        {
          writer.write(",");

          if (needsQuoting)
          {
            writer.write("'");
          }

          writer.write(height);

          if (needsQuoting)
          {
            writer.write("'");
          }
        }
      }

      writer.write(")");

      writer.endElement("script");
    }
  }



  /**
   * Renders an icon.  If the the specified iconURL is a relative URI,
   * it is appended to the value of the base Image URI from
   * the Configuration to form an absolute path.
   */
  protected void renderIcon(
    UIXRenderingContext context,
    Object           iconURL,
    Object           width,
    Object           height
    ) throws IOException
  {
    renderIcon(context, iconURL, width, height, null);
  }

  /**
   * Renders an icon.  If the the specified iconURL is a relative URI,
   * it is appended to the value of the base Image URI from
   * the Configuration to form an absolute path.
   */
  protected void renderIcon(
    UIXRenderingContext context,
    Object           iconURL,
    Object           width,
    Object           height,
    Object           id
    ) throws IOException
  {
    renderIcon(context, iconURL, width, height, id, null /*altText*/);
  }

  /**
   * Renders an icon.  If the the specified iconURL is a relative URI,
   * it is appended to the value of the base Image URI from
   * the Configuration to form an absolute path.
   */
  protected final void renderIcon(
    UIXRenderingContext context,
    Object           iconURL,
    Object           width,
    Object           height,
    Object           id,
    Object           altText
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("img", null);
    if (id != null)
      renderID(context, id, false);

    // Convert iconURL to an absolute uri
    writeAbsoluteImageURI(context, "src", iconURL.toString());

    if (altText != null)
      writer.writeAttribute("alt", altText, null);
    else if (!isInaccessibleMode(context))
      writer.writeAttribute("alt", "", null);

    if (width != null)
    {
      writer.writeAttribute("width", width, null);
    }

    if (height != null)
    {
      writer.writeAttribute("height", height, null);
    }

    writer.endElement("img");
  }


  protected void renderIcon(
    UIXRenderingContext context,
    Object           iconURL,
    int              width,
    int              height
    ) throws IOException
  {
    renderIcon(context,
               iconURL,
               (width  != -1) ?  getInteger(width) : null,
               (height != -1) ?  getInteger(height) : null);
  }


  /**
   * iconURI must be an absolute URI
 * @param isBlock wether the icon should be rendered in block mode
   */
  protected void renderIcon(
    UIXRenderingContext context,
    String           iconAbsoluteURI,
    String           altTextKey,
    Object           destination,
    Object           anchor,
    Object           onClick,
    Object           imgAlign,
    Object           targetFrame,
    boolean isBlock
    ) throws IOException
  {
    if (iconAbsoluteURI != null)
    {
      ResponseWriter writer = context.getResponseWriter();

      if ((destination != null) ||
          (onClick != null)     ||
          (anchor != null))
      {
        if (supportsNavigation(context))
        {
          writer.startElement("a", null);
          renderEncodedActionURI(context, HREF_ATTRIBUTE, destination);
          writer.writeAttribute("onclick", onClick, null);
          writer.writeAttribute("target", targetFrame, null);

          if (anchor != null)
          {
            writer.writeAttribute("name", anchor, null);
          }
        }
      }

      writer.startElement("img", null);

      Object altText = null;
      if (altTextKey != null)
        altText = getTranslatedValue(context, altTextKey);

      // Ensure that we're never rendering null;  see bug 4161181
      // why this logic is not in renderAltAndTooltipForImage().
      // This is, in essence, re-introducing a more restricted version
      // of that bug, but avoids needing to chase down
      // all the calls to XhtmlLafRenderer.renderIcon() at this late date.
      renderAltAndTooltipForImage(context, altText == null ? "" : altText);

      // get the correct alignment to use for the agent
      writer.writeAttribute("align", imgAlign, null);

      renderEncodedResourceURI(context, "src", iconAbsoluteURI);
      writer.writeAttribute("border", "0", null);
      if (isBlock)
      {
        XhtmlLafRenderer
            .renderStyleClassAttribute(context, "p_OraDisplayBlock");
      }
      writer.endElement("img");

      if ((destination != null) || (onClick != null) || (anchor != null))
      {
        if (supportsNavigation(context))
          writer.endElement("a");
      }
    }
  }

  protected void renderIcon(UIXRenderingContext context, Object iconURL,
      Object width, Object height, boolean isBlock) throws IOException
  {
    String fulluri = getBaseImageURI(context) + iconURL.toString();
    renderIcon(context,fulluri,null,null,null,null,null,null,isBlock);
  }



  protected void renderStretchedImage(
    UIXRenderingContext context,
    String           imageURL,
    int              height
    ) throws IOException
  {
    renderIcon(context, imageURL, "100%", getInteger(height));
  }


  public static void writeAbsoluteImageURI(
    UIXRenderingContext context,
    String           attribute,
    String           uri) throws IOException
  {
    if ((uri == null) || (uri.length() == 0))
      return;

    ResponseWriter writer = context.getResponseWriter();

    String encodedUri = getBaseImageURI(context) + uri;
    FacesContext facesContext = context.getFacesContext();
    if (facesContext != null)
      encodedUri = facesContext.getExternalContext().encodeResourceURL(encodedUri);
    writer.writeURIAttribute(attribute, encodedUri, null);
  }


  protected static void renderHAlign(
    UIXRenderingContext context,
    Object           hAlign) throws IOException
  {
    if (hAlign != null)
    {
      boolean rtl = isRightToLeft(context);

      if ("start".equals(hAlign))
      {
        hAlign = (rtl)
                  ? "right"
                  : "left";
      }
      else if ("end".equals(hAlign))
      {
        hAlign = (rtl)
                  ? "left"
                  : "right";
      }

      context.getResponseWriter().writeAttribute("align", hAlign, null);
    }
  }


  protected void renderHAlign(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderHAlign(context, node.getAttributeValue(context, H_ALIGN_ATTR));
  }


  /**
   * Renders the text with the access key highlighted as appropriate.
   */
  protected void renderAccessKeyText(
    UIXRenderingContext context,
    Object           textValue,
    int              keyIndex,
    String           accessKeyClass
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if (textValue != null)
    {
      // create the String containing the access key, if any.
      // we don't render access key indicators on Netscape because
      // Netscape doesn't support accesskeys
      if ((keyIndex != -1) && supportsAccessKeys(context))
      {
        String textString = textValue.toString();

        char[] textChars = textString.toCharArray();

        // write text before the mnemonic
        writer.writeText(textChars, 0, keyIndex);

        if (accessKeyClass != null && accessKeyClass.length() > 0){
        
          writer.startElement ("span", null);
          XhtmlRenderer.renderStyleClass (context.getFacesContext(), 
                                          RenderingContext.getCurrentInstance(),  
                                          accessKeyClass);                                          
          writer.writeText (textChars, keyIndex, 1);
          writer.endElement ("span");
          
          // write text after the mnemonic
          keyIndex++;
        }

        int charsLeft = textChars.length - keyIndex;

        if (charsLeft > 0)
        {
          writer.writeText(textChars, keyIndex, charsLeft);
        }
      }
      else
      {
        // output the text directly since we have no access key
        writer.writeText(textValue, null);
      }
    }
  }

  /**
   * Renders the text with the access key highlighted as appropriate.
   */
  protected void renderAccessKeyText(
    UIXRenderingContext context,
    UINode           node,
    Object           textValue,
    String           accesskeyClass
    ) throws IOException
  {
    Object textString = (textValue != null)
                          ? textValue.toString()
                          : null;

    renderAccessKeyText(context,
                        textString,
                        getAccessKeyIndex(context, node, textString),
                        accesskeyClass);
  }

  /**
   * Extension of XhtmlLafRenderer.getResolvedSelectedIndex().
   * Specialized renderers who need frequent access to the selected index are
   * expected to call this method. We cache the selected index as local context
   * property on first call, and retrieve from cache on subsequent calls.
   */
  protected int getResolvedSelectedIndexFromCache(
    UIXRenderingContext context,
    UINode node
    )
  {
    int resolvedSelectedIndexFromCache = 0;
    Integer cachedIndex = (Integer)context.getLocalProperty(0,
      _SELECTED_CHILD_INDEX_KEY, null);
    if (cachedIndex != null)
    {
      resolvedSelectedIndexFromCache = cachedIndex.intValue();
    }
    else
    {
      resolvedSelectedIndexFromCache = getResolvedSelectedIndex(context, node);
      context.setLocalProperty(_SELECTED_CHILD_INDEX_KEY,
        resolvedSelectedIndexFromCache);
    }
    return resolvedSelectedIndexFromCache;
  }

  /**
   *
   * determine the selected child index
   *
   */

  public int getResolvedSelectedIndex(
    UIXRenderingContext context,
    UINode node )
  {
    //
    // First check all of the children for the selected index.
    //
    int selectedIndex  = NO_CHILD_INDEX;
    int currChildIndex = NO_CHILD_INDEX;
    do
    {
      currChildIndex = getNextRenderedChildIndex(context,
                                                 node,
                                                 currChildIndex);

      if (currChildIndex != NO_CHILD_INDEX)
      {
        UINode currChildNode = node.getIndexedChild(context, currChildIndex);

        if (Boolean.TRUE.equals(
                   currChildNode.getAttributeValue(context,
                                                   UIConstants.SELECTED_ATTR)))
        {
          selectedIndex = currChildIndex;
          break;
        }
      }
      else
      {
        break;
      }
    } while (true);


    // try getting the selected index from the parent
    if (selectedIndex == NO_CHILD_INDEX)
    {
      Integer selected = (Integer)node.getAttributeValue(
                                               context,
                                               UIConstants.SELECTED_INDEX_ATTR);

      if (selected != null)
      {
        selectedIndex = selected.intValue();
      }
    }

    return selectedIndex;
  }



  /**
   * Returns the index of the access key in the specified node's text.
   */
  protected static int getAccessKeyIndex(
    UIXRenderingContext context,
    UINode           node,
    Object           textValue
    )
  {
    int keyIndex = -1;

    if (textValue != null)
    {
      char accessChar = XhtmlLafUtils.getCharacterAttr(context,
                                                       node,
                                                       ACCESS_KEY_ATTR);
      if (accessChar != XhtmlLafUtils.CHAR_UNDEFINED)
      {
        String textString = textValue.toString();

        // underline the first instance of the access key in the text
        keyIndex = textString.indexOf(accessChar);

        // try the key in the opposite case if there was no match
        if (keyIndex == -1)
        {
          char oppositeChar = Character.toLowerCase(accessChar);

          if (oppositeChar == accessChar)
          {
            oppositeChar = Character.toUpperCase(accessChar);
          }

          if (oppositeChar != accessChar)
          {
            keyIndex = textString.indexOf(oppositeChar);
          }
        }
      }
    }
    return keyIndex;
  }


  protected static void renderLayoutTableAttributes(
    UIXRenderingContext context,
    Object           cellspacing,
    Object           tableWidth
    ) throws IOException
  {
    renderLayoutTableAttributes(context, "0", cellspacing, tableWidth);
  }


  protected static void renderLayoutTableAttributes(
    UIXRenderingContext context,
    Object           cellpadding,
    Object           cellspacing,
    Object           tableWidth
    ) throws IOException
  {
    renderLayoutTableAttributes(context, cellpadding, cellspacing, "0",
                                tableWidth);
  }

  /**
   * All layout tables should call this method, so that a special summary tag
   * (which silences OAC) is rendered
   */
  protected static void renderLayoutTableAttributes(
    UIXRenderingContext context,
    Object           cellpadding,
    Object           cellspacing,
    Object           border,
    Object           tableWidth
    ) throws IOException
  {
    renderLayoutTableAttributes(context, cellpadding, cellspacing, border,
                                tableWidth, "" /* summary */ );
  }

  /**
   * all data tables should call this one, so that a summary tag is written
   * out
   */
  protected static void renderLayoutTableAttributes(
    UIXRenderingContext context,
    Object           cellpadding,
    Object           cellspacing,
    Object           border,
    Object           tableWidth,
    Object           summary
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.writeAttribute("cellpadding", cellpadding, null);
    writer.writeAttribute("cellspacing", cellspacing, null);
    writer.writeAttribute("border", border, null);
    writer.writeAttribute("width", tableWidth, null);

    if (!isInaccessibleMode(context))
    {
      writer.writeAttribute("summary", summary, null);
    }
  }

  protected static void renderLayoutTableHeader(
    UIXRenderingContext context,
    Object           cellspacing,
    Object           tableWidth
    ) throws IOException
  {
    context.getResponseWriter().startElement("table", null);
    renderLayoutTableAttributes(context, cellspacing, tableWidth);
  }

  /**
  * Renders only the alt attribute
  * if that can be used as a tooltip on an image.
  * Otherwise it renders both the alt and the title attributes.
  */
  public static void renderAltAndTooltipForImage(
     UIXRenderingContext context,
     Object           textValue
     ) throws IOException
  {
    if (textValue == null)
      return;

    ResponseWriter writer = context.getResponseWriter();
    boolean wroteTitle = false;

    if (!supportsAltRendersTooltipOnImage(context))
    {
      if (!"".equals(textValue))
      {
        writer.writeAttribute("title", textValue, null);
        wroteTitle = true;
      }
    }

    // only write out both title and alt if
    // we really need both
    if (!wroteTitle || !isInaccessibleMode(context))
    {
      writer.writeAttribute(ALT_ATTRIBUTE, textValue, null);
    }
  }

  protected boolean isTextFormatted(Object textValue)
  {
    if (textValue == null)
      return false;

    // =-=AEW Ugh.  Could we avoid "toString()"'ing.  OTOH,
    // this shouldn't be a big performance issue, since
    // if it's
    String string = textValue.toString();
    // =-=AEW Do we support "<HTML>" (caps)?
    return string.startsWith("<html>");
  }

  final protected void renderPossiblyFormattedText(
    UIXRenderingContext context,
    Object           textValue
    ) throws IOException
  {
    if (textValue != null)
    {
      if (isTextFormatted(textValue))
        renderFormattedText(context, textValue);
      else
        context.getResponseWriter().writeText(textValue, null);
    }
  }

  final protected void renderFormattedText(
    UIXRenderingContext context,
    Object           textValue) throws IOException
  {
    if (textValue != null)
    {
      String text = textValue.toString();
      getFormattedTextParser(context).writeFormattedText(
        context.getFacesContext(), text);
    }
  }

  /**
   * Starts a block wrapper to enable screen readers identify skippable
   * related link groups. Also renders the 'hidden' skip link as the first
   * element in the group.
   * @param blockTitleKey The resource key for this blocks title
   */
  protected final void renderRelatedLinksBlockStart(
    UIXRenderingContext context,
    String blockTitleKey
    ) throws IOException
  {
    if (isScreenReaderMode(context))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("map", null);
      String blockTitleText = getTranslatedString(context, blockTitleKey);
      writer.writeAttribute("title", blockTitleText, null);
      String navBlockEndAnchorName = XhtmlLafUtils.generateUniqueID(context);
      context.setLocalProperty(_NAV_BLOCK_END_ANCHOR_NAME_KEY,
                                navBlockEndAnchorName);
      String pattern = getTranslatedString(context,
                                           "SKIP_CURRENT_NAVIGATION_BLOCK");
      String skipLinkAltText = formatString(context,
                                            pattern,
                                            new String[]{blockTitleText});
      _renderSkipNavigationLink(context,
                                navBlockEndAnchorName,
                                skipLinkAltText);
    }
  }

  /**
   * Ends a block wrapper to enable screen readers identify skippable
   * related link groups. Also renders the anchor to where the jump should
   * happen.
   */
  protected final void renderRelatedLinksBlockEnd(
    UIXRenderingContext context
    ) throws IOException
  {
    if (isScreenReaderMode(context))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.endElement("map");
      String navBlockEndAnchorName = (String)context.getLocalProperty(0,
                                      _NAV_BLOCK_END_ANCHOR_NAME_KEY, null);
      MarlinBean navBlockEndAnchor = new MarlinBean(UIConstants.LINK_NAME);
      navBlockEndAnchor.setAttributeValue(UIConstants.NAME_ATTR, navBlockEndAnchorName);
      navBlockEndAnchor.render(context);
    }
  }

  protected FormattedTextParser getFormattedTextParser(
    UIXRenderingContext context)
  {
    return XhtmlFormattedText.getFormattedTextParser();
  }


  /**
   * Renders the node text using the relevant style information.
   */
  final protected void renderStyledText(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderStyledText(context, node, false, true);
  }

  /**
   * Renders the node text using the relevant style information.
   */
  protected void renderStyledText(
    UIXRenderingContext context,
    UINode           node,
    boolean          renderAccessKeys,
    boolean          renderID
    ) throws IOException
  {
    Object textValue = getText(context, node);

    if (textValue != null)
    {
      ResponseWriter writer = context.getResponseWriter();

      //
      // Determine the style class to use, and default the style depending
      // depending on whether we are disabled, or not.
      //
      Object styleClass = getStyleClass(context, node);

      // get the inline style to use
      Object inlineStyle = getInlineStyle(context, node);


      boolean hasElementStyle   = false;

      UIComponent component = NodeUtils.getUIComponent(context, node);

      writer.startElement(SPAN_ELEMENT, component);
      if (renderID)
        renderID( context, node);

      // write the style information
      if ((styleClass != null) || (inlineStyle != null))
      {
        if (supportsStyleAttributes(context))
        {
          if (styleClass != null)
          {
            renderStyleClassAttribute(context, styleClass);
          }

          if (inlineStyle != null)
          {
            renderInlineStyleAttribute(context, inlineStyle);
          }
        }
        else if (supportsTextPresentation(context))
        {
          hasElementStyle = true;

          startRenderingStyleElements(context, null/*=-=AEWinlineStyle*/, styleClass);
        }
      }

      if (renderAccessKeys && supportsAccessKeys(context))
      {
        // hightlight any access keys with an underline
        renderAccessKeyText(context, node, textValue, 
                            SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
      }
      else
      {
        // output the text directly since we have no access key
        writer.writeText(textValue, null);
      }

      // close any element styles
      if (hasElementStyle)
      {
        XhtmlLafUtils.endRenderingStyleElements(context);
      }

      writer.endElement(SPAN_ELEMENT);
    }
  }

  @SuppressWarnings("unchecked")
  protected void addOnSubmitConverterValidators(
    UIXRenderingContext context,
    UINode           node,
    String           requiredMessageKey
  )throws IOException
  {

   // Bug 2748146: Don't do validation of a disabled field! If the field is
    // disabled, the user can't have updated it (there is one way for the
    // client to hurt themselves here: by changing the disabled state as part
    // of a PPR update after the user has updated the field).
    Object disabled = node.getAttributeValue(context, DISABLED_ATTR);

    if (! Boolean.TRUE.equals(disabled))
    {
      boolean requiredField = Boolean.TRUE.equals(
                           node.getAttributeValue(context, REQUIRED_ATTR));
      Converter converter = (Converter)node.getAttributeValue(context,
                                                  UIConstants.CONVERTER_ATTR);
      Iterator<Validator> validators = 
        (Iterator<Validator>) node.getAttributeValue(context,
                                                     UIConstants.VALIDATORS_ATTR);
      if (requiredField ||
          (converter != null) ||
          ((validators != null) && validators.hasNext()))
      {
        Object nodeName = getNodeName(context, node);
        UIComponent component = NodeUtils.getUIComponent(context, node);

        if (component == null)
        {
          _LOG.warning("NULL_COMPONENT_FOR_NODE", node.getLocalName());
        }

        boolean unvalidated =
                     Boolean.TRUE.equals(node.getAttributeValue(context,
                                                  UIConstants.UNVALIDATED_ATTR));

        FormRenderer.addOnSubmitConverterValidators(component,
                                                    converter,
                                                    validators,
                                                    nodeName.toString(),
                                                    unvalidated,
                                                    requiredField,
                                                    requiredMessageKey);

      }
    }
  }

  protected void addOnSubmitRequiredValidator(
    UIXRenderingContext context,
    UINode           node,
    String           requiredMessageKey
  )throws IOException
  {

    Object nodeName = getNodeName(context, node);
    XhtmlLafUtils.addOnSubmitRequiredValidator(context, node,
                                               requiredMessageKey, nodeName);

  }


  // Don't "toString()" Integers - use our cache
  static private String _toString(Object o)
  {
    if (o == null)
      return null;
    if (o.getClass() == Integer.class)
      return IntegerUtils.getString(((Integer) o).intValue());
    return o.toString();
  }

  /**
   * Returns an image from the ImageProvider
   */
  public static String getFlippedIconURI(
    UIXRenderingContext context,
    String           source
    )
  {
    ImageProviderResponse response = getFlippedIcon(context, source);

    if (response != null)
    {
      return LafIconProvider.getCacheImageURI(context) + response.getImageURI();
    }
    else
    {
      return null;
    }
  }

 /**
  * Get the URI of an image that might need to be flipped
  */
  protected Object getFlippableURI(
    UIXRenderingContext context,
    UINode           node,
    AttributeKey    attrKey
    )
  {
    // Get the original URI
    Object uri = node.getAttributeValue(context, attrKey);

    // if going right-to-left and autoflip turned on
    if ( isRightToLeft(context) &&
         Boolean.TRUE.equals(node.getAttributeValue( context, AUTOFLIP_ATTR)))
    {
      if ( uri != null )
      {
        // get the uri of the flipped image
        Object flippeduri = getFlippedIconURI(context, uri.toString() );

        // if uri of flipped image is null that means original
        // image couldn't be found to be flipped, so just
        // return original uri
        if ( flippeduri != null )
          uri = flippeduri;
      }
    }

    return uri;
  }

  /**
   * Returns an image from the ImageProvider
   */
  public static ImageProviderResponse getFlippedIcon(
    UIXRenderingContext context,
    String           sourceURI
    )
  {
    ImageProvider provider = (ImageProvider)
      context.getProperty(ImageConstants.TECATE_NAMESPACE,
                          ImageConstants.IMAGE_PROVIDER_PROPERTY);

    if (provider == null)
    {
      return null;
    }

    // Get the context and request objects
    ImageContext imageContext = context.getImageContext();
    ImageProviderRequest request = new FlippedIconRequest(
                                  context,
                                  sourceURI);

    // Make the request
    ImageProviderResponse response = provider.getImage(imageContext, request);

    // Log any problems
    if (response == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("CANNOT_FLIP_ICON", sourceURI);
    }

    return response;
  }

  // Returns the style class, first looking for a local style class
  // property.
  private Object _getLocalStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object prop = context.getLocalProperty(0, _STYLE_CLASS_KEY, null);

    // If the local property was explicitly set to _DOES_NOT_EXIST,
    // that means that the STYLE_CLASS_ATTR is not set on the UINode.
    if (prop == _DOES_NOT_EXIST)
      return null;

    return (prop == null) ? getStyleClass(context, node) : prop;
  }

  // Returns the inline style, first looking for a local inline style
  // property.
  /* =-=AEW UNUSED
  private Object _getLocalInlineStyle(
    RenderingContext context,
    UINode           node
    )
  {
    Object prop = context.getLocalProperty(0, _INLINE_STYLE_KEY, null);

    // If the local property was explicitly set to _DOES_NOT_EXIST,
    // that means that the INLINE_STYLE_ATTR is not set on the UINode.
    if (prop == _DOES_NOT_EXIST)
      return null;

    return (prop == null) ? getInlineStyle(context, node) : prop;
  }
  */

  /**
   * Renders a link which could be used to skip to an anchor ahead in the page.
   * @param skipNavigationAnchor The destination anchor name
   * @param altText Translated alt text value (ScreenReaders' speak this out)
   */
  private static void _renderSkipNavigationLink(
    UIXRenderingContext context,
    String skipNavigationAnchor,
    String altText
    ) throws IOException
  {
    MarlinBean skipLink = new MarlinBean(UIConstants.IMAGE_NAME);
    skipLink.setAttributeValue(UIConstants.DESTINATION_ATTR, "#"+skipNavigationAnchor);
    skipLink.setAttributeValue(UIConstants.SHORT_DESC_ATTR,
                               altText);
    skipLink.setAttributeValue(UIConstants.SOURCE_ATTR, new IconURIBoundValue(TRANSPARENT_GIF));
    //--pu-- Hack: Use the existing label hiding class for hiding images as well
    skipLink.setStyleClass(XhtmlLafConstants.HIDDEN_LABEL_STYLE_CLASS);
    skipLink.render(context);
  }

  private static final class Counter
  {
    public int count = 0;
  }

  // Rendering context key for transparent gif
  // This MUST REMAIN THE SAME VALUE as
  //                 XhtmlRenderer._TRANSPARENT_FUNCTION_WRITTEN_KEY
  static final String __TRANSPARENT_URL_KEY = "_t.gif";

  // This property is used to control whether or not scripts should
  // be deferred.  We only defer scripts if we are rendering a
  // partial page response (or full page response in response to
  // a partial page request).  We could just check for a PartialPageContext,
  // but we actually need to know whether or not we are currently buffering
  // scripts.  We don't want to defer scripts which are rendered before
  // the ScriptBufferingResponseWriter kicks in - such as Core.js -
  // since IE has problems with deferred scripts.  This property is
  // set to Boolean.TRUE by BodyRenderer when rendering the contents
  // of the body.
  static final String __DEFER_SCRIPTS_KEY = "_defer";

  // object indicating that the value does not exist
  private static final Object _DOES_NOT_EXIST = new Object();

  // Key to look up the number of times we have rendered the script spacer:
  private static final Object _SCRIPT_SPACER_COUNT = new Object();

  // Key to look up style class as local property
  private static final Object _STYLE_CLASS_KEY = new Object();

  // Key to look up inline style as local property
  private static final Object _INLINE_STYLE_KEY = new Object();

  // Key to look up the anchor outside of the current navigation block
  private static final Object _NAV_BLOCK_END_ANCHOR_NAME_KEY = new Object();

  // Key to lookup the selected child index
  private static final Object _SELECTED_CHILD_INDEX_KEY = new Object();

  // The value bound to the type attribute in script tags in accessibilty mode.
  private static final String _ACCESSIBILITY_SCRIPT_TYPE = "text/javascript";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(XhtmlLafRenderer.class);
}
