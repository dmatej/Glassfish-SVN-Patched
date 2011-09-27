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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.awt.Color;
import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.image.ImageContext;
import org.apache.myfaces.trinidadinternal.image.ImageProvider;
import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;
import org.apache.myfaces.trinidadinternal.image.ImageProviderResponse;
import org.apache.myfaces.trinidadinternal.image.cache.CompositeButtonKey;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.style.CoreStyle;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;
import org.apache.myfaces.trinidadinternal.style.util.FontProxy;
import org.apache.myfaces.trinidadinternal.style.util.MutableFontProxy;
import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.ui.action.FireAction;


/**
 * Renderer for button nodes.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/oracle/desktop/ButtonRenderer.java#1 $) $Date: 11-nov-2005.14:59:41 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class ButtonRenderer extends GeneratedImageRenderer
{
  /**
   * Override of BaseRenderer.prerender().  Note that ButtonRenderer
   * performs the actual button content generation in
   * <code>renderImageContent(RenderingContext,
   *         UINode,
   *         ImageProviderResponse)
   * </code>
   * Subclasses should override <code>renderImageContent()</code>instead of
   * prerender() if additional prerendering is needed.
   * @param context The rendering context
   * @param node the node to be rendered
   * @throws IOException
   * @see #renderContent(UIXRenderingContext,UINode)
   * @see #renderImageContent(UIXRenderingContext,UINode,ImageProviderResponse)
   */
  @Override
  protected final void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // Don't bother with prerendering - it's all done in renderContent()
    // Note: we explicitly avoid calling super.prerender(), since we
    // want prerender() to be a no-op.
  }

  /**
   * No post rendering action required all action is done in
   * <code>renderContent(RenderingContext,UINode)</code>
   * @param context The rendering context
   * @param node the node to be rendered
   * @throws IOException
   */
  @Override
  protected final void postrender(
    UIXRenderingContext context,
    UINode node
    ) throws IOException
  {
    // Don't bother with postrendering - it's all done in renderContent()
    // Note: we explicitly avoid calling super.postrender(), since we
    // want postrender() to be a no-op.
  }

  /**
   * Override of BaseRender.renderContent().  renderContent() attempts
   * to generate the button image, and then delegates to one of two
   * methods: <code>renderImageContent()</code> is called if image
   * generation succeeds. <code> Otherwise renderAltContent()</code> is called.
   * @param context The rendering context
   * @param node the node to be rendered
   * @throws IOException
   */
  @Override
  protected final void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    ImageProviderResponse response = _getImage(context, node);

    if (response != null)
      renderImageContent(context, node ,response);
    else
      renderAltContent(context, node);
  }

  /**
   * Tests whether the button should be rendered as an image.
   * If true, the button will be rendered as an image.  If
   * false, the button will be rendered using alternate content
   */
  protected boolean doRenderImageContent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // We render the button as an image as long as we
    // aren't running in screen reader mode.
    return !(isScreenReaderMode(context));
  }

  /**
   * Renders the button as an image using the image specified by
   * the ImageProviderResponse.  This method is called by
   * <code>renderContent()</code> when image generation succeeds.  Otherwise,
   * <code>renderAltContent()</code> is called.
   * @param context The rendering context
   * @param node the node to be rendered
   * @param response ImageProviderResponse which descibes the button
   *  image to render
   * @throws IOException
   */
  protected void renderImageContent(
    UIXRenderingContext context,
    UINode node,
    ImageProviderResponse response
    ) throws IOException
  {
    // If we've got a ClientAction, let it write its dependencies
    // before we start rendering the link
    _writeClientActionDependency(context, node);

    renderImage(context, node, response, null);

    // render onkeydown if adfbtn attribute has been rendered.
    // this means that the accessKey has been rendered in IE
    _renderOnKeyDownScript(context);
  }

  /**
   * Override of BaseRenderer.render().
   * Subclasses that need to customize button rendering should
   * override <code>renderImageContent()</code> or
   * <code>renderAltContent()</code>.
   * @param context The rendering context
   * @param node the node to be rendered
   * image to render
   * @throws IOException
   */
  @Override
  public final void render(UIXRenderingContext context, UINode node)
    throws IOException
  {
    // We override render() so that we can bypass the normal rendering
    // mechanism when we are running in screen reader mode.  We don't
    // want to render image content in screen reader mode, and instead
    // we delegate to the laf.base.xhtml.ButtonRenderer to render a
    // standard HTML button in this mode.
    //
    // Note: we do this check in render() instead of renderContent(),
    // because we want to be able to short-circuit to the alternate Renderer
    // *before* any prerendering is performed.
    //
    // And also note: because we don't really use renderContent(), prerender()
    // and postrender() they way that they are meant to be used, we probably
    // should even extend BaseRenderer at all.  But we want to extend
    // BlafRenderer so that we can pick up its convenience functions,
    // so we end up inheriting these methods anyway.

    // In "printable" facet, do not render buttons at all (bug 3327791)
    if (supportsNavigation(context))
    {
      if (doRenderImageContent(context, node))
      {
        super.render(context, node);
      }
      else
      {
        renderAltContent(context, node);
      }
    }
  }


  /**
   * Renders alternate content in the event that the button can not
   * or should not be rendered as an image.  This is called either if
   * image generation fails, or if we are running in screen reader mode.
   * @param context The rendering context
   * @param node the node to be rendered
   * @throws IOException
   */
  protected void renderAltContent(
    UIXRenderingContext context,
    UINode node
    ) throws IOException
  {
    _getAlternateRenderer(context).render(context, node);
  }

  /**
   * Returns the alternate Renderer for button.
   * @return xhtml ButtonRenderer
   */
    protected Renderer getAltRenderer()
    {
      return _ALTERNATE_RENDERER;
    }
    
  @Override
  protected Object getOnClick(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // We build up the actual onclick handler by chaining the value the
    // ONCLICK_ATTR with the script of the PRIMARY_CLIENT_ACTION_ATTR.
    // Since we get the onclick handler multiple times (getDestination(),
    // which is called multiple times, and renderEventHandlers()), and
    // since building up the handler can be expensive, we store the
    // script in a local property, so that this work does not need to
    // be repeated.
    Object prop = context.getLocalProperty(0,
                                           _LOCAL_ON_CLICK_KEY,
                                           _ON_CLICK_NONE);
    if (prop != _ON_CLICK_NONE)
      return prop;

    Object onClick = super.getOnClick(context, node);
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    String actionScript = null;

    // TODO: This is a temporary way of setting blocking. Once the
    // AutoSubmitUtils makes it into the main branch, dump this and pass a
    // parameter to getSubmitScript(), which will tell it to look for and turn
    // on blocking. Similar code in LinkRenderer also has to change.
    if (action instanceof FireAction)
    {
      UIComponent component;
      component = node.getUIComponent();
      if ((component != null)
          && Boolean.TRUE.equals(component.getAttributes().get("blocking")))
        ((FireAction) action).setBlocking(true);
    }

    if (action != null)
      actionScript = action.getScript(context, node, Boolean.FALSE);

    Object chainedScript = BaseDesktopUtils.getChainedJS(onClick,
                                                      actionScript,
                                                      true);

    // Store away the script for next time
    context.setLocalProperty(_LOCAL_ON_CLICK_KEY, chainedScript);
    return chainedScript;
  }

  @Override
  protected Object getText(
    UIXRenderingContext context,
    UINode node
    )
  {
    Object text = node.getAttributeValue(context, TEXT_ATTR);

    // If the text is null, create a button with an empty label
    if (text == null)
      return _EMPTY_LABEL;

    return text;
  }

  /**
   * Override to provide defaults
   */
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Buttons should not render the style class - style info
    // is passed directly to Tecate.
    return null;
  }

  // Returns the vertical alignment
  @Override
  protected Object getVAlign(UIXRenderingContext context, UINode node)
  {
    // This is kind of a hack to address bug #2047577
    // Instead of adding an attribute to control placement of the button,
    // we just force it to middle (which is what everybody wants anyway).
    // Mozilla renders "absmiddle" the same as middle
    // IE renders "absmiddle" better than middle.
    // For some reason, the default is "bottom".
    // Bug #2268480
    // Netscape screws up absmiddle in some cases, it usually does better
    // with just plain middle.
    // Bug #2471515
    // Mozilla likes Top alignment best, but middle is almost as good and
    // doesn't cause problems in table cells.
    // Bug #3426092
    // In screen reader mode, "middle" instead of "absmiddle" for vAlign to
    // comply with 4.01 HTML Spec.

    TrinidadAgent.Application application = context.getAgent().getAgentApplication();
    Object align;

    if ((application == TrinidadAgent.Application.NETSCAPE)
        || (application == TrinidadAgent.Application.GECKO)
        || (isScreenReaderMode(context)))
    {
      align = UIConstants.V_ALIGN_MIDDLE;
    }
    else
    {
      // default to absMiddle
      align  = UIConstants.V_ALIGN_ABSMIDDLE;
    }
    return align;
  }

  @Override
  protected void renderButtonAccessKey(
    UIXRenderingContext context,
    UINode           node
    )    throws IOException
  {
    Object accessKey = node.getAttributeValue(context, ACCESS_KEY_ATTR);
    if (accessKey != null)
    {
      renderAttribute(context, "accesskey", accessKey);
      // for IE, but not on a Mac, we want the accesskey to work on these
      // buttons like it does for a <button> html element, and that is
      // it activates the button rather than set focus like it does for a link.
      // We have javascript that handles this, but the javascript code must
      // know that the element is our uix button. So we set a flag attribute
      // for this purpose, called "adfbtn". We also set the rendering property
      // so that in renderImageContent we know that we rendered this special
      // attribute and then we will register the onkeydown event handler.
      if ((isIE(context)) &&
          !(context.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS ))
      {
        context.getResponseWriter().writeAttribute("adfbtn", "t", null);
        setRenderingProperty(context, _ADF_BTN_ATTR, Boolean.TRUE);
      }
    }
  }

  /**
   * Creates the ImageProviderRequest to use when looking up the
   * button image.
   */
  abstract protected ImageProviderRequest createImageProviderRequest(
    UIXRenderingContext context,
    Object       name,
    Object       text,
    Color        foreground,
    Color        background,
    Color        surroundingColor,
    FontProxy    font,
    boolean      disabled,
    boolean      textAntialias,
    boolean      startRounded,
    boolean      endRounded,
    char         accessKey
    );

  /**
   * Returns the name of the server-side style for styling
   * button text.
   */
  protected String getServerStyleName(
    UIXRenderingContext context,
    UINode           node,
    boolean          disabled
    )
  {
    return disabled ? _SERVER_DISABLED_STYLE_NAME : _SERVER_STYLE_NAME;
  }

  // This method collects all of the UINode attributes which affect
  // image generation and makes a request to the ImageProvider to
  // generate the corresponding image.
  private ImageProviderResponse _getImage(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // First, make sure we have an ImagePRovider
    ImageProvider provider = (ImageProvider)
      context.getProperty(TECATE_NAMESPACE, IMAGE_PROVIDER_PROPERTY);
    if (provider == null)
      return null;

    // Round up all of our properties
    boolean disabled = isDisabled(context, node);
    Object text = getText(context, node);
    boolean startRounded =
      getBooleanAttributeValue(context, node, _START_ROUNDED_ATTR, true);
    boolean endRounded =
      getBooleanAttributeValue(context, node, _END_ROUNDED_ATTR, true);
    Object name = getImageName(context, node);
    String styleName = getServerStyleName(context, node, disabled);
    CoreStyle style = getStyle(context, node, styleName);

    CoreStyle inlineStyle;
    Object inlineStyleObj = getInlineStyle(context, node);
    if (inlineStyleObj == null)
      inlineStyle = null;
    else
      inlineStyle = CSSUtils.parseStyle(inlineStyleObj.toString());

    Color background =
      getBackground(context, node, style, inlineStyle, styleName);
    Color foreground =
      getForeground(context, node, style, inlineStyle, styleName);
    Color surroundingColor = getSurroundingColor(context);
    char realAccessKey = _getAccessKey(context, node);

    char accessKey = supportsAccessKeys(context)
                       ? realAccessKey
                 : CompositeButtonKey.ACCESS_KEY_UNDEFINED;

    boolean textAntialias =
      isTextAntialiased(context, node, style, inlineStyle);
    int fontStyle =
      getFontStyle(context, node, style, inlineStyle, styleName);
    int fontSize =
      getFontSize(context, node, style, inlineStyle, styleName);
    Collection<Object> fontFamilies =
      getFontFamilies(context, node, style, styleName);
    Collection<Object> inlineFontFamilies =
      getFontFamilies(context, node, inlineStyle, null);
    MutableFontProxy font = new MutableFontProxy(null, fontStyle, fontSize);

    // Now that we've got all the properties, create an ImageProviderRequest
    // with the properties.
    ImageProviderRequest request = createImageProviderRequest(context,
                                                              name,
                                                              text,
                                                              foreground,
                                                              background,
                                                              surroundingColor,
                                                              font,
                                                              disabled,
                                                              textAntialias,
                                                              startRounded,
                                                              endRounded,
                                                              accessKey);
    // Do the look up
    return _getImage(context,
                     provider,
                     request,
                     font,
                     fontFamilies,
                     inlineFontFamilies);
  }

  // This overload of _getImage() loops through all inline font families
  // and style class font families trying to find a match.
  private ImageProviderResponse _getImage(
    UIXRenderingContext context,
    ImageProvider    provider,
    ImageProviderRequest request,
    MutableFontProxy font,
    Collection<Object> fontFamilies,
    Collection<Object> inlineFontFamilies
    )
  {
    ImageContext imageContext = context.getImageContext();
    ImageProviderResponse response = null;

    // First, loop through inline font families, if we've got any
    if (inlineFontFamilies != null)
    {
      Iterator<Object> inlineFontFamiliesIterator = inlineFontFamilies.iterator();
      while(inlineFontFamiliesIterator.hasNext())
      {
        String family = (String)inlineFontFamiliesIterator.next();
        font.setName(family);

        response = provider.getImage(imageContext, request);

        if (response != null)
          return response;
      }
    }

    // Next, try the font families defined by the style class
    if (fontFamilies != null)
    {
      Iterator<Object> fontFamiliesIterator = fontFamilies.iterator();
      while(fontFamiliesIterator.hasNext())
      {
        String family = (String)fontFamiliesIterator.next();
        font.setName(family);

        response = provider.getImage(imageContext, request);

        if (response != null)
          return response;
      }
    }

    // Finally, we match any font family by specifying a null font name
    font.setName(null);
    return provider.getImage(imageContext, request);
  }

  private char _getAccessKey(UIXRenderingContext context, UINode node)
  {
    char ch = BaseDesktopUtils.getCharacterAttr(context, node, ACCESS_KEY_ATTR);

    // In practice, these are the same, but let's be clean...
    if (ch == BaseDesktopUtils.CHAR_UNDEFINED)
      return CompositeButtonKey.ACCESS_KEY_UNDEFINED;

    return ch;
  }

  /**
    * If we've got a ClientAction, let it write its dependencies
    * This is normally done before rendering the link
    * @param context The rendering context
    * @param node the node to be rendered
    * @throws IOException
    */
  private static void _writeClientActionDependency(
    UIXRenderingContext context,
    UINode node
    ) throws IOException
  {
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if (action != null)
    {
      action.writeDependencies(context, node);
    }
  }

  /**
   * Render onKeydown if adfbtn attribute has been rendered. This
   * is means accessKey has been rendered in IE
   * @param context the rendering context
   * @throws IOException
   */
  private  static void _renderOnKeyDownScript(
    UIXRenderingContext context
    ) throws IOException
  {
    if (Boolean.TRUE.equals(getRenderingProperty(context, _ADF_BTN_ATTR)))
      {
        renderScriptOnce(context,
                         "document.onkeydown=_monitor;",
                         _ON_KEY_DOWN_RENDERED);
      }
  }

  private boolean _useLinkAlternateContent(UIXRenderingContext context)
  {
    return  (!supportsAdvancedButtons(context) &&
      (getParentFormName(context) == null));
  }


  /**
   *  Returns the Renderer to use to render the alternate content.
   */
  private Renderer _getAlternateRenderer (UIXRenderingContext context)
  {
    // We need to use a special Renderer for Netscape when there
    // is no form, but other than that we delegate to a
    // laf.base.xhtml.ButtonRenderer instance.
    if (_useLinkAlternateContent(context))
    {
      return _LINK_ALTERNATE_RENDERER;
    }
    else
    {
      return  getAltRenderer();
    }
  }

  // Fallback Renderer that is used for Netscape when the button is
  // not contained within a form.  We use this instead of the standard
  // fallback, since Netscape does not support <input type="button">
  // outside of a form.
  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  private class AlternateRenderer implements Renderer
  {
   /**
    * Render as links
    * @param context The rendering context
    * @param node the node to be rendered
    * @throws IOException
    */
    public void render(
      UIXRenderingContext context,
      UINode node
      ) throws IOException
    {

        // If we've got a ClientAction, let it write its dependencies
        // before we start rendering the link
        _writeClientActionDependency(context,node);

        Object text        = getText(context, node);
        Object destination = getDestination(context, node);
        boolean disabled   = isDisabled(context, node);
        boolean isLink     = ((destination != null) && !disabled);
        String element     = (isLink) ? "a" : "span";

        ResponseWriter writer = context.getResponseWriter();
        writer.startElement(element, node.getUIComponent());
        renderAttributes(context, node);

        if (isLink)
        {
           renderEncodedActionURI(context, "href", destination);
           renderAttribute(context, node, "target", TARGET_FRAME_ATTR);

          // Don't render access key on Netscape... Netscape doesn't
          // support access keys - if this ever changes, it would
          // be confusing if we rendered the accessKey attr without
          // also underlining the access key in the button text.
          if (supportsAccessKeys(context))
          {
             renderButtonAccessKey(context, node);
          }
          renderAccessKeyText(context, node, text, 
                              SkinSelectors.AF_LINKACCESSKEY_STYLE_CLASS);
        }
        else
        {
          renderText(context, node);
        }
        writer.endElement(element);
      }
  }

  // =-=ags These should be public and on ImageConstants once we decide on
  //        the correct API.
  private static final AttributeKey _START_ROUNDED_ATTR =
                  getAttributeKey("_startRounded");
  private static final AttributeKey _END_ROUNDED_ATTR =
                  getAttributeKey("_endRounded");

  private static final String _EMPTY_LABEL = "";

  // Server style class names
  private static final String _SERVER_STYLE_NAME =
    "BLAFServerButtonText";
  private static final String _SERVER_DISABLED_STYLE_NAME =
    "BLAFServerButtonTextDisabled";

  // object indicating that there is no onClick handler
  private static final Object _ON_CLICK_NONE = new Object();

  // object used to store the local copy of the onClick handler
  private static final Object _LOCAL_ON_CLICK_KEY = new Object();

  private static final Object _ON_KEY_DOWN_RENDERED = new Object();
  private static final Object _ADF_BTN_ATTR = new Object();

  // Alternate renderer in screen reader mode
  private static final Renderer _ALTERNATE_RENDERER =
    new org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ButtonRenderer();

  // since the class AlternateRenderer is not static and we use the methods
  // of the enclosing class - this class is not static in nature.
  private   final Renderer _LINK_ALTERNATE_RENDERER =
    new AlternateRenderer();

}



