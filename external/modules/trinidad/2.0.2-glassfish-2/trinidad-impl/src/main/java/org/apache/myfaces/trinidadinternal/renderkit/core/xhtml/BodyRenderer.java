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

import java.beans.Beans;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.html.HtmlBody;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.util.Service;


/**
 * Renderer for the panelPartialRoot.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/BodyRenderer.java#1 $) $Date: 11-nov-2005.14:59:41 $
 */
public class BodyRenderer extends PanelPartialRootRenderer
{
  public BodyRenderer()
  {
    this(HtmlBody.TYPE);
  }

  protected BodyRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _firstClickPassedKey = type.findKey("firstClickPassed");
    _initialFocusIdKey = type.findKey("initialFocusId");
    _onloadKey = type.findKey("onload");
    _onunloadKey = type.findKey("onunload");
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("body", component);
    renderId(context, component);
    renderAllAttributes(context, rc, component, bean);
    super.encodeAll(context, rc, component, bean);

    // Output a version comment at the bottom of the body
    _writeVersionInformation(context, rc);
  }

  @Override
  protected void renderAtEnd(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    _encodeServiceScripts(context);

    // trigger the rendering of targeted resource
    // for the BODY, on UIViewRoot - if there are
    // any...
    encodeComponentResources(context, "body");
    context.getResponseWriter().endElement("body");

    _renderInitialFocusScript(context, rc);
    
    _renderDisableJsfAjaxScript(context, rc);
  }

  @Override
  protected void renderPPRSupport(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    super.renderPPRSupport(context, rc, component, bean);
    if (getFirstClickPassed(component, bean))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", null);
      renderScriptDeferAttribute(context, rc);
      renderScriptTypeAttribute(context, rc);
      writer.writeText("var _pprFirstClickPass=true;", null);
      writer.endElement("script");
    }
  }

  @Override
  protected void renderContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    boolean isPartialPass = PartialPageUtils.isPartialRenderingPass(rc);

    _renderAnchorForTop(context);

    // Since we are supporting Non-JavaScript browsers of generic PDA,
    // we don't need to prompt any message in the client side regarding
    // the JavaScript capability of generic PDA browsers.

    if (supportsScripting(rc))
    {
      _renderNoScript(context, rc);
      _storeInitialFocus(rc, component, bean);
    }

    if (!isPartialPass)
    {
      // start the span here, and end it in postrender
      _renderPartialBackSupportSpan(context, rc, true);
    }

    super.renderContent(context, rc, component, bean);

    if (!isPartialPass)
    {
      // end the span for PPR Back button support (and render the hidden fields)
      _renderPartialBackSupportSpan(context, rc, false);
    }
  }

  @Override
  protected void renderEventHandlers(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean
    ) throws IOException
  {
    super.renderEventHandlers(context, component, bean);
    RenderingContext rc = RenderingContext.getCurrentInstance();
    ResponseWriter rw = context.getResponseWriter();

    if (PartialPageUtils.isPartialRenderingPass(rc))
    {
      rw.writeAttribute("onunload", _PARTIAL_ONUNLOAD_HANDLER, null);
    }
    else
    {
      rw.writeAttribute("onload", getOnload(rc, component, bean), "onload");
      rw.writeAttribute("onunload", getOnunload(rc, component, bean), "onunload");

      // If partial back is supported,
      // render an onbeforeunload event handler. This javascript function
      // will save the page's state when the page is unloaded. This way if the
      // user goes back to the page via the Back button, we'll be able to
      // restore the state: the html and the javascript.
      if (_isPartialBackSupported(rc))
      {
        rw.writeAttribute("onbeforeunload",
                          _PPR_BACK_UNLOAD_SCRIPT,
                          null);
      }
    }
  }

  protected boolean getFirstClickPassed(
    UIComponent component,
    FacesBean   bean)
  {
    // =-=AEW firstClickPassed is not currently supported on document
    if (_firstClickPassedKey == null)
      return false;

    Object o = bean.getProperty(_firstClickPassedKey);
    if (o == null)
      o = _firstClickPassedKey.getDefault();
    return Boolean.TRUE.equals(o);
  }

  protected String getInitialFocusId(
    UIComponent component,
    FacesBean   bean)
  {
    return toString(bean.getProperty(_initialFocusIdKey));
  }

  protected String getOnload(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    String onload;
    if (_onloadKey == null)
      onload = null;
    else
      onload = toString(bean.getProperty(_onloadKey));

    String checkLoad = "";

    //PH: Currently, if a browser supports PPR, _checkLoad function is called
    //that sets initialFocus if set.For non-PPR browsers like blackBerry 4.0,
    //no body onload function is called. hence, initialFocus cannot is not set.
    //Therefore, created another function _checkLoadNoPPR() This function is
    //called by the onLoad JS handler of body tag when device does not support
    //PPR
    if (PartialPageUtils.supportsPartialRendering(rc))
    {
      // Don't short circuit...
      //PH:_checkLoad(event) is replaced by _checkLoad() because on certain
      //devices like IE Mobile , event object is not defined. Moreover,
      //_checkLoad function does not use event object. So, remove it altogether
      //for all PPR devices
      checkLoad = "_checkLoad()";
    }
    else
    {
      //HKuhn - in printable mode we don't need PPR checking
      // Check only, if Agents supports Navigation or Editing
      if (supportsNavigation(rc) || supportsEditing(rc))
        checkLoad = "_checkLoadNoPPR()";
    }

    return XhtmlUtils.getClientEventHandler(FacesContext.getCurrentInstance(), component,
             "load", null, XhtmlUtils.getChainedJS(checkLoad, onload, false), null);
  }

  protected String getOnunload(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {
    String onunload;
    if (_onunloadKey == null)
      onunload = null;
    else
      onunload = toString(bean.getProperty(_onunloadKey));
    if (PartialPageUtils.supportsPartialRendering(rc))
    {
      // Don't short circuit...
      onunload = XhtmlUtils.getChainedJS("_checkUnload(event)",
                                         onunload,
                                         false);
    }

    return XhtmlUtils.getClientEventHandler(FacesContext.getCurrentInstance(), component,
             "unload", null, onunload, null);
  }

  /**
   * Renders a top anchor at the top of the page
   * In quirks mode this is not required,but Mozilla will complain
   * in standards mode.
   * @param context
   * @throws IOException
   */
  private void _renderAnchorForTop(
    FacesContext context
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("a",null);
    writer.writeAttribute("name","top",null);
    writer.endElement("a");
  }

  private void _renderNoScript(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    // Some accessibility standards rather oddly claim that NOSCRIPT
    // tags are essential for compliance.  So, render NOSCRIPT, at
    // least when we're not in "inacessible" mode.
    //
    // But don't bother in design time mode - this check is
    // largely there for JDev 10.1.3 preview, which was rendering
    // the contents of any NOSCRIPT tags in the VE, but it's
    // a check that does no harm.
    if (!isInaccessibleMode(rc) && !Beans.isDesignTime())
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("noscript",null);
      String message = rc.getTranslatedString("NO_SCRIPT_MESSAGE");
      writer.writeText(message, null);
      writer.endElement("noscript");
    }
  }

  private void _storeInitialFocus(
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {

    // The initialFocus functionality is only supported in inaccessible mode,
    // and only platforms that support scripting.
    if (!isInaccessibleMode(rc) || !supportsScripting(rc))
    {
      return;
    }

    // initial focus is the id of the component to which you want the
    // focus to be when the page full-page loads. In a PPR, the focus is
    // not set, which is a good thing.
    String initialFocusID = getInitialFocusId(component, bean);
    if (initialFocusID != null)
    {
      // Put the initial focus id on the rendering context for use in
      // postrender and also so that it can be modified
      // by the component with this id if necessary. For example,
      // the NavigationBar needs its initial focus on the Next button, but
      // the component's id does not get rendered on the Next button. The
      // NavigationBar creates a special id for the Next button, and sticks
      // this id back on the AdfRenderingContext for the body to know about in
      // postrender.
      rc.getProperties().put(XhtmlConstants.INITIAL_FOCUS_CONTEXT_PROPERTY,
                              initialFocusID);
    }
  }

  //
  // Writes a small script that sets the _initialFocusID variable on the page.
  //
  private void _renderInitialFocusScript(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {

    // The initialFocus functionality is not supported if not inaccessible mode
    // nor on Netscape nor on platforms that do not support scripting.
    if (!isInaccessibleMode(rc) || !supportsScripting(rc))
    {
      return;
    }

    // Render the initial focus id, if it exists on the rendering context.
    // The initial focus id was initially set in prerender, and may have
    // been overwritten by the component's renderer if need be.
    String initialFocusID = (String) rc.getProperties().get(
                                    XhtmlConstants.INITIAL_FOCUS_CONTEXT_PROPERTY);

    if (initialFocusID != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", null);
      XhtmlRenderer.renderScriptTypeAttribute(context, rc);
      writer.writeText("_initialFocusID='", null);
      writer.writeText(initialFocusID, null);
      writer.writeText("';", null);
      writer.endElement("script");
    }
  }
  
  // Reverts to the legacy PPR channel (disables PPR over JSF Ajax)
  // if teh conetxt parameter is set
  private void _renderDisableJsfAjaxScript(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException 
  {
    if (!supportsScripting(rc))
    {
      return;
    }
    ExternalContext extContext = context.getExternalContext();
    if ("off".equalsIgnoreCase(extContext.getInitParameter(_PPR_OVER_JSF_AJAX)))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", null);
      writer.write("TrPage.getInstance().__disablePprOverJsfAjax()");
      writer.endElement("script");
    }
  }

  // If partial back is supported,
  // render a span with the _PPR_BACK_CONTENT_ID id.
  // This is used to be able to save the html content of the page
  // when the user leaves the page. This way we can restore the html if
  // the user used the Back button to go back to the page.
  private static void _renderPartialBackSupportSpan(
    FacesContext     context,
    RenderingContext rc,
    boolean          isStart
    ) throws IOException
  {
    if (_isPartialBackSupported(rc))
    {
      ResponseWriter writer = context.getResponseWriter();

      if (isStart)
      {
        writer.startElement("span", null);
        writer.writeAttribute("id", _PPR_BACK_CONTENT_ID, null);
      }
      else
      {
        writer.endElement("span");
        // render these hidden, disabled fields to save the javascript
        // library names, the inline scripts, and the page's contents
        // these will be restored when we come back to the page after
        // leaving it.
        _renderPartialBackSupportHiddenFields(writer, _PPR_BACK_SAVE_LIBRARY_ID);
        _renderPartialBackSupportHiddenFields(writer, _PPR_BACK_SAVE_SCRIPT_ID);
        _renderPartialBackSupportHiddenFields(writer, _PPR_BACK_SAVE_CONTENT_ID);
      }
    }
  }

  // given the field name, render an input element, with the fieldName
  // as the id, and make it hidden and disabled, so that the values do
  // not go to the server when the form is submitted.
  private static void _renderPartialBackSupportHiddenFields(
    ResponseWriter writer,
    String         fieldName
    ) throws IOException
  {
    writer.startElement("input", null);

    writer.writeAttribute("id", fieldName, null);
    writer.writeAttribute("type", "hidden", null);
    writer.writeAttribute("disabled", Boolean.TRUE, null);

    writer.endElement("input");
  }


  private static boolean _isPartialBackSupported(
    RenderingContext rc)
  {
    /*
      // Only supported on IE  - but comment this out while
      // it's off altogether
    if (!isIE(arc))
      return false;
    */

    // =-=AEW We need a mechanism to turn it on... or we need
    // to remove it altogether
    return false;
  }

  //
  // Writes version information about the page.
  //
  static private void _writeVersionInformation(
    FacesContext     context,
    RenderingContext rc
    ) throws IOException
  {
    String comment = _VERSION_COMMENT;

    Class<BodyRenderer> implClass = BodyRenderer.class;
    Package implPkg = implClass.getPackage();

    Class<FacesBean> apiClass  =  FacesBean.class;
    Package apiPkg = apiClass.getPackage();

    String versionInfo = _getVersionInfo(apiPkg, implPkg);

    comment += versionInfo;

    String accessibilityMode = null;
    if (isInaccessibleMode(rc))
      accessibilityMode = "disabled";
    else if (isScreenReaderMode(rc))
      accessibilityMode = "enhanced";

    if (accessibilityMode != null)
    {
      comment += ", Accessibility:"+accessibilityMode;
    }

    // Tack on the Skin id
    Skin skin = rc.getSkin();
    String skinId = skin.getId();
    if (skinId != null)
    {
      comment += ", skin:" + skinId;

      // Also log preferred Skin if we have one
      RequestContext requestContext = RequestContext.getCurrentInstance();
      String preferredSkin = requestContext.getSkinFamily();
      if (preferredSkin != null)
        comment += (" (" + preferredSkin + ")");
    }

    context.getResponseWriter().writeComment(comment);
  }

  static private void _encodeServiceScripts(
    FacesContext context
    ) throws IOException
  {
    ExtendedRenderKitService service =
      Service.getRenderKitService(context, ExtendedRenderKitService.class);
    if (service != null)
    {
      service.encodeScripts(context);
    }
  }

  static private String _getVersionInfo(
    Package apiPackage,
    Package implPackage)
  {

    String versionInfo    = "";
    String apiSpecTitle   = "Apache Trinidad API";
    String implSpecTitle  = "Apache Trinidad Implementation";
    String apiVersion     = "??";
    String implVersion    = "??";

    String temp;
    // This normally happens in dev environment, when we have mapped to the
    // dependencies. If you need to see the version info, then map it to the
    // api snapshot and impl snapshot jar and remove the dependency on api and impl
    // in the project. If we don't want to impact golden files, we can simply
    // return a blank string.
    if (apiPackage == null && implPackage == null)
    {
      return "(Version unknown)";
    }
    else
    {
      if (apiPackage != null)
      {
        apiSpecTitle = (((temp = apiPackage.getSpecificationTitle()) != null)?
                        temp : apiSpecTitle);

        apiVersion   = (((temp = apiPackage.getImplementationVersion()) != null)?
                        temp : apiVersion);
      }

      if(implPackage != null)
      {
        implSpecTitle = (((temp = implPackage.getSpecificationTitle())!= null)?
                        temp : implSpecTitle);

        implVersion   = (((temp = implPackage.getImplementationVersion())!= null)?
                        temp : implVersion);
       }

      // if there is version mismatch let us mark and print a verbose
      // information
      if (apiVersion == "??" || implVersion == "??")
        versionInfo = "Version unknown: ";
      else if (apiVersion != "??" && implVersion != "??"
                && !apiVersion.equals(implVersion))
        versionInfo = "Version mismatch: ";
    }

    return "(" + versionInfo + apiSpecTitle  + " - " + apiVersion  + "/"
                             + implSpecTitle + " - " + implVersion + ")";
  }

  private PropertyKey _firstClickPassedKey;
  private PropertyKey _initialFocusIdKey;
  private PropertyKey _onloadKey;
  private PropertyKey _onunloadKey;

  // Onunload handler used for partial page rendering
  private static final String _PARTIAL_ONUNLOAD_HANDLER = "_partialUnload()";

  static private final String _VERSION_COMMENT = "Created by Apache Trinidad ";

  // Constants for PPR back
  static private final String _PPR_BACK_UNLOAD_SCRIPT = "_savePageStateIE()";
  static private final String _PPR_BACK_CONTENT_ID = "_pprPageContent";
  static private final String _PPR_BACK_SAVE_CONTENT_ID = "_pprSavePage";
  static private final String _PPR_BACK_SAVE_SCRIPT_ID = "_pprSaveScript";
  static private final String _PPR_BACK_SAVE_LIBRARY_ID = "_pprSaveLib";
  
  static private final String _PPR_OVER_JSF_AJAX = "org.apache.myfaces.trinidadinternal.PPR_OVER_JSF_AJAX";
}
