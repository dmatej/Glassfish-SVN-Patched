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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.html.HtmlFrameBorderLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FrameBorderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:53:53 $
 */
public class FrameBorderLayoutRenderer extends XhtmlRenderer
{
  public FrameBorderLayoutRenderer()
  {
    this(HtmlFrameBorderLayout.TYPE);
  }

  protected FrameBorderLayoutRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(
    FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _onloadKey = type.findKey("onload");
    _onunloadKey = type.findKey("onunload");
    _frameSpacingKey = type.findKey("frameSpacing");
    _borderWidthKey = type.findKey("borderWidth");
    _frameBorderWidthKey = type.findKey("frameBorderWidth");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }


  @Override
  protected void renderAllAttributes(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    renderShortDescAttribute(context, rc, component, bean);
    renderStyleAttributes(context, rc, component, bean);

    // Explicitly render rows to ensure onunload handler will fire
    writer.writeAttribute(XhtmlConstants.ROWS_ATTRIBUTE, "100%,*", null);

    if (supportsScripting(rc))
    {
      writer.writeAttribute("onload",
                            bean.getProperty(_onloadKey),
                            null);

      /**
       * =-= bts
       * We can't chain the JavaScript because we have no way of
       * making sure that Core.js has been included in the HEAD
       * tag
       */
      // =-=AEW We no longer attempt to chain in _checkUnload() (contrary
      // to the comment from bts, we were doing so).  Note that
      // _checkUnload() only needs to be called from a modal window.
      // But the only framesets we use on modal window are created
      // by fred.jsp (and its ancestor, frameRedirect.jsp).  These
      // use HTMLWebBean to create the frameset.
      // We're still left with the possibility that some clients
      // will have used FrameBorderLayoutRenderer as the top piece
      // of a modal window, and went to the effort of adding
      // a ScriptBean to the head section to load Core.js.
      // We could help these people out quite a bit by modifying
      // _checkUnload() so that firing on the BodyBean of one
      // of the contained frames is sufficient (it should be), but let's
      // be conservative for now about mucking around with our Javascript
      writer.writeAttribute("onunload",
                            bean.getProperty(_onunloadKey),
                            null);
    }
  }

  @Override
  protected final void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("frameset", component);
    renderId(context, component);

    renderAllAttributes(context, rc, component, bean);

    String leftName       = HtmlFrameBorderLayout.LEFT_FACET;
    String innerLeftName  = HtmlFrameBorderLayout.INNER_LEFT_FACET;
    String rightName      = HtmlFrameBorderLayout.RIGHT_FACET;
    String innerRightName = HtmlFrameBorderLayout.INNER_RIGHT_FACET;

    UIComponent center = getFacet(component, HtmlFrameBorderLayout.CENTER_FACET);
    UIComponent top    = getFacet(component, HtmlFrameBorderLayout.TOP_FACET);
    UIComponent bottom = getFacet(component, HtmlFrameBorderLayout.BOTTOM_FACET);
    UIComponent left   = getFacet(component, leftName);
    UIComponent right  = getFacet(component, rightName);
    UIComponent innerLeft   = getFacet(component, innerLeftName);
    UIComponent innerRight  = getFacet(component, innerRightName);

    boolean r2l = rc.getLocaleContext().isRightToLeft();
    if (left == null)
    {
      leftName = r2l ? HtmlFrameBorderLayout.END_FACET : HtmlFrameBorderLayout.START_FACET;
      left     = getFacet(component, leftName);
    }

    if (right == null)
    {
      rightName = r2l ? HtmlFrameBorderLayout.START_FACET : HtmlFrameBorderLayout.END_FACET;
      right     = getFacet(component, rightName);
    }

    if (innerLeft == null)
    {
      innerLeftName = r2l ? HtmlFrameBorderLayout.INNER_END_FACET : HtmlFrameBorderLayout.INNER_START_FACET;
      innerLeft     = getFacet(component, innerLeftName);
    }

    if (innerRight == null)
    {
      innerRightName = r2l ? HtmlFrameBorderLayout.INNER_START_FACET : HtmlFrameBorderLayout.INNER_END_FACET;
      innerRight     = getFacet(component, innerRightName);
    }

    //sizes is set to be "top.height , * , bottom.height"
    String sizes = _getSizeString(top,
                                  null,
                                  null,
                                  bottom,
                                  "height");

    // if sizes=="*" then there is no need for a frameset since there is only
    // the center frame
    boolean renderTopBottomFrame = (sizes.length() > 1);

    if (renderTopBottomFrame)
    {
      // this frameset contains the top and bottom children
      writer.startElement("frameset", null);
      if (!isInaccessibleMode(rc))
      {
        writer.writeAttribute(
          "title",
          rc.getTranslatedString(
            "af_frameBorderLayout.HORIZONTAL_FRAMESET_LAYOUT_CONTAINER_TITLE"),
          null);
      }
      writer.writeAttribute(XhtmlConstants.ROWS_ATTRIBUTE, sizes, null);

      _renderFrameBorderAndSpacing(writer, component, bean);
    }

    _encodeFacet(context, top);

    // sizes is set to be
    // "left.width , innerLeft.width , * , innerRight.width , right.width"
    sizes = _getSizeString(left,
                           innerLeft,
                           innerRight,
                           right,
                           "width");

    // if sizes=="*" then there is no need for a frameset since there is only
    // the center frame
    boolean renderLeftRightFrame = (sizes.length() > 1);

    if (renderLeftRightFrame)
    {
      // this frameset renders the left, center and right children
      writer.startElement("frameset", null);
      if (!isInaccessibleMode(rc))
      {
        writer.writeAttribute(
          "title",
          rc.getTranslatedString(
            "af_frameBorderLayout.VERTICAL_FRAMESET_LAYOUT_CONTAINER_TITLE"),
          null);
      }
      writer.writeAttribute(XhtmlConstants.COLS_ATTRIBUTE, sizes, null);

      _renderFrameBorderAndSpacing(writer, component, bean);
    }

    _encodeFacet(context, left);
    _encodeFacet(context, innerLeft);
    _encodeFacet(context, center);
    _encodeFacet(context, innerRight);
    _encodeFacet(context, right);

    if (renderLeftRightFrame)
    {
      // end the left-center-right frameset
      writer.endElement("frameset");
    }

    _encodeFacet(context, bottom);

    if (renderTopBottomFrame)
    {
      // end the top-bottom frameset
      writer.endElement("frameset");
    }

    UIComponent alternateContent = getFacet(component,
                                 HtmlFrameBorderLayout.ALTERNATE_CONTENT_FACET);
    if (alternateContent != null)
    {
      writer.startElement("noframes", null);
      encodeChild(context, alternateContent);
      writer.endElement("noframes");
    }

    writer.endElement("frameset");
  }

  private String _getSizeString(
    UIComponent outer1,
    UIComponent inner1,
    UIComponent inner2,
    UIComponent outer2,
    String      attr)
  {
    StringBuffer buf = new StringBuffer(17);

    if (outer1 != null)
      _getPropertyValue(outer1, attr, buf).append(',');

    if (inner1 != null)
      _getPropertyValue(inner1, attr, buf).append(',');

    buf.append('*');

    if (inner2 != null)
      _getPropertyValue(inner2, attr, buf.append(','));

    if (outer2 != null)
      _getPropertyValue(outer2, attr, buf.append(','));

    return buf.toString();
  }

  private StringBuffer _getPropertyValue(
    UIComponent  frame,
    String       attr,
    StringBuffer result)
  {
    Object val = frame.getAttributes().get(attr);

    if (val == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("FRAME_MISSING_ATTRIBUTE", new Object[] { frame.getId(), attr });
      val = "0";
    }

    result.append(val);
    return result;
  }

  private void _encodeFacet(
    FacesContext context,
    UIComponent  component
    ) throws IOException
  {
    if (component != null)
    {
      encodeChild(context, component);
    }
  }

  private void _renderFrameBorderAndSpacing(
    ResponseWriter writer,
    UIComponent    component,
    FacesBean      bean
    ) throws IOException
  {
    writer.writeAttribute("framespacing", _getFrameSpacing(component, bean), "frameSpacing");
    writer.writeAttribute("border", _getBorderWidth(component, bean), "borderWidth");
    writer.writeAttribute("frameborder", _getFrameBorderWidth(component, bean), "frameBorderWidth");
  }

  private Object _getFrameSpacing(
    UIComponent component,
    FacesBean   bean)
  {
    Object value = bean.getProperty(_frameSpacingKey);
    if (value == null)
      value = _frameSpacingKey.getDefault();

    return value;
  }

  private Object _getBorderWidth(
    UIComponent component,
    FacesBean   bean)
  {
    Object value = bean.getProperty(_borderWidthKey);
    if (value == null)
      value = _borderWidthKey.getDefault();

    return value;
  }

  private Object _getFrameBorderWidth(
    UIComponent component,
    FacesBean   bean)
  {
    Object value = bean.getProperty(_frameBorderWidthKey);
    if (value == null)
      value = _frameBorderWidthKey.getDefault();

    return value;
  }

  private PropertyKey _onloadKey;
  private PropertyKey _onunloadKey;
  private PropertyKey _frameSpacingKey;
  private PropertyKey _borderWidthKey;
  private PropertyKey _frameBorderWidthKey;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FrameBorderLayoutRenderer.class);
}
