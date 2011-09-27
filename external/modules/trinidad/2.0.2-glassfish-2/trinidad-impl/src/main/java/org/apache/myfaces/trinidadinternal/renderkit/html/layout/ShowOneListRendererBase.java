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
package org.apache.myfaces.trinidadinternal.renderkit.html.layout;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXShowDetail;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.RenderUtils;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.uinode.UINodeRendererBase;


/**
 *  Base Renderer for ShowOneChoice and PanelRadio
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/html/layout/ShowOneListRendererBase.java#0 $) $Date: 10-nov-2005.19:01:14 $
 */
abstract class ShowOneListRendererBase extends UINodeRendererBase
{
  /**
   *  {@inheritDoc}
   *  @return true always
   */
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   *  If nothing is disclosed, makes the first child disclosed.
   *
   *  Makes sure that the child it's disclosing has rendered = true
   *  and is not disabled.
   *
   * @param context
   * @param component
   * @throws IOException
   */
  @SuppressWarnings("unchecked")
  @Override
  public void encodeBegin(FacesContext context, UIComponent component)
    throws IOException
  {
    _LOG.entering("ShowOneListRendererBase", "encodeBegin");
    List<UIComponent> children = component.getChildren();
    int numChildren = children.size();
    UIComponent disclosedChild = null;
    UIXShowDetail renderableChild = null;

    for (int indxChild = 0; indxChild < numChildren ; indxChild++ )
    {
      UIComponent child = children.get(indxChild);
      if (! (child instanceof UIXShowDetail) )
      {
        continue;
      }

      UIXShowDetail detailChild = (UIXShowDetail) children.get(indxChild);

      if (detailChild.isRendered())
      {
        // Mark the first renderable child
        Object disabled =
          detailChild.getAttributes().get(
            UIConstants.DISABLED_ATTR.getAttributeName());
        if (Boolean.TRUE.equals(disabled))
        {
          continue;
        }
        if (renderableChild == null)
        {
          renderableChild = detailChild;
        }
        if (detailChild.isDisclosed())
        {
          disclosedChild = detailChild;
        }
      }
    }

    // If nothing has been disclosed as of yet, disclose the first rendered one
    if ( (disclosedChild == null) && (renderableChild != null) &&
      !renderableChild.isDisclosedTransient())
    {
      renderableChild.setDisclosed(true);
    }
    _LOG.exiting("ShowOneListRendererBase", "encodeBegin");
  }

  /**
   *  Renders a table and renders a choice list / radio button within it.
   *
   *  Depending on position and alignment attributes, renders the list control
   *  and disclosed showDetail child in the correct row and column.
   *
   *  Non UIXShowDetail children are ignored.
   *  The label of each of radio / choice items is the same as the text assigned
   *  to CoreShowDetailItem child. If no text attrbute value specified,
   *  the label remains blank.
   */
  @Override
  public void encodeChildren(FacesContext context, UIComponent component)
    throws IOException
  {
    writeAdditionalJS(context, component); // To spit out additional JS if any
    String position;
    String alignment;
    
    // In the case of narrow-screen PDAs, to reduce component's width, 
    // position is always top and alignment is always left. 
    if(XhtmlRenderer.supportsNarrowScreen
                    (RenderingContext.getCurrentInstance()))
    {
      position = _POSITION_TOP;
      alignment = _ALIGNMENT_LEFT;
    }
    else
    {
      alignment = _getAlignment(component);
      if (alignment == null)
      {
        alignment = _ALIGNMENT_DEFAULT_VALUE;
      }
      // No need to check for invalid value of alignment attr value as
      // it's handled below.

      // If the position is either null or an invalid value, default it.
      position = _getPosition(component);
      if (! positionMap.containsKey(position))
      {
        position = _POSITION_DEFAULT_VALUE;
      }
    }

    _LOG.finest("ShowOneListRendererBase.encodeChildren: alignment: {0}, position: {1} ",
                new Object[]{alignment, position} );

    ResponseWriter out = context.getResponseWriter();

    String disclosedItemId = _getDisclosedItemId(context, component);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    boolean isDesktop = (arc.getAgent().getType().equals(Agent.TYPE_DESKTOP));

    // Wrap the table element with a div tag for mobile browsers since some of it 
    // doesn't support PPR for table element. 
    if(!isDesktop )
    {
      out.startElement("div", component);
      out.writeAttribute("id", component.getClientId(context), null);
    }
    out.startElement("table", component);
    if(isDesktop )
    {
      out.writeAttribute("id", component.getClientId(context), null);
    }

    String shortDesc = (String) component.getAttributes().get("shortDesc");
    if (shortDesc != null)
    {
      out.writeAttribute("summary", shortDesc, null);
    }
    else
    {
      out.writeAttribute("summary", "", null);
    }
    out.writeAttribute("border", "0", null);
    out.writeAttribute("cellspacing", "0", null);
    out.writeAttribute("cellpadding", "0", null);

    String styleClass = (String) component.getAttributes().get("styleClass");
    
    if (styleClass != null)
    {
      XhtmlRenderer.renderStyleClass(context, arc, styleClass);
    }

    UIXRenderingContext rCtx = getRenderingContext(context, component);
    ShowOneUtils.renderGenericAttributes(rCtx, component, out);

    out.startElement("tr", component);

    // =-= rbaranwa push some of the below into functions
    if (position.equals(_POSITION_LEFT))
    {
      out.startElement("td", component);

      _renderAlignmentTopBottom(out, alignment);

      renderListDisplay(context, component,  disclosedItemId);
      out.endElement("td");

      renderSpacerTD(out, component, _SEPARATOR_SIZE);

      out.startElement("td", component);
      out.writeAttribute("align", "left", null);
      _findAndEncodeChild(context, component, disclosedItemId);
      out.endElement("td");
    }

    if (position.equals(_POSITION_RIGHT))
    {
      out.startElement("td", component);
      out.writeAttribute("align", "right", null);
      _findAndEncodeChild(context, component, disclosedItemId);
      out.endElement("td");

      renderSpacerTD(out, component, _SEPARATOR_SIZE);

      out.startElement("td", component);
      _renderAlignmentTopBottom(out, alignment);
      out.writeAttribute("align", "left", null);
      renderListDisplay(context, component,  disclosedItemId);
      out.endElement("td");
    }

    if (position.equals(_POSITION_BOTTOM))
    {
      out.startElement("td", component);
      _findAndEncodeChild(context, component, disclosedItemId);
      out.endElement("td");
      out.endElement("tr");

      _renderSpacerTR(out, component);

      out.startElement("tr", component);
      out.startElement("td", component);

      _renderAlignmentLeftRight(out, alignment);

      renderListDisplay(context, component,  disclosedItemId);
      out.endElement("td");
    }

    if (position.equals(_POSITION_TOP))
    {
      out.startElement("td", component);

      _renderAlignmentLeftRight(out, alignment);

      renderListDisplay(context, component,  disclosedItemId);
      out.endElement("td");
      out.endElement("tr");

      _renderSpacerTR(out, component);

      out.startElement("tr", component);

      out.startElement("td", component);
      _findAndEncodeChild(context, component, disclosedItemId);
      out.endElement("td");
    }

    out.endElement("tr");
    out.endElement("table");
    if(!isDesktop )
    { 
      out.endElement("div");
    }
  }

  /**
   *  Dummy method to prevent base class' encodeEnd call.
   *
   * @param context the faces context object
   * @param component the UIComponent object
   * @throws IOException when some issues while writing output
   */
  @Override
  public void encodeEnd(FacesContext context,
                        UIComponent component)
    throws IOException
  {
    // This is done so that the UIXComponentUINode does not
    // go looking for a UIX 22 style renderer for this component
  }

  /**
   *  Writes any additional javascriipt required to support the control.
   *
   *  Default implementation doesn't do anything.
   *
   *  @param context the facesContext object
   *  @param component
   *  @throws IOException when any IO error while writing markup
   */
  protected void writeAdditionalJS(FacesContext context,
                                   UIComponent component)
    throws IOException
  {
    // Default implementation writes no javascript.
  }


  /**
   * Returns true if the showOneChoice element has an id
   * and the browser supports PPR
   */
  protected boolean elementSupportsPartial(UIXRenderingContext context,
                                           String           id)
  {
    return ((id != null) &&
            XhtmlLafRenderer.supportsPartialRendering(context));
  }


  /**
   *  Renders the label associated with the control.
   *
   *  Takes care of encoding for label text and the accessKey.
   *
   */
  protected void renderSelectLabel(UIXRenderingContext rCtx,
                                   UIComponent component,
                                   ResponseWriter out,
                                   String compId)
   throws IOException
  {
    out.startElement("td", component);
    out.writeAttribute("align", "left", null);
    out.writeAttribute("nowrap", Boolean.TRUE, null);
    out.startElement("span", component);

    XhtmlLafRenderer.renderStyleClassAttribute(rCtx, getLabelClassName());

    out.startElement("label", component);

    String controlId = getHTMLControlID(compId);

    out.writeAttribute("for", controlId, null);

    Character accessChar =
      (Character) component.getAttributes().get("accessKey");
    if (accessChar != null)
    {
      out.writeAttribute("accessKey", accessChar.toString(), null);
    }

    writeLabel(out,
              component,
              (String) component.getAttributes().get("label"));

    out.endElement("label");
    out.endElement("span");
    out.endElement("td");
  }

  /**
   *  Renders the label text with character underlined for label.
   *
   *  Takes care of encoding for label text and the accessKey.
   *
   */
  protected void writeLabel(ResponseWriter out,
                            UIComponent component,
                            String label)
    throws IOException
  {
    // AdamWiner: TODO: replace this with a call to AccessKeyUtils
    Character accessChar =
      (Character) component.getAttributes().get("accessKey");

    if (label != null)
    {
      if (accessChar == null)
      {
        out.writeText(label, null);
      }
      else
      {
        int accessKeyIndex = label.indexOf(accessChar.charValue());
        if (accessKeyIndex < 0)
        {
          out.writeText(label, null);
        }
        else
        {
          // String stripping to remove &amp; will already be done in Tag impl
          String strBefAccessKey = label.substring(0, accessKeyIndex);
          String strAfterAccessKey = label.substring(accessKeyIndex + 1,
                                                     label.length());
          out.writeText(strBefAccessKey, null);

          //ADFFACES-153: use default style (underline) for access key
          out.startElement ("span", null);
          XhtmlRenderer.renderStyleClass (FacesContext.getCurrentInstance(),
                                          RenderingContext.getCurrentInstance(),
                                          SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
          out.writeText (accessChar.toString(), null);
          out.endElement ("span");

          out.writeText(strAfterAccessKey, null);
        }
      }
    }
  }

  /**
   *  Generates markup for rendering a blank TD.
   *
   */
  protected void renderSpacerTD(ResponseWriter out,
                                UIComponent component,
                                String separatorSize)
    throws IOException
  {
    out.startElement("td", component);
    out.writeAttribute("width", separatorSize, null);
    out.endElement("td");
  }

  /**
   *  Gets id of the rendered HTML control
   */
  protected String getHTMLControlID(String compId)
  {
    // Default implementation just returns the same id.
    // Choice implementation returns id of the select HTML control
    return compId;
  }

  /**
   *  Gets classname of the text associated with HTML control text
   */
  protected String getFieldTextClass()
  {
    return SkinSelectors.AF_FIELD_TEXT_STYLE_CLASS;
  }

  /**
   *  Gets classname of the text associated with HTML control text
   */
  protected String getLabelControlSeparatorSize()
  {
    return _LABEL_CONTROL_SEPARATOR_SIZE;
  }

  /**
   *  Renders the list UI Control either radio buttons or choice.
   *
   */
  protected abstract void renderListDisplay(FacesContext context,
                                            UIComponent component,
                                            String disclosedChildId)
    throws IOException;



  /**
   *  Gets class name to be used for controls' label.
   *
   */
  private String getLabelClassName()
  {
    return SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS;
  }

  /**
   *  Returns first disclosed child's Id.
   *
   * @param context the facesContext object
   * @param component
   * @return String id of the first showDetailItem child that can be rendered
   *                and is disclosed
   * @throws IOException
   */
  @SuppressWarnings("unchecked")
  private static String _getDisclosedItemId(FacesContext context,
                                            UIComponent component)
    throws IOException
  {
    String returnId = null;
    ListIterator<UIComponent> iter = component.getChildren().listIterator();

    if (iter == null)
    {
      return null;
    }

    // This will only render UIXShowDetail children.
    // other children are ignored.
    while ( iter.hasNext())
    {
      UIComponent child = iter.next();
      if (! child.isRendered() )
      {
        continue;
      }

      // only CoreShowDetailItem children are rendered. Rest are ignored.
      if ( (child instanceof UIXShowDetail))
      {
        UIXShowDetail detailChild = (UIXShowDetail) child;
        if (detailChild.isDisclosed())
        {
          returnId = detailChild.getClientId(context);
          break;
        }
      }
    }
    return returnId;
  }


  /**
   *  Get attribute value for position attribute.
   *
   * @param component
   * @return value contained in position attribute in component's attr map
   */
  private static String _getPosition(UIComponent component)
  {
    return (String) component.getAttributes().get("position");
  }

  /**
   *  Get attribute value for alignment attribute.
   *
   * @param component
   * @return value contained in alignment attribute in component's attr map
   */
  private static String _getAlignment(UIComponent component)
  {
    return (String) component.getAttributes().get("alignment");
  }

  /**
   *  Find and encodes recursivly the child specified.
   *
   *  If the specified child can't be found, doesn't do anything.
   *
   *  @param context the facesContext object
   *  @param parent which contains the child to be encoded
   *  @param disclosedChildId id of the child which occurs in parent
   *         and has to be encoded recursively.
   *
   *  @throws IOException when any IO error while writing markup
   */
  @SuppressWarnings("unchecked")
  private static void _findAndEncodeChild(FacesContext context,
                                          UIComponent parent,
                                          String disclosedChildId)
    throws IOException
  {
    UIComponent disclosedChild = _findChild(context,
                                            parent,
                                            disclosedChildId);

    if (disclosedChild == null)
    {
      return;
    }

    // Paint container span for showDetail child
    ResponseWriter out = context.getResponseWriter();
    out.startElement("span", disclosedChild);

    // Render the children of disclosedChild since the child is disclosed.
    if (disclosedChild.isRendered())
    {
      if (disclosedChild.getChildCount() > 0)
      {
        for(UIComponent child : (List<UIComponent>)disclosedChild.getChildren())
        {
          RenderUtils.encodeRecursive(context, child);
        }
      }
    }

    out.endElement("span");
    // end container span for showDetail child
  }

  /**
   *  Find the child whose Id is passed as argument.
   *
   *  If the specified child can't be found, doesn't do anything.
   *
   *  @param context the facesContext object
   *  @param parent which may contain disclosedChildId as it's child
   *  @param disclosedChildId id of the child to be found among it's children
   *
   */
  @SuppressWarnings("unchecked")
  private static UIComponent _findChild(FacesContext context,
                                        UIComponent component,
                                        String disclosedChildId)
  {
    if  (disclosedChildId == null)
    {
      return null;
    }

    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      String childId = child.getClientId(context);
      if (disclosedChildId.equals(childId))
      {
        return child;
      }
    }

    return null;
  }

  /**
   *  Generates markup for rendering a blank TR and TD within it.
   *
   */
  private void _renderSpacerTR(ResponseWriter out, UIComponent component)
    throws IOException
  {
    out.startElement("tr", component);
    out.startElement("td", component);
    out.writeAttribute("height", _SEPARATOR_SIZE, null);
    out.endElement("td");
    out.endElement("tr");
  }

  /**
   *  Generates markup for rendering aligning along top/bottom.
   *
   */
  private void _renderAlignmentTopBottom(ResponseWriter out, String alignment)
    throws IOException
  {
    if (alignment.equals(_ALIGNMENT_TOP))
    {
      out.writeAttribute("valign", "top", null);
    }
    else if (alignment.equals(_ALIGNMENT_BOTTOM))
    {
      out.writeAttribute("valign", "bottom", null);
    }
    else // is an invalid combination now
    {
      out.writeAttribute("valign", "middle", null);
    }
  }

  /**
   *  Generates markup for rendering aligning along start(left)/end(right).
   *
   */
  private void _renderAlignmentLeftRight(ResponseWriter out, String alignment)
    throws IOException
  {
    if (alignment.equals(_ALIGNMENT_LEFT))
    {
      out.writeAttribute("align", "left", null);
    }
    else if (alignment.equals(_ALIGNMENT_RIGHT))
    {
      out.writeAttribute("align", "right", null);
    }
    else // is an invalid combination now
    {
      out.writeAttribute("align", "center", null);
    }
  }


  // ShowOne alignment related constants
  private static final String _ALIGNMENT_LEFT    = "start";
  private static final String _ALIGNMENT_RIGHT   = "end";
  private static final String _ALIGNMENT_TOP     = "top";
  private static final String _ALIGNMENT_BOTTOM  = "bottom";
  private static final String _ALIGNMENT_CENTER  = "center";
  private static final String _ALIGNMENT_DEFAULT_VALUE = _ALIGNMENT_CENTER;

  // ShowOne position related constants
  private static final String _POSITION_LEFT    = "start";
  private static final String _POSITION_RIGHT   = "end";
  private static final String _POSITION_TOP     = "top";
  private static final String _POSITION_BOTTOM  = "bottom";
  private static final String _POSITION_DEFAULT_VALUE = _POSITION_LEFT;

  private static Map<String, String> positionMap;

  static
  {
    positionMap =  new HashMap<String, String>(4);
    positionMap.put(_POSITION_LEFT,   _POSITION_LEFT);
    positionMap.put(_POSITION_RIGHT,  _POSITION_RIGHT);
    positionMap.put(_POSITION_TOP,    _POSITION_TOP);
    positionMap.put(_POSITION_BOTTOM, _POSITION_BOTTOM);
  }

  private static final String _SEPARATOR_SIZE = "8";
  private static final String _LABEL_CONTROL_SEPARATOR_SIZE = "12";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ShowOneListRendererBase.class);
}
