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

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.html.HTMLWebBean;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FormElementRenderer.java#0 $) $Date: 10-nov-2005.18:53:50 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
abstract public class FormElementRenderer extends XhtmlLafRenderer
{
  @Override
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (renderAsElement(context, node))
    {
      // 2484841 PDA: TOO MUCH WHITESPACE BETWEEN INPUT ELEMENTS IN LABELEDFIELD
      // 2981538 PDA: TOO MUCH WHITESPACE UNDER INPUT ELEMENTS IN TABLE
      // This is a browser bug workaround,
      // hopefully we can remove it eventually
      if ( context.getAgent().getAgentType() == TrinidadAgent.TYPE_PDA &&
           context.getAgent().getAgentApplication() ==
                                                TrinidadAgent.Application.IEXPLORER )
        setRenderingProperty(context,
                             VISIBLE_FORM_ELEMENT_RENDERED,
                             Boolean.TRUE);
      super.render(context, node);
    }
    else
    {
      renderAsNonElement(context, node);
    }
  }

  protected void renderAsNonElement(UIXRenderingContext context,
                                    UINode node)
    throws IOException
  {
    // Added prerender to put a span around the entire element
    // for the Visual Editor and PPR. See bug # 2222541.
    UIComponent component = NodeUtils.getUIComponent(context, node);

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(SPAN_ELEMENT, component);
    renderID( context, node );
    renderStyleAttrs(context, node);
    renderAsNonElementContent(context, node);
    writer.endElement(SPAN_ELEMENT);
  }

  protected void renderAsNonElementContent(
     UIXRenderingContext context, UINode node) throws IOException
  {
    renderContent(context, node);
  }

  @Override
  protected void postrender(UIXRenderingContext context, UINode node)
    throws IOException
  {
    super.postrender(context, node);

    // see bug 2880407 we dont need to render hidden label when wrapped with
    // fieldset and legend
    if (isHiddenLabelRequired(context, node))
      renderShortDescAsHiddenLabel(context, node, getID(context, node));
  }

  protected boolean isHiddenLabelRequired(UIXRenderingContext context, UINode node)
  {
    return true;
  }

  /**
   * @param ID the id of the form control that needs the hidden label.
   */
  public static void renderShortDescAsHiddenLabel(
    UIXRenderingContext context,
    UINode           node,
    Object           id
    )
    throws IOException
  {
    Object shortDesc = node.getAttributeValue(context,
                                              SHORT_DESC_ATTR);
    // because of bug 1856547, in addition to rendering title attributes for
    // short descriptions, we also render LABELs with invisible text. However,
    // these labels are useless if the browser does not support IDs
    if ((shortDesc!=null) &&
        HiddenLabelUtils.supportsHiddenLabels(context))
    {
      if (id != null)
      {
        HiddenLabelUtils.outputHiddenLabel(context,
                                           id.toString(),
                                           shortDesc, node.getUIComponent());
      }
    }
  }

  @Override
  protected Object getID(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // check to see if we already have a cached ID
    Object id = context.getLocalProperty(0, ID_ATTR, NOT_FOUND);

    if (id == NOT_FOUND)
    {
      // we do not have a cached ID, so call the superclass method to get one
      id = super.getID(context, node);

      if (id == null)
      {
        // the superclass method has not found an ID. We may need to create
        // one if we definitely need an ID. We will need an ID if a LABEL
        // element needs to be created. LABEL elements are created for short
        // descriptions. see bug 1856547
        if (needsUniqueID(context, node))
        {
          // since we have a short description, we definitely need an ID
          id = XhtmlLafUtils.generateUniqueID(context);
        }
      }
      // cache the ID (which may be null). if you change this, make sure you
      // see CheckBoxRenderer.getID(..)
      context.setLocalProperty(ID_ATTR, id);
    }

    return id;
  }

  protected boolean needsUniqueID(
    UIXRenderingContext context,
    UINode           node)
  {
    // In "inaccessible" mode, we won't create a label for the "shortDesc";
    // so don't generate the ID, which is a waste of time and space.
    if (isInaccessibleMode(context))
      return false;

    Object shortDesc = node.getAttributeValue(context,
                                              SHORT_DESC_ATTR);
    return (shortDesc != null);
  }

  /**
   * Returns true if we are rendering as the FormElement that we are and not
   * some placeholder for that value.  The typical reason for this method
   * returning false, is that the UINode is read-only.
   */
  protected final boolean renderAsElement(
    UIXRenderingContext context,
    UINode           node)
  {
    // Cache the value of "renderAsElement", which is getting
    // retrieved 5 times for text inputs, and more for other element
    // types.
    Object o = context.getLocalProperty(0, _AS_ELEMENT_KEY, null);
    if (o == null)
    {
      boolean asElement =
         ((!Boolean.TRUE.equals(getReadOnly(context, node)) ||
           renderReadOnlyAsElement(context, node)) &&
          (supportsDisabledFormElements(context) ||
           !Boolean.TRUE.equals(getDisabled(context, node))));
      context.setLocalProperty(_AS_ELEMENT_KEY,
                               asElement ? Boolean.TRUE : Boolean.FALSE);
      return asElement;
    }
    else
    {
      return Boolean.TRUE == o;
    }
  }

  protected boolean renderReadOnlyAsElement(
    UIXRenderingContext context,
    UINode           node)
  {
    return false;
  }

  protected Boolean getDisabled(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (Boolean.TRUE.equals(node.getAttributeValue(context, DISABLED_ATTR)))
      return Boolean.TRUE;
    return Boolean.FALSE;
  }

  protected Boolean getReadOnly(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (!supportsEditing(context))
      return Boolean.TRUE;

    if (Boolean.TRUE.equals(node.getAttributeValue(context, READ_ONLY_ATTR)))
      return Boolean.TRUE;
    return Boolean.FALSE;
  }

  protected Object getOnBlur(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_BLUR_ATTR);
  }

  protected Object getOnFocus(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, ON_FOCUS_ATTR);
  }


  /**
   * Override to return different styles depending on whether we
   * are disabled or not.
   */
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // get any style class that the client may have set
    Object styleClass = super.getStyleClass(context, node);

    if (styleClass == null)
    {
      styleClass = (isDisabled(context, node))
                     ? getDefaultDisabledStyleClass(context, node)
                     : getDefaultStyleClass(context, node);
    }

    return styleClass;
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
  @Override
  protected boolean doRenderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (renderStyleElements(context))
    {
      // =-=AEW Is this necessary???
      Object renderAsElement = context.getLocalProperty(0,
                                                        _AS_ELEMENT_KEY,
                                                        null);
      // =-=AEW In 2.2, the below line compared Boolean.TRUE
      // to _AS_ELEMENT_KEY!!! which would always be false.
      return ( Boolean.TRUE.equals( renderAsElement ) );
    }

    return true;
  }



  protected Object getDefaultStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return AF_FIELD_TEXT_STYLE_CLASS;
  }


  protected Object getDefaultDisabledStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return AF_FIELD_TEXT_DISABLED_STYLE_CLASS;
  }


  /**
   * Returns the value associated with the text attribute
   */
  @Override
  protected Object getText(UIXRenderingContext context,  UINode  node)
  {
    return BaseLafUtils.getLocalTextAttribute(context, node, TEXT_ATTR);
  }

  /**
   * Returns the name of the node, transformed for the given context
   */
  @Override
  protected Object getTransformedName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // check to see if we already have a cached transName
    Object transName =
      context.getLocalProperty(0, _TRANSFORMED_NAME_KEY, NOT_FOUND);

    if (transName == NOT_FOUND)
    {
      // we do not have a cached transName, so call superclass method to get one
      transName = super.getTransformedName(context, node);

      // cache the transName (which may be null).
      context.setLocalProperty(_TRANSFORMED_NAME_KEY, transName);
    }

    return transName;
  }

  static private final Object _TRANSFORMED_NAME_KEY = new Object();
  static private final Object _AS_ELEMENT_KEY = new Object();

  // 2484841 PDA: TOO MUCH WHITESPACE BETWEEN INPUT ELEMENTS IN LABELEDFIELD
  // 2981538 PDA: TOO MUCH WHITESPACE UNDER INPUT ELEMENTS IN TABLE
  // This is a browser bug workaround,  hopefully we can remove it eventually
  /**
   * @deprecated
   */
  @Deprecated
  public static final Object VISIBLE_FORM_ELEMENT_RENDERED = new Object();
  // 2484841 PDA: TOO MUCH WHITESPACE BETWEEN INPUT ELEMENTS IN LABELEDFIELD
  // 2981538 PDA: TOO MUCH WHITESPACE UNDER INPUT ELEMENTS IN TABLE
  // This is a browser bug workaround, hopefully we can remove it eventually
  /**
   * @deprecated
   */
   @Deprecated
  public static final MutableUINode PDA_SPACE = new HTMLWebBean("div");
   static{
     MarlinBean spacer = new MarlinBean(SPACER_NAME);
     spacer.setAttributeValue(WIDTH_ATTR, "1");
     spacer.setAttributeValue(HEIGHT_ATTR, "0");
     PDA_SPACE.addIndexedChild(spacer);
   }
}
