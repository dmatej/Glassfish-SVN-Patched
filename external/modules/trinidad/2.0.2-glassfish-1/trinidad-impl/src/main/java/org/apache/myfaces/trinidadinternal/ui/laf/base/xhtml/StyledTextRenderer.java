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

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXValue;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;


/**
 * Renderer for drawing text that can be styled, can include a destination,
 * and can act as a label for another control.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/StyledTextRenderer.java#0 $) $Date: 10-nov-2005.18:54:14 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class StyledTextRenderer extends XhtmlLafRenderer
{
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // render the element text here
    Object textValue = getText(context, node);

    // if there's no text or children, render nothing
    if (_isEmpty(context, node, textValue))
    {
      // If we've got an ID, we need to render something
      if (supportsID(context))
      {
        Object id = getID(context, node);
        if (id != null)
          _renderEmptySpan(context, id);
      }

      return;
    }

    ResponseWriter writer = context.getResponseWriter();


    int     keyIndex = -1;

    String accessString = null;

    // string version of textValue
    String textString = null;

    if ((textValue != null) && supportsAccessKeys(context))
    {
      textString = textValue.toString();

      // determine the index of the access key, if any
      keyIndex = getAccessKeyIndex(context, node, textString);

      // create the String containing the access key, if any
      accessString = (keyIndex != -1)
                       ? String.valueOf(textString.charAt(keyIndex))
                       : null;
    }

    _renderDescription(context, node);


    if (textValue != null)
    {
      Object labeledNodeID = getLabeledNodeID(context, node);

      if (labeledNodeID != null)
      {
        String labeledNodeIDString = labeledNodeID.toString();

        // write the component that this is a label for
        if (supportsID(context))
        {
          writer.writeAttribute("for", labeledNodeID, null);
          HiddenLabelUtils.rememberLabel(context, labeledNodeIDString);
        }

        if (textString == null)
          textString = textValue.toString();

        // add the mapping from the target ID to this label
        FormRenderer.addLabelMapping( labeledNodeIDString, textString);
      }

      if (keyIndex != -1)
      {
        //
        // the access key exists in the text
        //

        // write the access string, if we haven't already written it
        writer.writeAttribute("accesskey", accessString, null);

        // render the access key text
        // use underline style class for the access key
        renderAccessKeyText(context,
                            textValue,
                            keyIndex,
                            SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
      }
      else
      {
        // output the text directly since we have no access key
        writer.writeText(textValue, UIXValue.VALUE_KEY.getName());
      }
    }

    super.renderContent(context, node);

    // give subclasses a chance to render scripts
    renderScripts(context, node);
  }

  /**
   * Actually renders the style attributes.
   * @see #doRenderStyleAttrs
   */
  @Override
  protected void renderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderStyleClassAttribute(context, getInheritedStyleClass(context, node));

    renderInlineStyleAttribute(context, node);
  }

  protected void renderScripts(
    UIXRenderingContext context,
    UINode node
    )
    throws IOException
  {
    // do nothing
  }

  /**
   * Returns false because styled text is read only, so its value does not get
   * submitted to the server, making it unavailable for form data repopulation.
   * The literal or data bound attribute value should be used instead.
   */
  protected boolean shouldUseFormData(
    UIXRenderingContext  context,
    UINode            node
    )
  {
    return false;
  }

  protected Object getInheritedStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return getStyleClass(context, node);
  }

  @Override
  protected Object getText(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return XhtmlLafUtils.getLocalTextAttribute(context,
                                               node,
                                               TEXT_ATTR);
  }

  @Override
  protected boolean doRenderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return true;
  }


  protected Object getLabeledNodeID(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return XhtmlLafUtils.getLocalAttribute(context,
                                           node,
                                           LABELED_NODE_ID_ATTR);
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // don't render anything if the text is null and no children exist
    if (_isEmpty(context, node, getText(context, node)))
      return null;

    String name = null;

    if (supportsID(context) && (getLabeledNodeID(context, node) != null))
      name = "label";
    else 
      name = getDefaultElement();

    return name;
  }

  /**
   * Hook for subclasses to change the default element used
   */
  protected String getDefaultElement()
  {
    return SPAN_ELEMENT;
  }

  private boolean _isEmpty(
    UIXRenderingContext context,
    UINode           node,
    Object           text
    )
  {
    return ((text == null) &&
            (node.getIndexedChildCount(context) <= 0));
  }

  // Renders an empty span with an ID - this gets rendered if we
  // have no text
  private void _renderEmptySpan(
    UIXRenderingContext context,
    Object          id
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("span", null);
    renderID(context, id, false);
    writer.endElement("span");
  }


  /**
   * Used for inserting a SPAN tag with p_OraHiddenLabel style class and
   * the description attribute.
   * @param context The rendering context
   * @param node The UINode which is being rendered
   * @throws IOException
   */
  @SuppressWarnings("deprecation")
  private void _renderDescription(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (isInaccessibleMode(context))
      return;

    Object label = node.getAttributeValue(context, DESCRIPTION_ATTR);
    if (label == null)
      return;

    // Do not attempt to render this label if the underlying
    // platform does not support hidden labels
    if (!HiddenLabelUtils.supportsHiddenLabels(context))
      return;

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("span", null);
    renderStyleClassAttribute(context,HIDDEN_LABEL_STYLE_CLASS);
    writer.writeText(label, null);
    writer.endElement("span");
  }
}
